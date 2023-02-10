# SCRIPT TO GENERATE REPORTS
# Author: Lauro Marques Vicari

# loading packages

library(sf)
library(leaflet)
library(dplyr)
library(ctmgtools)
library(reshape2)
library(readxl)
library(leafpop)
library(ggplot2)
library(htmlwidgets)
library(ggmap)
library(stringi)
library(viridis)
library(leafem)
library(rmarkdown)
library(plotly)
library(htmltools)
library(RColorBrewer)
library(xml2)
library(stringr)
library(nngeo)

# loading data

dados_r <- dados_estrutura
dados_r$SETOR <- dados_r$SETOR |> as.numeric()

df_acomp07_r <- left_join(df_acomp07, dados_r, by = c("GeoCodigo" = "SETOR"))
df_acomp07_r$COD_MUNIC <- df_acomp07_r$COD_MUNIC |> as.numeric()

df_acomp07_r[is.na(df_acomp07_r$STATUS), "STATUS"] <- 0

df_acomp07_r$DPP <- df_acomp07_r$TotalDomOcupados + df_acomp07_r$TotalDomVagos + df_acomp07_r$TotalDomOcasional

# base data.frame

df_acomp07_r_munic <- df_acomp07_r |>
  group_by(COD_MUNIC, NM_MUNIC) |>
  summarise(EDOMOC = sum(EstimativaDppo),
            DPPO = sum(TotalDomOcupados),
            AUSENTES = sum(TotalDomAusentes),
            RECUSAS = sum(TotalDomRecusas),
            VAGOS = sum(TotalDomVagos),
            OCASIONAL = sum(TotalDomOcasional),
            DPPO_COLETADO_ESTIMADO = sum(TotalDomOcupados)/sum(EstimativaDppo),
            AUSENTES_percent = sum(TotalDomAusentes)/sum(TotalDomOcupados),
            RECUSA_percent = sum(TotalDomRecusas)/sum(TotalDomOcupados),
            VAGOS_percent = sum(TotalDomVagos)/sum(DPP),
            OCASIONAL_percent = sum(TotalDomOcasional)/sum(DPP),
            PESSOAS_2022 = sum(TotalPessoas),
            HOMENS_2022 = sum(TotalHomens),
            MULHERES_2022 = sum(TotalMulheres),
            RAZAO_SEXO_2022 = sum(TotalHomens) *100 /sum(TotalMulheres),
            OCUPADO_percent = sum(TotalDomOcupados)/sum(DPP)
  )

# working with thematic data from sex

pessoas_estimativas_r <- read_excel('pessoas_homens_mulheres_2010.xlsx')

df_acomp07_r_munic <- left_join(df_acomp07_r_munic, pessoas_estimativas_r, by = "COD_MUNIC") 
df_acomp07_r_munic <- df_acomp07_r_munic |>
  mutate(RAZAO_SEXO_2010 = HOMENS_2010 * 100 / MULHERES_2010)

cor_raca_2010_r <- read_excel('cor_raca_munic_2010.xlsx')

df_acomp07_r_munic <- left_join(df_acomp07_r_munic, cor_raca_2010_r, by = "COD_MUNIC")

# treating data

pessoas <- df_acomp07_r_munic[,c(1,35,20:31,14)]

names(pessoas)[15] <- "2022"
names(pessoas)[2] <- "NM_MUNIC"

pessoas_long <- melt(pessoas, id.vars = c("COD_MUNIC", "NM_MUNIC"))
pessoas_long$variable <- factor(pessoas_long$variable, levels = c("2010", "2011", "2012", "2013", "2014", "2015", "2016",
                                                                  "2017", "2018", "2019", "2020", "2021", "2022"))

homem <- df_acomp07_r_munic[,c(1,35,32,15)]
names(homem)[2] <- "NM_MUNIC"

homem_long <- melt(homem, id.vars = c("COD_MUNIC", "NM_MUNIC"))
homem_long$ANO <- str_sub(homem_long$variable, -4)
names(homem_long)[4] <- 'Homens'

mulher <- df_acomp07_r_munic[,c(1,35,33,16)]
names(mulher)[2] <- "NM_MUNIC"

mulher_long <- melt(mulher, id.vars = c("COD_MUNIC", "NM_MUNIC"))
mulher_long$ANO <- str_sub(mulher_long$variable, -4)
names(mulher_long)[4] <- 'Mulheres'

homem_long <- homem_long[order(homem_long$COD_MUNIC, homem_long$ANO, decreasing = F),]
mulher_long <- mulher_long[order(mulher_long$COD_MUNIC, mulher_long$ANO, decreasing = F),]

homem_mulher_long <- cbind(homem_long[,c(1,2,5,4)], Mulheres = mulher_long$Mulheres)

# demographic index

homem_mulher_long$razao_sex <- round(homem_mulher_long$Homens * 100/homem_mulher_long$Mulheres, 2)

# working with thematic data from race

munic_cor_raca_2010 <- read_excel("C:/Users/lauro/Desktop/api/cor_raca_munic_2010.xlsx")

munic_cor_raca_2010$NM_MUNIC  <- toupper(stri_trans_general(str = munic_cor_raca_2010$NM_MUNIC, id = "Latin-ASCII"))

tabela_universo <- tabela_universo[-nrow(tabela_universo),]
tabela_universo <- tabela_universo[order(tabela_universo$Indicadoras, decreasing = FALSE),]

munic_cor_raca_2010 <- munic_cor_raca_2010[order(munic_cor_raca_2010$NM_MUNIC, decreasing = FALSE),]

df_cor_raca <- cbind(munic_cor_raca_2010, tabela_universo)
colnames(df_cor_raca)[3:7] <- paste0(colnames(df_cor_raca)[3:7], "_2010")
colnames(df_cor_raca)[14:18] <- paste0(colnames(df_cor_raca)[14:18], "_2022")
#

cor_raca_munic_2010_b <- df_cor_raca[,c(1,2,3,14)]

cor_raca_munic_2010_b_long <- melt(cor_raca_munic_2010_b, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_b_long$ANO <- str_sub(cor_raca_munic_2010_b_long$variable, -4)
names(cor_raca_munic_2010_b_long)[4] <- 'Branca'

cor_raca_munic_2010_p <- df_cor_raca[,c(1,2,4,15)]

cor_raca_munic_2010_p_long <- melt(cor_raca_munic_2010_p, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_p_long$ANO <- str_sub(cor_raca_munic_2010_p_long$variable, -4)
names(cor_raca_munic_2010_p_long)[4] <- 'Preta'

cor_raca_munic_2010_pa <- df_cor_raca[,c(1,2,6,17)]

cor_raca_munic_2010_pa_long <- melt(cor_raca_munic_2010_pa, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_pa_long$ANO <- str_sub(cor_raca_munic_2010_pa_long$variable, -4)
names(cor_raca_munic_2010_pa_long)[4] <- 'Parda'

cor_raca_munic_2010_a <- df_cor_raca[,c(1,2,5,16)]

cor_raca_munic_2010_a_long <- melt(cor_raca_munic_2010_a, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_a_long$ANO <- str_sub(cor_raca_munic_2010_a_long$variable, -4)
names(cor_raca_munic_2010_a_long)[4] <- 'Amarela'

cor_raca_munic_2010_i <- df_cor_raca[,c(1,2,7,18)]

cor_raca_munic_2010_i_long <- melt(cor_raca_munic_2010_i, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_i_long$ANO <- str_sub(cor_raca_munic_2010_i_long$variable, -4)
names(cor_raca_munic_2010_i_long)[4] <- 'Indigena'

cor_raca_munic_long <- cbind(cor_raca_munic_2010_b_long, 
                             Preta = cor_raca_munic_2010_p_long$Preta,
                             Parda = cor_raca_munic_2010_pa_long$Parda,
                             Amarela = cor_raca_munic_2010_a_long$Amarela,
                             Indigena = cor_raca_munic_2010_i_long$Indigena)

cor_raca_munic_2010_b_p <- df_cor_raca[,c(1,2,8,19)]

cor_raca_munic_2010_b_p_long <- melt(cor_raca_munic_2010_b_p, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_b_p_long$ANO <- str_sub(cor_raca_munic_2010_b_p_long$variable, -4)
names(cor_raca_munic_2010_b_p_long)[4] <- 'Branca'

cor_raca_munic_2010_p_p <- df_cor_raca[,c(1,2,9,20)]

cor_raca_munic_2010_p_p_long <- melt(cor_raca_munic_2010_p_p, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_p_p_long$ANO <- str_sub(cor_raca_munic_2010_p_p_long$variable, -4)
names(cor_raca_munic_2010_p_p_long)[4] <- 'Preta'

cor_raca_munic_2010_pa_p <- df_cor_raca[,c(1,2,11,22)]

cor_raca_munic_2010_pa_p_long <- melt(cor_raca_munic_2010_pa_p, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_pa_p_long$ANO <- str_sub(cor_raca_munic_2010_pa_p_long$variable, -4)
names(cor_raca_munic_2010_pa_p_long)[4] <- 'Parda'

cor_raca_munic_2010_a_p <- df_cor_raca[,c(1,2,10,21)]

cor_raca_munic_2010_a_p_long <- melt(cor_raca_munic_2010_a_p, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_a_p_long$ANO <- str_sub(cor_raca_munic_2010_a_p_long$variable, -4)
names(cor_raca_munic_2010_a_p_long)[4] <- 'Amarela'

cor_raca_munic_2010_i_p <- df_cor_raca[,c(1,2,12,23)]

cor_raca_munic_2010_i_p_long <- melt(cor_raca_munic_2010_i_p, id.vars = c("COD_MUNIC", "NM_MUNIC"))
cor_raca_munic_2010_i_p_long$ANO <- str_sub(cor_raca_munic_2010_i_p_long$variable, -4)
names(cor_raca_munic_2010_i_p_long)[4] <- 'Indigena'

cor_raca_munic_p_long <- cbind(cor_raca_munic_2010_b_p_long, 
                               Preta = cor_raca_munic_2010_p_p_long$Preta,
                               Parda = cor_raca_munic_2010_pa_p_long$Parda,
                               Amarela = cor_raca_munic_2010_a_p_long$Amarela,
                               Indigena = cor_raca_munic_2010_i_p_long$Indigena)

cor_raca_munic_p_long[,c(4,6:9)] <- apply(cor_raca_munic_p_long[,c(4,6:9)], 2, function(x) {100 * round(x,4)})

# loop to generate graphs

for (i in 1:nrow(municipios_loop)) {
  
  rm(list = ls(pattern = "var"))
  
  # graph population
  
  filter_pessoas <- subset(pessoas_long, COD_MUNIC == municipios_loop[i,1])
  
  graph_pop <- plot_ly(filter_pessoas, type='bar', x = ~variable, y = ~value, text = ~value, width = 3,
                       marker = list(color = c(rep("gray", 12), "black")), textposition = 'outside',
                       hoverinfo = "text",
                       hovertext = paste("Ano :", filter_pessoas$variable,
                                         "<br> Pop. :", filter_pessoas$value))
  
  graph_pop <- graph_pop |>  layout(margin = list(b = 50, l = 50, t = 60, r = 50), hoverlabel=list(bgcolor="white"),
                                    yaxis = list(title = ""), xaxis = list(title = ""), bargap = 0.4,
                                    annotations =  list(
                                      list(x = 0, y = -0.13, text = paste0("Fontes: CD 2010, Estimativas IBGE, SIGC CD 2022 - Acomp07", " (", time, ")"),
                                           xref='paper', yref='paper', showarrow = F, 
                                           xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                                           font = list(size = 10)),
                                      list(x = 0, y = 1.1, text = "Estimativas populacionais e População coletada (2010 - 2022)",
                                           xref='paper', yref='paper', showarrow = F, 
                                           xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                           font = list(size = 18))))
  
  # text
  
  var1 = filter_pessoas$NM_MUNIC[1]
  var2 = filter_pessoas$value[13]
  var4 = abs(filter_pessoas$value[13] - filter_pessoas$value[1])
  var5 = filter_pessoas$value[1]
  
  
  if (filter_pessoas$value[13] > filter_pessoas$value[1]) {
    var3 = "um aumento de "
    var6 = "cresceu em "
    var7 = round((filter_pessoas$value[13] - filter_pessoas$value[1]) * 100/filter_pessoas$value[1],2)
  } else {
    var3 = "uma queda de "
    var6 = "diminuiu em "
    var7 = abs(round((filter_pessoas$value[13] - filter_pessoas$value[1]) * 100/filter_pessoas$value[1],2))
    
  }
  
  texto1 <- paste0(
    "O Censo Demográfico em ", var1," coletou ", var2, " pessoas em 2022. Este resultado representa ",
    var3, var4, " pessoas, em relação à população coletada em 2010 (", var5, " pessoas).",
    " Em termos percentuais, a população do município ", var6, var7, "%.")
  
  # graph sex
  
  filter_homem_mulher <- subset(homem_mulher_long, COD_MUNIC == municipios_loop[i,1])
  
  range_y_h_m <- max(c(filter_homem_mulher$Homens, filter_homem_mulher$Mulheres)) * 1.1
  
  graph_homem_mulher <- plot_ly(filter_homem_mulher, x = ~ANO, y = ~Homens, type = 'bar', name = "Homens", width = 400, height = 400,
                                text = ~Homens, marker = list(color = "steelblue"), textposition = 'outside',
                                hoverinfo = "text",
                                hovertext = paste("Ano :", filter_homem_mulher$ANO,
                                                  "<br> Homens:", filter_homem_mulher$Homens))
  
  graph_homem_mulher <- graph_homem_mulher |> add_trace(y = ~Mulheres, name = 'Mulheres',
                                                        text = ~Mulheres, marker = list(color = "red"), textposition = 'outside', cliponaxis = T,
                                                        hoverinfo = "text",
                                                        hovertext = paste("Ano :", filter_homem_mulher$ANO,
                                                                          "<br> Mulheres:", filter_homem_mulher$Mulheres))
  
  graph_homem_mulher <- graph_homem_mulher |>  layout(margin = list(b = 60, l = 50, t = 60, r = 50),
                                                      yaxis = list(title = "", range=c(0, range_y_h_m)), xaxis = list(title = ""), bargap = 0.4,
                                                      annotations =  list(
                                                        list(x = 0, y = -0.2, text = paste0("Fontes: CD 2010, SIGC CD 2022 - Acomp07", " (", time, ")"),
                                                             xref='paper', yref='paper', showarrow = F, 
                                                             xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                                                             font = list(size = 10)),
                                                        list(x = 0, y = 1.15, text = "Homens e mulheres (2010 - 2022)",
                                                             xref='paper', yref='paper', showarrow = F, 
                                                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                                             font = list(size = 15))),
                                                      legend = list(orientation = 'h', x = 0.5, y = -0.05, xanchor = "center"))
  
  # graph demographic index
  
  range_y_rsex <- max(filter_homem_mulher$razao_sex) + 20
  
  graph_raz_sex <- plot_ly(filter_homem_mulher, type='bar', x = ~ANO, y = ~razao_sex, text = ~razao_sex, width = 400, height = 400,
                           marker = list(color = c("orange", "orange")), textposition = 'outside',
                           hoverinfo = "text",
                           hovertext = paste("Ano :", filter_homem_mulher$ANO,
                                             "<br> Raz. Sexo:", filter_homem_mulher$razao_sex))
  
  graph_raz_sex <- graph_raz_sex |>  layout(margin = list(b = 60, l = 50, t = 60, r = 50), hoverlabel=list(bgcolor="white"),
                                            yaxis = list(title = "", range = c(0, range_y_rsex)), xaxis = list(title = ""), bargap = 0.4,
                                            annotations =  list(
                                              list(x = 0, y = -0.2, text = paste0("Fontes: CD 2010, SIGC CD 2022 - Acomp07", " (", time, ")"),
                                                   xref='paper', yref='paper', showarrow = F, 
                                                   xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                                                   font = list(size = 10)),
                                              list(x = 0, y = 1.15, text = "Razão de sexo (2010 - 2022)",
                                                   xref='paper', yref='paper', showarrow = F, 
                                                   xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                                   font = list(size = 15))))
  
  graphs_sex <- browsable(div(style = "width:100%;",
                              div(style = "width:50%; float: left;", graph_homem_mulher),
                              div(style = "width:50%; float: right;", graph_raz_sex)))
  
  # text
  
  var8 = filter_homem_mulher$Homens[2]
  var9 = filter_homem_mulher$Mulheres[2]
  var11 = abs(filter_homem_mulher$Mulheres[2] - filter_homem_mulher$Mulheres[1])
  var13 = abs(filter_homem_mulher$Homens[2] - filter_homem_mulher$Homens[1])
  var15 = round(abs(filter_homem_mulher$razao_sex[2] - filter_homem_mulher$razao_sex[1]), 2)
  var16 = floor(filter_homem_mulher$razao_sex[2])
  
  if(filter_homem_mulher$Mulheres[2] > filter_homem_mulher$Mulheres[1]) {
    var10 = "um aumento de "
  } else {
    var10 = "uma queda de "
  }
  
  if(filter_homem_mulher$Homens[2] > filter_homem_mulher$Homens[1]) {
    var12 = "um aumento de "
  } else {
    var12 = "uma queda de "
  }
  
  if(filter_homem_mulher$razao_sex[2] > filter_homem_mulher$razao_sex[1]) {
    var14 = "da elevação de "
  } else {
    var14 = "da diminuição de "
  }
  
  texto2 <- paste0(
    "Em relação à distribuição da população quanto ao sexo, a operação censitária de 2022 registrou ",
    var9,
    " mulheres e ",
    var8,
    " homens, resultado que caracteriza ",
    var10,
    var11,
    " mulheres e ",
    var12,
    var13,
    " homens, em relação aos números de 2010.")
  
  texto3 <- paste0(
    " Este cenário se expressa de forma mais direta, por meio ",
    var14,
    var15,
    " pontos do indicador Razão de Sexo, que evidencia a proporção de homens e mulheres em uma população. ",
    "O resultado para ",
    var1,
    " revela que a cada 100 mulheres da população existem ",
    var16,
    " homens residindo no município.")
  
  # graph race 1
  
  filter_cor_raca <- subset(cor_raca_munic_long, COD_MUNIC == municipios_loop[i,1])
  
  graph_cor_raca <- plot_ly(filter_cor_raca, x = ~ANO, y = ~Branca, type = 'bar', name = "Branca",
                            text = ~Branca, marker = list(color = "purple"), textposition = 'outside',
                            hoverinfo = "text",
                            hovertext = paste("Ano :", filter_cor_raca$ANO,
                                              "<br> Branca:", filter_cor_raca$Branca))
  
  graph_cor_raca <- graph_cor_raca |> 
    add_trace(y = ~Preta, name = 'Preta', text = ~Preta, marker = list(color = "yellow"), textposition = 'outside', hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca$ANO,
                                "<br> Preta:", filter_cor_raca$Preta)) |>
    add_trace(y = ~Amarela, name = 'Amarela', text = ~Amarela, marker = list(color = "orange"), textposition = 'outside',
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca$ANO,
                                "<br> Amarela:", filter_cor_raca$Amarela)) |>
    add_trace(y = ~Parda, name = 'Parda', text = ~Parda, marker = list(color = "green"), textposition = 'outside',
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca$ANO,
                                "<br> Parda:", filter_cor_raca$Parda)) |>
    add_trace(y = ~Indigena, name = 'Indigena', text = ~Indigena, marker = list(color = "pink"), textposition = 'outside',
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca$ANO,
                                "<br> Indígena:", filter_cor_raca$Indigena))
  
  graph_cor_raca <- graph_cor_raca |>  layout(margin = list(b = 60, l = 50, t = 60, r = 50), barmode = 'group',
                                              yaxis = list(title = ""), xaxis = list(title = ""), bargap = 0.4,
                                              annotations =  list(
                                                list(x = 0, y = -0.2, text = paste0("Fontes: CD 2010, SIGC CD 2022 - Tabulações", " (", time, ")"),
                                                     xref='paper', yref='paper', showarrow = F, 
                                                     xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                                                     font = list(size = 10)),
                                                list(x = 0, y = 1.1, text = "Cor ou raça (2010 - 2022)",
                                                     xref='paper', yref='paper', showarrow = F, 
                                                     xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                                     font = list(size = 15))),
                                              legend = list(orientation = 'h', x = 0.05, y = -0.045, xanchor = "left",  font = list(size = 7)))
  
  # graph race 2
  
  filter_cor_raca_p <- cor_raca_munic_p_long |> filter(COD_MUNIC == municipios_loop[i,1])
  
  graph_cor_raca_p <- plot_ly(filter_cor_raca_p, x = ~ANO, y = ~Preta, type = 'bar', name = "Preta",
                              text = ~Preta, marker = list(color = "yellow"), textposition = 'inside', insidetextanchor = "middle",
                              hoverinfo = "text",
                              hovertext = paste("Ano :", filter_cor_raca_p$ANO,
                                                "<br> Preta:", filter_cor_raca_p$Preta))
  
  graph_cor_raca_p <- graph_cor_raca_p |> 
    add_trace(y = ~Branca, name = 'Branca', text = ~Branca, marker = list(color = "Purple"), textposition = 'inside', insidetextanchor = "middle",
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca_p$ANO,
                                "<br> Branca:", filter_cor_raca_p$Branca)) |>
    add_trace(y = ~Amarela, name = 'Amarela', text = ~Amarela, marker = list(color = "orange"), textposition = 'outside',
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca_p$ANO,
                                "<br> Amarela:", filter_cor_raca_p$Amarela)) |>
    add_trace(y = ~Parda, name = 'Parda', text = ~Parda, marker = list(color = "green"), textposition = 'inside', insidetextanchor = "middle",
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca_p$ANO,
                                "<br> Parda:", filter_cor_raca_p$Parda)) |>
    add_trace(y = ~Indigena, name = 'Indigena', text = ~Indigena, marker = list(color = "pink"), textposition = 'outside',
              hoverinfo = "text",
              hovertext = paste("Ano :", filter_cor_raca_p$ANO,
                                "<br> Indígena:", filter_cor_raca_p$Indígena))
  
  graph_cor_raca_p <- graph_cor_raca_p |>  layout(margin = list(b = 60, l = 50, t = 60, r = 50), barmode = "stack",
                                                  yaxis = list(title = "", range = c(0, 110)), xaxis = list(title = ""), bargap = 0.4,
                                                  annotations =  list(
                                                    list(x = 0, y = -0.2, text = paste0("Fontes: CD 2010, SIGC CD 2022 - Tabulações", " (", time, ")"),
                                                         xref='paper', yref='paper', showarrow = F, 
                                                         xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                                                         font = list(size = 10)),
                                                    list(x = 0, y = 1.1, text = "Cor ou raça (%) (2010 - 2022)",
                                                         xref='paper', yref='paper', showarrow = F, 
                                                         xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                                         font = list(size = 15))),
                                                  legend = list(orientation = 'h', x = 0.05, y = -0.045, xanchor = "left", font = list(size = 7)),
                                                  uniformtext=list(minsize=8, mode='show'))
  
  graphs_cor_raca <- browsable(div(style = "width:100%;",
                                   div(style = "width:50%; float: left;", graph_cor_raca),
                                   div(style = "width:50%; float: right;", graph_cor_raca_p)))
  
  # text
  
  var17 = filter_cor_raca$Branca[2]
  var18 = filter_cor_raca_p$Branca[2]
  var19 = filter_cor_raca$Preta[2]
  var20 = filter_cor_raca_p$Preta[2]
  var21 = filter_cor_raca$Amarela[2]
  var22 = filter_cor_raca_p$Amarela[2]
  var23 = filter_cor_raca$Parda[2]
  var24 = filter_cor_raca_p$Parda[2]
  var25 = filter_cor_raca$Indigena[2]
  var26 = filter_cor_raca_p$Indigena[2]
  
  parametro_cor_raca <- filter_cor_raca_p[,c(2,4,6:9)]
  parametro_cor_raca_long_2010 <- melt(parametro_cor_raca[1,], id.vars = "NM_MUNIC")
  parametro_cor_raca_long_2022 <- melt(parametro_cor_raca[2,], id.vars = "NM_MUNIC")
  
  parametro_cor_raca_long <- cbind(parametro_cor_raca_long_2010, ano_2022 = parametro_cor_raca_long_2022$value)
  names(parametro_cor_raca_long)[3] <- 'ano_2010'
  parametro_cor_raca_long$dif <- parametro_cor_raca_long$ano_2022 - parametro_cor_raca_long$ano_2010
  parametro_cor_raca_long$dif_abs <- abs(parametro_cor_raca_long$ano_2022 - parametro_cor_raca_long$ano_2010)
  
  if (sum(abs(parametro_cor_raca_long$dif) > 5) >= 2) {
    
    var27 = "relativa alteração"
    
  } else {
    
    var27 = "relativa estabilidade"
    
  }
  
  if (sum(abs(parametro_cor_raca_long$dif) > 5) >= 2) {
    
    var28 = parametro_cor_raca_long[parametro_cor_raca_long$dif_abs == max(parametro_cor_raca_long$dif_abs), "variable"] |> as.character()
    
  } 
  
  if (exists('var28')) {
    
    if(subset(parametro_cor_raca_long, variable == var28)$dif > 0) {
      
      var29 = "elevação de "
      
    } else {
      
      var29 = "diminuição de "
      
    }
    
  }
  
  if (exists('var28')) {
    
    var30 = abs(round(subset(parametro_cor_raca_long, variable == var28)$dif, 2))
    
  }
  
  if (exists('var29')) { 
    
    if(var29 == "elevação de ") {
      
      var31 = "uma queda"
      
    } else {
      
      var31 = "um aumento"
    }
    
  }
  
  if (exists('var31')) {
    
    if(var31 == "uma queda") {
      
      var32 = parametro_cor_raca_long[parametro_cor_raca_long$dif == min(parametro_cor_raca_long$dif), "variable"] |> as.character()
      
    } else {
      
      var32 = parametro_cor_raca_long[parametro_cor_raca_long$dif == max(parametro_cor_raca_long$dif), "variable"] |> as.character()
      
    }
    
  }
  
  
  if (exists('var31')) {
    
    if(var31 == "uma queda") {
      
      var32 = parametro_cor_raca_long[parametro_cor_raca_long$dif == min(parametro_cor_raca_long$dif), "variable"] |> as.character()
      
    } else {
      
      var32 = parametro_cor_raca_long[parametro_cor_raca_long$dif == max(parametro_cor_raca_long$dif), "variable"] |> as.character()
      
    }
    
  }  
  
  if(exists('var32')) {
    
    var33 = round(subset(parametro_cor_raca_long, variable == var32)$dif_abs,2)
    
  }
  
  if(exists('var28')) {
    
    texto4 <- paste0(
      "Já no que diz respeito ao quesito cor/raça, a coleta do Censo 2022 apresentou os seguintes resultados para o município: ",
      "(i) Brancos: ",
      var17,
      " (",
      var18,
      "%), ",
      "(ii) Pretos: ",
      var19,
      " (",
      var20,
      "%), ",
      "(iii) Amarelos: ",
      var21,
      " (",
      var22,
      "%), ",
      "(iv) Pardos: ",
      var23,
      " (",
      var24,
      "%), ",
      "e (v) Indígenas: ",
      var25,
      " (",
      var26,
      "%).",
      
      "Esta distribuição apresentou ",
      var27,
      " em relação ao observado em 2010. ",
      
      "A maior mudança se deu na categoria ",
      var28,
      ", sendo observada uma ",
      var29,
      var30,
      " p. p. em relação a 2010.",
      " Já em sentido contrário observou-se, em maior grau, ",
      var31,
      " na categoria ",
      var32,
      ", na magnitude de ",
      var33,
      " p. p. em comparação ao último Censo.")
    
  } else {
    
    texto4 <- paste0(
      "Já no que diz respeito ao quesito cor/raça, a coleta do Censo 2022 apresentou os seguintes resultados para o município: ",
      "(i) Brancos: ",
      var17,
      " (",
      var18,
      "%), ",
      "(ii) Pretos: ",
      var19,
      " (",
      var20,
      "%), ",
      "(iii) Amarelos: ",
      var21,
      " (",
      var22,
      "%), ",
      "(iv) Pardos: ",
      var23,
      " (",
      var24,
      "%), ",
      "e (v) Indígenas: ",
      var25,
      " (",
      var26,
      "%).",
      
      "Esta distribuição apresentou ",
      var27,
      " em relação ao observado em 2010.")
    
    
  }
  
  # saving
  
  render("C:/Users/lauro/Desktop/template_prov.rmd", output_file = paste0('C:/Users/lauro...', municipios_loop[i,2], "/", municipios_loop[i,2], "_relatorio_fechamento", '.html'))
  
}

