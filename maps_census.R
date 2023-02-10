# SCRIPT TO GENERATE MAPS 
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

# setting system time

time <- format(Sys.time(), "%d/%m/%y %H:%M:%S")
time_save <- format(Sys.time(), "%d-%m-%y %H-%M-%S")

# sourcing webscraping scripts

source("C:/Users/lauro/Desktop/api/scripts/acomp07.R")

source("C:/Users/lauro/Desktop/api/scripts/cor_raca_mapas_areas.R")

# realing shp files

mg_munics <- st_read("D:/IBGE/censo_demografico/Shape_2021/MG_Municipios_2020/MG_Municipios_2020.shp")

# transforming and treating sf objects

mg_munics <- st_transform(mg_munics, st_crs('+proj=longlat +datum=WGS84'))
mg_munics$CD_MUN <- mg_munics$CD_MUN |> as.numeric()

dados <- dados_estrutura
dados$SETOR <- dados$SETOR |> as.numeric()

df_acomp07 <- left_join(df_acomp07, dados, by = c("GeoCodigo" = "SETOR"))
df_acomp07$COD_MUNIC <- df_acomp07$COD_MUNIC |> as.numeric()

# treating data

df_acomp07 <- df_acomp07 |> mutate(STATUS =
                                     case_when(Status == "Realizado" ~ "REALIZADO (+)",
                                               Status == "Liberado para pagamento" ~ "REALIZADO (+)",
                                               Status == "Supervisionado" ~ "REALIZADO (+)",
                                               Status == "Pago" ~ "REALIZADO (+)")                                   
)

df_acomp07[is.na(df_acomp07$STATUS), "STATUS"] <- 0

df_acomp07$DPP <- df_acomp07$TotalDomOcupados + df_acomp07$TotalDomVagos + df_acomp07$TotalDomOcasional

# base data.frame

df_acomp07_munic <- df_acomp07 |>
  group_by(COD_MUNIC, NM_MUNIC) |>
  summarise("REALIZADO (+)" = sum(STATUS == "REALIZADO (+)")/n(),
            "Não iniciado" = sum(Status == "Não iniciado"),
            "Associado" = sum(Status == "Associado"),
            "Carregado no DMC" = sum(Status == "Carregado no DMC"),
            "Em Andamento" = sum(Status == "Em Andamento"),
            "Paralisado" = sum(Status == "Paralisado"),
            "Realizado" = sum(Status == "Realizado"),
            "Supervisionado" = sum(Status == "Supervisionado"),
            "Liberado para pagamento" = sum(Status == "Liberado para pagamento"),
            "Pago" = sum(Status == "Pago"),
            EDOMOC = sum(EstimativaDppo),
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

pessoas_estimativas <- read_excel('pessoas_homens_mulheres_2010.xlsx')

df_acomp07_munic <- left_join(df_acomp07_munic, pessoas_estimativas, by = "COD_MUNIC") 
df_acomp07_munic <- df_acomp07_munic |>
  mutate(RAZAO_SEXO_2010 = HOMENS_2010 * 100 / MULHERES_2010)

# working with thematic data from race

cor_raca_2010 <- read_excel('cor_raca_munic_2010.xlsx')

# layer 1: status [cor = percent real (+); poupup = descrição de status]
# layer 2: DPPs: [cor = % DPP coletado/EDOMCO; label = %; poupup = grafico de setores DPPO; DPPV; DPPUO]
# layer 3: Recusa + ausente [cor = % (recusa + ausencia) - 3%; poupup = texto] 
# layer 4: pessoas [cor = pessoas coletadas 2022/2021; poupup = gráfico pessoas]
# layer 5: razao sexo [cor = se está aumentando ou diminuindo; poupup = grafico homem mulher 2010 2022]

filter_munic <- unique(df_acomp07_munic$COD_MUNIC)

mg_munics <- mg_munics |>
  filter(CD_MUN %in% c(filter_munic))

mg_munics <- left_join(mg_munics, df_acomp07_munic, by = c("CD_MUN" = "COD_MUNIC"))

# ploting graph

pessoas <- mg_munics[,c(1,32:44,27)] |>
  as.data.frame()

pessoas <- pessoas[,c(1:15)]

names(pessoas)[15] <- "2022"

pessoas_long <- melt(pessoas, id.vars = c("CD_MUN", "Município"))
pessoas_long$variable <- factor(pessoas_long$variable, levels = c("2010", "2011", "2012", "2013", "2014", "2015", "2016",
                                                                  "2017", "2018", "2019", "2020", "2021", "2022"))

mg_munics$graph_pessoas <- ""

# looping for graph

for (i in 1:nrow(mg_munics)) {
  
  filter_pessoas <- subset(pessoas_long, CD_MUN == pessoas_long$CD_MUN[i])
  
  graph <- ggplot(data = filter_pessoas, mapping = aes(x = variable, y = value)) +
    geom_col(width = 0.5, fill = c(rep("steelblue", 12), "red")) +
    geom_text(data = filter_pessoas, aes(label = value, x = variable, y = value), color = "black", size = 3, vjust = -0.5) +
    labs(title = filter_pessoas$Município[1], subtitle = "Estimativas populacionais e População de 2022",
         caption = paste0("Fontes: CD 2010, Estimativas IBGE, SIGC CD 2022 - Acomp07", " (", time, ")")) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(face = "bold", size = 8),
          legend.position = "none",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(hjust = 0))
  
  mg_munics$graph_pessoas[i] <- list(graph)
  
}

# working with thematic data of households

domicilios <- mg_munics[,c(1,32,31,25,26)] |>
  as.data.frame()

domicilios <- domicilios[,c(1:5)]

names(domicilios)[3:5] <- c("DPPO", "DPPV", "DPPUO")

domicilios_long <- melt(domicilios, id.vars = c("CD_MUN", "Município"))
domicilios_long$variable <- factor(domicilios_long$variable, levels = c("DPPO", "DPPV", "DPPUO"))

mg_munics$graph_domicilios <- ""

mycols <- c("royalblue4", "orange1", "red1")

# looping for graph

for (i in 1:nrow(mg_munics)) {
  
  filter_domicilios <- subset(domicilios_long, CD_MUN == domicilios_long$CD_MUN[i])
  
  graph_1 <- ggplot(data = filter_domicilios, mapping = aes(x = variable, y = value)) +
    geom_col(width = 0.5, fill = mycols) +
    geom_text(data = filter_domicilios, aes(label = paste0(round(value, 4) * 100, "%"), x = variable, y = value), color = "black", size = 3, vjust = -0.5) +
    labs(title = filter_domicilios$Município[1], subtitle = "Domicílios Particulares Permanentes (%)",
         caption = paste0("Fonte: SIGC CD 2022 - Acomp07", " (", time, ")")) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(face = "bold", size = 8),
          legend.position = "none",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(hjust = 0))
  
  mg_munics$graph_domicilios[i] <- list(graph_1)
  
}

# defining color palletes

mg_munics$REC_AUS_PERCENT <- mg_munics$AUSENTES_percent + mg_munics$RECUSA_percent
mg_munics$parametro_aus_rec <- 0.03 - mg_munics$REC_AUS_PERCENT

pallete_aus_rec <- mg_munics[!is.na(mg_munics$parametro_aus_rec), "parametro_aus_rec"] |> as.data.frame()

pal_aus_rec_1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(abs(ceiling(min(pallete_aus_rec$parametro_aus_rec * 100))))
pal_aus_rec_2 <- colorRampPalette(colors = c("white", "green"), space = "Lab")(abs(ceiling(max(pallete_aus_rec$parametro_aus_rec * 100))))

pallete_aus_rec_ramp <- c(pal_aus_rec_1, pal_aus_rec_2)

pal_aus_rec_uso <- colorNumeric(
  palette = pallete_aus_rec_ramp,
  domain = mg_munics$parametro_aus_rec)

razao_sex_completa <- mg_munics[,c(1,32,46,45,29,28,44)] |>
  as.data.frame()

razao_sex_completa$pessoas_coletadas <- rowSums(razao_sex_completa[,c(5,6)]/razao_sex_completa$`2021`)

razao_sex <- mg_munics[,c(1,32,46,45,29,28)] |>
  as.data.frame()

razao_sex$pessoas_coletadas <- rowSums(razao_sex[,c(5,6)])/rowSums(razao_sex[,c(3,4)])

razao_sex <- razao_sex[,c(1:6)]

razao_sex <- razao_sex |>
  mutate(percent_M_2010 = MULHERES_2010/(MULHERES_2010 + HOMENS_2010),
         percent_M_2022 = MULHERES_2022/(MULHERES_2022 + HOMENS_2022),
         percent_H_2010 = HOMENS_2010/(HOMENS_2010 + MULHERES_2010),
         percent_H_2022 = HOMENS_2022/(HOMENS_2022 + MULHERES_2022))

razao_sex_percent <- razao_sex[,c(1,2,7:10)]

razao_sex_long <- melt(razao_sex_percent, id.vars = c("CD_MUN", "Município"))
razao_sex_long$ANO <- str_sub(razao_sex_long$variable, 11, 14)
razao_sex_long$SEXO <- str_sub(razao_sex_long$variable, 9,9)

razao_sex_long <- razao_sex_long |>
  mutate(SEXO_LABEL = case_when(razao_sex_long$SEXO == "H" ~ "Homens",
                                razao_sex_long$SEXO == "M" ~ "Mulheres"))

razao_sex_long$SEXO_LABEL <- factor(razao_sex_long$SEXO_LABEL, levels = c("Mulheres", "Homens"))

mycols <- c("red", "steelblue")

mg_munics$graph_raz_sex <- ""

# looping for graph

for (i in 1:nrow(mg_munics)) {
  
  filter_raz_sex <- subset(razao_sex_long, CD_MUN == razao_sex_long$CD_MUN[i])
  
  percent_pessoas_coletadas <- subset(razao_sex_completa, CD_MUN == razao_sex$CD_MUN[i])
  
  graph_2 <- ggplot(filter_raz_sex, aes(fill=SEXO_LABEL, y=value, x=ANO)) + 
    geom_bar(position="fill", stat="identity", width = 0.3) +
    geom_text(data = filter_raz_sex, aes(label = paste0(round(value, 4) * 100, "%"), x = ANO, y = value), color = "black", size = 4, position = position_stack(vjust = 0.5)) +
    labs(title = paste0(filter_raz_sex$Município[1], " (", round(percent_pessoas_coletadas$pessoas_coletadas, 4) * 100, "% Coletado)" ), subtitle = "Homens e Mulheres 2010 x 2022",
         caption = paste0("Fontes: CD 2010, SIGC CD 2022 - Acomp07", " (", time, ")")) +
    scale_fill_manual(values = mycols) +
    labs(x="")+
    theme_classic() +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(face = "bold", size = 8),
          legend.position = "bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          legend.title=element_blank(),
          legend.text = element_text(size = 12),
          plot.caption = element_text(hjust = 0))
  
  mg_munics$graph_raz_sex[i] <- list(graph_2)
  
}

# defining color palletes

mg_munics$RAZ_SEX_DIF <- mg_munics$RAZAO_SEXO_2022 - mg_munics$RAZAO_SEXO_2010

pallete_raz_sex <- mg_munics[!is.na(mg_munics$RAZ_SEX_DIF), "RAZ_SEX_DIF"] |> as.data.frame()

pal_raz_sex_red <- colorRampPalette(colors = c("red", "white"), space = "Lab")(abs(ceiling(min(pallete_raz_sex$RAZ_SEX_DIF * 100))))
pal_raz_sex_blue <- colorRampPalette(colors = c("white", "blue"), space = "Lab")(abs(ceiling(max(pallete_raz_sex$RAZ_SEX_DIF * 100))))

rampcols_raz_sex <- c(pal_raz_sex_red, pal_raz_sex_blue)

pal_raz_sex <- colorNumeric(
  palette = rampcols_raz_sex,
  domain = mg_munics$RAZ_SEX_DIF)

mg_munics$percent_pessoas <- mg_munics$PESSOAS_2022 / mg_munics$`2021`

pal_pessoas <- colorNumeric(
  palette = "Blues",
  domain = mg_munics$percent_pessoas)

pal_domicilios <- colorNumeric(
  palette = "Blues",
  domain = mg_munics$DPPO_COLETADO_ESTIMADO)

# loading race data from 2010

munic_cor_raca_2010 <- read_excel("C:/Users/lauro/Desktop/api/cor_raca_munic_2010.xlsx")

munic_cor_raca_2010$NM_MUNIC  <- toupper(stri_trans_general(str = munic_cor_raca_2010$NM_MUNIC, id = "Latin-ASCII"))

df_cor_raca <- left_join(tabela_universo, munic_cor_raca_2010, by = c("Indicadoras" = "NM_MUNIC"))

munic_cor_raca_2010_2022 <- df_cor_raca |> mutate(
  dif_branca = branca_percent_2022 - branca_percent_2010,
  dif_amarela = amarela_percent_2022 - amarela_percent_2010,
  dif_parda = parda_percent_2022 - parda_percent_2010,
  dif_preta = preta_percent_2022 - preta_percent_2010,
  dif_indigena = indigena_percent_2022 - indigena_percent_2010
  
)

mg_munics <- left_join(mg_munics, munic_cor_raca_2010_2022, by = c("CD_MUN" = "COD_MUNIC"))

mg_munics$graph_cor_raca <- ""

cor_raca_munic <- mg_munics[,c(1,32,61:65,71:75)] |> as.data.frame()

cor_raca_munic <- cor_raca_munic[,-c(13)] 

cor_raca_munic_long <- melt(cor_raca_munic, id.vars = c("CD_MUN", "Município"))

cor_raca_munic_long$ANO <- str_sub(cor_raca_munic_long$variable, -4) |> as.numeric()
cor_raca_munic_long$ANO <- factor(cor_raca_munic_long$ANO, levels = c("2010", "2022"))

cor_raca_munic_long$COR <- gsub("_percent_2010", "", cor_raca_munic_long$variable)
cor_raca_munic_long$COR <- gsub("_percent_2022", "", cor_raca_munic_long$COR)

cor_raca_munic_long$COR <- factor(cor_raca_munic_long$COR, levels = c("amarela", "branca", "preta",
                                                                      "parda", "indigena"))

# looping for graph

for (i in 1:nrow(mg_munics)) {
  
  filter_cor_raca <- subset(cor_raca_munic_long, CD_MUN == mg_munics$CD_MUN[i])
  
  percent_pessoas_coletadas <- subset(razao_sex_completa, CD_MUN == mg_munics$CD_MUN[i])
  
  graph_3 <- ggplot(filter_cor_raca, aes(fill=COR, y=value, x=ANO)) + 
    geom_bar(position="fill", stat="identity", width = 0.3) +
    geom_text(data = filter_cor_raca, aes(label = paste0(round(value, 4) * 100, "%"), x = ANO, y = value), color = "black", size = 4, position = position_stack(vjust = 0.5)) +
    labs(title = paste0(filter_cor_raca$Município[1], " (", round(percent_pessoas_coletadas$pessoas_coletadas, 4) * 100, "% Coletado)" ), subtitle = "População segundo cor/raça 2010 x 2022",
         caption = paste0("Fontes: CD 2010, SIGC CD 2022 - Tabulações", " (", time, ")")) +
    scale_fill_brewer(palette = "Set2")+
    labs(x="")+
    theme_classic() +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(face = "bold", size = 8),
          legend.position = "bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          legend.title=element_blank(),
          plot.caption = element_text(hjust = 0),
          legend.text = element_text(size = 12)
    )
  
  mg_munics$graph_cor_raca[i] <- list(graph_3)
  
}

# working with thematic data of age

idade_presumida <- read_excel("idade_presumida.xlsx")

idade_presumida <- idade_presumida |>
  filter(CATEGORIA == "relativo" & COD_GEOGRAFICO %in% filter_munic)

mg_munics <- left_join(mg_munics, idade_presumida, by = c("CD_MUN" = "COD_GEOGRAFICO"))
mg_munics$idade_presumida <- mg_munics$VALOR * -1

mg_munics <- st_simplify(mg_munics, preserveTopology = TRUE, dTolerance = 1000)

pal_idade_presumida <- colorNumeric(
  palette = c("red", "white"),
  domain = mg_munics$idade_presumida)

pal_status <- colorNumeric(
  palette = c("white", "green"),
  domain = mg_munics$`REALIZADO (+)`)

pal_estimada <- colorNumeric(
  palette = "Blues",
  domain = mg_munics$DPPO_COLETADO_ESTIMADO)

cor_raca_variance <- munic_cor_raca_2010_2022[,c(12, 23:27)]
cor_raca_variance_long <- melt(cor_raca_variance, id.vars = "COD_MUNIC")

cor_raca_var <- aggregate(value~COD_MUNIC, data = cor_raca_variance_long, var)

mg_munics <- left_join(mg_munics, cor_raca_var, by = c("CD_MUN" = "COD_MUNIC"))

pal_cor_raca <- colorNumeric(
  palette = "Blues",
  domain = mg_munics$value)

areas <- dados[,c(5,1,2)] |> filter(COD_AREA %in% c(filter)) |>
  distinct()

areas$COD_MUNIC <- areas$COD_MUNIC |> as.numeric()

mg_munics <- left_join(mg_munics, areas, by = c("CD_MUN" = "COD_MUNIC"))

areas_unique <- unique(mg_munics$COD_AREA) |> as.numeric()

dir <- read_excel("dir.xlsx")

link <- ""

# looping to create maps

for (i in 1:length(areas_unique)) {
  
  mg_munics_filter <- subset(mg_munics, COD_AREA == areas_unique[i])
  
  dir_filter <- subset(dir, COD_AREA == areas_unique[i])
  
  setwd(paste0("C:/Users/lauro...", dir_filter$dir))
  
  file <- list.files(pattern = "mapa_acomp")
  
  if (file.exists(file)) {
    
    file.remove(file)
    
  }
  
  # mapa output
  
  mapa <- leaflet() |>
    addTiles() |>
    addProviderTiles("OpenStreetMap.Mapnik") |>
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8, group = "Andamento da coleta",
                opacity = 1, fillColor = ~pal_status(`REALIZADO (+)`), label = ~paste0("Setores realizados (+): ", round(`REALIZADO (+)`,2) * 100, "%"),
                popup = paste0("<div style='text-align:center'>", 
                               "<b></b>", "<strong>", mg_munics_filter$NM_MUN,
                               "</strong>", "<br>","</div>", 
                               "<b></b>", "<br>", "<b>Não iniciado: </b>", mg_munics_filter$`Não iniciado`, "<br>",
                               "<b>Associado: </b>", mg_munics_filter$Associado, "<br>",
                               "<b>Carregado no DMC: </b>", mg_munics_filter$`Carregado no DMC`, "<br>",
                               "<b>Em andamento: </b>", mg_munics_filter$`Em Andamento`, "<br>",
                               "<b>Paralisado: </b>", mg_munics_filter$Paralisado, "<br>",
                               "<b>Realizado: </b>", mg_munics_filter$Realizado, "<br>",
                               "<b>Supervisionado: </b>", mg_munics_filter$Supervisionado, "<br>",
                               "<b>Liberado para pagamento: </b>", mg_munics_filter$`Liberado para pagamento`, "<br>",
                               "<b>Pago: </b>", mg_munics_filter$Pago, "<br>"), labelOptions = labelOptions(textsize = "15px")) |>
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8, group = "Domicílios",
                opacity = 1, fillColor = ~pal_domicilios(DPPO_COLETADO_ESTIMADO), label = ~paste0("DPPO coletado/estimado: ",  round(mg_munics_filter$DPPO_COLETADO_ESTIMADO, 4) * 100, "%"),labelOptions = labelOptions(textsize = "15px"), popup = popupGraph(mg_munics_filter$graph_domicilios, height = 400, width = 600)) |>
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8, group = "Pessoas",
                opacity = 1, fillColor = ~pal_pessoas(percent_pessoas), label = ~paste0("Pessoas coletadas: ", round(mg_munics_filter$percent_pessoas, 4) * 100, "%"), popup = popupGraph(mg_munics_filter$graph_pessoas, height = 400, width = 600), labelOptions = labelOptions(textsize = "15px")) |>
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8,
                opacity = 1, fillColor = ~pal_aus_rec_uso(parametro_aus_rec), group = "Ausência e Recusa",
                popup = paste0("<div style='text-align:center'>", 
                               "<b></b>", "<strong>", mg_munics_filter$NM_MUN,
                               "</strong>", "<br>","</div>", 
                               "<b></b>", "<br>", "<b>Ausência: </b>", paste0(round(mg_munics_filter$AUSENTES_percent, 4) * 100, "%"), "<br>",
                               "<b>Recusa: </b>", paste0(round(mg_munics_filter$RECUSA_percent, 4) * 100, "%"), "<br>")) |>
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8, group = "Idade presumida",
                opacity = 1, fillColor = ~pal_idade_presumida(idade_presumida), label = ~paste0("Declaração de idade presumida: ", VALOR, "%"), labelOptions = labelOptions(textsize = "15px")) |> 
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8, group = "Cor/Raça",
                opacity = 1, fillColor = ~pal_cor_raca(value), popup = popupGraph(mg_munics_filter$graph_cor_raca, height = 400, width = 600)) |>
    addPolygons(data = mg_munics_filter, color = "black", weight = 1, fillOpacity = 0.8, group = "Razão de Sexo",
                opacity = 1, fillColor = ~pal_raz_sex(RAZ_SEX_DIF), label = ~paste0("Razão de Sexo - dif. p.p: ",  round(mg_munics_filter$RAZ_SEX_DIF, 2)), popup = popupGraph(mg_munics_filter$graph_raz_sex, height = 400, width = 600), labelOptions = labelOptions(textsize = "15px")) |>
    addLayersControl(
      baseGroups = c("Andamento da coleta", "Domicílios", "Pessoas",  "Ausência e Recusa", "Idade presumida", "Cor/Raça",  "Razão de Sexo"),
      options = layersControlOptions(collapsed = FALSE)
    ) |> htmlwidgets::onRender(paste0("function() {$('.leaflet-control-layers-list').",
                                      "prepend('<label style=\"text-align:center\">", "<b></b>", "<strong>", "CD2022 - IBGE/MG", "<b></b>", "<br>", "<b></b>", 
                                      paste0(paste0('<a href=', '"', link , '"', 'target="_blank"', '>'), mg_munics_filter$NM_AREA[1] , '</a>'), "</strong>", "<br>", "<b></b>", "(", time, ")", "<br>", "<br>", "<b></b>", 
                                      "</label>');}")) |>
    addLogo("https://servicodados.ibge.gov.br/api/v1/resize/image?caminho=agenciadenoticias.ibge.gov.br//images/censo2020/logotipo_censo2022_rgb.jpg&maxwidth=500&maxheight=500",
            position = "bottomleft")
  
  # saving
  
  saveWidget(mapa, file = paste0(mg_munics_filter$NM_AREA[1], "_", "mapa_acomp_", 
                                 time_save, ".html"))
  
  
}


