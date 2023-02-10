# SCRIPT TO IDENTIFY INVASIONS IN CENSUS TRACTS
# Author: Lauro Marques Vicari

# loading packages

library (jsonlite)
library (dplyr)
library (writexl)
library (readxl)
library (ctmgtools)
library (xml2)
library (sf)
library (stringr)
library (nngeo)
library (ggplot2)

# reading kml files

lista_kmls <- list.files("kmls/")

lista_kmls_df <- data.frame(file = lista_kmls,
                            setor = str_sub(lista_kmls, 1, 15))

# looping to work with features polygon and points

percent_coord_dentro <- list()

for (i in 1:nrow(lista_kmls_df)) {
  
  setor <- read_xml(paste0("kmls/", lista_kmls_df[i,1])) |> 
    xml_ns_strip()
  
  contorno <- xml_find_all(setor, '//Folder[@id = "contorno"]/Placemark/Polygon/outerBoundaryIs') %>% 
    xml_text()
  
  coordenadas <- read.table(text = contorno,
                            sep = ",",
                            col.names = c("X", "Y"))
  
  poligono <- st_multipoint(as.matrix(coordenadas)) |> 
    st_cast("LINESTRING")
  
  poligono_crs <- st_sfc(poligono, crs = 4674)
  
  # buffering to consider an error of coordinates of 35 meters
  
  poligono_buf <- st_buffer(poligono_crs, 35)
  
  entire_buf <- st_remove_holes(poligono_buf)
  
  enderecos <- xml_find_all(setor, '//Folder[@id = "pontos"]/Placemark[contains(name, "Inc") or contains(name, "Conf")]/Point') %>% 
    xml_text()
  
  questionarios <- xml_find_all(setor, '//Folder[@id = "questionarios"]/Placemark[contains(name, "Quest")]/Point') %>% 
    xml_text() 
  
  coordenadas_enderecos <- read.table(text = enderecos,
                                      sep = ",", 
                                      col.names = c("X", "Y"))
  
  pontos_ends <- st_as_sf(coordenadas_enderecos, coords = c("X", "Y"), crs = 4674)
  
  match_ends <- st_intersects(pontos_ends, entire_buf, sparse = FALSE) |>
    as.data.frame()
  
  # conditions to work with data
  
  if (!identical(questionarios, character(0))) {
    
    coordenadas_questionarios <- read.table(text = questionarios,
                                            sep = ",", 
                                            col.names = c("X", "Y"))
    # transformig the crs
    
    pontos_quests <- st_as_sf(coordenadas_questionarios, coords = c("X", "Y"), crs = 4674)
    
    match_quests <- st_intersects(pontos_quests, entire_buf, sparse = FALSE) |>
      as.data.frame()
    
    n_coordenadas_quests <- nrow(match_quests)
    percent_quests <- sum(match_quests$V1)/nrow(match_quests)
    n_coord_quest_dentro = sum(match_quests$V1)
    
    
  } else {
    
    n_coord_quest_dentro = 0
    n_coordenadas_quests = 0
    percent_quests = 0
    
  }
  
  setor <- lista_kmls_df$setor[i]
  n_coordenadas_ends <- nrow(match_ends)
  percent_ends <- sum(match_ends$V1)/nrow(match_ends)
  
  percent_coord_dentro[[i]] <- data.frame(
    SETOR = setor,
    N_COORD_END_TOTAL = n_coordenadas_ends,
    N_COORD_END_DENTRO = sum(match_ends$V1),
    PERCENT_END_DENTRO = percent_ends,
    N_COORD_QUEST_TOTAL = n_coordenadas_quests,
    N_COORD_QUEST_DENTRO = n_coord_quest_dentro,
    PERCENT_QUEST_DENTRO = percent_quests
  )
  
  # saving a ggplot chart
  
  if (percent_ends < 0.9 & percent_ends != 'NaN') {
    
    g <- ggplot() +
      geom_sf(data = entire_buf, fill = NA) +
      geom_sf(data = poligono_crs, fill = NA) +
      geom_sf(data = pontos_ends, fill = NA, color = "red") +
      ggtitle(setor) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      if (exists("pontos_quests")) geom_sf(data = pontos_quests, fill = NA, color = "purple")
    
    ggsave(plot = g, paste0("maps/", setor, ".png"))
    
  }
  
  rm(pontos_quests)
  
}

# transforming to data.frame

df_final <- do.call(rbind, percent_coord_dentro)

df_final$SETOR <- as.numeric(df_final$SETOR)

dados <- dados_estrutura

dados$SETOR <- dados$SETOR |> as.numeric()

df_final_estrutura <- left_join(df_final,
                                dados,
                                by = "SETOR")

df_final_estrutura_ <- df_final_estrutura[,c(8:16,1:7)]

df_final_estrutura_ <- df_final_estrutura_ |>
  filter(PERCENT_END_DENTRO != 1 | PERCENT_QUEST_DENTRO != 1)

# saving to xlsx

writexl::write_xlsx(df_final_estrutura_, paste0("invasoes_end_quest_",
                                                format(Sys.time(), "%d-%m-%y %H-%M-%S"), ".xlsx"))







