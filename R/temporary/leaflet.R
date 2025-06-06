library(leaflet)
library(sf)
library(geobr)
library(tidyverse)


mortes_transito_mapa_estados <- read.csv(file = 'R/data/base_principal.csv') %>% 
  group_by(uf) %>% 
  summarise(num_mortes = sum(n_mortes_23),
            num_mortes_antigo = sum(media_mortes),
            meta_reducao = (sum(meta) - sum(media_mortes))/sum(media_mortes)*100,
            reducao_estado = (sum(n_mortes_23)-sum(media_mortes))/sum(media_mortes)*100,
            meta_atingida = reducao_estado/meta_reducao*100
            ) %>% 
  ungroup() %>% 
  rename(abbrev_state = uf) 

todos_os_estados_sf <- read_state(code_state = "all", year = 2020)

estados_com_dados_mortes <- todos_os_estados_sf %>% 
  left_join(mortes_transito_mapa_estados, by = 'abbrev_state')


opcoes_filtro <- c(
  "Número de Mortes no Trânsito em 2023" = "num_mortes",
  "Média de Mortes no Trânsito (2018-2020)" = "num_mortes_antigo",
  "Meta de Redução" = "meta_reducao",
  "Redução" = "reducao_estado",
  "Meta Atingida"= "meta_atingida"
)

paleta_cores_mortes <- colorNumeric(
  palette = "YlOrRd",
  domain = estados_com_dados_mortes$num_mortes
)


mapa_mortes_transito <- leaflet(estados_com_dados_mortes) %>% 
  addTiles(group = "OpenStreetMap Padrão") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite Esri") %>% 
  setView(lng = -55, lat = -15, zoom = 4) %>% 
  addPolygons(
    fillColor = ~paleta_cores_mortes(num_mortes), 
    weight = 1.5,              
    opacity = 1,               
    color = "grey",            
    fillOpacity = 0.8,         
    highlightOptions = highlightOptions( 
      weight = 4,
      color = "#444",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste(name_state, ": ", format(num_mortes, big.mark = ".", decimal.mark = ","), " mortes"), # Etiqueta ao passar o mouse
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    ),
    popup = ~paste0( 
      "<strong>Estado: </strong>", name_state, "<br>",
      "<strong>Sigla: </strong>", abbrev_state, "<br>",
      "<strong>Região: </strong>", name_region, "<br>",
      "<strong>Meta de Redução: </strong>", round(meta_reducao, 2), "%<br>",
      "<strong>Total de Mortes Registradas: </strong>", format(num_mortes, big.mark = ".", decimal.mark = ",")
    )
  ) %>%
  addLegend(
    pal = paleta_cores_mortes,
    values = ~num_mortes,
    opacity = 0.7,
    title = "Número de Mortes<br>no Trânsito em 2023", 
    position = "bottomright", 
    labFormat = labelFormat(big.mark = ".") 
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap Padrão", "CartoDB Positron", "Satélite Esri"),
    options = layersControlOptions(collapsed = TRUE) 
  )

mapa_mortes_transito



################################################################################

sigla_estado <- 'SP'
mortes_transito_mapa_municipios <- read.csv(file = 'R/data/base_principal.csv') %>% 
  mutate(municipio_normal = iconv(toupper(nome_do_municipio), from = "UTF-8", to = "ASCII//TRANSLIT"))

municipios_do_estado_sf <- read_municipality(code_muni = sigla_estado, year = 2020) %>%
  mutate(municipio_normal = iconv(toupper(name_muni), from = "UTF-8", to = "ASCII//TRANSLIT"))

municipios_com_dados_mortes_nome <- municipios_do_estado_sf %>%
  left_join(mortes_transito_mapa_municipios, by = 'municipio_normal')


paleta_cores_mortes_municipios_nome <- colorNumeric(
  palette = "YlOrBr",
  domain = municipios_com_dados_mortes_nome$n_mortes_23,
  na.color = "#E0E0E0" 
)

info_estado_sf <- read_state(code_state = sigla_estado, year = 2020)
bounds_estado <- st_bbox(info_estado_sf)


mapa_mortes_municipios_nome <- leaflet(data = municipios_com_dados_mortes_nome) %>%
  addTiles(group = "OpenStreetMap Padrão") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
  fitBounds(lng1 = bounds_estado["xmin"],
            lat1 = bounds_estado["ymin"],
            lng2 = bounds_estado["xmax"],
            lat2 = bounds_estado["ymax"]) %>%
  addPolygons(
    fillColor = ~paleta_cores_mortes_municipios_nome(n_mortes_23),
    weight = 0.8,
    opacity = 1,
    color = "grey",
    fillOpacity = 0.8,
    highlightOptions = highlightOptions(
      weight = 2.5,
      color = "#333",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste(name_muni, "(", municipio_normal, "): ", format(n_mortes_23, big.mark = ".", decimal.mark = ","), " mortes"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto"
    ),
    popup = ~paste0(
      "<strong>Total de Mortes Registradas: </strong>",
      format(n_mortes_23, big.mark = ".", decimal.mark = ",", nsmall = 0) # nsmall=0 para inteiros
    )
  ) %>%
  addLegend(
    pal = paleta_cores_mortes_municipios_nome,
    values = ~n_mortes_23,
    opacity = 0.7,
    title = paste("Mortes no Trânsito<br>Municípios de", sigla_estado, "<br>(Junção por Nome)"),
    position = "bottomright",
    labFormat = labelFormat(big.mark = ".",
                            decimal.mark = ",",
                            digits = 0 )
    
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap Padrão", "CartoDB Positron"),
    options = layersControlOptions(collapsed = TRUE)
  )

mapa_mortes_municipios_nome
