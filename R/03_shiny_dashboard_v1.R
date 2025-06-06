require(shiny)
require(bs4Dash)
require(dplyr)
require(readxl)
require(plotly)
require(leaflet)
require(DT)
require(scales)
require(tidyr)
require(hrbrthemes)
require(gt)
require(readr)
require(glue)
require(forcats)


source('02_functions_v1.R')


base_principal <- read.csv('data/base_principal.csv')

calculo_br(base_principal)
base_filtrada_br <- filtrar_dados(base_principal)

estados <- c(
  "Acre" = "acre",
  "Alagoas" = "alagoas",
  "Amapá" = "amapa",
  "Amazonas" = "amazonas",
  "Bahia" = "bahia",
  "Ceará" = "ceara",
  "Distrito Federal" = "distritofederal",
  "Espírito Santo" = "espiritosanto",
  "Goiás" = "goias",
  "Maranhão" = "maranhao",
  "Mato Grosso" = "matogrosso",
  "Mato Grosso do Sul" = "matogrossodosul",
  "Minas Gerais" = "minasgerais",
  "Pará" = "para",
  "Paraíba" = "paraiba",
  "Paraná" = "parana",
  "Pernambuco" = "pernambuco",
  "Piauí" = "piaui",
  "Rio de Janeiro" = "riodejaneiro",
  "Rio Grande do Norte" = "riograndedonorte",
  "Rio Grande do Sul" = "riograndedosul",
  "Rondônia" = "rondonia",
  "Roraima" = "roraima",
  "Santa Catarina" = "santacatarina",
  "São Paulo" = "saopaulo",
  "Sergipe" = "sergipe",
  "Tocantins" = "tocantins"
)



siglas <- unique(base_principal$uf)
lapply(siglas, function(uf){
  calculo_estado(base_principal, uf)
})

  
ui <- dashboardPage(
  dark = NULL,
  help = NULL, 
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  title = "ONSV - Cumprimento das Metas",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "ONSV",
      image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTiUGcruSnnUaYj84CofuRj9oRE1ZX7K-JpcQ&s"
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Início", tabName = "inicio", icon = icon("home")),
      menuItem("Brasil", tabName = "brasil", icon = icon("globe")),
      lapply(names(estados), function(nome_estado) {
        menuItem(nome_estado, tabName = estados[[nome_estado]], icon = icon("map-marker-alt"))
      })
    )
  ),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        .jumbotron {
          background-color: #c0c0c0 !important;
          color: #000000 !important;
        }
        .jumbotron .btn {
          background-color: #555555 !important;
          color: white !important;
          border: none !important;
        }
      "))
    ),
    tags$head(
      tags$style(HTML("
        .nav-pills .nav-link.active,
        .nav-pills .show>.nav-link {
          background-color: #007bff !important;
          color: white !important;
        }
        
        .nav-sidebar .nav-item>.nav-link.active {
          background-color: #FFCC00 !important;
          color: white !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        
        tabName = "inicio",
        jumbotron(
          title = "Análise do Cumprimento das Metas de Redução de Mortes no Trânsito.",
          status = "primary",
          lead = "Visualização e análise do cumprimento das metas de redução de mortes no trânsito até 2023, estabelecidas para os municípios brasileiros pelo PNATRANS em 2021.",
          btnName = "Relatório",
          href = "https://betrizmarques.github.io/cumprimento-metas/",
          "Você pode verificar o relatório deste DashBoard, acessando o link abaixo:"
         
        ), 
        
        fluidRow(
          
          userBox(
            collapsible = T,
            title = userDescription(
              title = "Observatório Nacional de Segurança Viária",
              subtitle = "",
              image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTiUGcruSnnUaYj84CofuRj9oRE1ZX7K-JpcQ&s",
              type = 1
            ), 
            status = "warning",
            HTML("<br>"),
            "O Observatório Nacional de Segurança Viária é uma instituição social
            sem fins lucrativos, dedicada a desenvolver ações que contribuam efetivamente
            para a redução dos elevados índices de ocorrências no trânsito brasileiro. Com 
            esse objetivo, um grupo de profissionais multidisciplinares decidiu reunir todo 
            o seu conhecimento, experiência e motivação em um único projeto grandioso e desafiador: 
            mobilizar a sociedade em prol de um trânsito mais seguro."

          ),
          box(title = 'Introdução',
              width = 6,
              collapsible = T,
              blockQuote(
                
              
                
                "O Plano Nacional de Redução de Mortes e Lesões no Trânsito (PNATRANS), criado pela Lei Federal 
                nº 13.614/2018, tem como objetivo estabelecer metas de redução da mortalidade no trânsito para os 
                estados e para o país entre 2019 e 2028. Em 2021, o plano foi revisado, prorrogando o prazo das metas 
                até 2030 e reformulando seus pilares de atuação. As metas de redução foram definidas para os municípios
                brasileiros através das técnicas de clusterização e benchmarking. O objetivo, agora, é observar o desempenho
                de cada município até 2023, atráves da visualização de dados. Para isso, foi criado um Dashboard que contém 
                todas as informações necessárias para essa análise.",
                
                
                color = "warning"
              )
              
          )
        )
      ),
      gerar_tab_brasil(),
      gerar_tab_estado("acre", mortes_antigo_ac, mortes_23_ac, atingiram_meta_ac,
                       n_municipios_ac, meta_reducao_ac, reducao_23_ac,
                       "scatterplot_ac", "barplot_ac", 'tabela_ac', "capitais_ac"),
      gerar_tab_estado("alagoas", mortes_antigo_al, mortes_23_al, atingiram_meta_al,
                       n_municipios_al, meta_reducao_al, reducao_23_al,
                       "scatterplot_al", "barplot_al", 'tabela_al', "capitais_al"),
      gerar_tab_estado("amapa", mortes_antigo_ap, mortes_23_ap, atingiram_meta_ap,
                       n_municipios_ap, meta_reducao_ap, reducao_23_ap,
                       "scatterplot_ap", "barplot_ap", 'tabela_ap', "capitais_ap"),
      gerar_tab_estado("amazonas", mortes_antigo_am, mortes_23_am, atingiram_meta_am,
                       n_municipios_am, meta_reducao_am, reducao_23_am,
                       "scatterplot_am", "barplot_am", 'tabela_am', "capitais_am"),
      gerar_tab_estado("bahia", mortes_antigo_ba, mortes_23_ba, atingiram_meta_ba,
                       n_municipios_ba, meta_reducao_ba, reducao_23_ba,
                       "scatterplot_ba", "barplot_ba", 'tabela_ba', "capitais_ba"),
      gerar_tab_estado("ceara", mortes_antigo_ce, mortes_23_ce, atingiram_meta_ce,
                       n_municipios_ce, meta_reducao_ce, reducao_23_ce,
                       "scatterplot_ce", "barplot_ce", 'tabela_ce', "capitais_ce"),
      gerar_tab_estado("distritofederal", mortes_antigo_df, mortes_23_df, atingiram_meta_df,
                       n_municipios_df, meta_reducao_df, reducao_23_df,
                       "scatterplot_df", "barplot_df", 'tabela_df', "capitais_df"),
      gerar_tab_estado("espiritosanto", mortes_antigo_es, mortes_23_es, atingiram_meta_es,
                       n_municipios_es, meta_reducao_es, reducao_23_es,
                       "scatterplot_es", "barplot_es", 'tabela_es', "capitais_es"),
      gerar_tab_estado("goias", mortes_antigo_go, mortes_23_go, atingiram_meta_go,
                       n_municipios_go, meta_reducao_go, reducao_23_go,
                       "scatterplot_go", "barplot_go", 'tabela_go', "capitais_go"),
      gerar_tab_estado("maranhao", mortes_antigo_ma, mortes_23_ma, atingiram_meta_ma,
                       n_municipios_ma, meta_reducao_ma, reducao_23_ma,
                       "scatterplot_ma", "barplot_ma", 'tabela_ma', "capitais_ma"),
      gerar_tab_estado("matogrosso", mortes_antigo_mt, mortes_23_mt, atingiram_meta_mt,
                       n_municipios_mt, meta_reducao_mt, reducao_23_mt,
                       "scatterplot_mt", "barplot_mt", 'tabela_mt', "capitais_mt"),
      gerar_tab_estado("matogrossodosul", mortes_antigo_ms, mortes_23_ms, atingiram_meta_ms,
                       n_municipios_ms, meta_reducao_ms, reducao_23_ms,
                       "scatterplot_ms", "barplot_ms", 'tabela_ms', "capitais_ms"),
      gerar_tab_estado("minasgerais", mortes_antigo_mg, mortes_23_mg, atingiram_meta_mg,
                       n_municipios_mg, meta_reducao_mg, reducao_23_mg,
                       "scatterplot_mg", "barplot_mg", 'tabela_mg', "capitais_mg"),
      gerar_tab_estado("para", mortes_antigo_pa, mortes_23_pa, atingiram_meta_pa,
                       n_municipios_pa, meta_reducao_pa, reducao_23_pa,
                       "scatterplot_pa", "barplot_pa", 'tabela_pa', "capitais_pa"),
      gerar_tab_estado("paraiba", mortes_antigo_pb, mortes_23_pb, atingiram_meta_pb,
                       n_municipios_pb, meta_reducao_pb, reducao_23_pb,
                       "scatterplot_pb", "barplot_pb", 'tabela_pb', "capitais_pb"),
      gerar_tab_estado("parana", mortes_antigo_pr, mortes_23_pr, atingiram_meta_pr,
                       n_municipios_pr, meta_reducao_pr, reducao_23_pr,
                       "scatterplot_pr", "barplot_pr", 'tabela_pr', "capitais_pr"),
      gerar_tab_estado("pernambuco", mortes_antigo_pe, mortes_23_pe, atingiram_meta_pe,
                       n_municipios_pe, meta_reducao_pe, reducao_23_pe,
                       "scatterplot_pe", "barplot_pe", 'tabela_pe', "capitais_pe"),
      gerar_tab_estado("piaui", mortes_antigo_pi, mortes_23_pi, atingiram_meta_pi,
                       n_municipios_pi, meta_reducao_pi, reducao_23_pi,
                       "scatterplot_pi", "barplot_pi", 'tabela_pi', "capitais_pi"),
      gerar_tab_estado("riodejaneiro", mortes_antigo_rj, mortes_23_rj, atingiram_meta_rj,
                       n_municipios_rj, meta_reducao_rj, reducao_23_rj,
                       "scatterplot_rj", "barplot_rj", 'tabela_rj', "capitais_rj"),
      gerar_tab_estado("riograndedonorte", mortes_antigo_rn, mortes_23_rn, atingiram_meta_rn,
                       n_municipios_rn, meta_reducao_rn, reducao_23_rn,
                       "scatterplot_rn", "barplot_rn", 'tabela_rn', "capitais_rn"),
      gerar_tab_estado("riograndedosul", mortes_antigo_rs, mortes_23_rs, atingiram_meta_rs,
                       n_municipios_rs, meta_reducao_rs, reducao_23_rs,
                       "scatterplot_rs", "barplot_rs", 'tabela_rs', "capitais_rs"),
      gerar_tab_estado("rondonia", mortes_antigo_ro, mortes_23_ro, atingiram_meta_ro,
                       n_municipios_ro, meta_reducao_ro, reducao_23_ro,
                       "scatterplot_ro", "barplot_ro", 'tabela_ro', "capitais_ro"),
      gerar_tab_estado("roraima", mortes_antigo_rr, mortes_23_rr, atingiram_meta_rr,
                       n_municipios_rr, meta_reducao_rr, reducao_23_rr,
                       "scatterplot_rr", "barplot_rr", 'tabela_rr', "capitais_rr"),
      gerar_tab_estado("santacatarina", mortes_antigo_sc, mortes_23_sc, atingiram_meta_sc,
                       n_municipios_sc, meta_reducao_sc, reducao_23_sc,
                       "scatterplot_sc", "barplot_sc", 'tabela_sc', "capitais_sc"),
      gerar_tab_estado("saopaulo", mortes_antigo_sp, mortes_23_sp, atingiram_meta_sp,
                       n_municipios_sp, meta_reducao_sp, reducao_23_sp,
                       "scatterplot_sp", "barplot_sp", 'tabela_sp', "capitais_sp"),
      gerar_tab_estado("sergipe", mortes_antigo_se, mortes_23_se, atingiram_meta_se,
                       n_municipios_se, meta_reducao_se, reducao_23_se,
                       "scatterplot_se", "barplot_se", 'tabela_se', "capitais_se"),
      gerar_tab_estado("tocantins", mortes_antigo_to, mortes_23_to, atingiram_meta_to,
                       n_municipios_to, meta_reducao_to, reducao_23_to,
                       "scatterplot_to", "barplot_to", 'tabela_to', "capitais_to")
      
    )
  )
)


server <- function(input, output) {
  mortes_transito_mapa_estados <- base_principal %>% 
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
    "Mortes no Trânsito 2023" = "num_mortes",
    "Mortes no Trânsito (2018-2020)" = "num_mortes_antigo",
    "Redução" = "reducao_estado",
    "Meta Atingida"= "meta_atingida",
    "Meta de Redução" = "meta_reducao"
  )
  
  output$mapa_brasil <- renderLeaflet({
    
    
   
    variavel_padrao <- "num_mortes" 
    dados_padrao <- estados_com_dados_mortes[[variavel_padrao]]
    titulo_padrao <- names(opcoes_filtro)[opcoes_filtro == variavel_padrao]
    
    paleta_padrao <- colorNumeric(
      palette = "YlOrRd",
      domain = dados_padrao
    )
    
    leaflet(estados_com_dados_mortes) %>%
     
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap Padrão") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite Esri") %>%
      setView(lng = -55, lat = -15, zoom = 4) %>%
      
  
      addPolygons(
        fillColor = ~paleta_padrao(dados_padrao),
        weight = 1.5, opacity = 1, color = "grey", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 4, color = "#444", fillOpacity = 0.9, bringToFront = TRUE),
        popup = ~paste0(
          "<strong>Estado: </strong>", name_state, "<br>",
          "<strong>Total de Mortes (2023): </strong>", format(num_mortes, big.mark = "."), "<br>",
          "<strong>Redução de Mortes: </strong>", round(reducao_estado, 2), "%<br>",
          "<strong>% da Meta Atingida: </strong>", round(meta_atingida, 2), "%"
        )
      ) %>%
      addLegend(
        pal = paleta_padrao,
        values = ~dados_padrao,
        opacity = 0.7,
        title = titulo_padrao,
        position = "bottomleft",
        labFormat = labelFormat(big.mark = ".")
      ) %>%
      addLayersControl(
       
        baseGroups = c("CartoDB Positron", "OpenStreetMap Padrão", "Satélite Esri"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft"
      )
  })
O
  observe({
    variavel <- input$variavel_mapa
    dados_da_variavel <- estados_com_dados_mortes[[variavel]]
    titulo_legenda <- names(opcoes_filtro)[opcoes_filtro == variavel]
    
    if (variavel %in% c("num_mortes", "num_mortes_antigo")) {
      paleta_cores <- colorNumeric(palette = "YlOrRd", domain = dados_da_variavel)
      formato_legenda <- labelFormat(big.mark = ".")
    } else if (variavel == "meta_reducao") {

      paleta_cores <- colorNumeric(
        palette = "YlOrRd", 
        domain = dados_da_variavel,
        reverse = TRUE 
      )
      formato_legenda <- labelFormat(suffix = " %", digits = 2)
      
    } else if (variavel == "reducao_estado") {
      paleta_cores <- colorNumeric(palette = "YlOrRd", domain = dados_da_variavel)
      formato_legenda <- labelFormat(suffix = " %", digits = 2)
    } else if (variavel == "meta_atingida") {
      paleta_cores <- colorNumeric(palette = "YlOrRd", domain = dados_da_variavel)
      formato_legenda <- labelFormat(suffix = " %", digits = 2)
    }
    
    leafletProxy("mapa_brasil", data = estados_com_dados_mortes) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~paleta_cores(dados_da_variavel),
        weight = 1.5,
        opacity = 1,
        color = "grey",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 4, color = "#444", fillOpacity = 0.9, bringToFront = TRUE
        ),
        popup = ~paste0(
          "<strong>Estado: </strong>", name_state, "<br>",
          "<strong>Total de Mortes (2023): </strong>", format(num_mortes, big.mark = "."), "<br>",
          "<strong>Redução de Mortes: </strong>", round(reducao_estado, 2), "%<br>",
          "<strong>% da Meta Atingida: </strong>", round(meta_atingida, 2), "%"
        )
      ) %>%
      addLegend(
        pal = paleta_cores,
        values = ~dados_da_variavel,
        opacity = 0.7,
        title = titulo_legenda,
        position = "bottomleft",
        labFormat = formato_legenda
      ) %>%
      addLayersControl(
        baseGroups = c("CartoDB Positron", "OpenStreetMap Padrão", "Satélite Esri"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft"
      )
  }) 
  
  output$barplot_br <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "")
  })
  
  
  output$scatterplot_br <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_principal))
  })

  
  output$tabela_br <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_principal))
  })

  
info_estados <- data.frame(
  sufixo <- c("ac", "al", "ap", "am", "ba", "ce", "df", "es", "go", "ma",
              "mt", "ms", "mg", "pa", "pb", "pr", "pe", "pi", "rj", "rn",
              "rs", "ro", "rr", "sc", "sp", "se", "to"),
  nome_completo <- c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará",
                     "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão",
                     "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará",
                     "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro",
                     "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia",
                     "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"),
  stringsAsFactors = FALSE
)

lapply(1:nrow(info_estados), function(i){
  estado_info <- info_estados[i,]
  nome_df_filtrado <- paste0("base_filtrada_", estado_info$sufixo)
  

  output[[paste0("scatterplot_", estado_info$sufixo)]] <- renderPlotly({

    base_filtrada_atual <- get(nome_df_filtrado)
    grafico_meta_atingida(base_graficos(base_filtrada_atual))
  })
  

  output[[paste0("barplot_", estado_info$sufixo)]] <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = estado_info$nome_completo)
  })
  

  output[[paste0("capitais_", estado_info$sufixo)]] <- renderPlotly({
    plot_capitais(base_principal, destaque = estado_info$nome_completo)
  })
  
 
  output[[paste0("tabela_", estado_info$sufixo)]] <- renderDT({
    base_filtrada_atual <- get(nome_df_filtrado)
    tabela_reducao_aumento(filtrar_dados(base_filtrada_atual))
  })
  
})
  }

shinyApp(ui, server)