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
      gerar_tab_estado("brasil", mortes_antigo_br, mortes_23_br, atingiram_meta_br,
                       n_municipios_br, meta_reducao_br, reducao_23_br,
                       "scatterplot_br", "barplot_br", 'tabela_br', "capitais_br"),
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
  
  output$scatterplot_br <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_principal))
  })
  
  output$barplot_br <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "")
  })
  
  
  output$capitais_br <- renderPlotly({
    plot_capitais(base_principal, destaque =  "")
    
  })
  
  output$tabela_br <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_principal))
  })
  ##
  output$scatterplot_ac <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ac))
  })
  
  output$barplot_ac <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Acre")
  })
  
  
  output$capitais_ac <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Acre")
   
  })

  output$tabela_ac <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ac))
  })
  ##
  output$scatterplot_al <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_al))
  })
  
  output$barplot_al <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Alagoas")
  })
  
  
  output$capitais_al <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Alagoas")
    
  })
  
  output$tabela_al <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_al))
  })
  ##
  output$scatterplot_ap <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ap))
  })
  
  output$barplot_ap <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Amapá")
  })
  
  
  output$capitais_ap <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Amapá")
    
  })
  
  output$tabela_ap <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ap))
  })
##
  
  output$scatterplot_am <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_am))
  })
  
  output$barplot_am <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Amazonas")
  })
  
  
  output$capitais_am <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Amazonas")
    
  })
  
  output$tabela_am <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_am))
  })
##
  
  output$scatterplot_ba <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ba))
  })
  
  output$barplot_ba <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Bahia")
  })
  
  
  output$capitais_ba <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Bahia")
    
  })
  
  output$tabela_ba <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ba))
  })
  
  ## CE
  
  output$scatterplot_ce <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ce))
  })
  
  output$barplot_ce <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Ceará")
  })
  
  
  output$capitais_ce <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Ceará")
    
  })
  
  output$tabela_ce <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ce))
  })
  
  ##
  
  output$scatterplot_df <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_df))
  })
  
  output$barplot_df <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Distrito Federal")
  })
  
  
  output$capitais_df <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Distrito Federal")
    
  })
  
  output$tabela_df <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_df))
  })
  
  ##
  
  output$scatterplot_es <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_es))
  })
  
  output$barplot_es <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Espírito Santo")
  })
  
  
  output$capitais_es <- renderPlotly({
    
    plot_capitais(base_principal, destaque =  "Espírito Santo")
  })
  
  output$tabela_es <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_es))
  })
  
  output$scatterplot_go <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_go))
  })
  
  output$barplot_go <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Goiás")
  })
  
  
  output$capitais_go <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Goiás")
  })
  
  output$tabela_go <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_go))
  })
  
  ##
  
  output$scatterplot_ma <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ma))
  })
  
  output$barplot_go <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Goiás")
  })
  
  
  output$capitais_go <- renderPlotly({
    plot_capitais(base_principal, destaque =  "Goiás")
  })
  
  output$tabela_go <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_go))
  })
  # Goiás
  output$scatterplot_go <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_go))
  })
  output$barplot_go <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Goiás")
  })
  output$capitais_go <- renderPlotly({
    plot_capitais(base_principal, destaque = "Goiás")
  })
  output$tabela_go <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_go))
  })
  
  # Maranhão
  output$scatterplot_ma <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ma))
  })
  output$barplot_ma <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Maranhão")
  })
  output$capitais_ma <- renderPlotly({
    plot_capitais(base_principal, destaque = "Maranhão")
  })
  output$tabela_ma <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ma))
  })
  
  # Mato Grosso
  output$scatterplot_mt <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_mt))
  })
  output$barplot_mt <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Mato Grosso")
  })
  output$capitais_mt <- renderPlotly({
    plot_capitais(base_principal, destaque = "Mato Grosso")
  })
  output$tabela_mt <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_mt))
  })
  
  # Mato Grosso do Sul
  output$scatterplot_ms <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ms))
  })
  output$barplot_ms <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Mato Grosso do Sul")
  })
  output$capitais_ms <- renderPlotly({
    plot_capitais(base_principal, destaque = "Mato Grosso do Sul")
  })
  output$tabela_ms <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ms))
  })
  
  # Minas Gerais
  output$scatterplot_mg <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_mg))
  })
  output$barplot_mg <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Minas Gerais")
  })
  output$capitais_mg <- renderPlotly({
    plot_capitais(base_principal, destaque = "Minas Gerais")
  })
  output$tabela_mg <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_mg))
  })
  
  # Pará
  output$scatterplot_pa <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_pa))
  })
  output$barplot_pa <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Pará")
  })
  output$capitais_pa <- renderPlotly({
    plot_capitais(base_principal, destaque = "Pará")
  })
  output$tabela_pa <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_pa))
  })
  
  # Paraíba
  output$scatterplot_pb <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_pb))
  })
  output$barplot_pb <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Paraíba")
  })
  output$capitais_pb <- renderPlotly({
    plot_capitais(base_principal, destaque = "Paraíba")
  })
  output$tabela_pb <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_pb))
  })
  
  # Paraná
  output$scatterplot_pr <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_pr))
  })
  output$barplot_pr <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Paraná")
  })
  output$capitais_pr <- renderPlotly({
    plot_capitais(base_principal, destaque = "Paraná")
  })
  output$tabela_pr <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_pr))
  })
  
  # Pernambuco
  output$scatterplot_pe <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_pe))
  })
  output$barplot_pe <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Pernambuco")
  })
  output$capitais_pe <- renderPlotly({
    plot_capitais(base_principal, destaque = "Pernambuco")
  })
  output$tabela_pe <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_pe))
  })
  
  # Piauí
  output$scatterplot_pi <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_pi))
  })
  output$barplot_pi <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Piauí")
  })
  output$capitais_pi <- renderPlotly({
    plot_capitais(base_principal, destaque = "Piauí")
  })
  output$tabela_pi <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_pi))
  })
  
  # Rio de Janeiro
  output$scatterplot_rj <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_rj))
  })
  output$barplot_rj <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Rio de Janeiro")
  })
  output$capitais_rj <- renderPlotly({
    plot_capitais(base_principal, destaque = "Rio de Janeiro")
  })
  output$tabela_rj <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_rj))
  })
  
  #Rio Grande do Norte
  output$scatterplot_rn <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_rn))
  })
  output$barplot_rn <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Rio Grande do Norte")
  })
  output$capitais_rn <- renderPlotly({
    plot_capitais(base_principal, destaque = "Rio Grande do Norte")
  })
  output$tabela_rn <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_rn))
  })
  
  #Rio Grande do Sul
  output$scatterplot_rs <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_rs))
  })
  output$barplot_rs <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Rio Grande do Sul")
  })
  output$capitais_rs <- renderPlotly({
    plot_capitais(base_principal, destaque = "Rio Grande do Sul")
  })
  output$tabela_rs <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_rs))
  })
  
  # Rondônia
  output$scatterplot_ro <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_ro))
  })
  output$barplot_ro <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Rondônia")
  })
  output$capitais_ro <- renderPlotly({
    plot_capitais(base_principal, destaque = "Rondônia")
  })
  output$tabela_ro <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_ro))
  })
  
  #Roraima
  output$scatterplot_rr <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_rr))
  })
  output$barplot_rr <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Roraima")
  })
  output$capitais_rr <- renderPlotly({
    plot_capitais(base_principal, destaque = "Roraima")
  })
  output$tabela_rr <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_rr))
  })
  
  # Santa Catarina
  output$scatterplot_sc <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_sc))
  })
  output$barplot_sc <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Santa Catarina")
  })
  output$capitais_sc <- renderPlotly({
    plot_capitais(base_principal, destaque = "Santa Catarina")
  })
  output$tabela_sc <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_sc))
  })
  
  #São Paulo
  output$scatterplot_sp <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_sp))
  })
  output$barplot_sp <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "São Paulo")
  })
  output$capitais_sp <- renderPlotly({
    plot_capitais(base_principal, destaque = "São Paulo")
  })
  output$tabela_sp <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_sp))
  })
  
  # Sergipe
  output$scatterplot_se <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_se))
  })
  output$barplot_se <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Sergipe")
  })
  output$capitais_se <- renderPlotly({
    plot_capitais(base_principal, destaque = "Sergipe")
  })
  output$tabela_se <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_se))
  })
  
  #Tocantins
  output$scatterplot_to <- renderPlotly({
    grafico_meta_atingida(base_graficos(base_filtrada_to))
  })
  output$barplot_to <- renderPlotly({
    plot_reducao_estados(base_principal, destaque = "Tocantins")
  })
  output$capitais_to <- renderPlotly({
    plot_capitais(base_principal, destaque = "Tocantins")
  })
  output$tabela_to <- renderDT({
    tabela_reducao_aumento(filtrar_dados(base_filtrada_to))
  })
  
}

shinyApp(ui, server)