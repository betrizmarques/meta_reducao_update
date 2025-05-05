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
source('C:/Users/absil/OneDrive/Documents/UFPR/ONSV/METAS MUNICÍPIOS/meta_reducao_update/R/02_functions_v1.R')

base_principal <- read.csv('C:/Users/absil/OneDrive/Documents/UFPR/ONSV/METAS MUNICÍPIOS/meta_reducao_update/data/base_principal.csv')
calculo_br(base_principal)
dados_filtrados(base_principal)

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


ui <- dashboardPage(
  help = NULL, 
  fullscreen = TRUE,
  
  title = "Cumprimento das Metas de Redução de Mortes no Trânsito dos Municípios Brasileiros",
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
        .nav-pills .nav-link.active,
        .nav-pills .show>.nav-link {
          background-color: #007bff !important;
          color: white !important;
        }
        
        .nav-sidebar .nav-item>.nav-link.active {
          background-color: #3c9c74 !important;
          color: white !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "inicio",
        
        jumbotron(
          title = "Metas de Redução de Mortes no Trânsito dos Municípios Brasileiros",
          status = "info",
          lead = "Visualização e análise do avanço das metas estabelecidas para os municípios brasileiros em 2020",
          btnName = "Download",
          href = "https://github.com/pabsantos/roadtrafficdeaths",
          "Você pode fazer o download da base de dados, acessando o link abaixo:"
        ), 
        
        fluidRow(
          
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Observatório Nacional de Segurança Viária",
              image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTiUGcruSnnUaYj84CofuRj9oRE1ZX7K-JpcQ&s",
              type = 1
            ), 
            status = "olive"
          ),
          box(title = 'Introdução',
              width = 6,
              collapsible = FALSE,
              blockQuote(
                "O OBSERVATÓRIO Nacional de Segurança Viária é uma instituição social sem fins lucrativos,
                dedicada a desenvolver ações que contribuam efetivamente para a redução dos elevados índices 
                de vítimas no trânsito brasileiro. Com esse objetivo, um grupo de profissionais multidisciplinares 
                decidiu reunir todo o seu conhecimento, experiência e motivação em um único projeto grandioso e desafiador:
                mobilizar a sociedade em prol de um trânsito mais seguro.",
                color = "olive"
              )
          )
        )
      )
    ),
    gerar_tab_estado("acre", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("alagoas", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("amazonas", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("bahia", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("ceara", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("distritofederal", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("espiritosanto", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("goias", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("maranhao", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("matogrosso", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("matogrossodosul", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("minasgerais", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("para", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("paraiba", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("parana", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("pernambuco", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("piaui", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("riodejaneiro", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("riograndedonorte", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("riograndedosul", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("rondonia", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("roraima", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("santacatarina", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("saopaulo", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("sergipe", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac"),
    gerar_tab_estado("tocantins", media_mortes_antigo_ac, media_mortes_23_ac, n_municipios_ac,
                     "g4_ac", "histograma_ac", "boxplot_ac", "infantil_ac")
  )
)


server <- function(input, output) {
  
  output$g4_sc <- renderPlotly({
    
    plot_ly(
      data = dados_sc,
      x = ~ `% de pessoas cobertas por planos de saúde suplementar`,
      y = ~`% de internações por condições sensíveis à atenção primária` ,
      text = ~Territorialidades, 
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text' 
    ) %>% 
      layout(
        xaxis = list(
          title = list(
            text = "% de Pessoas cobertas por planos de saúde suplementar",
            font = list(size = 12)  
          )
        ),
        yaxis = list(
          title = list(
            text = " % de Internações por condições sensíveis à atenção primária",
            font = list(size = 12)  
          )
        )
      )
  })
  
  output$g4_br <- renderPlotly({
    
    plot_ly(
      data = dados_brasil,
      x = ~ `% de pessoas cobertas por planos de saúde suplementar`,
      y = ~`% de internações por condições sensíveis à atenção primária` ,
      text = ~Territorialidades, 
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text' 
    ) %>% 
      layout(
        xaxis = list(
          title = list(
            text = "% de Pessoas cobertas por planos de saúde suplementar",
            font = list(size = 12)  
          )
        ),
        yaxis = list(
          title = list(
            text = " % de Internações por condições sensíveis à atenção primária",
            font = list(size = 12)  
          )
        )
      )
    
  })
  
  
  output$histograma_sc <- renderPlotly({
    plot_ly(
      data = dados_sc, 
      x = ~`% de internações por doenças relacionadas ao saneamento ambiental inadequado`, 
      type = 'histogram', 
      marker = list(color = '#3c9c74', line = list(color = '#3c9c74', width = 1))
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "% de internações por doenças relacionadas ao saneamento ambiental inadequado",
          titlefont = list(size = 12) 
        ),
        yaxis = list(
          title = "Frequência",
          titlefont = list(size = 12) 
        ),
        bargap = 0.2 
      )
  })
  
  
  
  output$bar_plot_brasil <- renderPlotly({
    p <- ggplot(dados_brasil) +
      aes(x = Estado, y = `Taxa bruta de mortalidade`, fill = Estado) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Distribuição da Taxa Bruta de Mortalidade por Estado")
    
    ggplotly(p, tooltip = "Estado")
    
  })
  
  
  
  output$histograma_brasil <- renderPlotly({
    plot_ly(
      data = dados_brasil, 
      x = ~`% de internações por doenças relacionadas ao saneamento ambiental inadequado`, 
      type = 'histogram', 
      marker = list(color = '#3c9c74', line = list(color = '#3c9c74', width = 1))
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "% de internações por doenças relacionadas ao saneamento ambiental inadequado",
          titlefont = list(size = 12) 
        ),
        yaxis = list(
          title = "Frequência",
          titlefont = list(size = 12) 
        ),
        bargap = 0.2 
      )
  })
  
  
  output$infantil_br <- renderPlotly({
    
    plot_ly(
      data = dados_brasil,
      x = ~ `% de nascidos vivos com pelo menos sete consultas de pré-natal`,
      y = ~ `% de nascidos vivos com baixo peso ao nascer`,
      text = ~Territorialidades,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = "% de nascidos vivos com pelo menos sete consultas de pré-natal", titlefont = list(size = 12)),
        yaxis = list(title = "% de nascidos vivos com baixo peso ao nascer", titlefont = list(size = 12))
      )
    
  })
  
  
  output$infantil_sc <- renderPlot({
    
    plot_capitas_sc(capitais_base_principal, destaque = 'Santa Catarina')
    
  })
  
  
  output$reducaoestado <- renderPlot({
    plot <- plot_reducao_estados(base_principal, destaque = 'Santa Catarina')
    plot
  })
  
  
  output$boxplot_sc <- render_gt({
    dados_ordenados%>% 
      gt() %>%
      cols_label(
        `Redução/Aumento` = html("<span style='color:#228B22'>Redução</span>/<span style='color:#B22222'>Aumento</span>")
      ) %>% 
      tab_style(
        style = list(cell_text(color = "#228B22")),
        locations = cells_body(
          columns = 'Redução/Aumento',
          rows = as.numeric(str_replace_all(`Redução/Aumento`, "%", "")) < 0
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#B22222")),
        locations = cells_body(
          columns = 'Redução/Aumento',
          rows = as.numeric(str_replace_all(`Redução/Aumento`, "%", "")) > 0
        )
      )
  })
}


shinyApp(ui, server)