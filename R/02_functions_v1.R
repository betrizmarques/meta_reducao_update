require(tidyverse)
base_principal <- read.csv('data/base_principal.csv')

#Função para gerar a aba do Dashboard de cada estado.
gerar_tab_estado <- function(nome_aba, media_antigo, media_23, atingiram_meta,
                             n_municipios, meta_reducao, reducao_23,
                             scatterplot, barras_estados, tabela, barras_capitais){
  tabItem(
    tabName = nome_aba,
    fluidRow(
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Média de Mortes no Trânsito (2018-2020)",
          value = round(media_antigo, digits = 2),
          icon = tags$i(class = "fas fa-car-crash", 
                        style = "background-color: white; color: red; padding: 12px; border-radius: 5px;"),
          color = "white"  
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Mortes no Trânsito (2023)",
          value = round(media_23, digits = 2),
          icon = tags$i(class = "fas fa-car-crash", 
                        style = "background-color: white; color: red; padding: 12px; border-radius: 5px;"),
          color = "white"  
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "% de Municípios que Atingiram a Meta",
          value = paste0(round(atingiram_meta*100, digits = 2), "%"),
          icon = icon("location-dot"),
          color = "white"
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Total de Municípios",
          value = n_municipios,
          icon = icon("location-dot"),
          color = "white"
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Meta de Redução de Mortes (%)",
          value = paste0(round(meta_reducao*100, digits = 2),"%"),
          icon = icon("chart-line"),
          color = "white"
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Redução / Aumento de Mortes (%)",
          value = paste0(round(reducao_23*100, digits = 2), "%"),
          icon = icon("chart-bar"),
          color = "white"
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        
        box(
          width = 12,
          title = "Metas de Redução por Estado",
          plotlyOutput(barras_estados)
        )
      ),
      column(
        width = 6,
        box(
          width = 12,
          title = "Redução por Capital",
          plotlyOutput(barras_capitais)
        )
      ),
      box(
        width = 12, 
        height = "600px", 
        title = "Gráfico de Dispersão: Meta de Redução x Meta Atingida",
        plotlyOutput(scatterplot, height = "550px")
      ),
      box(
        width = 12,
        title = "Tabela",
        div(
          style = "overflow-x: auto; max-height: 700px; width: 100%;",
          DTOutput(tabela)
        )
      )
    )
  )
}




atingiram_a_meta <- function(base){
  numero <- base %>% 
    filter(meta_atingida >= 100) %>% 
    nrow()
  return(numero)
}
atingiram_a_meta(base_principal)

#Calcular médias do Brasil.
calculo_br <- function(base) {
  assign('mortes_antigo_br', sum(base$media_mortes), envir = .GlobalEnv)
  assign("meta_mortes_br", sum(base$meta), envir = .GlobalEnv)
  assign("mortes_23_br", sum(base$n_mortes_23), envir = .GlobalEnv)
  assign("var_23_br", mortes_23_br - mortes_antigo_br, envir = .GlobalEnv)
  assign("reducao_23_br", var_23_br / mortes_antigo_br, envir = .GlobalEnv)
  assign("meta_reducao_br", (meta_mortes_br - mortes_antigo_br) / mortes_antigo_br, envir = .GlobalEnv)
  assign("n_municipios_br", nrow(base), envir = .GlobalEnv)
  assign("atingiram_meta_br", (atingiram_a_meta(base))/nrow(base), envir = .GlobalEnv)
}

calculo_br(base = base_principal)

filtrar_por_estado <- function(base, uf){
  estado <- base %>% 
    filter(uf == {{uf}})
  
  nome_variavel <- paste0("base_filtrada_", tolower(uf))
  assign(nome_variavel, estado, envir = .GlobalEnv)
  
}


#Calcular médias para cada estado.
calculo_estado <- function(base, estado){
  base_filtrada <- filtrar_por_estado({{base}}, estado)
  assign(paste0('mortes_antigo_', tolower(estado)), sum(base_filtrada$media_mortes), envir = .GlobalEnv)
  assign(paste0('meta_mortes_', tolower(estado)), sum(base_filtrada$meta), envir = .GlobalEnv)
  assign(paste0('mortes_23_', tolower(estado)), sum(base_filtrada$n_mortes_23), envir = .GlobalEnv)
  assign(paste0('var_23_', tolower(estado)), sum(base_filtrada$n_mortes_23) - sum(base_filtrada$media_mortes), envir = .GlobalEnv)
  assign(paste0('reducao_23_', tolower(estado)), (sum(base_filtrada$n_mortes_23) - sum(base_filtrada$media_mortes))/sum(base_filtrada$media_mortes), envir = .GlobalEnv)
  assign(paste0('meta_reducao_', tolower(estado)), (sum(base_filtrada$meta) - sum(base_filtrada$media_mortes))/ sum(base_filtrada$media_mortes), envir = .GlobalEnv)
  assign(paste0('n_municipios_', tolower(estado)), nrow(base_filtrada), envir = .GlobalEnv)
  assign(paste0('atingiram_meta_', tolower(estado)), atingiram_a_meta(base_filtrada)/nrow(base_filtrada), envir = .GlobalEnv)
}


#Filtrar dados para a tabela.
filtrar_dados <- function(base){
  dados_filtrados <- 
    base %>% 
    mutate(meta_percentual = paste0(round(var_perc*100, 2),'%'),
           reducao_percentual = paste0(round(reducao*100, 2),'%'),
           media_mortes = round(media_mortes, 2),
           cor = ifelse(meta_atingida >= 0, "red", "green"),
           atingiu_meta = ifelse(meta_atingida>=100, "Sim", "Não"),
           meta_atingida_limite = case_when(
             meta_atingida > 100 ~ 100,
             meta_atingida < -100 ~ -100,
             TRUE ~ meta_atingida
           ),
           meta_atingida_limite = paste0(round(meta_atingida_limite, 2),'%')) %>% 
    select(prioridade,nome_do_municipio, uf, media_mortes, meta_percentual, n_mortes_23, frota_23, populacao_23, reducao_percentual, meta_atingida_limite, atingiu_meta) %>% 
    rename('Prioridade' = prioridade,
           'Município' = nome_do_municipio,
           'Média de mortes (2018-2020)' = media_mortes,
           'UF'= uf,
           'Meta de Redução' = meta_percentual,
           'Número de mortes (2023)' = n_mortes_23,
           'Frota (2023)' = frota_23,
           'População (2023)' = populacao_23,
           'Redução/Aumento'= reducao_percentual,
           'Meta Atingida' = meta_atingida_limite)
  
  assign('dados_filtrados', dados_filtrados, envir = .GlobalEnv)
}




#Plot das
plot_capitais <- function(municipios, destaque = "SP") {
  label_br <- round(reducao_23_br * 100)
  
  capitais <- c(
    "Rio Branco", "Maceió", "Macapá", "Manaus", "Salvador",
    "Fortaleza", "Brasília", "Vitória", "Goiânia", "São Luís",
    "Cuiabá", "Campo Grande", "Belo Horizonte", "Belém", "João Pessoa",
    "Curitiba", "Recife", "Teresina", "Rio de Janeiro", "Natal",
    "Porto Alegre", "Porto Velho", "Boa Vista", "Florianópolis",
    "São Paulo", "Aracaju", "Palmas"
  )
  
  capitais_base_principal <- municipios %>% 
    filter(nome_do_municipio %in% capitais, !(X %in% c(2251, 2281, 1140, 3350, 190)))
  
  
  p <- capitais_base_principal %>%
    mutate(
      destaque_estado = ifelse(estado_nome == destaque, "Destaque", "Outros"),
      nome_do_municipio = fct_reorder(nome_do_municipio, reducao, .desc = TRUE)
    ) %>%
    ggplot(aes(
      x = nome_do_municipio,
      y = reducao,
      fill = destaque_estado,
      text = glue(
        "Município: {nome_do_municipio}<br>",
        "Redução: {round(reducao * 100, 2)}%"
      )
    )) +
    geom_col() +
    scale_fill_manual(values = c("Destaque" = "#FF5B5B", "Outros" = "grey60")) +
    geom_hline(
      yintercept = reducao_23_br,
      linetype = "dashed",
      color = "red"
    ) +
    annotate(
      geom = "text",
      y = reducao_23_br + 0.10,
      x = 20,
      label = glue("Média\n do Brasil: {label_br}%"),
      size = 2,
      color = "red"
    ) +
    coord_flip() +
    theme_minimal(base_size = 8) +
    scale_y_continuous(
      minor_breaks = NULL,
      breaks = seq(-0.6, 0.6, 0.1),
      limits = c(-0.6, 0.6),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      y = "Redução / Aumento (%)",
      x = "Estado",
      fill = NULL
    ) +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "none"
    )
  
 
  ggplotly(p, tooltip = "text") %>%
    layout(margin = list(l = 90))  
}

#Plot da Redução dos Estados.
plot_reducao_estados <- function(municipios, destaque = "SP") {
  label_br <- round(reducao_23_br * 100)
  
  dados <- municipios %>%
    group_by(estado_nome) %>%
    summarise(var_23 = sum(var_23), mortes = sum(media_mortes)) %>%
    mutate(
      perc_var_23 = var_23 / mortes,
      destaque_estado = ifelse(estado_nome == destaque, "Destaque", "Outros"),
      estado_nome = fct_reorder(estado_nome, desc(perc_var_23)),
      texto_tooltip = glue(
        "Estado: {estado_nome}\nRedução: {round(perc_var_23 * 100, 2)}%"
      )
    )
  
  g <- ggplot(dados, aes(x = estado_nome, y = perc_var_23, fill = destaque_estado,
                         text = texto_tooltip)) +
    geom_col() +
    scale_fill_manual(values = c("Destaque" = "#FF5B5B", "Outros" = "grey60")) +
    geom_hline(
      yintercept = reducao_23_br,
      linetype = "dashed",
      color = "red"
    ) +
    annotate(
      geom = "text",
      y = reducao_23_br + 0.10,
      x = 20,
      label = glue("Média\n do Brasil: {label_br}%"),
      size = 2,
      color = "red"
    ) +
    coord_flip() +
    theme_minimal(base_size = 8) +
    scale_y_continuous(
      minor_breaks = NULL,
      breaks = seq(-0.6, 0.6, 0.1),
      limits = c(-0.6, 0.6),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      y = "Redução / Aumento (%)",
      x = "Estado",
      fill = NULL
    ) +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "none"
    )
  
  ggplotly(g, tooltip = "text")
}


#Tabela de Redução ou aumento.

base_graficos <- function(base){
  base %>% 
    mutate(
      cor = ifelse(meta_atingida >= 0, "green", "red"),
      meta_atingida_limite = case_when(
        meta_atingida > 100 ~ 100,
        meta_atingida < -100 ~ -100,
        TRUE ~ meta_atingida
      ),
      tipo = ifelse(meta_atingida_limite > 0, 'Redução: ', 'Aumento: ' ),
      tooltip = paste0(
        "Município: ", nome_do_municipio, "<br>",
        "UF: ", uf, "<br>",
        "Meta de Redução: ", round(var_perc*100, 2), "%<br>",
        tipo, round(reducao*100, 1), "%<br>",
        "Meta Atingida: ", round(meta_atingida_limite, 2), "%", "<br>",
        'Média de Mortes (2018-2020): ', round(media_mortes, 2), "<br>",
        'Número de Mortes (2023): ', n_mortes_23
      )
    )
}

tabela_reducao_aumento <- function(dados_filtrados) {
  dados_ordenados <- dados_filtrados %>%
    arrange(Prioridade) %>%
    mutate(
      cor_html = ifelse(
        as.numeric(gsub("%", "", `Redução/Aumento`)) < 0,
        paste0("<span style='color:#228B22'>", `Redução/Aumento`, "</span>"),
        paste0("<span style='color:#B22222'>", `Redução/Aumento`, "</span>")
      )
    )
  
  
  dados_ordenados$`Redução/Aumento` <- dados_ordenados$cor_html
  dados_ordenados$cor_html <- NULL  
  

  datatable(
    dados_ordenados,
    escape = FALSE, 
    filter = "none",  
    rownames = FALSE,
    options = list(
      dom = 'ft',
      paging = F, 
      scrollY = "500px",
      scrollCollapse = T,
      autoWidth = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "  this.api().columns().every(function(index) {",
        "    if(index === 10) {", 
        "      var column = this;",
        "      var header = $(column.header()).empty();",
        "      header.append('<div style=\"font-weight:bold; font-size:16px;\">Atingiu a Meta? </div>');",
        "      var select = $('<select><option value=\"\">Mostrar todos</option></select>')",
        "        .appendTo(header)",
        "        .on('change', function() {",
        "          var val = $.fn.dataTable.util.escapeRegex($(this).val());",
        "          column.search(val ? '^' + val + '$' : '', true, false).draw();",
        "        });",
        "      column.data().unique().sort().each(function(d, j) {",
        "        select.append('<option value=\"'+d+'\">'+d+'</option>');",
        "      });",
        "    }",
        "  });",
        "}"
      ),
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")  
      )
    )
  )
}

capitais <- c(
  "Rio Branco", "Maceió", "Macapá", "Manaus", "Salvador",
  "Fortaleza", "Brasília", "Vitória", "Goiânia", "São Luís",
  "Cuiabá", "Campo Grande", "Belo Horizonte", "Belém", "João Pessoa",
  "Curitiba", "Recife", "Teresina", "Rio de Janeiro", "Natal",
  "Porto Alegre", "Porto Velho", "Boa Vista", "Florianópolis",
  "São Paulo", "Aracaju", "Palmas"
)


#Scatterplot
grafico_meta_atingida <- function(base_graficos) {
  plot_ly(
    data = base_graficos,
    x = ~var_perc*100,
    y = ~meta_atingida_limite,
    type = 'scatter',
    mode = 'markers',
    color = ~cor,
    colors = c("red" = "#E74C3C", "green" = "#2ECC71"),
    text = ~tooltip,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "Meta de Redução (%)",
        titlefont = list(size = 14),
        tickfont = list(size = 12),
        showgrid = TRUE,
        zeroline = TRUE,
        zerolinewidth = 1,
        zerolinecolor = "#999",
        tickvals = seq(-100, 100, 20),
        ticktext = paste0(seq(-100, 100, 20), "%")
      ),
      yaxis = list(
        title = "Percentual da Meta Atingida (%)",
        titlefont = list(size = 14),
        tickfont = list(size = 12),
        showgrid = TRUE,
        zeroline = TRUE,
        zerolinewidth = 1,
        zerolinecolor = "#999",
        range = c(-105, 105),
        tickvals = seq(-100, 100, 20),
        ticktext = paste0(seq(-100, 100, 20), '%')
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#ffffff",
      margin = list(l = 20, r = 10, b = 30, t = 30),
      showlegend = FALSE
    )
}




