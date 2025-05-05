

require(tidyverse)

calculo_br <- function(base) {
  assign('mortes_br_antigo', sum(base$media_mortes), envir = .GlobalEnv)
  assign("meta_mortes_br", sum(base$meta_round), envir = .GlobalEnv)
  assign("mortes_br_23", sum(base$n_mortes_23), envir = .GlobalEnv)
  assign("var_br_23", mortes_br_23 - mortes_br_antigo, envir = .GlobalEnv)
  assign("reducao_23", var_br_23 / mortes_br_antigo, envir = .GlobalEnv)
  assign("meta_reducao", (meta_mortes_br - mortes_br_antigo) / mortes_br_antigo, envir = .GlobalEnv)
}

dados_filtrados <- function(base){
  dados_filtrados <- 
  base %>% 
    mutate(meta_percentual = paste0(round(var_perc, 2),'%'),
           reducao_percentual = paste0(round(reducao*100, 2),'%'),
           media_mortes = round(media_mortes),
           cor = ifelse(meta_atingida >= 0, "red", "green"),
           meta_atingida_limite = case_when(
             meta_atingida > 100 ~ 100,
             meta_atingida < -100 ~ -100,
             TRUE ~ meta_atingida
           ),
           meta_atingida_limite = paste0(round(meta_atingida_limite, 2),'%')) %>% 
    select(prioridade,nome_do_municipio, uf, media_mortes, meta_percentual, n_mortes_23, reducao_percentual, meta_atingida_limite) %>% 
    rename('Prioridade' = prioridade,
           'Município' = nome_do_municipio,
           'Média de mortes (2018-2020)' = media_mortes,
           'Meta de Redução' = meta_percentual,
           'Número de mortes (2023)' = n_mortes_23,
           'Redução/Aumento'= reducao_percentual,
           'Meta Atingida' = meta_atingida_limite)
  
  assign('dados_filtrados', dados_filtrados, envir = .GlobalEnv)
}

plot_capitais <- function(municipios, destaque = "SP") {
  label_br <- round(var_perc_br_23 * 100)
  
  municipios %>%
    mutate(
      destaque_estado = ifelse(estado_nome == destaque, "Destaque", "Outros"),
      nome_do_municipio = fct_reorder(nome_do_municipio, perc_var_23, .desc = T)  
    ) %>%
    ggplot(aes(x = nome_do_municipio, y = perc_var_23, fill = destaque_estado)) +
    geom_col() +
    scale_fill_manual(values = c("Destaque" = "#FF5B5B", "Outros" = "grey60")) +
    geom_text(
      aes(label = round(perc_var_23 * 100)),
      color = "white",
      size = 2,
      position = position_stack(vjust = 0.5)
    ) +
    geom_hline(
      yintercept = var_perc_br_23,
      linetype = "dashed",
      color = "red"
    ) +
    annotate(
      geom = "text",
      y = var_perc_br_23 + 0.10,
      x = 20,
      label = glue("Aumento\n do Brasil: {label_br}%"),
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
    )+
    labs(
      y = "Redução (%)",
      x = "Estado",
      fill = NULL
    ) +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "none"
    )
}

plot_reducao_estados <- function(municipios, destaque = "SP") {
  label_br <- round(reducao_23 * 100)
  
  municipios %>%
    group_by(estado_nome) %>%
    summarise(var_23 = sum(var_23), mortes = sum(media_mortes)) %>%
    mutate(
      perc_var_23 = var_23 / mortes,
      destaque_estado = ifelse(estado_nome == destaque, "Destaque", "Outros"),
      estado_nome = fct_reorder(estado_nome, desc(perc_var_23))
    ) %>%
    ggplot(aes(x = estado_nome, y = perc_var_23, fill = destaque_estado)) +
    geom_col() +
    scale_fill_manual(values = c("Destaque" = "#FF5B5B", "Outros" = "grey60")) +
    geom_text(
      aes(label = round(perc_var_23 * 100)),
      color = "white",
      size = 2,
      position = position_stack(vjust = 0.5)
    ) +
    geom_hline(
      yintercept = var_perc_br_23,
      linetype = "dashed",
      color = "red"
    ) +
    annotate(
      geom = "text",
      y = var_perc_br_23 + 0.10,
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
    )+
    labs(
      y = "Redução / Aumento (%)",
      x = "Estado",
      fill = NULL
    ) +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "none"
    )
}


gerar_tab_estado <- function(nome_aba, media_antigo, media_23, n_municipios, 
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
          icon = icon("car"),
          color = "gray-dark"
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Número de Mortes no Trânsito (2023)",
          value = round(media_23, digits = 2),
          icon = icon("car"),
          color = "gray-dark"
        )
      ),
      column(
        width = 4,
        infoBox(
          width = 12,
          title = "Total de Municípios",
          value = n_municipios,
          icon = icon("location-dot"),
          color = "gray-dark"
        )
      )
    ),
    
    fluidRow(
      sortable(
        width = 6,
        box(
          width = 12,
          title = "Gráfico de Dispersão: Meta de Redução x Meta Atingida",
          plotlyOutput(plot_dispersao)
        ),
        box(
          width = 12,
          title = "Metas de Redução por Estado",
          plotlyOutput(plot_histograma)
        )
      ),
      sortable(
        width = 6,
        box(
          width = 12,
          title = "Tabela",
          gt_output(plot_box)
        ),
        box(
          width = 12,
          title = "Redução por Capital",
          plotlyOutput(plot_infantil)
        )
      )
    )
  )
}

tabela_reducao_aumento <- function(dados_filtrados) {
  dados_ordenados <- dados_filtrados %>%
    arrange(Prioridade) 
  
  dados_ordenados %>%
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
}


dados_filtrados(base_principal)
fig <- tabela_reducao_aumento(dados_filtrados)
fig
