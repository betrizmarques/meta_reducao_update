library(roadtrafficdeaths)


mortes <- rtdeaths %>% 
  filter(ano_ocorrencia == 2023)


municipios_por_uf <- rtdeaths %>% 
  group_by(nome_uf_ocor) %>% 
  summarise(n_municipios_base_de_mortes = n_distinct(nome_municipio_ocor))

base_por_uf <- base_principal %>% 
  group_by(estado_nome) %>% 
  summarise(n_municipios_base_das_metas = n_distinct(nome_do_municipio))


numero_de_municipios_por_base <- left_join(municipios_por_uf, base_por_uf, by = c("nome_uf_ocor" = "estado_nome"))


numero_de_municipios_por_base <- numero_de_municipios_por_base %>% 
  mutate(diferenca = n_municipios_base_de_mortes - n_municipios_base_das_metas)


municipios_ja_atingiram_meta <- base_principal %>% 
  filter(meta_atingida>= 100)

