require(tidyverse)

# agora que já temos a base principal, podemos fazer alguns cálculos gerais.

mortes_br <- sum(base_principal$media_mortes) #soma a média de mortes de 2018-2020 no Brasil.
meta_br <- sum(base_principal$meta_round) #calcula a meta de número de mortes para o Brasil.
mortes_br_23 <- sum(base_principal$n_mortes_23) #soma todas as mortes que ocorreram  em 2023.
var_br_23 <- mortes_br_23-mortes_br #calcula a variação do número de mortes.
var_perc_br_23 <- var_br_23 / mortes_br # calcula o percentual da variação em relação as mortes.

agrupados_por_uf <- base_principal %>% # fazendo o cálculo por estado
  group_by(estado_nome) %>%
  summarise(n_mortes_23 = sum(n_mortes_23), media_mortes = sum(media_mortes), meta = sum(meta)) %>% 
  mutate(var_23 = n_mortes_23-media_mortes, perc_reducao = var_23/media_mortes, meta_perc = (meta-media_mortes)/media_mortes)

agrupados_por_porte <- base_principal %>% # fazendo o cálculo por tamanho porte do município.
  group_by(porte) %>% 
  summarise(n_mortes_23 = sum(n_mortes_23), media_mortes = sum(media_mortes)) %>% 
  mutate(var_23 = n_mortes_23-media_mortes, perc_reducao = var_23/media_mortes, perc = perc_reducao*100)

base_especifica <- base_principal %>% 
  select(estado_nome, nome_do_municipio, porte, media_mortes, meta_round, n_mortes_23, var_perc, perc_reducao, meta_atingida)


