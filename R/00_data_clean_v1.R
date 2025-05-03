###----------------------------------------------------
###--- Cumprimento das Metas de Redução de Mortes -----
#--- no Trânsito de Municípios Brasileiros ------------
###----------------------------------------------------
### O objetivo principal deste projeto de pesquisa é observar o cumprimento das metas
### de redução de mortes no trânsito dos municípios brasileiros estabelecidas em 2020, 
### por uma pesquisa que utilizou a técnica de benchmarking para estabelecer metas
### para municípios com características semelhantes.


### Por: A. Beatriz Marques, Graduanda de Estatística e Ciência de Dados - UFPR
### Dr. Jorge Tiago Bastos - Observatório Nacional de Segurança Viária

require(roadtrafficdeaths)
require(tidyverse)

# Carregando bases de dados para a limpeza.

mortes_transito <- rtdeaths #base das mortes de trânsito

frota_municipios <- readxl::read_xlsx('data/FrotapormunicipioetipoDezembro2024.xlsx',
                                      skip = 3) # base das frotas dos municípios

municipios_metas <- read.csv('data/municipios_metas.csv') #base das metas estabelecidas para cada município

ufs <- tibble::tibble( 
  sigla = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
            "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
            "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  nome = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal",
           "Espírito Santo", "Goiás", "Maranhão", "Minas Gerais", "Mato Grosso do Sul",
           "Mato Grosso", "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro",
           "Rio Grande do Norte", "Rondônia", "Roraima", "Rio Grande do Sul", "Santa Catarina",
           "Sergipe", "São Paulo", "Tocantins")
)

municipios_metas <- municipios_metas %>%  #adicionando a sigla: preparação para a junção das bases.
  left_join(ufs, by = c("uf" = "nome")) %>% 
  rename(estado_nome = uf, estado = sigla)

mortes_transito_23 <- mortes_transito %>% # filtrando apenas as mortes em 2023.
  filter(ano_ocorrencia == 2023) %>% 
  left_join(ufs, by = c("nome_uf_ocor" = "nome" )) %>% 
  rename(uf = sigla)

mortes_agrupadas_municipio <- mortes_transito_23 %>% #agrupando as mortes por município.
  group_by(uf, nome_municipio_ocor) %>% 
  summarise(numero_de_mortes = n(), .groups = "drop") %>% 
  rename(nome_do_municipio = nome_municipio_ocor)

municipios_metas <- municipios_metas %>% #renomeando as variáveis: preparação para a junção das bases.
  rename(nome_do_municipio = nome, uf = estado)

base_principal <- left_join(municipios_metas, mortes_agrupadas_municipio,
                            by = c("uf", "nome_do_municipio")) %>% 
  mutate(municipios_minusculo = tolower(abjutils::rm_accent(nome_do_municipio))) # juntando as bases.

frota_municipios <- frota_municipios %>% #renomeando as variávies: preparação para a junção das bases.
  mutate(municipios_minusculo = tolower(MUNICIPIO)) 

old_names <- names(frota_municipios)
new_names <- tolower(old_names)
colnames(frota_municipios) <- new_names

frota_municipios <- frota_municipios %>% #selecionando apenas as variáveis necessárias.
  select(uf, municipios_minusculo, total)

base_principal <- left_join(base_principal, frota_municipios, 
                            by = c('municipios_minusculo', 'uf')) %>% 
  select(-municipios_minusculo) %>% 
  rename(frota_23 = total, n_mortes_23 = numero_de_mortes) #juntando as bases.

base_principal <- base_principal %>% # substituindo os NA's do numero de mortes por 0.
  mutate(n_mortes_23 = replace_na(n_mortes_23, 0),
         var_23 = n_mortes_23-media_mortes, 
         reducao = var_23/media_mortes,
         meta_atingida = ((perc_reducao)/(var_perc)*100))

write.csv(base_principal, file = "data/base_principal.csv") #salvando a base com todas as informações necessárias para a análise.

# agora que já temos a base principal, podemos fazer alguns cálculos gerais para o Brasil.

mortes_br <- sum(base_principal$media_mortes) #soma a média de mortes de 2018-2020 no Brasil.
meta_br <- sum(base_principal$meta_round) #calcula a meta de número de mortes para o Brasil.
mortes_br_23 <- sum(base_principal$n_mortes_23) #soma todas as mortes que ocorreram  em 2023.
var_br_23 <- mortes_br_23-mortes_br #calcula a variação do número de mortes.
var_perc_br_23 <- var_br_23 / mortes_br# calcula o percentual da variação em relação as mortes.

agrupados_por_uf <- base_principal %>% 
  group_by(estado_nome) %>%
  summarise(n_mortes_23 = sum(n_mortes_23), media_mortes = sum(media_mortes), meta = sum(meta)) %>% 
  mutate(var_23 = n_mortes_23-media_mortes, perc_reducao = var_23/media_mortes, meta_perc = (meta-media_mortes)/media_mortes)

agrupados_por_porte <- base_principal %>% 
  group_by(porte) %>% 
  summarise(n_mortes_23 = sum(n_mortes_23), media_mortes = sum(media_mortes)) %>% 
  mutate(var_23 = n_mortes_23-media_mortes, perc_reducao = var_23/media_mortes, perc = perc_reducao*100)

base_principal <- base_principal %>% 
  mutate(meta_atingida = ((perc_reducao)/(var_perc)*100))

base_especifica <- base_principal %>% 
  select(estado_nome, nome_do_municipio, porte, media_mortes, meta_round, n_mortes_23, var_perc, perc_reducao, meta_atingida)


