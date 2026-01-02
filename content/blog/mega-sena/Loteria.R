# devtools::install_github("damarals/loteria")
# 
# dados_megasena <- loteria::dados_sorteios(modalidade = 'megasena') 

# [link para o site da Caixa](http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena/)

# Esse ano acabei lendo algumas reportagens sobre os números sorteados com maior frequência na Mega-Sena, e isso me deu vontade de explorar esses dados com mais calma. Então, trago aqui uma **análise exploratória de todos os 2955 sorteios já realizados** até o momento em que escrevo este post, no início de 2026. E, como você pode imaginar, não ganhei na virada de 2025 para 2026 -- mas já estou com as esperanças renovadas para o ano que vem, haha!


library(tidyverse)

dados <- readxl::read_excel("Mega-Sena.xlsx")

dados$`Data do Sorteio` <- 
  as.Date(dados$`Data do Sorteio`, format = "%d/%m/%Y")

dados[,c(11,13,15:19)] <-
  lapply(dados[,c(11,13,15:19)], \(x) gsub("\\.", "", x))

dados[,c(11,13,15:19)] <-
  lapply(dados[,c(11,13,15:19)], \(x) gsub("R\\$", "", x))

dados[,c(11,13,15:19)] <-
  lapply(dados[,c(11,13,15:19)], \(x) as.numeric(gsub(",", "\\.", x)))

dados$`Valor Mega` <- dados$`Ganhadores 6 acertos`*dados$`Rateio 6 acertos`

dados_long <- dados |> 
  pivot_longer(cols = 3:8, names_to = "Ordem", values_to = "Número",
               names_prefix = "Bola")

dicionario <- fstatix::dicionario(dados)


dados_long |> 
  count(Número) |> 
  mutate(`%` = 100*n/2955) |> 
  slice_max(n, n = 10)

dados_long |> 
  count(Número) |> 
  mutate(`%` = 100*n/2955) |> 
  slice_min(n, n = 10)



ggplot(dados |> filter(`Valor Mega` > 0),
       aes(x = `Valor Mega`)) +
  geom_histogram()

dados |> filter(`Valor Mega` > 0) |> View()


comb_sorteios <- dados_long |>
  select(c("Concurso", "Número")) |> 
  arrange(Concurso, Número) |>
  group_by(Concurso) |>
  summarise(
    combinacao = paste(Número, collapse = "-"),
    .groups = "drop"
  )

comb_sorteios |>
  group_by(combinacao) |>
  filter(n() > 1)
