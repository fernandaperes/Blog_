pacman::p_load(tidyverse, ggrepel)

dados <- readxl::read_excel("Base_hamburgueres.xlsx")

dados <- dados |> 
  mutate(
    Ranking = str_extract(Listagem, "^\\d+"),
    Nome = str_trim(str_extract(str_replace(Listagem, "^\\d+[.:]\\s*", ""), "^[^\\(]+")),
    Valores = str_extract_all(Listagem, "(?<=R\\$\\s{0,3})\\d+"),
    `X-burguer` = sapply(Valores, \(x) if (length(x) > 1) x[1] else x[1]),
    `X-salada` = sapply(Valores, \(x) if (length(x) > 1) x[2] else x[1])
  )

dados[,c(2,5:6)] <- lapply(dados[,c(2,5:6)], as.numeric)

dados <- dados |> 
  mutate(
    Nota_padronizada = scale(101-Ranking)[,1],
    Preco_xburguer_pad = scale(`X-burguer`)[,1]
  ) |> 
  mutate(
    CB = ifelse(Nota_padronizada > Preco_xburguer_pad, "Benefício > Custo",
                "Custo > Benefício"),
    Relacao_CB = Nota_padronizada - Preco_xburguer_pad
  )

top10  <- dados |> slice_max(Relacao_CB, n = 10, with_ties = TRUE) |> pull(Relacao_CB)
bottom10 <- dados |> slice_min(Relacao_CB, n = 10, with_ties = TRUE) |> pull(Relacao_CB)

dados <- dados |>
  mutate(
    Categoria = case_when(
      Relacao_CB %in% top10    ~ "Melhores custos-benefícios",
      Relacao_CB %in% bottom10 ~ "Piores custos-benefícios",
      TRUE ~ NA_character_
    )
  )

dados$Categoria <- factor(dados$Categoria,
                          levels = c("Piores custos-benefícios",
                                     "Melhores custos-benefícios"))

# ggplot(dados, aes(x = Ranking, y = `X-burguer`)) +
#   geom_point() +
#   geom_text_repel(aes(label = Nome), size = 2.5) +
#   theme_minimal()

ggplot(dados, aes(x = Nota_padronizada, y = Preco_xburguer_pad)) +
  geom_point(aes(color = Categoria)) +
  geom_abline() +
  geom_text_repel(aes(label = Nome), size = 2.5) +
  labs(y = "Preço do Cheeseburger (padronizado)",
       x = "Nota (padronizada)", color = NULL) +
  scale_color_discrete(breaks = \(x) x[!is.na(x)], na.value = "grey70") +
  theme_minimal() +
  theme(legend.position = "top")



graf <- ggplot(dados, aes(x = Nota_padronizada, y = Preco_xburguer_pad)) +
  geom_point(aes(color = Categoria,
                 text = paste0(Nome,
                               "<br>",
                               "Preço Cheeseburger: ", `X-burguer`,
                               "<br>", 
                               "Posição no ranking: ", paste0(Ranking, "º lugar")))) +
  geom_abline() +
  labs(y = "Preço do Cheeseburger (padronizado)",
       x = "Nota (padronizada)", color = NULL) +
  scale_color_discrete(breaks = \(x) x[!is.na(x)], na.value = "grey70") +
  theme_minimal() +
  theme(legend.position = "top")

library(plotly)
ggplotly(graf, tooltip = c("text"))


