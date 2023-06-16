pacman::p_load(tidyverse, readxl, ggtext)

dados <- readxl::read_xlsx("Gols.xlsx")

dados |>
  mutate(porc = Gols/sum(Gols),
         label = paste0(Gols, " (", fstatix::arred(100*porc, 1), "%)")) |> 
  ggplot(aes(y = fct_reorder(Jogador, Gols), x = Gols)) +
  geom_bar(stat = "identity", fill = "#8C0303") +
  geom_text(aes(label = label), size = 2.8, color = "black",
            hjust = -0.1) +
  coord_cartesian(clip = "off") +
  labs(y = NULL,
       title = "Distribuição dos 501 gols do<br>elenco atual pelo <b style='color:#8C0303'>Flamengo</b><br>por jogador",
       x = "Gols marcados") +
  theme_minimal() +
  theme(plot.margin = margin(5, 40, 5, 5),
        plot.title = element_markdown(margin = margin(0,0,2,0), size = 11),
        plot.background = element_rect(fill = "white"))

ggsave("plot1.png", height = 6.5, width = 5)

dados |>
  mutate(Jogador_unido = ifelse(Gols <= 4, "Outros", Jogador)) |> 
  group_by(Jogador_unido) |> 
  mutate(Gols = sum(Gols)) |> 
  select(-Jogador) |> 
  distinct() |> 
  ungroup() |> 
  mutate(porc = Gols/sum(Gols),
         label = paste0(Gols, " (", fstatix::arred(100*porc, 1), "%)")) |> 
  ggplot(aes(y = fct_inorder(Jogador_unido), x = Gols,
             fill = ifelse(Jogador_unido == "Outros", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), size = 2.8, color = "black",
            hjust = -0.1) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = c("Highlighted" = "#8C0303", "Normal" = "#8C0303")) +
  labs(y = NULL,
       title = "Distribuição dos 501 gols do<br>elenco atual pelo <b style='color:#8C0303'>Flamengo</b><br>por jogador",
       x = "Gols marcados") +
  theme_minimal() +
  theme(plot.margin = margin(5, 40, 5, 5),
        plot.title = element_markdown(margin = margin(0,0,2,0), size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = "white"))

ggsave("plot2.png", height = 5.5, width = 5)





dados2 <- readxl::read_xlsx("Gols.xlsx", sheet = 2)

imagens <- c("https://conteudo.cbf.com.br/cdn/imagens/escudos/00004pr.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/60646rj.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00009rj.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00005ba.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00021sp.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00009go.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/60175rj.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00019sp.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00008rs.jpg",
             "https://conteudo.cbf.com.br/cdn/imagens/escudos/00018sp.jpg")

# labels <- setNames(
#   paste0("<font style='padding-right:8px;'>", dados2$Time, "</font> <img src='", imagens, "' width='20' />"),
#   dados2$Time
# )



dados2$labels <- setNames(
  paste0("<img src='", imagens, "' width='20' style='padding-left:20px;' /><br>", dados2$Time),
  dados2$Time
)


ggplot(dados2, aes(y = fct_inorder(Time), x = Gols)) +
  geom_bar(stat = "identity", fill = "#8C0303") +
  scale_y_discrete(labels = rev(dados2$labels), limits = rev) +
  geom_text(aes(label = Gols), size = 2.8, color = "white",
            hjust = 1.3, fontface = "bold") +
  coord_cartesian(clip = "off") +
  labs(y = NULL,
       title = "Times que mais levaram gols do<br>elenco atual do <b style='color:#8C0303'>Flamengo</b>",
       x = "Gols sofridos") +
  theme_minimal() +
  theme(axis.text.y  = element_markdown(color = "black", size = 7),
        plot.margin = margin(5, 10, 5, 5),
        plot.title = element_markdown(margin = margin(0,0,2,0)),
        plot.background = element_rect(fill = "white"))

ggsave("plot3.png", height = 5.5, width = 5)



