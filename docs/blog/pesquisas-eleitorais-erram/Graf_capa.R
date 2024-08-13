dados <- as.data.frame(list(Candidato = rep(c("A", "B"), each = 6),
                            Votos = c(0.4, 0.35, 0.32, 0.37, 0.38, 0.42,
                                      0.6, 0.65, 0.68, 0.63, 0.62, 0.58),
                            Momento = c(1:6, 1:6)))

library(tidyverse)
ggplot(dados, aes(x = Momento, y = Votos, color = Candidato, group = Candidato)) +
  geom_line(show.legend = F, linewidth = 1.2) +
  annotate(geom = "text", label = "42%", x = 6, y = 0.42, hjust = -0.2,
           family = "Nunito", fontface = 2) +
  annotate(geom = "text", label = "58%", x = 6, y = 0.58, hjust = -0.2,
           family = "Nunito", fontface = 2) +
  coord_cartesian(ylim = c(0.2, 0.8), xlim = c(1, 6.8)) +
  scale_color_manual(values = c("#404775", "#BE643E")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  theme(text = element_text(family = "Nunito"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))


ggsave("graf_eleicoes.png", width = 3, height = 2, dpi = 600)


votos1 <- c(0.4, 0.35, 0.32, 0.37, 0.38, 0.42)

dput(1-votos1)
