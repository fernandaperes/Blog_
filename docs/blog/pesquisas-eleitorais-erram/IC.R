library(tidyverse)

ci_low <- function(prop, n){
  prop - 1.96*sqrt((prop*(1-prop))/n)
}

ci_up <- function(prop, n){
  prop + 1.96*sqrt((prop*(1-prop))/n)
}

set.seed(4321)
votos <- sample(x = c(1, 0), size = 1000000, prob = c(0.31, 1-0.31), replace = T)
pop <- as.data.frame(votos)



# Estabelecendo a quantidade de amostras aleatórias a serem extraídas
R <- 50

set.seed(13082024)

# Sorteio

for(i in 1:R){
  
  res <- pop |> slice_sample(n = 2500) |> 
    summarise(prop = mean(votos),
              cilow = ci_low(prop, 2500),
              ciup = ci_up(prop, 2500),
              x = i) |> as.data.frame()
  
  df_res[i,] <- res
  
}


df_res <- df_res %>% mutate(cor = case_when(ciup > 0.31 & cilow > 0.31 ~ "#B5284B",
                                            ciup < 0.31 & cilow < 0.31 ~ "#B5284B",
                                            TRUE ~ "#2794A0"))


ggplot(df_res, aes(x = prop, y = x, color = cor)) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(xmin = cilow, xmax = ciup)) +
  geom_vline(aes(xintercept = 0.31), linetype = "dashed") +
  labs(x = "Intenção de votos", y = "Amostra") +
  scale_color_identity() +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(text = element_text(size = 10))

ggsave("ic_95.png", dpi = 600, width = 4, height = 3.3)

