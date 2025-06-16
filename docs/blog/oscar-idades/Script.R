pacman::p_load(tidyverse, ggpubr, plotly)

dados <- read_csv("the_oscar_award.csv") |> 
  dplyr::filter(grepl('ACTOR|ACTRESS', category)) |>
  filter(winner == T)

dados2 <- read_csv("Oscars-demographics-DFE.csv") |> 
  dplyr::filter(grepl('Actor|Actress', award)) |> 
  select(c(date_of_birth, person)) |> 
  distinct()

colnames(dados2)[2] <- c("name")

dados <- dplyr::left_join(dados, dados2) |> 
  filter(!is.na(category))

dados_com_data <- dados |> 
  filter(!is.na(date_of_birth)) |> 
  separate_wider_delim(cols = 8, delim = "-",
                       names = c("d", "m", "y")) |> 
  mutate(d = ifelse(as.numeric(d) < 10, paste0("0", d), d),
         y = ifelse(as.numeric(y) > 1000, y,
                    paste0("19", y)),
         date_of_birth = as.Date(paste0(d, "-", m, "-", y),
                                 format = "%d-%b-%Y")) |> 
  select(-c("d", "m", "y"))

dados_sem_data <- dados |> 
  filter(is.na(date_of_birth))

# dados_sem_data |> 
#   writexl::write_xlsx("dados_para_adicionar_data.xlsx")

dados_sem_data <- readxl::read_excel("dados_para_adicionar_data.xlsx")

dados <- rbind(dados_com_data, dados_sem_data)
rm(dados_com_data, dados_sem_data, dados2)

dados <- dados |> 
  mutate(age_filming = as.numeric(difftime(as.Date(paste0(year_film,"-01-01")),
                                           date_of_birth)/365.25),
         age_ceremony = as.numeric(difftime(as.Date(paste0(year_ceremony,"-03-01")),
                                            date_of_birth)/365.25)) |> 
  mutate(age_filming = floor(age_filming),
         age_ceremony = floor(age_ceremony))


dados$gender <- ifelse(stringr::str_detect(dados$category, "ACTRESS"),
                       "Feminino", "Masculino")

dados$category <- factor(dados$category,
                         levels = c("ACTOR",
                                    "ACTOR IN A LEADING ROLE",
                                    "ACTOR IN A SUPPORTING ROLE", 
                                    "ACTRESS",
                                    "ACTRESS IN A LEADING ROLE",
                                    "ACTRESS IN A SUPPORTING ROLE"),
                         labels = c("Melhor ator",
                                    "Melhor ator",
                                    "Melhor ator coadjuvante", 
                                    "Melhor atriz",
                                    "Melhor atriz",
                                    "Melhor atriz coadjuvante"))

colnames(dados) <- c("Ano do filme", "Ano da cerimônia", "Cerimônia",
                     "Categoria", "Nome", "Filme", "Vencedor",
                     "Data de nascimento", "Idade (anos) no filme",
                     "Idade (anos) na cerimônia", "Gênero")


fstatix::paleta_f()

ggplot(dados, aes(y = `Idade (anos) no filme`, x = Categoria)) +
  ggbeeswarm::geom_beeswarm(color = azul, alpha = 0.3) +
  geom_crossbar(stat = "summary", fun = "mean", fatten = 1, width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito"))


dados |> 
  mutate(`Década da cerimônia` = `Ano da cerimônia` - `Ano da cerimônia` %% 10) |> 
  ggplot(aes(x = `Década da cerimônia`,
             y = `Idade (anos) na cerimônia`,
             color = Categoria)) +
  geom_line(stat = "summary", fun = "mean") +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  scale_color_manual(values = c(azul, amarelo, rosa, roxo)) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito"))



ggplot(dados |> filter(`Ano da cerimônia` > 1980),
       aes(y = `Idade (anos) no filme`, x = Categoria)) +
  ggbeeswarm::geom_beeswarm(color = azul, alpha = 0.3) +
  geom_crossbar(stat = "summary", fun = "mean", fatten = 1, width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito"))





graf <- ggplot(dados, aes(y = `Idade (anos) na cerimônia`, x = Categoria)) +
  ggbeeswarm::geom_beeswarm(color = azul, alpha = 0.7,
                            aes(text = paste0("Idade: ", `Idade (anos) na cerimônia`,
                                              "<br>", Nome, "<br>",
                                              "Ano da cerimônia: ", `Ano da cerimônia`))) +
  # geom_crossbar(stat = "summary", fun = "mean", fatten = 1, width = 0.6) +
  # geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito"))



ggplotly(graf, tooltip = c("text"))
