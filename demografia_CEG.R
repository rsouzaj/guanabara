library(tidyverse)


# Dados demográficos CEG --------------------------------------------------


df <-  readr::read_csv("Cadastro_2023 13.10.2023 V2.xlsx - Atual.csv",
                       skip = 1)

names(df)

df |>
  summarise(.by = "Situação financeira",
            N= n()
  )

library(dplyr)
library(lubridate)


# Todos os sócios ---------------------------------------------------------


df_todos <- df |>
  mutate(
    Nascimento = as.Date(Nascimento,tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
    `Admissão` = as.Date(`Admissão`,tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
    Idade =floor(as.numeric(difftime(Sys.Date(), Nascimento, units = "days"))/365),
    tempo_associado = floor(as.numeric(difftime(Sys.Date(), `Admissão`, units = "days"))/365),
    faixa_etaria = case_when(
      tempo_associado <= 3 ~ "Menos de 3 anos",
      tempo_associado > 3 & tempo_associado <=5 ~ "Entre 3 e 5 anos",
      tempo_associado >5 & tempo_associado <=10 ~ "Entre 5 e 10 anos",
      tempo_associado > 10& tempo_associado<= 20 ~ "Entre 10 e 20 anos",
      tempo_associado > 20 ~ "Mais de 20 anos"
    ),
    faixa_etaria = fct_relevel(faixa_etaria,
                               c("Menos de 3 anos","Entre 3 e 5 anos","Entre 5 e 10 anos", "Entre 10 e 20 anos","Mais de 20 anos"   )
    ),
    guia = if_else(!is.na(`Guia desde`), 'Sim', 'Não')
  )


# Somente sócios ativos ---------------------------------------------------



df_ativo <- df |>
  mutate(
    Nascimento = as.Date(Nascimento,tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
    `Admissão` = as.Date(`Admissão`,tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
    Idade =floor(as.numeric(difftime(Sys.Date(), Nascimento, units = "days"))/365),
    tempo_associado = floor(as.numeric(difftime(Sys.Date(), `Admissão`, units = "days"))/365),
    faixa_etaria = case_when(
      tempo_associado <= 3 ~ "Menos de 3 anos",
      tempo_associado > 3 & tempo_associado <=5 ~ "Entre 3 e 5 anos",
      tempo_associado >5 & tempo_associado <=10 ~ "Entre 5 e 10 anos",
      tempo_associado > 10& tempo_associado<= 20 ~ "Entre 10 e 20 anos",
      tempo_associado > 20 ~ "Mais de 20 anos"
    ),
    faixa_etaria = fct_relevel(faixa_etaria,
                               c("Menos de 3 anos","Entre 3 e 5 anos","Entre 5 e 10 anos", "Entre 10 e 20 anos","Mais de 20 anos")
    ),
    guia = if_else(!is.na(`Guia desde`), 'Sim', 'Não')
  ) |>
  filter(`Situação financeira` =="Ativo")


df_ativo |>
  ggplot(aes(Idade)) +
  geom_boxplot()

df_ativo |>
  View()

df_ativo |>
  filter(tempo_associado < 5) |>
  ggplot(aes(tempo_associado ))+
  geom_bar()

df_ativo |>
  filter( tempo_associado<5) |>
  view()

df_ativo |>
  ggplot(aes(tempo_associado ))+
  geom_bar()

df_ativo |>
  filter( tempo_associado < 5) |>
  ggplot(aes(tempo_associado ))+
  geom_bar()

df_ativo |>
  filter( tempo_associado>=30) |> View()


df_ativo |>
  ggplot(aes(faixa_etaria))+
  geom_bar()


df_todos |>
  ggplot(aes(guia, fill= `Situação financeira` =="Ativo")) +
  geom_bar(position ='dodge')


df_ativo %>%
  group_by(guia) %>%
  summarise(
    `Média de idade` = mean(Idade, na.rm = T),
    Numero = n(),
    total_socios = nrow(.),
    Porcentagem = paste0(round((Numero / total_socios)*100, digits = 1), "%")
  ) |>
  xtable::xtable() |>
  flextable::as_flextable()


