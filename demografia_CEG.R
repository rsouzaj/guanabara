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

df <- df |>
  mutate(
    Nascimento = as.Date(Nascimento,tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
    `Admissão` = as.Date(`Admissão`,tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
    Idade =floor(as.numeric(difftime(Sys.Date(), Nascimento, units = "days"))/365),
    tempo_associado = floor(as.numeric(difftime(Sys.Date(), `Admissão`, units = "days"))/365)
  )

df |>
  filter(`Situação financeira` =="Ativo") |>
  ggplot(aes(Idade)) +
  geom_boxplot()


df |>
  filter(Idade<20) |>
  view()

df |>
  filter(`Situação financeira` =="Ativo") |>
  ggplot(aes(tempo_associado ))+
  geom_bar()

df |>
  filter(`Situação financeira` =="Ativo"& tempo_associado < 5) |>
  ggplot(aes(tempo_associado ))+
  geom_bar()

df |>
  filter(`Situação financeira` =="Ativo" & tempo_associado>=30) |> View()


df |>
  filter(`Situação financeira` =="Ativo") |>
  mutate(
   faixa_etaria = case_when(
     tempo_associado <= 3 ~ "Menos de 3 anos",
     tempo_associado > 3 & tempo_associado <=5 ~ "Entre 3 e 5 anos",
     tempo_associado >5 & tempo_associado <=10 ~ "Entre 5 e 10 anos",
    tempo_associado > 10& tempo_associado<= 20 ~ "Entre 10 e 20 anos",
    tempo_associado > 20 ~ "Mais de 20 anos"
   ),
   faixa_etaria = fct_relevel(faixa_etaria,
     c("Menos de 3 anos","Entre 3 e 5 anos","Entre 5 e 10 anos", "Entre 10 e 20 anos","Mais de 20 anos"   )
   )
  ) |>
  ggplot(aes(faixa_etaria))+
  geom_bar()

df |>
  filter(is.na(tempo_associado) & `Situação financeira` =="Ativo") |>
  View()
