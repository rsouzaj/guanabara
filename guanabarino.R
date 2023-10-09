library(tidyverse)

df <- read.delim("Conhecendo o(a) Guanabarino(a)  (respostas) - Respostas ao formulário 1.tsv")

df <-  df |>
  mutate(
    Trabalho = if_else(str_detect(`Se.não.frequenta..qual.o.motivo.`, "Trabalho"), 1, 0)
  )

df |>
  select(Qual.sua.idade.,Frequenta.a.sede.do.CEG.com.que.frequência.,
         Há.quantos.anos.ingressou.no.clube.) |>
gtsummary::tbl_summary(
  by= `Frequenta.a.sede.do.CEG.com.que.frequência.`,

)

df %>%
  ggplot(aes(Qual.sua.idade.))+
  geom_boxplot()+
  theme_classic()+
  xlab("Idade do(a) Guanabarino(a)")

df %>%
  ggplot(aes(Frequenta.a.sede.do.CEG.com.que.frequência.))+
  geom_bar()+
  coord_flip()

df %>%
  ggplot(aes(Se.não.frequenta..qual.o.motivo.))+
  geom_bar()+
  coord_flip()

df %>%
  ggplot(aes(Há.quantos.anos.ingressou.no.clube.))+
  geom_bar()+
  scale_y_continuous(breaks = seq(0, 16, by=2))


df %>%
  ggplot(aes(Você.fez.CBM))+
  geom_bar()

df %>%
  ggplot(aes(Dentro.do.montanhismo.com.o.que.você.mais.se.identifica.))+
  geom_bar()+
  # coord_flip()+
  scale_y_continuous(breaks = seq(0, 16, by=2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df %>%
  ggplot(aes(Quanto.à.ESCALADA..você.))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 30, by=2))

df %>%
  ggplot(aes(Quanto.à.CAMINHADA..você.))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 30, by=2))


df %>%
  ggplot(aes(Qual.o.grau.de.conhecimento.técnico.que.você.tem.no.momento.))+
  geom_bar()+
  coord_flip()+
  theme(axis.text.y = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 30, by=2))

df %>%
  ggplot(aes(Quantas.vezes.você.escala.por.mês..em.média.))+
  geom_boxplot()

df %>%
  ggplot(aes(Quantas.vezes.você.pratica.caminhada.em.montanha.por.mês..em.média.))+
  geom_boxplot()




df %>%
  rename( key =Diga.um.motivo.para.você.escolher.o.CEG.como.seu.clube.preferencial..caso.isso.seja.verdade.
  ) %>%
  reframe(
    Amizade = str_count(key, 'amig|Amig|amiz|Amiz|Amor|amor|Amo|Coração|coração|CORAÇÃO'),
    Pessoas = str_count(key, 'pesso|Pess|Grupo|grupo'),
    `Aspectos técnicos` = str_count(key, 'Técn|Tecn|tecn|técn|Capaci|capaci|guia|Guia|CBM|cbm|étic|Étic'),
    Acolhimento = str_count(key, 'Acolh|acolh|acolhi|empatia|Empatia|companhe|Companhe|recep|Recep'),
    `Identidade com o clube` = str_count(key, 'ident|Ident|afinid|Afinida|Hist|hist'),
    # CBM = str_count(key, 'CBM|cbm'),
   `Animação` = str_count(key, 'Anima|anima|alto|Alto|fest|Fest|Brincad|brincad'),
    `Ambiente familiar/descontraído` = str_count(key, 'Ambie|ambie|famil|Famil|famíl|Famíl|Descont|descont'),
   # `Reuniões` = str_count(key, 'Reun|reun|reu|Reu')
  ) %>%
  reframe(
  Amizade = sum(Amizade),
  Pessoas = sum(Pessoas),
  `Aspectos técnicos` = sum (`Aspectos técnicos`),
  Acolhimento =sum(Acolhimento),
  `Identidade com o clube`= sum(`Identidade com o clube`),
  # CBM = sum(CBM),
  `Animação` = sum(`Animação`),
  `Ambiente familiar/descontraído` = sum(`Ambiente familiar/descontraído`),
  # `Reuniões` = sum(`Reuniões`)
) %>%
  as_tibble(column_name = 'Respostas') %>%
  pivot_longer(
    cols = everything(),
    values_to = 'Respostas',
    names_to = 'Palavras'
  ) %>%
  arrange(desc(Respostas)) |>
  xtable::xtable() |>
  flextable::as_flextable(include.rownames=FALSE) |>
  flextable::theme_booktabs()



