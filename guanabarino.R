library(tidyverse)

df <- read.delim("Conhecendo o(a) Guanabarino(a)  (respostas) - Respostas ao formulário 1.tsv")


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
    Amizade = str_count(key, 'amig|Amig|amiz'),
    Pessoas = str_count(key, 'pesso|Pess'),
    Tecnica = str_count(key, 'Técn|Tecn|tecn|técn'),
    Acolhimento = str_count(key, 'Acolh|acolh|acolhi'),
    Identidade = str_count(key, 'ident|Ident'),
    CBM = str_count(key, 'CBM|cbm'),
   `Animação` = str_count(key, 'Anima|anima|alto|Alto'),
    Ambiente = str_count(key, 'Ambie|ambie'),
   `Reuniões` = str_count(key, 'Reun|reun|reu|Reu')
  ) %>%
  reframe(
  Amizade = sum(Amizade),
  Pessoas = sum(Pessoas),
  Tecnica = sum (Tecnica),
  Acolhimento =sum(Acolhimento),
  Identidade = sum(Identidade),
  CBM = sum(CBM),
  `Animação` = sum(`Animação`),
  Ambiente = sum(Ambiente),
  `Reuniões` = sum(`Reuniões`)
) %>%
  as_tibble(column_name = 'Respostas') %>%
  pivot_longer(
    cols = everything(),
    values_to = 'Respostas',
    names_to = 'Palavras'
  ) %>%
  arrange(desc(Respostas))



