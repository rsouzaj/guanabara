df <- read.delim("Conhecendo o(a) Guanabarino(a)  (respostas) - Respostas ao formulário 1.tsv")

 df_fim <-  df |>
    rename(
      n_frequenta = `Se.não.frequenta..qual.o.motivo.`,
      frequencia = Frequenta.a.sede.do.CEG.com.que.frequência.
    ) |>
  mutate(
    Idade = case_when(
      Qual.sua.idade. >= 30 & Qual.sua.idade.< 40 ~ 'Entre 30 e 39 anos',
      Qual.sua.idade. >= 40 & Qual.sua.idade.< 50 ~ 'Entre 40 e 49 anos',
      Qual.sua.idade. >= 50 & Qual.sua.idade. < 60 ~'Entre 50 e 59 anos',
      Qual.sua.idade.>= 60 ~ 'Mais de 60 anos'
    ),
    Trabalho = if_else(str_detect(n_frequenta, "Trabalho"), 1, 0),
    Frequência = case_when(
     frequencia == 'Menos de 1 vez por mês' ~'Menos de 1 vez por mês',
     frequencia == 'Duas vezes ou mais por mês' ~ 'Duas vezes ou mais por mês',
     frequencia == 'Uma vez por mês' ~ 'Uma vez por mês',
     frequencia == 'Não frequento há mais de 6 meses' ~ 'Não frequento há mais de 6 meses'
    ),
    Frequência = if_else( str_detect(frequencia, 'Sempre|Uma vez por semana') , 'Duas vezes ou mais por mês', frequencia),
    Frequência = if_else( str_detect(frequencia, 'às vezes|apenas 2 vezes|Depende') , 'Menos de 1 vez por mês', Frequência),
    Frequência = fct_relevel(Frequência,
      c('Duas vezes ou mais por mês', 'Uma vez por mês','Menos de 1 vez por mês', 'Não frequento há mais de 6 meses'
    )),
    `Tempo de clube` = fct_relevel(Há.quantos.anos.ingressou.no.clube.,
      c('< 2 anos', 'Entre 2 e 5 anos','Entre 5 e 10 anos', 'Entre 10 e 20 anos',
      '> 20 anos'
    )),
    `CBM` = fct_relevel(Você.fez.CBM,
                        c('No CEG', 'Em outro clube', 'Não fiz CBM')),
    identificacao = if_else(str_detect(
      Dentro.do.montanhismo.com.o.que.você.mais.se.identifica., 'trilha leve'), 'Trilhas leves', Dentro.do.montanhismo.com.o.que.você.mais.se.identifica.),
    identificacao = if_else(str_detect(
      identificacao, 'urbana') & str_detect(identificacao, 'remota'), 'Escalada urbana e remota', identificacao),
    identificacao = if_else(str_detect(
      identificacao, 'Aventura|todos os luga|opções acima|todos os tipos|Escalada e trilhas'), 'Outras respostas', identificacao
    ),
    identificacao = fct_relevel(identificacao,
      c('Escalada urbana', 'Escalada em áreas remotas', 'Escalada urbana e remota',
        'Trilhas leves','Trilhas moderadas', 'Trilhas pesadas', 'Outras respostas')
    ),
    `Escaladas/mês` = as.numeric(Quantas.vezes.você.escala.por.mês..em.média.),
    `Caminhadas/mês` = as.numeric(Quantas.vezes.você.pratica.caminhada.em.montanha.por.mês..em.média.)

  )



 df_fim |>
   select(Idade, `Tempo de clube`, CBM, Frequência) |>
  gtsummary::tbl_summary()


 df_fim |>
   mutate(
     `Tempo de clube`= case_when(
       `Tempo de clube` == '< 2 anos' ~ '< 5 anos',
       `Tempo de clube` == 'Entre 2 e 5 anos'~ '< 5 anos',
       `Tempo de clube` =='Entre 5 e 10 anos' ~ 'Entre 5 e 10 anos',
       `Tempo de clube` =='> 20 anos'  ~  '> 10 anos',
       `Tempo de clube` == 'Entre 10 e 20 anos' ~  '> 10 anos'
     ),
     `Tempo de clube` = fct_relevel(`Tempo de clube`,
                                    c('< 5 anos', 'Entre 5 e 10 anos','> 10 anos' ))
   ) |>
   select(Qual.sua.idade., `Tempo de clube`, CBM, Frequência) |>
   gtsummary::tbl_summary(by = Frequência)



 df_fim |>
   mutate(
   `Tempo de clube`= case_when(
     `Tempo de clube` == '< 2 anos' ~ '< 5 anos',
     `Tempo de clube` == 'Entre 2 e 5 anos'~ '< 5 anos',
     `Tempo de clube` =='Entre 5 e 10 anos' ~ 'Entre 5 e 10 anos',
     `Tempo de clube` =='> 20 anos'  ~  '> 10 anos',
     `Tempo de clube` == 'Entre 10 e 20 anos' ~  '> 10 anos'
   ),
   `Tempo de clube` = fct_relevel(`Tempo de clube`,
                                  c('< 5 anos', 'Entre 5 e 10 anos','> 10 anos' ))
   )|>
   ggplot(aes(Frequência, fill = `Tempo de clube`))+
   geom_bar(position = 'dodge')+
   # theme_classic()+
   labs(
     y = 'Número de respondentes'
   )+
   scale_y_continuous(breaks = seq(0, 16, by=2))+
   theme(panel.background = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),
         axis.line = element_line('black'))




 df_fim %>%
   ggplot(aes(identificacao))+
   geom_bar(fill = '#00B898')+
   # theme_classic()+
   labs(
     y = 'Número de respondentes',
     x = 'Identidade com a montanha'
       )+
   # coord_flip()+
   scale_y_continuous(breaks = seq(0, 16, by=2))+
   theme(panel.background = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),
         axis.line = element_line('black'))


  df |>
    ggplot(aes(Frequência, fill = Idade))+
    geom_bar(position = 'dodge')+
    coord_flip()+
    theme_classic()





df |>
  select(Qual.sua.idade.,Frequenta.a.sede.do.CEG.com.que.frequência.,
         Há.quantos.anos.ingressou.no.clube.) |>
  gtsummary::tbl_summary(
    by= `Frequenta.a.sede.do.CEG.com.que.frequência.`,

  )
