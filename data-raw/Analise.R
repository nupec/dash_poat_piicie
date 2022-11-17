library(highcharter)
library(devtools)
library(fdth)
library(gtsummary)
library(patchwork)
library(graphics)
library(tidyverse)
library(skimr)
library(tidymodels)
library(knitr)
library(ggrepel)
library(gt)
library(DT)
library(cowplot)
library(rstatix)
library(emmeans)
library(kableExtra)
library(htmltools)
library(lubridate)
library(scales)
library(scatterplot3d)


# 1) Carregando as base de dados --------------------------------------------------------------
baseNN <- readr::read_rds("data/rds/baseNN.rds")  |>
  dplyr::mutate(rnn = round(nn/matricula, 3)) |>
  tidyr::drop_na(ciclo) |>
  janitor::clean_names() |>
  dplyr::mutate(
    Ano = make_date(stringr::str_sub(anoescolar, 1,4)),
  ) |>
  dplyr::relocate(Ano, .before = anoescolar)

baseNN <- baseNN |>
  dplyr::mutate(
    ciclo = stringr::str_replace_all(baseNN$ciclo,
                                     c("cb2" = "2CEB",
                                       "cb3" = "3CEB")),
    anoescolar = stringr::str_replace_all(baseNN$anoescolar,
                                          c("2014/15" = "2014/2015",
                                            "2015/16" = "2015/2016",
                                            "2016/17" = "2016/2017",
                                            "2017/18" = "2017/2018")))
tabelaCidadesNN <- baseNN |>
  dplyr::group_by(Ano, municipio) |>
  dplyr::summarise(media_TNN = mean(rnn, na.rm = TRUE),
                   tt_aluno_nn= sum(matricula),
                   tt_nn     = sum(nn))



tabelaCidCicloNN <- baseNN |>
  dplyr::group_by(Ano, municipio, ciclo) |>
  dplyr::summarise(media_rnn = round(mean(rnn, na.rm = TRUE),3))

# grafico temporal 1

tabelaCidadesNN |>
  dplyr::group_by(Ano, municipio) |>
  dplyr::summarise(
    TNN = mean(media_TNN)
  ) |>
  ggplot() +
  aes(x = Ano, y = TNN, color = municipio) +
  geom_line() +
  geom_point()

# grafico temporal 2 TNN

tabelaCidCicloNN |>
  dplyr::group_by(Ano, ciclo) |>
  dplyr::summarise(
    TNN = mean(media_rnn)
  ) |>
  ggplot() +
  aes(x = Ano, y = TNN, color = ciclo) +
  geom_line() +
  geom_point()

# Estatísticas descritivas

baseNN |>
  tibble::rownames_to_column() |>
  ggplot(aes(matricula, nn, color = ciclo )) +
  geom_point() +
  labs(
    #title = "Figura xa: Boxplot da TNN para os 17 Municípios da AMP, 2014/15 a 2018/19",
    #subtitle = "2º Ciclo do Ensino Básico",
    x = "Total de alunos por ano curricular",
    y = "Níveis Negativos",
    size = 8)

baseNN |>
  tibble::rownames_to_column() |>
  ggplot(aes(matricula, nn,color = anocurricular )) +
  geom_point() +
  labs(
    #title = "Total de Alunos por Ano Escolar X Níveis Negativos, 2014/15-2018/19 ",
   # subtitle = "Escolas da AMP",
    x = "Total de Alunos por ano curricular",
    y = "Níveis Negativos", size = 8)


# Tabela

dados_summ <- baseNN |>
    as_tibble() |>
    group_by(Ano, municipio, ciclo) |>
    summarise(
      `Média TNN` = mean(rnn, na.rm=T))|>
    tidyr::pivot_wider(
      names_from = Ano,
      values_from =  `Média TNN`
    )


dados_summ |>
  # cria tabela base
  gt(rowname_col = "ciclo")|>
  # adiciona headers
  tab_header(
    title = "Taxa de Níveis Negativos",
    subtitle = "Área Metroplitana do Porto, por ano censitário"
  ) |>
  # cantinho
  tab_stubhead("Municípios/Ciclo") %>%
  # rotulos das colunas
  cols_label(
    `2014-01-01` = "2014/15",
    `2015-01-01` = "2015/16",
    `2016-01-01` = "2016/17",
    `2017-01-01` = "2017/18",
    `2018-01-01` = "2018/19"
  ) |>
# %>%
#   tab_spanner(
#     label = "Índice de Gini",
#     columns = starts_with("gini_")
#   ) %>%
#   # edita colunas
#   fmt_number(
#     columns = starts_with("pop_"),
#     sep_mark = ".",
#     dec_mark = ",",
#     decimals = 0,
#     suffixing = "K"
#   ) %>%
  fmt_percent(
    columns = starts_with("20"),
    decimals = 1,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  # fmt_number(
  #   columns = starts_with("gini_"),
  #   sep_mark = ".",
  #   dec_mark = ",",
  #   decimals = 3,
  # ) %>%
  # rodapé
  tab_source_note(
    "Fonte:Ministério da Educação"
  )


# Gráfico 3d


base<- readxl::read_excel("data/similaridade/baseModelo1.xlsx")

# scatterplot3d(baseBasico$tt_alunos,  xlab = "Total de alunos",
#               baseBasico$tx_ret, ylab = "Taxa de Retenção",
#               baseBasico$equidade, zlab = "Índice de Equidade",
#               highlight.3d=TRUE,
#               col.axis="black", col.grid="lightblue",
#               main="Total de Alunos X TRD X Equidade ", pch=20)



### Retenção

ret <- baseRETGeral |>
  dplyr::mutate(
    ANO = as.numeric(ANO),
    TRD = retAlunos/qtdeAlunos)

 tabRet1 <-  ret |>
   dplyr::filter(NUTSIII_2013 == "Área Metropolitana do Porto") |>
  dplyr::group_by(ANO, Município, ciclo) |>
  dplyr::summarise(
    TRD_media = mean(TRD)
  )

 tabRet1 |>
   dplyr::group_by(ANO, ciclo) |>
   dplyr::summarise(
     TRD = mean(TRD_media)) |>
   ggplot() +
   aes(x = ANO, y = TRD, color = ciclo) +
   geom_line() +
   geom_point()

 tabRet2 <- ret |>
   dplyr::filter(NUTSIII_2013 == "Área Metropolitana do Porto")

 #library(ggstatsplot)

 tabRet2 |>
 dplyr::filter(ciclo == "1CEB") |>
   dplyr::group_by(Município, ciclo) |>
   ggplot() +
   geom_boxplot(aes(x = TRD, y = Município),
                outlier.colour = "red")

 (CEB1_SOL <- subset(tabRet2, tabRet2$TRD > 0 & tabRet2$TRD < 0.7) |>
   dplyr::filter(ciclo == "1CEB") |>
   dplyr::group_by(Município, ciclo) |>
   ggplot() +
   geom_boxplot(aes(x = TRD, y = Município),
                outlier.colour = "red") +
     labs(
       title = "TRD para os 17 Municípios da AMP",
       subtitle = "1º Ciclo do Ensino Básico, 2014/15 a 2018/19",
       x = "TNN",
       y = "Municípios da AMP", size = 4))


 (CEB2_SOL <- subset(tabRet2, tabRet2$TRD > 0 & tabRet2$TRD < 0.7) |>
   dplyr::filter(ciclo == "2CEB") |>
   dplyr::group_by(Município, ciclo) |>
   ggplot() +
   geom_boxplot(aes(x = TRD, y = Município),
                outlier.colour = "red")+
     labs(
       title = "TRD para os 17 Municípios da AMP",
       subtitle = "2º Ciclo do Ensino Básico, 2014/15 a 2018/19",
       x = "TNN",
       y = "Municípios da AMP", size = 4))


 (CEB3_SOL <- subset(tabRet2, tabRet2$TRD > 0 & tabRet2$TRD < 0.7) |>
   dplyr::filter(ciclo == "3CEB") |>
   dplyr::group_by(Município, ciclo) |>
   ggplot() +
   geom_boxplot(aes(x = TRD, y = Município),
                outlier.colour = "red")+
     labs(
       title = "TRD para os 17 Municípios da AMP",
       subtitle = "3º Ciclo do Ensino Básico, 2014/15 a 2018/19",
       x = "TNN",
       y = "Municípios da AMP", size = 4))


 (SEC_SOL <- subset(tabRet2, tabRet2$TRD > 0 & tabRet2$TRD < 0.7) |>
     dplyr::filter(ciclo == "SEC") |>
     dplyr::group_by(Município, ciclo) |>
     ggplot() +
     geom_boxplot(aes(x = TRD, y = Município),
                  outlier.colour = "red")+
     labs(
       title = "TRD para os 17 Municípios da AMP",
       subtitle = "Ensino Secundário, 2014/15 a 2018/19",
       x = "TNN",
       y = "Municípios da AMP", size = 4))



# TNN -----------------------------------------------------------------------------------------

 baseNN <- readr::read_rds("data/rds/baseNN.rds")  |>
   dplyr::mutate(rnn = round(nn/matricula, 3)) |>
   tidyr::drop_na(ciclo) |>
   janitor::clean_names() |>
   dplyr::mutate(
     Ano = stringr::str_sub(anoescolar, 1,4),
   ) |>
   dplyr::relocate(Ano, .before = anoescolar)

 baseNN2 <- baseNN |>
   dplyr::mutate(
     ciclo = stringr::str_replace_all(baseNN$ciclo,
                                      c("cb2" = "2CEB",
                                        "cb3" = "3CEB")),
     anoescolar = stringr::str_replace_all(baseNN$anoescolar,
                                           c("2014-15" = "2014/2015",
                                             "2015-16" = "2015/2016",
                                             "2016-17" = "2016/2017",
                                             "2017-18" = "2017/2018")))
 tabelaCidadesNN <- baseNN |>
   dplyr::group_by(anoescolar, municipio) |>
   dplyr::summarise(media_rnn = mean(rnn, na.rm = TRUE),
                    tt_aluno_nn= sum(matricula),
                    tt_nn     = sum(nn))

 tabelaCidCicloNN <- baseNN2 |>
   dplyr::group_by(anoescolar, municipio, ciclo) |>
   dplyr::summarise(media_rnn = round(mean(rnn, na.rm = TRUE),3))

 baseFiltrada <- dplyr::select(baseNN2, c(2,8, 13:17))

 bpcb2 <- baseNN |>
   dplyr::filter(ciclo=="cb2") |>
   dplyr::group_by(municipio, ciclo, size = 4) |>
   ggplot() +
   geom_boxplot(aes(x = rnn, y = municipio),
                outlier.colour = "red") +
   labs(
     title = "TNN para os 17 Municípios da AMP, 2014/15 a 2019/20",
     subtitle = "2º Ciclo do Ensino Básico",
     x = "TNN",
     y = "Municípios da AMP", size = 4)


 bpcb2

 # Cosico Básico 3

 bpcb3 <- baseNN |>
   dplyr::filter(ciclo=="cb3") |>
   dplyr::group_by(municipio, ciclo) |>
   ggplot() +
   geom_boxplot(aes(x = rnn, y = municipio),
                outlier.colour = "red")+
   labs(
     title = "TNN para os 17 Municípios da AMP, 2014/15 a 2019/20",
     subtitle = "3º Ciclo do Ensino Básico",
     x = "TNN",
     y = "Municípios da AMP", size = 4)

 bpcb3

 bp2 <- baseNN |>
   dplyr::group_by(municipio, ciclo) |>
   ggplot() +
   geom_boxplot(aes(x = rnn, y = ciclo),
                outlier.colour = "red")


 (bpcb2 + bpcb3)
 (CEB1_SOL+ CEB2_SOL)
 (CEB3_SOL+SEC_SOL)


# NN 2014/2019 --------------------------------------------------------------------------------

 baseNN <- baseNNGeral |>
   dplyr::mutate(TNN = round(NIVEIS_NEGATIVOS/NRO_ALUNO, 3))

  tabelaCidadesNN <- baseNN |>
   dplyr::group_by(ANO, Município) |>
   dplyr::summarise(media_TNN = mean(TNN, na.rm = TRUE))

  tabelaCidCicloNN <- baseNN |>
    dplyr::group_by(ANO, Município, CICLO) |>
    dplyr::summarise(TNN = round(mean(TNN, na.rm = TRUE),3))

# grafico temporal 1
  library(ggplot2)

 tabelaCidadesNN |>
   dplyr::group_by(ANO, Município)|>
   dplyr::summarise(
     TNN = mean(media_TNN)
   ) |>
   ggplot() +
   aes(x = ANO, y = TNN, color = Município) +
   geom_line() +
   geom_point()

 # grafico temporal 2 TNN

 tabelaCidCicloNN |>
   dplyr::group_by(ANO, CICLO) |>
   dplyr::summarise(
     TNN = mean(TNN)
   ) |>
   ggplot() +
   aes(x = ANO, y = TNN, color = CICLO) +
   geom_line() +
   geom_point()
