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
  dplyr::group_by(municipio, ciclo, size = 8) |>
  ggplot() +
  geom_boxplot(aes(x = rnn, y = municipio),
               outlier.colour = "red") +
  labs(
    title = "Boxplot da TNN para os 17 Municípios da AMP, 2014/15 a 2019/20",
    subtitle = "2º Ciclo do Ensino Básico",
    x = "TNN",
    y = "Municípios da AMP", size = 8)


bpcb2

# Cosico Básico 3

bpcb3 <- baseNN |>
  dplyr::filter(ciclo=="cb3") |>
  dplyr::group_by(municipio, ciclo) |>
  ggplot() +
  geom_boxplot(aes(x = rnn, y = municipio),
               outlier.colour = "red")+
  labs(
    title = "Boxplot da TNN para os 17 Municípios da AMP, 2014/15 a 2019/20",
    subtitle = "3º Ciclo do Ensino Básico",
    x = "TNN",
    y = "Municípios da AMP", size = 8)

bpcb3

bp2 <- baseNN |>
  dplyr::group_by(municipio, ciclo) |>
  ggplot() +
  geom_boxplot(aes(x = rnn, y = ciclo),
               outlier.colour = "red")

(bpcb2 + bpcb3)


