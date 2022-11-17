## code to prepare `baseRet` dataset goes here

## Base PNUD usada nos exemplos

# pnud <-  readr::read_rds("data-raw/pnud_min.rds")
# usethis::use_data(pnud, overwrite = TRUE)

## Importando a base Níveis Negativos

 baseNNGeral <- readxl::read_xlsx("data/xlsx/baseNNGeral.xlsx")
 usethis::use_data(baseNNGeral, overwrite = TRUE)

# baseNN <- readxl::read_xlsx("../db_master/dataset/baseNN.xlsx")
# usethis::use_data(baseNN, overwrite = TRUE)

## Importando a base Retenção

baseRETGeral <- readxl::read_xlsx("data/xlsx/baseRETAMP.xlsx")
usethis::use_data(baseRETGeral, overwrite = TRUE)

# Importando as bases tratadas para o modelo kmeans

matriculaPT <- readr::read_rds("data/rds/baseMatricula.rds")
usethis::use_data(matriculaPT, overwrite = TRUE)

MatriculaBasSecD <- readr::read_rds("data/rds/baseMatriculaBasSecDa.rds")
usethis::use_data(MatriculaBasSecD, overwrite = TRUE)

retencaoPT <- readr::read_rds("data/rds/baseRetencao.rds")
usethis::use_data(retencaoPT, overwrite = TRUE)

cofinanciamentoPT <- readr::read_rds("data/rds/baseCofinanciamento.rds")
usethis::use_data(cofinanciamentoPT, overwrite = TRUE)

equidadePT <- readr::read_rds("data/rds/baseEquidade.rds")
usethis::use_data(equidadePT, overwrite = TRUE)

equidadeMedia <- readr::read_rds("data/rds/equidadeMedia.rds")
usethis::use_data(equidadeMedia, overwrite = TRUE)

