library(dplyr)

### Carregando as bases

### Carregando as bases
# Fonte: PORTAL INFOESCOLAS - 1 .º CICLO - ENSINO GERAL - DADOS POR NUTS III / MUNICÍPIO
#        Fonte: DGEEC/MEdu (Dados reportados pelas escolas ao sistema de informação do MEdu)
# Disponível em:

# url("https://infoescolas.pt/bds.asp")

# 1) Bases  -----------------------------------------------------------------------------------
cofinanciamento <- readxl::read_excel("data-raw/Cofinanciamento/Fundos_PIICIE_Educação.xls",
                                              sheet = "gisMUNICIPIOS") |>
  janitor::clean_names() |>
  dplyr::select(-c(4:10))

cofTt <- readxl::read_excel("data-raw/Cofinanciamento/Fundos_PIICIE_Educação.xls",
                                     sheet = "gisMunNutsIII") |>
  janitor::clean_names()

cofTtPivot <- cofTt

cofTt <- cofTtPivot

baseNN <- MatriculaBasSecD

CofTotal <- cofTt |>
  dplyr::group_by(regiao, local) |>
  dplyr::summarise(
    Total_Fin_Local = sum(cofinanciamento, na.rm = T)
  )



base <- cofinanciamento |>
  dplyr::rename(Total_FA = total_fa)

 munCof <- base[1] |> dplyr::pull()

# 2) Censo Portugal 2021 -------------------------------------------------------------------------

censo21 <- readxl::read_excel("data-raw/Cofinanciamento/Censos_2021.xls",
                              sheet = "censoMun") |>
  janitor::clean_names() |>
  dplyr::mutate(cod = as.numeric(cod))

subPop21Cof <- censo21 |>
 # dplyr::select(1:5) |>
  dplyr::mutate(cod = as.numeric(cod),
                x0_24_anos = x0_14_anos + x15_24_anos) |>
  dplyr::select(-c(6,7))

# 3) Cofinancimento - população ------------------------------------------------------------------

total_cofinanciamento = sum(cofinanciamento$total_fa)

baseCofPop <- dplyr::left_join(subPop21Cof, base, by = c("cod" = "dico"))|>
  dplyr::select(-7) |>
  dplyr::mutate(
    inv_0a14_spop_mun  = Total_FA/x0_24_anos*x0_14_anos,
    inv_15a24_spop_mun = Total_FA/x0_24_anos*x15_24_anos,
    inv_0a24_spop_mun  = Total_FA/x0_24_anos,
    inv_0a14_percapita  = Total_FA/sum(total)*x0_14_anos,
    inv_15a24_percapita = Total_FA/sum(total)*x15_24_anos,
    inv_percapita = Total_FA/sum(total)*x0_24_anos) |>
  dplyr::select(-c(3:6))

  baseCofPopTidy <- baseCofPop |>
  tidyr::pivot_longer(cols = c(inv_0a14_spop_mun:inv_percapita),
                      names_to = "Estatísticas",
                      values_to = "Valores")

  # readr::write_rds(baseCofPop, "data/rds/baseCofPop.rds")
  # writexl::write_xlsx(baseCofPop, "data/xlsx/baseCofPop.xlsx")
  # writexl::write_xlsx(baseCofPopTidy, "data/xlsx/baseCofPopTidy.xlsx")
   #
# 4) Cofinanciamento ciclo escolar ---------------------------------------------------------------
baseNNTotal <- matriculaPT |>
  janitor::clean_names() |>
#  dplyr::filter(dico %in% munCof) |>
  dplyr::group_by(dico, ano, municipio, ciclo2) |>
  dplyr::summarise(ciclo = sum(tt_alunos, na.rm =T)) |>
  dplyr::filter(ano == "2019/2020") |>
      dplyr::summarise(
        total_geral = sum(ciclo)
      )

 baseNNt <- dplyr::left_join(baseNNTotal, matriculaPT, by= "dico") |>
   dplyr::filter(ano.y=="2019/2020") |>
   dplyr::select(dico, municipio, ciclo2, tt_alunos, total_geral)

popMatCof <- dplyr::left_join(baseNNt, subPop21Cof, by = c("dico"="cod"))

# Cofinaciamento e ciclos escolares -----------------------------------------------------------

# popMatCof <- matriculaPT |>
#   dplyr::filter(dico %in% munCof) |>
#   tidyr::pivot_wider(
#     names_from = c(ciclo2),
#     values_from = tt_alunos) |>
#   dplyr::group_by(dico, ano, Município) |>
#   dplyr::summarise(
#     ciclo_basico = sum(cb, na.rm=T),
#     secundario   = sum(sec, na.rm = T)
#   ) |>
#   dplyr::mutate(
#     total = ciclo_basico+secundario
#   ) |>
#   dplyr::filter(ano == "2019/2020")

baseCofCiclo <- dplyr::left_join(popMatCof, base, by = "dico") |>
  dplyr::select(1:5, 10,12) |>
  dplyr::mutate(
    inv_ciclo = Total_FA/total_geral*tt_alunos
  )

 baseCofCicloTidy <- baseCofCiclo |>
   tidyr::pivot_longer(
       cols = tt_alunos:inv_ciclo,
       names_to = "Tipo de Investimento",
       values_to = "$"
   )

 # readr::write_rds(baseCofCiclo, "data/rds/baseCofCiclo.rds")
 # writexl::write_xlsx (baseCofCiclo, "data/xlsx/baseCofCiclo.xlsx")
