
# Modelo 1: Matricula, Retenção, equidade -----------------------------------------------------

baseModelo1 <- NULL
equidade <- NULL

matricula <- matriculaPT
retencao <- retencaoPT
equidade <- equidadeMedia

join1 <- dplyr::left_join(matricula, retencao, by = "chave") |>
  dplyr::select(1:6, 11)

join2 <- dplyr::left_join(join1, equidade,  by = "chave")

baseModelo1 <- join2 |>
  dplyr::select(1:7, 14) |>
  as.data.frame()

colnames(baseModelo1) <- c("chave", "dico", "ano", "municipio", "ciclo",
                           "tt_alunos", "tx_ret", "equidade")

#  Salavando baseModelo1
  # readr::write_rds(baseModelo1, "data/rds/baseModelo1.rds")
  # writexl::write_xlsx(baseModelo1, "data/similaridade/baseModelo1.xlsx")

# Modelo 2: -----------------------------------------------------------------------------------


