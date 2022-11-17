equidade <- equidadeMedia |>
  dplyr::filter(municipio == "Município")

baseModeloMR <- dplyr::left_join(MatriculaBasSecD, retencaoPT,
                                 by = "chave")

baseModeloME <- dplyr::left_join(matriculaPT, equidade,
                                 by = "chave")


baseModelo1 <- dplyr::left_join(baseModeloME, baseModeloMR,
                                 by = "chave")

baseModeloCof <- readxl::read_xlsx("data/similaridade/baseModeloCof.xlsx")

baseModelo <- dplyr::left_join(baseModelo1, baseModeloCof,
                                by = "chave")

# baseModelo <- dplyr::select(baseModelo, ano.x.x, Município, total_matricula.x,
#               media_retençao, tt_ASE, equidade_media) |>
#   dplyr::rename(ano =  ano.x.x,
#                 total_alunos = total_matricula.x
#                 )

#  readr::write_rds(baseModelo, "data/rds/baseModelo.rds")
# #
 #writexl::write_xlsx(baseModelo, "data/similaridade/baseModelo3.xlsx")
