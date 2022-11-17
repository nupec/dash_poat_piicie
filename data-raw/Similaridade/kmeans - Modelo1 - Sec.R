library(fpc)
library(stats)
library(scatterplot3d)
library(purrr)
library(skimr)
library(cluster)

# Esta função cria k clusters para uma dataframe

# 2) Aplicação para o Secundário  ------------------------------------------------------------
baseSec<- readxl::read_excel("data/similaridade/baseModelo1.xlsx") |>
  dplyr::filter(ciclo == "sec")

base <- baseSec

v1 <- "ano"
v2 <- NULL
v3 <- "tx_ret"
v4 <- "equidade"
v5 <- NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(t, k){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})   #padroniza as variáveis
  df <- scale(df[2:3])
  base <- stats::kmeans(df, {{k}})
}

periodo <- 2017:2019
nperiodo <- length(periodo)
k <- 2:4
nk <- length(k)
aux <- NULL
baseCluster <- NULL

for (t in periodo) {
  for (k1 in k) {
    aux <- NULL
    aux <-  clusterGeral({{t}}, {{k1}})
    baseCluster <- cbind(baseCluster, aux)
    baseCluster <- cbind(baseCluster, {{k1}}, {{t}}) # calcula os clusters
  }
}

# Construindo a base final

cidades <- dplyr::select(base, "municipio") |>
  dplyr::pull() |> rep(nk) |>  as.data.frame()

colnames(cidades) <- "municipios"

kGeral <- NULL
aux <- NULL

i=1

for (i in seq(1, 3*nk-2 , 3)) {
  aux = cbind(baseCluster[[1, i  ]],
              baseCluster[[1, i+1]],
              baseCluster[[1, i+2]])

  kGeral = rbind(kGeral, aux)
}

kGeral <- kGeral |> as.data.frame()

kGeralBasico <- cbind(cidades, kGeral) |>
  dplyr::mutate(pais = "Portugal") |>
  dplyr::rename(nroClusters = V1,
                clusterS = V2,
                ano = V3) |>
  dplyr::relocate(pais, .before = municipios)

writexl::write_xlsx(kGeralBasico, "data/similaridade/modelo01CB.xlsx")


#writexl::write_xlsx(kGeral, "data/similaridade/ttaRetASE.xlsx")

# scatterplot3d::scatterplot3d(df$nro_aluno, df$niveis_negativos,
#                              df$inn)

