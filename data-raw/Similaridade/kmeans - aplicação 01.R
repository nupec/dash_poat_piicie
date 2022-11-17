library(fpc)
library(stats)
library(scatterplot3d)
library(purrr)
library(skimr)
library(cluster)

# Esta função cria k clusters para uma dataframe

base <- readxl::read_excel("data/similaridade/baseModelo.xlsx")

v1 <- "ano"
v2 <- "total_alunos"
v3 <- "media_retençao"
v4 <- NULL
v5 <- NULL


df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                                                        dplyr::all_of(v4)) |>
  dplyr::mutate(
    ano = stringr::str_remove(base$ano, c("/2018", "/2019", "/2020"))
  ) #|> tidyr::drop_na()

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

kGeral <- NULL
aux <- NULL

i=1

cidades <- unique(base$Município) |>
  rep(nperiodo*nk) |>
  data.frame()

colnames(cidades) <- "Município"

for (i in seq(1, 3*nk*nperiodo-2, 3)) {
  aux = cbind(baseCluster[[1, i  ]],
              baseCluster[[1, i+1]],
              baseCluster[[1, i+2]])


  kGeral = rbind(kGeral, aux)

}

kGeral <- kGeral |>
  as.data.frame()

kGeral <- cbind(cidades, kGeral) |>
  dplyr::mutate(País = "Portugal") |>
  dplyr::relocate(País, .before = Município) |>
  dplyr::rename(Cluster = V1,
                NroK = V2,
                Ano = V3) |>
  dplyr::relocate(Cluster, .after = NroK)

# writexl::write_xlsx(kGeral, "data/similaridade/ttaRetASE.xlsx")

# scatterplot3d::scatterplot3d(df$nro_aluno, df$niveis_negativos,
#                              df$inn)

