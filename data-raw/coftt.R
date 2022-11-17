
coftt <- readxl::read_xlsx("data/xlsx/CofTt.xlsx")

cofttTidy <- coftt |>
  tidyr::pivot_longer(
    cols = c(COF_MUN,
             COF_NUTSIII,
             `COF_NUTSIII+MUN`,
             `Investimento Pré- Escola`,
             `Investimento Básico`,
             `Investimento Secundário`),
    names_to = "Cofinanciamento",
    values_to = "valores"
  )

#writexl::write_xlsx(cofttTidy, "data/xlsx/cofttTidy.xlsx")
