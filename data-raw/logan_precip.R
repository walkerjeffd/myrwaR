library(myrwaR)

precip_logan <- load_precip_from_xls(system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR"))
precip_logan <- subset(precip_logan, !is.na(Precip))

devtools::use_data(precip_logan, overwrite = TRUE)
