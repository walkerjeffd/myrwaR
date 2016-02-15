library(myrwaR)

# precip_logan <- load_precip_from_xls(system.file("extdata", "LoganPrecip.xlsx", package = "myrwaR"))
precip_logan <- load_precip_from_xls("D:/Dropbox/Work/mystic/_db/Processed/Precip/LoganPrecip.xlsx")
precip_logan <- precip_logan[!is.na(precip_logan$Precip), ]

devtools::use_data(precip_logan, overwrite = TRUE)
