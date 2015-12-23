# Load WQX Domain Values from CSV Files
# CSV Files Extracted from Physical-Chemical Template v1.04

folder <- system.file("extdata/wqx_domain", package = "myrwaR")
files <- list.files(folder)

wqx_domain <- lapply(files, function (file) {
  path <- file.path(folder, file)
  x <- read.csv(path, stringsAsFactors = FALSE)
  names(x) <- gsub('[.]', '', names(x))
  x
})
names(wqx_domain) <- gsub('.csv', '', files)

devtools::use_data(wqx_domain, overwrite = TRUE)
