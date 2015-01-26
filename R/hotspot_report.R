#' Hotspot Report Template
#' 
#' Rmarkdown template for hotspot reporting
#' 
#' @export
hotspot_report <- function(keep_tex=TRUE) {
  template <- system.file("rmarkdown", "templates", "hotspot_report", "resources", "template.tex",
                          package = "myrwaR")
  
  base <- rmarkdown::pdf_document(template = template,
                                  keep_tex = keep_tex,
                                  includes = NULL,
                                  fig_caption=TRUE)
  
  base$knitr$opts_knit$out.format <- "sweave"
  base$knitr$knit_hooks$plot <- knitr:::hook_plot_tex
  
  base
}