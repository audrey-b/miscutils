#' A function to render and preview markdown documents.
#'
#' This function provides a solution to the knit button in RStudio, which fails when working from a mounted disk. It has the
#' advantage over the rmarkdown::render function to provide a preview of the rendered document.
#' 
#' @param filename The name of the input file to be rendered. Generally an R Markdown document (.Rmd).
#' @param dir The directory where the input file is located.
#' @param output_format The R Markdown output format to convert to. Currently only "pdf_document" and "html_document" are implemented.
#' @param ... Arguments to pass to rmarkdown::render.
#' 
#' @export
#'

render2 <- function(filename, dir = getwd(), output_format = "pdf_document", ...){
  
  if(output_format == "pdf_document") format<-"pdf"
  if(output_format == "html_document") format<-"html"
  
  path <- file.path(dir, filename)
  
  filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filename))
  
  file <- basename(path)
  tempDir <- tempfile()
  dir.create(tempDir)
  newPath <- file.path(tempDir, file)
  file.copy(path, newPath)
  
  rmarkdown::render(newPath, output_format = output_format, ...)
  renderedPath <- file.path(tempDir, paste0(filename, ".", format))
  file.copy(renderedPath, file.path(dir, paste0(filename, ".", format)))
  
  viewer <- getOption("viewer")
  viewer(renderedPath)
  
}
