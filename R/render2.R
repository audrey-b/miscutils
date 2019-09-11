#' A function to render and preview markdown documents.
#'
#' This function provides a solution to the knit button in RStudio, which fails when working from a mapped disk. It has the
#' advantage over the rmarkdown::render function to provide a preview of the rendered document.
#' 
#' @param input The path of the input file to be rendered. Generally an R Markdown document (.Rmd). Defaults to the active file in the RStudio editor.
#' @param replace A list of two strings. The first string will be replaced by the second one in the path. This can be used to solve issues when working from a mapped disk.
#' @param output_format The R Markdown output format to convert to. Currently only "pdf_document" and "html_document" are supported
#' @param ... Arguments to pass to rmarkdown::render.
#' 
#' @export
#'
#'
#'
#'

render2 <- function(input = rstudioapi::getSourceEditorContext()$path,
                    replace = getOption("path.replace.list"),
                    output_format = "pdf_document", 
                    ...){
  
  if(output_format == "pdf_document") format<-"pdf"
  if(output_format == "html_document") format<-"html"
  
  filename <- basename(input)
  filenameraw <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename)
  dir <- normalizePath(dirname(input), winslash = "/") 
  
  if(!is.null(replace)){
    newdir <- sub(replace[[1]], replace[[2]], dir, fixed=TRUE)
    newdir <- sub("\\\\", "", newdir, fixed=TRUE)
  }
  
  renderedPath <- file.path(newdir, paste0(filenameraw, ".", format))
  newinput <- file.path(newdir, filename)
  
  rmarkdown::render(newinput, output_format = output_format, ...)
  # file <- basename(renderedPath)
  # tempDir <- tempfile()
  # dir.create(tempDir)
  # renderedFile <- file.path(tempDir, file)
  # file.copy(renderedPath, renderedFile)
  # viewer <- getOption("viewer")
  # viewer(renderedFile)
  
  viewer <- getOption("viewer")
  viewer(renderedPath)
}

