#' A function to render and preview markdown documents.
#'
#' This function provides an alternative to the Knit button in RStudio, which tends to fail when working from a mapped disk. 
#' It is based on the \code{\link[rmarkdown]{render}} function but also provides a preview of the rendered document like Knit.
#' 
#' @param input The path of the input file to be rendered. Generally an R Markdown document (.Rmd). Defaults to the active file in the RStudio editor.
#' @param replace A list of two strings. The first string will be replaced by the second one in the path. This can be used to solve issues when working from a mapped disk.
#' @param preview Whether to preview the rendered documents.
#' @param ... Arguments to pass to \code{\link[rmarkdown]{render}}. For example, output_format="all" will render all formats defined within the file.
#' 
#' @export
#'


render2 <- function(input = rstudioapi::getSourceEditorContext()$path,
                    replace = getOption("path.replace.list"),
                    preview = TRUE,
                    ...){
  
  filename <- basename(input)
  filenameraw <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename)
  dir <- normalizePath(dirname(input), winslash = "/") 
  
  if(!is.null(replace)){
    newdir <- sub(replace[[1]], replace[[2]], dir, fixed=TRUE)
    newdir <- sub("\\\\", "", newdir, fixed=TRUE)
  }
  
  newinput <- file.path(newdir, filename)
  
  tempDir <- tempfile()
  dir.create(tempDir)
  
  rmarkdown::render(newinput, output_dir = tempDir, ...)
  renderedFiles <- list.files(tempDir, full.names = TRUE)
  file.copy(renderedFiles, newdir, overwrite=TRUE)
  
  if(preview==TRUE){
  viewer <- getOption("viewer")
  for(i in length(renderedFiles):1){
    rstudioapi::viewer(renderedFiles[[i]])
  }}
  
}

