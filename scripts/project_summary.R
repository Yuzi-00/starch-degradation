#' Create a project summary HTML page
#'
#' Consistent starting point for Data School participants creating
#' a project summary page.
#'
#' @param ... Options passed through to \code{\link[bookdown]{html_document2}}
#'
#' @return R Markdown output format
#' @export
#'
#' @examples
#' render("input.Rmd", project_summary())
project_summary <- function(..., number_sections = FALSE) {

  pkg_resource = function(...) {
    system.file(..., package = "DSreport")
  }

  template = pkg_resource("rmarkdown/templates/project/skeleton/resources/template.html")

  bookdown::html_document2(
    template = template,
    number_sections = number_sections,
    ...
  )

}
