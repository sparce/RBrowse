#' Run RBrowse demo
#'
#' @param demo Name of the demo to run.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'     RBrowse_demo()
#' }
RBrowse_demo <- function(demo = "simple-demo", ...) {
    app_file = system.file("shiny-examples", demo, paste0(demo, ".R"), package = "RBrowse")

    shiny::runApp(app_file, ...)
}
