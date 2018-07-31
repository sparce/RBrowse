#' Run RBrowse demo
#'
#' This crashes out to the debug console when exiting the function, but not when you
#' just run the \code{runApp} call separately. Don't know why yet.
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
