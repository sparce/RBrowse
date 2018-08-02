#' UI for an RBrowse entity plot
#'
#' The \code{entityPlot} provides a generic interface for producing plots from
#' genomic entities (genes, etc.).
#'
#' @param id Shiny id of the plot. This must be unique within your app.
#'
#' @return A set of shiny components comprising an RBrowse entity plot
#' @export
entityPlotUI <- function(id) {
    ns = shiny::NS(id)

    shiny::fluidRow(
        shiny::uiOutput(ns("options_dropdown")),
        plotly::plotlyOutput(ns("entity_plot"))
    )
}


#' Server code for an RBrowse entity plot
#'
#' This produces the server code necessary for an RBrowse \code{entityPlot} to function.
#' It is not called directly, but rather using the \code{\link[shiny]{callModule}} function
#' from shiny.
#'
#' @param input The shiny input object. Passed automatically from \code{callModule}
#' @param output The shiny output object. Passed automatically from \code{callModule}
#' @param session The shiny session object. Passed automatically from \code{callModule}
#'
#' @param data The data to be passed in for the plot. This may be a file path or
#' an in-memory object depending on how the \code{data_fn} is written
#' @param data_fn A function that takes \code{data} and prepares it for plotting.
#' Will be run in a reactive context, so can access input options
#' @param plot_fn A function that takes the output of \code{data_fn} and produces
#' a ggplot plot that will eventually be converted with \code{\link[plotly]{ggplotly}}.
#' Will be run in a reactive context, so can access input options
#' @param options An optional \code{\link[shiny]{tagList}} containing input elements used as
#' configuration options for the entity plot. These are available to be used in the
#' \code{data_fn} and \code{plot_fn} and will be accessible through a dropdown box
#' @param overview The return value from calling the \code{\link{overviewPlot}} module.
#' This provides the context necessary for producing the plot (eg. what genomic range is being
#' viewed to filter only relevant genes).
#'
#' @return A reactive list of plot features. For now only returns the ggplot from which
#' the plotly plot is produced.
#'
#' @export
entityPlot <- function(input, output, session, data, data_fn, plot_fn, options = NULL, overview) {
    ns <- session$ns

    output$options_dropdown <- shiny::renderUI({
        shiny::validate(shiny::need(options(), message = F))

        shiny::div(style = "position: absolute; z-index: 1;",
                   shinyWidgets::dropdown(
                       options(),
                       style = "unite",
                       icon = icon("gear"),
                       status = "royal",
                       #width = "80%",
                       animate = shinyWidgets::animateOptions(enter = shinyWidgets::animations$fading_entrances$fadeInLeft, exit = shinyWidgets::animations$fading_exits$fadeOutLeft, duration = 0.3)
                   )
        )
    })

    data_r <- shiny::reactive({ data_fn(data) })

    plot_r <- shiny::reactive({ plot_fn(data_r())})

    plotly_r <- shiny::reactive({
        req(data_r(), plot_r())

        plotly::ggplotly(plot_r(), tooltip = "text")
    })

    output$entity_plot <- plotly::renderPlotly({
        plotly_r()
    })

    return(shiny::reactive(list(plot = plot_r())))

}
