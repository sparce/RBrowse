#' UI for an RBrowse tracked plot
#'
#' The \code{trackedPlot} provides a generic interface for producing plots from
#' position based genomic data.
#'
#' @param id
#'
#' @return A set of shiny components comprising an RBrowse tracked plot
#' @export
#'
#' @examples
trackedPlotUI <- function(id) {
    ns = NS(id)

    fluidRow(
        uiOutput(ns("options_dropdown")),
        plotly::plotlyOutput(ns("tracked_plot"))
    )
}


#' Server code for an RBrowse tracked plot
#'
#' This produces the server code necessary for an RBrowse \code{trackedPlot} to function.
#' It is not called directly, but rather using the \code{\link[shiny]{callModule}} function
#' from shiny. Before designing a custom visualisation with \code{trackedPlot}
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
#' configuration options for the tracked plot. These are available to be used in the
#' \code{data_fn} and \code{plot_fn} and will be accessible through a dropdown box
#' @param overview The return value from calling the \code{\link{overviewPlot}} module.
#' This is necessary to provide the automatic range adjustment behaviour and the overlay of a selected gene
#'
#' @return A reactive list of plot features. For now only returns the ggplot from which
#' the plotly plot is produced.
#' @export
#'
#' @examples
trackedPlot <- function(input, output, session, data, data_fn, plot_fn, options = NULL, overview) {
    ns <- session$ns

    output$options_dropdown <- renderUI({
        validate(need(options(), message = F))

        div(style = "position: absolute; z-index: 1;",
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

    data_r <- reactive({ data_fn(data) })

    selection_box <- reactiveVal(NULL)

    plot_r <- reactive({ plot_fn(data_r()) + selection_box() })

    plotly_r <- reactive({
        req(data_r(), plot_r())

        plotly::ggplotly(
            plot_r() +
                coord_cartesian(xlim = c(overview()$range_min, overview()$range_max))+
                scale_x_continuous(expand = c(0,0))
            , tooltip = "text") %>%
            plotly::layout(
                showlegend = F,
                title = plot_r()$labels$y,
                titlefont = list(size = 12),
                xaxis = list(
                    title = NA,
                    fixedrange = T,
                    autorange = F,
                    tickmode = 'auto',
                    nticks = 10),
                yaxis = list(
                    title = NA
                )
            )
    })

    output$tracked_plot <- plotly::renderPlotly({
        plotly_r()
    })

    observeEvent(plotly::event_data("plotly_relayout", source = "overview"), {
        d <- plotly::event_data("plotly_relayout", source = "overview")

        plotly::plotlyProxy(ns("tracked_plot"), session) %>%
            plotly::plotlyProxyInvoke("relayout", xaxis = list(range = c(d[['xaxis.range[0]']], d[['xaxis.range[1]']])), automargin = F)
    })

    observeEvent(overview()$selected_gene, {
        #browser()
        if(!is.data.frame(overview()$selected_gene)) {
            #browser()
            selection_box(NULL)
        } else {
            yrange <- isolate(plotly_r()$x$layout$yaxis$range)
            xrange <- isolate(c(overview()$selected_gene$start, overview()$selected_gene$end))
            sel <- geom_polygon(
                data = data.frame(x = c(xrange[1], xrange[1], xrange[2] ,xrange[2], xrange[1]), y = c(yrange[1], yrange[2], yrange[2], yrange[1], yrange[1])),
                aes(x, y),
                colour = NA,
                fill = "#FF0000",
                alpha = 0.1,
                inherit.aes = F
            )
            selection_box(list(sel, scale_y_continuous(expand = c(0,0))))
        }
    }, ignoreInit = T)

    return(reactive(list(plot = plot_r())))

}
