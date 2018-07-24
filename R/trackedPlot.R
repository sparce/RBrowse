#' Title
#'
#' @param id
#' @param options
#' @param ...
#'
#' @return
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


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param data
#' @param data_fn
#' @param plot_fn
#' @param overview
#'
#' @return
#' @export
#'
#' @examples
trackedPlot <- function(input, output, session, data, data_fn, plot_fn, options = NULL, overview) {
    ns <- session$ns

    output$options_dropdown <- renderUI({
        validate(need(options(), message = F))

        shinyWidgets::dropdown(
            options(),
            style = "unite",
            icon = icon("gear"),
            status = "royal",
            width = "30%",
            animate = shinyWidgets::animateOptions(enter = shinyWidgets::animations$fading_entrances$fadeInLeft, exit = shinyWidgets::animations$fading_exits$fadeOutLeft, duration = 0.3)
        )
    })

    data_r <- reactive({ data_fn(data) })

    plot_r <- reactive({ plot_fn(data_r()) })

    output$tracked_plot <- plotly::renderPlotly({
        plotly::ggplotly(
            plot_r() +
                coord_cartesian(xlim = c(overview()$range_min, overview()$range_max))+
                scale_x_continuous(expand = c(0,0))
            ) %>%
            plotly::layout(
                showlegend = F,
                xaxis = list(
                    fixedrange = T,
                    autorange = F,
                    tickmode = 'auto',
                    nticks = 10)
                )
    })

    observeEvent(plotly::event_data("plotly_relayout", source = "overview"), {
        d <- plotly::event_data("plotly_relayout", source = "overview")

        #browser()

        plotly::plotlyProxy(ns("tracked_plot"), session) %>%
            plotly::plotlyProxyInvoke("relayout", xaxis = list(range = c(d[['xaxis.range[0]']], d[['xaxis.range[1]']])), automargin = F)
    })


}
