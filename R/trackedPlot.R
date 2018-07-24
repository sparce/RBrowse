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

    selection_box <- reactiveVal(NULL)

    plot_r <- reactive({ plot_fn(data_r()) + selection_box() })

    plotly_r <- reactive({
        plotly::ggplotly(
            plot_r() +
                coord_cartesian(xlim = c(overview()$range_min, overview()$range_max))+
                scale_x_continuous(expand = c(0,0))
            , tooltip = "text") %>%
            plotly::layout(
                showlegend = F,
                xaxis = list(
                    fixedrange = T,
                    autorange = F,
                    tickmode = 'auto',
                    nticks = 10)
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


}
