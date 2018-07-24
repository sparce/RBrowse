#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
overviewPlotUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::fluidRow(class = "overview",
        shiny::uiOutput(ns("options_dropdown")),
        plotly::plotlyOutput(ns("overview_plot"), height = "200px"),
        #verbatimTextOutput(ns("debug")),
        NULL
    )
}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param genome_fasta
#' @param gene_annotation
#'
#' @return
#' @export
#'
#' @examples
overviewPlot <- function(input, output, session, genome, gene_annotation = NULL) {
    # If given path to a file, try to load them in a sensible fashion.
    # Otherwise, we will assume that we are passed objects of the correct type
    if(is.character(genome)) genome <- Biostrings::readDNAStringSet(genome)

    if(is.character(gene_annotation)) {
        extension <- tools::file_ext(gene_annotation)

        #GFF/GTF file
        if(stringr::str_detect(extension, "gff|gtf")) gene_annotation <- GenomicFeatures::makeTxDbFromGFF(gene_annotation)
    }

    # Customisation options for overview plot
    output$options_dropdown <- shiny::renderUI({
            ns <- session$ns

            validate(need(genome, message = F))

            annotation_opts <- NULL
            if(!is.null(gene_annotation)) {
                annotation_opts <- shiny::checkboxInput(ns("by_strand"), "Colour by strand:")
            }

            shinyWidgets::dropdown(
                shiny::tagList(
                    shiny::selectizeInput(ns("chrom"), "Sequence:", choices = names(genome), selected = NULL),
                    annotation_opts
                )
            )
    })

    overview_data <- reactiveValues(
        chrom = "",
        range_min = 0,
        range_max = 0,
        selected_gene = NA
    )

    observeEvent(input$chrom, {
        #pass through data on this view
        overview_data$chrom <- input$chrom
        overview_data$range_min <- 0
        overview_data$range_max <- Biostrings::nchar(genome[input$chrom])
        overview_data$selected_gene <- NA
        }, ignoreNULL = TRUE)

    # Underlying data
    gene_data <- reactive({
        req(input$chrom)

        #browser()
        if(inherits(gene_annotation, "TxDb")) {
            GenomeInfoDb::seqlevels(gene_annotation) <- input$chrom

            GenomicFeatures::transcripts(gene_annotation) %>%
                biovizBase::addStepping(group.selfish = F, extend.size = 100) %>%
                dplyr::as_data_frame() %>%
                dplyr::mutate_if(is.factor, as.character) %>%
                dplyr::group_by(tx_name) %>%
                dplyr::mutate(
                    arrowx = list(RBrowse::arrow_points(start, end, width, strand, base_y = stepping, coord = "x")),
                    arrowy = list(RBrowse::arrow_points(start, end, width, strand, base_y = stepping, coord = "y"))
                ) %>%
                tidyr::unnest()
        } else {
            NULL
        }
    })

    #sd = crosstalk::SharedData$new(gene_data, key = ~tx_id)

    # Main plot
    output$overview_plot <- plotly::renderPlotly({
        validate(need(input$chrom, message = F))
        #browser()
        #Empty plot
        p <- ggplot(data.frame(x=c(0, Biostrings::nchar(genome[input$chrom]))), aes(x))

        if(inherits(gene_annotation, "TxDb")) {
            p <- p + geom_polygon(data = gene_data(), aes(arrowx, arrowy, text = tx_name, key = tx_id), colour = "black", size = .1, fill = "#777777")

            #plus_strand <- SharedData$new(gene_data %>% dplyr::filter(strand == "+"), key = ~tx_name)
            #minus_strand <- SharedData$new(gene_data %>% dplyr::filter(strand == "-"), key = ~tx_name)
        }
        # # Add +/- strand separately for later styling
        # p <- p %>%
        #     plotly::add_polygons(
        #         data = plus_strand,
        #         x = ~arrowx,
        #         y = ~arrowy,
        #         text = ~tx_name,
        #         line = list(width = 1, color = '#333'),
        #         fillcolor = "#777",
        #         hoveron = "fills",
        #         hoverinfo = "text"
        #             ) %>%
        #     plotly::add_polygons(
        #         data = minus_strand,
        #         x = ~arrowx,
        #         y = ~arrowy,
        #         text = ~tx_name,
        #         line = list(width = 1, color = '#333'),
        #         fillcolor = "#777",
        #         hoveron = "fills",
        #         hoverinfo = "text"
        #     )
        #
        # # Show plot
        # p

        plotly::ggplotly(p + scale_x_continuous(expand=c(0.02,0)), tooltip = 'text', source = "overview") %>%
            plotly::layout(
                showlegend = F,
                yaxis = list(fixedrange = T),
                xaxis = list(autorange = F, tickmode = 'auto', nticks = 10)
                ) %>%
            plotly::highlight()
    })

    output$debug <- renderText(input$chrom)

    # Colour genes by strand if selected
    observeEvent(c(input$by_strand), {
        ns <- session$ns

        if(input$by_strand) {
            plotly::plotlyProxy(ns("overview_plot"), session) %>%
                plotly::plotlyProxyInvoke(
                    "restyle",
                    list(fillcolor="blue"),
                    gene_data() %>% dplyr::group_by(tx_name) %>% dplyr::summarise(strand = unique(strand)) %>% .$strand %>% grep("+",.)
                    ) %>%
                plotly::plotlyProxyInvoke(
                    "restyle",
                    list(fillcolor="red"),
                    gene_data() %>% dplyr::group_by(tx_name) %>% dplyr::summarise(strand = unique(strand)) %>% .$strand %>% grep("-",.)
                    )
        } else {
            plotly::plotlyProxy(ns("overview_plot"), session) %>%
                plotly::plotlyProxyInvoke(
                    "restyle",
                    list(fillcolor = "#777")
                    )
        }
    }, ignoreNULL = T)


    observeEvent(plotly::event_data("plotly_click", source = "overview"), {
        d <- plotly::event_data("plotly_click", source = "overview")
        #browser()
        overview_data$selected_gene <- d %>%
            dplyr::mutate_at("key",as.integer) %>%
            dplyr::left_join(gene_data() %>% dplyr::select(-stepping, -arrowx, -arrowy), by = c(key = "tx_id")) %>%
            unique
        #overview_data$selected_gene_coords <- biovizBase::crunch(gene_annotation, which = list(tx_id=d$key))

        plotly::plotlyProxy(session$ns("overview_plot")) %>%
            plotly::plotlyProxyInvoke("restyle", list(opacity = 0.2)) %>%
            plotly::plotlyProxyInvoke("restyle", list(opacity = 1), list(d$curveNumber))
    })

    observeEvent(plotly::event_data("plotly_relayout", source = "overview"), {
        d <- plotly::event_data("plotly_relayout", source = "overview")

        #browser()
        overview_data$selected_gene <- NA
        overview_data$range_min <- d[['xaxis.range[0]']]
        overview_data$range_max <- d[['xaxis.range[1]']]

        plotly::plotlyProxy(session$ns("overview_plot")) %>%
            plotly::plotlyProxyInvoke("restyle", list(opacity = 1))
    })


    od <- reactive(reactiveValuesToList(overview_data))
    return(od)
}
