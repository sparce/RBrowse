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
        selected_gene = NA,
        selected_gene_coords = NA
    )

    observeEvent(input$chrom, {
        GenomeInfoDb::seqlevels(gene_annotation) <- input$chrom

        #pass through data on this view
        overview_data$chrom <- input$chrom
        overview_data$range_min <- 0
        overview_data$range_max <- Biostrings::nchar(genome[input$chrom])
        overview_data$selected_gene <- NA
        overview_data$selected_gene_coords <- NA
        }, ignoreNULL = TRUE)

    # Main plot
    output$overview_plot <- plotly::renderPlotly({
        validate(need(input$chrom, message = F))

        #Empty plot
        p <- ggplot(data.frame(x=c(0, Biostrings::nchar(genome[input$chrom]))), aes(x))

        if(inherits(gene_annotation, "TxDb")) {
            gene_data <- GenomicFeatures::transcripts(gene_annotation) %>%
                biovizBase::addStepping(group.selfish = F, extend.size = 100) %>%
                dplyr::as_data_frame() %>%
                dplyr::mutate_if(is.factor, as.character) %>%
                dplyr::group_by(tx_name) %>%
                dplyr::mutate(
                    arrowx = list(RBrowse::arrow_points(start, end, width, strand, base_y = stepping, coord = "x")),
                    arrowy = list(RBrowse::arrow_points(start, end, width, strand, base_y = stepping, coord = "y"))
                    ) %>%
                tidyr::unnest()

            sd <- SharedData$new(gene_data, key = ~tx_id)
            p <- p + geom_polygon(data = sd, aes(arrowx, arrowy, text = tx_name), colour = "black", size = .1, fill = "#777777")

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

        ggplotly(p, tooltip = 'text', source = "overview") %>%
            layout(showlegend = F, yaxis = list(fixedrange = T)) %>%
            highlight()
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
                    sd$data() %>% group_by(tx_name) %>% summarise(strand = unique(strand)) %>% .$strand %>% grep("+",.)
                    ) %>%
                plotly::plotlyProxyInvoke(
                    "restyle",
                    list(fillcolor="red"),
                    sd$data() %>% group_by(tx_name) %>% summarise(strand = unique(strand)) %>% .$strand %>% grep("-",.)
                    )
        } else {
            plotly::plotlyProxy(ns("overview_plot"), session) %>%
                plotly::plotlyProxyInvoke(
                    "restyle",
                    list(fillcolor = "#777")
                    )
        }
    }, ignoreNULL = T)


    observeEvent(event_data("plotly_click", source = "overview"), {
        d <- event_data("plotly_click", source = "overview")

        overview_data$selected_gene <- c(d, tx_name = sd$data() %>% filter(tx_id == d$key) %>% .$tx_name %>% unique)
        overview_data$selected_gene_coords <- biovizBase::crunch(gene_annotation, which = list(tx_id=d$key))
    })

    od <- reactive(reactiveValuesToList(overview_data))
    return(od)
}
