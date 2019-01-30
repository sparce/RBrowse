#' UI for an RBrowse overview plot
#'
#' The \code{overviewPlot} provides the main way of controlling and interacting with
#' an RBrowse browser. It allows selection of a chromosome/scaffold as well as a
#' genomic range to view from a fasta file. If gene annotations are provided it
#' also allows selection of an individual transcript to highlight in downstream plots.
#'
#' You should include one (and only one) \code{overviewPlot} for each set of linked,
#' interactive downstream plots you wish to produce
#'
#' @param id Shiny id of the plot. This must be unique within your app.
#'
#' @return A set of shiny components comprising an RBrowse overview plot
#' @export
overviewPlotUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::fluidRow(class = "overview",
                    shiny::uiOutput(ns("options_dropdown")),
                    plotly::plotlyOutput(ns("overview_plot"), height = "200px"),
                    #verbatimTextOutput(ns("debug")),
                    NULL
    )
}


#' Server code for an RBrowse overview plot
#'
#' This produces the server code necessary for an RBrowse \code{overviewPlot} to function.
#' It is not called directly, but rather using the \code{\link[shiny]{callModule}} function
#' from shiny.
#'
#' @param input The shiny input object. Passed automatically from \code{callModule}
#' @param output The shiny output object. Passed automatically from \code{callModule}
#' @param session The shiny session object. Passed automatically from \code{callModule}
#'
#' @param genome File path to a fasta formatted file containing the genome assembly
#' @param gene_annotation Optional set of gene annotations as either a file path to a
#' gff formatted file, a \code{\link[GenomicFeatures]{TxDb}} object, or a filepath to a
#' \code{TxDb} object saved with \code{\link[AnnotationDbi]{saveDb}}.
#'
#' @return The server function required to produce and interact with an RBrowse
#' overview plot. In addition, the function returns a reactive object containing
#' information about the plot (chromosome/range/gene selection) which can be passed
#' to downstream visualisations (see examples for how to use this).
#'
#' @export
overviewPlot <- function(input, output, session, genome, gene_annotation = NULL) {
    # If given path to a file, try to load them in a sensible fashion.
    # Otherwise, we will assume that we are passed objects of the correct type
    if(is.character(genome)) genome <- Biostrings::readDNAStringSet(genome)

    if(is.character(gene_annotation)) {
        extension <- tools::file_ext(gene_annotation)

        #GFF/GTF file
        if(stringr::str_detect(extension, "gff|gtf")) gene_annotation <- GenomicFeatures::makeTxDbFromGFF(gene_annotation)

        #TxDb object saved with AnnotationDbi::saveDb
        if(stringr::str_detect(extension, "sqlite") & requireNamespace("GenomicFeatures", quietly = TRUE)) gene_annotation <- AnnotationDbi::loadDb(gene_annotation)
    }

    # Customisation options for overview plot
    output$options_dropdown <- shiny::renderUI({
        ns <- session$ns

        shiny::req(genome)

        annotation_opts <- NULL
        if(!is.null(gene_annotation)) {
            annotation_opts <- shiny::checkboxInput(ns("by_strand"), "Colour by strand:")
        }

        div(style = "position: absolute; z-index: 1;",
            shinyWidgets::dropdown(
                shiny::tagList(
                    shiny::selectizeInput(ns("chrom"), "Sequence:", choices = names(genome), selected = NULL),
                    annotation_opts
                )
            )
        )
    })

    overview_data <- shiny::reactiveValues(
        chrom = "",
        range_min = 0,
        range_max = 0,
        selected_gene = NA
    )

    shiny::observeEvent(input$chrom, {
        #pass through data on this view
        overview_data$chrom <- input$chrom
        overview_data$range_min <- 0
        overview_data$range_max <- Biostrings::nchar(genome[input$chrom])
        overview_data$selected_gene <- NA
    }, ignoreNULL = TRUE)

    # Underlying data
    gene_data <- shiny::reactive({
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
                    arrowx = list(arrow_points(start, end, width, strand, base_y = stepping, coord = "x")),
                    arrowy = list(arrow_points(start, end, width, strand, base_y = stepping, coord = "y"))
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

        #Empty plot
        p <- ggplot2::ggplot(data.frame(x=c(0, Biostrings::nchar(genome[input$chrom]))), ggplot2::aes(x))

        if(inherits(gene_annotation, "TxDb")) {
            p <- p + ggplot2::geom_polygon(data = gene_data(), ggplot2::aes(arrowx, arrowy, text = tx_name, key = tx_id), colour = "black", size = .1, fill = "#777777")

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

        plotly::ggplotly(p + ggplot2::scale_x_continuous(expand=c(0.02,0)), tooltip = 'text', source = "overview") %>%
            plotly::layout(
                showlegend = F,
                title = input$chrom,
                titlefont = list(size = 14),
                yaxis = list(fixedrange = T, title = NA, showticklabels = F, ticklen=0),
                xaxis = list(title = NA, autorange = F, tickmode = 'auto', nticks = 10)
            ) %>%
            plotly::highlight()
    })

    output$debug <- shiny::renderText(input$chrom)

    # Colour genes by strand if selected
    shiny::observeEvent(c(input$by_strand), {
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


    shiny::observeEvent(plotly::event_data("plotly_click", source = "overview"), {
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

    shiny::observeEvent(plotly::event_data("plotly_relayout", source = "overview"), {
        d <- plotly::event_data("plotly_relayout", source = "overview")

        #browser()
        overview_data$selected_gene <- NA
        overview_data$range_min <- d[['xaxis.range[0]']]
        overview_data$range_max <- d[['xaxis.range[1]']]

        plotly::plotlyProxy(session$ns("overview_plot")) %>%
            plotly::plotlyProxyInvoke("restyle", list(opacity = 1))
    })


    od <- shiny::reactive(reactiveValuesToList(overview_data))
    return(od)
}
