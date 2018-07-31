#' UI for an RBrowse test plot
#'
#' The \code{testPlot} is a test run at configuring plots using the \code{\link{trackedPlot}} module
#'
#' @param id Shiny id of the plot. This must be unique within your app.
#'
#' @return A set of shiny components comprising an RBrowse test plot
#' @export
testPlotUI <- function(id) {
    ns = shiny::NS(id)
     RBrowse::trackedPlotUI(ns("tracked"))
}

#' Server code for an RBrowse test plot
#'
#' The \code{testPlot} is a test run at configuring plots using the \code{\link{trackedPlot}} module.
#' It will be used as an example for creating a number of modules designed to produce common
#' types of genomic tracked plots (eg. manhattanPlot, bamPlot, snpPlot, geneStructurePlot, ...)
#' It shows how to produce the necessary inputs for \code{trackedPlot} and how to call it
#'
#' @param input The shiny input object. Passed automatically from \code{callModule}
#' @param output The shiny output object. Passed automatically from \code{callModule}
#' @param session The shiny session object. Passed automatically from \code{callModule}
#'
#' @param data_file File path to a data file.
#' @param overview Returned data from calling the \code{\link{overviewPlot}} module. Needed to coordinate the view ranges
#' @param bin_width Width of the bins in the data file. Included as an example of how you can use
#' additional arguments
#'
#' @return passes through the output from \code{trackedPlot}, ie. a reactive list containing
#' the plot
#' @export
testPlot <- function(input, output, session, data_file, overview, bin_width = NULL) {
    ns = session$ns

    data <- readr::read_tsv(data_file)

    plot_data <- function(d) {
        d %>%
            dplyr::mutate(width = bin_width) %>%
            dplyr::rename(seqnames="CHROM", start = "BIN_START") %>%
            plyranges::as_granges()

    }

    option_setters <- shiny::reactive({
        shiny::tagList(
            shiny::selectizeInput(ns("data"), "Data to plot:", choices = colnames(GenomicRanges::mcols(plot_data(data)))),
            shiny::numericInput(ns("cutoff"), "Value cutoff:", 1, min = 0, step = 1),
            shiny::numericInput(ns("highlight"), "Value highlight:", 5, min = 0, step = 1)
        )
    })

    plot_f <- function(p) {
        shiny::req(input$data, overview()$range_min, overview()$range_max)

        sym_y <- rlang::sym(input$data)

        p %>%
            plyranges::filter_by_overlaps(GenomicRanges::GRanges(seqnames = overview()$chrom, ranges = IRanges::IRanges(start = overview()$range_min, end = overview()$range_max))) %>%
            plyranges::filter(!!sym_y > input$cutoff) %>%as.data.frame() %>%
            ggplot2::ggplot(ggplot2::aes(x = start+(width/2), y = !!sym_y, yend = !!sym_y, text = !!sym_y, key = !!sym_y)) +
            ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, colour = !!sym_y > input$highlight), size = 0.5) +
            ggplot2::geom_point(ggplot2::aes(colour = !!sym_y > input$highlight))
    }

    tracked_plot_output = shiny::callModule(trackedPlot, "tracked", data = data, data_fn = plot_data, plot_fn = plot_f, options = option_setters, overview = overview)

    return(tracked_plot_output)
}
