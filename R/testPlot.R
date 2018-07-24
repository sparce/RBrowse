#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
testPlotUI <- function(id) {
    ns = NS(id)
    trackedPlotUI(ns("tracked"))
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param data_file
#' @param overview
#' @param bin_width
#'
#' @return
#' @export
#'
#' @examples
testPlot <- function(input, output, session, data_file, overview, bin_width = NULL) {
    ns = session$ns

    data <- readr::read_tsv(data_file)

    plot_data <- function(d) {
        d %>%
            dplyr::mutate(width = bin_width) %>%
            dplyr::rename(seqnames="CHROM", start = "BIN_START") %>%
            plyranges::as_granges()

    }

    option_setters <- reactive({
        tagList(
            selectizeInput(ns("data"), "Data to plot:", choices = colnames(GenomicRanges::mcols(plot_data(data)))),
            numericInput(ns("cutoff"), "Value cutoff:", 1, min = 0, step = 1),
            numericInput(ns("highlight"), "Value highlight:", 5, min = 0, step = 1)
        )
    })

    plot_f <- function(p) {
        validate(need(input$data, message = F))

        sym_y <- rlang::sym(input$data)

        p %>%
            plyranges::filter_by_overlaps(GenomicRanges::GRanges(seqnames = overview()$chrom, ranges = IRanges::IRanges(start = overview()$range_min, end = overview()$range_max))) %>%
            plyranges::filter(!!sym_y > input$cutoff) %>%as.data.frame() %>%
            ggplot(aes(x = start+(width/2), y = !!sym_y, yend = !!sym_y)) +
            geom_segment(aes(x = start, xend = end, colour = !!sym_y > input$highlight), size = 0.5) +
            geom_point(aes(colour = !!sym_y > input$highlight))
    }

    plot_result = callModule(trackedPlot, "tracked", data = data, data_fn = plot_data, plot_fn = plot_f, options = option_setters, overview = overview)

}
