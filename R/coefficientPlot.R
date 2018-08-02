#' UI for an RBrowse coefficient plot
#'
#' The \code{coefficientPlot} is a test run at configuring plots using the \code{\link{entityPlot}} module
#'
#' @param id Shiny id of the plot. This must be unique within your app.
#'
#' @return A set of shiny components comprising an RBrowse coefficient plot
#' @export
coefficientPlotUI <- function(id) {
    ns = shiny::NS(id)
    RBrowse::entityPlotUI(ns("coefficient"))
}

#' Server code for an RBrowse coefficient plot
#'
#' The \code{coefficientPlot} is a test run at configuring plots using the \code{\link{entityPlot}} module.
#' It will be used as an example for creating a number of modules designed to produce common
#' plots of data associated with a single genomic entity
#' It shows how to produce the necessary inputs for \code{entityPlot} and how to call it
#'
#' @param input The shiny input object. Passed automatically from \code{callModule}
#' @param output The shiny output object. Passed automatically from \code{callModule}
#' @param session The shiny session object. Passed automatically from \code{callModule}
#'
#' @param data_file File path to a data file.
#' @param overview Returned data from calling the \code{\link{overviewPlot}} module. Needed to determine the
#' gene to plot
#'
#' @return passes through the output from \code{entityPlot}, ie. a reactive list containing
#' the plot
#'
#' @export
coefficientPlot <- function(input, output, session, data_file, overview) {
    ns = session$ns

    fitted_model <- readr::read_rds(data_file)

    tidied_coefficients <- broom::tidy(fitted_model) %>% dplyr::filter(!stringr::str_detect(term, "sva"))

    tidied_stdev <- as.data.frame(fitted_model$stdev.unscaled) %>%
        dplyr::mutate(gene = rownames(.)) %>%
        tidyr::gather(term, stdev, -gene)

    data = dplyr::left_join(tidied_coefficients, tidied_stdev)

    plot_data <- function(d) {
        shiny::req(overview()$selected_gene)
        d %>%
            dplyr::filter(gene == overview()$selected_gene$tx_name) %>%
            dplyr::mutate(DE = dplyr::case_when(
                p.value > 0.05 ~ NA_character_,
                estimate > input$lfc ~ "up",
                abs(estimate) > input$lfc ~ "down",
                TRUE ~ NA_character_
            ))

    }

    option_setters <- shiny::reactive({
        shiny::tagList(
            shiny::numericInput(ns("lfc"), "logFC cutoff:", 0, min = 0)
        )
    })

    plot_f <- function(p) {
        pal <- RColorBrewer::brewer.pal(3, "Set1")

        p %>%
            ggplot2::ggplot(ggplot2::aes(
                x = term,
                y = estimate,
                ymin = estimate - stdev,
                ymax = estimate + stdev,
                colour = DE,
                text = paste0(stringr::str_remove(term,"diet"), round(estimate, 2), " +/- ", round(stdev, 2))
                )) +
            ggplot2::geom_errorbar(width = 0.2) +
            ggplot2::geom_pointrange() +
            ggplot2::scale_color_manual(limits = c("up", "down"),values = c("up" = pal[1], "down" = pal[2]), na.value = "grey40", guide = ggplot2::guide_legend(title = "Regulation"))
    }

    entity_plot_output = shiny::callModule(entityPlot, "coefficient", data = data, data_fn = plot_data, plot_fn = plot_f, options = option_setters, overview = overview)

    return(entity_plot_output)
}
