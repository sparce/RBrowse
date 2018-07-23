library(shiny)
library(RBrowse)

ui <- fluidPage(
    RBrowse::overviewPlotUI("simple-demo"),
    jsonview::jsonviewOutput("info"),
    #checkboxInput("test","Test"),
    NULL
)

server <- function(input, output, session) {
    overview <- callModule(
        overviewPlot,
        "simple-demo",
        genome = "../shared-data/demo_genome.fasta",
        gene_annotation = "../shared-data/demo.gff3"
        )

    observeEvent(input$test, browser(), ignoreNULL = T, ignoreInit = T)

    output$info <- jsonview::renderJsonview({overview() %>% purrr::modify_at("selected_gene_coords", as.data.frame) %>% jsonview::json_view()})
}

shinyApp(ui, server)
