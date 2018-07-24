library(shiny)
library(RBrowse)



ui <- fluidPage(
    RBrowse::overviewPlotUI("simple-demo"),
    RBrowse::testPlotUI("track1"),
    jsonview::jsonviewOutput("info"),
    #checkboxInput("test","Test"),
    NULL
)

server <- function(input, output, session) {
    txdb <- AnnotationDbi::loadDb("../shared-data/demo.gff3.sqlite")

    overview <- callModule(
        overviewPlot,
        "simple-demo",
        genome = "../shared-data/demo_genome.fasta",
        gene_annotation = txdb
        )

    callModule(testPlot, "track1", data_file = "~/Documents/Helicoverpa/HarmPopGen/data/Harm_10kb.Tajima.D", overview = overview, bin_width = 10000)

    observeEvent(input$test, browser(), ignoreNULL = T, ignoreInit = T)

    output$info <- jsonview::renderJsonview({overview() %>% purrr::modify_at("selected_gene_coords", as.data.frame) %>% jsonview::json_view()})
}

shinyApp(ui, server)
