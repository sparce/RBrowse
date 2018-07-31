library(shiny)
library(RBrowse)

#Needs GenomicFeatures loaded before calling AnnotationDbi::loadDb
library(GenomicFeatures)

ui <- fluidPage(
    h3("overviewPlot"),
    RBrowse::overviewPlotUI("simple-demo"),

    h3("testPlot"),
    RBrowse::testPlotUI("track1"),

    h3("testPlot 2"),
    p("Exactly the same as above, just given a different ID. Since these are modules, shiny
      handles namespacing of the inputs/outputs so that these two plots can be interacted with
      separately."),
    RBrowse::testPlotUI("track2"),

    hr(),

    h3("testPlot returned value"),
    HTML(glue::glue("Showing that data can be passed back out of these modules
    (see code at <code>{system.file('shiny-examples', 'simple-demo', 'simple-demo.R', package='RBrowse')}</code> for how to access).
    Will be useful for future analyses such as GO enrichments of genes around selected SNPs, etc.")),
    plotOutput("track1_plot")
)

server <- function(input, output, session) {
    txdb <- AnnotationDbi::loadDb("../shared-data/demo.gff3.sqlite")

    overview <- callModule(
        overviewPlot,
        "simple-demo",
        genome = "../shared-data/demo_genome.fasta",
        gene_annotation = txdb
        )

    track1_return <- callModule(testPlot, "track1", data_file = "../shared-data/demo_testdata.tsv", overview = overview, bin_width = 10000)

    #Don't have to save the return value if you don't intend to do anything with it
    callModule(testPlot, "track2", data_file = "../shared-data/demo_testdata.tsv", overview = overview, bin_width = 10000)

    output$track1_plot <- renderPlot(track1_return()$plot)
}

shinyApp(ui, server)
