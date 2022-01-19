if (interactive()) {
    
    library("shiny")
    library("shinyWidgets")
    
    ui <- fluidPage(
        column(
            width = 7,
            tags$b("Default"), br(),
            progressBar(id = "pb1", value = 50),
            sliderInput(
                inputId = "up1",
                label = "Update",
                min = 0,
                max = 100,
                value = 50
            ),
            br(),
            tags$b("Other options"), br(),
            progressBar(
                id = "pb2",
                value = 0,
                total = 100,
                title = "",
                display_pct = TRUE
            ),
            actionButton(
                inputId = "go",
                label = "Launch calculation"
            )
        )
    )
    
    server <- function(input, output, session) {
        observeEvent(input$up1, {
            updateProgressBar(
                session = session,
                id = "pb1",
                value = input$up1
            )
        })
        observeEvent(input$go, {
            for (i in 1:100) {
                updateProgressBar(
                    session = session,
                    id = "pb2",
                    value = i, total = 100,
                    title = paste("Process", trunc(i/10))
                )
                Sys.sleep(0.1)
            }
        })
    }
    
    shinyApp(ui = ui, server = server)
    
}
