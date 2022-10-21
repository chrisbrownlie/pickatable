#' Module UI
#' 
#' @return module UI
comparison_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("Comparison", class = "tab-title"),
        p("Use this tab to compare two tables side by side. NOTE: this will keep 
          any options you have chosen on the respective tabs.")
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "table-div",
          selectInput(ns("first_selection"),
                      "Table 1:",
                      choices = c("DT", "flextable", "gt", "kableExtra", "plotly")),
          uiOutput(ns("first_table"))
        )
      ),
      column(
        width = 6,
        div(
          class = "table-div",
          selectInput(ns("second_selection"),
                      "Table 2:",
                      choices = c("DT", "flextable", "gt", "kableExtra", "plotly")),
          uiOutput(ns("second_table"))
        )
      )
    )
  )
}

#' Module server
#' 
#' @return module server
comparison_server <- function(id,
                          app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 data = app_data) {

                 ns <- session$ns
                 
                 output$first_table <- renderUI({
                   req(input$first_selection)
                   if (input$first_selection == "DT") {
                     DT::DTOutput(ns("dt_table"))
                   } else if (input$first_selection == "flextable") {
                     uiOutput(ns("flextable_table"))
                   } else if (input$first_selection == "gt") {
                     gt::gt_output(ns("gt_table"))
                   } else if (input$first_selection == "kableExtra") {
                     tableOutput(ns("kable_table"))
                   } else if (input$first_selection == "plotly") {
                     plotly::plotlyOutput(ns("plotly_table"))
                   }
                 })
                 
                 output$second_table <- renderUI({
                   req(input$second_selection)
                   if (input$second_selection == "DT") {
                     DT::DTOutput(ns("dt_table"))
                   } else if (input$second_selection == "flextable") {
                     uiOutput(ns("flextable_table"))
                   } else if (input$second_selection == "gt") {
                     gt::gt_output(ns("gt_table"))
                   } else if (input$second_selection == "kableExtra") {
                     tableOutput(ns("kable_table"))
                   } else if (input$second_selection == "plotly") {
                     plotly::plotlyOutput(ns("plotly_table"))
                   }
                 })
                 
                 output$dt_table <- DT::renderDT({
                   ad[["DT"]]()$plot()
                 })
                 output$flextable_table <- renderUI({
                   ad[["flextable"]]()$plot()
                 })
                 output$gt_table <- gt::render_gt({
                   ad[["gt"]]()$plot()
                 })
                 output$kable_table <- function() {data[["kable_table"]]()}
                 output$plotly_table <- plotly::renderPlotly({
                   ad[["plotly"]]()$plot()
                 })
               })
}