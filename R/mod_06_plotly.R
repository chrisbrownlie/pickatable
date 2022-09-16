#' Module UI
#' 
#' @return module UI
plotly_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("plotly", class = "tab-title")
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "table-div",
          plotly::plotlyOutput(ns("table"))
        )
      ),
      column(
        width = 6,
        div(
          class = "code-div",
          p("(Code that can be copied to recreate table on left)")
        )
      )
    ),
    
    fluidRow(
      div(
        class = "options-div",
        bslib::navs_pill_card(
          nav(
            title = "Elements",
            p("e.g. dom, show/hide columns, rownames etc.")
          ),
          nav(
            title = "Layout",
            p("e.g. rows to show, size etc.")
          ),
          nav(
            title = "Interaction",
            p("e.g. row selection, double clicking etc.")
          )
        )
      )
    )
  )
}

#' Module server
#' 
#' @return module server
plotly_server <- function(id,
                      app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 data = app_data) {
                 ns <- session$ns
                 
                 data$plotly_table <- reactive({
                   get(data[["dataset"]]) |>
                     plotly::plot_ly(type = "table",
                                     header = list(values = names(get(data[["dataset"]]))), 
                                     cells = list(values = unname(get(data[["dataset"]]))))
                 })
                 output$table <- plotly::renderPlotly({
                   data[["plotly_table"]]()
                 })
                 
               })
}