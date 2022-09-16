#' Module UI
#' 
#' @return module UI
flextable_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("flextable", class = "tab-title")
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "table-div",
          uiOutput(ns("table"))
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
          shinyjs::disabled(nav(title = "Interaction"))
        )
      )
    )
  )
}

#' Module server
#' 
#' @return module server
flextable_server <- function(id,
                             app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 data = app_data) {
                 ns <- session$ns
                 
                 data$flextable_table <- reactive({
                   get(data[["dataset"]]) |>
                     head() |>
                     flextable::flextable() |>
                     flextable::htmltools_value()
                 })
                 output$table <- renderUI({
                   data[["flextable_table"]]()
                 })
                 
               })
}