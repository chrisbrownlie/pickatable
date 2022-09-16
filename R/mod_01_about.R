#' Module UI
#' 
#' @return module UI
about_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("pickatable", class = "tab-title")
      )
    ),
    fluidRow(
      column(
        width = 12,
        p("This app is designed to showcase the most popular table packages in R and help you choose which one
          you want to use in your project.")
      )
    ),
    fluidRow(
      column(
        width = 12,
        p("Use the options below to alter all tables, then see the different tabs and use inputs on those tabs
          to see some of the capabilities of each package"),
        selectInput(ns("opt_dataset"),
                    "Data to view:",
                    choices = c("iris", "mtcars", "airquality"))
      )
    )
  )
}

#' Module server
#' 
#' @return module server
about_server <- function(id,
                         app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 data = app_data) {
                 ns <- session$ns
                 
                 observe({data[["dataset"]] <- input$opt_dataset})
                 
               })
}