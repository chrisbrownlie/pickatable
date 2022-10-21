#' Module UI
#' 
#' @return module UI
kable_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("kableExtra", class = "tab-title")
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "table-div",
          tableOutput(ns("table"))
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
kable_server <- function(id,
                         app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 ad = app_data) {
                 ns <- session$ns
                 
                 ad[["kable"]] <- Table$new(fun = "kbl",
                                            pkg = "kableExtra",
                                            x = ad[["dataset"]])$reactive()

                 output$table <- function() {ad[["kable"]]()$plot()}
                 
               })
}