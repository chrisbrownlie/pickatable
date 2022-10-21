#' Module UI
#' 
#' @return module UI
gt_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("gt", class = "tab-title")
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "table-div",
          gt::gt_output(ns("table"))
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
gt_server <- function(id,
                      app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 ad = app_data) {
                 ns <- session$ns
                 
                 ad[["gt"]] <- Table$new(fun = "gt",
                                         pkg = "gt",
                                         data = ad[["dataset"]])$reactive()
                 
                 output$table <- gt::render_gt({
                   ad[["gt"]]()$plot()
                 })
                 
               })
}