#' Module UI
#' 
#' @return module UI
dt_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h3("DT", class = "tab-title"),
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "table-div",
          DT::DTOutput(ns("table"))
        )
      ),
      column(
        width = 6,
        div(
          class = "code-div",
          p("(Code that can be copied to recreate table on left)"),
          code_snippet_ui(ns("DT"))
        )
      )
    ),
    
    fluidRow(
      div(
        class = "options-div",
        bslib::navs_pill_card(
          nav(
            title = "Elements",
            p("e.g. dom, show/hide columns, rownames etc."),
            checkboxInput(ns("rownames"),
                          label = "Show row names?",
                          value = FALSE),
            uiOutput(ns("colnames_ui")),
            textInput(ns("caption"),
                      label = "Table caption:",
                      value = ""),
            radioButtons(ns("filter"),
                         label = "Column filters",
                         choices = c("None" = "none",
                                     "Top" = "top",
                                     "Bottom" = "bottom"),
                         selected = "none")
          ),
          nav(
            title = "Layout",
            p("e.g. rows/cols to show")
          ),
          nav(
            title = "Interaction",
            p("e.g. row selection, double clicking etc."),
            selectInput(ns("selection"),
                        label = "Single click:",
                        choices = c("Nothing" = "none",
                                    "Select single rows" = "single",
                                    "Select multiple rows" = "multiple"),
                        selected = "none"),
            selectInput(ns("editable_choice"),
                        label = "Double click:",
                        choices = c("Nothing" = "none",
                                    "Edit column" = "column",
                                    "Edit row" = "row",
                                    "Edit cell" = "cell",
                                    "Edit all" = "all",
                                    "Custom event" = "custom"),
                        selected = "none"
                        )
          )
        )
      )
    )
  )
}

#' Module server
#' 
#' @return module server
dt_server <- function(id,
                      app_data) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 ad = app_data) {
                 ns <- session$ns
                 
                 # Create DT table object
                 observeEvent(ad[["dataset"]], {
                   if (ad[["dataset"]] == "iris") {
                     ad[["DT"]] <- Table$new(fun = "datatable",
                                             pkg = "DT",
                                             data = iris)$reactive()
                   } else if (ad[["dataset"]] == "mtcars") {
                     ad[["DT"]] <- Table$new(fun = "datatable",
                                             pkg = "DT",
                                             data = mtcars)$reactive()
                   } else {
                     ad[["DT"]] <- Table$new(fun = "datatable",
                                             pkg = "DT",
                                             data = airquality)$reactive()
                   }
                 })
                 
                 code_snippet_server("DT",
                                     table_def = ad[["DT"]])
                 
                 # Render table
                 output$table <- DT::renderDT({
                   req(ad[["DT"]])
                   ad[["DT"]]()$plot()
                 })
                 
                 # Show rownames
                 observeEvent(input$rownames, ad[["DT"]]()$set(rownames = !!input$rownames))
                 
                 # Hide/show columns ('options' argument)
                 column_names <- reactive(names(get(ad[["dataset"]])))
                 output$colnames_ui <- renderUI({
                   selectInput(ns("colnames"),
                               label = "Columns to show:",
                               choices = column_names(),
                               selected = column_names(),
                               multiple = TRUE)
                 })
                 
                 # Single observer to set options
                 observe({
                   # Iteratively add to options list
                   options_list <- list()
                   
                   # Hide/view columns
                   view_cols <- match(input$colnames, column_names())
                   hide_cols <- setdiff(seq_along(column_names()), view_cols)-as.numeric(input$rownames==F)
                   if (length(hide_cols)) {
                     options_list["columnDefs"] <- list(list(list(visible=FALSE, targets=hide_cols)))
                   }
                   
                   isolate(ad[["DT"]]())$set(options = NULL)
                   if (length(options_list)) {
                     isolate(ad[["DT"]]())$set(options = !!options_list)
                   }
                 })
                 
                 # Interaction - double click
                 table_editable <- reactiveVal(FALSE)
                 observeEvent(input$editable_choice, {
                   if (input$editable_choice != "custom") {
                     ad[["DT"]]()$set(editable = !!input$editable_choice)
                   } else {
                     ad[["DT"]]()$set(editable = NULL,
                                      callback = DT::JS('table.on("dblclick.dt","tr", function() {alert("You have double clicked a row, triggering any action you want!")})'))
                   }
                 })
                 
                 # Caption
                 observe(isolate(ad[["DT"]]())$set(caption = !!input$caption))
                 
               })
}