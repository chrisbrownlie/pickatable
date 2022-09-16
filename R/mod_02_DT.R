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
                                 data = app_data) {
                 ns <- session$ns
                 
                 # Create DT table object
                 data[["dt_table"]] <- reactiveValues(
                   pkg = "DT",
                   definition = reactive({
                     paste0(
                         "DT::datatable(",
                         parse_args(c("data" = data[["dataset"]],
                                      "rownames" = input$rownames,
                                      "colnames" = paste_list_as_vector(input$colnames),
                                      "caption" = in_quotes(input$caption),
                                      "filter" = in_quotes(input$filter),
                                      "selection" = in_quotes(input$selection),
                                      "editable" = in_quotes(table_editable()),
                                      "callback" = paste0("DT::JS(", in_quotes(table_callback()), ")"))),
                         ")"
                     )
                   })
                 )
                 
                 code_snippet_server("DT",
                                     table_def = data$dt_table$definition)
                 
                 # Render table
                 output$table <- DT::renderDT({
                   data$dt_table$definition() |>
                     rlang::parse_expr() |>
                     rlang::eval_bare()
                 })
                 
                 
                 # Options
                 column_names <- reactive(names(get(data[["dataset"]])))
                 output$colnames_ui <- renderUI({
                   selectInput(ns("colnames"),
                               label = "Columns to show:",
                               choices = column_names(),
                               selected = column_names(),
                               multiple = TRUE)
                 })
                 
                 table_editable <- reactiveVal(FALSE)
                 observeEvent(input$editable_choice, {
                   if (input$editable_choice != "custom") {
                     table_editable(input$editable_choice)
                   } else {
                     table_editable(FALSE)
                   }
                 })
                 
                 table_callback <- reactive({
                   if (input$editable_choice == "custom") {
                     'table.on("dblclick.dt","tr", function() {alert("You have double clicked a row, triggering any action you want!")})'
                   } else {
                     "return table;"
                   }
                 })
                 
               })
}