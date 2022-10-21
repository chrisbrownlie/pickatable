#' Module UI
#' 
#' @return module UI
code_snippet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    span(verbatimTextOutput(ns("snippet")), style="text-align:left;"),
    uiOutput(ns("button"))
  )
}


#' Module server
#' 
#' @return module server
code_snippet_server <- function(id,
                                table_def) {
  moduleServer(id = id,
               module = function(input,
                                 output,
                                 session,
                                 def = table_def) {
                 ns <- session$ns
                 
                 output$snippet <- renderPrint({
                   req(def)
                   def()$get()
                 })
                 output$button <- renderUI({
                   req(def)
                   snip_def <- gsub(def()$get(),
                                    pattern = "(?<=, )", 
                                    replacement = "\\1\n",
                                    perl = TRUE) |>
                     gsub(pattern = '\\"', replacement = '\\\\"')

                   sprintf('<button id="codesnip" type="button" class="btn btn-default action-button" data-clipboard-text="%s">
                            <i class="fa fa-clipboard" role="presentation" aria-label="clipboard icon">
                            </i>
                            Copy code
                           </button>',
                           snip_def) |>
                     HTML()
                 })
                 
               })
  
}