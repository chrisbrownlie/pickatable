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
                   code_vec <- def() |>
                     strsplit(split = "(?<=, )",
                              perl = TRUE) |>
                     unlist()
                   formatted_vec <- purrr::imap(code_vec, function(x, i, cv = code_vec){
                     n_pre_brackets <- ifelse(i==1,
                                              0,
                                              length(grep("\\(", cv[1:i-1]))-length(grep("\\)", cv[1:i-1])))
                     # At the start of each line, add a number of tab spaces equivalent to
                     # the number of preceding open brackets minus the number of preceding 
                     # closing brackets
                     gsub(x, 
                          pattern = "^(.)", 
                          replacement = paste0(strrep("\t", n_pre_brackets), "\\1"))
                   })
                   
                   formatted_vec |>
                     paste(collapse = "\n") |>
                     cat()
                 })
                 output$button <- renderUI({
                   snip_def <- gsub(def(),
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