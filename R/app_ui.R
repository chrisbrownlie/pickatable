#' App UI
#' 
#' @import shiny
#' @import bslib
#' 
#' @return a shiny app UI
app_ui <- function() {
  
  bslib::page_navbar(
    title = "pickatable",
    theme = bslib::bs_theme(bootswatch = "yeti"),
    collapsible = TRUE,
    
    header = tagList(
      # Include custom CSS
      tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
      # Include custom icon
      tags$link(rel = "shortcut icon", href = ""),
      
      # Include custom JS
      tags$script(src = "clipboard.min.js"), # clipboard.js
      tags$script(src = "initiate_clipboard.js"), # custom clipboard function
    ),
    
    tabPanel("About",
             value = "tab_about",
             icon = NULL,
             about_ui("about")),
    
    tabPanel("DT",
             value = "tab_DT",
             icon = NULL,
             dt_ui("dt")),
    
    tabPanel("flextable",
             value = "tab_flextable",
             icon = NULL,
             flextable_ui("flextable")),
    
    tabPanel("gt",
             value = "tab_gt",
             icon = NULL,
             gt_ui("gt")),
    
    tabPanel("kableExtra",
             value = "tab_kable",
             icon = NULL,
             kable_ui("kable")),
    
    tabPanel("plotly",
             value = "tab_plotly",
             icon = NULL,
             plotly_ui("plotly")),
    
    tabPanel("Side-by-side",
             value = "tab_comparison",
             icon = NULL,
             comparison_ui("comparison"))
  )
}