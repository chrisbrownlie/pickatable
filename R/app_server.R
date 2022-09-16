#' App server
#' 
#' A function that takes arguments input, output and session. This function
#' is not designed to be called interactively but instead acts as the server 
#' for a shiny app.
app_server <- function(input, output, session) {
  
  data <- reactiveValues()
  
  #outputOptions(output, "data", suspendWhenHidden = FALSE)
  
  about_server("about",
               app_data = data)
  
  dt_server("dt",
            app_data = data)
  
  flextable_server("flextable",
                   app_data = data)
  
  gt_server("gt",
            app_data = data)
  
  kable_server("kable",
               app_data = data)
  
  plotly_server("plotly",
                app_data = data)
  
  comparison_server("comparison",
                    app_data = data)
  
}