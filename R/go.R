#' Run app
#' 
#' @return launches the shiny app
#' 
#' @export
go <- function() {
  
  if (interactive()) {
    
    # If interactive session, start app
    shiny::runApp(appDir = system.file("app", 
                                       package = "pickatable",
                                       mustWork = TRUE))
    
  } else {
    
    # If not an interactive session, return the app as an app object
    shiny::shinyAppDir(appDir = system.file("app", 
                                            package = "pickatable",
                                            mustWork = TRUE))
    
  }
}