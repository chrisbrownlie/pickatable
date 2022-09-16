#' Table R6 class
#' 
#' Class for each table, using R6 so that only one version of each table is created 
#' in each app session.
#' 
#' @export
Table <- R6::R6Class(
  classname = "Table",
  
  public = list(
    
    #' @field pkg_name the name of the package used to create the table
    pkg_name = NULL,
    
    #' @field options a list of various options that can be used in the
    #' table definition
    options = list(),
    
    #' @field data the 
    
    #' @field definition the table definition as an rlang expr, using the
    #' options field
    definition = NULL,
    
    #' @description Initialise a new table object
    #' 
    #' @param pkg the name of the package used to create the table
    #' @param definition the default definition of the table
    #' @param options a list of options that can be referenced in the definition of the table
    #' 
    #' @return a new object of class Table
    initialise = function(pkg,
                          definition,
                          options) {
      
      stopifnot("pkg must be a string" = is.character(pkg),
                "pkg must be installed" = pkg %in% loadedNamespaces())
      self$pkg_name <- pkg
      
      stopifnot("definition must be an rlang expression" = rlang::is_expression(definition))
      self$definition <- definition
      
      stopifnot("options must be a list" = is.list(options))
      self$options <- options
    },
    
    #' @description get the current state of the table
    #' 
    #' @return the table object with its current options and definition
    get = function() {
      rlang::eval(self$definition)
    }
    
  ),
  
  private = list()
)