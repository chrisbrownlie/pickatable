#' Table R6 class
#' 
#' Reactive-aware R6 class for a table that can be used to access its definition
#' and other metadata.
#' 
#' Reactive-awareness inspired by discussion here: https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890/8
#' 
#' @export
Table <- R6::R6Class(
  classname = "Table",
  
  private = list(
    fun = "",
    pkg = "",
    definition = NULL,
    arguments = NULL,
    
    # Reactivity
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function() {
      private$count <- private$count+1
      private$reactiveDep(private$count)
      invisible()
    },
    count = 0
  ),
  
  public = list(
    
    #' @description Initialise a new table object
    #' 
    #' @param pkg the name of the package that the table function is from
    #' @param fun the name of the function for creating the table
    #' @param ... arguments to pass to the table
    #' 
    #' @return a new object of class Table
    initialize = function(pkg,
                          fun,
                          ...) {
  
      stopifnot("pkg must be a string" = is.character(pkg))
      private$pkg <- pkg
      
      stopifnot("fun must be a string" = is.character(fun))
      private$fun <- fun

      # Quote all definition elements
      private$definition <- enexprs(...)
      
      # Get all possible arguments
      private$arguments <- rlang::fn_fmls_names(get(fun, envir = rlang::ns_env(pkg)))
      
      private$reactiveDep <- function(x) NULL
      invisible(self)
    },
    
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    },
    
    #' @description get valid named arguments as vector
    #' 
    #' @return all possible named arguments
    named_args = function() {
      private$arguments
    },
    
    #' @description overwrite the current table definition
    #' 
    #' @return saves the new definition of the table arguments
    set = function(...) {
      private$definition <- modifyList(private$definition, enexprs(...))
      private$invalidate()
    },
    
    #' @description get the current table definition
    #' 
    #' @param raw if TRUE, return list of arguments else return call that can be used to
    #' create table
    #' 
    #' @return the current definition of the table (code needed to plot/run it) as a call
    get = function(raw = F) {
      if (raw) {
        # List of arguments
        private$definition
      } else {
        # Code to create table
        rlang::call2(private$fun, !!!private$definition, .ns = private$pkg)
      }
    },
    
    #' @description plot the current table
    #' 
    #' @return the table object
    plot = function() {
      rlang::eval_bare(self$get())
    }
    
  )
)