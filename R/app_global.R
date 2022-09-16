#' Create any variables needed for the app
#' 
#' @return creates variables and assigns them into the pickatable_global environment
app_global <- function() {
  if(length(ls())) stop("No arguments can be supplied to this function")

}

#' Unpack contents of pickatable global environment into another
#' 
#' @param into the environment to copy objects to
#' 
#' @return invisibly copies the contents of the pickatable_global environment into another
unpack_global <- function(into) {
  #for(n in ls(.pickatable_global, all.names=TRUE)) assign(n, get(n, .pickatable_global), into)
}