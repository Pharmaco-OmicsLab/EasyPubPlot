#' EasyPubPlot - Easy and Publishable Ploting
#'
#' Interactive, customizable, and coding-free Shiny App to easily create publishable plots for scientific papers.
#' @export
#' @importFrom grDevices dev.off png
#' @importFrom stats setNames var
#' @importFrom utils download.file read.csv

runEasyPubPlot = function() {

  # Run the Application
  library(shiny)
  shinyApp(ui = ui, server = server)

}

# Interactive (current version supports volcano plot and bubble plot)
runInteractiveEasyPubPlot = function() {
  
  # Run the Application
  library(shiny)
  shinyApp(ui = ui_Interactive, server = server_Interactive)
  
}
