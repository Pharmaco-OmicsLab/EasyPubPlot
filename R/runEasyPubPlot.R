#' EasyPubPlot - Easy and Publishable Ploting
#'
#' Interactive, customizable, and coding-free Shiny App to easily create publishable plots for scientific papers.
#' @export
#' @importFrom grDevices dev.off png
#' @importFrom stats setNames var
#' @importFrom utils download.file read.csv

runEasyPubPlot = function() {

  # library(shiny)
  # library(shinyjs)
  # library(shinyWidgets)
  # library(colourpicker)
  # library(bslib)
  # library(shinytoastr)  # for pop-up messeges
  # 
  # library(dplyr)
  # library(magrittr)
  # library(ggplot2)
  # library(tibble)
  # library(tidyr)
  # library(ggthemes)
  # library(EnhancedVolcano)
  # library(ComplexHeatmap)

  # Source UI
  source("R/ui.R")

  # Source Server
  source("R/server.R")

  # Run the Application
  shinyApp(ui = ui, server = server)

}
