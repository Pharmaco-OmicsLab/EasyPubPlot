#' EasyPubPlot - Easy and Publishable Ploting
#'
#' Interactive, customizable, and coding-free Shiny App to easily create publishable plots for scientific papers.
#' @export
#' @importFrom grDevices dev.off png
#' @importFrom stats setNames var
#' @importFrom utils download.file read.csv

runEasyPubPlot = function() {

  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(colourpicker)
  library(bslib)
  library(shinytoastr)  # for pop-up messeges

  # library(tidyverse)
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(tibble)
  library(tidyr)
  library(ggthemes)
  library(EnhancedVolcano)
  library(ComplexHeatmap)

  # Define UI
  ui <- fluidPage(
    # Custom CSS
    tags$style(HTML("

    /* Customzie sidebarLayout */
    .well {
      border-radius: 15px; /* Rounded corners for sidebar panel */
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); /* Subtle shadow for sidebar panel */
      background-color: #f8f9fa; /* Optional: Lighten the background */
      padding: 15px;
    }

    /* Prevent tab titles from being upper case */
    .navbar-nav .nav-link {
        text-transform: none; /* Ensure text is displayed in original case */
        font-size: 18px; /* Change to increase font size */
    }

    /* Customzie hearder font of tabPanel */
    .navbar-brand {
      text-transform: none !important; /* Prevent uppercase for the title */
      font-size: 30px; /* Change to increase font size */
    }

    h2, h3, h4, h5, h6 {
      text-transform: none !important; /* Prevent uppercase for headers */
    }

    h6 {
      font-size: 13px; /* Change to increase font size */
    }

    /* Scroll sidebar */
    .scrollable-sidebar {
      max-height: 700px;  /* Set the maximum height */
      overflow-y: auto;
      padding-right: 10px;  /* Add some padding for scrollbar clearance */
    }

  ")),


    navbarPage(
      title = "EasyPubPlot",
      id = "navbar",  # Set an ID here for reference
      theme = bs_theme(
        version = 4,
        bootswatch = "lux",
        primary = "#47B0C3",
        base_font = font_google("Roboto")
      ),

      useToastr(),  # Initialize shinytoastr
      shinyjs::useShinyjs(),  # Allow to use shinyjs

      # Introduction Tab
      tabPanel(
        title = "Welcome",
        tagList(
          # Use with action link below to customize its UI
          #   tags$head(
          #     tags$style(HTML("
          #   .clickable-link {
          #     color: blue;
          #     text-decoration: underline;
          #     cursor: pointer;
          #   }
          #   .clickable-link:hover {
          #     color: darkblue;
          #   }
          # "))
          #   ),

          tags$main(
            tags$h2("EasyPubPlot - Easy and Publishable Plotting"),
            tags$p(
              "EasyPubPlot provides an interactive and customizable tools to easily",
              "create publishable plots for scientific papers"
            ),
            # actionLink("go_to_tutorials",
            #            tags$h4("Click here to start", class = "clickable-link")),  # Clickable text

            tags$p("\n"),

            tags$button(
              id = "go_to_tutorials",
              class = "action-button shiny-bound-input",
              "Click here to start",
              style = "font-size: 20px; font-weight: bold; padding: 10px 20px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_tutorials', Math.random())"
            ),

            tags$p("\n"),
            div(
              img(src = "https://drive.google.com/thumbnail?id=12PUIle3BBlG8mCLfBd2uLNqXze_Nszyu", height = "500px"),
              style = "text-align: center;"
            )
          )
        )
      ),

      # Volcano Plot Tab
      tabPanel(
        title = "Volcano Plot",

        tags$button(
          id = "go_to_tutorials_VolcanoPlot",
          class = "action-button shiny-bound-input",
          "Back to Tutorials",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('go_to_tutorials_VolcanoPlot', Math.random())"
        ),

        # This work but not beautiful
        # actionButton("reload_app_button", "Reset App",
        #              style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;"),

        tags$button(
          id = "reload_tab_button_Volcano",
          class = "action-button shiny-bound-input",
          "Reset This Tab",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          # onclick = "Shiny.setInputValue('reload_tab_button_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        sidebarLayout(
          sidebarPanel(
            width = 4,

            # Add a scrollable div wrapper
            div(
              class = "scrollable-sidebar",

              id = "inputs_container_Volcano",  # Add an id to target for resetting

              # Controls for the Volcano Plot
              tabsetPanel(
                id = "volcanoTabs",
                type = "pills",

                # File upload inputs for Volcano Plot
                tabPanel(
                  "Data Upload",

                  # Use Pvalue or adjPvalue

                  tags$p(
                    "By default, the volcano plot module uses the adjusted P-value. Untick the option if you want to use the P-value instead.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  checkboxInput("checkbox_adjPvalue_Volcano", "Adjusted P-value", value = TRUE),

                  fileInput("volcanoFile", "Upload STAT Results File:", accept = c(".csv"))
                ),

                # Figure Sizes Tab
                tabPanel(
                  "Figure Sizes & Themes",

                  numericInput("plotWidth_Volcano", "Width (in pixels):", value = 800, step = 5),
                  numericInput("plotHeight_Volcano", "Height (in pixels):", value = 600, step = 5),

                  selectInput(
                    "plotTheme_Volcano", "Plot Theme:",
                    choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
                    selected = "none"
                  ),
                ),

                # Cut-offs Tab
                tabPanel(
                  "Cut-offs",

                  numericInput("FDR_cut_off_Volcano", "(Adjusted) P-value Cut-off:", min = 0, max = 10, value = 0.05, step = 0.01),
                  numericInput("FC_cut_off_Volcano", "Fold Change Cut-off:", min = 0, max = 500, value = 1.5, step = 0.5),

                  tags$p(
                    "NOTE: Please remember to manually update the cut-off values in the caption.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  # caption
                  checkboxInput("Show_caption_Volcano", "Show caption", value = TRUE),
                  textInput("caption_Volcano", "Caption:"),  # value will be returned from the server
                  numericInput("captionLabSize_Volcano", "Caption Size:", value = 16)

                ),

                tabPanel(
                  "Colors",

                  colourpicker::colourInput("color_NotSig_Volcano", "Not Significant Color:", value = "#848484"),
                  colourpicker::colourInput("color_DownFDR_Volcano", "(Adjusted) P-value, Down-regulation Color:", value = "#BCF1B9"),
                  colourpicker::colourInput("color_UpFDR_Volcano", "(Adjusted) P-value, Up-regulation:", value = "#FEB0B0"),
                  colourpicker::colourInput("color_DownFDR_FC_Volcano", "(Adjusted) P-value & FC, Down-regulation):", value = "#5FDD59"),
                  colourpicker::colourInput("color_UpFDR_FC_Volcano", "(Adjusted) P-value & FC, Up-regulation):", value = "#FE5E5E")

                ),

                # Points & Legends
                tabPanel(
                  "Points & Legends",
                  numericInput("pointSize_Volcano", "Point Size:", value = 3, step = 0.5),

                  # legend
                  checkboxInput("Show_legend_Volcano", "Show legend", value = TRUE),
                  numericInput("legendTextSize_Volcano", "Legend Text Size:", value = 12),
                  # numericInput("legendIconSize_Volcano", "Legend Icon Size:", value = 6)

                  # Legend position
                  selectInput(
                    "legendPosition_Volcano", "Legend Position:",
                    choices = c("right", "top", "bottom"),
                    selected = "top"
                  ),

                ),

                # Axis Labels Tab
                tabPanel(
                  "Axis Labels",
                  textInput("xLabel_Volcano", "X-axis Label:", value = "log2(FC)"),
                  textInput("yLabel_Volcano", "Y-axis Label:"), # value will be returned from the server
                  numericInput("labelSize_Volcano", "Axis Label Size:", value = 24),
                  checkboxInput("checkbox_Axis_bold_Volcano", "Axis bold", value = TRUE),

                  numericInput("tickLabelSize_Volcano", "Tick Label Size:", value = 20),
                  checkboxInput("checkbox_Tick_bold_Volcano", "Tick bold", value = FALSE)
                ),

                # Axis Limits & Breaks Tab
                tabPanel(
                  "Axis Limits",

                  tags$p(
                    "NOTE: If used, make sure to set both the minimum and maximum values.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  numericInput("xMin_Volcano", "X-axis Minimum:", value = NA, step = 0.1),
                  numericInput("xMax_Volcano", "X-axis Maximum:", value = NA, step = 0.1),
                  numericInput("yMin_Volcano", "Y-axis Minimum:", value = NA, step = 0.1),
                  numericInput("yMax_Volcano", "Y-axis Maximum:", value = NA, step = 0.1)
                ),

                tabPanel(
                  "Axis Breaks",
                  numericInput("xBreaks_Volcano", "X-axis Breaks:", value = NA, step = 0.1),
                  numericInput("yBreaks_Volcano", "Y-axis Breaks:", value = NA, step = 0.1)
                ),

                # Save Figure Tab
                tabPanel(
                  "Save Figure",
                  # Ensure the consistency with variables name in the server
                  numericInput("volcanoDPI", "Resolution (DPI):", value = 300, step = 300),
                  selectInput(
                    "formatdownloadVolcano", "Format:",
                    choices = c(".png", ".svg", ".tiff", ".pdf"),
                    selected = ".png"
                  ),
                  downloadButton("download_VolcanoPlot", "Download Plot")
                )
              )
            )

          ),

          mainPanel(
            plotOutput("Render_volcanoPlot", width = "100%", height = "600px")
          )
        )
      ),

      # Heatmap tap
      tabPanel(
        title = "Heatmap",

        tags$button(
          id = "go_to_tutorials_HeatmapSimple",
          class = "action-button shiny-bound-input",
          "Back to Tutorials",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('go_to_tutorials_HeatmapSimple', Math.random())"
        ),

        tags$button(
          id = "reload_tab_button_HeatmapSimple",
          class = "action-button shiny-bound-input",
          "Reset This Tab",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          # onclick = "Shiny.setInputValue('reload_tab_button_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        sidebarLayout(
          sidebarPanel(
            width = 4,

            # Add a scrollable div wrapper
            div(
              class = "scrollable-sidebar",

              id = "inputs_container_HeatmapSimple",  # Add an id to target for resetting

              # Tabs for different settings
              tabsetPanel(
                id = "HeatmapSimpletabs",
                type = "pills",

                # Data upload
                tabPanel(
                  "Data Upload",
                  # File upload inputs
                  fileInput("metadataFile_HeatmapSimple", "Upload Metadata File:", accept = c(".csv")),
                  fileInput("NormDataFile_HeatmapSimple", "Upload Normalized Data File:", accept = c(".csv")),

                  # Group Color => Use dynamical
                  uiOutput("groupLevelSelector_HeatmapSimple"),  # Cannot change group order in the ComplexHeatmap
                  uiOutput("dynamicColorInputs_HeatmapSimple"),
                  uiOutput("dynamicLegendInputs_HeatmapSimple"),

                  # Show col name
                  checkboxInput("checkbox_showColname_HeatmapSimple", "Show Column Name", value = FALSE)
                ),

                # # Group Tab
                # tabPanel(
                #   "Plot Appearance"
                # ),

                # Figure Sizes Tab
                tabPanel(
                  "Figure Sizes",

                  tags$p(
                    "TIP: Adjusting the width and height can help scale your plot as needed.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  numericInput("plotWidth_HeatmapSimple", "Width (in pixels):", value = 300, step = 5),
                  numericInput("plotHeight_HeatmapSimple", "Height (in pixels):", value = 600, step = 5),
                ),

                # Colors Tab
                tabPanel(
                  "Heatmap Colors",

                  # selectInput(
                  #   "Color_Palette_HeatmapSimple", "Select Color Palette:",
                  #   choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
                  #   selected = "none"
                  # ),
                  #
                  # tags$p(
                  #   "Or Select Manually:",
                  # ),

                  # Heatmap Color and Scale
                  colourpicker::colourInput("color_DownHeatmap_HeatmapSimple", "Down-regulation Color:", value = "#23446f"),
                  colourpicker::colourInput("color_UnchangedHeatmap_HeatmapSimple", "Unchange Color:", value = "white"),
                  colourpicker::colourInput("color_UpHeatmap_HeatmapSimple", "Up-regulation Color:", value = "#ad190d"),
                  numericInput("color_scaleHeatmap_HeatmapSimple", "Color Scale:", value = 2, step = 0.5)
                ),

                # Text Size Tab
                tabPanel(
                  "Text & Size",
                  # Feature Text Size
                  numericInput("size_Features_HeatmapSimple", "Features Size:", value = 12),
                  checkboxInput("checkbox_italicFeatures_HeatmapSimple", "Italic Text", value = FALSE),

                  # Top level
                  # numericInput("size_TopAnnotation_HeatmapSimple", "Top Annotation Size, top:", value = 16),

                  # Legend
                  textInput("TopAnnotation_legend_HeatmapSimple", "Level-1 Annotation:", value = "Group"),
                  textInput("HeatmapAnnotation_legend_HeatmapSimple", "Heatmap Annotation:", value = "Expression (scaled)")
                ),

                # Clustering Tab
                tabPanel(
                  "Clustering",
                  # Row clustering
                  checkboxInput("checkbox_Row_clustering_HeatmapSimple", "Cluster Row", value = TRUE),
                  # Col clustering
                  checkboxInput("checkbox_Col_clustering_HeatmapSimple", "Cluster Column", value = FALSE),
                ),

                # Save Figure Tab
                tabPanel(
                  "Save Figure",

                  numericInput("dpi_HeatmapSimple", "Resolution (DPI):", value = 300, step = 300),
                  # selectInput(
                  #   "formatdownload_HeatmapSimple", "Format:",
                  #   choices = c(".png", ".svg", ".tiff", ".pdf"),
                  #   selected = ".png"
                  # ),
                  downloadButton("download_HeatmapSimple", "Download Plot as .png")
                ),
              )
            )

          ),

          mainPanel(
            plotOutput("Render_HeatmapSimple", width = "100%", height = "600px")
          )
        )
      ),

      # Scores Plot Tab
      tabPanel(
        title = "Scores Plot",

        tags$button(
          id = "go_to_tutorials_ScoresPlot",
          class = "action-button shiny-bound-input",
          "Back to Tutorials",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('go_to_tutorials_ScoresPlot', Math.random())"
        ),

        tags$button(
          id = "reload_tab_button_ScorePlot",
          class = "action-button shiny-bound-input",
          "Reset This Tab",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          # onclick = "Shiny.setInputValue('reload_tab_button_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        sidebarLayout(
          sidebarPanel(
            width = 4,

            # Add a scrollable div wrapper
            div(
              class = "scrollable-sidebar",

              id = "inputs_container_ScorePlot",  # Add an id to target for resetting

              # Tabs for different settings
              tabsetPanel(
                id = "ScorePlottabs",
                type = "pills",

                # Data upload
                tabPanel(
                  "Data Upload",

                  # File upload inputs
                  fileInput("metadataFile_ScorePlot", "Upload Metadata File:", accept = c(".csv")),
                  fileInput("scoreFile_ScorePlot", "Upload Scores File:", accept = c(".csv")),

                  tags$p(
                    "NOTE: After the plot appears, please manually edit the % explained variance in the Axis Labels subtab.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  uiOutput("groupLevelSelector_ScorePlot"),  # This will be dynamically generated based on the uploaded data
                  uiOutput("dynamicColorInputs_ScorePlot"),

                  uiOutput("dynamicLegendInputs_ScorePlot"),  # For dynamic legend labels

                  checkboxInput("checkbox_95CI_ScorePlot", "Display 95% confidence ellipse", value = TRUE)

                ),

                # # Plot Appearance Tab
                # tabPanel(
                #   "Plot Appearance",
                #
                # ),

                # Figure Sizes Tab
                tabPanel(
                  "Figure Sizes & Themes",

                  numericInput("plotWidth_ScorePlot", "Width (in pixels):", value = 600, step = 5),
                  numericInput("plotHeight_ScorePlot", "Height (in pixels):", value = 600, step = 5),

                  selectInput(
                    "plotTheme_ScorePlot", "Plot Theme:",
                    choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
                    selected = "theme_Publication"
                  )
                ),

                # Points & Legends
                tabPanel(
                  "Points & Legends",
                  numericInput("pointSize_ScorePlot", "Point Size:", value = 5),

                  # legend
                  checkboxInput("Show_legend_ScorePlot", "Show legend", value = TRUE),
                  numericInput("legendTextSize_ScorePlot", "Legend Text Size:", value = 12),
                  # numericInput("legendIconSize_ScorePlot", "Legend Icon Size:", value = 6)

                  # Legend position
                  selectInput(
                    "legendPosition_ScorePlot", "Legend Position:",
                    choices = c("right", "top", "bottom"),
                    selected = "top"
                  )
                ),

                # Axis Labels Tab
                tabPanel(
                  "Axis Labels",
                  textInput("xLabel_ScorePlot", "X-axis Label:", value = "The first component (A %)"),
                  textInput("yLabel_ScorePlot", "Y-axis Label:", value = "The second component (B %)"),
                  numericInput("labelSize_ScorePlot", "Axis Label Size:", value = 25),
                  checkboxInput("checkbox_Axis_bold_ScorePlot", "Axis bold", value = TRUE),

                  numericInput("tickLabelSize_ScorePlot", "Tick Label Size:", value = 20),
                  checkboxInput("checkbox_Tick_bold_ScorePlot", "Tick bold", value = FALSE),
                ),

                # Axis Limits & Breaks Tab
                tabPanel(
                  "Axis Limits",

                  tags$p(
                    "NOTE: (1) If used, make sure to set both the minimum and maximum values. (2) If the ellipse looks disrupted, try setting broader ranges.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  numericInput("xMin_ScorePlot", "X-axis Minimum:", value = NA, step = 0.1),
                  numericInput("xMax_ScorePlot", "X-axis Maximum:", value = NA, step = 0.1),
                  numericInput("yMin_ScorePlot", "Y-axis Minimum:", value = NA, step = 0.1),
                  numericInput("yMax_ScorePlot", "Y-axis Maximum:", value = NA, step = 0.1)
                ),

                tabPanel(
                  "Axis Breaks",
                  numericInput("xBreaks_ScorePlot", "X-axis Breaks:", value = NA, step = 0.1),
                  numericInput("yBreaks_ScorePlot", "Y-axis Breaks:", value = NA, step = 0.1)
                ),

                # Save Figure Tab
                tabPanel(
                  "Save Figure",

                  # numericInput("plotWidth_ScorePlot", "Width (in pixels):", value = 600, step = 5),
                  # numericInput("plotHeight_ScorePlot", "Height (in pixels):", value = 600, step = 5),
                  numericInput("dpi_ScorePlot", "Resolution (DPI):", value = 300, step = 300),
                  selectInput(
                    "formatdownloadScorePlot", "Format:",
                    choices = c(".png", ".svg", ".tiff", ".pdf"),
                    selected = ".png"
                  ),
                  downloadButton("downloadScorePlot", "Download Plot")
                ),
              )
            )
          ),

          mainPanel(
            plotOutput("Render_ScorePlot", width = "100%", height = "600px")
          )
        )
      ),

      # Box Plot Tab
      tabPanel(
        title = "Box Plot",

        tags$button(
          id = "go_to_tutorials_BoxPlot",
          class = "action-button shiny-bound-input",
          "Back to Tutorials",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('go_to_tutorials_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_tab_button_BoxPlot",
          class = "action-button shiny-bound-input",
          "Reset This Tab",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          # onclick = "Shiny.setInputValue('reload_tab_button_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        sidebarLayout(
          sidebarPanel(
            width = 4,

            # Add a scrollable div wrapper
            div(
              class = "scrollable-sidebar",

              id = "inputs_container_BoxPlot",  # Add an id to target for resetting

              # Tabs for different settings
              tabsetPanel(
                id = "BoxPlottabs",
                type = "pills",

                # Data upload
                tabPanel(
                  "Data Upload",
                  # File upload inputs
                  fileInput("metadataFile_BoxPlot", "Upload Metadata File:", accept = c(".csv")),
                  fileInput("expressionFile_BoxPlot", "Upload Normalized Data File:", accept = c(".csv")),


                  uiOutput("groupLevelSelector_BoxPlot"),  # This will be dynamically generated based on the uploaded data
                  uiOutput("dynamicColorInputs_BoxPlot"),

                  uiOutput("dynamicLegendInputs_BoxPlot"), # For dynamic legend labels
                ),

                # # Plot Appearance Tab
                # tabPanel(
                #   "Plot Appearance",
                #
                #
                # ),

                # Figure Sizes Tab
                tabPanel(
                  "Figure Sizes  & Themes",

                  tags$p(
                    "TIP: Adjusting the width and height can help scale your plot as needed.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  numericInput("FacetnCol_BoxPlot", "Number of Box Plot's Columns:", value = 5, step = 1),

                  numericInput("plotWidth_BoxPlot", "Width (in pixels):", value = 1200, step = 5),
                  numericInput("plotHeight_BoxPlot", "Height (in pixels):", value = 1350, step = 5),

                  selectInput(
                    "plotTheme_BoxPlot", "Plot Theme:",
                    choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
                    selected = "theme_Publication"
                  )
                ),

                # point, jitter, width
                tabPanel(
                  "Sizes",
                  numericInput("pointSize_BoxPlot", "Point Size:", value = 3),
                  numericInput("BoxWidth_BoxPlot", "Box Width:", value = 0.5, step = 0.1),
                  numericInput("JitterWidth_BoxPlot", "Jitter Width:", value = 0.18, step = 0.02)
                ),

                # Axis Labels Tab
                tabPanel(
                  "Axis Labels",
                  textInput("yLabel_BoxPlot", "Y-axis Label:", value = "Normalized Abundance"),
                  numericInput("labelSize_BoxPlot", "Axis Label Size:", value = 20),
                  checkboxInput("checkbox_Axis_bold_BoxPlot", "Axis bold", value = TRUE),

                  numericInput("tickLabelSize_BoxPlot", "Tick Label Size:", value = 15),
                  checkboxInput("checkbox_Tick_bold_BoxPlot", "Tick bold", value = FALSE),

                  numericInput("stripLabelSize_BoxPlot", "Features Label Size:", value = 15, step = 1),
                ),

                # Save Figure Tab
                tabPanel(
                  "Save Figure",

                  numericInput("dpi_BoxPlot", "Resolution (DPI):", value = 300, step = 300),
                  selectInput(
                    "formatdownload_BoxPlot", "Format:",
                    choices = c(".png", ".svg", ".tiff", ".pdf"),
                    selected = ".png"
                  ),
                  downloadButton("download_BoxPlot", "Download Plot")
                ),
              )
            )
          ),

          mainPanel(
            plotOutput("Render_BoxPlot", width = "100%", height = "600px")
          )
        )
      ),

      # Dot Plot tab
      tabPanel(
        title = "Dot Plot",

        tags$button(
          id = "go_to_tutorials_DotPlot",
          class = "action-button shiny-bound-input",
          "Back to Tutorials",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('go_to_tutorials_DotPlot', Math.random())"
        ),

        tags$button(
          id = "reload_tab_button_DotPlot",
          class = "action-button shiny-bound-input",
          "Reset This Tab",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          # onclick = "Shiny.setInputValue('reload_tab_button_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        sidebarLayout(
          sidebarPanel(
            width = 4,

            # Add a scrollable div wrapper
            div(
              class = "scrollable-sidebar",

              id = "inputs_container_DotPlot",  # Add an id to target for resetting

              # Tabs for different settings
              tabsetPanel(
                id = "DotPlottabs",
                type = "pills",

                # Data upload
                tabPanel(
                  "Data Upload",

                  # GSEA or ORA
                  selectInput(
                    "PathwayAnalysisMode_DotPlot", "Pathway Analysis Mode:",
                    choices = c("ORA", "GSEA"),
                    selected = "ORA"
                  ),

                  # Metabolomics or Transcriptomics
                  selectInput(
                    "PathwayFromOmics_DotPlot", "Using:",
                    choices = c("Metabolomics", "Transcriptomics"),
                    selected = "Transcriptomics"
                  ),

                  # Use Pvalue or adjPvalue
                  checkboxInput("checkbox_adjPvalue_DotPlot", "Adjusted P-value"),  # Get automatically from the server

                  # File upload inputs
                  fileInput("PathwayDataFile_DotPlot", "Upload Pathway Results File:", accept = c(".csv"))
                ),

                # Plot Size and Theme Tab
                tabPanel(
                  "Figure Sizes & Themes",

                  tags$p(
                    "TIP: Adjusting the width and height can help scale your plot as needed.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  # Plot Size
                  numericInput("plotWidth_DotPlot", "Width (in pixels):", value = 800, step = 50),
                  numericInput("plotHeight_DotPlot", "Height (in pixels):", value = 600, step = 50),

                  # Plot Theme
                  selectInput(
                    "plotTheme_DotPlot", "Plot Theme:",
                    choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
                    selected = "theme_Publication"
                  )
                ),

                # Customized Points Tab
                tabPanel(
                  "Points",

                  numericInput("small_size_scale_DotPlot", "Point Size Scale, Small:", value = 2, step = 1),
                  numericInput("big_size_scale_DotPlot", "Point Size Scale, Big:", value = 7, step = 1),

                  colourpicker::colourInput("color_lowPvalue_DotPlot", "(Adj) P-value Color, Low:", value = "#7fc97f"),
                  colourpicker::colourInput("color_interPvalue_DotPlot", "(Adj) P-value Color, Intermediate:", value = "#fdb462"),
                  colourpicker::colourInput("color_highPvalue_DotPlot", "(Adj) P-value Color, High:", value = "#ef3b2c")
                ),

                # Legend Tab
                tabPanel(
                  "Legend",

                  numericInput("legendTitleSize_DotPlot", "Legend Title Size:", value = 15, step = 1),
                  numericInput("legendTextSize_DotPlot", "Legend Text Size:", value = 14, step = 1),
                  numericInput("legendkeySize_DotPlot", "Legend Icon Size:", value = 0.7, step = 0.1, min = 0.1, max = 4),

                  textInput("ColorTitle_DotPlot", "Color Title:"), # value will be returned from the server

                  textInput("PointSizeTitle_DotPlot", "Point Size Title:", value = "Hits Count"),
                ),

                # Axis Labels Tab
                tabPanel(
                  "Axis Labels",
                  textInput("xLabel_DotPlot", "X-axis Label:"), # value will be returned from the server
                  numericInput("labelSize_DotPlot", "Axis Label Size:", value = 18),
                  checkboxInput("checkbox_Axis_bold_DotPlot", "Axis Bold", value = TRUE),

                  numericInput("tickLabelSize_xAxis_DotPlot", "Tick Label Size, x-Axis:", value = 13),
                  numericInput("tickLabelSize_yAxis_DotPlot", "Tick Label Size, y-Axis:", value = 15),
                  checkboxInput("checkbox_Tick_bold_DotPlot", "Tick Bold", value = FALSE),
                ),

                # Axis Limits & Breaks Tab
                tabPanel(
                  "Limits & Breaks",
                  numericInput("xMin_DotPlot", "X-axis Minimum:", value = NA, step = 0.1),
                  numericInput("xMax_DotPlot", "X-axis Maximum:", value = NA, step = 0.1),

                  numericInput("xBreaks_DotPlot", "X-axis Breaks:", value = NA, step = 0.1)
                ),

                # Save Figure Tab
                tabPanel(
                  "Save Figure",

                  # numericInput("plotWidth_DotPlot", "Width (in pixels):", value = 800, step = 50),
                  # numericInput("plotHeight_DotPlot", "Height (in pixels):", value = 600, step = 50),
                  numericInput("dpi_DotPlot", "Resolution (DPI):", value = 300, step = 300),
                  selectInput(
                    "formatdownload_DotPlot", "Format:",
                    choices = c(".png", ".svg", ".tiff", ".pdf", ".pptx"),
                    selected = ".png"
                  ),
                  downloadButton("download_DotPlot", "Download Plot")
                ),
              )
            )
          ),

          mainPanel(
            plotOutput("Render_DotPlot", width = "auto", height = "auto")
          )
        )
      ),

      # BubblePlot Tab
      tabPanel(
        title = "Bubble Plot",

        tags$button(
          id = "go_to_tutorials_BubblePlot",
          class = "action-button shiny-bound-input",
          "Back to Tutorials",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('go_to_tutorials_BubblePlot', Math.random())"
        ),

        tags$button(
          id = "reload_tab_button_BubblePlot",
          class = "action-button shiny-bound-input",
          "Reset This Tab",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          # onclick = "Shiny.setInputValue('reload_tab_button_BoxPlot', Math.random())"
        ),

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        sidebarLayout(
          sidebarPanel(
            width = 4,

            # Add a scrollable div wrapper
            div(
              class = "scrollable-sidebar",

              id = "inputs_container_BubblePlot",  # Add an id to target for resetting

              # Tabs for different settings
              tabsetPanel(
                id = "BubblePlottabs",
                type = "pills",

                # Data upload
                tabPanel(
                  "Data Upload",

                  # Metabolomics or Transcriptomics
                  selectInput(
                    "PathwayFromOmics_BubblePlot", "Omics Data:",
                    choices = c("Metabolomics", "Transcriptomics"),
                    selected = "Metabolomics"
                  ),

                  # Use Pvalue or adjPvalue
                  checkboxInput("checkbox_adjPvalue_BubblePlot", "Adjusted P-value"), # value will be returned from the server

                  # File upload inputs
                  fileInput("PathwayDataFile_BubblePlot", "Upload Pathway Results File:", accept = c(".csv"))
                ),

                # Plot Size and Theme Tab
                tabPanel(
                  "Figure Sizes & Themes",

                  # Plot Size
                  numericInput("plotWidth_BubblePlot", "Width (in pixels):", value = 800, step = 50),
                  numericInput("plotHeight_BubblePlot", "Height (in pixels):", value = 600, step = 50),

                  # Plot Theme
                  selectInput(
                    "plotTheme_BubblePlot", "Plot Theme:",
                    choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
                    selected = "theme_Publication"
                  )
                ),

                # Points
                tabPanel(
                  "Points",

                  numericInput("small_size_scale_BubblePlot", "Point Size Scale, Small:", value = 4, step = 1),
                  numericInput("big_size_scale_BubblePlot", "Point Size Scale, Big:", value = 12, step = 1),

                  colourpicker::colourInput("color_lowPvalue_BubblePlot", "(Adj) P-value Color, Low:", value = "#7fc97f"),
                  colourpicker::colourInput("color_interPvalue_BubblePlot", "(Adj) P-value Color, Intermediate:", value = "#fdb462"),
                  colourpicker::colourInput("color_highPvalue_BubblePlot", "(Adj) P-value Color, High:", value = "#ef3b2c")
                ),

                # Legend
                tabPanel(
                  "Legend",

                  checkboxInput("showLegend_BubblePlot", "Show Legend", value = TRUE),

                  numericInput("legendTitleSize_BubblePlot", "Legend Title Size:", value = 18, step = 1),
                  numericInput("legendTextSize_BubblePlot", "Legend Text Size:", value = 15, step = 1),
                  numericInput("legendkeySize_BubblePlot", "Legend Icon Size:", value = 0.7, step = 0.1, min = 0.1, max = 4),

                  textInput("ColorTitle_BubblePlot", "Color Title:"), # value will be returned from the server

                  textInput("PointSizeTitleBubblePlot", "Point Size Title:") # value will be returned from the server
                ),

                # Axis Labels Tab
                tabPanel(
                  "Axis Labels",
                  textInput("xLabel_BubblePlot", "X-axis Label:"), # value will be returned from the server

                  textInput("yLabel_BubblePlot", "Y-axis Label:"), # value will be returned from the server

                  numericInput("labelSize_BubblePlot", "Axis Label Size:", value = 22),
                  checkboxInput("checkbox_Axis_bold_BubblePlot", "Axis Bold", value = TRUE),

                  numericInput("tickLabelSize_BubblePlot", "Tick Label Size:", value = 18),
                  checkboxInput("checkbox_Tick_bold_BubblePlot", "Tick Bold", value = FALSE),
                ),

                # Axis Limits & Breaks Tab
                tabPanel(
                  "Axis Limits",

                  tags$p(
                    "NOTE: If used, make sure to set both the minimum and maximum values.",
                    style = "font-size: 16px; font-weight: bold;"
                  ),

                  numericInput("xMin_BubblePlot", "X-axis Minimum:", value = NA, step = 0.1),
                  numericInput("xMax_BubblePlot", "X-axis Maximum:", value = NA, step = 0.1),

                  numericInput("yMin_BubblePlot", "Y-axis Minimum:", value = NA, step = 0.1),
                  numericInput("yMax_BubblePlot", "Y-axis Maximum:", value = NA, step = 0.1)
                ),

                tabPanel(
                  "Axis Breaks",
                  numericInput("xBreaks_BubblePlot", "X-axis Breaks:", value = NA, step = 0.1),
                  numericInput("yBreaks_BubblePlot", "Y-axis Breaks:", value = NA, step = 0.1)
                ),

                # Save Figure Tab
                tabPanel(
                  "Save Figure",
                  # numericInput("plotWidth_BubblePlot", "Width (in pixels):", value = 800, step = 50),
                  # numericInput("plotHeight_BubblePlot", "Height (in pixels):", value = 600, step = 50),
                  numericInput("dpi_BubblePlot", "Resolution (DPI):", value = 300, step = 300),
                  selectInput(
                    "formatdownload_BubblePlot", "Format:",
                    choices = c(".png", ".svg", ".tiff", ".pdf", ".pptx"),
                    selected = ".png"
                  ),
                  downloadButton("download_BubblePlot", "Download Plot")
                ),
              )
            )
          ),

          mainPanel(
            plotOutput("Render_BubblePlot", width = "100%", height = "600px")
          )
        )
      ),

      # Tutorial Tab
      tabPanel(
        title = "Tutorials",

        tags$button(
          id = "reload_app_button",
          class = "action-button shiny-bound-input",
          "Reset App",
          style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
          onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
        ),

        fluidPage(
          fluidRow(
            column(
              width = 3,
              #wellPanel(
              navlistPanel(
                id = "introTab",
                # tabPanel("Welcome", value = "welcome"),
                tabPanel("Volcano Plot", value = "VolcanoPlot_infor"),
                tabPanel("Heatmap", value = "HeatmapSimple_infor"),
                tabPanel("Scores PLot", value = "ScoresPlot_infor"),
                tabPanel("Box PLot", value = "BoxPlot_infor"),
                tabPanel("Dot Plot", value = "DotPlot_infor"),
                tabPanel("Bubble Plot", value = "BubblePlot_infor")
              )
              #)
            ),
            column(
              width = 8,
              uiOutput("TutorialsContent")
            )
          )
        )
      )
    )
  )


  # Define Server
  server <- function(input, output, session) {

    #<-- Reset -->
    # Observe the reset button and reload the app when clicked
    observeEvent(input$reload_app_button, {
      session$reload()
    })

    # Observe the reset Volcano Plot button and reset the Volcano Plot data
    observeEvent(input$reload_tab_button_Volcano, {

      values$volcanoData <- NULL

      # Reset all input elements inside the container with id 'inputs_container_Volcano'
      shinyjs::reset("inputs_container_Volcano")

      toastr_info(message = "Please re-upload your data files.", timeOut = 4500,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, ignoreInit = TRUE)  # Avoid duplicate triggers

    # Observe the reset Heatmap Plot button and reset the heatmap Plot data
    observeEvent(input$reload_tab_button_HeatmapSimple, {

      values$metadata_HeatmapSimple <- NULL
      values$NormData_HeatmapSimple <- NULL
      updateSelectInput(session, "groupLevels_selected_HeatmapSimple", selected = character(0))

      # Reset all input elements inside the container with id 'inputs_container_HeatmapSimple'
      shinyjs::reset("inputs_container_HeatmapSimple")

      toastr_info(message = "Please re-upload your data files.", timeOut = 4500,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, ignoreInit = TRUE)  # Avoid duplicate triggers

    # Observe the reset Box Plot button and reset the Box Plot data
    observeEvent(input$reload_tab_button_ScorePlot, {

      values$metadata_ScorePlot <- NULL
      values$score_ScorePlot <- NULL
      updateSelectInput(session, "groupLevels_selected_ScorePlot", selected = character(0))

      # Reset all input elements inside the container with id 'inputs_container_ScorePlot'
      shinyjs::reset("inputs_container_ScorePlot")

      toastr_info(message = "Please re-upload your data files.", timeOut = 4500,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, ignoreInit = TRUE)  # Avoid duplicate triggers

    # Observe the reset Box Plot button and reset the Box Plot data
    observeEvent(input$reload_tab_button_BoxPlot, {

      values$metadata_BoxPlot <- NULL
      values$expression_BoxPlot <- NULL
      updateSelectInput(session, "groupLevels_selected_BoxPlot", selected = character(0))

      # Reset all input elements inside the container with id 'inputs_container_BoxPlot'
      shinyjs::reset("inputs_container_BoxPlot")

      toastr_info(message = "Please re-upload your data files.", timeOut = 4500,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, ignoreInit = TRUE)  # Avoid duplicate triggers

    # Observe the reset dot Plot button and reset the dot Plot data
    observeEvent(input$reload_tab_button_DotPlot, {

      values$PathwayData_DotPlot <- NULL

      # Reset all input elements inside the container with id 'inputs_container_DotPlot'
      shinyjs::reset("inputs_container_DotPlot")

      toastr_info(message = "Please re-upload your data files.", timeOut = 4500,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, ignoreInit = TRUE)  # Avoid duplicate triggers

    # Observe the reset bubble Plot button and reset the bubble Plot data
    observeEvent(input$reload_tab_button_BubblePlot, {

      values$PathwayData_BubblePlot <- NULL

      # Reset all input elements inside the container with id 'inputs_container_BubblePlot'
      shinyjs::reset("inputs_container_BubblePlot")

      toastr_info(message = "Please re-upload your data files.", timeOut = 4500,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, ignoreInit = TRUE)  # Avoid duplicate triggers


    #<-- Navigate when click -->
    ## Go to tutorial
    observeEvent(input$go_to_tutorials, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")
    })

    ## Go to each module of the tutorial
    observeEvent(input$go_to_tutorials_VolcanoPlot, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")  # Go to Tutorials tab
      updateTabsetPanel(session, inputId = "introTab", selected = "VolcanoPlot_infor")  # Then, go to Volcano Plot sub-tab
    })

    observeEvent(input$go_to_tutorials_HeatmapSimple, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")  # Go to Tutorials tab
      updateTabsetPanel(session, inputId = "introTab", selected = "HeatmapSimple_infor")  # Then, go to Volcano Plot sub-tab
    })

    observeEvent(input$go_to_tutorials_ScoresPlot, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")  # Go to Tutorials tab
      updateTabsetPanel(session, inputId = "introTab", selected = "ScoresPlot_infor")  # Then, go to Volcano Plot sub-tab
    })

    observeEvent(input$go_to_tutorials_BoxPlot, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")  # Go to Tutorials tab
      updateTabsetPanel(session, inputId = "introTab", selected = "BoxPlot_infor")  # Then, go to Volcano Plot sub-tab
    })

    observeEvent(input$go_to_tutorials_DotPlot, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")  # Go to Tutorials tab
      updateTabsetPanel(session, inputId = "introTab", selected = "DotPlot_infor")  # Then, go to Volcano Plot sub-tab
    })

    observeEvent(input$go_to_tutorials_BubblePlot, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Tutorials")  # Go to Tutorials tab
      updateTabsetPanel(session, inputId = "introTab", selected = "BubblePlot_infor")  # Then, go to Volcano Plot sub-tab
    })

    ## Go to Volcano Plot module
    observeEvent(input$go_to_VolcanoPlot_module, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Volcano Plot")
    })

    ## Go to Heatmap module
    observeEvent(input$go_to_HeatmapSimple_module, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Heatmap")
    })

    ## Go to Scores Plot module
    observeEvent(input$go_to_ScoresPlot_module, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Scores Plot")
    })

    ## Go to Box Plot module
    observeEvent(input$go_to_BoxPlot_module, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Box Plot")
    })

    ## Go to Dot Plot module
    observeEvent(input$go_to_DotPlot_module, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Dot Plot")
    })

    ## Go to Bubble Plot module
    observeEvent(input$go_to_BubblePlot_module, {
      updateTabsetPanel(session, inputId = "navbar", selected = "Bubble Plot")
    })

    #<-- Dynamic content for Introduction tab Handling -->
    output$TutorialsContent <- renderUI({
      if (input$introTab == "ScoresPlot_infor") {

        tagList(
          # Use with action link below
          #     tags$head(
          #       tags$style(HTML("
          #   .clickable-link {
          #     color: blue;
          #     text-decoration: underline;
          #     cursor: pointer;
          #   }
          #   .clickable-link:hover {
          #     color: darkblue;
          #   }
          # "))
          #     ),

          tags$main(
            tags$h3("2D scores plot"),

            img(src = "https://drive.google.com/thumbnail?id=1uacjgrXmt0b97V7UdmCf_uUloAYgFDkk", height = "300px"),

            tags$h4("First, download a step-by-step tutorial"),
            downloadLink("download_ScoresPlot_Tutorial_pdf", "Link to download", class = "clickable-link"),

            tags$h4("Next, prepare the input data"),

            tags$p("The input data are principal component scores obtained in the PCA/PLS-DA analysis"),
            tags$p("This module also requires metadata"),
            downloadLink("download_ScoresPlot_ExampleScoresData", "Link to download example scores data,", class = "clickable-link"),  # Link to the example data
            downloadLink("download_ScoresPlot_ExampleMetaData", "Link to download example metadata data", class = "clickable-link"),

            # actionLink("go_to_ScoresPlot_module",
            #            tags$h4("Click here to make your first plot", class = "clickable-link")),  # Clickable text

            tags$p("\n"),

            tags$button(
              id = "go_to_ScoresPlot_module",
              class = "action-button shiny-bound-input",
              "Click here to make your amazing plot",
              style = "font-size: 20px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_ScoresPlot_module', Math.random())"
            )
          )
        )


      } else if (input$introTab == "VolcanoPlot_infor") {

        tagList(
          tags$main(
            tags$h3("Volcano plot"),

            img(src = "https://drive.google.com/thumbnail?id=1ubOEI67ERmQzcJ4tMcArX8-MoUou4-ei&usp", height = "200px"),

            tags$h4("First, download a step-by-step tutorial"),
            downloadLink("download_VolcanoPlot_Tutorial_pdf", "Link to download"),

            tags$h4("Next, prepare the input data"),

            tags$p("The input data were statistical output from e.g., MetaboAnalyst, ExpressAnalyst, and DESeq2."),
            tags$p("Five columns are required: Features, FoldChange, log2FoldChange, P-value, adj.P.Val."),
            tags$p(
              "Note: Please ensure the columns are in the same order as in the example data. If a column’s information isn’t available, just leave it empty (e.g., FOLDCHANGE and P_VALUE). Column names can be different.",
              style = "font-weight: bold;"
            ),
            downloadLink("download_VolcanoPlot_ExampleData", "Link to download example input data"),  # Link to the example data

            # Add a placeholder for the data frame
            tableOutput("data_summary_table"),  # This will display the data frame  # or Tried: DT::dataTableOutput("data_summary_table"),

            tags$p("\n"),

            tags$button(
              id = "go_to_VolcanoPlot_module",
              class = "action-button shiny-bound-input",
              "Click here to make your amazing plot",
              style = "font-size: 20px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_VolcanoPlot_module', Math.random())"
            )
          )
        )


      } else if (input$introTab == "HeatmapSimple_infor") {

        tagList(
          tags$main(
            tags$h3("Heatmap plot"),

            img(src = "https://drive.google.com/thumbnail?id=1Rx7G21L0abazFohXaFhJtgRwMKmwtbpU", height = "300px"),

            tags$h4("First, download a step-by-step tutorial"),
            downloadLink("download_HeatmapSimple_Tutorial_pdf", "Link to download", class = "clickable-link"),

            tags$h4("Next, prepare the input data"),

            tags$p("The input data are e.g., normalized gene expression or metabolites abundance data"),
            tags$p("This module also requires metadata"),
            downloadLink("download_HeatmapSimple_ExampleNormData", "Link to download example normalized data,"),  # Link to the example data
            downloadLink("download_HeatmapSimple_ExampleMetaData", "Link to download example metadata data"),

            tags$p("\n"),

            tags$button(
              id = "go_to_HeatmapSimple_module",
              class = "action-button shiny-bound-input",
              "Click here to make your amazing plot",
              style = "font-size: 20px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_HeatmapSimple_module', Math.random())"
            )
          )
        )

      } else if (input$introTab == "BoxPlot_infor") {

        tagList(
          tags$main(
            tags$h3("Box plot"),

            img(src = "https://drive.google.com/thumbnail?id=1S-N9OWh4rhA3JWtBWcF75ullRgfMj-T9", height = "200px"),

            tags$h4("First, download a step-by-step tutorial"),
            downloadLink("download_BoxPlot_Tutorial_pdf", "Link to download"),

            tags$h4("Next, prepare the input data"),

            tags$p("The input data are e.g., normalized gene expression or metabolites abundance data"),
            tags$p("This module also requires metadata"),
            downloadLink("download_BoxPlot_ExampleNormData", "Link to download example normalized data,"),  # Link to the example data
            downloadLink("download_BoxPlot_ExampleMetaData", "Link to download example metadata data"),

            tags$p("\n"),

            tags$button(
              id = "go_to_BoxPlot_module",
              class = "action-button shiny-bound-input",
              "Click here to make your amazing plot",
              style = "font-size: 20px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_BoxPlot_module', Math.random())"
            )
          )
        )

      } else if (input$introTab == "DotPlot_infor") {

        tagList(
          tags$main(
            tags$h3("Dot plot"),

            img(src = "https://drive.google.com/thumbnail?id=126HZDZ1pwZ4R6qwgPaOWJxpvEwJ0_JXp", height = "300px"),

            tags$h4("First, download a step-by-step tutorial"),
            downloadLink("download_DotPlot_Tutorial_pdf", "Link to download"),

            tags$h4("Next, prepare the input data"),

            tags$p("The input data are e.g., results obtained via Pathway Analysis in clusterProfiler, ExpressAnalyst, and MetaboAnalyst"),
            tags$p(
              "Note: Please ensure the columns are in the same order as in the example data. If a column’s information isn’t available, just leave it empty. Column names can be different.",
              style = "font-weight: bold;"
            ),
            tags$p("\n"),
            downloadLink("download_DotPlot_ExampleGSEA_1", "Link to download example GSEA-derived pathway data (Transcriptomics): Example 1"),  # Link to the example data
            # downloadLink("download_DotPlot_ExampleGSEA_2", "Example 2"),  ## CHECK AGAIN: temporal removal of Example 2: results from ExpressAnalyst
            tags$p("\n"),
            downloadLink("download_DotPlot_ExampleORA_1", "Link to download example ORA-derived pathway data: Example 1 (Transcriptomics),"),
            downloadLink("download_DotPlot_ExampleORA_2", "Example 2 (Metabolomics)"),

            tags$p("\n"),

            tags$button(
              id = "go_to_DotPlot_module",
              class = "action-button shiny-bound-input",
              "Click here to make your amazing plot",
              style = "font-size: 20px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_DotPlot_module', Math.random())"
            )
          )
        )

      } else if (input$introTab == "BubblePlot_infor") {

        tagList(
          tags$main(
            tags$h3("Bubble plot"),

            img(src = "https://drive.google.com/thumbnail?id=1292cIWMe3LIjavag3V1aBLw75g7bnFSv", height = "300px"),

            tags$h4("First, download a step-by-step tutorial"),
            downloadLink("download_BubblePlot_Tutorial_pdf", "Link to download"),

            tags$h4("Next, prepare the input data"),

            tags$p("The input data are e.g., results obtained via Pathway Analysis in clusterProfiler, ExpressAnalyst, and MetaboAnalyst"),
            tags$p(
              "Note: Please ensure the columns are in the same order as in the example data. If a column’s information isn’t available, just leave it empty. Column names can be different.",
              style = "font-weight: bold;"
            ),
            tags$p("\n"),

            downloadLink("download_BubblePlot_ExampleORA_1", "Link to download example Metabolomics-derived pathway data"),  # Link to the example data
            tags$p("\n"),
            downloadLink("download_BubblePlot_ExampleORA_2", "Link to download example Transcriptomics-derived pathway data"),

            tags$p("\n"),

            tags$button(
              id = "go_to_BubblePlot_module",
              class = "action-button shiny-bound-input",
              "Click here to make your amazing plot",
              style = "font-size: 20px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
              onclick = "Shiny.setInputValue('go_to_BubblePlot_module', Math.random())"
            )
          )
        )

      }

    })


    #<-- Download example data and tutorials Handling -->
    # Scores Plot
    ## Scores data
    output$download_ScoresPlot_ExampleScoresData <- downloadHandler(
      filename = function() {
        "ScoresPlot_ExampleScoreData_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1sattL6OMC4VHhd_Xok0uvTi1pACe7tl_",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Metadata
    output$download_ScoresPlot_ExampleMetaData <- downloadHandler(
      filename = function() {
        "ScoresPlot_ExampleMetadataData_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1sVH-q8RB5AjJTuR_yqCu2GEx3madd2XR",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorial
    output$download_ScoresPlot_Tutorial_pdf <- downloadHandler(
      filename = function() {
        "EasyPubPlot_Tutorials_ScoresPlot.pdf"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1QOMruiA5DHI0nSuuEskXYhJY8TOyg3XM",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    # Volcano Plot
    ## STAT data
    output$download_VolcanoPlot_ExampleData <- downloadHandler(
      filename = function() {
        "VolcanoPlotExampleData.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1tM88JhwkxQ4KIrCeX-023Q3hRcgoFxQs",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorials
    output$download_VolcanoPlot_Tutorial_pdf <- downloadHandler(
      filename = function() {
        "EasyPubPlot_Tutorials_VolcanoPlot.pdf"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1SKrJcmSJyRRvss9H6tajsOL_B7DquCDK",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorial - Example data frame to display
    output$data_summary_table <- renderTable({  # if use DT pacakge: DT::renderDataTable({
      data.frame(
        Features = c("Feature_1", "Feature_2", "Feature_3", "Feature_4", "Feature_5"),
        FoldChange = c("", "", "", "", ""),
        log2FoldChange = c(-0.22764, 0.081665, -1.3846, 0.13877, -0.47932),
        P_value = c("", "", "", "", ""),
        adj.P.Val = c(0.36784, 0.81759, 0.21195, 0.83978, 0.0014746)
      )
    }, rownames = FALSE, colnames = TRUE)

    # Heatmap Plot
    ## Heatmap data
    output$download_HeatmapSimple_ExampleNormData <- downloadHandler(
      filename = function() {
        "HeatmapBoxPlot_ExampleNormData_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1RehE0MZxJnFCSejUt-V9g9MKVOqaQULn",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Metadata
    output$download_HeatmapSimple_ExampleMetaData <- downloadHandler(
      filename = function() {
        "HeatmapBoxPlot_ExampleMetaData_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1RhubgrcRfo-0w_avJdmMu6HwFq9Ot5T9",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorial
    output$download_HeatmapSimple_Tutorial_pdf <- downloadHandler(
      filename = function() {
        "EasyPubPlot_Tutorials_HeatmapSimple.pdf"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1WEiHhXnGdpm5p0qX89U2rJkpBKT-NZ5I",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    # Box Plot
    ## Box Norm data
    output$download_BoxPlot_ExampleNormData <- downloadHandler(
      filename = function() {
        "HeatmapBoxPlot_ExampleNormData_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1RehE0MZxJnFCSejUt-V9g9MKVOqaQULn",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Metadata
    output$download_BoxPlot_ExampleMetaData <- downloadHandler(
      filename = function() {
        "HeatmapBoxPlot_ExampleMetaData_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1RhubgrcRfo-0w_avJdmMu6HwFq9Ot5T9",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorial
    output$download_BoxPlot_Tutorial_pdf <- downloadHandler(
      filename = function() {
        "EasyPubPlot_Tutorials_BoxPlot.pdf"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1WJ7VQ-03TREDM6ECkUe-jLYegz_KHkDi",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    # Dot Plot
    ## Example 1 data - GSEA - clusterProfiler
    output$download_DotPlot_ExampleGSEA_1 <- downloadHandler(
      filename = function() {
        "DotPlot_ExampleGSEA_1.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=12HKJacKLBwhXmQs184MCidczKgg4N6Om",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Example 2 data - GSEA - ExpressAnalyst
    output$download_DotPlot_ExampleGSEA_2 <- downloadHandler(
      filename = function() {
        "DotPlot_ExampleGSEA_2.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=12NVDJM-0G23ekUXvJcJUDwutCDgfpPhk",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Example 1 data - ORA - ExpressAnalyst
    output$download_DotPlot_ExampleORA_1 <- downloadHandler(
      filename = function() {
        "DotPlot_BubblePlot_ExampleORA_Transcriptomics.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=12GJzca4ETuyC308eLyc3K-4q0pt7yCUJ",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Example 2 data - ORA - MetaboAnalyst
    output$download_DotPlot_ExampleORA_2 <- downloadHandler(
      filename = function() {
        "DotPlot_BubblePlot_ExampleORA_Metabolomics.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=12LoaTHegT-LSxcIrV1ZVA6DAB2MDAuL8",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorial
    output$download_DotPlot_Tutorial_pdf <- downloadHandler(
      filename = function() {
        "EasyPubPlot_Tutorials_DotPlot.pdf"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1H8ICEeOC1bq3-E1tpGYGwar3xt--iU0z",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }

    )

    # Bubble Plot
    ## Example 1 data - ORA - ExpressAnalyst
    output$download_BubblePlot_ExampleORA_1 <- downloadHandler(
      filename = function() {
        "DotPlot_BubblePlot_ExampleORA_Metabolomics.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=12LoaTHegT-LSxcIrV1ZVA6DAB2MDAuL8",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Example 2 data - ORA - MetaboAnalyst
    output$download_BubblePlot_ExampleORA_2 <- downloadHandler(
      filename = function() {
        "DotPlot_BubblePlot_ExampleORA_Transcriptomics.csv"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=12GJzca4ETuyC308eLyc3K-4q0pt7yCUJ",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }
    )

    ## Tutorial
    output$download_BubblePlot_Tutorial_pdf <- downloadHandler(
      filename = function() {
        "EasyPubPlot_Tutorials_BubblePlot.pdf"  # Name of the file to be downloaded
      },
      content = function(file) {
        # Download the file from Google Drive and save it to 'file'
        download.file(
          url = "https://drive.google.com/uc?export=download&id=1H5qavKwdq2QaH3ooR_ryDza5jtF4TZpZ",
          destfile = file,
          mode = "wb"  # 'wb' mode to handle binary files like PDFs
        )
      }

    )


    #<-- Define some background function -->
    ## theme_Publication - copy from the source code
    theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.2), hjust = 0.5),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(#face = "bold",
                  size = rel(1)),
                axis.title.y = element_text(angle = 90,vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(),
                axis.line = element_line(colour="black"),
                axis.ticks = element_line(),
                panel.grid.major = element_line(colour="#f0f0f0"),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.size= unit(0.2, "cm"),
                legend.margin = margin(unit(0, "cm")),
                legend.title = element_text(face="italic"),
                plot.margin=unit(c(10,5,5,5),"mm"),
                strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold")
        ))

    }

    ## theme_Publication (for DotPlot and BubblePlot) - copy from the source code
    theme_Publication_modifed_DotPlot <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.2), hjust = 0.5),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(#face = "bold",
                  size = rel(1)),
                axis.title.y = element_text(angle = 90,vjust = 2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(),
                axis.line = element_line(colour="black"),
                axis.ticks = element_line(),
                panel.grid.major = element_line(colour="#f0f0f0"),
                panel.grid.minor = element_blank(),
                # legend.key = element_rect(colour = NA),
                # legend.position = "bottom",
                # legend.direction = "horizontal",
                # legend.key.size= unit(0.2, "cm"),
                # legend.margin = unit(0, "cm"),
                # legend.title = element_text(face="italic"),
                plot.margin = unit(c(10,5,5,5),"mm"),
                strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
                strip.text = element_text(face = "bold")
        ))

    }

    scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

    }

    scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

    }


    #<--Reactive values to store uploaded data for all modules -->
    values <- reactiveValues(
      # PCA/PLS-DA score_ScorePlot plot
      metadata_ScorePlot = NULL, score_ScorePlot = NULL,
      # Volcano plots
      volcanoData = NULL,
      # Box Plots
      expression_BoxPlot = NULL, metadata_BoxPlot= NULL,
      # Heatmap simple
      metadata_HeatmapSimple = NULL, NormData_HeatmapSimple = NULL,
      # Dot Plot
      PathwayData_DotPlot = NULL,
      # Bubble Plot
      PathwayData_BubblePlot = NULL
    )

    #<-- PCA/PLS-DA score_ScorePlot plot Handling -->
    # Observe metadata_ScorePlot file upload
    observeEvent(input$metadataFile_ScorePlot, {
      req(input$metadataFile_ScorePlot)
      values$metadata_ScorePlot <- read.csv(input$metadataFile_ScorePlot$datapath)

      # Data integrity (i.e., two columns)
      if (ncol(values$metadata_ScorePlot) == 2) {

        values$metadata_ScorePlot <- values$metadata_ScorePlot %>%
          setNames(c("Sample", "Group")) %>%  # Standardize the variable name
          mutate(across(c("Group"), ~ gsub(" ", "_", .)))

      }

    })

    # Observe score_ScorePlot file upload
    observeEvent(input$scoreFile_ScorePlot, {
      req(input$scoreFile_ScorePlot)
      values$score_ScorePlot <- read.csv(input$scoreFile_ScorePlot$datapath, row.names = 1, check.names = FALSE) %>%
        rownames_to_column(var  = "Sample")

      # Determine group levels dynamically after both files are uploaded
      if (!is.null(values$metadata_ScorePlot)) {
        group_levels_ScorePlot <- unique(values$metadata_ScorePlot$Group)

        # Update UI for selecting and ordering group levels
        output$groupLevelSelector_ScorePlot <- renderUI({
          selectInput("groupLevels_selected_ScorePlot", "Group Levels (If used, click on the blank space -> Press the backspace key to delete -> Select the appropriate order))",
                      choices = group_levels_ScorePlot,
                      selected = group_levels_ScorePlot,
                      multiple = TRUE)
        })
      }

      # # SHow pop-up messages
      # Sys.sleep(3)
      # toastr_info(message = "Ensure: (1) 2 columns in metadata; and (2) identical sample names between files.", timeOut = 12000,
      #             position = "top-right", closeButton = TRUE)
      toastr_info(title = "NOTE", message = "If the plot doesn’t appear or you see an error message, please double-check your input data and try again. If the issue persists, don't hesitate to contact us—we're here to help!", timeOut = 12000,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

      # Check matched sample names or not
      if (!(identical(sort(values$score_ScorePlot$Sample), sort(values$metadata_ScorePlot$Sample)))) {
        toastr_error("Sample names did not match between 2 files.", position = "top-right", timeOut = 12000)
      }

    })

    # # Check matched sample names or not
    # observeEvent(input$scoreFile_ScorePlot, {
    #
    #
    #
    # })

    # UI for dynamic color inputs based on group levels
    output$dynamicColorInputs_ScorePlot <- renderUI({
      req(input$groupLevels_selected_ScorePlot)  # Ensure group levels are available

      # Create a list of color inputs for each group level
      Publication_color_code_ScorePlot = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
      color_inputs_ScorePlot <- lapply(seq_along(input$groupLevels_selected_ScorePlot), function(i) {
        level <- input$groupLevels_selected_ScorePlot[i]
        default_color <- Publication_color_code_ScorePlot[(i - 1) %% length(Publication_color_code_ScorePlot) + 1]  # Cycle through Publication_color_code_ScorePlot
        colourpicker::colourInput(inputId = paste0("color_ScorePlot_", level), label = paste("Color for", level, "Group:"), value = default_color)
      })

      # Return the list of color input elements
      do.call(tagList, color_inputs_ScorePlot)
    })

    # Reactive expression to create a named color palette from the selected group levels
    color_palette_ScorePlot <- reactive({
      req(input$groupLevels_selected_ScorePlot)  # Ensure group levels are available

      # Define your Publication_color_code_ScorePlot palette
      Publication_color_code_ScorePlot <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")

      # Assign colors based on the group levels, cycling through Publication_color_code_ScorePlot
      sapply(seq_along(input$groupLevels_selected_ScorePlot), function(i) {
        level <- input$groupLevels_selected_ScorePlot[i]

        # If input is not available yet, fallback to Publication_color_code_ScorePlot
        input_color_ScorePlot <- input[[paste0("color_ScorePlot_", level)]]

        if (is.null(input_color_ScorePlot)) {
          # Assign color from Publication_color_code_ScorePlot based on the index
          default_color <- Publication_color_code_ScorePlot[(i - 1) %% length(Publication_color_code_ScorePlot) + 1]
          return(default_color)
        }

        return(input_color_ScorePlot)
      }, simplify = FALSE)
    })

    # UI for allowing to edit legend labels
    output$dynamicLegendInputs_ScorePlot <- renderUI({
      req(input$groupLevels_selected_ScorePlot)  # Ensure group levels are available

      # Create text input fields for each group level to edit legend labels
      legend_inputs_ScorePlot <- lapply(input$groupLevels_selected_ScorePlot, function(level) {
        textInput(
          inputId = paste0("legend_ScorePlot_", level),
          label = paste("Legend label for", level, "Group:"),
          value = level  # Set default value to the current level name
        )
      })

      # Return the list of legend input elements
      do.call(tagList, legend_inputs_ScorePlot)
    })

    # Reactive expression to create a named vector of legend labels
    legend_labels_ScorePlot <- reactive({
      req(input$groupLevels_selected_ScorePlot)  # Ensure group levels are available

      # Collect legend labels dynamically from inputs
      labels_ScorePlot <- sapply(input$groupLevels_selected_ScorePlot, function(level) {
        input[[paste0("legend_ScorePlot_", level)]]  # Access each text input value dynamically
      }, simplify = TRUE)

      # Return a named vector where the names are the group levels
      labels_ScorePlot
    })

    # Render the plot
    output$Render_ScorePlot <- renderPlot({
      req(values$metadata_ScorePlot, values$score_ScorePlot, input$groupLevels_selected_ScorePlot) # Ensure data and group level input are available

      # Combine datasets
      PCA_score_df <- values$score_ScorePlot %>%
        inner_join(values$metadata_ScorePlot %>% dplyr::select(Sample, Group), by = "Sample") %>%
        mutate(Group = factor(Group, levels = input$groupLevels_selected_ScorePlot))  # Use selected group levels

      # Generate dynamic color palette based on the number of groups
      color_code_ScorePlot <- color_palette_ScorePlot()  # Get dynamic color palette

      # Get the legend labels from reactive expression
      legend_labels_vector_ScorePlot <- legend_labels_ScorePlot()

      # Define the theme dynamically
      plot_theme <- switch(
        input$plotTheme_ScorePlot,
        "theme_Publication" = theme_Publication(),
        "theme_bw" = theme_bw(),
        "theme_minimal" = theme_minimal(),
        "theme_linedraw" = theme_linedraw(),
        "theme_classic" = theme_classic(),
        "theme_gray" = theme_gray(),
        "theme_bw" = theme_bw(),
      )

      # Get max x-value and y-value
      # max(abs(pull(PCA_score_df[2]))) -> max_x_limits_ScorePlot
      # max(abs(pull(PCA_score_df[3]))) -> max_y_limits_ScorePlot

      # Prepare axis limits and breaks
      x_limits <- if (!is.na(input$xMin_ScorePlot) && !is.na(input$xMax_ScorePlot) && input$xMin_ScorePlot < input$xMax_ScorePlot) c(input$xMin_ScorePlot, input$xMax_ScorePlot) else NULL #c(-max_x_limits_ScorePlot - 20, max_x_limits_ScorePlot + 20)
      y_limits <- if (!is.na(input$yMin_ScorePlot) && !is.na(input$yMax_ScorePlot) && input$yMin_ScorePlot < input$yMax_ScorePlot) c(input$yMin_ScorePlot, input$yMax_ScorePlot) else NULL #c(-max_y_limits_ScorePlot, max_y_limits_ScorePlot)
      x_breaks <- if (!is.na(input$xBreaks_ScorePlot) && !is.na(input$xMin_ScorePlot) && !is.na(input$xMax_ScorePlot) && input$xBreaks_ScorePlot > 0) seq(from = input$xMin_ScorePlot, to = input$xMax_ScorePlot, by = input$xBreaks_ScorePlot) else waiver()
      y_breaks <- if (!is.na(input$yBreaks_ScorePlot) && !is.na(input$yMin_ScorePlot) && !is.na(input$yMax_ScorePlot) && input$yBreaks_ScorePlot > 0) seq(from = input$yMin_ScorePlot, to = input$yMax_ScorePlot, by = input$yBreaks_ScorePlot) else waiver()

      # Make a data.frame for 95CI -> Follow `MetaboAnalystR` package (https://github.com/xia-lab/MetaboAnalystR/blob/1c6aa245388f7c0ba617111e264fa53bec221c83/R/stats_chemometrics.R#L227)
      ## Extract score
      pc1  <- pull(PCA_score_df[2])
      pc2  <- pull(PCA_score_df[3])

      ## Get group and levels
      cls1 <- PCA_score_df$Group
      lvs <- levels(cls1)

      ## Generate array for ellipse with 100 points, using self-calculated var and mean of pc1 and pc2 <-- Follow MetaboAnlystR package
      pts.array <- array(0, dim = c(100,2,length(lvs)))

      for(i in 1:length(lvs)){
        inx <- cls1 == lvs[i];
        groupVar <- var(cbind(pc1[inx],pc2[inx]), na.rm=T);
        groupMean <- cbind(mean(pc1[inx], na.rm=T),mean(pc2[inx], na.rm=T));
        pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = 0.95, npoints=100);
      }

      ### Convert array to df for ploting
      pts.df <- 1:length(lvs) %>%
        lapply(function(i) as.data.frame(pts.array[,,i])) %>%
        do.call(rbind, .) %>% #setNames(c("X", "Y"))  # Rename columns if needed
        dplyr::mutate(Group = rep(lvs, each = 100))

      # Generate the Score plot
      p_ScorePlot = ggplot(PCA_score_df, aes(x = pull(PCA_score_df[2]), y = pull(PCA_score_df[3]), color = Group, fill = Group)) +
        geom_point(size = input$pointSize_ScorePlot, alpha = 1) +
        scale_color_manual(name = NULL, values = color_code_ScorePlot, labels = legend_labels_vector_ScorePlot) +
        scale_fill_manual(name = NULL, values = color_code_ScorePlot, labels = legend_labels_vector_ScorePlot) +
        # stat_ellipse(geom = "polygon", aes(fill = Group), type = "norm", level = 0.95, alpha = 0.2, show.legend = FALSE, size = 0) +
        labs(x = input$xLabel_ScorePlot, y = input$yLabel_ScorePlot) +
        plot_theme +
        theme(
          axis.title.x = element_text(size = input$labelSize_ScorePlot),
          axis.title.y = element_text(size = input$labelSize_ScorePlot),
          axis.text.x = element_text(size = input$tickLabelSize_ScorePlot),
          axis.text.y = element_text(size = input$tickLabelSize_ScorePlot),
          legend.position = input$legendPosition_ScorePlot,
          legend.title = element_blank(),
          legend.text = element_text(size = input$legendTextSize_ScorePlot)
        ) +
        #coord_cartesian(xlim = x_limits, ylim = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        scale_y_continuous(limits = y_limits, breaks = y_breaks)

      # Change to two 1 row for right legend
      if ((input$legendPosition_Volcano == "right")) {
        p_volcano = p_volcano + guides(color = guide_legend(nrow = 1, byrow = FALSE))
      }

      # Conditionally add the 95% CI ellipse based on the checkbox
      if (input$checkbox_95CI_ScorePlot) {
        p_ScorePlot <- p_ScorePlot +
          geom_polygon(aes(x = V1, y = V2, fill = Group), data = pts.df, alpha = 0.2, color = NA)
        # stat_ellipse(
        #   geom = "polygon", aes(fill = Group), type = "norm", level = 0.95, alpha = 0.2, show.legend = FALSE, size = 0
        # )
      }

      # Conditionally add the bold for axis and tick
      if (input$checkbox_Axis_bold_ScorePlot) {
        p_ScorePlot <- p_ScorePlot + theme(axis.title = element_text(face = "bold"))
      }

      if (input$checkbox_Tick_bold_ScorePlot) {
        p_ScorePlot <- p_ScorePlot + theme(axis.text = element_text(face = "bold"))
      }

      # Assign final plot for exporting
      p_ScorePlot <<- p_ScorePlot

      # Render the plot
      p_ScorePlot



    }, width = reactive({ input$plotWidth_ScorePlot }), height = reactive({ input$plotHeight_ScorePlot }), res  = 72)


    # <-- Download handlers -->
    ## Score plot
    output$downloadScorePlot <- downloadHandler(
      filename = function() {
        paste(gsub("-", "_", Sys.Date()), "_2DScores_Plot", input$formatdownloadScorePlot, sep = "")
      },
      content = function(file) {
        ggsave(
          file, plot = p_ScorePlot,
          # Adjust height and width accordingly
          width = input$plotWidth_ScorePlot * input$dpi_ScorePlot / 72,
          height = input$plotHeight_ScorePlot * input$dpi_ScorePlot / 72,
          dpi = input$dpi_ScorePlot,
          units = "px"
        )
      }
    )


    # <-- Volcano Plot Handling -->
    observeEvent(input$volcanoFile, {
      req(input$volcanoFile)
      values$volcanoData <- read.csv(input$volcanoFile$datapath)

      # Data integrity (i.e., 5 columns)
      if (ncol(values$volcanoData) == 5) {

        if (input$checkbox_adjPvalue_Volcano) {
          # FDR
          values$volcanoData = values$volcanoData %>%
            setNames(c("Features", "FC", "log2FoldChange", "Not_use", "FDR"))
        } else {
          # P-value
          values$volcanoData = values$volcanoData %>%
            setNames(c("Features", "FC", "log2FoldChange", "FDR", "Not_use")) #pseudo FDR name to avoid to modify the below pre-coded variable name
        }
      }

      # SHow pop-up messages
      # toastr_info(message = "Ensure: (1) 5 columns in files.", timeOut = 12000,
      #             position = "top-right", closeButton = TRUE)
      toastr_info(title = "NOTE", message = "If the plot doesn’t appear or you see an error message, please double-check your input data and try again. If the issue persists, don't hesitate to contact us—we're here to help!", timeOut = 12000,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)
    })

    # Define the AdjPval or Pval and update the label automatically
    observeEvent(input$checkbox_adjPvalue_Volcano, {

      # yLabel
      updateTextInput(
        session,
        "yLabel_Volcano",
        value = if (input$checkbox_adjPvalue_Volcano) "-log10(Adjusted P-value)" else "-log10(P-value)"
      )

      # Update color label
      updateTextInput(
        session,
        "caption_Volcano",
        value = if (input$checkbox_adjPvalue_Volcano) "FC cut-off, 1.5; Adjusted P-value cut-off, 0.05" else "FC cut-off, 1.5; P-value cut-off, 0.05"
      )

    })

    output$Render_volcanoPlot <- renderPlot({
      req(values$volcanoData)

      # Define the theme dynamically
      plot_theme <- switch(
        input$plotTheme_Volcano,
        "theme_Publication" = theme_Publication_modifed_DotPlot(),
        "theme_bw" = theme_bw(),
        "theme_minimal" = theme_minimal(),
        "theme_linedraw" = theme_linedraw(),
        "theme_classic" = theme_classic(),
        "theme_gray" = theme_gray(),
        "theme_bw" = theme_bw(),
      )

      # Get max x-value (i.e., log2FC)
      max(abs(values$volcanoData$log2FoldChange)) -> max_x_limits_Volcano

      # Prepare axis limits and breaks
      x_limits <- if (!is.na(input$xMin_Volcano) && !is.na(input$xMax_Volcano) && input$xMin_Volcano < input$xMax_Volcano) c(input$xMin_Volcano, input$xMax_Volcano) else c(-max_x_limits_Volcano - 2, max_x_limits_Volcano + 2)
      y_limits <- if (!is.na(input$yMin_Volcano) && !is.na(input$yMax_Volcano) && input$yMin_Volcano < input$yMax_Volcano) c(input$yMin_Volcano, input$yMax_Volcano) else NULL
      x_breaks <- if (!is.na(input$xBreaks_Volcano) && !is.na(input$xMin_Volcano) && !is.na(input$xMax_Volcano) && input$xBreaks_Volcano > 0) seq(from = input$xMin_Volcano, to = input$xMax_Volcano, by = input$xBreaks_Volcano) else waiver()
      y_breaks <- if (!is.na(input$yBreaks_Volcano) && !is.na(input$yMin_Volcano) && !is.na(input$yMax_Volcano) && input$yBreaks_Volcano > 0) seq(from = input$yMin_Volcano, to = input$yMax_Volcano, by = input$yBreaks_Volcano) else waiver()

      # Volcano plot function
      draw_volcano <- function(stat_df, use_FDR = TRUE, FC_cut_of = 1.5, FDR_cut_of = 0.05){
        keyvals2 <- case_when(
          # FDR and log2FC
          (stat_df$log2FoldChange < -log2(FC_cut_of)) & (stat_df$FDR < FDR_cut_of)  ~ input$color_DownFDR_FC_Volcano,
          (stat_df$log2FoldChange > log2(FC_cut_of)) & (stat_df$FDR < FDR_cut_of)   ~ input$color_UpFDR_FC_Volcano,
          # FDR
          (stat_df$log2FoldChange < 0) & (stat_df$FDR < FDR_cut_of)                 ~ input$color_DownFDR_Volcano,
          (stat_df$log2FoldChange > 0) & (stat_df$FDR < FDR_cut_of)                 ~ input$color_UpFDR_Volcano,

          # Not sig
          TRUE                                                                      ~ input$color_NotSig_Volcano
        )

        if (use_FDR) {

          names(keyvals2)[keyvals2 == input$color_NotSig_Volcano] <- 'Not significant'
          names(keyvals2)[keyvals2 == input$color_DownFDR_Volcano] <- 'Adjusted P-value (Down-regulation)'
          names(keyvals2)[keyvals2 == input$color_UpFDR_Volcano] <- 'Adjusted P-value (Up-regulation)'
          names(keyvals2)[keyvals2 == input$color_DownFDR_FC_Volcano] <- 'Adjusted P-value and FC (Down-regulation)'
          names(keyvals2)[keyvals2 == input$color_UpFDR_FC_Volcano] <- 'Adjusted P-value and FC (Up-regulation)'

        } else {

          names(keyvals2)[keyvals2 == input$color_NotSig_Volcano] <- 'Not significant'
          names(keyvals2)[keyvals2 == input$color_DownFDR_Volcano] <- 'P-value (Down-regulation)'
          names(keyvals2)[keyvals2 == input$color_UpFDR_Volcano] <- 'P-value (Up-regulation)'
          names(keyvals2)[keyvals2 == input$color_DownFDR_FC_Volcano] <- 'P-value and FC (Down-regulation)'
          names(keyvals2)[keyvals2 == input$color_UpFDR_FC_Volcano] <- 'P-value and FC (Up-regulation)'

        }

        library(EnhancedVolcano)
        if (FC_cut_of != 1 & FDR_cut_of != 0) {
          EnhancedVolcano(
            stat_df,
            lab = "",
            x = 'log2FoldChange',
            y = 'FDR',
            xlab = input$xLabel_Volcano,
            ylab = input$yLabel_Volcano,
            axisLabSize = input$labelSize_Volcano,
            title = NULL,
            pCutoff = NA,
            FCcutoff = NA,
            cutoffLineWidth = 0.8,
            pointSize = input$pointSize_Volcano,
            labSize = 0,
            boxedLabels = FALSE,
            colAlpha = 0.48,
            colCustom = keyvals2,
            # legendLabSize = input$legendTextSize_Volcano,
            legendIconSize = input$pointSize_Volcano,
            drawConnectors = FALSE,
            subtitle = "",
            # caption = input$caption_Volcano,
            # captionLabSize = input$captionLabSize_Volcano,
            hline = c(FDR_cut_of),
            hlineCol = c("grey30"),
            hlineType = c("dotted"),
            hlineWidth = c(0.8),
            vline = c(-log2(FC_cut_of), log2(FC_cut_of)),
            vlineCol = c("grey30", "grey30"),
            vlineType = c("dotted", "dotted"),
            vlineWidth = c(0.8, 0.8)
          )
        } else if (FC_cut_of == 1 & FDR_cut_of != 0) {
          EnhancedVolcano(
            stat_df,
            lab = "",
            x = 'log2FoldChange',
            y = 'FDR',
            xlab = input$xLabel_Volcano,
            ylab = input$yLabel_Volcano,
            axisLabSize = input$labelSize_Volcano,
            title = NULL,
            pCutoff = NA,
            FCcutoff = NA,
            cutoffLineWidth = 0.8,
            pointSize = input$pointSize_Volcano,
            labSize = 0,
            boxedLabels = FALSE,
            colAlpha = 0.48,
            colCustom = keyvals2,
            # legendLabSize = input$legendTextSize_Volcano,
            legendIconSize = input$pointSize_Volcano,
            drawConnectors = FALSE,
            subtitle = "",
            # caption = input$caption_Volcano,
            # captionLabSize = input$captionLabSize_Volcano,
            hline = c(FDR_cut_of),
            hlineCol = c("grey30"),
            hlineType = c("dotted"),
            hlineWidth = c(0.8),
            # vline = c(-log2(FC_cut_of), log2(FC_cut_of)),
            # vlineCol = c("grey30", "grey30"),
            # vlineType = c("dotted", "dotted"),
            # vlineWidth = c(0.8, 0.8)
          )
        }

      }

      # Call the volcano plot function
      draw_volcano(
        values$volcanoData,
        use_FDR = input$checkbox_adjPvalue_Volcano,
        FC_cut_of = input$FC_cut_off_Volcano, FDR_cut_of = input$FDR_cut_off_Volcano
      ) -> p_volcano

      # If chose theme_Publication -> use default customization from EnhancedVolcano
      if (!(input$plotTheme_Volcano == "theme_Publication")) {
        p_volcano + plot_theme -> p_volcano
      } else {
        p_volcano +
          ggplot2::theme(legend.position = "right") -> p_volcano
      }

      # Other customization
      p_volcano = p_volcano +
        ggplot2::theme(
          axis.title.x = element_text(size = input$labelSize_Volcano),
          axis.title.y = element_text(size = input$labelSize_Volcano),
          axis.text.x = element_text(size = input$tickLabelSize_Volcano),
          axis.text.y = element_text(size = input$tickLabelSize_Volcano),
          legend.position = input$legendPosition_Volcano,
          legend.title = element_blank(),
          legend.text = element_text(size = input$legendTextSize_Volcano)
        ) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks) +
        # labs(caption = input$caption_Volcano) +
        ggplot2::theme(
          plot.caption = element_text(size = input$captionLabSize_Volcano)
        )

      # Change to two rows for top and bottom legend (and with FC != 1)
      if (!(input$legendPosition_Volcano == "right") & (input$FC_cut_off_Volcano != 1)) {
        p_volcano = p_volcano + guides(color = guide_legend(nrow = 2, byrow = FALSE))
      }

      # Show caption or not
      if (input$Show_caption_Volcano) {
        p_volcano = p_volcano + labs(caption = input$caption_Volcano)
      } else {
        p_volcano = p_volcano + labs(caption = "")
      }

      # Show legend or not
      if (!(input$Show_legend_Volcano)) {
        p_volcano + ggplot2::theme(legend.position = "none") -> p_volcano
      }

      # Conditionally add the bold for axis and tick
      if (input$checkbox_Axis_bold_Volcano) {
        p_volcano <- p_volcano + theme(axis.title = element_text(face = "bold"))
      }

      if (input$checkbox_Tick_bold_Volcano) {
        p_volcano <- p_volcano + theme(axis.text = element_text(face = "bold"))
      }

      # Assign final plot for exporting
      p_volcano <<- p_volcano

      # For render
      p_volcano

    }, width = reactive({ input$plotWidth_Volcano }), height = reactive({ input$plotHeight_Volcano }), res  = 72)

    # <-- Download handlers -->
    ## Volcano plot
    output$download_VolcanoPlot <- downloadHandler(
      filename = function() {
        paste(gsub("-", "_", Sys.Date()), "_Volcano_Plot", input$formatdownloadVolcano, sep = "")
      },
      content = function(file) {
        ggsave(
          file, plot = p_volcano,
          # Adjust height and width accordingly
          width = input$plotWidth_Volcano * input$volcanoDPI / 72,
          height = input$plotHeight_Volcano * input$volcanoDPI / 72,
          dpi = input$volcanoDPI,
          units = "px"
        )
      }
    )

    #<-- Boxplot Handling -->
    # Observe metadata_BoxPlot file upload
    observeEvent(input$metadataFile_BoxPlot, {
      req(input$metadataFile_BoxPlot)
      values$metadata_BoxPlot <- read.csv(input$metadataFile_BoxPlot$datapath)

      # Data integrity (i.e., two columns)
      if (ncol(values$metadata_BoxPlot) == 2) {

        values$metadata_BoxPlot <- values$metadata_BoxPlot %>%
          setNames(c("Sample", "Group")) %>%  # Standardize the variable name
          mutate(across(c("Group"), ~ gsub(" ", "_", .))) # Treat metadata if containing space

      }

    })

    # Observe expression_BoxPlot file upload
    observeEvent(input$expressionFile_BoxPlot, {

      # SHow pop-up messages
      # toastr_info(message = "Ensure: (1) 2 columns in metadata; (2) identical sample names between files; and (3) not duplicated features.", timeOut = 15000,
      #             position = "top-right", closeButton = TRUE)
      toastr_info(title = "NOTE", message = "If the plot doesn’t appear or you see an error message, please double-check your input data and try again. If the issue persists, don't hesitate to contact us—we're here to help!", timeOut = 15000,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

      # Load input data
      req(input$expressionFile_BoxPlot)

      ## Show messages if failed
      tryCatch({

        values$expression_BoxPlot <- read.csv(input$expressionFile_BoxPlot$datapath, row.names = 1, check.names = FALSE)

        # Check matched sample names or not
        if (!(identical(sort(names(values$expression_BoxPlot)), sort(values$metadata_BoxPlot$Sample)))) {
          toastr_error("Sample names did not match between 2 files.", position = "top-right", timeOut = 15000)
        }

      }, error = function(e) {
        # If an error occurs, show a failure message
        toastr_error(title = "ERROR", "Data containe duplicated features.", position = "top-right", timeOut = 15000)

      })

      # Treat data if samples containing space <- Not needed
      # names(values$expression_BoxPlot) = gsub(" ", "_", names(values$expression_BoxPlot))

      # Determine group levels dynamically after both files are uploaded
      if (!is.null(values$metadata_BoxPlot)) {
        # combined_data <- inner_join(values$expression_BoxPlot, values$metadata_BoxPlot %>% dplyr::select(Sample, Group), by = "Sample")
        group_levels_BoxPlot <- unique(values$metadata_BoxPlot$Group)

        # Update UI for selecting and ordering group levels
        output$groupLevelSelector_BoxPlot <- renderUI({
          selectInput("groupLevels_selected_BoxPlot", "Group Levels (If used, click on the blank space -> Press the backspace key to delete -> Select the appropriate order)",
                      choices = group_levels_BoxPlot,
                      selected = group_levels_BoxPlot,
                      multiple = TRUE)
        })
      }

    })

    # UI for dynamic color inputs based on group levels
    output$dynamicColorInputs_BoxPlot <- renderUI({
      req(input$groupLevels_selected_BoxPlot)  # Ensure group levels are available

      # Create a list of color inputs for each group level
      Publication_color_code_BoxPlot = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
      color_inputs_BoxPlot <- lapply(seq_along(input$groupLevels_selected_BoxPlot), function(i) {
        level <- input$groupLevels_selected_BoxPlot[i]
        default_color <- Publication_color_code_BoxPlot[(i - 1) %% length(Publication_color_code_BoxPlot) + 1]  # Cycle through Publication_color_code_BoxPlot
        colourpicker::colourInput(inputId = paste0("color_BoxPlot_", level), label = paste("Color for", level, "Group:"), value = default_color)
      })

      # Return the list of color input elements
      do.call(tagList, color_inputs_BoxPlot)
    })

    # Reactive expression to create a named color palette from the selected group levels
    color_palette_BoxPlot <- reactive({
      req(input$groupLevels_selected_BoxPlot)  # Ensure group levels are available

      # Define your Publication_color_code_BoxPlot palette
      Publication_color_code_BoxPlot <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")

      # Assign colors based on the group levels, cycling through Publication_color_code_BoxPlot
      sapply(seq_along(input$groupLevels_selected_BoxPlot), function(i) {
        level <- input$groupLevels_selected_BoxPlot[i]

        # If input is not available yet, fallback to Publication_color_code_BoxPlot
        input_color_BoxPlot <- input[[paste0("color_BoxPlot_", level)]]

        if (is.null(input_color_BoxPlot)) {
          # Assign color from Publication_color_code_BoxPlot based on the index
          default_color <- Publication_color_code_BoxPlot[(i - 1) %% length(Publication_color_code_BoxPlot) + 1]
          return(default_color)
        }

        return(input_color_BoxPlot)
      }, simplify = FALSE)
    })

    # UI for allowing to edit legend labels
    output$dynamicLegendInputs_BoxPlot <- renderUI({
      req(input$groupLevels_selected_BoxPlot)  # Ensure group levels are available

      # Create text input fields for each group level to edit legend labels
      legend_inputs_BoxPlot <- lapply(input$groupLevels_selected_BoxPlot, function(level) {
        textInput(
          inputId = paste0("legend_BoxPlot_", level),
          label = paste("Legend label for", level, "Group:"),
          value = level  # Set default value to the current level name
        )
      })

      # Return the list of legend input elements
      do.call(tagList, legend_inputs_BoxPlot)
    })

    # Reactive expression to create a named vector of legend labels
    legend_labels_BoxPlot <- reactive({
      req(input$groupLevels_selected_BoxPlot)  # Ensure group levels are available

      # Collect legend labels dynamically from inputs
      labels_BoxPlot <- sapply(input$groupLevels_selected_BoxPlot, function(level) {
        input[[paste0("legend_BoxPlot_", level)]]  # Access each text input value dynamically
      }, simplify = TRUE)

      # Return a named vector where the names are the group levels
      labels_BoxPlot
    })

    # Render the plot
    output$Render_BoxPlot <- renderPlot({
      req(values$metadata_BoxPlot, values$expression_BoxPlot, input$groupLevels_selected_BoxPlot) # Ensure data and group level input are available

      # Get pre-defined level of features first
      rownames(values$expression_BoxPlot) -> Features_level_BoxPlot

      # Combine datasets
      values$expression_BoxPlot %>%
        t() %>% as.data.frame() %>%
        rownames_to_column(var = "Sample") -> Exp_df_BoxPlot

      data_normalized_STAT_DEMs = values$metadata_BoxPlot %>%
        inner_join(Exp_df_BoxPlot, by = "Sample") %>%
        mutate(Group = factor(Group, levels = input$groupLevels_selected_BoxPlot))  # Use selected group levels

      # Transform data
      data_gathered <- gather(data_normalized_STAT_DEMs, key = "Features", value = "Intensity", -Sample, -Group) %>%
        mutate(
          Intensity = as.numeric(Intensity),
          Features = factor(Features, levels = Features_level_BoxPlot)
        )

      # Generate dynamic color palette based on the number of groups
      color_code_BoxPlot <- color_palette_BoxPlot()  # Get dynamic color palette

      # # Get the legend labels from reactive expression
      legend_labels_vector_BoxPlot <- legend_labels_BoxPlot()

      # Define the theme dynamically
      plot_theme <- switch(
        input$plotTheme_BoxPlot,
        "theme_Publication" = theme_Publication(),
        "theme_bw" = theme_bw(),
        "theme_minimal" = theme_minimal(),
        "theme_linedraw" = theme_linedraw(),
        "theme_classic" = theme_classic(),
        "theme_gray" = theme_gray(),
      )

      # Prepare axis limits and breaks
      # x_limits <- if (!is.na(input$xMin_BoxPlot) && !is.na(input$xMax_BoxPlot) && input$xMin_BoxPlot < input$xMax_BoxPlot) c(input$xMin_BoxPlot, input$xMax_BoxPlot) else NULL
      # y_limits <- if (!is.na(input$yMin_BoxPlot) && !is.na(input$yMax_BoxPlot) && input$yMin_BoxPlot < input$yMax_BoxPlot) c(input$yMin_BoxPlot, input$yMax_BoxPlot) else NULL
      # x_breaks <- if (!is.na(input$xBreaks_BoxPlot) && !is.na(input$xMin_BoxPlot) && !is.na(input$xMax_BoxPlot) && input$xBreaks_BoxPlot > 0) seq(from = input$xMin_BoxPlot, to = input$xMax_BoxPlot, by = input$xBreaks_BoxPlot) else waiver()
      # y_breaks <- if (!is.na(input$yBreaks_BoxPlot) && !is.na(input$yMin_BoxPlot) && !is.na(input$yMax_BoxPlot) && input$yBreaks_BoxPlot > 0) seq(from = input$yMin_BoxPlot, to = input$yMax_BoxPlot, by = input$yBreaks_BoxPlot) else waiver()

      p_BoxPlot = data_gathered %>%
        ggplot(aes(x = Group, y = Intensity)) +
        geom_boxplot(
          aes(fill = Group, color = Group), #ERROR when only set color = Group: supplied color is neither numeric nor character
          outlier.shape = NA, alpha = .36, width = input$BoxWidth_BoxPlot, size = 0.5
        ) +
        geom_point(
          aes(color = Group),
          position = position_jitter(width = input$JitterWidth_BoxPlot, seed = 123), size = input$pointSize_BoxPlot#, shape = 21,# alpha = 0.32, stroke = NA
        ) +
        facet_wrap(~Features, scales = "free", ncol = input$FacetnCol_BoxPlot) +
        labs(y = input$yLabel_BoxPlot, x = "") +
        scale_color_manual(values = color_code_BoxPlot) +
        scale_fill_manual(values = color_code_BoxPlot) +
        plot_theme +
        theme(
          legend.position = "none",
          axis.title.x = element_text(size = input$labelSize_BoxPlot),
          axis.title.y = element_text(size = input$labelSize_BoxPlot),
          axis.text.x = element_text(size = input$tickLabelSize_BoxPlot),
          axis.text.y = element_text(size = input$tickLabelSize_BoxPlot),
          strip.text = element_text(size = input$stripLabelSize_BoxPlot)
        ) +
        scale_x_discrete(labels = legend_labels_vector_BoxPlot)
      # scale_y_continuous(limits = y_limits, breaks = y_breaks)

      # Conditionally add the bold for axis and tick
      if (input$checkbox_Axis_bold_BoxPlot) {
        p_BoxPlot <- p_BoxPlot + theme(axis.title = element_text(face = "bold"))
      }

      if (input$checkbox_Tick_bold_BoxPlot) {
        p_BoxPlot <- p_BoxPlot + theme(axis.text = element_text(face = "bold"))
      }

      # Assign final plot for exporting
      p_BoxPlot <<- p_BoxPlot

      # Show plot
      p_BoxPlot

    }, width = reactive({ input$plotWidth_BoxPlot }), height = reactive({ input$plotHeight_BoxPlot }), res  = 72)

    # <-- Download handlers -->
    ## Box plot
    output$download_BoxPlot <- downloadHandler(
      filename = function() {
        paste(gsub("-", "_", Sys.Date()), "_Box_Plots", input$formatdownload_BoxPlot, sep = "")
      },
      content = function(file) {
        ggsave(
          file, plot = p_BoxPlot,
          # Adjust height and width accordingly
          width = input$plotWidth_BoxPlot * input$dpi_BoxPlot / 72,
          height = input$plotHeight_BoxPlot * input$dpi_BoxPlot / 72,
          dpi = input$dpi_BoxPlot,
          units = "px"
        )
      }
    )


    #<-- HeatmapSimple plot Handling -->
    library(ComplexHeatmap)

    # Observe metadata_HeatmapSimple file upload
    observeEvent(input$metadataFile_HeatmapSimple, {
      req(input$metadataFile_HeatmapSimple)
      values$metadata_HeatmapSimple <- read.csv(input$metadataFile_HeatmapSimple$datapath)

      # Data integrity (i.e., two columns)
      if (ncol(values$metadata_HeatmapSimple) == 2) {

        values$metadata_HeatmapSimple <- values$metadata_HeatmapSimple %>%
          setNames(c("Sample", "GroupLevel1")) %>%  # Standardize the variable name
          mutate(across(c("GroupLevel1"), ~ gsub(" ", "_", .))) # Treat metadata if containing space
      }

    })

    # Observe NormData_HeatmapSimple file upload
    observeEvent(input$NormDataFile_HeatmapSimple, {

      # SHow pop-up messages
      # toastr_info(message = "Ensure: (1) 2 columns in metadata; (2) identical sample names between files; and (3) not duplicated features.", timeOut = 15000,
      #             position = "top-right", closeButton = TRUE)#, progressBar = TRUE)
      toastr_info(title = "NOTE", message = "If the plot doesn’t appear or you see an error message, please double-check your input data and try again. If the issue persists, don't hesitate to contact us—we're here to help!", timeOut = 15000,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

      # Load input data
      req(input$NormDataFile_HeatmapSimple)

      ## Show messages if failed
      tryCatch({

        values$NormData_HeatmapSimple <- read.csv(input$NormDataFile_HeatmapSimple$datapath, row.names = 1, check.names = FALSE) %>%
          rownames_to_column(var = "Features")

        # Check matched sample names or not
        if (!(identical(sort(names(values$NormData_HeatmapSimple)[-1]), sort(values$metadata_HeatmapSimple$Sample)))) { #[-1] because we used rownames_to_column(var = "Features") above
          toastr_error("Sample names did not match between 2 files.", position = "top-right", timeOut = 15000)
        }

      }, error = function(e) {
        # If an error occurs, show a failure message
        toastr_error(title = "ERROR", "Data containe duplicated features.", position = "top-right", timeOut = 15000)

      })


      # Determine group levels dynamically after both files are uploaded
      if (!is.null(values$metadata_HeatmapSimple)) {
        group_levels_HeatmapSimple <- unique(values$metadata_HeatmapSimple$GroupLevel1)

        # Update UI for selecting and ordering group levels
        output$groupLevelSelector_HeatmapSimple <- renderUI({
          selectInput("groupLevels_selected_HeatmapSimple", "Group Levels (If used, click on the blank space -> Press the backspace key to delete -> Select the appropriate order)",
                      choices = group_levels_HeatmapSimple,
                      selected = group_levels_HeatmapSimple,
                      multiple = TRUE)
        })
      }

    })

    # UI for dynamic color inputs based on group levels
    output$dynamicColorInputs_HeatmapSimple <- renderUI({
      req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available

      # Create a list of color inputs for each group level
      Publication_color_code_HeatmapSimple = c("#323232", "#EB9F49", "#7fc97f", "#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
      color_inputs_HeatmapSimple <- lapply(seq_along(input$groupLevels_selected_HeatmapSimple), function(i) {
        level <- input$groupLevels_selected_HeatmapSimple[i]
        default_color <- Publication_color_code_HeatmapSimple[(i - 1) %% length(Publication_color_code_HeatmapSimple) + 1]  # Cycle through Publication_color_code_HeatmapSimple
        colourpicker::colourInput(inputId = paste0("color_HeatmapSimple_", level), label = paste("Color for", level, "Group:"), value = default_color)
      })

      # Return the list of color input elements
      do.call(tagList, color_inputs_HeatmapSimple)
    })

    # Reactive expression to create a named color palette from the selected group levels
    color_palette_HeatmapSimple <- reactive({
      req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available

      # Define your Publication_color_code_HeatmapSimple palette
      Publication_color_code_HeatmapSimple <- c("#323232", "#EB9F49", "#7fc97f", "#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")

      # Assign colors based on the group levels, cycling through Publication_color_code_HeatmapSimple
      sapply(seq_along(input$groupLevels_selected_HeatmapSimple), function(i) {
        level <- input$groupLevels_selected_HeatmapSimple[i]

        # If input is not available yet, fallback to Publication_color_code_HeatmapSimple
        input_color_HeatmapSimple <- input[[paste0("color_HeatmapSimple_", level)]]

        if (is.null(input_color_HeatmapSimple)) {
          # Assign color from Publication_color_code_HeatmapSimple based on the index
          default_color <- Publication_color_code_HeatmapSimple[(i - 1) %% length(Publication_color_code_HeatmapSimple) + 1]
          return(default_color)
        }

        return(input_color_HeatmapSimple)
      }, simplify = FALSE)
    })


    # UI for allowing to edit legend labels
    output$dynamicLegendInputs_HeatmapSimple <- renderUI({
      req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available

      # Create text input fields for each group level to edit legend labels
      legend_inputs_HeatmapSimple <- lapply(input$groupLevels_selected_HeatmapSimple, function(level) {
        textInput(
          inputId = paste0("legend_HeatmapSimple_", level),
          label = paste("Legend label for", level, "Group:"),
          value = level  # Set default value to the current level name
        )
      })

      # Return the list of legend input elements
      do.call(tagList, legend_inputs_HeatmapSimple)
    })

    # Reactive expression to create a named vector of legend labels
    legend_labels_HeatmapSimple <- reactive({
      req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available

      # Collect legend labels dynamically from inputs
      labels_HeatmapSimple <- sapply(input$groupLevels_selected_HeatmapSimple, function(level) {
        input[[paste0("legend_HeatmapSimple_", level)]]  # Access each text input value dynamically
      }, simplify = TRUE)

      # Return a named vector where the names are the group levels
      labels_HeatmapSimple
    })

    # Render the plot
    output$Render_HeatmapSimple <- renderPlot({
      req(
        # Ensure data and group level input are available
        values$metadata_HeatmapSimple, values$NormData_HeatmapSimple,
        # Use to sort Group
        input$groupLevels_selected_HeatmapSimple
      )

      Heatmap_df = values$NormData_HeatmapSimple
      metadata_df_Heatmap = values$metadata_HeatmapSimple

      # metadata_Group_Heatmap = unique(metadata_df_Heatmap$GroupLevel1)

      ########## Global variables ##########
      # Mapping color code automatically => like this (manually): mapping_col_vect = c(Group_1 = "#323232",Group_2 = "#1B6393")
      ## Define the color vector
      group_col_vect <- unlist(color_palette_HeatmapSimple()) #c("#323232", "#1B6393")
      ## Define metadata group heatmap
      metadata_Group_Heatmap <- input$groupLevels_selected_HeatmapSimple #c("Group_1", "Group_2")
      ## Use setNames to assign the group names as names to the color vector
      mapping_col_vect <- setNames(group_col_vect, metadata_Group_Heatmap)

      # Heatmap color
      heatmap_col_vect = c(input$color_DownHeatmap_HeatmapSimple, input$color_UnchangedHeatmap_HeatmapSimple, input$color_UpHeatmap_HeatmapSimple)
      heatmap_col_scale = c(-input$color_scaleHeatmap_HeatmapSimple, 0, input$color_scaleHeatmap_HeatmapSimple)

      labels_group_vect = legend_labels_HeatmapSimple()
      fontsize_row_name = input$size_Features_HeatmapSimple

      ############# IMPORTANT: Print to check debugging information before the next step #############
      # Require for mapping_col_vect, e.g., output of c(Group_1 = "#323232",Group_2 = "#1B6393")
      # print("Group levels:")
      # print(input$groupLevels_selected_HeatmapSimple)
      # print("Group color vector:")
      # print(group_col_vect)
      # print("Mapping color vector:")
      # print(mapping_col_vect)
      # print(legend_labels_HeatmapSimple())

      ############# Heatmap Annotation ##############

      # Row annotation
      if (input$checkbox_italicFeatures_HeatmapSimple == TRUE) {
        ha_row_txt <- rowAnnotation(
          labels = anno_text(
            Heatmap_df$Features,
            which = "row",
            gp = gpar(fontsize = fontsize_row_name, fontface = "italic")
          )
        )
      } else {
        ha_row_txt <- rowAnnotation(
          labels = anno_text(
            Heatmap_df$Features,
            which = "row",
            gp = gpar(fontsize = fontsize_row_name)
          )
        )
      }

      # Col annotation: Original code: ha_col = HeatmapAnnotation("Group" = anno_simple(metadata_df_Heatmap$GroupLevel1, col = mapping_col_vect))
      # do.call(HeatmapAnnotation, setNames(list(
      #   anno_simple(metadata_df_Heatmap$GroupLevel1, col = mapping_col_vect)
      # ), input$TopAnnotation_legend_HeatmapSimple))
      ha_col = HeatmapAnnotation(" " = anno_simple(metadata_df_Heatmap$GroupLevel1, col = mapping_col_vect))

      ## Col splitting
      # split1 <- metadata_df_Heatmap$GroupLevel1
      ## Row splitting
      # split2 = Heatmap_df$Pathway

      ############# Heatmap ##############

      # Matrix data scaling
      dat_pathway <- Heatmap_df %>% dplyr::select(all_of(metadata_df_Heatmap$Sample)) %>% mutate_if(is.character, as.numeric)
      dat_pathway <- t(scale(t(dat_pathway)))
      dat_pathway <- as.matrix(dat_pathway)

      # Visualization
      Hist1  <- Heatmap(
        dat_pathway,
        cluster_columns = input$checkbox_Col_clustering_HeatmapSimple,
        cluster_rows = input$checkbox_Row_clustering_HeatmapSimple,
        #cluster_row_slices = FALSE,
        name = input$HeatmapAnnotation_legend_HeatmapSimple,
        circlize::colorRamp2(heatmap_col_scale, heatmap_col_vect),
        top_annotation = ha_col,
        # left_annotation = ha_row,
        right_annotation = ha_row_txt,
        show_row_names = FALSE,
        show_column_names = input$checkbox_showColname_HeatmapSimple,
        # column_title_rot = 45,
        heatmap_legend_param = list(legend_direction = "horizontal"),
        column_split = factor(metadata_df_Heatmap$GroupLevel1, levels = input$groupLevels_selected_HeatmapSimple),
        # column_title_gp = gpar(fontsize = input$size_TopAnnotation_HeatmapSimple)
        column_title = NULL  # dont show top annotation
        #row_split = split2,
        #row_title=NULL
      )

      ############# Legend ##############
      lgd_group = Legend(title = input$TopAnnotation_legend_HeatmapSimple, legend_gp = gpar(fill = group_col_vect), labels = labels_group_vect)

      ############# Exporting ##############
      pd = packLegend(lgd_group,#lgd_compare, lgd_sig, lgd_pvalue, #lgd_pathway,
                      max_width = unit(12, "cm"), direction = "horizontal", column_gap = unit(5, "mm"), row_gap = unit(2, "mm"))

      # Show the plot with legend
      draw(Hist1, heatmap_legend_list = pd, #ht_gap =,
           heatmap_legend_side = "bottom", annotation_legend_side = "bottom", adjust_annotation_extension = TRUE)

      # # Show pop-up message <- cannot show here
      # toastr_info(title = "TIPS", message = "Customize the figure sizes (in next subtab) helps us to achieve a suitable figure scale.", timeOut = 4500,
      #             position = "top-right", closeButton = TRUE)#, progressBar = TRUE)

    }, width = reactive({ input$plotWidth_HeatmapSimple }), height = reactive({ input$plotHeight_HeatmapSimple }), res  = 72)

    # <-- Download handlers -->
    ## Heatmap simple
    output$download_HeatmapSimple <- downloadHandler(
      filename = function() {
        paste(gsub("-", "_", Sys.Date()), "_Heatmap", ".png", sep = "")
      },
      content = function(file) {
        png(
          file,
          # Adjust height and width accordingly
          width = input$plotWidth_HeatmapSimple * input$dpi_HeatmapSimple / 72,
          height = input$plotHeight_HeatmapSimple * input$dpi_HeatmapSimple / 72,
          res = input$dpi_HeatmapSimple, units = "px"
        )

        ######### IMPORTANT: Copy all to here => Remember when modify in the render -> Copy here #########
        req(
          # Ensure data and group level input are available
          values$metadata_HeatmapSimple, values$NormData_HeatmapSimple,
          # Use to sort Group
          input$groupLevels_selected_HeatmapSimple
        )

        Heatmap_df = values$NormData_HeatmapSimple
        metadata_df_Heatmap = values$metadata_HeatmapSimple

        # metadata_Group_Heatmap = unique(metadata_df_Heatmap$GroupLevel1)

        ########## Global variables ##########
        # Mapping color code automatically => like this (manually): mapping_col_vect = c(Group_1 = "#323232",Group_2 = "#1B6393")
        ## Define the color vector
        group_col_vect <- unlist(color_palette_HeatmapSimple()) #c("#323232", "#1B6393")
        ## Define metadata group heatmap
        metadata_Group_Heatmap <- input$groupLevels_selected_HeatmapSimple #c("Group_1", "Group_2")
        ## Use setNames to assign the group names as names to the color vector
        mapping_col_vect <- setNames(group_col_vect, metadata_Group_Heatmap)

        # Heatmap color
        heatmap_col_vect = c(input$color_DownHeatmap_HeatmapSimple, input$color_UnchangedHeatmap_HeatmapSimple, input$color_UpHeatmap_HeatmapSimple)
        heatmap_col_scale = c(-input$color_scaleHeatmap_HeatmapSimple, 0, input$color_scaleHeatmap_HeatmapSimple)

        labels_group_vect = legend_labels_HeatmapSimple()
        fontsize_row_name = input$size_Features_HeatmapSimple

        ############# IMPORTANT: Print to check debugging information before the next step #############
        # Require for mapping_col_vect, e.g., output of c(Group_1 = "#323232",Group_2 = "#1B6393")
        # print("Group levels:")
        # print(input$groupLevels_selected_HeatmapSimple)
        # print("Group color vector:")
        # print(group_col_vect)
        # print("Mapping color vector:")
        # print(mapping_col_vect)
        # print(legend_labels_HeatmapSimple())

        ############# Heatmap Annotation ##############

        # Row annotation
        if (input$checkbox_italicFeatures_HeatmapSimple == TRUE) {
          ha_row_txt <- rowAnnotation(
            labels = anno_text(
              Heatmap_df$Features,
              which = "row",
              gp = gpar(fontsize = fontsize_row_name, fontface = "italic")
            )
          )
        } else {
          ha_row_txt <- rowAnnotation(
            labels = anno_text(
              Heatmap_df$Features,
              which = "row",
              gp = gpar(fontsize = fontsize_row_name)
            )
          )
        }

        # Col annotation: Original code: ha_col = HeatmapAnnotation("Group" = anno_simple(metadata_df_Heatmap$GroupLevel1, col = mapping_col_vect))
        # do.call(HeatmapAnnotation, setNames(list(
        #   anno_simple(metadata_df_Heatmap$GroupLevel1, col = mapping_col_vect)
        # ), input$TopAnnotation_legend_HeatmapSimple))
        ha_col = HeatmapAnnotation(" " = anno_simple(metadata_df_Heatmap$GroupLevel1, col = mapping_col_vect))

        ## Col splitting
        # split1 <- metadata_df_Heatmap$GroupLevel1
        ## Row splitting
        # split2 = Heatmap_df$Pathway

        ############# Heatmap ##############

        # Matrix data scaling
        dat_pathway <- Heatmap_df %>% dplyr::select(all_of(metadata_df_Heatmap$Sample)) %>% mutate_if(is.character, as.numeric)
        dat_pathway <- t(scale(t(dat_pathway)))
        dat_pathway <- as.matrix(dat_pathway)

        # Visualization
        Hist1  <- Heatmap(
          dat_pathway,
          cluster_columns = input$checkbox_Col_clustering_HeatmapSimple,
          cluster_rows = input$checkbox_Row_clustering_HeatmapSimple,
          #cluster_row_slices = FALSE,
          name = input$HeatmapAnnotation_legend_HeatmapSimple,
          circlize::colorRamp2(heatmap_col_scale, heatmap_col_vect),
          top_annotation = ha_col,
          # left_annotation = ha_row,
          right_annotation = ha_row_txt,
          show_row_names = FALSE,
          show_column_names = input$checkbox_showColname_HeatmapSimple,
          # column_title_rot = 45,
          heatmap_legend_param = list(legend_direction = "horizontal"),
          column_split = factor(metadata_df_Heatmap$GroupLevel1, levels = input$groupLevels_selected_HeatmapSimple),
          # column_title_gp = gpar(fontsize = input$size_TopAnnotation_HeatmapSimple)
          column_title = NULL  # dont show top annotation
          #row_split = split2,
          #row_title=NULL
        )

        ############# Legend ##############
        lgd_group = Legend(title = input$TopAnnotation_legend_HeatmapSimple, legend_gp = gpar(fill = group_col_vect), labels = labels_group_vect)

        ############# Exporting ##############
        pd = packLegend(lgd_group,#lgd_compare, lgd_sig, lgd_pvalue, #lgd_pathway,
                        max_width = unit(12, "cm"), direction = "horizontal", column_gap = unit(5, "mm"), row_gap = unit(2, "mm"))

        # Show the plot with legend
        draw(Hist1, heatmap_legend_list = pd, #ht_gap =,
             heatmap_legend_side = "bottom", annotation_legend_side = "bottom", adjust_annotation_extension = TRUE)


        dev.off()
      }
    )

    #<-- DotPlot plot Handling -->
    # Observe PathwayData_DotPlot file upload
    observeEvent(input$PathwayDataFile_DotPlot, {
      req(input$PathwayDataFile_DotPlot)
      values$PathwayData_DotPlot <- read.csv(input$PathwayDataFile_DotPlot$datapath, check.names = FALSE)

      # SHow pop-up messages
      # toastr_info(message = "Ensure: (1) accurate number of columns.", timeOut = 12000,
      #             position = "top-right", closeButton = TRUE)
      toastr_info(title = "NOTE", message = "If the plot doesn’t appear or you see an error message, please double-check your input data and try again. If the issue persists, don't hesitate to contact us—we're here to help!", timeOut = 12000,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)
    })

    # Define the Omics data and update the label of x-axis automatically based on type of Omics data
    observeEvent(input$PathwayFromOmics_DotPlot, {
      Input_data_DotPlot <- switch(
        input$PathwayFromOmics_DotPlot,
        "Transcriptomics" = "Transcriptomics",
        "Metabolomics" = "Metabolomics"
      )

      # Update x-axis label based on Input_data_DotPlot
      updateTextInput(
        session,
        "xLabel_DotPlot",
        value = if (Input_data_DotPlot == "Transcriptomics") "Gene Ratio" else "Pathway Impact"
      )

      # Update color label based on Input_data_DotPlot
      updateTextInput(
        session,
        "ColorTitle_DotPlot",
        value = if (Input_data_DotPlot == "Transcriptomics") "Adjusted P-value" else "P-value"
      )

      # Choose AdjPvalue by default if select Transcriptomics
      updateTextInput(
        session,
        "checkbox_adjPvalue_DotPlot",
        value = if (Input_data_DotPlot == "Transcriptomics") TRUE else FALSE
      )

    })

    # Define the AdjPval or Pval and update the color label automatically
    observeEvent(input$checkbox_adjPvalue_DotPlot, {

      # Update color label
      updateTextInput(
        session,
        "ColorTitle_DotPlot",
        value = if (input$checkbox_adjPvalue_DotPlot) "Adjusted P-value" else "P-value"
      )

    })

    # Render the plot
    output$Render_DotPlot <- renderPlot({
      req(values$PathwayData_DotPlot) # Ensure data and group level input are available

      pathway_input_data = values$PathwayData_DotPlot

      # Define the theme dynamically
      plot_theme <- switch(
        input$plotTheme_DotPlot,
        "theme_Publication" = theme_Publication_modifed_DotPlot(),
        "theme_bw" = theme_bw(),
        "theme_minimal" = theme_minimal(),
        "theme_linedraw" = theme_linedraw(),
        "theme_classic" = theme_classic(),
        "theme_gray" = theme_gray(),
      )

      # Prepare axis limits and breaks
      x_limits <- if (!is.na(input$xMin_DotPlot) && !is.na(input$xMax_DotPlot) && input$xMin_DotPlot < input$xMax_DotPlot) c(input$xMin_DotPlot, input$xMax_DotPlot) else NULL
      # y_limits <- if (!is.na(input$yMin_DotPlot) && !is.na(input$yMax_DotPlot) && input$yMin_DotPlot < input$yMax_DotPlot) c(input$yMin_DotPlot, input$yMax_DotPlot) else NULL
      x_breaks <- if (!is.na(input$xBreaks_DotPlot) && !is.na(input$xMin_DotPlot) && !is.na(input$xMax_DotPlot) && input$xBreaks_DotPlot > 0) seq(from = input$xMin_DotPlot, to = input$xMax_DotPlot, by = input$xBreaks_DotPlot) else waiver()
      # y_breaks <- if (!is.na(input$yBreaks_DotPlot) && !is.na(input$yMin_DotPlot) && !is.na(input$yMax_DotPlot) && input$yBreaks_DotPlot > 0) seq(from = input$yMin_DotPlot, to = input$yMax_DotPlot, by = input$yBreaks_DotPlot) else waiver()

      # Define the Pathway Analysis Mode
      Pathway_analysis_mode <- switch(
        input$PathwayAnalysisMode_DotPlot,
        "ORA" = "ORA",
        "GSEA" = "GSEA"
      )

      # Define the Omics data => NOTE: also define outside (above) to update the x-axis label autonmatically
      Input_data_DotPlot <- switch(
        input$PathwayFromOmics_DotPlot,
        "Transcriptomics" = "Transcriptomics",
        "Metabolomics" = "Metabolomics"
      )

      # Ploting
      if (Pathway_analysis_mode == "ORA") {

        ######### For ORA #########
        if (Input_data_DotPlot == "Transcriptomics") {

          # <-- Transcriptomics -->

          # Standardize variables name
          names(pathway_input_data) = c("Description", "Hits_count", "Total_input_gene", "GeneRatio", "Pvalue", "P.adjust", "FeaturesID")

          # Check if hit count available or not. If not -> calculate based on list of FeaturesID
          if (all(is.na(pathway_input_data$Hits_count))) {
            pathway_input_data %>%
              mutate(Hits_count = sapply(strsplit(FeaturesID, "/|,|;"), length)) -> pathway_input_data
          }

          # Add other needed variables
          pathway_input_data %>%
            mutate(
              # Calculate gene ratio
              GeneRatio = Hits_count/Total_input_gene
            ) -> pathway_input_data

          # Get the levels of Pathways based on the GeneRatio
          pathway_input_data %>% arrange(GeneRatio) %>% dplyr::select(Description) %>% pull() -> Description_level

          # Visualize
          if (input$checkbox_adjPvalue_DotPlot) {
            # Use adjPvalue for visualization
            p_DotPlot = pathway_input_data %>%
              dplyr::select(-Pvalue) %>%  # QC to ensure that did not select P-value in the analysis
              mutate(Description = factor(Description, level = Description_level)) %>%
              ggplot(aes(x = `GeneRatio`, y = Description)) +
              geom_point(aes(size = Hits_count, color = P.adjust)) +
              geom_segment(aes(xend = 0, yend = Description))
          } else {
            # Use Pvalue for visualization
            p_DotPlot = pathway_input_data %>%
              dplyr::select(-P.adjust) %>%  # QC to ensure that did not select P.adjust in the analysis
              mutate(Description = factor(Description, level = Description_level)) %>%
              ggplot(aes(x = `GeneRatio`, y = Description)) +
              geom_point(aes(size = Hits_count, color = Pvalue)) +
              geom_segment(aes(xend = 0, yend = Description))
          }

        } else if (Input_data_DotPlot == "Metabolomics") {

          # <-- Metabolomics -->

          # Standardize the variable names
          names(pathway_input_data) = c("Description", "Hits_count", "Total_input_gene", "Pathway_impact", "Pvalue", "P.adjust", "FeaturesID")

          # Check if hit count available or not. If not -> calculate based on list of FeaturesID
          if (all(is.na(pathway_input_data$Hits_count))) {
            pathway_input_data %>%
              mutate(Hits_count = sapply(strsplit(FeaturesID, "/|,|;"), length)) -> pathway_input_data
          }

          # Get the levels of Pathways based on the Pathway_impact
          pathway_input_data %>% arrange(Pathway_impact) %>% dplyr::select(Description) %>% pull() -> Description_level

          # Visualize
          if (input$checkbox_adjPvalue_DotPlot) {
            # Use adjPvalue for visualization
            p_DotPlot = pathway_input_data %>%
              dplyr::select(-Pvalue) %>%  # QC to ensure that did not select P-value in the analysis
              mutate(Description = factor(Description, level = Description_level)) %>%
              ggplot(aes(x = `Pathway_impact`, y = Description)) +
              geom_point(aes(size = Hits_count, color = P.adjust)) +
              geom_segment(aes(xend = 0, yend = Description))
          } else {
            # Use Pvalue for visualization
            p_DotPlot = pathway_input_data %>%
              dplyr::select(-P.adjust) %>%  # QC to ensure that did not select P.adjust in the analysis
              mutate(Description = factor(Description, level = Description_level)) %>%
              ggplot(aes(x = `Pathway_impact`, y = Description)) +
              geom_point(aes(size = Hits_count, color = Pvalue)) +
              geom_segment(aes(xend = 0, yend = Description))
          }

        }

      } else if (
        (Pathway_analysis_mode == "GSEA") & (Input_data_DotPlot == "Transcriptomics")
      ) {

        ######### For GSEA #########

        # Standardize the variable names
        names(pathway_input_data) = c("Description", "Set_size", "Hits_count", "Enrichment_score", "Normalized_enrichment_score", "Pvalue", "P.adjust", "FeaturesID")

        # Check if hit count available or not. If not -> calculate based on list of FeaturesID
        if (all(is.na(pathway_input_data$Hits_count))) {
          pathway_input_data %>%
            mutate(Hits_count = sapply(strsplit(FeaturesID, "/|,|;"), length)) -> pathway_input_data
        }

        # Add other needed variables
        if (!all(is.na(pathway_input_data$Normalized_enrichment_score))) {
          #### Results from clusterProfiler  ####
          pathway_input_data %>%
            mutate(
              # Calculate gene ratio
              GeneRatio = Hits_count/Set_size,
              # # Assign to Activated or Suppressed group
              Sign = ifelse(Normalized_enrichment_score < 0, "Suppressed", "Activated")
            ) -> pathway_input_data
        } else {
          # #### Results from ExpressAnalyst  #### CHECK AGAIN, NOT SURE
          # pathway_input_data %>%
          #   mutate(
          #     # Calculate gene ratio
          #     GeneRatio = Hits_count/Set_size,
          #     # # Assign to Activated or Suppressed group
          #     Sign = ifelse(Enrichment_score < 0, "Suppressed", "Activated")
          #   ) -> pathway_input_data
        }

        # Get the levels of Pathways based on the GeneRatio
        pathway_input_data %>% arrange(GeneRatio) %>% dplyr::select(Description) %>% pull() -> Description_level

        # Option to use adjPval or not
        # Visualize
        if (input$checkbox_adjPvalue_DotPlot) {
          # Use adjPvalue for visualization
          p_DotPlot = pathway_input_data %>%
            dplyr::select(-Pvalue) %>%  # QC to ensure that did not select P-value in the analysis
            mutate(Description = factor(Description, level = Description_level)) %>%
            ggplot(aes(x = `GeneRatio`, y = Description)) +
            geom_point(aes(size = Hits_count, color = P.adjust)) +
            geom_segment(aes(xend = 0, yend = Description)) +
            facet_wrap(vars(Sign))
        } else {
          # Use Pvalue for visualization
          p_DotPlot = pathway_input_data %>%
            dplyr::select(-P.adjust) %>%  # QC to ensure that did not select P.adjust in the analysis
            mutate(Description = factor(Description, level = Description_level)) %>%
            ggplot(aes(x = `GeneRatio`, y = Description)) +
            geom_point(aes(size = Hits_count, color = Pvalue)) +
            geom_segment(aes(xend = 0, yend = Description)) +
            facet_wrap(vars(Sign))
        }

      }

      # Plot customization
      p_DotPlot = p_DotPlot +
        scale_color_gradientn(
          colours = c(input$color_lowPvalue_DotPlot, input$color_interPvalue_DotPlot, input$color_highPvalue_DotPlot),
          guide = guide_colorbar(reverse = T, order = 1)
        ) +
        scale_size_continuous(range = c(input$small_size_scale_DotPlot, input$big_size_scale_DotPlot)) +
        labs(y = "", x = input$xLabel_DotPlot, color = input$ColorTitle_DotPlot, size = input$PointSizeTitle_DotPlot) +
        plot_theme +
        theme(
          axis.title.x = element_text(size = input$labelSize_DotPlot),
          axis.text.x = element_text(size = input$tickLabelSize_xAxis_DotPlot),
          axis.text.y = element_text(size = input$tickLabelSize_yAxis_DotPlot),
          legend.title = element_text(size = input$legendTitleSize_DotPlot),
          legend.text = element_text(size = input$legendTextSize_DotPlot),
          legend.key.size = unit(input$legendkeySize_DotPlot, 'cm')
        ) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks)

      # Conditionally add the bold for axis and tick
      if (input$checkbox_Axis_bold_DotPlot) {
        p_DotPlot <- p_DotPlot + theme(axis.title = element_text(face = "bold"))
      }

      if (input$checkbox_Tick_bold_DotPlot) {
        p_DotPlot <- p_DotPlot + theme(axis.text = element_text(face = "bold"))
      }

      # Assign final plot for exporting
      p_DotPlot <<- p_DotPlot

      # Render
      p_DotPlot

    }, width = reactive({ input$plotWidth_DotPlot }), height = reactive({ input$plotHeight_DotPlot }), res = 72)

    # <-- Download handlers -->
    ## Dot plot
    output$download_DotPlot <- downloadHandler(
      filename = function() {
        paste(gsub("-", "_", Sys.Date()), "_DotPlot", input$formatdownload_DotPlot, sep = "")
      },
      content = function(file) {
        ggsave(
          file, plot = p_DotPlot,
          # Adjust height and width accordingly
          width = input$plotWidth_DotPlot * input$dpi_DotPlot / 72,
          height = input$plotHeight_DotPlot * input$dpi_DotPlot / 72,
          dpi = input$dpi_DotPlot,
          units = "px"
        )
      }
    )

    #<-- BubblePlot plot Handling -->
    # Observe PathwayData_BubblePlot file upload
    observeEvent(input$PathwayDataFile_BubblePlot, {
      req(input$PathwayDataFile_BubblePlot)
      values$PathwayData_BubblePlot <- read.csv(input$PathwayDataFile_BubblePlot$datapath, check.names = FALSE)

      # SHow pop-up messages
      # toastr_info(message = "Ensure: (1) accurate number of columns.", timeOut = 12000,
      #             position = "top-right", closeButton = TRUE)
      toastr_info(title = "NOTE", message = "If the plot doesn’t appear or you see an error message, please double-check your input data and try again. If the issue persists, don't hesitate to contact us—we're here to help!", timeOut = 12000,
                  position = "top-right", closeButton = TRUE)#, progressBar = TRUE)
    })

    # Define the Omics data and update the label of x-axis automatically based on type of Omics data
    observeEvent(input$PathwayFromOmics_BubblePlot, {
      Input_data_BubblePlot <- switch(
        input$PathwayFromOmics_BubblePlot,
        "Transcriptomics" = "Transcriptomics",
        "Metabolomics" = "Metabolomics"
      )

      # Update x-axis label based on Input_data_BubblePlot
      updateTextInput(
        session,
        "xLabel_BubblePlot",
        value = if (Input_data_BubblePlot == "Transcriptomics") "Gene Ratio" else "Pathway Impact"
      )

      # Update PointSize label based on Input_data_BubblePlot
      updateTextInput(
        session,
        "PointSizeTitleBubblePlot",
        value = if (Input_data_BubblePlot == "Transcriptomics") "Gene Ratio" else "Pathway Impact"
      )

      # Update Y-axis label based on Input_data_BubblePlot
      updateTextInput(
        session,
        "yLabel_BubblePlot",
        value = if (Input_data_BubblePlot == "Transcriptomics") "-log10(Adjusted P-value)" else "-log10(P-value)"
      )

      # Update color label based on Input_data_BubblePlot
      updateTextInput(
        session,
        "ColorTitle_BubblePlot",
        value = if (Input_data_BubblePlot == "Transcriptomics") "Adjusted P-value" else "P-value"
      )

      # Choose AdjPvalue by default if select Transcriptomics
      updateTextInput(
        session,
        "checkbox_adjPvalue_BubblePlot",
        value = if (Input_data_BubblePlot == "Transcriptomics") TRUE else FALSE
      )

    })

    # Define the AdjPval or Pval and update the label automatically
    observeEvent(input$checkbox_adjPvalue_BubblePlot, {

      # yLabel
      updateTextInput(
        session,
        "yLabel_BubblePlot",
        value = if (input$checkbox_adjPvalue_BubblePlot) "-log10(Adjusted P-value)" else "-log10(P-value)"
      )

      # Update color label
      updateTextInput(
        session,
        "ColorTitle_BubblePlot",
        value = if (input$checkbox_adjPvalue_BubblePlot) "Adjusted P-value" else "P-value"
      )

    })

    # Render the plot
    output$Render_BubblePlot <- renderPlot({
      req(values$PathwayData_BubblePlot) # Ensure data and group level input are available

      pathway_input_data = values$PathwayData_BubblePlot

      # Define the theme dynamically
      plot_theme <- switch(
        input$plotTheme_BubblePlot,
        "theme_Publication" = theme_Publication_modifed_DotPlot(),
        "theme_bw" = theme_bw(),
        "theme_minimal" = theme_minimal(),
        "theme_linedraw" = theme_linedraw(),
        "theme_classic" = theme_classic(),
        "theme_gray" = theme_gray(),
      )

      # # Get max x-value and y-value
      # max(abs(values$pathway_input_data$log2FoldChange)) -> max_x_limits_Volcano

      # Prepare axis limits and breaks
      x_limits <- if (!is.na(input$xMin_BubblePlot) && !is.na(input$xMax_BubblePlot) && input$xMin_BubblePlot < input$xMax_BubblePlot) c(input$xMin_BubblePlot, input$xMax_BubblePlot) else NULL
      y_limits <- if (!is.na(input$yMin_BubblePlot) && !is.na(input$yMax_BubblePlot) && input$yMin_BubblePlot < input$yMax_BubblePlot) c(input$yMin_BubblePlot, input$yMax_BubblePlot) else NULL
      x_breaks <- if (!is.na(input$xBreaks_BubblePlot) && !is.na(input$xMin_BubblePlot) && !is.na(input$xMax_BubblePlot) && input$xBreaks_BubblePlot > 0) seq(from = input$xMin_BubblePlot, to = input$xMax_BubblePlot, by = input$xBreaks_BubblePlot) else waiver()
      y_breaks <- if (!is.na(input$yBreaks_BubblePlot) && !is.na(input$yMin_BubblePlot) && !is.na(input$yMax_BubblePlot) && input$yBreaks_BubblePlot > 0) seq(from = input$yMin_BubblePlot, to = input$yMax_BubblePlot, by = input$yBreaks_BubblePlot) else waiver()

      # Define the Omics data => NOTE: also define outside (above) to update the x-axis label autonmatically
      Input_data_BubblePlot <- switch(
        input$PathwayFromOmics_BubblePlot,
        "Transcriptomics" = "Transcriptomics",
        "Metabolomics" = "Metabolomics"
      )

      # Ploting
      ######### Bubble only For ORA #########
      if (Input_data_BubblePlot == "Transcriptomics") {

        # <-- Transcriptomics -->

        # Standardize variables name
        names(pathway_input_data) = c("Description", "Hits_count", "Total_input_gene", "GeneRatio", "Pvalue", "P.adjust", "FeaturesID")

        # Check if hit count available or not. If not -> calculate based on list of FeaturesID
        if (all(is.na(pathway_input_data$Hits_count))) {
          pathway_input_data %>%
            mutate(Hits_count = sapply(strsplit(FeaturesID, "/|,|;"), length)) -> pathway_input_data
        }

        # Add other needed variables
        pathway_input_data %>%
          mutate(
            # Calculate gene ratio
            GeneRatio = Hits_count/Total_input_gene
          ) -> pathway_input_data

        # Get the levels of Pathways based on the GeneRatio
        pathway_input_data %>% arrange(GeneRatio) %>% dplyr::select(Description) %>% pull() -> Description_level

        # Visualize
        if (input$checkbox_adjPvalue_BubblePlot) {
          # Use adjPvalue for visualization
          p_BubblePlot = pathway_input_data %>%
            dplyr::select(-Pvalue) %>%  # QC to ensure that did not select P-value in the analysis
            ggplot(aes(x = `GeneRatio`, y = -log10(P.adjust))) +
            geom_point(aes(size = GeneRatio, color = P.adjust))

        } else {
          # Use Pvalue for visualization
          p_BubblePlot = pathway_input_data %>%
            dplyr::select(-P.adjust) %>%  # QC to ensure that did not select P.adjust in the analysis
            ggplot(aes(x = `GeneRatio`, y = -log10(Pvalue))) +
            geom_point(aes(size = GeneRatio, color = Pvalue))
        }

      } else if (Input_data_BubblePlot == "Metabolomics") {

        # <-- Metabolomics -->

        # Standardize the variable names
        names(pathway_input_data) = c("Description", "Hits_count", "Total_input_gene", "Pathway_impact", "Pvalue", "P.adjust", "FeaturesID")
        # names(pathway_input_data) = c("Description", "Total_input_gene", "Expected", "Hits_count", "Pvalue", "minusLOG10p", "Holm adjust", "P.adjust", "Pathway_impact")

        # Check if hit count available or not. If not -> calculate based on list of FeaturesID
        if (all(is.na(pathway_input_data$Hits_count))) {
          pathway_input_data %>%
            mutate(Hits_count = sapply(strsplit(FeaturesID, "/|,|;"), length)) -> pathway_input_data
        }

        # Get the levels of Pathways based on the Pathway_impact
        pathway_input_data %>% arrange(Pathway_impact) %>% dplyr::select(Description) %>% pull() -> Description_level

        # Visualize
        if (input$checkbox_adjPvalue_BubblePlot) {
          # Use adjPvalue for visualization
          p_BubblePlot = pathway_input_data %>%
            dplyr::select(-Pvalue) %>%  # QC to ensure that did not select P-value in the analysis
            ggplot(aes(x = `Pathway_impact`, y = -log10(P.adjust))) +
            geom_point(aes(size = Pathway_impact, color = P.adjust))

        } else {
          # Use Pvalue for visualization
          p_BubblePlot = pathway_input_data %>%
            dplyr::select(-P.adjust) %>%  # QC to ensure that did not select P.adjust in the analysis
            ggplot(aes(x = `Pathway_impact`, y = -log10(Pvalue))) +
            geom_point(aes(size = Pathway_impact, color = Pvalue))
        }

      }

      # Plot customization
      p_BubblePlot = p_BubblePlot +
        scale_color_gradientn(
          colours = c(input$color_lowPvalue_BubblePlot, input$color_interPvalue_BubblePlot, input$color_highPvalue_BubblePlot),
          guide = guide_colorbar(reverse = T, order = 1)
        ) +
        scale_size_continuous(range = c(input$small_size_scale_BubblePlot, input$big_size_scale_BubblePlot)) +
        labs(y = input$yLabel_BubblePlot, x = input$xLabel_BubblePlot, color = input$ColorTitle_BubblePlot, size = input$PointSizeTitleBubblePlot) +
        plot_theme +
        theme(
          axis.title = element_text(size = input$labelSize_BubblePlot),
          axis.text = element_text(size = input$tickLabelSize_BubblePlot),
          legend.title = element_text(size = input$legendTitleSize_BubblePlot),
          legend.text = element_text(size = input$legendTextSize_BubblePlot),
          legend.key.size = unit(input$legendkeySize_BubblePlot, 'cm')
        ) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        scale_y_continuous(limits = y_limits, breaks = y_breaks)

      # Conditionally add the bold for axis and tick
      if (input$checkbox_Axis_bold_BubblePlot) {
        p_BubblePlot <- p_BubblePlot + theme(axis.title = element_text(face = "bold"))
      }

      if (input$checkbox_Tick_bold_BubblePlot) {
        p_BubblePlot <- p_BubblePlot + theme(axis.text = element_text(face = "bold"))
      }

      # Conditionally show legend or not
      if (!input$showLegend_BubblePlot) {
        p_BubblePlot <- p_BubblePlot + theme(legend.position = "none")
      }

      # Assign final plot for exporting
      p_BubblePlot <<- p_BubblePlot

      # Render
      p_BubblePlot

    }, width = reactive({ input$plotWidth_BubblePlot }), height = reactive({ input$plotHeight_BubblePlot }), res = 72)

    # <-- Download handlers -->
    ## Bubble plot
    output$download_BubblePlot <- downloadHandler(
      filename = function() {
        paste(gsub("-", "_", Sys.Date()), "_BubblePlot", input$formatdownload_BubblePlot, sep = "")
      },
      content = function(file) {
        ggsave(
          file, plot = p_BubblePlot,
          # Adjust height and width accordingly
          width = input$plotWidth_BubblePlot * input$dpi_BubblePlot / 72,
          height = input$plotHeight_BubblePlot * input$dpi_BubblePlot / 72,
          dpi = input$dpi_BubblePlot,
          units = "px"
        )
      }
    )
  }

  # Run the Application
  shinyApp(ui = ui, server = server)

}
