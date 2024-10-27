library(shiny)
library(shinyWidgets)
library(colourpicker)
library(tidyverse)
library(bslib)

# Define UI
ui <- navbarPage(
  title = "EasyPubPlot",
  id = "navbar",  # Set an ID here for reference
  theme = bs_theme(
    version = 4,
    bootswatch = "lux",
    primary = "#47B0C3",
    base_font = font_google("Roboto")
  ),
  
  # Introduction Tab
  tabPanel(
    title = "Introduction",
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
        img(src = "https://drive.google.com/thumbnail?id=1-Xzh6vEfQF-B30I4Dk2JFDmwhte6Ye_T", height = "500px")
        
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
      id = "reload_app_button",
      class = "action-button shiny-bound-input",
      "Reset App",
      style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
      onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        # Controls for the Volcano Plot
        tabsetPanel(
          id = "volcanoTabs",
          type = "pills",
          
          # File upload inputs for Volcano Plot
          tabPanel(
            "Data Upload",
            fileInput("volcanoFile", "Upload STAT Results File:", accept = c(".csv"))
          ),
          
          # Plot Dimension Tab
          tabPanel(
            "Plot Dimension",
            
            numericInput("plotWidth_Volcano", "Width (in pixels):", value = 600, step = 5),
            numericInput("plotHeight_Volcano", "Height (in pixels):", value = 600, step = 5)
          ),
          
          # Cut-offs Tab
          tabPanel(
            "Cut-offs",
            
            numericInput("FDR_cut_off_Volcano", "(adj) P-value Cut-off:", min = 0, max = 10, value = 0.05, step = 0.01),
            numericInput("FC_cut_off_Volcano", "Fold Change Cut-off:", min = 0, max = 500, value = 1.5, step = 0.5),
            
            textInput("caption_Volcano", "Caption:", value = "FC cut-off, 1.5; FDR cut-off, 0.05"),
            numericInput("captionLabSize_Volcano", "Caption Size:", value = 20)
            
          ),
          
          tabPanel(
            "Colors",
            
            colourInput("color_NotSig_Volcano", "Not Significant Color:", value = "#848484"),
            colourInput("color_DownFDR_Volcano", "(adj) P-value, Down-regulation Color:", value = "#BCF1B9"),
            colourInput("color_UpFDR_Volcano", "(adj) P-value, Up-regulation:", value = "#FEB0B0"),
            colourInput("color_DownFDR_FC_Volcano", "(adj) P-value & FC, Down-regulation):", value = "#5FDD59"),
            colourInput("color_UpFDR_FC_Volcano", "(adj) P-value & FC, Up-regulation):", value = "#FE5E5E")
            
          ),
          
          # Plot Appearance
          tabPanel(
            "Points & Legends",
            numericInput("pointSize_Volcano", "Point Size:", value = 1.6, step = 0.5),
            
            # legend
            checkboxInput("Show_legend_Volcano", "Show legend", value = FALSE),
            numericInput("legendLabSize_Volcano", "Legend Title Size:", value = 24),
            numericInput("legendIconSize_Volcano", "Legend Icon Size:", value = 6),
            
            selectInput(
              "plotTheme_Volcano", "Plot Theme:",
              choices = c("none"),#, "theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
              selected = "none"
            )
          ),
          
          # Axis Labels Tab
          tabPanel(
            "Axis Labels",
            textInput("xLabel_Volcano", "X-axis Label:", value = "log2(FC)"),
            textInput("yLabel_Volcano", "Y-axis Label:", value = "-log10(FDR)"),
            numericInput("labelSize_Volcano", "Axis Label Size:", value = 24),
            checkboxInput("checkbox_Axis_bold_Volcano", "Axis bold", value = TRUE),
            
            numericInput("tickLabelSize_Volcano", "Tick Label Size:", value = 20),
            checkboxInput("checkbox_Tick_bold_Volcano", "Tick bold", value = FALSE)
          ),
          
          # Axis Limits & Breaks Tab
          tabPanel(
            "Axis Limits",
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
          
          # Save Plot Tab
          tabPanel(
            "Save Plot",
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
      id = "reload_app_button",
      class = "action-button shiny-bound-input",
      "Reset App",
      style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
      onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        # Tabs for different settings
        tabsetPanel(
          id = "HeatmapSimpletabs",
          type = "pills",
          
          # Data upload
          tabPanel(
            "Data Upload",
            # File upload inputs
            fileInput("metadataFile_HeatmapSimple", "Upload Metadata File:", accept = c(".csv")),
            fileInput("NormDataFile_HeatmapSimple", "Upload Normalized Data File:", accept = c(".csv"))
          ),
          
          # Group Tab
          tabPanel(
            "Plot Appearance",
            # Group Color => Use dynamical
            uiOutput("groupLevelSelector_HeatmapSimple"),  # Cannot change group order in the ComplexHeatmap
            uiOutput("dynamicColorInputs_HeatmapSimple"),
            uiOutput("dynamicLegendInputs_HeatmapSimple")
          ),
          
          # Plot Dimension Tab
          tabPanel(
            "Plot Dimension",
            
            numericInput("plotWidth_HeatmapSimple", "Width (in pixels):", value = 300, step = 5),
            numericInput("plotHeight_HeatmapSimple", "Height (in pixels):", value = 600, step = 5),
          ),
          
          # Colors Tab
          tabPanel(
            "Heatmap Colors",
            # Heatmap Color and Scale
            colourInput("color_DownHeatmap_HeatmapSimple", "Down-regulation Color:", value = "#23446f"),
            colourInput("color_UnchangedHeatmap_HeatmapSimple", "Unchange Color:", value = "white"),
            colourInput("color_UpHeatmap_HeatmapSimple", "Up-regulation Color:", value = "#ad190d"),
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
            # Show col name
            checkboxInput("checkbox_showColname_HeatmapSimple", "Show Column Name", value = FALSE)
          ),
          
          # Save Plot Tab
          tabPanel(
            "Save Plot",
            
            numericInput("dpi_HeatmapSimple", "Resolution (DPI):", value = 300, step = 300),
            # selectInput(
            #   "formatdownload_HeatmapSimple", "Format:",
            #   choices = c(".png", ".svg", ".tiff", ".pdf"),
            #   selected = ".png"
            # ),
            downloadButton("download_HeatmapSimple", "Download Plot as .png")
          ),
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
      id = "reload_app_button",
      class = "action-button shiny-bound-input",
      "Reset App",
      style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
      onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        # Tabs for different settings
        tabsetPanel(
          id = "ScorePlottabs",
          type = "pills",
          
          # Data upload
          tabPanel(
            "Data Upload",
            # File upload inputs
            fileInput("metadataFile_ScorePlot", "Upload Metadata File:", accept = c(".csv")),
            fileInput("scoreFile_ScorePlot", "Upload Scores File:", accept = c(".csv"))
          ),
          
          # Plot Appearance Tab
          tabPanel(
            "Plot Appearance",
            uiOutput("groupLevelSelector_ScorePlot"),  # This will be dynamically generated based on the uploaded data
            uiOutput("dynamicColorInputs_ScorePlot"),
            
            uiOutput("dynamicLegendInputs_ScorePlot"),  # For dynamic legend labels
            
            checkboxInput("checkbox_95CI_ScorePlot", "Display 95% confidence ellipse", value = TRUE)
            
          ),
          
          # Plot Dimension Tab
          tabPanel(
            "Plot Dimension",
            
            numericInput("plotWidth_ScorePlot", "Width (in pixels):", value = 600, step = 5),
            numericInput("plotHeight_ScorePlot", "Height (in pixels):", value = 600, step = 5)
          ),
          
          # Points and Themes
          tabPanel(
            "Points & Themes",
            
            numericInput("pointSize_ScorePlot", "Point Size:", value = 4),
            
            selectInput(
              "plotTheme_ScorePlot", "Plot Theme:",
              choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
              selected = "theme_Publication"
            )
          ),
          
          # Axis Labels Tab
          tabPanel(
            "Axis Labels",
            textInput("xLabel_ScorePlot", "X-axis Label:", value = "The first component (A %)"),
            textInput("yLabel_ScorePlot", "Y-axis Label:", value = "The second component (B %)"),
            numericInput("labelSize_ScorePlot", "Axis Label Size:", value = 15),
            checkboxInput("checkbox_Axis_bold_ScorePlot", "Axis bold", value = TRUE),
            
            numericInput("tickLabelSize_ScorePlot", "Tick Label Size:", value = 15),
            checkboxInput("checkbox_Tick_bold_ScorePlot", "Tick bold", value = FALSE),
          ),
          
          # Axis Limits & Breaks Tab
          tabPanel(
            "Axis Limits",
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
          
          # Save Plot Tab
          tabPanel(
            "Save Plot",
            
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
      id = "reload_app_button",
      class = "action-button shiny-bound-input",
      "Reset App",
      style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
      onclick = "Shiny.setInputValue('reload_app_button', Math.random())"
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        # Tabs for different settings
        tabsetPanel(
          id = "BoxPlottabs",
          type = "pills",
          
          # Data upload
          tabPanel(
            "Data Upload",
            # File upload inputs
            fileInput("metadataFile_BoxPlot", "Upload Metadata File:", accept = c(".csv")),
            fileInput("expressionFile_BoxPlot", "Upload Normalized Data File:", accept = c(".csv"))
          ),
          
          # Plot Appearance Tab
          tabPanel(
            "Plot Appearance",
            uiOutput("groupLevelSelector_BoxPlot"),  # This will be dynamically generated based on the uploaded data
            uiOutput("dynamicColorInputs_BoxPlot"),
            
            uiOutput("dynamicLegendInputs_BoxPlot"), # For dynamic legend labels
            
            selectInput(
              "plotTheme_BoxPlot", "Plot Theme:",
              choices = c("theme_Publication", "theme_classic", "theme_bw", "theme_minimal", "theme_linedraw", "theme_gray"),
              selected = "theme_Publication"
            )
          ),
          
          # Plot Dimension Tab
          tabPanel(
            "Plot Dimension",
            
            numericInput("plotWidth_BoxPlot", "Width (in pixels):", value = 800, step = 5),
            numericInput("plotHeight_BoxPlot", "Height (in pixels):", value = 400, step = 5)
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
          
          # Save Plot Tab
          tabPanel(
            "Save Plot",
            
            numericInput("dpi_BoxPlot", "Resolution (DPI):", value = 300, step = 300),
            selectInput(
              "formatdownload_BoxPlot", "Format:",
              choices = c(".png", ".svg", ".tiff", ".pdf"),
              selected = ".png"
            ),
            downloadButton("download_BoxPlot", "Download Plot")
          ),
        )
      ),
      
      mainPanel(
        plotOutput("Render_BoxPlot", width = "100%", height = "600px")
      )
    )
  ),
  
  # Dot Plot tab
  tabPanel(
    title = "DotPlot",
    
    tags$button(
      id = "go_to_tutorials_DotPlot",
      class = "action-button shiny-bound-input",
      "Back to Tutorials",
      style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
      onclick = "Shiny.setInputValue('go_to_tutorials_DotPlot', Math.random())"
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
            "Plot Dimension & Themes",
            
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
            
            colourInput("color_lowPvalue_DotPlot", "(Adj) P-value Color, Low:", value = "#7fc97f"),
            colourInput("color_interPvalue_DotPlot", "(Adj) P-value Color, Intermediate:", value = "#fdb462"),
            colourInput("color_highPvalue_DotPlot", "(Adj) P-value Color, High:", value = "#ef3b2c")
          ),
          
          # Legend Tab
          tabPanel(
            "Legend",
            
            numericInput("legendTitleSize_DotPlot", "Legend Title Size:", value = 15, step = 1),
            numericInput("legendTextSize_DotPlot", "Legend Text Size:", value = 14, step = 1),
            numericInput("legendkeySize_DotPlot", "Legend Key Size:", value = 0.7, step = 0.1, min = 0.1, max = 4),
            
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
          
          # Save Plot Tab
          tabPanel(
            "Save Plot",
            
            # numericInput("plotWidth_DotPlot", "Width (in pixels):", value = 800, step = 50),
            # numericInput("plotHeight_DotPlot", "Height (in pixels):", value = 600, step = 50),
            numericInput("dpi_DotPlot", "DPI for Saving:", value = 300, step = 300),
            selectInput(
              "formatdownload_DotPlot", "Format:",
              choices = c(".png", ".svg", ".tiff", ".pdf", ".pptx"),
              selected = ".png"
            ),
            downloadButton("download_DotPlot", "Download Plot")
          ),
        )
      ),
      
      mainPanel(
        plotOutput("Render_DotPlot", width = "auto", height = "auto")
      )
    )
  ),
  
  # BubblePlot Tab
  tabPanel(
    title = "BubblePlot",
    
    tags$button(
      id = "go_to_tutorials_BubblePlot",
      class = "action-button shiny-bound-input",
      "Back to Tutorials",
      style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
      onclick = "Shiny.setInputValue('go_to_tutorials_BubblePlot', Math.random())"
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
            "Plot Dimension & Themes",
            
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
            
            colourInput("color_lowPvalue_BubblePlot", "(Adj) P-value Color, Low:", value = "#7fc97f"),
            colourInput("color_interPvalue_BubblePlot", "(Adj) P-value Color, Intermediate:", value = "#fdb462"),
            colourInput("color_highPvalue_BubblePlot", "(Adj) P-value Color, High:", value = "#ef3b2c")
          ),
          
          # Legend
          tabPanel(
            "Legend",
            
            checkboxInput("showLegend_BubblePlot", "Show Legend", value = TRUE),
            
            numericInput("legendTitleSize_BubblePlot", "Legend Title Size:", value = 18, step = 1),
            numericInput("legendTextSize_BubblePlot", "Legend Text Size:", value = 15, step = 1),
            numericInput("legendkeySize_BubblePlot", "Legend Key Size:", value = 0.7, step = 0.1, min = 0.1, max = 4),
            
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
            numericInput("xMin_BubblePlot", "X-axis Minimum:", value = NA, step = 0.1),
            numericInput("xMax_BubblePlot", "X-axis Maximum:", value = NA, step = 0.1),
            
            numericInput("yMin_BubblePlot", "X-axis Minimum:", value = NA, step = 0.1),
            numericInput("yMax_BubblePlot", "X-axis Maximum:", value = NA, step = 0.1)
          ),
          
          tabPanel(
            "Axis Breaks",
            numericInput("xBreaks_BubblePlot", "X-axis Breaks:", value = NA, step = 0.1),
            numericInput("yBreaks_BubblePlot", "X-axis Breaks:", value = NA, step = 0.1)
          ),
          
          # Save Plot Tab
          tabPanel(
            "Save Plot",
            # numericInput("plotWidth_BubblePlot", "Width (in pixels):", value = 800, step = 50),
            # numericInput("plotHeight_BubblePlot", "Height (in pixels):", value = 600, step = 50),
            numericInput("dpi_BubblePlot", "DPI for Saving:", value = 300, step = 300),
            selectInput(
              "formatdownload_BubblePlot", "Format:",
              choices = c(".png", ".svg", ".tiff", ".pdf", ".pptx"),
              selected = ".png"
            ),
            downloadButton("download_BubblePlot", "Download Plot")
          ),
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
            # tabPanel("Dot Plot", value = "DotPlot_info"),
            # tabPanel("Bubble Plot", value = "Bubble_info")
          )
          #)
        ),
        column(
          width = 9,
          uiOutput("TutorialsContent")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Observe the reset button and reload the app when clicked
  observeEvent(input$reload_app_button, {
    session$reload()
  })
  
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
          
          tags$h4("First, download a step-by-step guide"),
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
          
          tags$h4("First, download a step-by-step guide"),
          downloadLink("download_VolcanoPlot_Tutorial_pdf", "Link to download"), 
          
          tags$h4("Next, prepare the input data"),
          
          tags$p("Three columns are required: Features, log2FoldChange, adj.P.Val."),
          tags$p("The input data were statistical output from e.g., MetaboAnalyst, ExpressAnalyst, and DESeq2."),
          downloadLink("download_VolcanoPlot_ExampleData", "Link to download example input data"),  # Link to the example data
          
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
          
          tags$h4("First, download a step-by-step guide"),
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
      
    } else {
      
      tagList(
        tags$main(
          tags$h3("Box plot"),
          
          img(src = "https://drive.google.com/thumbnail?id=1S-N9OWh4rhA3JWtBWcF75ullRgfMj-T9", height = "200px"),
          
          tags$h4("First, download a step-by-step guide"),
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
  
  # Heatmap Plot
  ## Heatmap data
  output$download_HeatmapSimple_ExampleNormData <- downloadHandler(
    filename = function() {
      "HeatmapSimple_ExampleNormData_1.csv"  # Name of the file to be downloaded
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
      "HeatmapSimple_ExampleMetaData_1.csv"  # Name of the file to be downloaded
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
      "BoxPlot_ExampleNormData_1.csv"  # Name of the file to be downloaded
    },
    content = function(file) {
      # Download the file from Google Drive and save it to 'file'
      download.file(
        url = "https://drive.google.com/uc?export=download&id=1QaYJ_NLsVYaQvSRY95zG-SWihvnYVm40", 
        destfile = file,
        mode = "wb"  # 'wb' mode to handle binary files like PDFs
      )
    }
  )
  
  ## Metadata
  output$download_BoxPlot_ExampleMetaData <- downloadHandler(
    filename = function() {
      "BoxPlot_ExampleMetaData_1.csv"  # Name of the file to be downloaded
    },
    content = function(file) {
      # Download the file from Google Drive and save it to 'file'
      download.file(
        url = "https://drive.google.com/uc?export=download&id=1QfkDrmA4jcbne5gdo1TlsGq9ABsHqYvw",
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
              axis.title = element_text(face = "bold",size = rel(1)),
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
  
  
  #<--Reactive values to store uploaded data for all modudeles -->
  values <- reactiveValues(
    # PCA/PLS-DA score_ScorePlot plot
    metadata_ScorePlot = NULL, score_ScorePlot = NULL, group_levels_ScorePlot = NULL, 
    # Volcano plots
    volcanoData = NULL,
    # Box Plots
    expression_BoxPlot = NULL, metadata_BoxPlot= NULL, group_levels_BoxPlot = NULL,
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
    values$metadata_ScorePlot <- read.csv(input$metadataFile_ScorePlot$datapath) %>% dplyr::mutate(Group = factor(Group))
  })
  
  # Observe score_ScorePlot file upload
  observeEvent(input$scoreFile_ScorePlot, {
    req(input$scoreFile_ScorePlot)
    values$score_ScorePlot <- read.csv(input$scoreFile_ScorePlot$datapath) %>% dplyr::rename(Sample = X)
    
    # Determine group levels dynamically after both files are uploaded
    if (!is.null(values$metadata_ScorePlot)) {
      combined_data <- inner_join(values$score_ScorePlot, values$metadata_ScorePlot %>% dplyr::select(Sample, Group), by = "Sample")
      values$group_levels_ScorePlot <- unique(combined_data$Group)
      
      # Update UI for selecting and ordering group levels
      output$groupLevelSelector_ScorePlot <- renderUI({
        selectInput("groupLevels", "Select and Order Group Levels", 
                    choices = values$group_levels_ScorePlot, 
                    selected = values$group_levels_ScorePlot,
                    multiple = TRUE)
      })
    }
  })
  
  # UI for dynamic color inputs based on group levels
  output$dynamicColorInputs_ScorePlot <- renderUI({
    req(values$group_levels_ScorePlot)  # Ensure group levels are available
    
    # Create a list of color inputs for each group level
    Publication_color_code = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
    color_inputs <- lapply(seq_along(values$group_levels_ScorePlot), function(i) {
      level <- values$group_levels_ScorePlot[i]
      default_color <- Publication_color_code[(i - 1) %% length(Publication_color_code) + 1]  # Cycle through Publication_color_code
      colourInput(inputId = paste0("color_", level), label = paste("Color for", level, "Group:"), value = default_color)
    })
    
    # Return the list of color input elements
    do.call(tagList, color_inputs)
  })
  
  # Reactive expression to create a named color palette from the selected group levels
  color_palette <- reactive({
    req(values$group_levels_ScorePlot)  # Ensure group levels are available
    sapply(values$group_levels_ScorePlot, function(level) {
      input[[paste0("color_", level)]]  # Extract color inputs dynamically
    }, simplify = FALSE)
  })
  
  # UI for allowing to edit legend labels
  output$dynamicLegendInputs_ScorePlot <- renderUI({
    req(values$group_levels_ScorePlot)  # Ensure group levels are available
    
    # Create text input fields for each group level to edit legend labels
    legend_inputs <- lapply(values$group_levels_ScorePlot, function(level) {
      textInput(
        inputId = paste0("legend_", level), 
        label = paste("Legend label for", level, "Group:"), 
        value = level  # Set default value to the current level name
      )
    })
    
    # Return the list of legend input elements
    do.call(tagList, legend_inputs)
  })
  
  # Reactive expression to create a named vector of legend labels
  legend_labels_ScorePlot <- reactive({
    req(values$group_levels_ScorePlot)  # Ensure group levels are available
    
    # Collect legend labels dynamically from inputs
    labels <- sapply(values$group_levels_ScorePlot, function(level) {
      input[[paste0("legend_", level)]]  # Access each text input value dynamically
    }, simplify = TRUE)
    
    # Return a named vector where the names are the group levels
    setNames(labels, values$group_levels_ScorePlot)
  })
  
  # Render the plot
  output$Render_ScorePlot <- renderPlot({
    req(values$metadata_ScorePlot, values$score_ScorePlot, input$groupLevels) # Ensure data and group level input are available
    
    # Combine datasets
    PCA_score_df <- values$score_ScorePlot %>%
      inner_join(values$metadata_ScorePlot %>% dplyr::select(Sample, Group), by = "Sample") %>%
      mutate(Group = factor(Group, levels = input$groupLevels))  # Use selected group levels
    
    # Generate dynamic color palette based on the number of groups
    color_code <- color_palette()  # Get dynamic color palette
    
    # Get the legend labels from reactive expression
    legend_labels_vector <- legend_labels_ScorePlot()
    
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
    
    # Prepare axis limits and breaks
    x_limits <- if (!is.na(input$xMin_ScorePlot) && !is.na(input$xMax_ScorePlot) && input$xMin_ScorePlot < input$xMax_ScorePlot) c(input$xMin_ScorePlot, input$xMax_ScorePlot) else NULL
    y_limits <- if (!is.na(input$yMin_ScorePlot) && !is.na(input$yMax_ScorePlot) && input$yMin_ScorePlot < input$yMax_ScorePlot) c(input$yMin_ScorePlot, input$yMax_ScorePlot) else NULL
    x_breaks <- if (!is.na(input$xBreaks_ScorePlot) && !is.na(input$xMin_ScorePlot) && !is.na(input$xMax_ScorePlot) && input$xBreaks_ScorePlot > 0) seq(from = input$xMin_ScorePlot, to = input$xMax_ScorePlot, by = input$xBreaks_ScorePlot) else waiver()
    y_breaks <- if (!is.na(input$yBreaks_ScorePlot) && !is.na(input$yMin_ScorePlot) && !is.na(input$yMax_ScorePlot) && input$yBreaks_ScorePlot > 0) seq(from = input$yMin_ScorePlot, to = input$yMax_ScorePlot, by = input$yBreaks_ScorePlot) else waiver()
    
    # Make a data.frame for 95CI -> Follow `MetaboAnalystR` package (https://github.com/xia-lab/MetaboAnalystR/blob/1c6aa245388f7c0ba617111e264fa53bec221c83/R/stats_chemometrics.R#L227)
    ## Extract score
    pc1  <- pull(PCA_score_df[2])
    pc2  <- pull(PCA_score_df[3])
    
    ## Get group and levels
    cls1 <- PCA_score_df$Group
    lvs <- levels(cls1)
    
    ## Generate array for ellipse with 100 points, using self-calculated var and mean of pc1 and pc2
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
    p = ggplot(PCA_score_df, aes(x = pull(PCA_score_df[2]), y = pull(PCA_score_df[3]), color = Group, fill = Group)) +
      geom_point(size = input$pointSize_ScorePlot, alpha = 1) +
      scale_color_manual(name = NULL, values = color_code, labels = legend_labels_vector) +
      scale_fill_manual(name = NULL, values = color_code, labels = legend_labels_vector) +
      # stat_ellipse(geom = "polygon", aes(fill = Group), type = "norm", level = 0.95, alpha = 0.2, show.legend = FALSE, size = 0) +
      labs(x = input$xLabel_ScorePlot, y = input$yLabel_ScorePlot) +
      plot_theme +
      theme(
        axis.title.x = element_text(size = input$labelSize_ScorePlot),
        axis.title.y = element_text(size = input$labelSize_ScorePlot),
        axis.text.x = element_text(size = input$tickLabelSize_ScorePlot),
        axis.text.y = element_text(size = input$tickLabelSize_ScorePlot),
        legend.title = element_blank(),
        legend.position = "top"
      ) +
      #coord_cartesian(xlim = x_limits, ylim = y_limits) +
      scale_x_continuous(limits = x_limits, breaks = x_breaks) +
      scale_y_continuous(limits = y_limits, breaks = y_breaks)
    
    # Conditionally add the 95% CI ellipse based on the checkbox
    if (input$checkbox_95CI_ScorePlot) {
      p <- p + 
        geom_polygon(aes(x = V1, y = V2, fill = Group), data = pts.df, alpha = 0.2, color = NA)
      # stat_ellipse(
      #   geom = "polygon", aes(fill = Group), type = "norm", level = 0.95, alpha = 0.2, show.legend = FALSE, size = 0
      # )
    }
    
    # Conditionally add the bold for axis and tick
    if (input$checkbox_Axis_bold_ScorePlot) {
      p <- p + theme(axis.title = element_text(face = "bold"))
    }
    
    if (input$checkbox_Tick_bold_ScorePlot) {
      p <- p + theme(axis.text = element_text(face = "bold"))
    }
    
    # Render the plot
    p
    
  }, width = reactive({ input$plotWidth_ScorePlot }), height = reactive({ input$plotHeight_ScorePlot }), res  = 72)
  
  # <-- Volcano Plot Handling -->
  observeEvent(input$volcanoFile, {
    req(input$volcanoFile)
    values$volcanoData <- read.csv(input$volcanoFile$datapath) %>% dplyr::rename(FDR = `adj.P.Val`)
  })
  
  output$Render_volcanoPlot <- renderPlot({
    req(values$volcanoData)
    
    # Define the theme dynamically
    plot_theme <- switch(
      input$plotTheme_Volcano,
      "theme_Publication" = theme_Publication(), 
      "theme_bw" = theme_bw(), 
      "theme_minimal" = theme_minimal(),
      "theme_linedraw" = theme_linedraw(), 
      "theme_classic" = theme_classic(),
      "theme_gray" = theme_gray(),
      "theme_bw" = theme_bw(),
    )
    
    # Prepare axis limits and breaks
    x_limits <- if (!is.na(input$xMin_Volcano) && !is.na(input$xMax_Volcano) && input$xMin_Volcano < input$xMax_Volcano) c(input$xMin_Volcano, input$xMax_Volcano) else NULL
    y_limits <- if (!is.na(input$yMin_Volcano) && !is.na(input$yMax_Volcano) && input$yMin_Volcano < input$yMax_Volcano) c(input$yMin_Volcano, input$yMax_Volcano) else NULL
    x_breaks <- if (!is.na(input$xBreaks_Volcano) && !is.na(input$xMin_Volcano) && !is.na(input$xMax_Volcano) && input$xBreaks_Volcano > 0) seq(from = input$xMin_Volcano, to = input$xMax_Volcano, by = input$xBreaks_Volcano) else waiver()
    y_breaks <- if (!is.na(input$yBreaks_Volcano) && !is.na(input$yMin_Volcano) && !is.na(input$yMax_Volcano) && input$yBreaks_Volcano > 0) seq(from = input$yMin_Volcano, to = input$yMax_Volcano, by = input$yBreaks_Volcano) else waiver()
    
    # Volcano plot function
    draw_volcano <- function(stat_df, FC_cut_of = 1.5, FDR_cut_of = 0.05){
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
      
      names(keyvals2)[keyvals2 == input$color_NotSig_Volcano] <- 'Not significant'
      names(keyvals2)[keyvals2 == input$color_DownFDR_Volcano] <- 'FDR (Down-regulation)' 
      names(keyvals2)[keyvals2 == input$color_UpFDR_Volcano] <- 'FDR (Up-regulation)'  
      names(keyvals2)[keyvals2 == input$color_DownFDR_FC_Volcano] <- 'FDR and FC (Down-regulation)'
      names(keyvals2)[keyvals2 == input$color_UpFDR_FC_Volcano] <- 'FDR and FC (Up-regulation)'
      
      
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
          legendLabSize = input$legendLabSize_Volcano,
          legendIconSize = input$legendIconSize_Volcano,
          drawConnectors = FALSE,
          subtitle = "",
          caption = input$caption_Volcano,
          captionLabSize = input$captionLabSize_Volcano,
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
          legendLabSize = input$legendLabSize_Volcano,
          legendIconSize = input$legendIconSize_Volcano,
          drawConnectors = FALSE,
          subtitle = "",
          caption = input$caption_Volcano,
          captionLabSize = input$captionLabSize_Volcano,
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
    draw_volcano(values$volcanoData, FC_cut_of = input$FC_cut_off_Volcano, FDR_cut_of = input$FDR_cut_off_Volcano) + 
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(
        axis.title.x = element_text(size = input$labelSize_Volcano),
        axis.title.y = element_text(size = input$labelSize_Volcano),
        axis.text.x = element_text(size = input$tickLabelSize_Volcano),
        axis.text.y = element_text(size = input$tickLabelSize_Volcano)
      ) +
      ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
      ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks) -> p_volcano
    
    # Show legend or nor
    if (input$Show_legend_Volcano) {
      p_volcano + ggplot2::theme(legend.position = "top",
                                 legend.title = element_blank()) -> p_volcano
    }
    
    ## If select other themes
    if (input$plotTheme_Volcano != "none" & input$Show_legend_Volcano) {
      p_volcano + 
        plot_theme + 
        ggplot2::theme(legend.position = "top", legend.title = element_blank()) -> p_volcano
    } 
    
    if (input$plotTheme_Volcano != "none" & input$Show_legend_Volcano == FALSE){
      p_volcano + 
        plot_theme + 
        ggplot2::theme(legend.position = "none") -> p_volcano
    }
    
    # Conditionally add the bold for axis and tick
    if (input$checkbox_Axis_bold_Volcano) {
      p_volcano <- p_volcano + theme(axis.title = element_text(face = "bold"))
    }
    
    if (input$checkbox_Tick_bold_Volcano) {
      p_volcano <- p_volcano + theme(axis.text = element_text(face = "bold"))
    }
    
    # For render
    p_volcano
    
  }, width = reactive({ input$plotWidth_Volcano }), height = reactive({ input$plotHeight_Volcano }), res  = 72)
  
  #<-- Boxplot Handling -->
  # Observe metadata_BoxPlot file upload
  observeEvent(input$metadataFile_BoxPlot, {
    req(input$metadataFile_BoxPlot)
    values$metadata_BoxPlot <- read.csv(input$metadataFile_BoxPlot$datapath) %>% dplyr::mutate(Group = factor(Group))
  })
  
  # Observe expression_BoxPlot file upload
  observeEvent(input$expressionFile_BoxPlot, {
    req(input$expressionFile_BoxPlot)
    values$expression_BoxPlot <- read.csv(input$expressionFile_BoxPlot$datapath, row.names = 1)
    
    # Determine group levels dynamically after both files are uploaded
    if (!is.null(values$metadata_BoxPlot)) {
      # combined_data <- inner_join(values$expression_BoxPlot, values$metadata_BoxPlot %>% dplyr::select(Sample, Group), by = "Sample")
      values$group_levels_BoxPlot <- unique(values$metadata_BoxPlot$Group)
      
      # Update UI for selecting and ordering group levels
      output$groupLevelSelector_BoxPlot <- renderUI({
        selectInput("groupLevels_selected_BoxPlot", "Select and Order Group Levels",
                    choices = values$group_levels_BoxPlot,
                    selected = values$group_levels_BoxPlot,
                    multiple = TRUE)
      })
    }
    
    # Determine features list dynamically after both files are uploaded
    
  })
  
  # UI for dynamic color inputs based on group levels
  output$dynamicColorInputs_BoxPlot <- renderUI({
    req(values$group_levels_BoxPlot)  # Ensure group levels are available
    
    # Create a list of color inputs for each group level
    Publication_color_code = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
    color_inputs <- lapply(seq_along(values$group_levels_BoxPlot), function(i) {
      level <- values$group_levels_BoxPlot[i]
      default_color <- Publication_color_code[(i - 1) %% length(Publication_color_code) + 1]  # Cycle through Publication_color_code
      colourInput(inputId = paste0("color_", level), label = paste("Color for", level, "Group:"), value = default_color)
    })
    
    # Return the list of color input elements
    do.call(tagList, color_inputs)
  })
  
  # Reactive expression to create a named color palette from the selected group levels
  color_palette_BoxPlot <- reactive({
    req(values$group_levels_BoxPlot)  # Ensure group levels are available
    sapply(values$group_levels_BoxPlot, function(level) {
      input[[paste0("color_", level)]]  # Extract color inputs dynamically
    }, simplify = FALSE)
  })
  
  # UI for allowing to edit legend labels
  output$dynamicLegendInputs_BoxPlot <- renderUI({
    req(values$group_levels_BoxPlot)  # Ensure group levels are available
    
    # Create text input fields for each group level to edit legend labels
    legend_inputs <- lapply(values$group_levels_BoxPlot, function(level) {
      textInput(
        inputId = paste0("legend_", level),
        label = paste("Legend label for", level, "Group:"),
        value = level  # Set default value to the current level name
      )
    })
    
    # Return the list of legend input elements
    do.call(tagList, legend_inputs)
  })
  
  # Reactive expression to create a named vector of legend labels
  legend_labels_BoxPlot <- reactive({
    req(values$group_levels_BoxPlot)  # Ensure group levels are available
    
    # Collect legend labels dynamically from inputs
    labels <- sapply(values$group_levels_BoxPlot, function(level) {
      input[[paste0("legend_", level)]]  # Access each text input value dynamically
    }, simplify = TRUE)
    
    # Return a named vector where the names are the group levels
    setNames(labels, values$group_levels_BoxPlot)
  })
  
  # Render the plot
  output$Render_BoxPlot <- renderPlot({
    req(values$metadata_BoxPlot, values$expression_BoxPlot, input$groupLevels_selected_BoxPlot) # Ensure data and group level input are available
    
    # Combine datasets
    values$expression_BoxPlot %>%
      t() %>% as.data.frame() %>%
      rownames_to_column(var = "Sample") -> Exp_df_BoxPlot
    
    data_normalized_STAT_DEMs = values$metadata_BoxPlot %>%
      inner_join(Exp_df_BoxPlot, by = "Sample") %>%
      mutate(Group = factor(Group, levels = input$groupLevels_selected_BoxPlot))  # Use selected group levels
    
    # Transform data
    data_gathered <- gather(data_normalized_STAT_DEMs, key = "Features", value = "Intensity", -Sample, -Group) %>%
      mutate(Intensity = as.numeric(Intensity))
    
    # Generate dynamic color palette based on the number of groups
    color_code <- color_palette_BoxPlot()  # Get dynamic color palette
    
    # # Get the legend labels from reactive expression
    legend_labels_vector <- legend_labels_BoxPlot()
    
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
      facet_wrap(~Features, scales = "free") +
      labs(y = input$yLabel_BoxPlot, x = "") +
      scale_color_manual(values = color_code) +
      scale_fill_manual(values = color_code) +
      plot_theme +
      theme(
        legend.position = "none",
        axis.title.x = element_text(size = input$labelSize_BoxPlot),
        axis.title.y = element_text(size = input$labelSize_BoxPlot),
        axis.text.x = element_text(size = input$tickLabelSize_BoxPlot),
        axis.text.y = element_text(size = input$tickLabelSize_BoxPlot),
        strip.text = element_text(size = input$stripLabelSize_BoxPlot)
      ) +
      scale_x_discrete(labels = legend_labels_vector)
    # scale_y_continuous(limits = y_limits, breaks = y_breaks)
    
    # Conditionally add the bold for axis and tick
    if (input$checkbox_Axis_bold_BoxPlot) {
      p_BoxPlot <- p_BoxPlot + theme(axis.title = element_text(face = "bold"))
    }
    
    if (input$checkbox_Tick_bold_BoxPlot) {
      p_BoxPlot <- p_BoxPlot + theme(axis.text = element_text(face = "bold"))
    }
    
    # Show plot
    p_BoxPlot
    
  }, width = reactive({ input$plotWidth_BoxPlot }), height = reactive({ input$plotHeight_BoxPlot }), res  = 72)
  
  #<-- HeatmapSimple plot Handling -->
  library(ComplexHeatmap)
  
  # Observe metadata_HeatmapSimple file upload
  observeEvent(input$metadataFile_HeatmapSimple, {
    req(input$metadataFile_HeatmapSimple)
    values$metadata_HeatmapSimple <- read.csv(input$metadataFile_HeatmapSimple$datapath) #%>% dplyr::mutate(Group = factor(Group))
  })
  
  # Observe NormData_HeatmapSimple file upload
  observeEvent(input$NormDataFile_HeatmapSimple, {
    req(input$NormDataFile_HeatmapSimple)
    values$NormData_HeatmapSimple <- read.csv(input$NormDataFile_HeatmapSimple$datapath, row.names = 1, check.names = FALSE) %>% 
      rownames_to_column(var = "Features")
    
    # Determine group levels dynamically after both files are uploaded
    if (!is.null(values$metadata_HeatmapSimple)) {
      values$group_levels_HeatmapSimple <- unique(values$metadata_HeatmapSimple$GroupLevel1)
      
      # Update UI for selecting and ordering group levels
      output$groupLevelSelector_HeatmapSimple <- renderUI({
        selectInput("groupLevels_selected_HeatmapSimple", "Select and Order Group Levels", 
                    choices = values$group_levels_HeatmapSimple, 
                    selected = values$group_levels_HeatmapSimple,
                    multiple = TRUE)
      })
    }
  })
  
  
  # UI for dynamic color inputs based on group levels
  output$dynamicColorInputs_HeatmapSimple <- renderUI({
    req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available
    
    # Create a list of color inputs for each group level
    Publication_color_code = c("#323232", "#EB9F49", "#7fc97f", "#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
    color_inputs <- lapply(seq_along(input$groupLevels_selected_HeatmapSimple), function(i) {
      level <- input$groupLevels_selected_HeatmapSimple[i]
      default_color <- Publication_color_code[(i - 1) %% length(Publication_color_code) + 1]  # Cycle through Publication_color_code
      colourInput(inputId = paste0("color_", level), label = paste("Color for", level, "Group:"), value = default_color)
    })
    
    # Return the list of color input elements
    do.call(tagList, color_inputs)
  })
  
  # Reactive expression to create a named color palette from the selected group levels
  color_palette_HeatmapSimple <- reactive({
    req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available
    
    # Define your Publication_color_code palette
    Publication_color_code <- c("#323232", "#EB9F49", "#7fc97f", "#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
    
    # Assign colors based on the group levels, cycling through Publication_color_code
    sapply(seq_along(input$groupLevels_selected_HeatmapSimple), function(i) {
      level <- input$groupLevels_selected_HeatmapSimple[i]
      
      # If input is not available yet, fallback to Publication_color_code
      input_color <- input[[paste0("color_", level)]]
      
      if (is.null(input_color)) {
        # Assign color from Publication_color_code based on the index
        default_color <- Publication_color_code[(i - 1) %% length(Publication_color_code) + 1]
        return(default_color)
      }
      
      return(input_color)
    }, simplify = FALSE)
  })
  
  
  # UI for allowing to edit legend labels
  output$dynamicLegendInputs_HeatmapSimple <- renderUI({
    req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available
    
    # Create text input fields for each group level to edit legend labels
    legend_inputs <- lapply(input$groupLevels_selected_HeatmapSimple, function(level) {
      textInput(
        inputId = paste0("legend_", level), 
        label = paste("Legend label for", level, "Group:"), 
        value = level  # Set default value to the current level name
      )
    })
    
    # Return the list of legend input elements
    do.call(tagList, legend_inputs)
  })
  
  # Reactive expression to create a named vector of legend labels
  legend_labels_HeatmapSimple <- reactive({
    req(input$groupLevels_selected_HeatmapSimple)  # Ensure group levels are available
    
    # Collect legend labels dynamically from inputs
    labels <- sapply(input$groupLevels_selected_HeatmapSimple, function(level) {
      input[[paste0("legend_", level)]]  # Access each text input value dynamically
    }, simplify = TRUE)
    
    # Return a named vector where the names are the group levels
    labels
  })
  
  
  # Render the plot
  output$Render_HeatmapSimple <- renderPlot({
    req(
      # Ensure data and group level input are available
      values$metadata_HeatmapSimple, values$NormData_HeatmapSimple, #values$group_levels_HeatmapSimple, 
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
    
  }, width = reactive({ input$plotWidth_HeatmapSimple }), height = reactive({ input$plotHeight_HeatmapSimple }), res  = 72)
  
  
  #<-- DotPlot plot Handling -->
  library(tidyverse)
  
  # Observe PathwayData_DotPlot file upload
  observeEvent(input$PathwayDataFile_DotPlot, {
    req(input$PathwayDataFile_DotPlot)
    values$PathwayData_DotPlot <- read.csv(input$PathwayDataFile_DotPlot$datapath, check.names = FALSE)
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
    req(values$PathwayData_DotPlot)#, input$groupLevels) # Ensure data and group level input are available
    
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
        #### Results from ExpressAnalyst  ####
        pathway_input_data %>%
          mutate(
            # Calculate gene ratio
            GeneRatio = Hits_count/Set_size,
            # # Assign to Activated or Suppressed group
            Sign = ifelse(Enrichment_score < 0, "Suppressed", "Activated")
          ) -> pathway_input_data
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
    
    p_DotPlot
    
  }, width = reactive({ input$plotWidth_DotPlot }), height = reactive({ input$plotHeight_DotPlot }), res = 72)
  
  
  #<-- BubblePlot plot Handling -->
  library(tidyverse)
  
  # Observe PathwayData_BubblePlot file upload
  observeEvent(input$PathwayDataFile_BubblePlot, {
    req(input$PathwayDataFile_BubblePlot)
    values$PathwayData_BubblePlot <- read.csv(input$PathwayDataFile_BubblePlot$datapath, check.names = FALSE)
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
    req(values$PathwayData_BubblePlot)#, input$groupLevels) # Ensure data and group level input are available
    
    pathway_input_data = values$PathwayData_BubblePlot
    
    # Define the theme dynamically
    plot_theme <- switch(
      input$plotTheme_BubblePlot,
      "theme_Publication" = theme_Publication_modifedBubblePlot(),
      "theme_bw" = theme_bw(),
      "theme_minimal" = theme_minimal(),
      "theme_linedraw" = theme_linedraw(),
      "theme_classic" = theme_classic(),
      "theme_gray" = theme_gray(),
    )
    
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
    
    p_BubblePlot
    
  }, width = reactive({ input$plotWidth_BubblePlot }), height = reactive({ input$plotHeight_BubblePlot }), res = 72)
  
  
  # <-- Download handlers -->
  ## Score plot
  output$downloadScorePlot <- downloadHandler(
    filename = function() {
      paste(gsub("-", "_", Sys.Date()), "_2DScores_Plot", input$formatdownloadScorePlot, sep = "")
    },
    content = function(file) {
      ggsave(
        file, plot = last_plot(), 
        width = input$plotWidth_ScorePlot * input$dpi_ScorePlot / 72, 
        height = input$plotHeight_ScorePlot * input$dpi_ScorePlot / 72, 
        dpi = input$dpi_ScorePlot,
        units = "px"
      )
    }
  )
  
  ## Volcano plot
  output$download_VolcanoPlot <- downloadHandler(
    filename = function() {
      paste(gsub("-", "_", Sys.Date()), "_Volcano_Plot", input$formatdownloadVolcano, sep = "")
    },
    content = function(file) {
      ggsave(
        file, plot = last_plot(), 
        width = input$plotWidth_Volcano * input$volcanoDPI / 72, 
        height = input$plotHeight_Volcano * input$volcanoDPI / 72, 
        dpi = input$volcanoDPI,
        units = "px"
      )
    }
  )
  
  ## Box plot
  output$download_BoxPlot <- downloadHandler(
    filename = function() {
      paste(gsub("-", "_", Sys.Date()), "_Box_Plots", input$formatdownload_BoxPlot, sep = "")
    },
    content = function(file) {
      ggsave(
        file, plot = last_plot(), 
        # Adjust height and width accordingly
        width = input$plotWidth_BoxPlot * input$dpi_BoxPlot / 72, 
        height = input$plotHeight_BoxPlot * input$dpi_BoxPlot / 72, 
        dpi = input$dpi_BoxPlot, 
        units = "px"
      )
    }
  )
  
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
        values$metadata_HeatmapSimple, values$NormData_HeatmapSimple, #values$group_levels_HeatmapSimple, 
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
  
  ## Dot plot
  output$download_DotPlot <- downloadHandler(
    filename = function() {
      paste(gsub("-", "_", Sys.Date()), "_DotPlot", input$formatdownload_DotPlot, sep = "")
    },
    content = function(file) {
      ggsave(
        file, plot = last_plot(), 
        # Adjust height and width accordingly
        width = input$plotWidth_DotPlot * input$dpi_DotPlot / 72,  
        height = input$plotHeight_DotPlot * input$dpi_DotPlot / 72, 
        dpi = input$dpi_DotPlot, 
        units = "px"
      )
    }
  )
  
  ## Bubble plot
  output$download_BubblePlot <- downloadHandler(
    filename = function() {
      paste(gsub("-", "_", Sys.Date()), "_BubblePlot", input$formatdownload_BubblePlot, sep = "")
    },
    content = function(file) {
      ggsave(
        file, plot = last_plot(), 
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


