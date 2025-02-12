library(shiny)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)
library(bslib)
library(shinytoastr)
library(ggiraph)

# Define UI
ui_Interactive <- fluidPage(
  # Custom CSS ----
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
  
  # navbarPage ----
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
    
    ## Introduction Tab ----
    tabPanel(
      title = "Welcome",
      tagList(
        
        tags$main(
          tags$h2("EasyPubPlot - Easy and Publishable Plotting"),
          tags$p(
            "EasyPubPlot provides an interactive and customizable tools to easily",
            "create publishable plots for scientific papers"
          ),
          
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
            img(src = "https://github.com/Pharmaco-OmicsLab/EasyPubPlot/blob/0c198fa1beb3e7fc5a10173e20cf636ab1ea2337/docs/Example_plots/Embed_Graphical_Abstract.png?raw=true", height = "700px"),
            style = "text-align: center;"
          )
        )
      )
    ),
    
    ## Volcano Plot Tab ----
    tabPanel(
      title = "Volcano Plot",
      
      tags$button(
        id = "go_to_tutorials_VolcanoPlot",
        class = "action-button shiny-bound-input",
        "Back to Tutorials",
        style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;",  # Custom styles
        onclick = "Shiny.setInputValue('go_to_tutorials_VolcanoPlot', Math.random())"
      ),
      
      tags$button(
        id = "reload_tab_button_Volcano",
        class = "action-button shiny-bound-input",
        "Reset This Tab",
        style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;"  # Custom styles
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
                
                numericInput("plotWidth_VolcanoPlot", "Width (in pixels):", value = 800, step = 5),
                numericInput("plotHeight_VolcanoPlot", "Height (in pixels):", value = 600, step = 5),
                
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
                  selected = "right"
                ),
                
                tags$p(
                  "NOTE: In interactive mode, changing the legend position may cause it to overlap. This issue will not affect the downloaded plot.",
                  style = "font-size: 16px; font-weight: bold;"
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
                numericInput("dpi_VolcanoPlot", "Resolution (DPI):", value = 300, step = 300),
                selectInput(
                  "formatdownload_VolcanoPlot", "Format:",
                  choices = c(".png", ".svg", ".tiff", ".pdf"),
                  selected = ".png"
                ),
                downloadButton("download_VolcanoPlot", "Download Plot")
              )
            )
          )
          
        ),
        
        mainPanel(
          girafeOutput("Render_volcanoPlot", width = "100%", height = "600px")
        )
      )
    ),
    
    ## Heatmap tap ----
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
                selectInput(
                  "formatdownload_HeatmapSimple", "Format:",
                  choices = c(".png", ".svg", ".tiff", ".pdf"),
                  selected = ".png"
                ),
                downloadButton("download_HeatmapSimple", "Download Plot")
              ),
            )
          )
          
        ),
        
        mainPanel(
          plotOutput("Render_HeatmapSimple", width = "100%", height = "600px")
        )
      )
    ),
    
    ## Scores Plot Tab ----
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
                
                numericInput("dpi_ScorePlot", "Resolution (DPI):", value = 300, step = 300),
                selectInput(
                  "formatdownload_ScorePlot", "Format:",
                  choices = c(".png", ".svg", ".tiff", ".pdf"),
                  selected = ".png"
                ),
                downloadButton("download_ScorePlot", "Download Plot")
              ),
            )
          )
        ),
        
        mainPanel(
          plotOutput("Render_ScorePlot", width = "100%", height = "600px")
        )
      )
    ),
    
    ## Box Plot Tab ----
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
        style = "font-size: 15px; font-weight: bold; padding: 5px 15px; background-color: #47B0C3; color: white; border: none; border-radius: 5px; cursor: pointer;"  # Custom styles
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
                
                checkboxInput("checkbox_PvalCalculation_BoxPlot", "Calculate (Adjusted) P-value (Only support 2-group comparison)", value = FALSE),
                
                
                uiOutput("groupLevelSelector_BoxPlot"),  # This will be dynamically generated based on the uploaded data
                uiOutput("dynamicColorInputs_BoxPlot"),
                
                uiOutput("dynamicLegendInputs_BoxPlot"), # For dynamic legend labels
              ),
              
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
              
              # Statistics Tab
              tabPanel(
                "Statistics",
                
                tags$p(
                  "NOTE: The current version only supports unpaired two-sided t-test with equal variance.",
                  style = "font-size: 16px; font-weight: bold;"
                ),
                checkboxInput("checkbox_FDR_BoxPlot", "Adjust P-value", value = TRUE),
                selectInput(
                  "PValCorrectionMethod_BoxPlot", "P-value Correction Methods:",
                  # choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"),
                  choices = c("Holm", "Hochberg", "Hommel", "Bonferroni", "Benjamini-Hochberg (BH)"),
                  selected = "Benjamini-Hochberg (BH)"
                ),
                # checkboxInput("checkbox_showPvalue_BoxPlot", "Show (Adjusted) P-value", value = FALSE),
                # Download Pvalue
                downloadButton("download_PvalueBoxPlot", "Download (Adjusted) P-value Results"),
                
                tags$p(
                  "",
                  style = "font-size: 16px; font-weight: bold;"
                ),
                numericInput("labelSizeStatistic_BoxPlot", "Label Size:", value = 4)
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
    
    ## Dot Plot tab ----
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
                
                # Metabolomics or Transcriptomics/Proteomics
                selectInput(
                  "PathwayFromOmics_DotPlot", "Using:",
                  choices = c("Metabolomics", "Transcriptomics/Proteomics"),
                  selected = "Transcriptomics/Proteomics"
                ),
                
                # Use Pvalue or adjPvalue
                checkboxInput("checkbox_adjPvalue_DotPlot", "Adjusted P-value"),  # Get automatically from the server
                
                numericInput("STATcutoff_DotPlot", "(Adjusted) P-value Cut-off:", value = 0.05, step = 0.01),
                
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
                
                numericInput("dpi_DotPlot", "Resolution (DPI):", value = 300, step = 300),
                selectInput(
                  "formatdownload_DotPlot", "Format:",
                  choices = c(".png", ".svg", ".tiff", ".pdf"),
                  selected = ".png"
                ),
                downloadButton("download_DotPlot", "Download Plot")
              ),
            )
          )
        ),
        
        mainPanel(
          plotOutput("Render_DotPlot", width = "100%", height = "600px")
        )
      )
    ),
    
    ## Bubble Plot Tab ----
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
                
                # Metabolomics or Transcriptomics/Proteomics
                selectInput(
                  "PathwayFromOmics_BubblePlot", "Omics Data:",
                  choices = c("Metabolomics", "Transcriptomics/Proteomics"),
                  selected = "Metabolomics"
                ),
                
                # Use Pvalue or adjPvalue
                checkboxInput("checkbox_adjPvalue_BubblePlot", "Adjusted P-value"), # value will be returned from the server
                
                numericInput("STATcutoff_BubblePlot", "(Adjusted) P-value Cut-off:", value = 1, step = 0.01),
                
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
                
                numericInput("dpi_BubblePlot", "Resolution (DPI):", value = 300, step = 300),
                selectInput(
                  "formatdownload_BubblePlot", "Format:",
                  choices = c(".png", ".svg", ".tiff", ".pdf"),
                  selected = ".png"
                ),
                downloadButton("download_BubblePlot", "Download Plot")
              ),
            )
          )
        ),
        
        mainPanel(
          girafeOutput("Render_BubblePlot", width = "100%", height = "600px")
        )
      )
    ),
    
    ## Tutorial Tab ----
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
              
              tabPanel("Volcano Plot", value = "VolcanoPlot_infor"),
              tabPanel("Heatmap", value = "HeatmapSimple_infor"),
              tabPanel("Scores Plot", value = "ScoresPlot_infor"),
              tabPanel("Box Plot", value = "BoxPlot_infor"),
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