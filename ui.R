library(shiny)
library(tidygraph)
library(igraph)
library(ggraph)
library(DT)
library(ggplot2)
library(cluster)
library(bslib)
library(shinyWidgets)
library(shinyjs)


# Define the theme
theme <- bs_theme(
  version = 5,
  bootswatch = "cerulean",  # Professional dark theme
  bg = "#0a0a0a",
  fg = "#ffffff",
  primary = "#6366f1",
  secondary = "#8b5cf6",
  success = "#10b981",
  info = "#3b82f6",
  warning = "#f59e0b",
  danger = "#ef4444",
  light = "#f3f4f6",
  dark = "#1f2937"
)

# Create the harmonized UI
ui <- page_navbar(
  title = div(class = "d-flex align-items-center", style = "margin-right:10px;",
              img(src = "Victor_Logo2.png", height = "40px", style = "margin-right:10px;", class = "me-2"),
              div(
                style = "display: flex; flex-direction: column; line-height: 1.2;",
                span(style = "color:#373737; font-size: 14px; font-weight: 600;", "Knowledge"),
                span(style = "color:#373737; font-size: 14px; font-weight: 600;", "Segmentation")
              )
  ),
  theme = theme,
  id = "main_navbar",
  header = tagList(
    useShinyjs(),
    includeCSS("www/custom.css")
  ),
  navbar_options = navbar_options(collapsible = TRUE,position = "static-top",
                                  style = tags$style(type="text/css", "body {padding-top: 30px")),
  
  # Graph Tab ##################################################################
  nav_menu(
    "Graph Analysis",
    icon = icon("project-diagram"),
    
    nav_panel(
      "Network Visualization",
      value = "graph_tab",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          title = "Graph Controls",
          bg = "#1f2937",
          fg = "#ffffff",
          
          div(
            class = "mb-4",
            h5("Segmentation Parameters", class = "text-uppercase text-muted small mb-3"),
            
            selectInput(
              "seg_logic", 
              "Segmentation Type", 
              choices = c("Community Detection" = "community", 
                          "K-Means Clustering" = "kmeans_cluster", 
                          "Role-Based Segmentation" = "role_segment", 
                          "Dynamic Segmentation" = "dynamic_segment"), 
              selected = "community"
            ),
            
            sliderInput(
              "deg_thresh", 
              "Connector Degree Threshold", 
              min = 1, 
              max = 8, 
              value = 3,
              step = 0.5
            ),
            
            conditionalPanel(
              condition = "input.seg_logic == 'role_segment'",
              selectInput(
                "dept_target", 
                "Department for Segment", 
                choices = unique(na.omit(nodes$Education)), 
                selected = "Graduation"
              )
            )
          ),
          
          div(
            class = "mb-4",
            h5("Visualization Options", class = "text-uppercase text-muted small mb-3"),
            
            selectInput(
              "highlight_node", 
              "Highlight Node", 
              choices = nodes$label[1:20], 
              selected = "5524"
            ),
            
            switchInput(
              "show_labels",
              "Show Node Labels",
              value = TRUE,
              width = "100%"
            ),
            
            switchInput(
              "animate_layout",
              "Animate Layout",
              value = FALSE,
              width = "100%"
            )
          ),
          
          div(
            class = "mt-4",
            actionButton(
              "update_graph",
              "Update Graph",
              class = "btn-primary w-100",
              icon = icon("sync-alt")
            )
          )
        ),
        
        div(
          class = "p-3",
          card(
            full_screen = TRUE,
            height = "600px",
            card_body(
              plotOutput("graph_plot", height = "500px")
            )
          ),
          
          card(
            class = "mt-3",
            card_header(
              h5("Node Details", class = "mb-0")
            ),
            card_body(
              DTOutput("node_table")
            )
          ),
          # Add after DTOutput("node_table")
          fluidRow(
            column(6, 
                   card(
                     card_header( h5("Network Statistics", class = "mb-0")
                     ),
                     card_body( uiOutput("network_stats") )
                   )
            ),
            column(6,
                   card(
                     card_header( h5("Segment Distribution", class = "mb-0")
                     ),
                     card_body( plotOutput("segment_dist", height = "200px") )
                   )
            )
          ),
          fluidRow(
            column(12,
                   card(
                     card_header( h5("Centrality Distribution", class = "mb-0")
                     ),
                     card_body( plotOutput("centrality_dist", height = "450px") )
                   )
            )
          )
        ) # end of main 
      )
    ),
    
    nav_panel(
      "Influence Map",
      value = "influence_tab",
      fluidRow(
        column(7,
               card(
                 card_header( h5("Segment Influence Map", class = "mb-0")
                 ),
                 card_body( plotOutput("influence_plot", height = "400px") )
               )
        ),
        column(5,
               card(
                 card_header( h5("Influence Metrics", class = "mb-0")
                 ),
                 card_body( uiOutput("influence_metrics") )
               )
        )
      ),
      fluidRow(
        column(12,
               card(
                 card_header( h5("Segment-to-Segment COnnectivity", class = "mb-0")
                 ),
                 card_body( DTOutput("influence_table") )
               )
        )
      )
    ), # end of influence tab
    
    nav_panel(
      "Inter-Segment Bridges",
      value = "bridges_tab",
      fluidRow(
        column(6,
               card(
                 card_header(
                   h5("Bridge Impact Analysis", class = "mb-0")
                 ),
                 card_body(
                   plotOutput("bridge_impact", height = "500px")
                 )
               )
        ),
        column(6,
               card(
                 card_header(
                   h5("Bridge Metrics", class = "mb-0")
                 ),
                 card_body(
                   uiOutput("bridge_metrics")
                 )
               )
        )
      ),
      fluidRow(
        column(12,
               card(
                 card_header(
                   h5("Top Potential Bridges (Suggested Edges)", class = "mb-0")
                 ),
                 card_body(
                   DTOutput("bridges_table"),
                   br(),
                   div(style = "display: flex; justify-content: space-between;",
                       actionButton("apply_bridges", "Apply Selected Bridges", class = "btn-primary"),
                       actionButton("refresh_bridges", "Refresh Recommendations", class = "btn-secondary")
                   )
                 )
               )
        )
      )
    ) # end of inter-segment
  ),
  
  # Analysis Tabs ==============================================================
  nav_menu(
    "Analysis",
    icon = icon("chart-line"),
    
    nav_panel("Personal Dashboard",
              layout_sidebar(
                sidebar = sidebar(
                  selectInput("person_sel", "Person", choices = nodes$label[1:10]), #nodes$label[nodes$type == "Person"]),
                  sliderInput("connection_depth", "Connection Depth", min = 1, max = 3, value = 1),
                  checkboxInput("show_recommendations", "Show Recommendations", value = TRUE),
                  actionButton("analyze_person", "Analyze", class = "btn-primary")
                ),
                navset_tab(
                  nav_panel("Overview", 
                            layout_column_wrap(
                              card(
                                header = "Personal Metrics",
                                status = "primary",
                                solid_header = TRUE,
                                uiOutput("personal_metrics")
                              ),
                              card(
                                header = "Network Position",
                                status = "info",
                                solid_header = TRUE,
                                plotOutput("network_position", height = "250px")
                              )
                            )
                  ),
                  nav_panel("Connections",
                            card(
                              header = "Network Connections",
                              status = "success",
                              solid_header = TRUE,
                              plotOutput("personal_network", height = "400px")
                            )
                  ),
                  nav_panel("Recommendations",
                            conditionalPanel(
                              condition = "input.show_recommendations == true",
                              card(
                                header = "Personalized Recommendations",
                                status = "warning",
                                solid_header = TRUE,
                                uiOutput("personal_recommendations")
                              )
                            )
                  )
                )
              )
    ),
    
    nav_panel("Segment Evolution",
              layout_columns(
                col_widths = c(8, 4),
                card(
                  full_screen = TRUE,
                  card_header("Segment Evolution Timeline"),
                  layout_column_wrap(
                    plotOutput("evo_plot", height = "400px"),
                    div(style = "display: flex; justify-content: center; margin-top: 10px; gap: 10px;",
                        actionButton("play_animation", "Play Animation", class = "btn-primary"),
                        actionButton("pause_animation", "Pause", class = "btn-secondary")
                    )
                  )
                ),
                card(
                  full_screen = TRUE,
                  card_header("Evolution Controls"),
                  layout_columns(col_widths = 12,
                    sliderInput("time_range", "Time Range", 
                                min = min(edges$timestamp), 
                                max = max(edges$timestamp), 
                                value = c(min(edges$timestamp), max(edges$timestamp)),
                                timeFormat = "%Y-%m-%d"),
                    selectInput("evo_segment_type", "Segment Type", 
                                choices = c("community", "kmeans_cluster", "role_segment"), 
                                selected = "community"),
                    checkboxInput("show_stability", "Show Stability Metrics", value = TRUE)
                  )
                )
              ),
              conditionalPanel(
                condition = "input.show_stability == true",
                card(
                  full_screen = TRUE,
                  card_header("Segment Stability Analysis"),
                  uiOutput("stability_analysis")
                )
              )
    ),
    
    nav_panel("Segment Similarity",
              layout_columns(
                col_widths = c(6, 6),
                card(
                  header = "Similarity Heatmap",
                  plotOutput("sim_heatmap", height = "400px")
                ),
                card(
                  header = "Similarity Network",
                  plotOutput("sim_network", height = "400px")
                )
              ),
              card(
                header = "Similarity Matrix",
                DTOutput("sim_matrix"),
                br(),
                div(style = "display: flex; justify-content: space-between;",
                    selectInput("similarity_metric", "Similarity Metric", 
                                choices = c("Euclidean", "Manhattan", "Cosine"), 
                                selected = "Euclidean"),
                    actionButton("recalculate_similarity", "Recalculate", class = "btn-primary")
                )
              )
    )
  ),
  
  # Export & Reports Tabs
  nav_menu(
    "Export & Reports",
    icon = icon("file-export"),
    
    nav_panel("Segment Drilldown/Export",
              layout_sidebar(
                sidebar = sidebar(
                  selectInput("seg_id", "Segment ID", choices = unique(nodes$community)),
                  selectInput("export_format", "Export Format", choices = c("CSV", "JSON", "Excel")),
                  checkboxInput("include_metrics", "Include Network Metrics", value = TRUE),
                  checkboxInput("include_recommendations", "Include Recommendations", value = FALSE),
                  actionButton("analyze_segment", "Analyze Segment", class = "btn-primary"),
                  br(), br(),
                  downloadButton("export_seg", "Download Segment Members")
                ),
                navset_tab(
                  nav_panel("Overview",
                            layout_column_wrap(
                              card(
                                header = "Segment Summary",
                                uiOutput("segment_summary")
                              ),
                              card(
                                header = "Segment Composition",
                                plotOutput("segment_composition", height = "250px")
                              )
                            )
                  ),
                  nav_panel("Members",
                            card(
                              header = "Segment Members",
                              DTOutput("seg_drill_table")
                            )
                  ),
                  nav_panel("Analysis",
                            conditionalPanel(
                              condition = "input.include_metrics == true",
                              card(
                                header = "Segment Network Analysis",
                                plotOutput("segment_network", height = "400px")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.include_recommendations == true",
                              card(
                                header = "Segment Recommendations",
                                uiOutput("segment_recommendations")
                              )
                            )
                  )
                )
              )
    ),
    
    nav_panel("Multi-Segment Membership",
              layout_columns(
                col_widths = c(6, 6),
                card(
                  header = "Multi-Segment Overlap Analysis",
                  plotOutput("multi_overlap", height = "500px")
                ),
                card(
                  header = "Membership Statistics",
                  uiOutput("multi_stats")
                )
              ),
              card(
                header = "Multi-Segment Members",
                DTOutput("multi_table")
              )
    ),
    
    nav_panel("Segment Alerts",
              layout_columns(
                col_widths = c(8, 4),
                card(
                  header = "Alert Timeline",
                  plotOutput("alert_timeline", height = "300px")
                ),
                card(
                  header = "Alert Summary",
                  uiOutput("alert_summary")
                )
              ),
              card(
                header = "Segment Change Alerts",
                DTOutput("alert_table"),
                br(),
                div(style = "display: flex; justify-content: space-between;",
                    selectInput("alert_severity", "Filter by Severity", 
                                choices = c("All", "High", "Medium", "Low"), 
                                selected = "All"),
                    actionButton("resolve_alerts", "Mark Selected as Resolved", class = "btn-success"),
                    actionButton("export_alerts", "Export Alerts", class = "btn-primary")
                )
              )
    ),
    
    nav_panel("Segment Report Export",
              layout_columns(
                col_widths = c(6, 6),
                card(
                  header = "Report Configuration",
                  selectInput("report_segment", "Select Segment", choices = unique(nodes$community)),
                  selectInput("report_format", "Report Format", choices = c("PDF", "HTML", "Word")),
                  checkboxInput("include_charts", "Include Charts", value = TRUE),
                  checkboxInput("include_recommendations", "Include Recommendations", value = TRUE),
                  checkboxInput("include_alerts", "Include Alerts", value = FALSE),
                  sliderInput("report_timeframe", "Timeframe (Days)", min = 7, max = 90, value = 30)
                ),
                card(
                  header = "Report Preview",
                  htmlOutput("report_preview")
                )
              ),
              card(
                header = "Generate Report",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                    div(
                      h5("Download segment summary for reporting."),
                      p("Reports include network metrics, segment analysis, and visualizations.")
                    ),
                    div(
                      downloadButton("export_report", "Download Segment Report", class = "btn-primary"),
                      actionButton("schedule_report", "Schedule Recurring Report", class = "btn-secondary")
                    )
                )
              )
    )
  ),
  
  # Settings Tab
  nav_menu(
    "Settings",
    icon = icon("cog"),
    
    nav_panel(
      "Application Settings",
      value = "settings_tab",
      fluidRow(
        column(
          width = 12,
          card(
            card_header(
              h5("Application Settings", class = "mb-0")
            ),
            card_body(
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  h6("Graph Settings", class = "mb-3"),
                  sliderInput(
                    "node_size",
                    "Default Node Size",
                    min = 5,
                    max = 20,
                    value = 10
                  ),
                  sliderInput(
                    "edge_width",
                    "Default Edge Width",
                    min = 1,
                    max = 5,
                    value = 2,
                    step = 0.5
                  ),
                  selectInput(
                    "layout_algorithm",
                    "Layout Algorithm",
                    choices = c("Force Directed", "Circular", "Hierarchical", "Grid"),
                    selected = "Force Directed"
                  )
                ),
                div(
                  class = "col-md-6",
                  h6("Display Settings", class = "mb-3"),
                  selectInput(
                    "color_scheme",
                    "Color Scheme",
                    choices = c("Default", "Dark", "Light", "Colorblind Friendly"),
                    selected = "Default"
                  ),
                  switchInput(
                    "show_tooltips",
                    "Show Tooltips",
                    value = TRUE
                  ),
                  switchInput(
                    "enable_zoom",
                    "Enable Zoom",
                    value = TRUE
                  )
                )
              ),
              
              hr(),
              
              div(
                class = "d-flex justify-content-end",
                actionButton(
                  "save_settings",
                  "Save Settings",
                  class = "btn-primary",
                  icon = icon("save")
                ),
                actionButton(
                  "reset_settings",
                  "Reset to Default",
                  class = "btn-outline-secondary ms-2",
                  icon = icon("undo")
                )
              )
            )
          )
        )
      )
    )
  )

#####################
) # end of UI