library(shiny)
library(tidygraph)
library(igraph)
library(ggraph)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(ggplot2)
library(lubridate)
library(cluster)







server <- function(input, output, session) {
  
  # ---- Reactive Data and Segmentation ----
  kg_reactive <- reactive({
    # Create graph
    kg <- tbl_graph(nodes = nodes %>% mutate(name = as.character(id)), 
                    edges = edges %>% mutate(from = as.character(from), to = as.character(to)), 
                    directed = TRUE)
    g_ig <- as.igraph(kg)
    
    # Calculate network metrics ================================================
    nodes$degree <- degree(g_ig)
    nodes$betweenness <- betweenness(g_ig)
    nodes$eigen_centrality <- eigen_centrality(g_ig)$vector
    nodes$closeness <- closeness(g_ig)
    nodes$community <- g_ig %>% as_undirected() %>% cluster_louvain() %>% membership() %>% as.integer()
    nodes$pagerank <- page_rank(g_ig, weights = E(g_ig)$strength)$vector
    
    # Dynamic segment based on degree threshold and department =================
    nodes$dynamic_segment <- ifelse(
      nodes$degree >= input$deg_thresh, "Connector",
      ifelse(nodes$type == "Person" & nodes$department == input$dept_target, "Target Dept", "Other")
    )
    
    # K-means clustering =======================================================
    feats <- nodes %>% select(degree, betweenness, eigen_centrality, closeness) %>% 
      mutate(across(where(is.numeric),\(x) tidyr::replace_na(x,0)))
    set.seed(123)
    
    nodes$kmeans_cluster <- as.factor(cluster::pam(scale(feats), k = 5, cluster.only = TRUE))
    
    # Role-based segmentation ==================================================
    nodes$role_segment <- case_when(
      nodes$degree >= quantile(nodes$degree, 0.75, na.rm = TRUE) ~ "Influencer",
      nodes$degree == 0 ~ "Isolated",
      nodes$betweenness >= quantile(nodes$betweenness, 0.9, na.rm = TRUE) ~ "Bridge",
      TRUE ~ "Regular"
    )
    
    # Multi-segment membership
    nodes$multi_segment <- paste0("C", nodes$community, "_K", nodes$kmeans_cluster)
    
    list(nodes = nodes, edges = edges, g_ig = g_ig)
  })
  
  # ---- Dynamic UI Updates ---- ###############################################
  observe({
    kg <- kg_reactive()
    updateSelectInput(session, "highlight_node", choices = kg$nodes$label[1:20])
    updateSelectInput(session, "person_sel", choices = kg$nodes$label[kg$nodes$type == "Customer"])
    updateSelectInput(session, "seg_id", choices = sort(unique(kg$nodes$community)))
    updateSelectInput(session, "drill_segment", choices = sort(unique(kg$nodes$community)))
  })
  
  # ---- Graph Visualization ---- ##############################################
  output$graph_plot <- renderPlot({
    kg <- kg_reactive()
    nodes <- kg$nodes
    seg_col <- input$seg_logic
    highlight <- input$highlight_node
    
    g <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
    
    ggraph(g, layout = "fr") +
      geom_edge_link(aes(width = strength), alpha = 0.4) +
      geom_node_point(aes_string(color = seg_col, shape = "role_segment", size = "degree"), show.legend = TRUE) +
      geom_node_text(aes(label = label), repel = TRUE, size = 4, 
                     color = ifelse(nodes$label == highlight, "red", "black")) +
      theme_void() +
      ggtitle(paste("Knowledge Graph with", seg_col, "Segmentation"))
  })
  
  # ---- Node Table ---- #######################################################
  output$node_table <- renderDT({
    kg <- kg_reactive()
    datatable(kg$nodes, options = list(pageLength = 8), rownames = FALSE)
  })
  
  # Network statistics
  output$network_stats <- renderUI({
    kg <- kg_reactive()
    g_ig <- kg$g_ig
    
    # Community metrics
    comm <- kg$nodes$community
    mod <- modularity(cluster_louvain(g_ig %>% as_undirected()))
    
    # Segment metrics
    seg_col <- input$seg_logic
    segments <- kg$nodes[[seg_col]]
    
    fluidRow(
      column(4,
             card(
               card_header("Network Overview"),
               div(style = "display: flex; flex-direction: column; gap: 10px;",
                   div(class = "d-flex justify-content-between",
                       span("Nodes:"), 
                       span(strong(vcount(g_ig)), class = "text-primary")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Edges:"), 
                       span(strong(ecount(g_ig)), class = "text-info")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Density:"), 
                       span(strong(round(edge_density(g_ig), 3)), class = "text-success")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Average Path Length:"), 
                       span(strong(round(mean_distance(g_ig), 2)), class = "text-warning")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Transitivity:"), 
                       span(strong(round(transitivity(g_ig), 3)), class = "text-danger")
                   )
               )
             )
      ),
      column(4,
             card(
               card_header("Community Metrics"),
               div(style = "display: flex; flex-direction: column; gap: 10px;",
                   div(class = "d-flex justify-content-between",
                       span("Number of Communities:"), 
                       span(strong(length(unique(comm))), class = "text-primary")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Modularity:"), 
                       span(strong(round(mod, 3)), class = "text-info")
                   )
               )
             )
      ),
      column(4,
             card(
               card_header(paste(seg_col, "Metrics")),
               div(style = "display: flex; flex-direction: column; gap: 10px;",
                   div(class = "d-flex justify-content-between",
                       span("Number of Segments:"), 
                       span(strong(length(unique(segments))), class = "text-primary")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Largest Segment Size:"), 
                       span(strong(max(table(segments))), class = "text-success")
                   ),
                   div(class = "d-flex justify-content-between",
                       span("Smallest Segment Size:"), 
                       span(strong(min(table(segments))), class = "text-warning")
                   )
               )
             )
      )
    )
  })
  
  # Segment distribution
  output$segment_dist <- renderPlot({
    kg <- kg_reactive()
    seg_col <- input$seg_logic
    segments <- kg$nodes[[seg_col]]
    
    seg_counts <- table(segments)
    seg_df <- data.frame(
      segment = names(seg_counts),
      count = as.numeric(seg_counts)
    )
    
    ggplot(seg_df, aes(x = reorder(segment, count), y = count, fill = segment)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Distribution of", seg_col),
           x = seg_col, y = "Count") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid = element_blank() )  })
  
  # Centrality distribution
  output$centrality_dist <- renderPlot({
    kg <- kg_reactive()
    nodes <- kg$nodes
    
    # Reshape data for plotting
    centrality_df <- nodes %>%
      select(id, degree, betweenness, eigen_centrality, closeness) %>%
      pivot_longer(cols = c(degree, betweenness, eigen_centrality, closeness),
                   names_to = "metric", values_to = "value")
    
    ggplot(centrality_df, aes(x = value, fill = metric)) +
      geom_density(alpha = 0.5) +
      scale_x_continuous(n.break = 8) + 
      scale_y_continuous(n.break = 8) +
      facet_wrap(~metric, scales = "free") +
      labs(title = "Centrality Measures Distribution",
           x = "Value", y = "Density", fill = "Metric") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid = element_blank(),
            title = element_text(face = "bold.italic", color = "blue3"),
            axis.line = element_line(linewidth = 0.5, linetype = "solid", color = "#373737"),
            strip.background = element_rect(linetype = "solid", linewidth = 0.5, color = '#373737', fill = "white")
      )
  })
  
  # ---- Influence Map ---- ####################################################
  output$influence_plot <- renderPlot({
    kg <- kg_reactive()
    edges2 <- kg$edges
    nodes2 <- kg$nodes
    
    edges2$from_community <- nodes2$community[match(edges2$from, nodes2$id)]
    edges2$to_community <- nodes2$community[match(edges2$to, nodes2$id)]
    
    infl_map <- edges2 %>%
      count(from_community, to_community) %>%
      mutate(from_community = factor(from_community), to_community = factor(to_community))
    
    ggplot(infl_map, aes(x = from_community, y = to_community, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Segment Influence Map", x = "From Segment", y = "To Segment", fill = "Edge Count") +
      theme_minimal() +
      theme(panel.border = element_rect(color = "#474747", fill = NA, linewidth = 0.5),
            panel.grid = element_blank())
  })
  
  output$influence_table <- renderDT({
    kg <- kg_reactive()
    edges2 <- kg$edges
    nodes2 <- kg$nodes
    
    edges2$from_community <- nodes2$community[match(edges2$from, nodes2$id)]
    edges2$to_community <- nodes2$community[match(edges2$to, nodes2$id)]
    
    datatable(edges2 %>% count(from_community, to_community), rownames = FALSE)
  })
  
  # Influence metrics
  output$influence_metrics <- renderUI({
    kg <- kg_reactive()
    edges2 <- kg$edges
    nodes2 <- kg$nodes
    
    edges2$from_community <- nodes2$community[match(edges2$from, nodes2$id)]
    edges2$to_community <- nodes2$community[match(edges2$to, nodes2$id)]
    
    # Calculate influence metrics
    infl_map <- edges2 %>%
      count(from_community, to_community) %>%
      ungroup()
    
    # Outgoing influence
    out_influence <- infl_map %>%
      group_by(from_community) %>%
      summarise(outgoing = sum(n), outgoing_targets = n_distinct(to_community))
    
    # Incoming influence
    in_influence <- infl_map %>%
      group_by(to_community) %>%
      summarise(incoming = sum(n), incoming_sources = n_distinct(from_community))
    
    # Combined metrics
    influence <- out_influence %>%
      left_join(in_influence, by = c("from_community" = "to_community")) %>%
      rename(community = from_community) %>%
      replace_na(list(incoming = 0, incoming_sources = 0)) %>%
      mutate(total_influence = outgoing + incoming,
             influence_ratio = ifelse(incoming > 0, outgoing / incoming, outgoing))
    
    # Identify most influential segments
    most_outgoing <- influence[which.max(influence$outgoing), "community"]
    most_incoming <- influence[which.max(influence$incoming), "community"]
    most_balanced <- influence[which.min(abs(influence$influence_ratio - 1)), "community"]
    
    fluidRow(
      # Key Insights Card
      column(12,
             card(
               header = "Key Insights",
               div(style = "display: flex; justify-content: space-between;",
                   div(class = "text-center",
                       h5("Most Influential (Outgoing)"),
                       h4(class = "text-primary", paste("Community", most_outgoing))
                   ),
                   div(class = "text-center",
                       h5("Most Influenced (Incoming)"),
                       h4(class = "text-info", paste("Community", most_incoming))
                   ),
                   div(class = "text-center",
                       h5("Most Balanced Influence"),
                       h4(class = "text-success", paste("Community", most_balanced))
                   )
               )
             )
      ),
      
      # Community Influence Metrics Table
      lapply(1:nrow(influence), function(i) {
        column(4,
               card(
                 header = paste("Community", influence$community[i]),
                 div(style = "display: flex; flex-direction: column; gap: 10px;",
                     div(class = "d-flex justify-content-between",
                         span("Outgoing Connections:"), 
                         span(strong(paste(influence$outgoing[i], "to", influence$outgoing_targets[i], "segments")), class = "text-primary")
                     ),
                     div(class = "d-flex justify-content-between",
                         span("Incoming Connections:"), 
                         span(strong(paste(influence$incoming[i], "from", influence$incoming_sources[i], "segments")), class = "text-info")
                     ),
                     div(class = "d-flex justify-content-between",
                         span("Total Influence:"), 
                         span(strong(influence$total_influence[i]), class = "text-success")
                     ),
                     div(class = "d-flex justify-content-between",
                         span("Influence Ratio:"), 
                         span(strong(round(influence$influence_ratio[i], 2)), class = "text-warning")
                     )
                 )
               )
        )
      })
    )
  })
  
  # ---- Bridge Recommendations ---- ###########################################
  output$bridges_table <- renderDT({
    kg <- kg_reactive()
    nodes2 <- kg$nodes
    edges2 <- kg$edges
    
    # All pairs not already connected, different community
    node_pairs <- expand_grid(
      from = nodes2$id, to = nodes2$id
    ) %>%
      filter(from != to) %>%
      left_join(nodes2 %>% select(id, from_comm = community, from_label = label, from_deg = degree), by = c("from" = "id")) %>%
      left_join(nodes2 %>% select(id, to_comm = community, to_label = label, to_deg = degree), by = c("to" = "id")) %>%
      filter(from_comm != to_comm)
    
    # Remove existing edges
    existing <- paste(edges2$from, edges2$to, sep = "-")
    node_pairs <- node_pairs %>%
      filter(!(paste(from, to, sep = "-") %in% existing))
    
    # Score: combined degree
    node_pairs <- node_pairs %>%
      mutate(bridge_score = from_deg + to_deg) %>%
      arrange(desc(bridge_score)) %>%
      slice_head(n = 10)
    
    datatable(node_pairs[, c("from_label", "to_label", "from_comm", "to_comm", "bridge_score")], rownames = FALSE)
  })
  
  # Bridge impact visualization
  output$bridge_impact <- renderPlot({
    kg <- kg_reactive()
    nodes2 <- kg$nodes
    edges2 <- kg$edges
    
    # Get top bridge recommendations
    node_pairs <- expand_grid(
      from = nodes2$id, to = nodes2$id
    ) %>%
      filter(from != to) %>%
      left_join(nodes2 %>% select(id, from_comm = community, from_label = label, from_deg = degree), by = c("from" = "id")) %>%
      left_join(nodes2 %>% select(id, to_comm = community, to_label = label, to_deg = degree), by = c("to" = "id")) %>%
      filter(from_comm != to_comm)
    
    existing <- paste(edges2$from, edges2$to, sep = "-")
    node_pairs <- node_pairs %>%
      filter(!(paste(from, to, sep = "-") %in% existing))
    
    node_pairs <- node_pairs %>%
      mutate(bridge_score = from_deg + to_deg) %>%
      arrange(desc(bridge_score)) %>%
      slice_head(n = 20)
    
    # Create a plot of bridge scores
    ggplot(node_pairs, aes(x = reorder(paste(from_label, "-", to_label), bridge_score), y = bridge_score)) +
      geom_col(fill = "#6366f1") +
      coord_cartesian(expand = FALSE) +
      coord_flip() +
      labs(title = "Top 20 Potential Bridges by Score",
           x = "Potential Bridge", y = "Bridge Score") +
      theme_minimal() + theme(panel.grid = element_blank())
  })
  
  # Bridge metrics
  output$bridge_metrics <- renderUI({
    kg <- kg_reactive()
    nodes2 <- kg$nodes
    edges2 <- kg$edges
    
    # --- Data Preparation (Core logic remains the same) ---
    
    # Get bridge recommendations
    node_pairs <- expand_grid(
      from = nodes2$id, to = nodes2$id
    ) %>%
      filter(from != to) %>%
      left_join(nodes2 %>% select(id, from_comm = community, from_label = label, from_deg = degree), by = c("from" = "id")) %>%
      left_join(nodes2 %>% select(id, to_comm = community, to_label = label, to_deg = degree), by = c("to" = "id")) %>%
      filter(from_comm != to_comm)
    
    existing <- paste(edges2$from, edges2$to, sep = "-")
    node_pairs <- node_pairs %>%
      filter(!(paste(from, to, sep = "-") %in% existing)) %>%
      mutate(bridge_score = from_deg + to_deg)
    
    # Analyze bridge potential by community
    bridge_by_comm <- node_pairs %>%
      group_by(from_comm, to_comm) %>%
      summarise(
        avg_score = mean(bridge_score),
        max_score = max(bridge_score),
        potential_bridges = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_score))
    
    # Calculate potential network improvement
    current_density <- edge_density(kg$g_ig)
    potential_density <- (ecount(kg$g_ig) + nrow(node_pairs)) / (vcount(kg$g_ig) * (vcount(kg$g_ig) - 1))
    density_improvement <- (potential_density - current_density) / current_density * 100
    
    # --- UI Rendering ---
    card(
      header = h4("Bridge Analysis"),
      
      # Top row: Key metrics as value boxes
      layout_column_wrap(
        value_box(
          nrow(node_pairs), 
          "Potential Bridges", 
          icon = icon("link"), 
          theme = "primary"
        ),
        value_box(
          round(mean(node_pairs$bridge_score), 2), 
          "Avg Bridge Score", 
          icon = icon("chart-line"), 
          theme = "info"
        ),
        value_box(
          max(node_pairs$bridge_score), 
          "Highest Bridge Score", 
          icon = icon("award"), 
          theme = "success"
        )
      ),
      
      hr(),
      
      # Second row: Top community pairs and network impact
      layout_columns(
        col_widths = c(7, 5),
        
        # Card 1: Top Community Pairs
        card(
          header = h5(span(icon("sitemap"), class="text-primary"), " Top Community Pairs for Bridging"),
          div(class = "table-responsive",
              tags$table(class = "table table-hover",
                    tags$thead(
                      tags$tr(
                        tags$th("Community Pair"),
                        tags$th("Avg Score"),
                        tags$th("Potential Bridges")
                      )
                    ),
                    tags$tbody(
                      lapply(1:min(5, nrow(bridge_by_comm)), function(i) {
                        tags$tr(
                          tags$td(paste("Community", bridge_by_comm$from_comm[i], "→", "Community", bridge_by_comm$to_comm[i])),
                          tags$td(span(class = "badge bg-info", round(bridge_by_comm$avg_score[i], 2))),
                          tags$td(bridge_by_comm$potential_bridges[i])
                        )
                      })
                    )
              )
          )
        ),
        
        # Card 2: Network Impact
        card(
          header = h5(span(icon("chart-area"), class="text-success"), " Network Impact"),
          div(style = "display: flex; flex-direction: column; gap: 15px;",
              div(class = "d-flex justify-content-between align-items-center",
                  span("Current Density:"),
                  strong(round(current_density, 4))
              ),
              div(class = "d-flex justify-content-between align-items-center",
                  span("Potential Density:"),
                  strong(round(potential_density, 4))
              ),
              div(class = "alert alert-success text-center", role = "alert",
                  h6(class = "alert-heading", "Potential Improvement"),
                  p(class = "display-6", paste0("+", round(density_improvement, 1), "%"))
              )
          )
        )
      )
    )
  })
  
  # Add bridge application functionality
  observeEvent(input$apply_bridges, {
    # This would apply selected bridges to the network
    showNotification("Bridge application feature would be implemented here", type = "message")
  })
  
  observeEvent(input$refresh_bridges, {
    # This would refresh the bridge recommendations
    showNotification("Bridge recommendations refreshed", type = "message")
  })
  
 # output$link_recs <- output$bridges_table  # Alias for consistency with UI
  
  # ---- Personal Dashboard ---- ###############################################
  output$personal_dash <- renderUI({
    req(input$person_sel)
    
    kg <- kg_reactive()
    name <- input$person_sel
    person <- kg$nodes %>% filter(label == name)
    
    # --- Data Preparation ---
    
    # Direct connections
    neighbors_ids <- neighbors(kg$g_ig, person$id[[1]], mode = "all")
    neighbor_labels <- kg$nodes$label[neighbors_ids]
    
    # Bridge recommendations
    node_pairs <- expand_grid(
      from = kg$nodes$id, to = kg$nodes$id
    ) %>%
      filter(from != to) %>%
      left_join(kg$nodes %>% select(id, from_comm = community, from_label = label, from_deg = degree), by = c("from" = "id")) %>%
      left_join(kg$nodes %>% select(id, to_comm = community, to_label = label, to_deg = degree), by = c("to" = "id")) %>%
      filter(from_comm != to_comm)
    
    existing <- paste(kg$edges$from, kg$edges$to, sep = "-")
    bridge_recs <- node_pairs %>%
      filter(!(paste(from, to, sep = "-") %in% existing)) %>%
      mutate(bridge_score = from_deg + to_deg) %>%
      arrange(desc(bridge_score)) %>%
      filter(from_label == name | to_label == name) %>%
      head(5) # Show top 5 recommendations
    
    # --- UI Rendering ---
    card(
      card_header(h4(strong("Profile for: "), span(name, class = "text-primary"))),
      
      # Top row: Profile Summary and Direct Connections
      layout_columns(
        col_widths = c(6, 6),
        
        # Card 1: Profile Summary
        card(
          card_header(h5(span(icon("user-circle"), class="text-info"), " Profile Summary")),
          div(style = "display: flex; flex-direction: column; gap: 10px;",
              div(class = "d-flex justify-content-between",
                  span("Community:"), 
                  span(class = "badge bg-primary rounded-pill", person$community[[1]])
              ),
              div(class = "d-flex justify-content-between",
                  span("Degree:"), 
                  strong(person$degree[[1]])
              ),
              div(class = "d-flex justify-content-between",
                  span("Role Segment:"), 
                  span(class = "badge bg-success rounded-pill", person$role_segment[[1]])
              ),
              div(class = "d-flex justify-content-between",
                  span("Dynamic Segment:"), 
                  span(class = "badge bg-info text-dark rounded-pill", person$dynamic_segment[[1]])
              )
          )
        ),
        
        # Card 2: Direct Connections
        card(
          card_header(h5(span(icon("link"), class="text-primary"), " Direct Connections")),
          if (length(neighbor_labels) > 0) {
            tags$ul(class = "list-group list-group-flush",
               lapply(neighbor_labels, function(label) {
                 tags$li(class = "list-group-item", label)
               })
            )
          } else {
            div(class = "text-muted p-3", "This person has no direct connections.")
          }
        )
      ),
      hr(),
      # Bottom row: Bridge Recommendations (Conditional)
      if (nrow(bridge_recs) > 0) {
        card(
          card_header(h5(span(icon("project-diagram"), class="text-warning"), " Bridge Recommendations")),
          p("Connecting with these individuals can bridge different communities:", class="text-muted"),
          div(DT::datatable(
            bridge_recs %>%
              mutate(
                `Connect With` = ifelse(from_label == name, to_label, from_label),
                `Target Community` = ifelse(from_label == name, to_comm, from_comm)
              ) %>%
              select(`Connect With`, `Target Community`, `Bridge Score` = bridge_score),
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'tip'
            ),
            rownames = FALSE,
            selection = 'none',
            class = 'cell-border stripe hover'
          ))
        )
      }
    )
  })
  
  # Personal metrics
  output$personal_metrics <- renderUI({
    req(input$analyze_person)
    
    kg <- kg_reactive()
    name <- input$person_sel
    person <- kg$nodes %>% filter(label == name)
    
    # Extract single values from the data frame row
    p_type <- person$type[[1]]
    p_role <- person$role[[1]]
    p_dept <- person$department[[1]]
    p_perf <- person$performance_score[[1]]
    p_community <- person$community[[1]]
    p_role_seg <- person$role_segment[[1]]
    p_dyn_seg <- person$dynamic_segment[[1]]
    p_degree <- person$degree[[1]]
    p_betweenness <- person$betweenness[[1]]
    p_eigen <- person$eigen_centrality[[1]]
    p_closeness <- person$closeness[[1]]
    
    # Calculate percentile rankings
    all_nodes <- kg$nodes
    degree_rank <- sum(all_nodes$degree < p_degree) / length(all_nodes$degree) * 100
    betweenness_rank <- sum(all_nodes$betweenness < p_betweenness) / length(all_nodes$betweenness) * 100
    eigen_rank <- sum(all_nodes$eigen_centrality < p_eigen) / length(all_nodes$eigen_centrality) * 100
    
    tagList(
      # Prominent Header with the person's name
      div(class = "text-center mb-4",
          h3(style = "color: #0d6efd;", paste("Profile for:", name))
      ),
      
      # Two-column layout for Basic Info and Network Metrics
      layout_columns(
        col_widths = c(6, 6),
        
        # Card 1: Basic Information
        card(
          header = "Basic Information",
          div(style = "display: flex; flex-direction: column; gap: 10px;",
              div(class = "d-flex justify-content-between",
                  span("Type:"), 
                  span(strong(p_type), class = "text-primary")
              ),
              div(class = "d-flex justify-content-between",
                  span("Role:"), 
                  span(strong(p_role), class = "text-info")
              ),
              div(class = "d-flex justify-content-between",
                  span("Department:"), 
                  span(strong(p_dept), class = "text-success")
              ),
              div(class = "d-flex justify-content-between",
                  span("Performance Score:"), 
                  span(strong(p_perf), class = "text-warning")
              )
          )
        ),
        
        # Card 2: Network Metrics
        card(
          header = "Network Metrics",
          div(style = "display: flex; flex-direction: column; gap: 10px;",
              div(class = "d-flex justify-content-between",
                  span("Community:"), 
                  span(strong(p_community), class = "badge bg-primary")
              ),
              div(class = "d-flex justify-content-between",
                  span("Role Segment:"), 
                  span(strong(p_role_seg), class = "badge bg-info")
              ),
              div(class = "d-flex justify-content-between",
                  span("Dynamic Segment:"), 
                  span(strong(p_dyn_seg), class = "badge bg-success")
              ),
              div(class = "d-flex justify-content-between",
                  span("Degree:"), 
                  span(strong(p_degree), class = "text-primary")
              ),
              div(class = "d-flex justify-content-between",
                  span("Betweenness:"), 
                  span(strong(round(p_betweenness, 2)), class = "text-info")
              ),
              div(class = "d-flex justify-content-between",
                  span("Eigenvector Centrality:"), 
                  span(strong(round(p_eigen, 3)), class = "text-success")
              ),
              div(class = "d-flex justify-content-between",
                  span("Closeness:"), 
                  span(strong(round(p_closeness, 3)), class = "text-warning")
              )
          )
        )
      ),
      
      # Full-width card for Percentile Rankings with Progress Bars =============
      card(
        header = "Network Percentile Rankings",
        p("Shows how this person's metrics compare to others in the network.", class="text-muted"),
        div(class = "mt-3",
            # Degree Progress Bar
            div(class = "mb-3",
                h6("Degree Centrality"),
                div(class = "progress",
                    div(style = paste0("width: ", degree_rank, "%;"),
                        class = "progress-bar bg-primary",
                        role = "progressbar",
                        `aria-valuenow` = degree_rank,
                        `aria-valuemin` = 0,
                        `aria-valuemax` = 100,
                        paste0("Rank: ", round(degree_rank, 1), "th percentile"))
                )
            ),
            # Betweenness Progress Bar =========================================
            div(class = "mb-3",
                h6("Betweenness Centrality"),
                div(class = "progress",
                    div(style = paste0("width: ", betweenness_rank, "%;"),
                        class = "progress-bar bg-info",
                        role = "progressbar",
                        `aria-valuenow` = betweenness_rank,
                        `aria-valuemin` = 0,
                        `aria-valuemax` = 100,
                        paste0("Rank: ", round(betweenness_rank, 1), "th percentile"))
                )
            ),
            # Eigenvector Progress Bar
            div(class = "mb-3",
                h6("Eigenvector Centrality"),
                div(class = "progress",
                    div(style = paste0("width: ", eigen_rank, "%;"),
                        class = "progress-bar bg-success",
                        role = "progressbar",
                        `aria-valuenow` = eigen_rank,
                        `aria-valuemin` = 0,
                        `aria-valuemax` = 100,
                        paste0("Rank: ", round(eigen_rank, 1), "th percentile"))
                )
            )
        )
      )
    )
  })
  
  # Network position visualization
  output$network_position <- renderPlot({
    req(input$analyze_person)
    kg <- kg_reactive()
    name <- input$person_sel
    person <- kg$nodes %>% filter(label == name)
    
    # Create a radar chart of the person's metrics
    metrics_df <- data.frame(
      metric = c("Degree", "Betweenness", "Eigenvector", "Closeness"),
      value = c(
        person$degree / max(kg$nodes$degree),
        person$betweenness / max(kg$nodes$betweenness),
        person$eigen_centrality / max(kg$nodes$eigen_centrality),
        person$closeness / max(kg$nodes$closeness)
      )
    )
    
    # Add average values for comparison
    avg_metrics <- data.frame(
      metric = c("Degree", "Betweenness", "Eigenvector", "Closeness"),
      value = c(
        mean(kg$nodes$degree) / max(kg$nodes$degree),
        mean(kg$nodes$betweenness) / max(kg$nodes$betweenness),
        mean(kg$nodes$eigen_centrality) / max(kg$nodes$eigen_centrality),
        mean(kg$nodes$closeness) / max(kg$nodes$closeness)
      ),
      type = "Average"
    )
    
    metrics_df$type <- name
    combined_df <- rbind(metrics_df, avg_metrics)
    
    ggplot(combined_df, aes(x = metric, y = value, fill = type, group = type)) +
      geom_polygon(alpha = 0.3, color = "black") +
      geom_point(size = 3) +
      scale_fill_manual(values = c(name = "#6366f1", "Average" = "#8b5cf6")) +
      coord_polar() +
      labs(title = "Network Position Radar Chart", x = "", y = "Normalized Value") +
      theme_minimal()
  })
  
  # Personal network visualization
  output$personal_network <- renderPlot({
    req(input$analyze_person)
    kg <- kg_reactive()
    name <- input$person_sel
    person <- kg$nodes %>% filter(label == name)
    depth <- input$connection_depth
    
    # Get ego network
    ego_net <- make_ego_graph(kg$g_ig, order = depth, nodes = person$id, mode = "all")
    if (length(ego_net) > 0) {
      ego_net <- ego_net[[1]]
      
      # Create a subgraph with the ego network
      ego_nodes <- kg$nodes[kg$nodes$id %in% V(ego_net)$name, ]
      ego_edges <- kg$edges[kg$edges$from %in% V(ego_net)$name & kg$edges$to %in% V(ego_net)$name, ]
      
      ego_graph <- tbl_graph(nodes = ego_nodes, edges = ego_edges, directed = TRUE)
      
      # Highlight the central person
      ego_nodes$is_center <- ego_nodes$id == person$id
      
      ggraph(ego_graph, layout = "fr") +
        geom_edge_link(aes(width = strength), alpha = 0.5) +
        geom_node_point(aes(color = is_center, size = degree)) +
        geom_node_text(aes(label = label), repel = TRUE, size = 3) +
        scale_color_manual(values = c("FALSE" = "#8b5cf6", "TRUE" = "#ef4444")) +
        theme_void() +
        ggtitle(paste("Ego Network for", name, "(Depth:", depth, ")"))
    } else {
      ggplot() + geom_blank() + theme_void() + 
        ggtitle(paste("No connections found for", name))
    }
  })
  
  # Personal recommendations
  output$personal_recommendations <- renderUI({
    req(input$analyze_person)
    
    kg <- kg_reactive()
    name <- input$person_sel
    person <- kg$nodes %>% filter(label == name)
    
    # Extract single values from the data frame row
    p_degree <- person$degree[[1]]
    p_betweenness <- person$betweenness[[1]]
    p_community <- person$community[[1]]
    
    # --- Calculate Recommendations ---
    
    # 1. Career development recommendations
    degree_quantile <- ecdf(kg$nodes$degree)(p_degree)
    if (degree_quantile < 0.25) {
      network_rec <- tagList(
        strong("NETWORK EXPANSION: ", style = "color: #dc3545;"),
        span("Your degree centrality is in the bottom 25%. Consider connecting with more colleagues across departments.")
      )
    } else if (degree_quantile >= 0.75) {
      network_rec <- tagList(
        strong("NETWORK OPTIMIZATION: ", style = "color: #198754;"),
        span("You're highly connected (top 25%). Focus on maintaining quality relationships rather than just quantity.")
      )
    } else {
      network_rec <- tagList(
        strong("NETWORK GROWTH: ", style = "color: #0dcaf0;"),
        span("You have a moderate number of connections. Consider strategic connections to key influencers in other segments.")
      )
    }
    
    # 2. Bridge potential recommendations
    betweenness_quantile <- ecdf(kg$nodes$betweenness)(p_betweenness)
    if (betweenness_quantile >= 0.75) {
      bridge_rec <- tagList(
        strong("BRIDGE LEVERAGE: ", style = "color: #198754;"),
        span("You're a key bridge between different parts of the network. Leverage this position to facilitate knowledge flow.")
      )
    } else if (betweenness_quantile < 0.25) {
      bridge_rec <- tagList(
        strong("BRIDGE BUILDING: ", style = "color: #ffc107;"),
        span("Consider connecting with people outside your immediate community to increase your betweenness centrality.")
      )
    } else {
      bridge_rec <- NULL # No recommendation if in the middle
    }
    
    # 3. Community-specific recommendations
    comm_nodes <- kg$nodes %>% filter(community == p_community)
    if (nrow(comm_nodes) > 5) {
      comm_rec <- tagList(
        strong("COMMUNITY ENGAGEMENT: ", style = "color: #0d6efd;"),
        span("Your community has ", nrow(comm_nodes), " members. Consider organizing community-specific events or initiatives.")
      )
    } else {
      comm_rec <- tagList(
        strong("COMMUNITY GROWTH: ", style = "color: #6f42c1;"),
        span("Your community is relatively small. Consider inviting others with similar interests to join.")
      )
    }
    
    # 4. Top bridge recommendations
    node_pairs <- expand_grid(
      from = kg$nodes$id, to = kg$nodes$id
    ) %>%
      filter(from != to) %>%
      left_join(kg$nodes %>% select(id, from_comm = community, from_label = label, from_deg = degree), by = c("from" = "id")) %>%
      left_join(kg$nodes %>% select(id, to_comm = community, to_label = label, to_deg = degree), by = c("to" = "id")) %>%
      filter(from_comm != to_comm)
    
    existing <- paste(kg$edges$from, kg$edges$to, sep = "-")
    strategic_connections <- node_pairs %>%
      filter(!(paste(from, to, sep = "-") %in% existing)) %>%
      mutate(bridge_score = from_deg + to_deg) %>%
      arrange(desc(bridge_score)) %>%
      filter(from_label == name | to_label == name) %>%
      head(3)
    
    # --- Render the UI ---
    card(
      card_header(h4(strong("Personalized Recommendations for: "), span(name, class = "text-primary"))),
      
      # Use a layout to organize the recommendations
      layout_column_wrap(
        # Recommendation 1: Network
        div(class = "alert alert-info", role = "alert",
            h5(class = "alert-heading", "1. Network Strategy"),
            p(network_rec)
        ),
        
        # Recommendation 2: Bridge (only show if it exists)
        if (!is.null(bridge_rec)) {
          div(class = "alert alert-success", role = "alert",
              h5(class = "alert-heading", "2. Bridge Potential"),
              p(bridge_rec)
          )
        },
        
        # Recommendation 3: Community
        div(class = "alert alert-primary", role = "alert",
            h5(class = "alert-heading", "3. Community Focus"),
            p(comm_rec)
        ),
        
        # Recommendation 4: Strategic Connections
        if (nrow(strategic_connections) > 0) {
          div(class = "alert alert-warning", role = "alert",
              h5(class = "alert-heading", "4. Strategic Connections"),
              p("Consider connecting with these people to bridge communities:"),
              tags$ul(
                lapply(1:nrow(strategic_connections), function(i) {
                  row <- strategic_connections[i, ]
                  if (row$from_label == name) {
                    tags$li(paste("Connect with", strong(row$to_label), "(Bridge Score:", row$bridge_score, ")"))
                  } else {
                    tags$li(paste("Connect with", strong(row$from_label), "(Bridge Score:", row$bridge_score, ")"))
                  }
                })
              )
          )
        }
      )
    )
  })
  
  #output$person_dash <- output$personal_dash  # Alias for consistency with UI
  
  # ---- Segment Evolution ---- ################################################
  
  # Enhanced segment evolution plot
  output$evo_plot <- renderPlot({
    kg <- kg_reactive()
    seg_type <- input$evo_segment_type
    time_range <- input$time_range
    
    # Filter edges by time range
    edges_filtered <- edges %>%
      filter(timestamp >= time_range[1] & timestamp <= time_range[2])
    
    # Group by month
    edges_filtered$month <- floor_date(edges_filtered$timestamp, "month")
    months <- unique(edges_filtered$month)
    months <- sort(months)
    
    # Create a list to store plots for each month
    plots <- list()
    
    for (i in 1:length(months)) {
      mo <- months[i]
      edge_now <- edges_filtered %>% filter(month <= mo)
      
      if (nrow(edge_now) > 0) {
        g_now <- tbl_graph(nodes, edge_now, directed = TRUE)
        
        # Calculate segments based on selected type
        if (seg_type == "community") {
          comm_now <- as.integer(membership(cluster_louvain(as.igraph(g_now) %>% as_undirected())))
          nodes$comm_now <- comm_now
          color_var <- "comm_now"
        } else if (seg_type == "kmeans_cluster") {
          # Recalculate kmeans for this time period
          g_ig_now <- as.igraph(g_now)
          nodes$degree_now <- degree(g_ig_now)
          nodes$betweenness_now <- betweenness(g_ig_now)
          nodes$eigen_centrality_now <- eigen_centrality(g_ig_now)$vector
          nodes$closeness_now <- closeness(g_ig_now)
          nodes$pagerank <- page_rank(g_ig, weights = E(g_ig)$strength)$vector
          
          feats_now <- nodes %>% select(degree_now, betweenness_now, eigen_centrality_now, closeness_now)
          set.seed(123)
          nodes$kmeans_now <- as.factor(cluster::pam(scale(feats_now, na.rm = TRUE), k = 3, cluster.only = TRUE))
          color_var <- "kmeans_now"
        } else {
          # Role segment
          g_ig_now <- as.igraph(g_now)
          nodes$degree_now <- degree(g_ig_now)
          nodes$betweenness_now <- betweenness(g_ig_now)
          
          nodes$role_now <- case_when(
            nodes$degree_now >= quantile(nodes$degree_now, 0.75, na.rm = TRUE) ~ "Influencer",
            nodes$degree_now == 0 ~ "Isolated",
            nodes$betweenness_now >= quantile(nodes$betweenness_now, 0.9, na.rm = TRUE) ~ "Bridge",
            TRUE ~ "Regular"
          )
          color_var <- "role_now"
        }
        
        p <- ggraph(g_now, layout = "fr") +
          geom_edge_link(alpha = 0.5) +
          geom_node_point(aes_string(color = color_var), size = 7) +
          geom_node_text(aes(label = label), repel = TRUE, size = 4) +
          theme_void() +
          ggtitle(paste("Segments as of", format(mo, "%b %Y")))
        
        plots[[i]] <- p
      }
    }
    
    # Return the latest plot if animation is not playing
    if (length(plots) > 0) {
      return(plots[[length(plots)]])
    } else {
      return(ggplot() + geom_blank() + theme_void() + ggtitle("No data available for selected time range"))
    }
  })
  
  # Segment stability analysis
  output$stability_analysis <- renderUI({
    kg <- kg_reactive()
    seg_type <- input$evo_segment_type
    time_range <- input$time_range
    
    # Filter edges by time range
    edges_filtered <- edges %>%
      filter(timestamp >= time_range[1] & timestamp <= time_range[2])
    
    # Group by month
    edges_filtered$month <- floor_date(edges_filtered$timestamp, "month")
    months <- unique(edges_filtered$month)
    months <- sort(months)
    
    # --- Handle Edge Case ---
    if (length(months) < 2) {
      return(
        div(class = "alert alert-warning", role = "alert",
            h4(class = "alert-heading", "Insufficient Data"),
            p("Need at least 2 months of data to analyze stability. Please expand the time range.")
        )
      )
    }
    
    # --- Calculate Segments and Stability (Core Logic) ---
    segment_history <- data.frame(id = nodes$id, label = nodes$label)
    
    for (i in 1:length(months)) {
      mo <- months[i]
      edge_now <- edges_filtered %>% filter(month <= mo)
      
      if (nrow(edge_now) > 0) {
        g_now <- tbl_graph(nodes, edge_now, directed = TRUE)
        
        if (seg_type == "community") {
          comm_now <- as.integer(membership(cluster_louvain(as.igraph(g_now) %>% as_undirected())))
          segment_history[[paste0("month_", i)]] <- comm_now
        } else if (seg_type == "kmeans_cluster") {
          g_ig_now <- as.igraph(g_now)
          nodes$degree_now <- degree(g_ig_now)
          nodes$betweenness_now <- betweenness(g_ig_now)
          nodes$eigen_centrality_now <- eigen_centrality(g_ig_now)$vector
          nodes$closeness_now <- closeness(g_ig_now)
          nodes$pagerank <- page_rank(g_ig, weights = E(g_ig)$strength)$vector
          
          feats_now <- nodes %>% select(degree_now, betweenness_now, eigen_centrality_now, closeness_now)
          set.seed(123)
          nodes$kmeans_now <- as.factor(kmeans(scale(feats_now, na.rm = TRUE), centers = 3)$cluster)
          segment_history[[paste0("month_", i)]] <- nodes$kmeans_now
        } else { # Role segment
          g_ig_now <- as.igraph(g_now)
          nodes$degree_now <- degree(g_ig_now)
          nodes$betweenness_now <- betweenness(g_ig_now)
          
          nodes$role_now <- case_when(
            nodes$degree_now >= quantile(nodes$degree_now, 0.75, na.rm = TRUE) ~ "Influencer",
            nodes$degree_now == 0 ~ "Isolated",
            nodes$betweenness_now >= quantile(nodes$betweenness_now, 0.9, na.rm = TRUE) ~ "Bridge",
            TRUE ~ "Regular"
          )
          segment_history[[paste0("month_", i)]] <- nodes$role_now
        }
      }
    }
    
    # Calculate stability metrics
    month_cols <- paste0("month_", 1:length(months))
    stability_scores <- c()
    for (i in 2:length(month_cols)) {
      same_segment <- sum(segment_history[[month_cols[i]]] == segment_history[[month_cols[i-1]]], na.rm = TRUE)
      total_nodes <- sum(!is.na(segment_history[[month_cols[i]]]) & !is.na(segment_history[[month_cols[i-1]]]))
      if (total_nodes > 0) {
        stability <- same_segment / total_nodes * 100
        stability_scores <- c(stability_scores, stability)
      }
    }
    
    avg_stability <- if(length(stability_scores) > 0) mean(stability_scores) else 0
    
    # Identify most stable and least stable nodes
    most_stable <- data.frame()
    least_stable <- data.frame()
    if (length(month_cols) >= 3 && length(stability_scores) > 0) {
      changes <- rep(0, nrow(segment_history))
      for (i in 2:length(month_cols)) {
        changes <- changes + (segment_history[[month_cols[i]]] != segment_history[[month_cols[i-1]]])
      }
      
      segment_history$changes <- changes
      most_stable <- segment_history %>% arrange(changes) %>% slice_head(n = 3)
      least_stable <- segment_history %>% arrange(desc(changes)) %>% slice_head(n = 3)
    }
    
    # --- Render the UI ---
    card(
      card_heade(h4("Segment Stability Analysis")),
      
      # Summary Section with Value Boxes
      layout_column_wrap(
        value_box(
          paste("Type:", seg_type),
          "Segment Type",
          icon = icon("tags"),
          theme = "primary"
        ),
        value_box(
          length(months),
          "Months Analyzed",
          icon = icon("calendar-alt"),
          theme = "info"
        ),
        value_box(
          paste0(round(avg_stability, 1), "%"),
          "Average Stability",
          icon = icon("chart-line"),
          theme = if(avg_stability > 70) "success" else if(avg_stability > 40) "warning" else "danger"
        )
      ),
      
      hr(),
      
      # Monthly Stability Progress Bars
      div(h5("Monthly Stability Scores"), class="mb-3"),
      div(class = "mb-4",
          lapply(1:length(stability_scores), function(i) {
            div(class = "mb-2",
                div(style = "display: flex; justify-content: space-between;",
                    span(format(months[i+1], "%Y-%m")),
                    span(paste0(round(stability_scores[i], 1), "%"))
                ),
                div(class = "progress",
                    div(style = paste0("width: ", stability_scores[i], "%;"),
                        class = "progress-bar",
                        role = "progressbar",
                        `aria-valuenow` = stability_scores[i],
                        `aria-valuemin` = 0,
                        `aria-valuemax` = 100)
                )
            )
          })
      ),
      
      # Most and Least Stable Nodes
      if (nrow(most_stable) > 0 && nrow(least_stable) > 0) {
        layout_columns(
          col_widths = c(6, 6),
          card(
            header = span(icon("thumbs-up", class="text-success"), " Most Stable Nodes"),
            tags$ul(class = "list-group list-group-flush",
               lapply(1:nrow(most_stable), function(i) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    most_stable$label[i],
                    span(class = "badge bg-success rounded-pill", paste("Changes:", most_stable$changes[i]))
                 )
               })
            )
          ),
          card(
            header = span(icon("thumbs-down", class="text-danger"), " Least Stable Nodes"),
            tags$ul(class = "list-group list-group-flush",
               lapply(1:nrow(least_stable), function(i) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    least_stable$label[i],
                    span(class = "badge bg-danger rounded-pill", paste("Changes:", least_stable$changes[i]))
                 )
               })
            )
          )
        )
      }
    )
  })
  
  # Animation controls
  observeEvent(input$play_animation, {
    # This would trigger the animation
    showNotification("Animation feature would be implemented here", type = "message")
  })
  
  observeEvent(input$pause_animation, {
    # This would pause the animation
    showNotification("Animation paused", type = "message")
  })

  
  # ---- Segment Drilldown/Export ---- #########################################
  output$seg_drill_table <- renderDT({
    kg <- kg_reactive()
    seg <- as.numeric(input$seg_id)
    datatable(kg$nodes %>% filter(community == seg), rownames = FALSE)
  })
  
  output$drill_table <- renderDT({
    kg <- kg_reactive()
    seg <- as.numeric(input$drill_segment)
    datatable(kg$nodes %>% filter(community == seg), rownames = FALSE)
  })
  
  output$export_seg <- downloadHandler(
    filename = function() paste0("segment_", input$seg_id, ".csv"),
    content = function(file) {
      kg <- kg_reactive()
      seg <- as.numeric(input$seg_id)
      write.csv(kg$nodes %>% filter(community == seg), file, row.names = FALSE)
    }
  )
  
  output$dl_segment <- downloadHandler(
    filename = function() paste0("segment_", input$drill_segment, ".csv"),
    content = function(file) {
      kg <- kg_reactive()
      seg <- as.numeric(input$drill_segment)
      write.csv(kg$nodes %>% filter(community == seg), file, row.names = FALSE)
    }
  )
  
  # Segment summary
  output$segment_summary <- renderUI({
    req(input$analyze_segment)
    kg <- kg_reactive()
    seg_id <- as.numeric(input$seg_id)
    
    segment_nodes <- kg$nodes %>% filter(community == seg_id)
    segment_edges <- kg$edges %>%
      filter(from %in% segment_nodes$id & to %in% segment_nodes$id)
    
    # Create subgraph
    segment_graph <- tbl_graph(nodes = segment_nodes, edges = segment_edges, directed = TRUE)
    segment_ig <- as.igraph(segment_graph)
    
    # --- Prepare data for UI ---
    type_counts <- if ("type" %in% colnames(segment_nodes)) table(segment_nodes$type) else NULL
    dept_counts <- if ("department" %in% colnames(segment_nodes)) table(segment_nodes$department[!is.na(segment_nodes$department)]) else NULL
    role_counts <- if ("role_segment" %in% colnames(segment_nodes)) table(segment_nodes$role_segment) else NULL
    
    # --- Render the UI ---
    card(
      card_header(h4(strong("Segment "), span(seg_id, class = "text-primary"), " Summary")),
      
      # Top row: Key metrics as value boxes
      layout_column_wrap(
        value_box(
          "Nodes",
          nrow(segment_nodes), 
          icon = icon("users"), 
          theme = "primary"
        ),
        value_box(
          "Internal Edges",
          nrow(segment_edges), 
          icon = icon("project-diagram"), 
          theme = "info"
        ),
        value_box(
          "Density",
          round(edge_density(segment_ig), 3), 
          icon = icon("compress-arrows-alt"), 
          theme = "success"
        )
      ),
      
      hr(),
      
      # Second row: Graph structure and centrality metrics
      layout_columns(
        col_widths = c(6, 6),
        
        # Card 1: Graph Structure
        card(
          card_header("Graph Structure"),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
              if (vcount(segment_ig) > 1) {
                tagList(
                  div(class = "d-flex justify-content-between",
                      span("Average Path Length:"), 
                      strong(round(mean_distance(segment_ig), 2))
                  ),
                  div(class = "d-flex justify-content-between",
                      span("Diameter:"), 
                      strong(diameter(segment_ig))
                  ),
                  div(class = "d-flex justify-content-between",
                      span("Transitivity:"), 
                      strong(round(transitivity(segment_ig), 3))
                  )
                )
              } else {
                div(class = "text-muted", "Not enough nodes to calculate path metrics.")
              }
          )
        ),
        
        # Card 2: Centrality Metrics
        card(
          card_header("Centrality Metrics"),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
              div(class = "d-flex justify-content-between",
                  span("Avg Degree:"), 
                  strong(round(mean(segment_nodes$degree), 2))
              ),
              div(class = "d-flex justify-content-between",
                  span("Max Degree:"), 
                  strong(max(segment_nodes$degree))
              ),
              div(class = "d-flex justify-content-between",
                  span("Avg Betweenness:"), 
                  strong(round(mean(segment_nodes$betweenness), 2))
              ),
              div(class = "d-flex justify-content-between",
                  span("Max Betweenness:"), 
                  strong(max(segment_nodes$betweenness))
              ),
              div(class = "d-flex justify-content-between",
                  span("Avg Eigenvector:"), 
                  strong(round(mean(segment_nodes$eigen_centrality), 3))
              ),
              div(class = "d-flex justify-content-between",
                  span("Max Eigenvector:"), 
                  strong(round(max(segment_nodes$eigen_centrality), 3))
              )
          )
        )
      ),
      
      hr(),
      
      # Bottom row: Composition breakdown
      h5("Composition Breakdown"),
      layout_column_wrap(
        # Card for Node Types
        if (!is.null(type_counts)) {
          card(
            card_header("Node Types"),
            tags$ul(class = "list-group list-group-flush",
               lapply(names(type_counts), function(type_name) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    type_name,
                    span(class = "badge bg-primary rounded-pill", type_counts[type_name])
                 )
               })
            )
          )
        },
        
        # Card for Departments
        if (!is.null(dept_counts) && length(dept_counts) > 0) {
          card(
            card_header("Departments"),
            tags$ul(class = "list-group list-group-flush",
               lapply(names(dept_counts), function(dept_name) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    dept_name,
                    span(class = "badge bg-info rounded-pill", dept_counts[dept_name])
                 )
               })
            )
          )
        },
        
        # Card for Role Segments
        if (!is.null(role_counts)) {
          card(
            card_header( "Role Segments"),
            tags$ul(class = "list-group list-group-flush",
               lapply(names(role_counts), function(role_name) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    role_name,
                    span(class = "badge bg-success rounded-pill", role_counts[role_name])
                 )
               })
            )
          )
        }
      )
    )
  })
  
  # Segment composition visualization
  output$segment_composition <- renderPlot({
    req(input$analyze_segment)
    kg <- kg_reactive()
    seg_id <- as.numeric(input$seg_id)
    
    segment_nodes <- kg$nodes %>% filter(community == seg_id)
    
    if (nrow(segment_nodes) == 0) {
      return(ggplot() + geom_blank() + theme_void() + ggtitle("No data available"))
    }
    
    # Create plots for different composition aspects
    if ("type" %in% colnames(segment_nodes) && length(unique(segment_nodes$type)) > 1) {
      p1 <- ggplot(segment_nodes, aes(x = type, fill = type)) +
        geom_bar() +
        labs(title = "Node Types", x = "", y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      p1 <- ggplot() + geom_blank() + theme_void() + ggtitle("No type data")
    }
    
    if ("department" %in% colnames(segment_nodes) && length(unique(na.omit(segment_nodes$department))) > 1) {
      p2 <- ggplot(segment_nodes, aes(x = department, fill = department)) +
        geom_bar() +
        labs(title = "Departments", x = "", y = "Count") +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      p2 <- ggplot() + geom_blank() + theme_void() + ggtitle("No department data")
    }
    
    if ("role_segment" %in% colnames(segment_nodes) && length(unique(segment_nodes$role_segment)) > 1) {
      p3 <- ggplot(segment_nodes, aes(x = role_segment, fill = role_segment)) +
        geom_bar() +
        labs(title = "Role Segments", x = "", y = "Count") +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      p3 <- ggplot() + geom_blank() + theme_void() + ggtitle("No role segment data")
    }
    
    # Combine plots
    gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
  })
  
  # Segment network visualization
  output$segment_network <- renderPlot({
    req(input$analyze_segment)
    kg <- kg_reactive()
    seg_id <- as.numeric(input$seg_id)
    
    segment_nodes <- kg$nodes %>% filter(community == seg_id)
    segment_edges <- kg$edges %>%
      filter(from %in% segment_nodes$id & to %in% segment_nodes$id)
    
    if (nrow(segment_nodes) == 0) {
      return(ggplot() + geom_blank() + theme_void() + ggtitle("No data available"))
    }
    
    # Create subgraph
    segment_graph <- tbl_graph(nodes = segment_nodes, edges = segment_edges, directed = TRUE)
    
    ggraph(segment_graph, layout = "fr") +
      geom_edge_link(aes(width = strength), alpha = 0.5) +
      geom_node_point(aes(color = role_segment, size = degree)) +
      geom_node_text(aes(label = label), repel = TRUE, size = 4) +
      theme_void() +
      ggtitle(paste("Internal Network of Segment", seg_id))
  })
  
  # Segment recommendations
  output$segment_recommendations <- renderUI({
    req(input$analyze_segment)
    kg <- kg_reactive()
    seg_id <- as.numeric(input$seg_id)
    
    segment_nodes <- kg$nodes %>% filter(community == seg_id)
    other_nodes <- kg$nodes %>% filter(community != seg_id)
    
    # --- Data Preparation ---
    
    # 1. Potential bridges
    bridge_candidates <- expand_grid(
      from = segment_nodes$id, to = other_nodes$id
    ) %>%
      left_join(segment_nodes %>% select(id, from_label = label, from_deg = degree), by = c("from" = "id")) %>%
      left_join(other_nodes %>% select(id, to_label = label, to_deg = degree, to_comm = community), by = c("to" = "id")) %>%
      mutate(bridge_score = from_deg + to_deg) %>%
      arrange(desc(bridge_score)) %>%
      slice_head(n = 5)
    
    # 2. Key influencers and bridges within segment
    top_influencers <- if (nrow(segment_nodes) > 0) {
      segment_nodes %>% arrange(desc(degree)) %>% slice_head(n = 3)
    } else { NULL }
    
    top_bridges <- if (nrow(segment_nodes) > 0) {
      segment_nodes %>% arrange(desc(betweenness)) %>% slice_head(n = 3)
    } else { NULL }
    
    # 3. Segment health
    density <- if (nrow(segment_nodes) > 0) {
      edge_density(as.igraph(tbl_graph(nodes = segment_nodes, 
                                       edges = kg$edges %>% filter(from %in% segment_nodes$id & to %in% segment_nodes$id), 
                                       directed = TRUE)))
    } else { 0 }
    
    isolated_count <- if (nrow(segment_nodes) > 0) sum(segment_nodes$degree == 0) else 0
    
    # --- UI Rendering ---
    card(
      card_header( h4(strong("Recommendations for Segment "), span(seg_id, class = "text-primary")) ),
      
      # Section 1: Potential Bridges
      if (nrow(bridge_candidates) > 0) {
        card(
          card_header( h5(span(icon("link"), class="text-info"), " Potential Bridges to Other Segments") ),
          tags$ol(class = "list-group list-group-numbered",
             lapply(1:nrow(bridge_candidates), function(i) {
               tags$li(class = "list-group-item",
                  "Connect ", strong(bridge_candidates$from_label[i]), " with ", strong(bridge_candidates$to_label[i]),
                  span(class = "badge bg-secondary rounded-pill float-end", paste("Score:", bridge_candidates$bridge_score[i])),
                  br(),
                  tags$small(paste0("(to Segment ", bridge_candidates$to_comm[i], ")"))
               )
             })
          )
        )
      },
      
      # Section 2: Influencers and Bridges (Side-by-side)
      if (!is.null(top_influencers) && !is.null(top_bridges)) {
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header( h5(span(icon("star"), class="text-warning"), " Top Influencers within Segment")),
            tags$ol(class = "list-group list-group-numbered",
               lapply(1:nrow(top_influencers), function(i) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    tags$strong(top_influencers$label[i]),
                    span(class = "badge bg-primary rounded-pill", paste("Degree:", top_influencers$degree[i]))
                 )
               })
            )
          ),
          card(
            card_header( h5(span(icon("project-diagram"), class="text-success"), " Top Bridge Nodes within Segment")),
            tags$ol(class = "list-group list-group-numbered",
               lapply(1:nrow(top_bridges), function(i) {
                 tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                    tags$strong(top_bridges$label[i]),
                    span(class = "badge bg-info rounded-pill", paste("Betweenness:", round(top_bridges$betweenness[i], 2)))
                 )
               })
            )
          )
        )
      },
      
      # Section 3: Segment Health Analysis
      card(
        card_header( h5(span(icon("heart-pulse"), class="text-danger"), " Segment Health Analysis")),
        div(class = "vstack gap-2", # A vertical stack for the alerts
            # Density Alert
            if (density < 0.1) {
              div(class = "alert alert-warning", role = "alert",
                  h6(class = "alert-heading", icon("exclamation-triangle"), " LOW DENSITY"),
                  p("This segment is loosely connected. Consider activities to strengthen internal connections.", class="mb-0")
              )
            } else if (density > 0.5) {
              div(class = "alert alert-success", role = "alert",
                  h6(class = "alert-heading", icon("check-circle"), " HIGH DENSITY"),
                  p("This segment is tightly connected. Leverage this cohesion for collaborative projects.", class="mb-0")
              )
            } else {
              div(class = "alert alert-info", role = "alert",
                  h6(class = "alert-heading", icon("info-circle"), " MODERATE DENSITY"),
                  p("This segment has balanced connectivity. Maintain current engagement levels.", class="mb-0")
              )
            },
            
            # Isolated Nodes Alert
            if (isolated_count > 0) {
              div(class = "alert alert-danger", role = "alert",
                  h6(class = "alert-heading", icon("user-slash"), " ISOLATED NODES"),
                  p(paste("There are", isolated_count, "isolated nodes in this segment. Consider targeted outreach."), class="mb-0")
              )
            }
        )
      )
    )
  })
  
  # Enhanced export functionality
  output$export_seg <- downloadHandler(
    filename = function() {
      ext <- switch(input$export_format, 
                    "CSV" = "csv", 
                    "JSON" = "json", 
                    "Excel" = "xlsx")
      paste0("segment_", input$seg_id, ".", ext)
    },
    content = function(file) {
      kg <- kg_reactive()
      seg_id <- as.numeric(input$seg_id)
      segment_nodes <- kg$nodes %>% filter(community == seg_id)
      
      # Add additional data if requested
      if (input$include_metrics) {
        # Metrics are already included in the nodes data
      }
      
      if (input$include_recommendations) {
        # Add recommendations as a column
        segment_nodes$recommendations <- "Individual recommendations would be added here"
      }
      
      # Export in the requested format
      if (input$export_format == "CSV") {
        write.csv(segment_nodes, file, row.names = FALSE)
      } else if (input$export_format == "JSON") {
        jsonlite::write_json(segment_nodes, file)
      } else if (input$export_format == "Excel") {
        writexl::write_xlsx(segment_nodes, file)
      }
    }
  )
  
  # ---- Multi-Segment Membership ---- #########################################
  output$multi_table <- renderDT({
    kg <- kg_reactive()
    datatable(kg$nodes %>% select(label, community, kmeans_cluster, multi_segment), rownames = FALSE)
  })
  
  # Multi-segment overlap visualization
  output$multi_overlap <- renderPlot({
    kg <- kg_reactive()
    
    # Create a contingency table of community vs kmeans_cluster
    contingency_table <- table(kg$nodes$community, kg$nodes$kmeans_cluster)
    
    # Convert to data frame for plotting
    overlap_df <- as.data.frame(contingency_table)
    colnames(overlap_df) <- c("Community", "KMeans", "Count")
    
    # Create a heatmap
    ggplot(overlap_df, aes(x = factor(Community), y = factor(KMeans), fill = Count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Count), color = "black", size = 4) +
      scale_fill_gradient(low = "white", high = "#6366f1") +
      labs(title = "Community vs K-Means Cluster Overlap",
           x = "Community", y = "K-Means Cluster") +
      theme_minimal()
  })
  
  # Multi-segment statistics
  output$multi_stats <- renderUI({
    kg <- kg_reactive()
    
    # --- Data Preparation (Core logic remains the same) ---
    
    # Calculate multi-segment membership
    kg$nodes$multi_segment <- paste0("C", kg$nodes$community, "_K", kg$nodes$kmeans_cluster)
    
    # Count unique multi-segment memberships
    unique_multi <- length(unique(kg$nodes$multi_segment))
    total_nodes <- nrow(kg$nodes)
    avg_multi_size <- total_nodes / unique_multi
    
    # Distribution of multi-segment sizes
    multi_counts <- table(kg$nodes$multi_segment)
    size_summary <- summary(as.numeric(multi_counts))
    
    # Largest and smallest multi-segments
    largest_multi <- names(multi_counts)[which.max(multi_counts)]
    smallest_multi <- names(multi_counts)[which.min(multi_counts)]
    
    # Role segment distribution
    role_dist <- if ("role_segment" %in% colnames(kg$nodes)) {
      prop.table(table(kg$nodes$role_segment)) * 100
    } else { NULL }
    
    # --- UI Rendering ---
    card(
      card_header (h4("Multi-Segment Membership Statistics")),
      
      # Top row: Key metrics as value boxes
      layout_column_wrap(
        value_box(
          "Total Nodes",
          total_nodes, 
          icon = icon("users"), 
          theme = "primary"
        ),
        value_box(
          "Unique Multi-Segment Memberships",
          unique_multi, 
          icon = icon("tags"), 
          theme = "info"
        ),
        value_box(
          "Average Multi-Segment Size",
          round(avg_multi_size, 1), 
          icon = icon("chart-bar"), 
          theme = "success"
        )
      ),
      
      hr(),
      
      # Second row: Size distribution and extremes
      layout_columns(
        col_widths = c(6, 6),
        
        # Card 1: Size Distribution
        card(
          card_header( h5(span(icon("chart-line"), class="text-info"), " Multi-Segment Size Distribution")),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
              div(class = "d-flex justify-content-between",
                  span("Minimum:"), 
                  strong(size_summary["Min."])
              ),
              div(class = "d-flex justify-content-between",
                  span("1st Quartile:"), 
                  strong(size_summary["1st Qu."])
              ),
              div(class = "d-flex justify-content-between",
                  span("Median:"), 
                  strong(size_summary["Median"])
              ),
              div(class = "d-flex justify-content-between",
                  span("Mean:"), 
                  strong(round(size_summary["Mean"], 2))
              ),
              div(class = "d-flex justify-content-between",
                  span("3rd Quartile:"), 
                  strong(size_summary["3rd Qu."])
              ),
              div(class = "d-flex justify-content-between",
                  span("Maximum:"), 
                  strong(size_summary["Max."])
              )
          )
        ),
        
        # Card 2: Largest and Smallest
        card(
          card_header( h5(span(icon("sort-amount-up"), class="text-warning"), " Segment Extremes")),
          div(class = "vstack gap-3", # Vertical stack
              div(class = "text-center p-2 border rounded",
                  h6("Largest Multi-Segment"),
                  p(strong(largest_multi), class="mb-0"),
                  tags$small(paste0("Size: ", max(multi_counts)))
              ),
              div(class = "text-center p-2 border rounded",
                  h6("Smallest Multi-Segment"),
                  p(strong(smallest_multi), class="mb-0"),
                  tags$small(paste0("Size: ", min(multi_counts)))
              )
          )
        )
      ),
      hr(),
      # Bottom row: Role Distribution
      if (!is.null(role_dist)) {
        card(
          card_header(h5(span(icon("user-tag"), class="text-primary"), " Role Segment Distribution")),
          tags$ul(class = "list-group list-group-flush",
             lapply(names(role_dist), function(role_name) {
               role_count <- sum(kg$nodes$role_segment == role_name)
               tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                  role_name,
                  span(class = "badge bg-primary rounded-pill", 
                       paste0(role_count, " (", round(role_dist[role_name], 1), "%)"))
               )
             })
          )                                                                                  
    )
  })
})
  
  # Enhanced multi-segment table
  output$multi_table <- renderDT({
    kg <- kg_reactive()
    
    # Calculate multi-segment membership
    kg$nodes$multi_segment <- paste0("C", kg$nodes$community, "_K", kg$nodes$kmeans_cluster)
    
    # Select relevant columns
    multi_df <- kg$nodes %>%
      select(label, community, kmeans_cluster, multi_segment, role_segment, degree, betweenness) %>%
      arrange(desc(degree))
    
    datatable(multi_df, 
              options = list(pageLength = 10), 
              rownames = FALSE,
              extensions = 'Buttons',
              callbacks = list(
                rowCallback = JS(
                  "function(row, data, index) {",
                  "  if (data[5] > 5) {",
                  "    $('td:eq(5)', row).css('font-weight', 'bold');",
                  "  }",
                  "}"
                )
              )
    )
  })
  
  # ---- Segment Similarity ----
  output$sim_matrix <- renderDT({
    kg <- kg_reactive()
    centers <- kg$nodes %>%
      group_by(kmeans_cluster) %>%
      summarise(across(c(degree, betweenness, eigen_centrality, closeness), mean, na.rm = TRUE))
    
    sim_mat <- as.data.frame(as.matrix(dist(centers[, -1])))
    rownames(sim_mat) <- paste0("K", centers$kmeans_cluster)
    
    datatable(sim_mat, options = list(pageLength = 5))
  })
  
  output$sim_table <- renderDT({
    kg <- kg_reactive()
    feats <- kg$nodes %>% select(degree, betweenness, eigen_centrality, closeness)
    centers <- aggregate(feats, by = list(kg$nodes$community), mean)
    colnames(centers)[1] <- "community"
    
    sim <- as.matrix(dist(centers[, -1]))
    rownames(sim) <- centers$community
    colnames(sim) <- centers$community
    
    datatable(round(sim, 2), options = list(pageLength = 5))
  })
  
  # Similarity heatmap
  output$sim_heatmap <- renderPlot({
    kg <- kg_reactive()
    
    # Calculate similarity matrix based on selected metric
    metric <- input$similarity_metric
    
    features <- kg$nodes %>% select(degree, betweenness, eigen_centrality, closeness)
    centers <- aggregate(features, by = list(kg$nodes$community), mean)
    colnames(centers)[1] <- "community"
    
    if (metric == "Euclidean") {
      sim_mat <- as.matrix(dist(centers[,-1], method = "euclidean"))
    } else if (metric == "Manhattan") {
      sim_mat <- as.matrix(dist(centers[,-1], method = "manhattan"))
    } else if (metric == "Cosine") {
      # Convert distance to similarity
      sim_mat <- as.matrix(dist(centers[,-1], method = "euclidean"))
      max_dist <- max(sim_mat)
      sim_mat <- 1 - (sim_mat / max_dist)
    }
    
    # Convert to data frame for plotting
    sim_df <- expand.grid(Community1 = centers$community, Community2 = centers$community)
    sim_df$Similarity <- as.vector(sim_mat)
    
    # Create heatmap
    ggplot(sim_df, aes(x = factor(Community1), y = factor(Community2), fill = Similarity)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Similarity, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "#8b5cf6", mid = "white", high = "#6366f1", 
                           midpoint = if (metric == "Cosine") 0.5 else mean(sim_mat)) +
      labs(title = paste("Segment Similarity Matrix (", metric, ")", sep = ""),
           x = "Community", y = "Community") +
      theme_minimal()
  })
  
  # Similarity network
  output$sim_network <- renderPlot({
    kg <- kg_reactive()
    
    # Calculate similarity matrix
    metric <- input$similarity_metric
    
    features <- kg$nodes %>% select(degree, betweenness, eigen_centrality, closeness)
    centers <- aggregate(features, by = list(kg$nodes$community), mean)
    colnames(centers)[1] <- "community"
    
    if (metric == "Euclidean") {
      sim_mat <- as.matrix(dist(centers[,-1], method = "euclidean"))
    } else if (metric == "Manhattan") {
      sim_mat <- as.matrix(dist(centers[,-1], method = "manhattan"))
    } else if (metric == "Cosine") {
      # Convert distance to similarity
      sim_mat <- as.matrix(dist(centers[,-1], method = "euclidean"))
      max_dist <- max(sim_mat)
      sim_mat <- 1 - (sim_mat / max_dist)
    }
    
    # Create a similarity graph
    if (metric == "Cosine") {
      # For cosine similarity, use the similarity values directly
      adj_mat <- sim_mat
      diag(adj_mat) <- 0  # Remove self-loops
    } else {
      # For distance metrics, convert to similarity
      max_dist <- max(sim_mat)
      adj_mat <- 1 - (sim_mat / max_dist)
      diag(adj_mat) <- 0  # Remove self-loops
    }
    
    # Create graph from adjacency matrix
    sim_graph <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)
    
    # Set vertex attributes
    V(sim_graph)$name <- centers$community
    V(sim_graph)$size <- table(kg$nodes$community)[as.character(centers$community)] * 5
    
    # Plot the similarity network
    plot(sim_graph,
         vertex.label = V(sim_graph)$name,
         vertex.size = V(sim_graph)$size,
         vertex.color = "#6366f1",
         edge.width = E(sim_graph)$weight * 10,
         edge.color = "#8b5cf6",
         main = paste("Segment Similarity Network (", metric, ")", sep = ""))
  })
  
  # Enhanced similarity matrix
  output$sim_matrix <- renderDT({
    kg <- kg_reactive()
    
    # Calculate similarity matrix
    metric <- input$similarity_metric
    
    features <- kg$nodes %>% select(degree, betweenness, eigen_centrality, closeness)
    centers <- aggregate(features, by = list(kg$nodes$community), mean)
    colnames(centers)[1] <- "community"
    
    if (metric == "Euclidean") {
      sim_mat <- as.matrix(dist(centers[,-1], method = "euclidean"))
    } else if (metric == "Manhattan") {
      sim_mat <- as.matrix(dist(centers[,-1], method = "manhattan"))
    } else if (metric == "Cosine") {
      # Convert distance to similarity
      sim_mat <- as.matrix(dist(centers[,-1], method = "euclidean"))
      max_dist <- max(sim_mat)
      sim_mat <- 1 - (sim_mat / max_dist)
    }
    
    # Convert to data frame for display
    sim_df <- as.data.frame(sim_mat)
    rownames(sim_df) <- centers$community
    colnames(sim_df) <- centers$community
    
    # Add community size information
    comm_sizes <- table(kg$nodes$community)
    sim_df$Size <- comm_sizes[as.character(rownames(sim_df))]
    
    datatable(round(sim_df, 3), 
              options = list(pageLength = 10),
              extensions = 'Buttons',
              callbacks = list(
                rowCallback = JS(
                  "function(row, data, index) {",
                  "    for (let j = 0; j <= index; j++) {",
                  "      $('td:eq(' + j + ')', row).css('background-color', '#f0f0f0');",
                  "    }",
                  "}"
                )
              )
    )
  })
  
  # Recalculate similarity button
  observeEvent(input$recalculate_similarity, {
    showNotification("Similarity matrix recalculated", type = "message")
  })
  
  # ---- Segment Alerts ----
  output$seg_alert <- renderUI({
    req(kg_reactive())
    kg <- kg_reactive()
    alerts <- kg$nodes %>% filter(role_segment != role_segment_prev & !is.na(role_segment_prev))
    
    if (nrow(alerts) > 0) {
      # --- UI for when alerts exist ---
      card(
        card_header( h4(icon("exclamation-triangle", class="text-warning"), " Segment/Role Change Alerts")),
        
        # Value box to show the count of alerts
        layout_column_wrap(
          value_box(
            "Total Alerts",
            nrow(alerts), 
            icon = icon("exclamation-triangle"), 
            theme = "warning"
          )
        ),
        
        hr(), # Horizontal rule for separation
        
        # The detailed table of alerts
        h5("Details of Changes:"),
        div(DT::datatable(
          alerts %>%
            select(`Node Label` = label, `Previous Role` = role_segment_prev, `New Role` = role_segment),
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'tip' # Simple table layout: Table, Information, Pagination
          ),
          rownames = FALSE,
          selection = 'none',
          class = 'cell-border stripe hover'
        ))
      )
    } else {
      # --- UI for when there are no alerts ---
      div(class = "alert alert-success d-flex align-items-center", role = "alert",
          icon("check-circle-fill", class = "flex-shrink-0 me-2"),
          div(
            h5(class = "alert-heading", "All Clear!"),
            p("No segment/role change alerts were detected in this period.", class="mb-0")
          )
      )
    }
  })
  
  output$alert_table <- renderDT({
    kg <- kg_reactive()
    alerts <- kg$nodes %>%
      filter(role_segment != role_segment_prev & !is.na(role_segment_prev)) %>%
      select(label, role_segment_prev, role_segment)
    
    datatable(alerts, rownames = FALSE)
  })
  
  # ---- Segment Report Export ---- ############################################
  output$export_report <- downloadHandler(
    filename = function() paste0("segment_report_", Sys.Date(), ".csv"),
    content = function(file) {
      kg <- kg_reactive()
      # Replace with rmarkdown::render for PDF/HTML reports!
      write.csv(kg$nodes, file, row.names = FALSE)
    }
  )
  
  # Report preview
  output$report_preview <- renderUI({
    kg <- kg_reactive()
    seg_id <- input$report_segment
    
    segment_nodes <- kg$nodes %>% filter(community == seg_id)
    
    # Create a preview of the report
    preview_content <- paste0(
      "<h4>Segment Report Preview</h4>",
      "<p><strong>Segment ID:</strong> ", seg_id, "</p>",
      "<p><strong>Segment Size:</strong> ", nrow(segment_nodes), " nodes</p>",
      "<p><strong>Report Format:</strong> ", input$report_format, "</p>",
      "<p><strong>Timeframe:</strong> ", input$report_timeframe, " days</p>",
      "<p><strong>Inclusions:</strong> ",
      ifelse(input$include_charts, "Charts, ", ""),
      ifelse(input$include_recommendations, "Recommendations, ", ""),
      ifelse(input$include_alerts, "Alerts", ""),
      "</p>",
      "<p><strong>Estimated Report Size:</strong> ~", 
      round(0.5 + 0.2 * nrow(segment_nodes) + 
              0.3 * ifelse(input$include_charts, 1, 0) + 
              0.2 * ifelse(input$include_recommendations, 1, 0) + 
              0.1 * ifelse(input$include_alerts, 1, 0), 1), 
      " MB</p>"
    )
    
    HTML(preview_content)
  })
  
  # Enhanced report export
  output$export_report <- downloadHandler(
    filename = function() {
      ext <- tolower(input$report_format)
      paste0("segment_report_", input$report_segment, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      kg <- kg_reactive()
      seg_id <- input$report_segment
      
      # In a real implementation, this would generate a proper report using rmarkdown
      # For now, we'll create a simple CSV file with the segment data
      
      segment_nodes <- kg$nodes %>% filter(community == seg_id)
      
      # Add additional data based on user selections
      if (input$include_recommendations) {
        segment_nodes$recommendations <- "Individual recommendations would be added here"
      }
      
      if (input$include_alerts) {
        segment_nodes$alerts <- "Alert information would be added here"
      }
      
      # Export the data
      write.csv(segment_nodes, file, row.names = FALSE)
      
      # In a real implementation, you would use something like:
      # rmarkdown::render("segment_report.Rmd", 
      #                    params = list(segment_id = seg_id, 
      #                                 include_charts = input$include_charts,
      #                                 include_recommendations = input$include_recommendations,
      #                                 include_alerts = input$include_alerts,
      #                                 timeframe = input$report_timeframe),
      #                    output_file = file)
    }
  )
  
  # Schedule report
  observeEvent(input$schedule_report, {
    showModal(modalDialog(
      title = "Schedule Recurring Report",
      "In a full implementation, this would open a form to schedule recurring reports.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
} # end of server