# Genji Network — Shiny App
# SOCI 226 Final Project
# Based on class template (April 30)

library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)

#### DATA SETUP ####

nodes_raw <- read.csv("~/Desktop/soci226/genji_app/SOCI226_final_nodes.csv")
edges_raw <- read.csv("~/Desktop/soci226/genji_app/SOCI226_final_edges.csv")

# Full directed network with centrality measures
genji_full <- tbl_graph(nodes = nodes_raw, edges = edges_raw, directed = TRUE)
genji_full <- genji_full |>
  activate(nodes) |>
  mutate(degree = centrality_degree(mode = "all"),
         indegree = centrality_degree(mode = "in"),
         outdegree = centrality_degree(mode = "out"),
         betweenness = centrality_betweenness(normalized = TRUE))

# Community detection (undirected)
genji_undir <- genji_full |> activate(edges) |> convert(to_undirected)
genji_undir <- genji_undir |> activate(nodes) |> mutate(cluster = group_louvain())
cluster_df <- genji_undir |> activate(nodes) |> as_tibble() |> select(id, cluster)
genji_full <- genji_full |> activate(nodes) |> left_join(cluster_df, by = "id")

# Dataframe for bar charts
genji_df <- genji_full |> activate(nodes) |> as_tibble()

# Modularity
mod_louvain <- modularity(genji_undir, membership = as.integer(as.factor(V(genji_undir)$cluster)))
mod_chapter <- modularity(genji_undir, membership = as.integer(as.factor(V(genji_undir)$first_appearance)))

# Gender assortativity
genji_ig <- graph_from_data_frame(d = edges_raw, vertices = nodes_raw, directed = TRUE)
gender_assort <- assortativity_nominal(genji_ig, as.integer(as.factor(V(genji_ig)$gender)))

# Colors
gender_colors <- c("F" = "#E07A5F", "M" = "#457B9D")
community_colors <- c("#E07A5F", "#3D405B", "#81B29A", "#F2CC8F",
                      "#5FA8D3", "#C97C5D", "#B8B8FF", "#FF6B6B", "#4ECDC4")
status_colors <- c("alive" = "#81B29A", "dies" = "#E07A5F", "NA" = "#CCCCCC")

# Helper: build chapter-specific network
build_chapter_net <- function(ch) {
  if (ch == "All") return(genji_full)
  ch_w <- paste0(tolower(ch), "_weight")
  ch_s <- paste0(tolower(ch), "_status")
  ch_edges <- edges_raw |> filter(.data[[ch_w]] > 0) |> mutate(weight = .data[[ch_w]])
  ch_nodes <- nodes_raw |> filter(.data[[ch_s]] != "NA")
  net <- tbl_graph(nodes = ch_nodes, edges = ch_edges, directed = TRUE)
  net <- net |> activate(nodes) |>
    mutate(degree = centrality_degree(mode = "all"),
           indegree = centrality_degree(mode = "in"),
           outdegree = centrality_degree(mode = "out"),
           betweenness = centrality_betweenness(normalized = TRUE))
  net_u <- net |> activate(edges) |> convert(to_undirected)
  net_u <- net_u |> activate(nodes) |> mutate(cluster = group_louvain())
  cl <- net_u |> activate(nodes) |> as_tibble() |> select(id, cluster)
  net <- net |> activate(nodes) |> left_join(cl, by = "id")
  return(net)
}


#### UI ####

ui <- fluidPage(
  
  titlePanel("The Tale of Genji — Social Interaction Network"),
  
  navlistPanel(
    widths = c(2, 10),
    
    #### TAB 1: INTRO ####
    tabPanel("Introduction",
             page_sidebar(
               title = "About This Project",
               sidebar = sidebar(
                 h5("Quick Stats"),
                 p(strong("33"), "characters"),
                 p(strong("78"), "directed edges"),
                 p(strong("3"), "chapters analyzed"),
                 hr(),
                 p("Chapters 1, 4, and 5 of",
                   em("The Tale of Genji"),
                   "(源氏物語, ca. 1010 CE)")
               ),
               
               card(
                 card_header("Introduction"),
                 p("The Tale of Genji (源氏物語), written by Murasaki Shikibu in early 11th-century Japan,
            is often called the world's first novel. This explores the social interaction
            network of its characters across three pivotal chapters: Kiritsubo (桐壺, Ch.1),
            Yugao (夕顔, Ch.4), and Wakamurasaki (若紫, Ch.5)."),
                 p("Each edge represent direct social interactions: face-to-face conversations, waka poetry
            exchanges, and messenger-relayed communications. Inner thoughts, kinship alone, and
            supernatural events (such as spirit possession) are excluded.")
               ),
               
               card(
                 card_header("Key Finding 1: Gender Asymmetry in Centrality"),
                 p("Male characters consistently show higher", strong("out-degree"), "(they initiate interactions),
            while female characters show equal or higher", strong("in-degree"),  "(they receive interactions).
            This reflects the Heian period's gendered social structure: men pursued women through
            letters and intermediaries, while women remained behind curtains (御簾)."),
                 p("Navigate to the", strong("Centrality"), "tab to explore this pattern with
            interactive charts.")
               ),
               
               card(
                 card_header("Key Finding 2: Communities Mirror Chapters"),
                 p("The Louvain community detection algorithm identifies clusters that closely align
            with the story's chapter structure. Court characters from Chapter 1 cluster together,
            Chapter 4's romantic world forms another group, and Chapter 5's Kitayama/Murasaki
            world forms a third. This suggests each chapter describes a relatively self-contained
            social world, connected primarily through Genji himself."),
                 p("Navigate to the", strong("Communities"), "tab to see the detected communities
            colored on the network.")
               ),
               
               card(
                 card_header("How to Use This App"),
                 tags$ul(
                   tags$li(strong("Network:"), "Static ggraph visualization. Filter by chapter and
                    toggle node sizing between degree and betweenness."),
                   tags$li(strong("Interactive:"), "Drag, zoom, and hover over nodes using visNetwork."),
                   tags$li(strong("Centrality:"), "Bar charts comparing degree, in/out-degree, and betweenness."),
                   tags$li(strong("Communities:"), "Louvain community detection colored on the network."),
                   tags$li(strong("Data:"), "Sources, methodology, and full data tables.")
                 )
               )
             )
    ),
    
    #### TAB 2: NETWORK ####
    tabPanel("Network",
             page_sidebar(
               title = "Network Visualization",
               sidebar = sidebar(
                 selectInput("net_chapter", "Chapter",
                             choices = list("All Chapters" = "All",
                                            "Ch.1 Kiritsubo (桐壺)" = "Ch1",
                                            "Ch.4 Yugao (夕顔)" = "Ch4",
                                            "Ch.5 Wakamurasaki (若紫)" = "Ch5"),
                             selected = "All"),
                 selectInput("net_size", "Node Size",
                             choices = list("Degree Centrality" = "degree",
                                            "Betweenness Centrality" = "betweenness"),
                             selected = "degree"),
                 selectInput("net_color", "Node Color",
                             choices = list("Gender" = "gender",
                                            "Community (Louvain)" = "cluster"),
                             selected = "gender"),
               ),
               
               card(
                 card_header("Static Network (ggraph)"),
                 "Filter by chapter to watch the network transform across the story.",
                 plotOutput("static_network", height = "600px")
               )
             )
    ),
    
    #### TAB 3: INTERACTIVE ####
    tabPanel("Interactive",
             page_sidebar(
               title = "Interactive Network",
               sidebar = sidebar(
                 radioButtons("vis_size", "Size nodes by:",
                              choices = c("Degree" = "degree",
                                          "Betweenness" = "betweenness"),
                              selected = "degree"),
                 radioButtons("vis_color", "Color nodes by:",
                              choices = c("Gender" = "gender",
                                          "Community (Louvain)" = "cluster"),
                              selected = "gender"),
               ),
               
               card(
                 card_header("Interactive Network (visNetwork)"),
                 visNetworkOutput("int_network", height = "650px")
               )
             )
    ),
    
    #### TAB 4: CENTRALITY ####
    tabPanel("Centrality",
             page_sidebar(
               title = "Centrality Measures",
               sidebar = sidebar(
                 selectInput("bar_measure", "Measure",
                             choices = list("Total Degree" = "degree",
                                            "In-Degree" = "indegree",
                                            "Out-Degree" = "outdegree",
                                            "Betweenness" = "betweenness"),
                             selected = "degree"),
                 selectInput("bar_chapter", "Filter by Chapter",
                             choices = list("All Chapters" = "All",
                                            "Ch.1 Kiritsubo" = "Ch1",
                                            "Ch.4 Yugao" = "Ch4",
                                            "Ch.5 Wakamurasaki" = "Ch5"),
                             selected = "All"),
                 hr(),
                 h5("Gender Assortativity"),
                 p(strong(round(gender_assort, 3))),
                 p("A negative value suggests characters interact more with the
            opposite gender, reflecting the cross-gender courtship
            dynamics of Heian court life.This also makes sense as this is a romance story book.")
               ),
               
               card(
                 card_header("Centrality Comparison"),
                 "Switch between measures to compare which characters are most central.
           Use the chapter filter to see how centrality changes across the story.",
                 plotOutput("bar_chart", height = "500px")
               ),
               
               card(
                 card_header("In-Degree vs Out-Degree by Gender"),
                 "This chart reveals the gendered asymmetry in Heian social interactions.
           Male characters initiate more (out-degree), female characters receive more (in-degree).",
                 plotOutput("inout_chart", height = "450px")
               )
             )
    ),
    
    #### TAB 5: COMMUNITIES ####
    tabPanel("Communities",
             page_sidebar(
               title = "Community Detection",
               sidebar = sidebar(
                 selectInput("comm_chapter", "Chapter",
                             choices = list("All Chapters" = "All",
                                            "Ch.1 Kiritsubo" = "Ch1",
                                            "Ch.4 Yugao" = "Ch4",
                                            "Ch.5 Wakamurasaki" = "Ch5"),
                             selected = "All"),
                 hr(),
                 h5("Modularity"),
                 textOutput("mod_text")
               ),
               
               card(
                 card_header("Community Detection (Louvain)"),
                 "Each color represents a community detected by the Louvain algorithm.
           Notice how communities roughly correspond to chapters — court characters
           from Ch.1 cluster together, Yugao's world forms another group, and the
           Kitayama/Murasaki world forms a third.",
                 plotOutput("comm_network", height = "600px")
               )
             )
    ),
    
    #### TAB 6: DATA ####
    tabPanel("Data",
             page_sidebar(
               title = "Data & Methodology",
               sidebar = sidebar(
                 p("All data was collected manually from a modern Japanese
            translation of The Tale of Genji."),
                 hr(),
                 tags$a("Source text",
                        href = "http://james.3zoku.com/genji/index.html",
                        target = "_blank")
               ),
               
               card(
                 card_header("Data Collection"),
                 p("This network was constructed by manually reading a modern Japanese translation
            of Chapters 1, 4, and 5 of The Tale of Genji, accessed at",
                   tags$a("james.3zoku.com/genji/", href = "http://james.3zoku.com/genji/index.html",
                          target = "_blank"),
                   "in April 2026. The site's text is based on Shibata Masaaki's corrected edition
            of Shibuya Eiichi's Genji Monogatari no Sekai (源氏物語の世界)."),
                 p("An interaction was defined as: face-to-face conversation, exchange of waka poetry
            (each direction counted separately), messenger-relayed communication (both legs counted),
            ceremonial co-presence with a functional role, or a marriage event."),
                 p("Excluded: inner thoughts, third-party remarks, kinship alone, and supernatural
            interactions (e.g., Rokujo's living spirit killing Yugao — the two women never
            physically meet)."),
                 p("Four characters (Kokiden, First Prince, Minister of Right, Fujitsubo's Mother)
            appear as isolated nodes: they are mentioned in the text but have no qualifying
            direct interactions. Their isolation is itself an analytical finding about invisible
            power in Heian court culture.")
               ),
               
               card(
                 card_header("Nodes"),
                 DT::dataTableOutput("nodes_table")
               ),
               
               card(
                 card_header("Edges"),
                 DT::dataTableOutput("edges_table")
               )
             )
    )
  )
)


#### SERVER ####

server <- function(input, output) {
  
  # Reactive: chapter-filtered network
  net_reactive <- reactive({ build_chapter_net(input$net_chapter) })
  bar_reactive <- reactive({ build_chapter_net(input$bar_chapter) })
  comm_reactive <- reactive({ build_chapter_net(input$comm_chapter) })
  
  # --- TAB 2: NETWORK ---
  
  output$net_summary <- renderText({
    net <- net_reactive()
    df <- net |> activate(nodes) |> as_tibble()
    n <- nrow(df)
    e <- net |> activate(edges) |> as_tibble() |> nrow()
    top <- df |> arrange(desc(degree)) |> slice(1) |> pull(name_en)
    paste0("Nodes: ", n, " | Edges: ", e, "\nMost connected: ", top)
  })
  
  output$static_network <- renderPlot({
    net <- net_reactive()
    size_var <- input$net_size
    color_var <- input$net_color
    
    if (color_var == "gender") {
      color_scale <- scale_color_manual(values = gender_colors, name = "Gender")
    } else {
      net <- net |> activate(nodes) |> mutate(cluster = as.factor(cluster))
      color_scale <- scale_color_manual(values = community_colors, name = "Community")
    }
    
    ggraph(net, layout = "fr") +
      geom_edge_link(aes(width = weight),
                     alpha = 0.2, color = "#888888",
                     arrow = arrow(length = unit(2, 'mm')),
                     end_cap = circle(3, 'mm')) +
      scale_edge_width(range = c(0.3, 2.5), name = "Weight") +
      geom_node_point(aes(size = .data[[size_var]],
                          color = .data[[color_var]]),
                      alpha = 0.85) +
      scale_size_continuous(range = c(1.5, 16), name = str_to_title(size_var)) +
      color_scale +
      geom_node_text(aes(label = name_en), repel = TRUE, size = 3,
                     color = "#333333", max.overlaps = 30) +
      theme_graph() +
      theme(legend.position = "right")
  }, res = 96)
  
  # --- TAB 3: INTERACTIVE ---
  
  vis_data <- reactive({
    net <- genji_full
    df <- net |> activate(nodes) |> as_tibble()
    ed <- net |> activate(edges) |> as_tibble()
    
    # Node sizing (visNetwork uses "value")
    size_col <- input$vis_size
    
    # Color assignment
    if (input$vis_color == "gender") {
      df <- df |> mutate(
        color = case_when(gender == "F" ~ "#E07A5F",
                          gender == "M" ~ "#457B9D",
                          TRUE ~ "#CCCCCC")
      )
    } else {
      df <- df |> mutate(
        color = community_colors[as.integer(cluster)]
      )
    }
    
    # visNetwork needs id column as row numbers
    vis_nodes <- df |>
      select(-id) |>
      rowid_to_column("id") |>
      mutate(label = name_en,
             value = .data[[size_col]],
             title = paste0("<b>", name_en, "</b> (", name_jp, ")<br>",
                            "Gender: ", gender, "<br>",
                            "Rank: ", rank, "<br>",
                            "Degree: ", degree, "<br>",
                            "Betweenness: ", round(betweenness, 3), "<br>",
                            "Community: ", cluster)) |>
      select(id, label, value, color, title)
    
    # tidygraph edges already use row-number indices
    vis_edges <- ed |>
      rename(from = 1, to = 2) |>
      mutate(width = weight * 0.8,
             arrows = "to") |>
      select(from, to, width, arrows)
    
    list(nodes = vis_nodes, edges = vis_edges)
  })
  
  output$int_network <- renderVisNetwork({
    data <- vis_data()
    
    visNetwork(data$nodes, data$edges) |>
      visNodes(borderWidth = 1,
               font = list(size = 14, color = "#333333")) |>
      visEdges(color = list(color = "#AAAAAA", highlight = "#333333"),
               smooth = list(type = "continuous")) |>
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                 nodesIdSelection = FALSE) |>
      visInteraction(dragNodes = TRUE,
                     dragView = TRUE,
                     zoomView = TRUE) |>
      visPhysics(stabilization = TRUE,
                 solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -30))
  })
  
  # --- TAB 4: CENTRALITY ---
  
  output$bar_chart <- renderPlot({
    net <- bar_reactive()
    df <- net |> activate(nodes) |> as_tibble() |>
      filter(.data[[input$bar_measure]] > 0)
    
    ggplot(df, aes(x = reorder(name_en, .data[[input$bar_measure]]),
                   y = .data[[input$bar_measure]],
                   fill = gender)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = gender_colors) +
      labs(x = "", y = str_to_title(input$bar_measure), fill = "Gender") +
      theme_minimal() +
      theme(text = element_text(size = 13))
  }, res = 96)
  
  output$inout_chart <- renderPlot({
    net <- bar_reactive()
    df <- net |> activate(nodes) |> as_tibble() |>
      filter(degree > 0) |>
      select(name_en, gender, indegree, outdegree) |>
      pivot_longer(cols = c(indegree, outdegree),
                   names_to = "type", values_to = "value") |>
      filter(value > 0)
    
    ggplot(df, aes(x = reorder(name_en, value), y = value, fill = type)) +
      geom_col(position = "dodge") +
      coord_flip() +
      facet_wrap(~gender) +
      scale_fill_manual(values = c("indegree" = "#E07A5F", "outdegree" = "#457B9D"),
                        labels = c("In-degree", "Out-degree")) +
      labs(x = "", y = "Degree", fill = "") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  }, res = 96)
  
  # --- TAB 5: COMMUNITIES ---
  
  output$mod_text <- renderText({
    if (input$comm_chapter == "All") {
      paste0("Louvain: ", round(mod_louvain, 3),
             "\nBy chapter: ", round(mod_chapter, 3))
    } else {
      net <- comm_reactive()
      nu <- net |> activate(edges) |> convert(to_undirected)
      nu <- nu |> activate(nodes) |> mutate(cl = group_louvain())
      m <- modularity(nu, membership = as.integer(as.factor(V(nu)$cl)))
      paste0("Louvain: ", round(m, 3))
    }
  })
  
  output$comm_table <- renderTable({
    df <- genji_df |> filter(degree > 0)
    t <- table(Community = df$cluster, Chapter = df$first_appearance)
    as.data.frame.matrix(t)
  }, rownames = TRUE)
  
  output$comm_network <- renderPlot({
    net <- comm_reactive()
    net <- net |> activate(nodes) |> mutate(cluster = as.factor(cluster))
    
    ggraph(net, layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.15, color = "#888888") +
      scale_edge_width(range = c(0.3, 2.5), name = "Weight") +
      geom_node_point(aes(size = degree, color = cluster), alpha = 0.85) +
      scale_size_continuous(range = c(2, 16), name = "Degree") +
      scale_color_manual(values = community_colors, name = "Community") +
      geom_node_text(aes(label = name_en), repel = TRUE, size = 3,
                     color = "#333333", max.overlaps = 30) +
      theme_graph() +
      theme(legend.position = "right")
  }, res = 96)
  
  # --- TAB 6: DATA ---
  
  output$nodes_table <- DT::renderDataTable({
    nodes_raw |> select(id, name_en, name_jp, gender, rank, status, first_appearance)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$edges_table <- DT::renderDataTable({
    edges_raw |> select(source, target, weight, ch1_weight, ch4_weight, ch5_weight, scene_notes)
  }, options = list(pageLength = 10, scrollX = TRUE))
}


#### RUN ####
shinyApp(ui = ui, server = server)