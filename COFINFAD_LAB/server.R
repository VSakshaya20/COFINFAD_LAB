pacman::p_load(shiny, shinydashboard, shinythemes, rlang, RColorBrewer, corrplot,
               plotly, tidyverse, ggstatsplot, tools, ggiraph, ggpubr, ggdist, ggridges, ggmosaic, tidytext, cluster, factoextra, fpc, treemap)

df <- read_csv("data/cleaned_data.csv")

my_palette <- brewer.pal(n = 12, "Paired")

cat_vars <- reactive({
  names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
})
num_vars <- reactive({
  names(df)[sapply(df, is.numeric)]
})

# Entropy Calculation
calc_entropy <- function(clusters) {
  props <- table(clusters) / length(clusters)
  round(-sum(props * log(props + 1e-10)),3)
}

# Silhouette Width Calculation
calc_sil_kmeans <- function(model, data) {
  sil <- silhouette(model$cluster, dist(data))
  round(mean(sil[, 3]),3)
}

# AIC/BIC Calculation
aic_bic_text <- function(model, data) {
  k <- nrow(model$centers)
  n <- nrow(data)
  p <- ncol(data)
  aic <- model$tot.withinss + 2 * k * p
  bic <- model$tot.withinss + log(n) * k * p
  paste0("AIC: ", round(aic,1), " | BIC: ", round(bic,1))
}

# Cluster Composition Plots
plot_cluster_bar <- function(clusters) {
  data.frame(cluster = factor(clusters)) %>%
    count(cluster) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot(aes(x = cluster, y = prop, fill = cluster)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
              vjust = -0.4, size = 4) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = "Cluster Size Distribution", x = "Cluster", y = "Proportion") +
    theme_minimal()
}

# Within-Cluster Profiles
plot_heatmap <- function(clusters, data) {
  # encode any character/factor columns numerically
  data_encoded <- data %>%
    mutate(across(where(is.character), ~ as.numeric(factor(.x)))) %>%
    mutate(across(where(is.factor),    ~ as.numeric(.x)))
  
  data.frame(cluster = factor(clusters), data_encoded) %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean), .groups = "drop") %>%
    pivot_longer(-cluster, names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    mutate(value_scaled = scale(value)[,1]) %>%
    ggplot(aes(x = variable, y = cluster, fill = value_scaled)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato", midpoint = 0) +
    labs(title = "Cluster Centroid Heatmap", x = NULL, y = "Cluster", fill = "Scaled\nMean") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# Evaluation Plots
plot_elbow <- function(data, max_k = 10) {
  wss <- sapply(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 10)$tot.withinss
  })
  
  data.frame(k = 1:max_k, wss = wss) %>%
    ggplot(aes(x = k, y = wss)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(size = 3, color = "steelblue") +
    scale_x_continuous(breaks = 1:max_k) +
    labs(title = "Elbow Plot", x = "Number of Clusters (k)",
         y = "Total Within-Cluster SS") +
    theme_minimal()
}

plot_silhouette <- function(model, data) {
  sil <- silhouette(model$cluster, dist(data))
  fviz_silhouette(sil) +
    labs(title = "Silhouette Plot") +
    theme_minimal()
}

plot_silhouette_clara <- function(model) {
  sil <- model$silinfo$widths
  data.frame(
    obs     = seq_len(nrow(sil)),
    cluster = factor(sil[, 1]),
    width   = sil[, 3]
  ) %>%
    arrange(cluster, width) %>%
    mutate(obs = row_number()) %>%
    ggplot(aes(x = obs, y = width, fill = cluster)) +
    geom_col(show.legend = TRUE) +
    geom_hline(yintercept = mean(sil[, 3]), linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = "Silhouette Plot", x = NULL,
         y = "Silhouette Width", fill = "Cluster") +
    theme_minimal()
}

function(input, output, session) {
  
  demo_var_data    <- reactive({ df[[input$demo_var]] })
  tx_var_data      <- reactive({ df[[input$tx_var]] })
  sat_var_data     <- reactive({ df[[input$sat_var]] })
  product_var_data <- reactive({ df[[input$product_var]] })
  app_var_data     <- reactive({ df[[input$app_var]] })

  var_info <- data.frame(
    Variable = names(df),
    Description = c(
      "Unique identification number for each customer",
      "Customer's age in years",
      "Customer's gender",
      "The city and department where the customer resides",
      "The customer's income bracket, divided into four segments, namely, Very High, High, Medium, and Low",
      "Occupation of the customer",
      "Highest education level attained by the customer",
      "Customer's marital status",
      "Number of people in the customer's household",
      "How the customer was acquired, namely, partnership, paid ad, referral, and organic",
      "Whether the customer has a savings account",
      "Whether the customer has a credit card",
      "Whether the customer has a personal loan",
      "Whether the customer has an investment account",
      "Whether the customer has an insurance product",
      "Number of active financial products the customer has",
      "Number of times the customer logs into the app per month",
      "Number of unique features used by the customer in the app",
      "Whether the customer uses bill payment feature",
      "Whether the customer has enabled auto-savings feature",
      "Ratio of credit used to credit available",
      "Number of international transactions made by the customer",
      "Number of failed transactions",
      "Total number of transactions made by the customer",
      "Average value of customer's transactions",
      "Total value of all transactions made by the customer",
      "Date of the customer's first transaction",
      "Date of the customer's most recent transaction",
      "Base satisfaction score for the customer",
      "Satisfaction score based on transaction history",
      "Satisfaction score based on product usage",
      "Overall customer satisfaction score",
      "Number of support tickets opened by the customer",
      "Ratio of resolved support tickets to total tickets",
      "Customer's rating of the app in the app store",
      "Features requested by the customer",
      "Main topics of customer's complaints",
      "Most frequent type of transaction for the customer",
      "Ratio of transactions made on weekends",
      "Average number of transactions per day",
      "Length of time as a customer in months"
    ),
    DataType = c(
      "String", "Integer", "String", "String", "String", "String", "String", 
      "String", "Integer", "String", "Boolean", "Boolean", "Boolean", "Boolean", 
      "Boolean", "Integer", "Integer", "Integer", "Boolean", "Boolean", "Float", 
      "Integer", "Integer", "Integer", "Float", "Float", "Date", "Date", "Float", 
      "Float", "Float", "Integer", "Integer", "Float", "Float", "String", "String", "String", "Float", "Float", "Float"
    )
  )
  
  output$var_table <- DT::renderDataTable({
    
    var_info   
    
  }, options = list(
    pageLength = 7,    
    lengthChange = FALSE  
  ))
  
  # DEMOGRAPHICS
  output$demo_plot <- renderPlot({
    
    var_name <- input$demo_var
    
    if(var_name == "age") {
      
      df$age_group <- cut(df$age,
                          breaks = c(0, 25, 35, 45, 60, 100),
                          labels = c("<25", "25-34", "35-44", "45-59", "60+"))
      
      ggplot(df, aes(x = age_group)) +
        geom_bar(fill = "#3498db") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        labs(title = "Age Distribution", x = "Age Group", y = "Count") +
        theme_minimal()
      
    } else if(var_name == "location" | var_name == "occupation") {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#3498db") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        labs(title = var_name, x = "", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#3498db") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        labs(title = var_name, x = "", y = "Count") +
        theme_minimal()
    }
  })
  
  output$demo_var_name <- renderText({
    req(input$demo_var)
    input$demo_var
  })
  
  output$demo_is_numeric <- reactive({
    is.numeric(demo_var_data())
  })
  outputOptions(output, "demo_is_numeric", suspendWhenHidden = FALSE)
  
  output$demo_mean   <- renderText({ req(is.numeric(demo_var_data())) 
    mean(demo_var_data(), na.rm = TRUE)})
  output$demo_median <- renderText({ req(is.numeric(demo_var_data())) 
    median(demo_var_data(), na.rm = TRUE)})
  output$demo_min    <- renderText({ req(is.numeric(demo_var_data())) 
    min(demo_var_data(), na.rm = TRUE)})
  output$demo_max    <- renderText({ req(is.numeric(demo_var_data())) 
    max(demo_var_data(), na.rm = TRUE)})
  output$demo_sd     <- renderText({ req(is.numeric(demo_var_data())) 
    sd(demo_var_data(), na.rm = TRUE)})
  
  
  # TRANSACTIONS
  output$tx_plot <- renderPlot({
    
    var_name <- input$tx_var
    
    if(is.numeric(df[[var_name]])) {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_histogram(fill = "#2ecc71", bins = 30) +
        labs(title = var_name, x = "", y = "Frequency") +
        theme_minimal()
      
    } else {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#2ecc71") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        labs(title = var_name, x = "", y = "Count") +
        theme_minimal()
    }
  })
  
  output$tx_var_name <- renderText({
    req(input$tx_var)
    input$tx_var
  })
  
  output$tx_is_numeric <- reactive({
    is.numeric(tx_var_data())
  })
  outputOptions(output, "tx_is_numeric", suspendWhenHidden = FALSE)
  
  output$tx_mean   <- renderText({ req(is.numeric(tx_var_data())) 
    mean(tx_var_data(), na.rm = TRUE)})
  output$tx_median <- renderText({ req(is.numeric(tx_var_data())) 
    median(tx_var_data(), na.rm = TRUE)})
  output$tx_min    <- renderText({ req(is.numeric(tx_var_data())) 
    min(tx_var_data(), na.rm = TRUE)})
  output$tx_max    <- renderText({ req(is.numeric(tx_var_data())) 
    max(tx_var_data(), na.rm = TRUE)})
  output$tx_sd     <- renderText({ req(is.numeric(tx_var_data())) 
    sd(tx_var_data(), na.rm = TRUE)})
  
  #Product
  output$product_plot <- renderPlot({
    
    var_name <- input$product_var
    
    ggplot(df, aes_string(x = var_name)) +
      geom_bar(fill = "#e67e22") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
      labs(title = var_name, x = "", y = "Count") +
      theme_minimal()
  })
  
  output$product_stats_inline <- renderText({
    
    var <- product_var_data()
    
    counts <- table(var)
    props  <- prop.table(counts)
    
    paste(
      paste0(names(counts), ": ", counts, " (", props * 100, "%)"),
      collapse = " | "
    )
  })
  
  output$product_var_name <- renderText({
    req(input$product_var)
    input$product_var
  })
  
  output$product_is_numeric <- reactive({
    is.numeric(product_var_data())
  })
  outputOptions(output, "product_is_numeric", suspendWhenHidden = FALSE)
  
  output$product_mean   <- renderText({ req(is.numeric(product_var_data())) 
    mean(product_var_data(), na.rm = TRUE)})
  output$product_median <- renderText({ req(is.numeric(product_var_data())) 
    median(product_var_data(), na.rm = TRUE)})
  output$product_min    <- renderText({ req(is.numeric(product_var_data())) 
    min(product_var_data(), na.rm = TRUE)})
  output$product_max    <- renderText({ req(is.numeric(product_var_data())) 
    max(product_var_data(), na.rm = TRUE)})
  output$product_sd     <- renderText({ req(is.numeric(product_var_data())) 
    sd(product_var_data(), na.rm = TRUE)})
  
  # APP USAGE
  output$app_plot <- renderPlot({
    
    var_name <- input$app_var
    
    ggplot(df, aes_string(x = var_name)) +
      geom_bar(fill = "#1abc9c") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
      labs(title = var_name, x = "", y = "Count") +
      theme_minimal()
  })
  
  output$app_stats_inline <- renderText({
    
    var <- app_var_data()
    
    counts <- table(var)
    props  <- prop.table(counts)
    
    paste(
      paste0(names(counts), ": ", counts, " (", props * 100, "%)"),
      collapse = " | "
    )
  })
  
  output$app_var_name <- renderText({
    req(input$app_var)
    input$app_var
  })
  
  output$app_is_numeric <- reactive({
    is.numeric(app_var_data())
  })
  outputOptions(output, "app_is_numeric", suspendWhenHidden = FALSE)
  
  output$app_mean   <- renderText({ req(is.numeric(app_var_data())) 
    mean(app_var_data(), na.rm = TRUE)})
  output$app_median <- renderText({ req(is.numeric(app_var_data())) 
    median(app_var_data(), na.rm = TRUE)})
  output$app_min    <- renderText({ req(is.numeric(app_var_data())) 
    min(app_var_data(), na.rm = TRUE)})
  output$app_max    <- renderText({ req(is.numeric(app_var_data())) 
    max(app_var_data(), na.rm = TRUE)})
  output$app_sd     <- renderText({ req(is.numeric(app_var_data())) 
    sd(app_var_data(), na.rm = TRUE)})
  
  
  # SATISFACTION
  output$sat_plot <- renderPlot({
    
    var_name <- input$sat_var
    
    if(var_name %in% c("satisfaction_score", "product_satisfaction")) {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#9b59b6") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        labs(title = var_name, x = "", y = "Count") +
        theme_minimal()
      
    } else if(is.numeric(df[[var_name]])) {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_histogram(fill = "#9b59b6", bins = 30) +
        labs(title = var_name, x = "", y = "Frequency") +
        theme_minimal()
      
    } else {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#9b59b6") +
        labs(title = var_name, x = "", y = "Count") +
        theme_minimal()
    }
  })
  
  output$sat_var_name <- renderText({
    req(input$sat_var)
    input$sat_var
  })
  
  output$sat_is_numeric <- reactive({
    is.numeric(sat_var_data())
  })
  outputOptions(output, "sat_is_numeric", suspendWhenHidden = FALSE)
  
  output$sat_mean   <- renderText({ req(is.numeric(sat_var_data())) 
    mean(sat_var_data(), na.rm = TRUE)})
  output$sat_median <- renderText({ req(is.numeric(sat_var_data())) 
    median(sat_var_data(), na.rm = TRUE)})
  output$app_min    <- renderText({ req(is.numeric(sat_var_data())) 
    min(sat_var_data(), na.rm = TRUE)})
  output$sat_max    <- renderText({ req(is.numeric(sat_var_data())) 
    max(sat_var_data(), na.rm = TRUE)})
  output$sat_sd     <- renderText({ req(is.numeric(sat_var_data())) 
    sd(sat_var_data(), na.rm = TRUE)})
  
  #Bivariate 
  output$bivar_inputs <- renderUI({
    tab <- input$bivar_tabs
    if(tab == "Category vs Numerical") {
      tagList(
        selectInput("var1", "Categorical Variable", choices = cat_vars()),
        selectInput("var2", "Numerical Variable", choices = num_vars())
      )
    } else if(tab == "Numerical vs Numerical") {
      tagList(
        selectInput("var1", "Variable 1", choices = num_vars()),
        selectInput("var2", "Variable 2", choices = num_vars())
      )
    } else {
      tagList(
        selectInput("var1", "Variable 1", choices = cat_vars()),
        selectInput("var2", "Variable 2", choices = cat_vars())
      )
    }
  })
  
  output$plot_selector <- renderUI({
    tab <- input$bivar_tabs
    if(tab == "Category vs Numerical") {
      selectInput("plot_type", "Plot Type",
                  choices = c("Boxplot", "Violin", "Raincloud", "Ridgeline"))
    } else if(tab == "Numerical vs Numerical") {
      selectInput("plot_type", "Plot Type",
                  choices = c("Scatter"))
    } else {
      selectInput("plot_type", "Plot Type",
                  choices = c("Stacked Bar", "Mosaic"))
    }
  })
  
  last_clicked <- reactiveVal("eda")
  observeEvent(input$eda_btn, {
    last_clicked("eda")
  })
  observeEvent(input$cda_btn, {
    last_clicked("cda")
  })
  
  output$bivar_plot <- renderPlot({
    req(input$var1, input$var2, input$plot_type, input$bivar_tabs)
    x <- input$var1
    y <- input$var2
    type <- input$plot_type
    tab <- input$bivar_tabs
    
    # CATEGORY vs NUMERICAL
    if(tab == "Category vs Numerical") {
      if(last_clicked() == "cda") {
        ggstatsplot::ggbetweenstats(
          data = df,
          x = !!sym(x),
          y = !!sym(y),
          type = input$test_type,
          conf.level = as.numeric(input$conf_level)
        )
      } else if(type == "Boxplot") {
        ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
          geom_boxplot(fill = "#9e68aa", outliers = FALSE) +
          coord_flip()
      }
      else if(type == "Violin") {
        ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
          geom_violin(fill = "#25238d") +
          coord_flip()
      }
      else if(type == "Raincloud") {
        ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
          ggdist::stat_halfeye(adjust = 0.5, justification = -0.2) +
          geom_boxplot(width = 0.2) +
          coord_flip()
      }
      else if(type == "Ridgeline") {
        ggplot(df, aes(x = .data[[y]], y = .data[[x]])) +
          ggridges::geom_density_ridges()
      }
    }
    
    # NUMERICAL vs NUMERICAL
    else if(tab == "Numerical vs Numerical") {
      if(last_clicked() == "cda") {
        ggstatsplot::ggscatterstats(
          data = df,
          x = !!sym(x),
          y = !!sym(y),
          type = input$test_type,
          conf.level = as.numeric(input$conf_level), 
          palette = my_palette
        )
      }
      else if(type == "Scatter") {
        ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
          geom_point(alpha = 0.6) +
          geom_smooth()
      }
  }
    
    # CATEGORICAL vs CATEGORICAL
    else {
      if(last_clicked() == "cda") {
        ggstatsplot::ggbarstats(
          data = df,
          x = !!sym(x),
          y = !!sym(y),
          palette = "Dark2"
        )
      }
      else if(type == "Stacked Bar") {
        ggplot(df, aes(x = .data[[x]], fill = .data[[y]])) +
          geom_bar(position = "fill") +
          labs(y = "Proportion")
      }
      else if(type == "Mosaic") {
        ggplot(data = df) +
          ggmosaic::geom_mosaic(
            aes_string(x = paste0("product(", x, ")"),
                       fill = y)
          )
        }
      }
    }
  )
  
  output$stat_controls <- renderUI({
    tab <- input$bivar_tabs
    # Hide controls for Categorical vs Categorical
    if(tab == "Categorical vs Categorical") {
        return(NULL)
    }
    # Show controls for other cases
    else {
      tagList(
      selectInput("test_type", "Type of test",
                  choices = c("parametric", "nonparametric")),
      selectInput("conf_level", "Confidence level",
                  choices = c(0.90, 0.95, 0.99),
                  selected = 0.95)
    )
    }
  })
  
  #Multivariate
  observe({
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    numeric_vars <- setdiff(numeric_vars, "customer_id")
    
    updateCheckboxGroupInput(
      session, "multi_vars",
      choices = numeric_vars,
      selected = numeric_vars[1:5]
    )
  })

  
  output$corr_plot <- renderPlot({
    req(input$multi_vars)
    data <- df[, input$multi_vars, drop = FALSE]
    validate(
      need(ncol(data) >= 2, "Please select at least 2 variables")
    )
    
    corr_matrix <- cor(
      data,
      method = input$corr_method,
      use = "complete.obs"
    )
    
    corrplot::corrplot(
      corr_matrix,
      method = "ellipse",     
      type = "full",      
      order = "original",   
      diag = TRUE,          
      addCoef.col = "black",    
      tl.col = "black",         
      tl.srt = 90,               
      col = colorRampPalette(c("#b2182b", "white", "#2166ac"))(200) 
    )
  })
  
  
  #Segmentation
  # DEMOGRAPHIC
  demo_res <- reactive({
    req(input$demo_vars)
    clara(df[, input$demo_vars], k = input$demo_k)
  })
  
  output$plot_demo <- renderPlot({
    fviz_cluster(demo_res())
  })
  
  output$plot_demo_bar <- renderPlot({
    plot_cluster_bar(demo_res()$clustering)
  })
  output$plot_demo_heatmap <- renderPlot({
    plot_heatmap(demo_res()$clustering, df[, input$demo_vars])
  })

  output$plot_demo_sil <- renderPlot({
    plot_silhouette_clara(demo_res())
  })
  
  output$sil_demo <- renderText(round(demo_res()$silinfo$avg.width,4))
  output$ent_demo <- renderText(calc_entropy(demo_res()$clustering))
  output$aicbic_demo <- renderText("N/A")
  
  # TRANSACTIONAL
  trans_res <- reactive({
    data <- scale(df[, input$trans_vars])
    kmeans(data, centers = input$trans_k)
  })
  
  output$plot_trans <- renderPlot({
    fviz_cluster(trans_res(), data = scale(df[, input$trans_vars]))
  })
  
  output$plot_trans_bar <- renderPlot({
    plot_cluster_bar(trans_res()$cluster)
  })
  
  output$plot_trans_heatmap <- renderPlot({
    plot_heatmap(trans_res()$cluster, as.data.frame(scale(df[, input$trans_vars])))
  })
  
  output$plot_trans_elbow <- renderPlot({
    plot_elbow(scale(df[, input$trans_vars]))
  })
  
  output$plot_trans_sil <- renderPlot({
    plot_silhouette(trans_res(), scale(df[, input$trans_vars]))
  })
  
  output$sil_trans <- renderText(
    calc_sil_kmeans(trans_res(), scale(df[, input$trans_vars]))
  )
  output$ent_trans <- renderText(calc_entropy(trans_res()$cluster))
  output$aicbic_trans <- renderText(
    aic_bic_text(trans_res(), scale(df[, input$trans_vars]))
  )
  
  # PRODUCT USAGE
  usage_res <- reactive({
    clara(df[, input$usage_vars], k = input$usage_k)
  })
  
  output$plot_usage <- renderPlot({
    fviz_cluster(usage_res())
  })
  
  output$plot_usage_bar <- renderPlot({
    plot_cluster_bar(usage_res()$clustering)
  })
  output$plot_usage_heatmap <- renderPlot({
    plot_heatmap(usage_res()$clustering, df[, input$usage_vars])
  })
  output$plot_usage_sil <- renderPlot({
    plot_silhouette_clara(usage_res())
  })
  
  output$sil_usage <- renderText(round(usage_res()$silinfo$avg.width,4))
  output$ent_usage <- renderText(calc_entropy(usage_res()$clustering))
  output$aicbic_usage <- renderText("N/A")
  
  # SATISFACTION
  sat_res <- reactive({
    data <- scale(df[, input$sat_vars])
    kmeans(data, centers = input$sat_k)
  })
  
  output$plot_sat <- renderPlot({
    fviz_cluster(sat_res(), data = scale(df[, input$sat_vars]))
  })
  
  output$plot_sat_bar <- renderPlot({
    plot_cluster_bar(sat_res()$cluster)
  })
  output$plot_sat_heatmap <- renderPlot({
    plot_heatmap(sat_res()$cluster, as.data.frame(scale(df[, input$sat_vars])))
  })
  output$plot_sat_elbow <- renderPlot({
    plot_elbow(scale(df[, input$sat_vars]))
  })
  output$plot_sat_sil <- renderPlot({
    plot_silhouette(sat_res(), scale(df[, input$sat_vars]))
  })
  
  output$sil_sat <- renderText(
    calc_sil_kmeans(sat_res(), scale(df[, input$sat_vars]))
  )
  output$ent_sat <- renderText(calc_entropy(sat_res()$cluster))
  output$aicbic_sat <- renderText(
    aic_bic_text(sat_res(), scale(df[, input$sat_vars]))
  )
  
  
  # LOCATION TREEMAP
  cluster_data_reactive <- reactive({
    req(df, input$treemap_vars)
    
    # Use selected vars OR fallback to your original 3 vars
    selected_vars <- if(length(input$treemap_vars) > 0) {
      input$treemap_vars
    } else {
      c("total_tx_volume", "tx_count", "app_logins_frequency")
    }
    
    df %>%
      select(customer_id, all_of(selected_vars)) %>%
      drop_na() %>%
      mutate(across(-customer_id, scale))
  })
  
  # Reactive: k-means (uses treemap_k slider)
  trans_kmeans_reactive <- reactive({
    clust_dat <- cluster_data_reactive()
    set.seed(123)
    kmeans(clust_dat[,-1], centers = input$treemap_k)
  })
  
  
  final_df_reactive <- reactive({
    clust_dat <- cluster_data_reactive()
    km       <- trans_kmeans_reactive()
    
    clust_dat$segment_id <- factor(km$cluster)
    
    df %>%
      inner_join(
        clust_dat %>% select(customer_id, segment_id),
        by = "customer_id"
      )
  })
  
  segment_summary_reactive <- reactive({
    final_df_reactive() %>%
      group_by(segment_id) %>%
      summarise(
        avg_age   = round(mean(age, na.rm = TRUE), 1),
        top_income = names(sort(table(income_bracket), decreasing = TRUE))[1],
        avg_vol   = round(mean(total_tx_volume, na.rm = TRUE), 0),
        .groups   = "drop"
      ) %>%
      mutate(persona = case_when(
        segment_id == "1" ~ "Young Digital",
        segment_id == "2" ~ "Affluent Professional",
        segment_id == "3" ~ "Mass Market",
        segment_id == "4" ~ "High‑Value Senior",
        TRUE ~ as.character(segment_id)
      ))
  })
  
  tree_prep_labeled_reactive <- reactive({
    df_seg <- final_df_reactive()
    summ   <- segment_summary_reactive()
    
    df_seg %>%
      group_by(location, segment_id) %>%
      summarise(total_vol = sum(total_tx_volume, na.rm = TRUE), .groups = "drop") %>%
      left_join(summ, by = "segment_id") %>%
      mutate(label_text = paste0(persona, " | Avg Age: ", avg_age))
  })
  
  output$treemap <- renderPlot({
    req(tree_prep_labeled_reactive())
    df_tree <- tree_prep_labeled_reactive()
    
    treemap(df_tree,
            index = c("location", "label_text"), 
            vSize = "total_vol",
            vColor = "location",
            type = "index",
            title = paste("Customer Personas by City and Transaction Volume", "(k =", input$treemap_k, ")"),
            palette = "Set3",
            fontsize.labels = c(14, 9),          
            fontcolor.labels = c("black", "black"),
            border.col = c("black", "white"),
            border.lwds = c(4, 1),
            align.labels = list(c("center", "center"), c("left", "top"))
    )
  }, height = 400, width = 800)

  output$treemap_summary <- renderUI({
    summ  <- segment_summary_reactive()
    final <- final_df_reactive()
    
    # Top city by volume
    top_city <- final %>%
      group_by(location) %>%
      summarise(vol = sum(total_tx_volume, na.rm = TRUE)) %>%
      slice_max(vol, n = 1) %>%
      pull(location)
    
    # Most common persona nationally
    top_persona <- summ %>%
      left_join(
        final %>% count(segment_id),
        by = "segment_id"
      ) %>%
      slice_max(n, n = 1) %>%
      pull(persona)
    
    # Largest and smallest segment
    largest  <- summ %>% slice_max(avg_vol, n = 1) %>% pull(persona)
    smallest <- summ %>% slice_min(avg_vol, n = 1) %>% pull(persona)
    
    # Average age overall
    avg_age <- round(mean(final$age, na.rm = TRUE), 1)
    
    # No. of cities
    n_cities <- n_distinct(final$location)
    
    tagList(
      h4("Key Insights"),
      fluidRow(
        column(3,
               div(style = "background: #dbeafe; border-radius: 8px; padding: 8px 8px; margin: 4px; min-height: 100px;",
                   tags$b("Largest Market"),
                   tags$p(style = "margin-bottom: 6px;", top_city),
                   tags$b("Cities Covered"),
                   tags$p(style = "margin-bottom: 0;", n_cities)
               )
        ),
        column(3,
               div(style = "background: #dcfce7; border-radius: 8px; padding: 8px 8px; margin: 4px; min-height: 100px;",
                   tags$b("Most Common Persona"),
                   tags$p(style = "margin-bottom: 6px;", top_persona),
                   tags$b("Average Customer Age"),
                   tags$p(style = "margin-bottom: 0;", paste(avg_age, "years"))
               )
        ),
        column(4,
               div(style = "background: #fef9c3; border-radius: 8px; padding: 8px 8px; margin: 4px; min-height: 100px;",
                   tags$b("Highest Average Volume Segment"),
                   tags$p(style = "margin-bottom: 6px;", largest),
                   tags$b("Lowest Average Volume Segment"),
                   tags$p(style = "margin-bottom: 0;", smallest)
               )
            )
        )
      )
  })
  
  
}

  
