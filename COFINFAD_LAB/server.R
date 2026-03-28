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

calc_entropy <- function(clusters) {
  props <- table(clusters) / length(clusters)
  -sum(props * log(props + 1e-10))
}

calc_sil_kmeans <- function(model, data) {
  sil <- silhouette(model$cluster, dist(data))
  mean(sil[, 3])
}

aic_bic_text <- function(model, data) {
  k <- nrow(model$centers)
  n <- nrow(data)
  p <- ncol(data)
  aic <- model$tot.withinss + 2 * k * p
  bic <- model$tot.withinss + log(n) * k * p
  paste0("AIC: ", round(aic,1), " | BIC: ", round(bic,1))
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
  
  output$demo_mean   <- renderText({ if(is.numeric(demo_var_data())) mean(demo_var_data(), na.rm = TRUE) else "" })
  output$demo_median <- renderText({ if(is.numeric(demo_var_data())) median(demo_var_data(), na.rm = TRUE) else "" })
  output$demo_min    <- renderText({ if(is.numeric(demo_var_data())) min(demo_var_data(), na.rm = TRUE) else "" })
  output$demo_max    <- renderText({ if(is.numeric(demo_var_data())) max(demo_var_data(), na.rm = TRUE) else "" })
  output$demo_sd     <- renderText({ if(is.numeric(demo_var_data())) sd(demo_var_data(), na.rm = TRUE) else "" })
  
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
        theme_minimal()
    }
  })
  
  output$tx_mean   <- renderText({ if(is.numeric(tx_var_data())) mean(tx_var_data(), na.rm = TRUE) else "" })
  output$tx_median <- renderText({ if(is.numeric(tx_var_data())) median(tx_var_data(), na.rm = TRUE) else "" })
  output$tx_min    <- renderText({ if(is.numeric(tx_var_data())) min(tx_var_data(), na.rm = TRUE) else "" })
  output$tx_max    <- renderText({ if(is.numeric(tx_var_data())) max(tx_var_data(), na.rm = TRUE) else "" })
  output$tx_sd     <- renderText({ if(is.numeric(tx_var_data())) sd(tx_var_data(), na.rm = TRUE) else "" })
  
  #Product
  output$product_plot <- renderPlot({
    
    var_name <- input$product_var
    
    ggplot(df, aes_string(x = var_name)) +
      geom_bar(fill = "#e67e22") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
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
  
  # APP USAGE
  output$app_plot <- renderPlot({
    
    var_name <- input$app_var
    
    ggplot(df, aes_string(x = var_name)) +
      geom_bar(fill = "#1abc9c") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
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
  
  # SATISFACTION
  output$sat_plot <- renderPlot({
    
    var_name <- input$sat_var
    
    if(var_name %in% c("satisfaction_score", "product_satisfaction")) {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#9b59b6") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
        theme_minimal()
      
    } else if(is.numeric(df[[var_name]])) {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_histogram(fill = "#9b59b6", bins = 30) +
        theme_minimal()
      
    } else {
      
      ggplot(df, aes_string(x = var_name)) +
        geom_bar(fill = "#9b59b6") +
        theme_minimal()
    }
  })
  
  output$sat_mean   <- renderText({ if(is.numeric(sat_var_data())) mean(sat_var_data(), na.rm = TRUE) else "" })
  output$sat_median <- renderText({ if(is.numeric(sat_var_data())) median(sat_var_data(), na.rm = TRUE) else "" })
  output$sat_min    <- renderText({ if(is.numeric(sat_var_data())) min(sat_var_data(), na.rm = TRUE) else "" })
  output$sat_max    <- renderText({ if(is.numeric(sat_var_data())) max(sat_var_data(), na.rm = TRUE) else "" })
  output$sat_sd     <- renderText({ if(is.numeric(sat_var_data())) sd(sat_var_data(), na.rm = TRUE) else "" })
  
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
          geom_boxplot(fill = "#3498db") +
          coord_flip()
      }
      else if(type == "Violin") {
        ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
          geom_violin(fill = "#3498db") +
          coord_flip()
      }
      else if(type == "Raincloud") {
        ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
          ggdist::stat_halfeye(adjust = 0.5, justification = -0.2) +
          geom_boxplot(width = 0.2) +
          coord_flip()
      }
      else if(type == "Ridgeline") {
        ggplot(df,
                         aes(x = .data[[y]], y = .data[[x]])) +
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
    updateCheckboxGroupInput(session, "multi_vars",
                             choices = names(df)[sapply(df, is.numeric)],
                             selected = names(df)[sapply(df, is.numeric)][1:5])
    
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
  
  output$sil_trans <- renderText(
    calc_sil_kmeans(trans_res(), scale(df[, input$trans_vars]))
  )
  output$ent_trans <- renderText(calc_entropy(trans_res()$cluster))
  output$aicbic_trans <- renderText(
    aic_bic_text(trans_res(), scale(df[, input$trans_vars]))
  )
  
  # USAGE
  usage_res <- reactive({
    clara(df[, input$usage_vars], k = input$usage_k)
  })
  
  output$plot_usage <- renderPlot({
    fviz_cluster(usage_res())
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
      mutate(label_text = paste0(persona, "\\nAvg Age: ", avg_age))
  })
  
  output$treemap <- renderPlot({
    req(tree_prep_labeled_reactive())
    df_tree <- tree_prep_labeled_reactive()
    
    treemap(df_tree,
            index = c("location", "label_text"), 
            vSize = "total_vol",
            vColor = "location",
            type = "index",
            title = paste("Geographic Segments by", paste(input$treemap_vars, collapse = ", "), "(k =", input$treemap_k, ")"),
            palette = "Set3",
            fontsize.labels = c(15, 8),          
            fontcolor.labels = c("black", "darkslategrey"),
            border.col = c("black", "white"),
            border.lwds = c(4, 1),
            align.labels = list(c("center", "center"), c("left", "top"))
    )
  }, height = 600, width = 900)
  
}

  
