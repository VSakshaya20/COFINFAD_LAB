pacman::p_load(shiny, shinydashboard, shinythemes, rlang,
               plotly, tidyverse, ggstatsplot, tools, ggiraph, ggpubr, ggdist, ggridges, ggmosaic, tidytext, cluster, factoextra, fpc, treemap)

df <- read_csv("../data/customer_data.csv")

df <- df[, !(names(df) %in% c("customer_segment", "nps_score", "last_survey_date", "feedback_sentiment", "monthly_transaction_count", 
                              "average_transaction_value", "total_transaction_volume", "transaction_frequency", "last_transaction_date", 
                              "first_transaction_date", "churn_probability", "clv_segment", "customer_lifetime_value"))]

df$customer_id <- as.character(df$customer_id)

df$gender <- as.factor(df$gender)
df$income_bracket <- as.factor(df$income_bracket)
df$occupation <- as.factor(df$occupation)
df$education_level <- as.factor(df$education_level)
df$marital_status <- as.factor(df$marital_status)
df$acquisition_channel <- as.factor(df$acquisition_channel)
df$preferred_transaction_type <- as.factor(df$preferred_transaction_type)
df$location <- as.factor(df$location)

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
  
  output$bivar_plot <- renderPlot({
    req(input$eda_btn)
    x <- input$var1
    y <- input$var2
    type <- input$plot_type
    tab <- input$bivar_tabs
    
    # CATEGORY vs NUMERICAL
    if(tab == "Category vs Numerical") {
      if(input$cda_btn > 0) {
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
        ggridges::ggplot(df,
                         aes(x = .data[[y]], y = .data[[x]])) +
          ggridges::geom_density_ridges()
      }
    }
    
    # NUMERICAL vs NUMERICAL
    else if(tab == "Numerical vs Numerical") {
      if(input$cda_btn > 0) {
        ggstatsplot::ggscatterstats(
          data = df,
          x = !!sym(x),
          y = !!sym(y),
          type = input$test_type,
          conf.level = as.numeric(input$conf_level)
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
      if(type == "Stacked Bar") {
        ggplot(df, aes(x = .data[[x]], fill = .data[[y]])) +
          geom_bar(position = "fill") +
          labs(y = "Proportion")
      }
      else if(type == "Mosaic") {
        ggplot(df) +
          ggmosaic::geom_mosaic(
            aes(x = product(.data[[x]]),
                fill = .data[[y]])
          )
      }
    }
  }
)
  
  #Segmentation
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
  
  usage_res <- reactive({
    clara(df[, input$usage_vars], k = input$usage_k)
  })
  
  output$plot_usage <- renderPlot({
    fviz_cluster(usage_res())
  })
  
  output$sil_usage <- renderText(round(usage_res()$silinfo$avg.width,4))
  output$ent_usage <- renderText(calc_entropy(usage_res()$clustering))
  output$aicbic_usage <- renderText("N/A")
  
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
  
  
  
  
  
  
  
}

  
