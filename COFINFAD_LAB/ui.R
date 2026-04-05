pacman::p_load(shiny, shinydashboard, shinythemes, rlang, RColorBrewer, corrplot, bslib, 
               plotly, tidyverse, ggstatsplot, tools, ggiraph, ggpubr, ggdist, ggridges, ggmosaic, tidytext, cluster, factoextra, fpc, treemap)

controls_ui <- function(id_prefix, vars, default_k = 4) {
  tagList(
    sliderInput(paste0(id_prefix, "_k"), "No. of Clusters",
                min = 1, max = 15, value = default_k),
    hr(),
    strong("Variables"),
    checkboxGroupInput(paste0(id_prefix, "_vars"), NULL,
                       choices = vars,
                       selected = vars)
  )
}

metrics_row_ui <- function(sil_id, ent_id, aicbic_id) {
  fluidRow(
    column(4, h4("Silhouette Width"), textOutput(sil_id)),
    column(4, h4("Entropy"), textOutput(ent_id)),
    column(4, h4("AIC / BIC"), textOutput(aicbic_id))
  )
}

var_pools <- list(
  demo  = c("age", "gender", "location", "income_bracket",
            "education_level", "marital_status", "occupation", "household_size"),
  trans = c("tx_count", "avg_tx_value", "total_tx_volume", "avg_daily_transactions", 
            "weekend_transaction_ratio", "international_transactions"),
  usage = c("active_products", "app_logins_frequency", "feature_usage_diversity", "bill_payment_user", "auto_savings_enabled"),
  sat   = c("satisfaction_score", "base_satisfaction", "tx_satisfaction", "product_satisfaction", "app_store_rating"),
  treemap = c("total_tx_volume", "tx_count", "app_logins_frequency") 
)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "COFINFAD LAB"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Variable Distribution", tabName = "dist"),
      menuItem("Comparing Variables", tabName = "compare", startExpanded = FALSE,
                menuSubItem("Bivariate", tabName = "biv"),
                menuSubItem("Multivariate", tabName = "multiv")),
      
      menuItem("Segmentation", tabName = "seg", startExpanded = FALSE,
               menuSubItem("Demographic", tabName = "demo"),
               menuSubItem("Transactional", tabName = "trans"),
               menuSubItem("Product Usage", tabName = "usage"),
               menuSubItem("Satisfaction", tabName = "sat"),
               menuSubItem("Location", tabName = "location")
      )
    )
  ),
  
  dashboardBody(
      theme = shinythemes::shinytheme("flatly"),
      tags$style(HTML("
  /* Pure white background */
  body, .content-wrapper, .right-side, .main-sidebar {
    background-color: #FFFFFF !important;
  }
  
  
  /* Remove dashboard skin tint */
  .skin-black .content-wrapper {
    background-color: #FFFFFF !important;
  }

  
  /* Keep your dark headers */
  .box-header {
    background-color: #020b21 !important;
    color: #FFFFFF !important;
  }
")),

    tabItems(
      tabItem(tabName = "intro",
              
              fluidRow(
                
                # LEFT: What you can do
                box(
                  title = "What you can do",
                  width = 3,
                  solidHeader = TRUE,
                  
                  p("Use this app to explore and analyse Colombian fintech customer data through interactive visualisations."),
                  p("Examine variable distributions, understand different customer segments, and identify meaningful patterns."),
                  tags$p("Data set used: ", 
                         tags$a(href = "https://data.mendeley.com/datasets/mhb4zn3258/1", "Colombian Fintech Financial Analytics Dataset", 
                                target = "_blank"))
                ),
                
                # RIGHT: Key Variables
                box(
                  title = "Customer Overview",
                  width = 9,
                  solidHeader = TRUE,
                  
                  fluidRow(
                    column(4, 
                           box(width = 12, title = "Number of Customers",
                               "48,723")
                    ),
                    column(4, 
                           box(width = 12, title = "Age",
                               "Mean: 44.5 years")
                    ),
                    column(4, 
                           box(width = 12, title = "Gender",
                               "Female: 49% | Male: 49%")
                    )
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(4, 
                           box(width = 12, title = "Active Products",
                               "Mean: 2 products")
                    ),
                    column(4, 
                           box(width = 12, title = "Average Transaction Value",
                               "Median: 1.76 million pesos")
                    ),
                    column(4, 
                           box(width = 12, title = "Customer Tenure",
                               "Mean: 11.3 months")
                      )
                  )
                )
              ),
              
              # BOTTOM: Variable Overview table
              fluidRow(
                box(
                  title = "Variable Overview",
                  width = 12,
                  solidHeader = TRUE,
                  
                  DT::dataTableOutput("var_table")
                )
              )
      ),
      
      
      tabItem(tabName = "dist",
              tabsetPanel(
                tabPanel("Demographics",
                         h4("Demographics"),
                         p("This section will examine the distribution of demographic variables that describe the characteristics of the customer base. The variables include age, gender, location, income_bracket, education_level, marital_status, occupation, and household_size."),
                         
                         br(),
                         
                         selectInput("demo_var", "Select Variable",
                                     choices = c("age", "gender", "location", "income_bracket",
                                                 "education_level", "marital_status",
                                                 "occupation", "household_size")),
                         
                         br(),
                         
                         plotOutput("demo_plot"),
                         conditionalPanel(
                           condition = "output.demo_is_numeric",
                           fluidRow(
                            column(1, style = "display: flex; align-items: flex-end; height: 55px;", textOutput("demo_var_name")),
                            column(2, align = "center", h5("Mean"), textOutput("demo_mean")),
                            column(2, align = "center", h5("Median"), textOutput("demo_median")),
                            column(2, align = "center", h5("Min"), textOutput("demo_min")),
                            column(2, align = "center", h5("Max"), textOutput("demo_max")),
                            column(2, align = "center", h5("SD"), textOutput("demo_sd"))
                         ))
                ),
                
                tabPanel("Transactions",
                         h4("Transaction Behaviour"),
                         p("This section explores variables related to customer financial activity. Key variables include tx_count, avg_tx_value, total_tx_volume, preferred_transaction_type, avg_daily_transactions, and weekend_transaction_ratio."),
                         
                         selectInput("tx_var", "Select Variable",
                                     choices = c("tx_count", "avg_tx_value", "total_tx_volume",
                                                 "preferred_transaction_type",
                                                 "avg_daily_transactions",
                                                 "weekend_transaction_ratio",
                                                 "international_transactions")),
                         
                         plotOutput("tx_plot"),
                         conditionalPanel(
                           condition = "output.tx_is_numeric",
                           fluidRow(
                            column(2, style = "display: flex; align-items: flex-end; height: 55px;", textOutput("tx_var_name")),
                            column(2, align = "center", h5("Mean"), textOutput("tx_mean")),
                            column(2, align = "center", h5("Median"), textOutput("tx_median")),
                            column(2, align = "center", h5("Min"), textOutput("tx_min")),
                            column(2, align = "center", h5("Max"), textOutput("tx_max")),
                            column(2, align = "center", h5("SD"), textOutput("tx_sd"))
                         ))
                ),
                
                tabPanel("Products",
                         h4("Product Ownership"),
                         p("This section analyses the distribution of financial products held by customers."),
                         
                         selectInput("product_var", "Select Variable",
                                     choices = c("savings_account", "credit_card", "personal_loan", "investment_account", "insurance_product", "active_products")),
                         
                         plotOutput("product_plot"),
                         conditionalPanel(
                           condition = "output.product_is_numeric",
                           fluidRow(
                            column(2, style = "display: flex; align-items: flex-end; height: 55px;", textOutput("product_var_name")),
                            column(2, align = "center", h5("Mean"), textOutput("product_mean")),
                            column(2, align = "center", h5("Median"), textOutput("product_median")),
                            column(2, align = "center", h5("Min"), textOutput("product_min")),
                            column(2, align = "center", h5("Max"), textOutput("product_max")),
                            column(2, align = "center", h5("SD"), textOutput("product_sd"))
                         ))
                ),
                
                tabPanel("App Usage",
                         h4("App Usage and Engagement"),
                         p("This section examines variables related to customer interaction with the digital platform."),
                         
                         selectInput("app_var", "Select Variable",
                                     choices = c("app_logins_frequency", "feature_usage_diversity", "bill_payment_user", "auto_savings_enabled")),
                         
                         plotOutput("app_plot"),
                         conditionalPanel(
                           condition = "output.usage_is_numeric",
                           fluidRow(
                            column(2, style = "display: flex; align-items: flex-end; height: 55px;", textOutput("app_var_name")),
                            column(2, align = "center", h5("Mean"), textOutput("app_mean")),
                            column(2, align = "center", h5("Median"), textOutput("app_median")),
                            column(2, align = "center", h5("Min"), textOutput("app_min")),
                            column(2, align = "center", h5("Max"), textOutput("app_max")),
                            column(2, align = "center", h5("SD"), textOutput("app_sd"))
                         ))
                ),
                
                tabPanel("Satisfaction",
                         h4("Customer Satisfaction"),
                         p("This section explores customer experience indicators."),
                         
                         selectInput("sat_var", "Select Variable",
                                     choices = c("satisfaction_score",
                                                 "base_satisfaction", "tx_satisfaction",
                                                 "product_satisfaction", "app_store_rating"
                                                 )),
                         
                         plotOutput("sat_plot"),
                         conditionalPanel(
                           condition = "output.sat_is_numeric",
                           fluidRow(
                            column(2, style = "display: flex; align-items: flex-end; height: 55px;", textOutput("sat_var_name")),
                            column(2, align = "center", h5("Mean"), textOutput("sat_mean")),
                            column(2, align = "center", h5("Median"), textOutput("sat_median")),
                            column(2, align = "center", h5("Min"), textOutput("sat_min")),
                            column(2, align = "center", h5("Max"), textOutput("sat_max")),
                            column(2, align = "center", h5("SD"), textOutput("sat_sd"))
                         ))
                )
              )
      ),
      
      tabItem(tabName = "biv",
              fluidRow(
                # LEFT PANEL (controls)
                column(3,
                       h4("Step 1: Visualisation"),
                       uiOutput("bivar_inputs"),
                       
                       
                       uiOutput("stat_controls")
                ),
                
                # RIGHT PANEL (tabs)
                column(9,
                       tabsetPanel(
                         id = "bivar_tabs",
                         tabPanel("Category vs Numerical"),
                         tabPanel("Numerical vs Numerical"),
                         tabPanel("Categorical vs Categorical")
                       ),
                       
                       br(),
                       
                       plotOutput("bivar_plot", height = "450px")
                )
              )
      ),
      

      tabItem(tabName = "multiv",
              
              fluidRow(
                # LEFT PANEL (controls)
                column(3,
                       h4("Multivariate Overview"),
                       p("Explore relationships across multiple numerical variables using a correlation matrix."),
                       checkboxGroupInput(inputId = "multi_vars",
                                          label = "Select Variables",
                                          choices = NULL ),
                       selectInput("corr_method",
                                   "Correlation Method",
                                   choices = c("pearson", "spearman"),
                                   selected = "Pearson"),
                       actionButton("multi_run", "Generate")
                ),
                # RIGHT PANEL (output)
                column(9,
                       plotOutput("corr_plot", height = "500px")
                )
              )
      ),
      
      # SEGMENTATION
      tabItem(tabName = "demo",
              fluidRow(
                column(3, controls_ui("demo", var_pools$demo, 4)),
                column(9,
                       tabsetPanel(
                         tabPanel("Cluster Evaluation",
                                  h4("Cluster Evaluation"),
                                  p("The silhouette plot measures how well each customer fits their assigned cluster. Each bar 
                                  (or filled region) represents individual customers within a cluster, and the red dashed line 
                                    marks the overall average silhouette width."),
                                  
                                  plotOutput("plot_demo_sil", height = "300px"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                    ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Scores close to 1 indicate a customer is well-matched to their cluster and clearly separated from neighbouring clusters"),
                                      tags$li("Negative scores suggest the customer may have been assigned to the wrong cluster"),
                                      tags$li("Clusters where most members exceed the average line are well-defined; clusters with many members falling below it may benefit from a different value of k"),
                                      tags$li("A mean silhouette width above 0.5 reflects strong structure; 0.25–0.5 is moderate; below 0.25 suggests weak or arbitrary groupings")
                                    )
                                  )
                         ),
                         
                         tabPanel("Cluster Plot",
                                  h4("Cluster Plot"),
                                  p("This plot projects all customers onto a 2D space using Principal Component Analysis (PCA),
                                  where each point represents a customer coloured by their assigned cluster. The axes (Dim1,
                                  Dim2) are linear combinations of the input variables, and the percentage shown indicates
                                    how much of the total variance each dimension explains."),
                                  
                                  plotOutput("plot_demo", height = "350px"),
                                  metrics_row_ui("sil_demo", "ent_demo", "aicbic_demo"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Well-separated, compact clusters indicate strong, meaningful segmentation"),
                                      tags$li("Heavily overlapping clusters suggest the variables may not clearly distinguish customer groups"),
                                      tags$li("Silhouette Width: Higher is better (max = 1); values above 0.5 indicate strong cluster separation, 0.25–0.5 is moderate."),
                                      tags$li("Entropy: Lower is better; indicates meaningfully uneven cluster sizes rather than arbitrary equal splits."),
                                      tags$li("AIC / BIC: N/A for CLARA-based clustering.")
                                      )
                                    )
                                  ),
                         
                         tabPanel("Cluster Composition",
                                  h4("Cluster Composition"),
                                  p("This bar chart shows the proportion of customers assigned to each cluster, helping you assess
                                    whether the segmentation produces meaningfully sized groups."),
                                  
                                  plotOutput("plot_demo_bar", height = "350px"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Balanced clusters (roughly equal proportions) suggest the algorithm has found evenly distributed natural groupings"),
                                      tags$li("A dominant cluster (e.g. >40%) may indicate a large average segment that absorbs customers who do not strongly belong elsewhere. Consider increasing k to break it down further"),
                                      tags$li("Very small clusters (<10%) may represent genuine niche segments or outliers; inspect their profiles carefully before acting on them")
                                      )
                                    )
                                  )
                         )
                       )
                )
              ),
      
      tabItem(tabName = "trans",
              fluidRow(
                column(3, controls_ui("trans", var_pools$trans, 4)),
                column(9,
                       tabsetPanel(
                         tabPanel("Cluster Evaluation",
                                  h4("Cluster Evaluation"),
                                  fluidRow(
                                    column(6,
                                           p("The elbow plot helps determine the optimal number of clusters (k) by showing how total 
                                    within-cluster sum of squares (WSS) decreases as k increases. A lower WSS indicates that 
                                      customers within each cluster are more similar to one another."),
                                           
                                           plotOutput("plot_trans_elbow", height = "300px"),
                                           
                                           tags$details(
                                             style = "margin-top: 10px;",
                                             tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                          "Click here to understand how to interpret the plot"
                                             ),
                                             tags$ul(
                                               style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                               tags$li("A sharp drop in WSS as k increases suggests each additional cluster is capturing meaningful structure in the data"),
                                               tags$li("The elbow, where the rate of decrease slows and the curve begins to flatten, indicates the optimal k, beyond which adding more clusters yields diminishing returns"),
                                               tags$li("A gradual curve with no clear elbow suggests the data does not have strong natural cluster structure, and any choice of k is somewhat arbitrary"),
                                               tags$li("Use this plot alongside the silhouette plot to confirm your choice of k. The optimal k from the elbow plot should ideally also correspond to a higher silhouette width")
                                             )
                                           )
                                    ),
                                    
                                    column(6,
                                           p("The silhouette plot measures how well each customer fits their assigned cluster. Each bar 
                                    (or filled region) represents individual customers within a cluster, and the red dashed line 
                                      marks the overall average silhouette width."),
                                           
                                           plotOutput("plot_trans_sil", height = "300px"),
                                           
                                           tags$details(
                                             style = "margin-top: 10px;",
                                             tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                          "Click here to understand how to interpret the plot"
                                             ),
                                             tags$ul(
                                               style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                               tags$li("Scores close to 1 indicate a customer is well-matched to their cluster and clearly separated from neighbouring clusters"),
                                               tags$li("Negative scores suggest the customer may have been assigned to the wrong cluster"),
                                               tags$li("Clusters where most members exceed the average line are well-defined; clusters with many members falling below it may benefit from a different value of k"),
                                               tags$li("A mean silhouette width above 0.5 reflects strong structure; 0.25–0.5 is moderate; below 0.25 suggests weak or arbitrary groupings")
                                             )
                                           )
                                    )
                                  )
                         ),
                         
                         tabPanel("Cluster Plot",
                                  h4("Cluster Plot"),
                                  p("This plot projects all customers onto a 2D space using Principal Component Analysis (PCA),
                                  where each point represents a customer coloured by their assigned cluster. The axes (Dim1,
                                  Dim2) are linear combinations of the input variables, and the percentage shown indicates
                                    how much of the total variance each dimension explains."),
                                  
                                  plotOutput("plot_trans", height = "350px"),
                                  metrics_row_ui("sil_trans", "ent_trans", "aicbic_trans"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Well-separated, compact clusters indicate strong, meaningful segmentation"),
                                      tags$li("Heavily overlapping clusters suggest the variables may not clearly distinguish customer groups"),
                                      tags$li("Silhouette Width: Higher is better (max = 1); values above 0.5 indicate strong cluster separation, 0.25–0.5 is moderate."),
                                      tags$li("Entropy: Lower is better; indicates meaningfully uneven cluster sizes rather than arbitrary equal splits."),
                                      tags$li("AIC / BIC: Lower is better; a drop as k increases suggests real structure; a plateau suggests overfitting.")
                                      )
                                    )
                                  ),
                         
                         tabPanel("Cluster Composition",
                                  h4("Cluster Composition"),
                                  p("This bar chart shows the proportion of customers assigned to each cluster, helping you assess
                                    whether the segmentation produces meaningfully sized groups."),
                                  
                                  plotOutput("plot_trans_bar", height = "350px"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Balanced clusters (roughly equal proportions) suggest the algorithm has found evenly distributed natural groupings"),
                                      tags$li("A dominant cluster (e.g. >40%) may indicate a large average segment that absorbs customers who do not strongly belong elsewhere; consider increasing k to break it down further"),
                                      tags$li("Very small clusters (<10%) may represent genuine niche segments or outliers; inspect their profiles carefully before acting on them")
                                      )
                                    )
                                  )
                       )
                )
              )
      ),
      
      tabItem(tabName = "usage",
              fluidRow(
                column(3, controls_ui("usage", var_pools$usage, 4)),
                column(9,
                       tabsetPanel(
                         tabPanel("Cluster Evaluation",
                                  h4("Cluster Evaluation"),
                                  p("The silhouette plot measures how well each customer fits their assigned cluster. Each bar 
                                  (or filled region) represents individual customers within a cluster, and the red dashed line 
                                    marks the overall average silhouette width."),
                                  
                                  plotOutput("plot_usage_sil", height = "300px"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                    ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Scores close to 1 indicate a customer is well-matched to their cluster and clearly separated from neighbouring clusters"),
                                      tags$li("Negative scores suggest the customer may have been assigned to the wrong cluster"),
                                      tags$li("Clusters where most members exceed the average line are well-defined; clusters with many members falling below it may benefit from a different value of k"),
                                      tags$li("A mean silhouette width above 0.5 reflects strong structure; 0.25–0.5 is moderate; below 0.25 suggests weak or arbitrary groupings")
                                    )
                                  )
                         ),
                         
                         tabPanel("Cluster Plot",
                                  h4("Cluster Plot"),
                                  p("This plot projects all customers onto a 2D space using Principal Component Analysis (PCA),
                                  where each point represents a customer coloured by their assigned cluster. The axes (Dim1,
                                  Dim2) are linear combinations of the input variables, and the percentage shown indicates
                                    how much of the total variance each dimension explains."),
                                            
                                   plotOutput("plot_usage", height = "350px"),
                                   metrics_row_ui("sil_usage", "ent_usage", "aicbic_usage"),
                                            
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                           "Click here to understand how to interpret the plot"
                                                 ),
                                   tags$ul(
                                     style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                     tags$li("Well-separated, compact clusters indicate strong, meaningful segmentation"),
                                     tags$li("Heavily overlapping clusters suggest the variables may not clearly distinguish customer groups"),
                                     tags$li("Silhouette Width: Higher is better (max = 1); values above 0.5 indicate strong cluster separation, 0.25–0.5 is moderate."),
                                     tags$li("Entropy: Lower is better; indicates meaningfully uneven cluster sizes rather than arbitrary equal splits."),
                                     tags$li("AIC / BIC: N/A for CLARA-based clustering.")
                                     )
                                   )
                                  ),
                         tabPanel("Cluster Composition",
                                  h4("Cluster Composition"),
                                  p("This bar chart shows the proportion of customers assigned to each cluster, helping you assess
                                    whether the segmentation produces meaningfully sized groups."),
                                  
                                  plotOutput("plot_usage_bar", height = "350px"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Balanced clusters (roughly equal proportions) suggest the algorithm has found evenly distributed natural groupings"),
                                      tags$li("A dominant cluster (e.g. >40%) may indicate a large average segment that absorbs customers who do not strongly belong elsewhere. Consider increasing k to break it down further"),
                                      tags$li("Very small clusters (<10%) may represent genuine niche segments or outliers; inspect their profiles carefully before acting on them")
                                      )
                                    )
                                  )
                         )
                       )
                )
              ),
      
      
      tabItem(tabName = "sat",
              fluidRow(
                column(3, controls_ui("sat", var_pools$sat, 4)),
                column(9,
                       tabsetPanel(
                         tabPanel("Cluster Evaluation",
                                  h4("Cluster Evaluation"),
                                  fluidRow(
                                    column(6,
                                           p("The elbow plot helps determine the optimal number of clusters (k) by showing how total 
                                    within-cluster sum of squares (WSS) decreases as k increases. A lower WSS indicates that 
                                      customers within each cluster are more similar to one another."),
                                           
                                           plotOutput("plot_sat_elbow", height = "300px"),
                                           
                                           tags$details(
                                             style = "margin-top: 10px;",
                                             tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                          "Click here to understand how to interpret the plot"
                                             ),
                                             tags$ul(
                                               style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                               tags$li("A sharp drop in WSS as k increases suggests each additional cluster is capturing meaningful structure in the data"),
                                               tags$li("The elbow, where the rate of decrease slows and the curve begins to flatten, indicates the optimal k, beyond which adding more clusters yields diminishing returns"),
                                               tags$li("A gradual curve with no clear elbow suggests the data does not have strong natural cluster structure, and any choice of k is somewhat arbitrary"),
                                               tags$li("Use this plot alongside the silhouette plot to confirm your choice of k. The optimal k from the elbow plot should ideally also correspond to a higher silhouette width")
                                             )
                                           )
                                    ),
                                    
                                    column(6,
                                           p("The silhouette plot measures how well each customer fits their assigned cluster. Each bar 
                                    (or filled region) represents individual customers within a cluster, and the red dashed line 
                                      marks the overall average silhouette width."),
                                           
                                           plotOutput("plot_sat_sil", height = "300px"),
                                           
                                           tags$details(
                                             style = "margin-top: 10px;",
                                             tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                          "Click here to understand how to interpret the plot"
                                             ),
                                             tags$ul(
                                               style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                               tags$li("Scores close to 1 indicate a customer is well-matched to their cluster and clearly separated from neighbouring clusters"),
                                               tags$li("Negative scores suggest the customer may have been assigned to the wrong cluster"),
                                               tags$li("Clusters where most members exceed the average line are well-defined; clusters with many members falling below it may benefit from a different value of k"),
                                               tags$li("A mean silhouette width above 0.5 reflects strong structure; 0.25–0.5 is moderate; below 0.25 suggests weak or arbitrary groupings")
                                             )
                                           )
                                    )
                                  )
                         ),
                         
                         tabPanel("Cluster Plot",
                                  h4("Cluster Plot"),
                                  p("This plot projects all customers onto a 2D space using Principal Component Analysis (PCA),
                                  where each point represents a customer coloured by their assigned cluster. The axes (Dim1,
                                  Dim2) are linear combinations of the input variables, and the percentage shown indicates
                                    how much of the total variance each dimension explains."),
                                  
                                  plotOutput("plot_sat", height = "350px"),
                                  metrics_row_ui("sil_sat", "ent_sat", "aicbic_sat"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Well-separated, compact clusters indicate strong, meaningful segmentation"),
                                      tags$li("Heavily overlapping clusters suggest the variables may not clearly distinguish customer groups"),
                                      tags$li("Silhouette Width: Higher is better (max = 1); values above 0.5 indicate strong cluster separation, 0.25–0.5 is moderate."),
                                      tags$li("Entropy: Lower is better; indicates meaningfully uneven cluster sizes rather than arbitrary equal splits."),
                                      tags$li("AIC / BIC: Lower is better; a drop as k increases suggests real structure; a plateau suggests overfitting.")
                                      )
                                  )
                         ),
                         
                         tabPanel("Cluster Composition",
                                  h4("Cluster Composition"),
                                  p("This bar chart shows the proportion of customers assigned to each cluster, helping you assess
                                    whether the segmentation produces meaningfully sized groups."),
                                  
                                  plotOutput("plot_sat_bar", height = "350px"),
                                  
                                  tags$details(
                                    style = "margin-top: 10px;",
                                    tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0;",
                                                 "Click here to understand how to interpret the plot"
                                                 ),
                                    tags$ul(
                                      style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                                      tags$li("Balanced clusters (roughly equal proportions) suggest the algorithm has found evenly distributed natural groupings"),
                                      tags$li("A dominant cluster (e.g. >40%) may indicate a large average segment that absorbs customers who do not strongly belong elsewhere. Consider increasing k to break it down further"),
                                      tags$li("Very small clusters (<10%) may represent genuine niche segments or outliers; inspect their profiles carefully before acting on them")
                                      )
                                    )
                                  )
                       )
                )
              )
      ),
      
      tabItem(tabName = "location",
              fluidRow(
                column(3, controls_ui("treemap", var_pools$treemap, 4)),
                column(9,
                       h4("Treemap of customer segments across Colombian cities, sized by total transaction volume"),
                       p("Each large rectangle represents a city, and within each city, smaller rectangles show the breakdown of customer personas derived from 
                         k-means clustering, labelled with the persona name and average customer age."),
                       
                       plotOutput("treemap"),
                       uiOutput("treemap_summary"),
                       
                       tags$details(
                         style = "margin-top: 10px;",
                         tags$summary(style = "cursor: pointer; color: #6b7280; font-size: 14px; padding: 4px 0; list-style: disclosure-closed;",
                                                "Click here to understand how to interpret the treemap"),
                         tags$ul(
                           style = "padding-left: 16px; margin-top: 8px; font-size: 14px; color: #374151;",
                           tags$li("Larger city rectangles indicate higher transaction volume markets — prioritise these for resource allocation"),
                           tags$li("The dominant persona in a city (largest sub-rectangle) reflects the primary customer profile in that market"),
                           tags$li("Cities where high-value personas dominate despite smaller overall size may represent untapped premium markets"),
                           tags$li("Uniform persona distribution across cities suggests segments are nationally distributed rather than geographically concentrated"),
                           tags$li("Adjust the number of clusters (k) and variables on the left to explore different segmentation scenarios")
                           )
                         )
                       )
                )
              )
      )
  )
)
