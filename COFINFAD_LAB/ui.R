pacman::p_load(shiny, shinydashboard, shinythemes, dashboardthemes, rlang, 
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
  demo  = c("age", "gender", "occupation", "income_bracket",
            "education_level", "marital_status", "household_size"),
  trans = c("tx_count", "avg_tx_value", "total_tx_volume",
            "avg_daily_transactions", "weekend_transaction_ratio"),
  usage = c("active_products", "app_logins_frequency",
            "feature_usage_diversity", "bill_payment_user"),
  sat   = c("satisfaction_score", "product_satisfaction",
            "app_store_rating")
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
               menuSubItem("Behaviour", tabName = "behaviour"),
               menuSubItem("Product Usage", tabName = "product"),
               menuSubItem("Satisfaction", tabName = "satisfaction"),
               menuSubItem("Location", tabName = "location")
      )
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "onenote"  
    ),
    tags$style(HTML("

  .box-header {
    background-color: #020b21 !important;
    border-bottom: 1px solid #ddd;
  }

  .box-title {
    color: #f7f6ee !important;
    font-weight: 600;
  }

")),

    tabItems(
      tabItem(tabName = "intro",
              
              fluidRow(
                
                # LEFT: What you can do
                box(
                  title = "What you can do",
                  width = 4,
                  solidHeader = TRUE,
                  
                  p("Use this app to explore and analyse Colombian fintech customer data through interactive visualisations."),
                  p("Examine variable distributions, understand different customer segments, and identify meaningful patterns.")
                ),
                
                # RIGHT: Key Variables
                box(
                  title = "Key Variables",
                  width = 6,
                  solidHeader = TRUE,
                  
                  fluidRow(
                    column(4, 
                           box(width = 12, title = "Age",
                               "Mean: 35", br(),
                               "Median: 33")
                    ),
                    column(4, 
                           box(width = 12, title = "Income",
                               "Mean: 50k", br(),
                               "Median: 45k")
                    ),
                    column(4, 
                           box(width = 12, title = "Score",
                               "Mean: 700", br(),
                               "Median: 680")
                    )
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(4, 
                           box(width = 12, title = "Spending",
                               "Mean: 200", br(),
                               "Median: 180")
                    ),
                    column(4, 
                           box(width = 12, title = "Visits",
                               "Mean: 5", br(),
                               "Median: 4")
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
              
              tabBox(
                width = 12,
                
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
                         fluidRow(
                           column(2, align = "center", h5("Mean"), textOutput("demo_mean")),
                           column(2, align = "center", h5("Median"), textOutput("demo_median")),
                           column(2, align = "center", h5("Min"), textOutput("demo_min")),
                           column(2, align = "center", h5("Max"), textOutput("demo_max")),
                           column(2, align = "center", h5("SD"), textOutput("demo_sd"))
                         )
                ),
                
                tabPanel("Transactions",
                         h4("Transaction Behaviour"),
                         p("This section explores variables related to customer financial activity. Key variables include tx_count, avg_tx_value, total_tx_volume, preferred_transaction_type, avg_daily_transactions, and weekend_transaction_ratio."),
                         
                         selectInput("tx_var", "Select Variable",
                                     choices = c("tx_count", "avg_tx_value", "total_tx_volume",
                                                 "preferred_transaction_type",
                                                 "avg_daily_transactions",
                                                 "weekend_transaction_ratio")),
                         
                         plotOutput("tx_plot"),
                         fluidRow(
                           column(2, align = "center", h5("Mean"), textOutput("tx_mean")),
                           column(2, align = "center", h5("Median"), textOutput("tx_median")),
                           column(2, align = "center", h5("Min"), textOutput("tx_min")),
                           column(2, align = "center", h5("Max"), textOutput("tx_max")),
                           column(2, align = "center", h5("SD"), textOutput("tx_sd"))
                         )
                ),
                
                tabPanel("Products",
                         h4("Product Ownership"),
                         p("This section analyses the distribution of financial products held by customers."),
                         
                         selectInput("product_var", "Select Variable",
                                     choices = c("savings_account", "credit_card", "personal_loan", "investment_account", "insurance_product", "active_products")),
                         
                         plotOutput("product_plot"),
                         fluidRow(
                           column(2, align = "center", h5("Mean"), textOutput("product_mean")),
                           column(2, align = "center", h5("Median"), textOutput("product_median")),
                           column(2, align = "center", h5("Min"), textOutput("product_min")),
                           column(2, align = "center", h5("Max"), textOutput("product_max")),
                           column(2, align = "center", h5("SD"), textOutput("product_sd"))
                         )
                ),
                
                tabPanel("App Usage",
                         h4("App Usage and Engagement"),
                         p("This section examines variables related to customer interaction with the digital platform."),
                         
                         selectInput("app_var", "Select Variable",
                                     choices = c("app_logins_frequency", "feature_usage_diversity", "bill_payment_user", "auto_savings_enabled")),
                         
                         plotOutput("app_plot"),
                         fluidRow(
                           column(2, align = "center", h5("Mean"), textOutput("app_mean")),
                           column(2, align = "center", h5("Median"), textOutput("app_median")),
                           column(2, align = "center", h5("Min"), textOutput("app_min")),
                           column(2, align = "center", h5("Max"), textOutput("app_max")),
                           column(2, align = "center", h5("SD"), textOutput("app_sd"))
                         )
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
                         fluidRow(
                           column(2, align = "center", h5("Mean"), textOutput("sat_mean")),
                           column(2, align = "center", h5("Median"), textOutput("sat_median")),
                           column(2, align = "center", h5("Min"), textOutput("sat_min")),
                           column(2, align = "center", h5("Max"), textOutput("sat_max")),
                           column(2, align = "center", h5("SD"), textOutput("sat_sd"))
                         )
                )
              )
      ),
      
      tabItem(tabName = "biv",
              fluidRow(
                # LEFT PANEL (controls)
                column(3,
                       h4("Step 1: Visualisation"),
                       uiOutput("bivar_inputs"),
                       uiOutput("plot_selector"),
                       actionButton("eda_btn", "Analyse"),
                       
                       br(), br(),
                       
                       h4("Step 2: Statistical Validity"),
                       uiOutput("stat_controls"),
                       actionButton("cda_btn", "Analyse")
          
                       
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
      

      tabItem(tabName = "multiv"),
      
      tabItem(tabName = "demo",
                      
                      fluidRow(
                        column(3,
                               controls_ui("demo", var_pools$demo, 4)
                        ),
                        column(9,
                               plotOutput("plot_demo", height = "400px"),
                               metrics_row_ui("sil_demo", "ent_demo", "aicbic_demo")
                        )
                      )
              ),
      tabItem(tabName = "behaviour",
                      
                      fluidRow(
                        column(3,
                               controls_ui("trans", var_pools$trans, 5)
                        ),
                        column(9,
                               plotOutput("plot_trans", height = "400px"),
                               metrics_row_ui("sil_trans", "ent_trans", "aicbic_trans")
                        )
                      )
              ),
      tabItem(tabName = "product",
              fluidRow(
                column(3,
                       controls_ui("usage", var_pools$usage, 3)
                ),
                column(9,
                       plotOutput("plot_usage", height = "400px"),
                       metrics_row_ui("sil_usage", "ent_usage", "aicbic_usage")
                )
              )),
      tabItem(tabName = "satisfaction",
              fluidRow(
                column(3,
                       controls_ui("sat", var_pools$sat, 3)
                ),
                column(9,
                       plotOutput("plot_sat", height = "400px"),
                       metrics_row_ui("sil_sat", "ent_sat", "aicbic_sat")
                )
              )),
      tabItem(tabName = "location")
      
    )
  )
)