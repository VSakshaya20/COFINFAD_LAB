pacman::p_load(tidyverse)

df <- read_csv("data/customer_data.csv")

clean_df <- df[, !(names(df) %in% c("customer_segment", "nps_score", "last_survey_date", "feedback_sentiment", "monthly_transaction_count", 
                              "average_transaction_value", "total_transaction_volume", "transaction_frequency", "last_transaction_date", 
                              "first_transaction_date", "churn_probability", "clv_segment", "customer_lifetime_value"))]

clean_df$customer_id <- as.character(clean_df$customer_id)

clean_df$gender <- as.factor(clean_df$gender)
clean_df$income_bracket <- as.factor(clean_df$income_bracket)
clean_df$occupation <- as.factor(clean_df$occupation)
clean_df$education_level <- as.factor(clean_df$education_level)
clean_df$marital_status <- as.factor(clean_df$marital_status)
clean_df$acquisition_channel <- as.factor(clean_df$acquisition_channel)
clean_df$preferred_transaction_type <- as.factor(clean_df$preferred_transaction_type)
clean_df$location <- as.factor(clean_df$location)




