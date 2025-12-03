
cat(" PHASE 1: DESCRIPTIVE EPIDEMIOLOGY STARTING...\n")

# 1.1 SIMPLE PACKAGE LOADING


# Install packages if not already installed
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gtsummary)) install.packages("gtsummary")
if (!require(janitor)) install.packages("janitor")
if (!require(skimr)) install.packages("skimr")
if (!require(knitr)) install.packages("knitr")

# Load packages directly
library(tidyverse)
library(gtsummary)
library(janitor)
library(skimr)
library(knitr)

cat("✅ All libraries loaded successfully.\n\n")

# 1.2 SIMULATING REALISTIC GBV DATASET & DATA QUALITY CHECK
cat(" Step 1: Simulating GBV dataset...\n")

simulation_success <- tryCatch({
  set.seed(42)
  n <- 3000
  
  # Base demographics
  data_sim <- data.frame(
    case_id = 1:n,
    age = sample(15:49, n, replace = TRUE),
    residence = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.35, 0.65)),
    wealth_quintile = sample(c("Lowest", "Second", "Middle", "Fourth", "Highest"), 
                             n, replace = TRUE),
    education = sample(c("No education", "Primary", "Secondary", "Higher"), 
                       n, replace = TRUE, prob = c(0.1, 0.35, 0.4, 0.15))
  )
  
  # Individual level factors - USING BASE R FOR INITIAL CREATION
  data_sim$childhood_trauma <- sample(c(0, 1), n, replace = TRUE, prob = c(0.75, 0.25))
  data_sim$mental_health_score <- round(rnorm(n, mean = 3 + (2 * data_sim$childhood_trauma), sd = 2))
  data_sim$partner_alcohol <- sample(c(0, 1), n, replace = TRUE, prob = c(0.70, 0.30))
  data_sim$mental_health_score <- pmax(0, pmin(10, data_sim$mental_health_score))
  
  # NOW USE DPLYR FOR MORE COMPLEX OPERATIONS
  data_sim <- data_sim %>%
    mutate(
      decision_power = round(rnorm(n, mean = 4 + (1 * (education == "Higher")), sd = 2.5)),
      communication_score = round(rnorm(n, mean = 5, sd = 2.5)),
      marital_conflict = round(rnorm(n, mean = 5 - (0.3 * communication_score), sd = 2))
    ) %>%
    mutate(
      decision_power = pmax(0, pmin(10, decision_power)),
      communication_score = pmax(0, pmin(10, communication_score)),
      marital_conflict = pmax(0, pmin(10, marital_conflict))
    )
  
  # Community level factors
  clusters <- data.frame(
    cluster_id = 1:100,
    community_patriarchy = runif(100, min = 0, max = 10),
    distance_to_services = runif(100, min = 1, max = 50),
    legal_implementation = runif(100, min = 2, max = 9)
  )
  
  data_sim$cluster_id <- sample(1:100, n, replace = TRUE)
  data_sim <- left_join(data_sim, clusters, by = "cluster_id")
  
  # Generating GBV outcome
  data_sim <- data_sim %>%
    mutate(
      risk_score = -2.5 + 
        (1.8 * partner_alcohol) +              # Strong risk
        (0.15 * marital_conflict) +            # Moderate risk
        (0.8 * childhood_trauma) +             # Strong risk
        (0.1 * community_patriarchy) -         # Weak risk
        (0.05 * decision_power) -              # Weak protective
        (0.05 * legal_implementation) -        # Weak protective
        (0.2 * (wealth_quintile == "Highest")), # Protective
      
      prob_violence = 1 / (1 + exp(-risk_score)),
      any_violence = rbinom(n, 1, prob_violence),
      
      # Create factors for tables
      any_violence_factor = factor(any_violence, levels = c(0, 1), 
                                   labels = c("No", "Yes")),
      partner_alcohol_factor = factor(partner_alcohol, levels = c(0, 1), 
                                      labels = c("No", "Yes")),
      childhood_trauma_factor = factor(childhood_trauma, levels = c(0, 1), 
                                       labels = c("No", "Yes")),
      age_group = case_when(
        age < 25 ~ "15-24",
        age < 35 ~ "25-34",
        age < 45 ~ "35-44",
        TRUE ~ "45-49"
      ),
      wealth_binary = ifelse(wealth_quintile %in% c("Lowest", "Second"), "Low", "High")
    )
  
  cat("Data simulation complete.\n")
  cat("   • N =", nrow(data_sim), "women\n")
  cat("   • GBV prevalence =", round(mean(data_sim$any_violence) * 100, 1), "%\n\n")
  
  # 1.3 DATA QUALITY CHECK
  cat(" Step 2: Data quality assessment...\n\n")
  
  # Checking for missing values
  missing_summary <- colSums(is.na(data_sim))
  if(sum(missing_summary) > 0) {
    cat("Missing values detected:\n")
    print(missing_summary[missing_summary > 0])
  } else {
    cat("✅ No missing values\n")
  }
  
  # Checking variable types
  cat("\n✅ Variable types:\n")
  capture.output(str(data_sim)) %>% head(10) %>% cat(sep = "\n")
  
  TRUE  # Return success
  
}, error = function(err) {
  cat("❌ ERROR in data simulation:\n")
  cat("Error message:", err$message, "\n")
  FALSE  # Return failure
})

# Only proceed if simulation was successful
if (!simulation_success) {
  stop("Data simulation failed. Cannot proceed with analysis.")
}

cat("\n✅ Phase 1 completed successfully!\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")


# 1.4 OVERALL PREVALENCE
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("📊 1.4 OVERALL GBV PREVALENCE\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

prevalence_overall <- data_sim %>%
  summarise(
    N = n(),
    GBV_cases = sum(any_violence),
    Prevalence_pct = (sum(any_violence) / n()) * 100,
    CI_lower = (sum(any_violence) - 1.96 * sqrt(sum(any_violence) * (1 - sum(any_violence)/n()))) / n() * 100,
    CI_upper = (sum(any_violence) + 1.96 * sqrt(sum(any_violence) * (1 - sum(any_violence)/n()))) / n() * 100
  )

print(prevalence_overall)
cat("\n")


# 1.5 TABLE 1: SAMPLE CHARACTERISTICS
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("📋 TABLE 1: SAMPLE CHARACTERISTICS (N = 3,000)\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

table1 <- data_sim %>%
  select(
    age, age_group, residence, wealth_quintile, education,
    partner_alcohol_factor, childhood_trauma_factor, 
    mental_health_score, marital_conflict, decision_power,
    community_patriarchy, legal_implementation
  ) %>%
  tbl_summary(
    by = NULL,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      age ~ "Age (years)",
      age_group ~ "Age group",
      residence ~ "Residence type",
      wealth_quintile ~ "Wealth quintile",
      education ~ "Education level",
      partner_alcohol_factor ~ "Partner alcohol use",
      childhood_trauma_factor ~ "Childhood trauma",
      mental_health_score ~ "Mental health score",
      marital_conflict ~ "Marital conflict score",
      decision_power ~ "Decision power score",
      community_patriarchy ~ "Community patriarchy level",
      legal_implementation ~ "Legal system implementation"
    )
  )

print(table1)
cat("\n")


# 1.6 TABLE 2: PREVALENCE BY DEMOGRAPHIC STRATA
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("📊 TABLE 2: GBV PREVALENCE BY DEMOGRAPHIC CHARACTERISTICS\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# By age group
prevalence_age <- data_sim %>%
  group_by(age_group) %>%
  summarise(
    N = n(),
    GBV_cases = sum(any_violence),
    Prevalence_pct = round((sum(any_violence) / n()) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Stratum = "Age group")

cat("By Age Group:\n")
print(prevalence_age)
cat("\n")

# By residence
prevalence_residence <- data_sim %>%
  group_by(residence) %>%
  summarise(
    N = n(),
    GBV_cases = sum(any_violence),
    Prevalence_pct = round((sum(any_violence) / n()) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Stratum = "Residence")

cat("By Residence Type:\n")
print(prevalence_residence)
cat("\n")

# By wealth quintile
prevalence_wealth <- data_sim %>%
  group_by(wealth_quintile) %>%
  summarise(
    N = n(),
    GBV_cases = sum(any_violence),
    Prevalence_pct = round((sum(any_violence) / n()) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Stratum = "Wealth")

cat("By Wealth Quintile:\n")
print(prevalence_wealth)
cat("\n")

# By education
prevalence_education <- data_sim %>%
  group_by(education) %>%
  summarise(
    N = n(),
    GBV_cases = sum(any_violence),
    Prevalence_pct = round((sum(any_violence) / n()) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Stratum = "Education")

cat("By Education Level:\n")
print(prevalence_education)
cat("\n")

# Bivariate for key categorical variables
bivariate_results <- data.frame()

predictors_categorical <- c("residence", "wealth_binary", "education", 
                           "partner_alcohol_factor", "childhood_trauma_factor")

for(pred in predictors_categorical) {
  result <- bivariate_analysis(data_sim, "any_violence_factor", pred)
  if(!is.null(result)) {
    bivariate_results <- rbind(bivariate_results, result)
  }
}

# Continuous variables - t-tests
continuous_vars <- c("age", "mental_health_score", "marital_conflict", 
                    "decision_power", "community_patriarchy", "legal_implementation")

for(var in continuous_vars) {
  gbv_yes <- data_sim$any_violence_factor == "Yes"
  mean_yes <- mean(data_sim[gbv_yes, var])
  mean_no <- mean(data_sim[!gbv_yes, var])
  sd_yes <- sd(data_sim[gbv_yes, var])
  sd_no <- sd(data_sim[!gbv_yes, var])
  
  t_test <- t.test(data_sim[gbv_yes, var], data_sim[!gbv_yes, var])
  pvalue <- t_test$p.value
  
  cat(sprintf("%s: Mean(GBV+)=%.2f, Mean(GBV-)=%.2f, p=%.4f %s\n",
              var, mean_yes, mean_no, pvalue,
              ifelse(pvalue < 0.001, "***",
                     ifelse(pvalue < 0.01, "**",
                            ifelse(pvalue < 0.05, "*", "NS")))))
}

cat("\n")
if(nrow(bivariate_results) > 0) {
  print(bivariate_results)
}
cat("\n")


# 1.7 BIVARIATE ASSOCIATIONS WITH GBV
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("📈 TABLE 3: BIVARIATE ASSOCIATIONS WITH GBV OUTCOME\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Function to calculate odds ratios and p-values
bivariate_analysis <- function(data, outcome, predictor) {
  # Creating contingency table
  ct <- table(data[[predictor]], data[[outcome]])
  
  # Calculating OR and p-value
  if(nrow(ct) == 2 & ncol(ct) == 2) {
    a <- ct[2, 2]  # Exposed and outcome
    b <- ct[2, 1]  # Exposed no outcome
    c <- ct[1, 2]  # Unexposed and outcome
    d <- ct[1, 1]  # Unexposed no outcome
    
    or <- (a * d) / (b * c)
    
    # Chi-square test
    chi_test <- chisq.test(ct)
    pvalue <- chi_test$p.value
    
    # 95% CI for OR
    log_or <- log(or)
    se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
    ci_lower <- exp(log_or - 1.96 * se_log_or)
    ci_upper <- exp(log_or + 1.96 * se_log_or)
    
    return(data.frame(
      Predictor = predictor,
      OR = round(or, 2),
      CI_lower = round(ci_lower, 2),
      CI_upper = round(ci_upper, 2),
      P_value = round(pvalue, 4),
      Sig = ifelse(pvalue < 0.001, "***", 
                   ifelse(pvalue < 0.01, "**",
                          ifelse(pvalue < 0.05, "*", "NS")))
    ))
  }
}

# Bivariate for key categorical variables
bivariate_results <- data.frame()

predictors_categorical <- c("residence", "wealth_binary", "education", 
                            "partner_alcohol_factor", "childhood_trauma_factor")

for(pred in predictors_categorical) {
  result <- bivariate_analysis(data_sim, "any_violence_factor", pred)
  if(!is.null(result)) {
    bivariate_results <- rbind(bivariate_results, result)
  }
}

# Continuous variables - t-tests
continuous_vars <- c("age", "mental_health_score", "marital_conflict", 
                     "decision_power", "community_patriarchy", "legal_implementation")

for(var in continuous_vars) {
  gbv_yes <- data_sim$any_violence_factor == "Yes"
  mean_yes <- mean(data_sim[gbv_yes, var])
  mean_no <- mean(data_sim[!gbv_yes, var])
  sd_yes <- sd(data_sim[gbv_yes, var])
  sd_no <- sd(data_sim[!gbv_yes, var])
  
  t_test <- t.test(data_sim[gbv_yes, var], data_sim[!gbv_yes, var])
  pvalue <- t_test$p.value
  
  cat(sprintf("%s: Mean(GBV+)=%.2f, Mean(GBV-)=%.2f, p=%.4f %s\n",
              var, mean_yes, mean_no, pvalue,
              ifelse(pvalue < 0.001, "***",
                     ifelse(pvalue < 0.01, "**",
                            ifelse(pvalue < 0.05, "*", "NS")))))
}

cat("\n")
if(nrow(bivariate_results) > 0) {
  print(bivariate_results)
}
cat("\n")


# 1.8 CORRELATION MATRIX
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("📊 CORRELATION MATRIX: CONTINUOUS PREDICTORS\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

correlation_data <- data_sim %>%
  select(
    any_violence, age, mental_health_score, marital_conflict, 
    decision_power, community_patriarchy, legal_implementation,
    partner_alcohol, childhood_trauma
  ) %>%
  cor(use = "complete.obs")

print(round(correlation_data, 3))
cat("\n")

# 1.9 CREATING CONSISTENT PREVALENCE TABLES FOR SAVING
cat("📋 Creating consistent prevalence tables...\n")

# Ensuring all prevalence tables have the same structure
prevalence_age <- data_sim %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    cases = sum(any_violence),
    prevalence = mean(any_violence),
    prevalence_pct = round(mean(any_violence) * 100, 1)
  ) %>%
  mutate(variable = "Age group", category = age_group) %>%
  select(variable, category, n, cases, prevalence, prevalence_pct)

prevalence_residence <- data_sim %>%
  group_by(residence) %>%
  summarise(
    n = n(),
    cases = sum(any_violence),
    prevalence = mean(any_violence),
    prevalence_pct = round(mean(any_violence) * 100, 1)
  ) %>%
  mutate(variable = "Residence", category = residence) %>%
  select(variable, category, n, cases, prevalence, prevalence_pct)

prevalence_wealth <- data_sim %>%
  group_by(wealth_quintile) %>%
  summarise(
    n = n(),
    cases = sum(any_violence),
    prevalence = mean(any_violence),
    prevalence_pct = round(mean(any_violence) * 100, 1)
  ) %>%
  mutate(variable = "Wealth quintile", category = wealth_quintile) %>%
  select(variable, category, n, cases, prevalence, prevalence_pct)

prevalence_education <- data_sim %>%
  group_by(education) %>%
  summarise(
    n = n(),
    cases = sum(any_violence),
    prevalence = mean(any_violence),
    prevalence_pct = round(mean(any_violence) * 100, 1)
  ) %>%
  mutate(variable = "Education", category = education) %>%
  select(variable, category, n, cases, prevalence, prevalence_pct)

# Creating overall prevalence table
prevalence_overall <- data.frame(
  variable = "Overall",
  category = "Total",
  n = nrow(data_sim),
  cases = sum(data_sim$any_violence),
  prevalence = mean(data_sim$any_violence),
  prevalence_pct = round(mean(data_sim$any_violence) * 100, 1)
)

# 1.10 CREATING BIVARIATE RESULTS TABLE
cat("📈 Creating bivariate results table...\n")

# First, let's check what columns exist in the correlation data
correlation_with_gbv <- correlation_data["any_violence", ]
bivariate_results <- data.frame(
  variable = names(correlation_with_gbv),
  correlation = as.numeric(correlation_with_gbv)
) %>%
  filter(variable != "any_violence") %>%
  mutate(
    correlation = round(correlation, 3),
    abs_correlation = abs(correlation),
    significant = ifelse(abs_correlation > 0.05, "Yes", "No")
  ) %>%
  arrange(desc(abs_correlation))

print(bivariate_results)
cat("\n")

# 1.11 SAVE OUTPUTS
cat("💾 Saving outputs...\n")

if(!dir.exists("outputs")) {
  dir.create("outputs", showWarnings = FALSE)
}

# Save cleaned dataset
write.csv(data_sim, "outputs/phase1_dataset_clean.csv", row.names = FALSE)

# Save prevalence summary
write.csv(prevalence_overall, "outputs/phase1_prevalence_overall.csv", row.names = FALSE)

# Combine all stratified prevalence tables
prevalence_combined <- bind_rows(
  prevalence_age,
  prevalence_residence,
  prevalence_wealth,
  prevalence_education
)

# Save stratified prevalence
write.csv(prevalence_combined, "outputs/phase1_prevalence_stratified.csv", row.names = FALSE)

# Save bivariate results
write.csv(bivariate_results, "outputs/phase1_bivariate_results.csv", row.names = FALSE)

cat("✅ Outputs saved to 'outputs/' folder\n\n")

# 1.12 SUMMARY STATISTICS
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("PHASE 1 SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Calculating confidence interval for overall prevalence
n_total <- nrow(data_sim)
p_hat <- mean(data_sim$any_violence)
se <- sqrt(p_hat * (1 - p_hat) / n_total)
ci_lower <- round((p_hat - 1.96 * se) * 100, 1)
ci_upper <- round((p_hat + 1.96 * se) * 100, 1)

cat("✅ Descriptive Phase Complete!\n\n")
cat("Key Findings:\n")
cat("• Total sample size:", n_total, "\n")
cat("• Overall GBV prevalence:", round(p_hat * 100, 1), "%\n")
cat("• 95% CI:", ci_lower, "-", ci_upper, "%\n")
cat("• Significant bivariate associations:", sum(bivariate_results$significant == "Yes"), "\n")

# Calculating range of GBV prevalence across strata
all_prevalence <- c(prevalence_age$prevalence_pct, 
                    prevalence_residence$prevalence_pct, 
                    prevalence_wealth$prevalence_pct, 
                    prevalence_education$prevalence_pct)

cat("• Range of GBV across strata:", 
    round(min(all_prevalence), 1), "-",
    round(max(all_prevalence), 1), "%\n\n")

# Display key correlations - FIXED VERSION
cat("Key correlations with GBV:\n")
key_correlations <- bivariate_results %>%
  arrange(desc(abs_correlation)) %>%
  head(5)

for(i in 1:nrow(key_correlations)) {
  cat("  •", key_correlations$variable[i], ":", 
      key_correlations$correlation[i], 
      ifelse(key_correlations$significant[i] == "Yes", "(significant)", ""), "\n")
}

cat("\n PHASE 1 COMPLETED SUCCESSFULLY! \n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

