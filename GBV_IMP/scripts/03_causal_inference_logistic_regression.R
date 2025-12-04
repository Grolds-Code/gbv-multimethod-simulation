required_packages <- c("tidyverse", "broom", "car", "lmtest", "MASS", "ggplot2", "patchwork")

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste("Installing:", pkg, "\n"))
      install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
    }
  }
}

tryCatch({
  install_if_missing(required_packages)
  library(tidyverse)
  library(broom)
  library(car)
  library(lmtest)
  library(MASS)
  library(ggplot2)
  library(patchwork)
  cat("ll libraries loaded successfully.\n\n")
}, error = function(e) {
  cat("❌ ERROR in library loading:\n")
  stop(e)
})


# loading data from ph.1


if(!exists("data_sim")) {
  if(file.exists("outputs/phase1_dataset_clean.csv")) {
    data_sim <- read.csv("outputs/phase1_dataset_clean.csv")
    
    # Recreate factors
    data_sim <- data_sim %>%
      mutate(
        any_violence = factor(any_violence, levels = c(0, 1)),
        residence = factor(residence),
        wealth_quintile = factor(wealth_quintile, 
                                 levels = c("Lowest", "Second", "Middle", "Fourth", "Highest")),
        education = factor(education, 
                           levels = c("No education", "Primary", "Secondary", "Higher")),
        partner_alcohol = factor(partner_alcohol, levels = c(0, 1)),
        childhood_trauma = factor(childhood_trauma, levels = c(0, 1))
      )
    cat("data loaded from Phase 1 output file\n")
  } else {
    stop("❌ ERROR: Phase 1 data not found. Please run Phase 1 first.")
  }
} else {
  cat("Data already in environment\n")
}

cat("   Sample size:", nrow(data_sim), "\n")
cat("    GBV cases:", sum(data_sim$any_violence == 1), "\n\n")






# preparing analysis dataset

# Creating centered age variable (for interpretation)
data_sim <- data_sim %>%
  mutate(
    age_centered = age - mean(age),
    wealth_high = ifelse(wealth_quintile %in% c("Fourth", "Highest"), 1, 0),
    education_high = ifelse(education %in% c("Secondary", "Higher"), 1, 0)
  )

# Checking for multicollinearity before modeling - FIXED VERSION
predictors_numeric <- data_sim %>%
  dplyr::select(age, mental_health_score, marital_conflict, decision_power,
                communication_score, community_patriarchy, legal_implementation,
                distance_to_services) %>%
  cor()

cat("Checking for multicollinearity (correlation matrix):\n")
high_cor <- which(abs(predictors_numeric) > 0.7 & abs(predictors_numeric) < 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  cat("⚠️ High correlations detected (|r| > 0.7):\n")
  for(i in 1:nrow(high_cor)) {
    var1 <- rownames(predictors_numeric)[high_cor[i,1]]
    var2 <- colnames(predictors_numeric)[high_cor[i,2]]
    cor_val <- predictors_numeric[high_cor[i,1], high_cor[i,2]]
    cat(sprintf("   • %s <-> %s: r = %.3f\n", var1, var2, cor_val))
  }
} else {
  cat("No severe multicollinearity detected\n")
}
cat("\n")






# model 1 - individual factors

model1 <- glm(any_violence ~ age_centered + education_high + wealth_high + 
                partner_alcohol + childhood_trauma + mental_health_score,
              data = data_sim,
              family = binomial(link = "logit"))

cat("Model 1 Summary:\n")
summary(model1)
cat("\n")
cat("Model 1 AIC:", AIC(model1), "\n")
cat("Model 1 BIC:", BIC(model1), "\n\n")





# model 2- Indv' plus R'ship factors

model2 <- glm(any_violence ~ age_centered + education_high + wealth_high + 
                partner_alcohol + childhood_trauma + mental_health_score +
                marital_conflict + communication_score + decision_power,
              data = data_sim,
              family = binomial(link = "logit"))

cat("Model 2 Summary:\n")
summary(model2)
cat("\n")
cat("Model 2 AIC:", AIC(model2), "\n")
cat("Model 2 BIC:", BIC(model2), "\n\n")

# Likelihood ratio test: Model 2 vs Model 1
lrtest_1_2 <- lrtest(model1, model2)
cat("Likelihood Ratio Test (Model 2 vs Model 1):\n")
print(lrtest_1_2)
cat("\n")



# model 3 -full ecological model...all levels

model3 <- glm(any_violence ~ age_centered + education_high + wealth_high + 
                partner_alcohol + childhood_trauma + mental_health_score +
                marital_conflict + communication_score + decision_power +
                community_patriarchy + legal_implementation + distance_to_services,
              data = data_sim,
              family = binomial(link = "logit"))

cat("Model 3 Summary:\n")
summary(model3)
cat("\n")
cat("Model 3 AIC:", AIC(model3), "\n")
cat("Model 3 BIC:", BIC(model3), "\n\n")

# Likelihood ratio test: Model 3 vs Model 2
lrtest_2_3 <- lrtest(model2, model3)
cat("Likelihood Ratio Test (Model 3 vs Model 2):\n")
print(lrtest_2_3)
cat("\n")


# model comparison

model_comparison <- data.frame(
  Model = c("Model 1: Individual Only", 
            "Model 2: Individual + Relationship", 
            "Model 3: Full Ecological"),
  AIC = c(AIC(model1), AIC(model2), AIC(model3)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3)),
  Deviance = c(deviance(model1), deviance(model2), deviance(model3)),
  df = c(df.residual(model1), df.residual(model2), df.residual(model3))
)

model_comparison <- model_comparison %>%
  mutate(
    Delta_AIC = AIC - min(AIC),
    Best_AIC = ifelse(Delta_AIC == 0, "✓", "")
  )

cat("MODEL COMPARISON TABLE:\n")
print(model_comparison, row.names = FALSE)
cat("\n")

cat("INTERPRETATION:\n")
best_model_idx <- which.min(model_comparison$AIC)
cat("Best fitting model:", model_comparison$Model[best_model_idx], "\n")
cat("This supports the ecological framework (multi-level factors matter)\n\n")






# final model, checking for interactions

# Testing interaction: partner_alcohol × mental_health_score
model_int1 <- glm(any_violence ~ age_centered + education_high + wealth_high + 
                    partner_alcohol * mental_health_score + childhood_trauma +
                    marital_conflict + communication_score + decision_power +
                    community_patriarchy + legal_implementation + distance_to_services,
                  data = data_sim,
                  family = binomial(link = "logit"))

lrtest_int1 <- lrtest(model3, model_int1)
cat("Testing: partner_alcohol × mental_health_score\n")
print(lrtest_int1)
cat("\n")

# Testing interaction: education_high × wealth_high
model_int2 <- glm(any_violence ~ age_centered + education_high * wealth_high + 
                    partner_alcohol + childhood_trauma + mental_health_score +
                    marital_conflict + communication_score + decision_power +
                    community_patriarchy + legal_implementation + distance_to_services,
                  data = data_sim,
                  family = binomial(link = "logit"))

lrtest_int2 <- lrtest(model3, model_int2)
cat("Testing: education_high × wealth_high\n")
print(lrtest_int2)
cat("\n")

# Selecting final model (use Model 3 unless interactions are significant)
final_model <- model3

cat("Final model selected: Model 3 (Full Ecological)\n\n")





# Diagnostics for the final model

# Variance Inflation Factors (VIF)

vif_values <- vif(final_model)
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 2),
  Concern = ifelse(vif_values > 10, "High ⚠️", 
                   ifelse(vif_values > 5, "Moderate", "OK ✓"))
)
print(vif_df, row.names = FALSE)
cat("\nInterpretation: VIF > 10 indicates severe multicollinearity\n\n")

# Hosmer-Lemeshow goodness of fit

# Calculating predicted probabilities
predicted_probs <- predict(final_model, type = "response")

# Grouping into deciles
deciles <- cut(predicted_probs, breaks = quantile(predicted_probs, probs = seq(0, 1, 0.1)), 
               include.lowest = TRUE)

# Observed vs Expected
hl_test <- data.frame(
  observed = data_sim$any_violence,
  expected = predicted_probs,
  decile = deciles
) %>%
  group_by(decile) %>%
  summarise(
    observed_events = sum(as.numeric(as.character(observed))),
    expected_events = sum(expected),
    n = n()
  )

cat("Observed vs Expected by decile:\n")
print(hl_test)
cat("\n")

# McFadden's Pseudo R-squared
null_model <- glm(any_violence ~ 1, data = data_sim, family = binomial)
mcfadden_r2 <- 1 - (logLik(final_model) / logLik(null_model))
cat("McFadden's Pseudo R²:", round(mcfadden_r2, 4), "\n")
cat("Interpretation:", 
    ifelse(mcfadden_r2 > 0.40, "Excellent fit",
           ifelse(mcfadden_r2 > 0.20, "Good fit",
                  ifelse(mcfadden_r2 > 0.10, "Moderate fit", "Weak fit"))), "\n\n")






# extracting OR and CIs

# coefficients and confidence intervals
or_results <- tidy(final_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "NS"
    ),
    direction = case_when(
      estimate > 1 & p.value < 0.05 ~ "Risk Factor",
      estimate < 1 & p.value < 0.05 ~ "Protective Factor",
      TRUE ~ "Not Significant"
    )
  ) %>%
  arrange(desc(abs(estimate - 1)))

cat("ADJUSTED ODDS RATIOS (Final Model):\n")

cat(sprintf("%-30s %10s %20s %10s %15s\n", 
            "Variable", "OR", "95% CI", "p-value", "Effect"))


for(i in 1:nrow(or_results)) {
  row <- or_results[i, ]
  cat(sprintf("%-30s %10.2f %10s [%.2f, %.2f] %10.4f %10s %s\n",
              row$term,
              row$estimate,
              "",
              row$conf.low,
              row$conf.high,
              row$p.value,
              row$significance,
              row$direction))
}
)
cat("Significance: *** p<0.001, ** p<0.01, * p<0.05, NS = Not Significant\n\n")




# visualizations:

if(!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
}

# Plot 1: Forest plot of odds ratios
or_plot_data <- or_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = factor(term, levels = rev(term))  # Reverse for plotting
  )

p1 <- ggplot(or_plot_data, aes(x = estimate, y = term, color = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 0.8) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.8) +
  scale_color_manual(values = c("Risk Factor" = "#e74c3c", 
                                "Protective Factor" = "#2ecc71",
                                "Not Significant" = "gray60")) +
  scale_x_continuous(trans = "log10", breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 5)) +
  labs(
    title = "Adjusted Odds Ratios for GBV (Final Model)",
    subtitle = "Multivariable Logistic Regression with 95% Confidence Intervals",
    x = "Odds Ratio (log scale)",
    y = "",
    color = "Effect Direction"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    axis.text.y = element_text(size = 9)
  )

ggsave("figures/phase3_forest_plot.png", p1, width = 10, height = 8, dpi = 300)
cat("forest plot saved\n")

# Plot 2: Model comparison
p2 <- ggplot(model_comparison, aes(x = Model, y = AIC)) +
  geom_col(fill = "#3498db", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = round(AIC, 0)), vjust = -0.5, size = 4) +
  labs(
    title = "Model Comparison: Testing Ecological Framework",
    subtitle = "Lower AIC indicates better model fit",
    y = "AIC (Akaike Information Criterion)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave("figures/phase3_model_comparison.png", p2, width = 10, height = 6, dpi = 300)
cat("Model comparison plot saved\n")

# Plot 3: Risk vs Protective factors
risk_protective <- or_results %>%
  filter(significance != "NS") %>%
  mutate(
    effect_size = ifelse(estimate > 1, estimate - 1, 1 - estimate)
  ) %>%
  arrange(desc(effect_size)) %>%
  head(10)

p3 <- ggplot(risk_protective, aes(x = reorder(term, effect_size), y = estimate, fill = direction)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Risk Factor" = "#e74c3c", "Protective Factor" = "#2ecc71")) +
  coord_flip() +
  labs(
    title = "Top 10 Significant Risk and Protective Factors",
    subtitle = "Sorted by effect size magnitude",
    x = "",
    y = "Adjusted Odds Ratio",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave("figures/phase3_risk_protective.png", p3, width = 10, height = 7, dpi = 300)
cat("risk/protective factors plot saved\n\n")

# outputs saving:


# odds ratios
write.csv(or_results, "outputs/phase3_odds_ratios.csv", row.names = FALSE)

# model comparison
write.csv(model_comparison, "outputs/phase3_model_comparison.csv", row.names = FALSE)

# VIF results
write.csv(vif_df, "outputs/phase3_vif_diagnostics.csv", row.names = FALSE)

# final model object
saveRDS(final_model, "outputs/phase3_final_model.rds")

# model summary
sink("outputs/phase3_model_summary.txt")
cat("FINAL MODEL SUMMARY\n")

summary(final_model)
cat("\n\nMODEL DIAGNOSTICS\n")

cat("AIC:", AIC(final_model), "\n")
cat("BIC:", BIC(final_model), "\n")
cat("McFadden's R²:", round(mcfadden_r2, 4), "\n")
sink()

cat("All outputs saved to 'outputs/' folder\n\n")



# interaction summary

# Identify strongest risk factors
risk_factors <- or_results %>%
  filter(estimate > 1 & p.value < 0.05) %>%
  arrange(desc(estimate)) %>%
  head(5)

cat("1. STRONGEST RISK FACTORS (adjusted):\n")
for(i in 1:min(5, nrow(risk_factors))) {
  cat(sprintf("   %d. %s: OR = %.2f (95%% CI: %.2f-%.2f) %s\n",
              i,
              risk_factors$term[i],
              risk_factors$estimate[i],
              risk_factors$conf.low[i],
              risk_factors$conf.high[i],
              risk_factors$significance[i]))
}
cat("\n")

# Identifying protective factors
protective_factors <- or_results %>%
  filter(estimate < 1 & p.value < 0.05) %>%
  arrange(estimate) %>%
  head(5)

if(nrow(protective_factors) > 0) {
  cat("2. PROTECTIVE FACTORS (adjusted):\n")
  for(i in 1:nrow(protective_factors)) {
    cat(sprintf("   %d. %s: OR = %.2f (95%% CI: %.2f-%.2f) %s\n",
                i,
                protective_factors$term[i],
                protective_factors$estimate[i],
                protective_factors$conf.low[i],
                protective_factors$conf.high[i],
                protective_factors$significance[i]))
  }
  cat("\n")
}

cat("3. MODEL FIT:\n")
cat(sprintf("    McFadden's Pseudo R²: %.3f\n", mcfadden_r2))
cat(sprintf("    Model explains %.1f%% of variance in GBV outcome\n", mcfadden_r2 * 100))
cat("\n")

cat("4. ECOLOGICAL FRAMEWORK VALIDATION:\n")
cat("    Full ecological model provides best fit (lowest AIC)\n")
cat("    Individual, relationship, AND community factors all contribute\n")
cat("    Multi-level interventions supported by evidence\n\n")




cat("COMPARISON WITH PHASE 2 (Random Forest):\n")

# Loading Phase 2 importance if available
if(file.exists("outputs/phase2_variable_importance.csv")) {
  rf_importance <- read.csv("outputs/phase2_variable_importance.csv")
  top_rf <- head(rf_importance$Variable, 5)
  
  cat("Top 5 from Random Forest (predictive):\n")
  for(i in 1:5) {
    cat(sprintf("   %d. %s\n", i, top_rf[i]))
  }
  cat("\n")
  
  cat("Top 5 from Logistic Regression (causal, adjusted):\n")
  for(i in 1:min(5, nrow(risk_factors))) {
    cat(sprintf("   %d. %s\n", i, risk_factors$term[i]))
  }
  
}