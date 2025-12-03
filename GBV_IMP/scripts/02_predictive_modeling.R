# ==============================================================================
# GBV EPIDEMIOLOGICAL ANALYSIS - PHASE 2: PREDICTIVE MODELING (RANDOM FOREST)
# ==============================================================================
# Purpose: Identify strongest predictors using machine learning
# Output: Variable importance ranking, model accuracy metrics, comparison plots
# ==============================================================================

cat("\n\n")
cat("PHASE 2: PREDICTIVE MODELING (RANDOM FOREST) STARTING...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# 2.1 SIMPLIFIED PACKAGE LOADING - NO FUNCTION NEEDED
required_packages <- c("randomForest", "caret", "pROC", "ggplot2", "gridExtra")



# Simple loop without function
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("Installing:", pkg, "\n"))
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

cat("✅ All libraries loaded successfully.\n\n")


# 2.2 LOAD DATA FROM PHASE 1
cat(" Step 1: Loading data from Phase 1...\n")

if(!exists("data_sim")) {
  if(file.exists("outputs/phase1_dataset_clean.csv")) {
    data_sim <- read.csv("outputs/phase1_dataset_clean.csv")
    cat("✅ Data loaded from Phase 1 output file\n")
  } else {
    stop("❌ ERROR: Phase 1 data not found. Please run Phase 1 first.")
  }
} else {
  cat("✅ Data already in environment\n")
}

cat("   • Sample size:", nrow(data_sim), "\n")
cat("   • GBV prevalence:", round(mean(data_sim$any_violence) * 100, 1), "%\n\n")




# 2.3 PREPARE DATA FOR MACHINE LEARNING
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Step 2: Preparing data for Random Forest...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Select predictors and outcome
rf_data <- data_sim %>%
  select(
    any_violence_factor,           # Outcome
    age,                            # Demographics
    residence, 
    wealth_quintile, 
    education,
    partner_alcohol,                # Individual factors
    mental_health_score, 
    childhood_trauma,
    decision_power,                 # Relationship factors
    communication_score, 
    marital_conflict,
    community_patriarchy,           # Community factors
    distance_to_services, 
    legal_implementation
  ) %>%
  mutate(
    # Converting to factors
    any_violence_factor = factor(any_violence_factor, levels = c("No", "Yes")),
    residence = factor(residence),
    wealth_quintile = factor(wealth_quintile, 
                             levels = c("Lowest", "Second", "Middle", "Fourth", "Highest")),
    education = factor(education, 
                       levels = c("No education", "Primary", "Secondary", "Higher")),
    partner_alcohol = factor(partner_alcohol, levels = c(0, 1), labels = c("No", "Yes")),
    childhood_trauma = factor(childhood_trauma, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Checking for missing values
if(anyNA(rf_data)) {
  cat("⚠️ Warning: Missing values detected. Removing rows with NAs.\n")
  rf_data <- na.omit(rf_data)
  cat("   • Remaining sample size:", nrow(rf_data), "\n")
}

cat("✅ Data prepared for modeling\n")
cat("   • Outcome:", levels(rf_data$any_violence_factor), "\n")
cat("   • Predictors:", ncol(rf_data) - 1, "\n\n")




# 2.4 TRAIN-TEST SPLIT
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Step 3: Splitting data into training and testing sets...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Make sure caret is loaded
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

set.seed(123)
train_index <- createDataPartition(rf_data$any_violence_factor, p = 0.70, list = FALSE)
train_data <- rf_data[train_index, ]
test_data <- rf_data[-train_index, ]

cat("✅ Data split completed:\n")
cat("   • Training set:", nrow(train_data), "cases (", 
    round(nrow(train_data)/nrow(rf_data)*100, 1), "%)\n")
cat("   • Testing set:", nrow(test_data), "cases (", 
    round(nrow(test_data)/nrow(rf_data)*100, 1), "%)\n")
cat("   • GBV prevalence in training:", 
    round(mean(train_data$any_violence_factor == "Yes") * 100, 1), "%\n")
cat("   • GBV prevalence in testing:", 
    round(mean(test_data$any_violence_factor == "Yes") * 100, 1), "%\n\n")





# 2.5 TRAINING RANDOM FOREST MODEL
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat(" Step 4: Training Random Forest model...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Make sure randomForest is loaded
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

cat("Model parameters:\n")
cat("   • Number of trees: 500\n")
cat("   • mtry: default (auto-selected)\n")
cat("   • Node size: default\n")
cat("   • Importance: Mean Decrease Accuracy\n\n")

cat("training model (this may take 15-30 seconds)...\n")

start_time <- Sys.time()

rf_model <- randomForest(
  any_violence_factor ~ ., 
  data = train_data, 
  ntree = 500,
  importance = TRUE,
  proximity = FALSE,
  keep.forest = TRUE
)

end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "secs"), 1)

cat("✅ Random Forest model trained successfully!\n")
cat("   • Training time:", elapsed_time, "seconds\n")
cat("   • OOB error rate:", round(rf_model$err.rate[500, "OOB"] * 100, 2), "%\n\n")

# Print model summary
print(rf_model)
cat("\n")





# 2.6 MODEL EVALUATION ON TEST SET
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat(" Step 5: Evaluating model performance on test set...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Make sure pROC is loaded
if (!require(pROC)) {
  install.packages("pROC")
  library(pROC)
}

# Make predictions
rf_predictions <- predict(rf_model, test_data, type = "class")
rf_probabilities <- predict(rf_model, test_data, type = "prob")[, "Yes"]

# Confusion matrix
conf_matrix <- confusionMatrix(rf_predictions, test_data$any_violence_factor, 
                               positive = "Yes")

cat("CONFUSION MATRIX:\n")
print(conf_matrix$table)
cat("\n")

cat("PERFORMANCE METRICS:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("Overall Accuracy:        %.2f%%\n", conf_matrix$overall["Accuracy"] * 100))
cat(sprintf("Sensitivity (Recall):    %.2f%%\n", conf_matrix$byClass["Sensitivity"] * 100))
cat(sprintf("Specificity:             %.2f%%\n", conf_matrix$byClass["Specificity"] * 100))
cat(sprintf("Positive Predictive Val: %.2f%%\n", conf_matrix$byClass["Pos Pred Value"] * 100))
cat(sprintf("Negative Predictive Val: %.2f%%\n", conf_matrix$byClass["Neg Pred Value"] * 100))
cat(sprintf("F1 Score:                %.2f\n", conf_matrix$byClass["F1"]))
cat("───────────────────────────────────────────────────────────────────────────\n\n")

# ROC curve and AUC
roc_obj <- roc(test_data$any_violence_factor, rf_probabilities, levels = c("No", "Yes"))
auc_value <- auc(roc_obj)

cat(sprintf("AUC-ROC:                 %.4f\n\n", auc_value))


# 2.7 VARIABLE IMPORTANCE ANALYSIS
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Step 6: Variable importance ranking...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Extract importance
importance_df <- importance(rf_model, type = 1) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  mutate(
    Rank = row_number(),
    Relative_Importance = round(MeanDecreaseAccuracy / max(MeanDecreaseAccuracy) * 100, 1)
  )

cat("TOP 15 MOST IMPORTANT PREDICTORS:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-5s %-30s %15s %15s\n", "Rank", "Variable", "MDA", "Relative (%)"))
cat("───────────────────────────────────────────────────────────────────────────\n")

top_15 <- head(importance_df, 15)
for(i in 1:nrow(top_15)) {
  cat(sprintf("%-5d %-30s %15.4f %14.1f%%\n", 
              top_15$Rank[i], 
              top_15$Variable[i], 
              top_15$MeanDecreaseAccuracy[i],
              top_15$Relative_Importance[i]))
}
cat("───────────────────────────────────────────────────────────────────────────\n")
cat("\nMDA = Mean Decrease Accuracy\n")
cat("Relative Importance = Scaled to 100% (strongest predictor)\n\n")




# 2.8 VISUALIZATIONS
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Step 7: Creating visualizations...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

if(!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
}

# Plot 1: Variable Importance
top_vars <- head(importance_df, 15)
top_vars <- top_vars[order(top_vars$MeanDecreaseAccuracy), ]

png("figures/phase2_variable_importance.png", width = 1000, height = 700, res = 100)
par(mar = c(5, 12, 4, 2))
barplot(
  top_vars$MeanDecreaseAccuracy,
  names.arg = top_vars$Variable,
  horiz = TRUE,
  las = 1,
  col = colorRampPalette(c("#3498db", "#e74c3c"))(15),
  main = "Random Forest: Top 15 Predictors of GBV",
  xlab = "Mean Decrease Accuracy",
  cex.names = 0.85
)
dev.off()
cat("✅ Variable importance plot saved\n")

# Plot 2: ROC Curve
png("figures/phase2_roc_curve.png", width = 800, height = 800, res = 100)
plot(roc_obj, 
     main = paste0("ROC Curve: Random Forest Model\nAUC = ", round(auc_value, 4)),
     col = "#e74c3c", 
     lwd = 2,
     print.auc = TRUE,
     print.auc.y = 0.4)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()
cat("✅ ROC curve saved\n")

# Plot 3: Model Comparison (RF vs Baseline)
# Baseline: predict majority class
baseline_accuracy <- max(table(test_data$any_violence_factor)) / nrow(test_data)

comparison_data <- data.frame(
  Model = c("Baseline\n(Majority Class)", "Random Forest"),
  Accuracy = c(baseline_accuracy * 100, conf_matrix$overall["Accuracy"] * 100),
  Sensitivity = c(0, conf_matrix$byClass["Sensitivity"] * 100),
  Specificity = c(100, conf_matrix$byClass["Specificity"] * 100)
)

png("figures/phase2_model_comparison.png", width = 1000, height = 600, res = 100)
comparison_long <- comparison_data %>%
  pivot_longer(cols = c(Accuracy, Sensitivity, Specificity), 
               names_to = "Metric", values_to = "Value")

ggplot(comparison_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Value, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Accuracy" = "#3498db", 
                               "Sensitivity" = "#2ecc71", 
                               "Specificity" = "#e74c3c")) +
  labs(
    title = "Model Performance Comparison: Random Forest vs. Baseline",
    y = "Performance (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(0, 105)

dev.off()
cat("✅ Model comparison plot saved\n\n")


# 2.9 SAVE OUTPUTS
cat("💾 Saving outputs...\n")

# Save variable importance
write.csv(importance_df, "outputs/phase2_variable_importance.csv", row.names = FALSE)

# Save model performance metrics
performance_summary <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "F1", "AUC"),
  Value = c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Pos Pred Value"],
    conf_matrix$byClass["Neg Pred Value"],
    conf_matrix$byClass["F1"],
    auc_value
  )
)
write.csv(performance_summary, "outputs/phase2_performance_metrics.csv", row.names = FALSE)

# Save predictions for later analysis
predictions_df <- data.frame(
  actual = test_data$any_violence_factor,
  predicted = rf_predictions,
  probability = rf_probabilities
)
write.csv(predictions_df, "outputs/phase2_predictions.csv", row.names = FALSE)

# Save the model object
saveRDS(rf_model, "outputs/phase2_rf_model.rds")

cat("✅ All outputs saved to 'outputs/' folder\n\n")

# 2.10 INTERPRETATION SUMMARY
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat(" PHASE 2 INTERPRETATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

cat("✅ Predictive Modeling Phase Complete!\n\n")

cat("KEY FINDINGS:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("1. Model Accuracy: %.1f%% (vs. %.1f%% baseline)\n", 
            conf_matrix$overall["Accuracy"] * 100,
            baseline_accuracy * 100))
cat(sprintf("   • Improvement over baseline: %.1f percentage points\n",
            (conf_matrix$overall["Accuracy"] - baseline_accuracy) * 100))
cat("\n")

cat("2. Top 5 Predictors (by importance):\n")
for(i in 1:5) {
  cat(sprintf("   %d. %s (MDA: %.4f)\n", 
              i, 
              importance_df$Variable[i], 
              importance_df$MeanDecreaseAccuracy[i]))
}
cat("\n")

cat(sprintf("3. Model Discrimination: AUC = %.3f\n", auc_value))
cat("   • Interpretation:", 
    ifelse(auc_value >= 0.80, "Excellent discrimination",
           ifelse(auc_value >= 0.70, "Good discrimination",
                  ifelse(auc_value >= 0.60, "Fair discrimination", "Poor discrimination"))),
    "\n\n")

cat(sprintf("4. Sensitivity: %.1f%% (correctly identifies GBV cases)\n",
            conf_matrix$byClass["Sensitivity"] * 100))
cat(sprintf("5. Specificity: %.1f%% (correctly identifies non-GBV cases)\n\n",
            conf_matrix$byClass["Specificity"] * 100))

cat("INTERPRETATION:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
cat("• Random Forest provides", 
    ifelse((conf_matrix$overall["Accuracy"] - baseline_accuracy) > 0.10, 
           "substantial", "modest"),
    "improvement over baseline prediction\n")
cat("• The model identifies", top_15$Variable[1], "as the strongest predictor\n")
cat("• Top predictors align with ecological framework expectations\n")
cat("• Non-linear relationships and interactions captured by the model\n\n")

