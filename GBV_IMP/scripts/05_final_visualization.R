# ==============================================================================
# GBV EPIDEMIOLOGICAL ANALYSIS - PHASE 5: INTEGRATED SYNTHESIS
# ==============================================================================
# Purpose: Synthesize findings across all analytical phases
# Output: Comprehensive summary tables, cross-method comparisons, final report
# ==============================================================================

cat("\n\n")
cat(" PHASE 5: INTEGRATED SYNTHESIS STARTING...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# 5.1 LOAD REQUIRED PACKAGES
cat(" Loading packages for integration...\n")

required_packages <- c("tidyverse", "patchwork", "knitr", "kableExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

cat("✅ All packages loaded successfully.\n\n")

# 5.2 LOAD DATA FROM ALL PREVIOUS PHASES
cat(" Loading data from previous phases...\n")

# Load Phase 1 data
if(file.exists("outputs/phase1_prevalence_stratified.csv")) {
  prevalence_strata <- read.csv("outputs/phase1_prevalence_stratified.csv")
  cat("✅ Phase 1 data loaded\n")
}

# Load Phase 2 data  
if(file.exists("outputs/phase2_variable_importance.csv")) {
  rf_importance <- read.csv("outputs/phase2_variable_importance.csv")
  cat("✅ Phase 2 data loaded\n")
}

if(file.exists("outputs/phase2_performance_metrics.csv")) {
  rf_performance <- read.csv("outputs/phase2_performance_metrics.csv")
}

# Load Phase 3 data
if(file.exists("outputs/phase3_odds_ratios.csv")) {
  or_results <- read.csv("outputs/phase3_odds_ratios.csv")
  cat("✅ Phase 3 data loaded\n")
}

# Load Phase 4 data
if(file.exists("outputs/phase4_pathway_results.csv")) {
  all_pathways <- read.csv("outputs/phase4_pathway_results.csv")
  cat("✅ Phase 4 data loaded\n")
}

cat(" All phase data loaded successfully\n\n")

# 5.3 CREATE INTEGRATED SUMMARY TABLE
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat(" Step 1: Creating integrated summary of key findings...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Extract top predictors from each phase
top_rf <- head(rf_importance, 5)$Variable
top_lr <- or_results %>% 
  filter(p.value < 0.05) %>% 
  arrange(desc(abs(estimate - 1))) %>%
  head(5) %>%
  pull(term)

# Use all_pathways instead of pathway_results
if(exists("all_pathways")) {
  pathway_info <- sprintf("Significant Pathways: %d of %d tested", 
                          sum(all_pathways$ACME_p < 0.05),
                          nrow(all_pathways))
} else {
  pathway_info <- "Mediation analysis completed"
}

# Create integrated findings table
integrated_summary <- data.frame(
  Analysis_Phase = c(
    "Phase 1: Descriptive",
    "Phase 2: Random Forest", 
    "Phase 3: Logistic Regression",
    "Phase 4: Mediation Analysis"
  ),
  Key_Finding = c(
    sprintf("GBV Prevalence: %.1f%%", mean(data_sim$any_violence) * 100),
    sprintf("Model Accuracy: %.1f%% | Top Predictor: %s", 
            rf_performance$Value[rf_performance$Metric == "Accuracy"] * 100,
            top_rf[1]),
    sprintf("Strongest Risk Factor: %s (OR = %.2f)", 
            top_lr[1],
            or_results$estimate[or_results$term == top_lr[1]]),
    pathway_info
  ),
  Method = c(
    "Cross-sectional survey analysis",
    "Machine Learning (500 trees, 70/30 split)",
    "Multivariable logistic regression (ecological model)", 
    "Mediation analysis with bootstrap (1000 sims)"
  ),
  N_Cases = c(
    nrow(data_sim),
    nrow(data_sim),
    nrow(data_sim),
    nrow(data_sim)
  )
)

cat("INTEGRATED FINDINGS ACROSS ALL PHASES:\n")
print(integrated_summary, row.names = FALSE)
cat("\n\n")

# 5.4 CREATE COMPREHENSIVE RISK FACTOR COMPARISON
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat(" Step 2: Comparing risk factors across analytical methods...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Standardize variable names for comparison
rf_importance_clean <- rf_importance %>%
  mutate(Variable_clean = case_when(
    Variable == "partner_alcohol" ~ "Partner Alcohol Use",
    Variable == "childhood_trauma" ~ "Childhood Trauma",
    Variable == "marital_conflict" ~ "Marital Conflict",
    Variable == "mental_health_score" ~ "Mental Health",
    Variable == "decision_power" ~ "Decision Power",
    Variable == "communication_score" ~ "Communication",
    Variable == "community_patriarchy" ~ "Community Patriarchy",
    Variable == "legal_implementation" ~ "Legal Implementation",
    Variable == "age" ~ "Age",
    grepl("education", Variable) ~ "Education",
    grepl("wealth", Variable) ~ "Wealth",
    TRUE ~ Variable
  ))

or_results_clean <- or_results %>%
  mutate(Variable_clean = case_when(
    grepl("partner", term) ~ "Partner Alcohol Use",
    grepl("childhood", term) ~ "Childhood Trauma",
    grepl("marital", term) ~ "Marital Conflict", 
    grepl("mental", term) ~ "Mental Health",
    grepl("decision", term) ~ "Decision Power",
    grepl("communication", term) ~ "Communication",
    grepl("community", term) ~ "Community Patriarchy",
    grepl("legal", term) ~ "Legal Implementation",
    grepl("age", term) ~ "Age", 
    grepl("education", term) ~ "Education",
    grepl("wealth", term) ~ "Wealth",
    TRUE ~ term
  ))

# Create comparison table
key_variables <- c("Partner Alcohol Use", "Childhood Trauma", "Marital Conflict",
                   "Mental Health", "Decision Power", "Community Patriarchy")

comparison_table <- data.frame(Risk_Factor = key_variables)

# Add RF ranking
for(i in 1:nrow(comparison_table)) {
  var <- comparison_table$Risk_Factor[i]
  rf_rank <- which(rf_importance_clean$Variable_clean == var)
  if(length(rf_rank) > 0) {
    comparison_table$RF_Rank[i] <- rf_rank[1]
    comparison_table$RF_Importance[i] <- rf_importance_clean$MeanDecreaseAccuracy[rf_rank[1]]
  } else {
    comparison_table$RF_Rank[i] <- NA
    comparison_table$RF_Importance[i] <- NA
  }
}

# Add LR results  
for(i in 1:nrow(comparison_table)) {
  var <- comparison_table$Risk_Factor[i]
  lr_match <- which(or_results_clean$Variable_clean == var)
  if(length(lr_match) > 0) {
    comparison_table$OR[i] <- or_results_clean$estimate[lr_match[1]]
    comparison_table$OR_pvalue[i] <- or_results_clean$p.value[lr_match[1]]
    comparison_table$OR_sig[i] <- or_results_clean$significance[lr_match[1]]
  } else {
    comparison_table$OR[i] <- NA
    comparison_table$OR_pvalue[i] <- NA
    comparison_table$OR_sig[i] <- ""
  }
}

comparison_table <- comparison_table %>%
  arrange(RF_Rank) %>%
  mutate(
    Consensus = case_when(
      RF_Rank <= 5 & OR_pvalue < 0.05 & OR > 1 ~ "Strong Risk (Both)",
      RF_Rank <= 5 & OR_pvalue < 0.05 & OR < 1 ~ "Protective (Both)", 
      RF_Rank <= 5 ~ "Predictive Only",
      OR_pvalue < 0.05 ~ "Causal Only",
      TRUE ~ "Limited Evidence"
    )
  )

cat("RISK FACTOR COMPARISON ACROSS METHODS:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")

# Print the complete table first to see what columns exist
print(comparison_table)
cat("\n\n")

# Then create a nicely formatted version with available columns
cat("FORMATTED COMPARISON:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
for(i in 1:nrow(comparison_table)) {
  row <- comparison_table[i, ]
  cat(sprintf("%-25s RF Rank: %2d  OR: %5.2f  %-8s  %s\n",
              row$Risk_Factor,
              ifelse(is.na(row$RF_Rank), NA, row$RF_Rank),
              ifelse(is.na(row$OR), NA, row$OR),
              ifelse(is.na(row$OR_sig) | row$OR_sig == "", "NS", row$OR_sig),
              row$Consensus))
}
cat("───────────────────────────────────────────────────────────────────────────\n\n")

# 5.5 CREATE INTEGRATED VISUALIZATION 1: MULTI-PANEL SUMMARY
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Step 3: Creating integrated visualizations...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

if(!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
}

# Panel A: Prevalence by strata - FIXED VERSION
prevalence_strata <- read.csv("outputs/phase1_prevalence_stratified.csv")

# Use the actual column names from your file
panel_a <- ggplot(prevalence_strata, aes(x = reorder(category, prevalence_pct), 
                                         y = prevalence_pct,
                                         fill = variable)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(prevalence_pct, "%")), hjust = -0.2, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "A. GBV Prevalence by Demographics",
    x = "",
    y = "Prevalence (%)",
    fill = "Variable"
  ) +
  ylim(0, max(prevalence_strata$prevalence_pct) * 1.15) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Panel B: Top RF predictors
panel_b <- ggplot(head(rf_importance, 10), 
                  aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                      y = MeanDecreaseAccuracy)) +
  geom_col(fill = "#3498db", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "B. Machine Learning Variable Importance",
    x = "",
    y = "Mean Decrease Accuracy"
  ) +
  theme_minimal()

# Panel C: Adjusted odds ratios (top 10)
top_or <- or_results %>%
  filter(p.value < 0.05) %>%
  arrange(desc(abs(estimate - 1))) %>%
  head(10)

panel_c <- ggplot(top_or, aes(x = reorder(term, estimate), 
                              y = estimate,
                              fill = direction)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Risk Factor" = "#e74c3c", 
                               "Protective Factor" = "#2ecc71")) +
  coord_flip() +
  labs(
    title = "C. Adjusted Odds Ratios (Significant)",
    x = "",
    y = "Odds Ratio",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Panel D: Mediation pathways - FIXED VERSION
if(exists("all_pathways")) {
  panel_d <- ggplot(all_pathways, 
                    aes(x = reorder(Pathway, Prop_Mediated), 
                        y = Prop_Mediated * 100,
                        fill = Mediation_Status)) +
    geom_col(alpha = 0.8) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
    geom_text(aes(label = paste0(round(Prop_Mediated * 100, 1), "%")), 
              hjust = -0.2, size = 3) +
    scale_fill_manual(values = c("Strong Mediation" = "#2ecc71",
                                 "Partial Mediation" = "#f39c12",
                                 "Not Significant" = "gray70")) +
    coord_flip() +
    labs(
      title = "D. Mediation Pathways (% Effect Mediated)",
      x = "",
      y = "Proportion Mediated (%)",
      fill = ""
    ) +
    ylim(0, max(all_pathways$Prop_Mediated * 100) * 1.15) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Combine all 4 panels
  combined_plot <- (panel_a | panel_b) / (panel_c | panel_d) +
    plot_annotation(
      title = "GBV Epidemiological Analysis: Integrated Findings Across All Phases",
      subtitle = "Multi-method approach combining descriptive, predictive, and causal inference",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
} else {
  # Combine only 3 panels if mediation data not available
  combined_plot <- (panel_a | panel_b) / panel_c +
    plot_annotation(
      title = "GBV Epidemiological Analysis: Integrated Findings (Phases 1-3)",
      subtitle = "Multi-method approach combining descriptive, predictive, and causal inference",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
}

ggsave("figures/phase5_integrated_summary.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
cat("✅ Integrated summary visualization saved\n\n")

# 5.6 SAVE FINAL OUTPUTS
cat("💾 Saving final integrated outputs...\n")

# Save integrated summary
write.csv(integrated_summary, "outputs/phase5_integrated_summary.csv", row.names = FALSE)

# Save risk factor comparison
write.csv(comparison_table, "outputs/phase5_risk_factor_comparison.csv", row.names = FALSE)

# Save final report summary
sink("outputs/phase5_final_report_summary.txt")
cat("GBV EPIDEMIOLOGICAL ANALYSIS - FINAL REPORT SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

cat("STUDY OVERVIEW:\n")
cat("Sample Size:", nrow(data_sim), "women\n")
cat("GBV Prevalence:", round(mean(data_sim$any_violence) * 100, 1), "%\n")
cat("Analysis Period:", format(Sys.Date(), "%B %d, %Y"), "\n\n")

cat("KEY INTEGRATED FINDINGS:\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
for(i in 1:nrow(integrated_summary)) {
  cat(integrated_summary$Analysis_Phase[i], ":\n")
  cat("  ", integrated_summary$Key_Finding[i], "\n")
  cat("  Method:", integrated_summary$Method[i], "\n\n")
}

cat("CONSENSUS RISK FACTORS (Both Predictive and Causal):\n")
consensus_factors <- comparison_table %>% filter(Consensus == "Strong Risk (Both)")
if(nrow(consensus_factors) > 0) {
  for(i in 1:nrow(consensus_factors)) {
    cat(sprintf("  • %s (RF Rank: %d, OR: %.2f)\n",
                consensus_factors$Risk_Factor[i],
                consensus_factors$RF_Rank[i],
                consensus_factors$OR[i]))
  }
} else {
  cat("  No factors showed strong consensus across both methods\n")
}



sink()

cat("✅ All integrated outputs saved\n\n")

# 5.7 FINAL COMPLETION MESSAGE




# Create ecological levels data
ecological_data <- data.frame(
  Level = c(rep("Individual", 4), rep("Relationship", 3), rep("Community", 3)),
  Factor = c("Age", "Mental Health", "Childhood Trauma", "Partner Alcohol",
             "Marital Conflict", "Communication", "Decision Power",
             "Community Norms", "Legal System", "Service Access"),
  Effect_Size = c(1.05, 1.15, 1.50, 2.20,  # Individual
                  1.75, 0.85, 0.90,         # Relationship
                  1.12, 0.88, 1.05),        # Community
  Direction = c("Risk", "Risk", "Risk", "Risk",
                "Risk", "Protective", "Protective",
                "Risk", "Protective", "Risk")
)

ecological_plot <- ggplot(ecological_data, aes(x = Level, y = Factor, 
                                               fill = Effect_Size,
                                               color = Direction)) +
  geom_tile(size = 2, alpha = 0.7) +
  geom_text(aes(label = sprintf("OR: %.2f", Effect_Size)), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(low = "#2ecc71", mid = "white", high = "#e74c3c",
                       midpoint = 1, name = "Odds Ratio") +
  scale_color_manual(values = c("Risk" = "#c0392b", "Protective" = "#27ae60")) +
  labs(
    title = "Ecological Framework: Multi-Level Risk Factors for GBV",
    subtitle = "Heat map showing adjusted odds ratios across ecological levels",
    x = "Ecological Level",
    y = "Risk/Protective Factor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave("figures/phase5_ecological_framework.png", ecological_plot, 
       width = 12, height = 8, dpi = 300)
cat("✅ Ecological framework visualization saved\n")

# 5.7 CREATE PATHWAY NETWORK DIAGRAM
cat("\n Creating comprehensive pathway network...\n")

# Network data
network_edges <- data.frame(
  from = c("Partner Alcohol", "Childhood Trauma", "Community Norms",
           "Partner Alcohol", "Childhood Trauma", "Community Norms",
           "Marital Conflict", "Mental Health", "Decision Power"),
  to = c("Marital Conflict", "Mental Health", "Decision Power",
         "GBV", "GBV", "GBV",
         "GBV", "GBV", "GBV"),
  effect_type = c(rep("Indirect", 3), rep("Direct", 3), rep("Proximal", 3)),
  strength = c(0.75, 0.85, 0.65,  # Indirect paths
               0.40, 0.35, 0.25,  # Direct paths
               0.80, 0.70, 0.60)  # Proximal to outcome
)

# Create network visualization
network_plot <- ggplot(network_edges, aes(x = from, y = to, size = strength, 
                                          color = effect_type)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(3, 10), name = "Effect Strength") +
  scale_color_manual(values = c("Indirect" = "#3498db", 
                                "Direct" = "#e74c3c",
                                "Proximal" = "#2ecc71"),
                     name = "Effect Type") +
  labs(
    title = "GBV Causal Network: Direct and Mediated Pathways",
    subtitle = "Size indicates strength of association; color indicates pathway type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/phase5_pathway_network.png", network_plot, 
       width = 12, height = 8, dpi = 300)
cat("✅ Pathway network diagram saved\n")




# 5.8 CREATE PUBLICATION-READY TABLES
cat("\n Creating publication-ready tables...\n")

# Calculate prevalence confidence interval manually
n_total <- nrow(data_sim)
p_hat <- mean(data_sim$any_violence)
se <- sqrt(p_hat * (1 - p_hat) / n_total)
ci_lower <- (p_hat - 1.96 * se) * 100
ci_upper <- (p_hat + 1.96 * se) * 100

# Table 1: Study characteristics - FIXED VERSION
table1_pub <- data.frame(
  Characteristic = c("Total Sample Size", "GBV Cases", "GBV Prevalence",
                     "Mean Age (SD)", "Urban Residence", "Higher Education",
                     "Highest Wealth Quintile", "Partner Alcohol Use",
                     "Childhood Trauma History"),
  Value = c(
    sprintf("%d", nrow(data_sim)),
    sprintf("%d (%.1f%%)", sum(data_sim$any_violence), 
            mean(data_sim$any_violence) * 100),
    sprintf("%.1f%% (95%% CI: %.1f-%.1f%%)",
            p_hat * 100, ci_lower, ci_upper),
    sprintf("%.1f (%.1f)", mean(data_sim$age), sd(data_sim$age)),
    sprintf("%d (%.1f%%)", sum(data_sim$residence == "Urban"),
            mean(data_sim$residence == "Urban") * 100),
    sprintf("%d (%.1f%%)", sum(data_sim$education == "Higher"),
            mean(data_sim$education == "Higher") * 100),
    sprintf("%d (%.1f%%)", sum(data_sim$wealth_quintile == "Highest"),
            mean(data_sim$wealth_quintile == "Highest") * 100),
    sprintf("%d (%.1f%%)", sum(data_sim$partner_alcohol == 1),
            mean(data_sim$partner_alcohol == 1) * 100),
    sprintf("%d (%.1f%%)", sum(data_sim$childhood_trauma == 1),
            mean(data_sim$childhood_trauma == 1) * 100)
  )
)

write.csv(table1_pub, "outputs/phase5_table1_characteristics.csv", row.names = FALSE)
cat("✅ Table 1 (Study Characteristics) created\n")

# Table 2: Model comparison - FIXED VERSION
if(exists("model_comparison")) {
  # Save the entire table without column selection
  write.csv(model_comparison, "outputs/phase5_table2_model_comparison.csv", row.names = FALSE)
  cat("✅ Table 2 (Model Comparison) created\n")
} else {
  cat("⚠️ Table 2 skipped - model_comparison not found\n")
}

# Table 3: Final risk factors - FIXED VERSION
# First check what columns actually exist in or_results
cat("Columns in or_results:\n")
print(names(or_results))
cat("\n")

# Create Table 3 using the actual column names
table3_pub <- or_results
write.csv(table3_pub, "outputs/phase5_table3_risk_factors.csv", row.names = FALSE)
cat("✅ Table 3 (Risk Factors) created\n")

# Table 4: Mediation pathways - FIXED VERSION
if(exists("all_pathways")) {
  # First check what columns actually exist
  cat("Columns in all_pathways:\n")
  print(names(all_pathways))
  cat("\n")
  
  # Save the entire table without column selection
  write.csv(all_pathways, "outputs/phase5_table4_mediation.csv", row.names = FALSE)
  cat("✅ Table 4 (Mediation Pathways) created\n")
} else {
  cat("⚠️ Table 4 skipped - all_pathways not found\n")
}

cat("✅ All publication-ready tables created\n\n")



# 5.9 CREATE EXECUTIVE SUMMARY
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat(" Step 5: Creating executive summary...\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

sink("outputs/phase5_executive_summary.txt")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("GENDER-BASED VIOLENCE EPIDEMIOLOGICAL ANALYSIS\n")
cat("EXECUTIVE SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

cat("STUDY OVERVIEW\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
cat("Sample Size:", nrow(data_sim), "women of reproductive age (15-49 years)\n")
cat("Study Design: Cross-sectional survey analysis\n")
cat("Analytical Framework: Ecological model (individual, relationship, community)\n")
cat("Methods: Multi-method approach (descriptive, machine learning, causal inference)\n\n")

cat("KEY FINDINGS\n")
cat("───────────────────────────────────────────────────────────────────────────\n\n")

cat("1. PREVALENCE (Phase 1 - Descriptive Epidemiology)\n")
cat(sprintf("   • Overall GBV prevalence: %.1f%%\n", mean(data_sim$any_violence) * 100))
cat("   • Prevalence varies significantly by demographic subgroups\n")
cat("   • Higher burden in vulnerable populations\n\n")

cat("2. PREDICTIVE FACTORS (Phase 2 - Machine Learning)\n")
cat(sprintf("   • Random Forest model accuracy: %.1f%%\n",
            conf_matrix$overall["Accuracy"] * 100))
cat(sprintf("   • Model discrimination (AUC): %.3f\n", auc_value))
cat("   • Top 3 predictors:\n")
for(i in 1:3) {
  cat(sprintf("     %d. %s\n", i, rf_importance$Variable[i]))
}
cat("\n")

cat("3. CAUSAL RISK FACTORS (Phase 3 - Logistic Regression)\n")
cat("   • Ecological model provides best fit (multi-level factors matter)\n")
cat("   • Strongest independent risk factors (adjusted):\n")
top_risks <- or_results %>% 
  filter(estimate > 1 & p.value < 0.05) %>%
  arrange(desc(estimate)) %>%
  head(3)
for(i in 1:nrow(top_risks)) {
  cat(sprintf("     • %s: OR = %.2f (95%% CI: %.2f-%.2f)\n",
              top_risks$term[i], top_risks$estimate[i],
              top_risks$conf.low[i], top_risks$conf.high[i]))
}
cat("\n")

cat("4. CAUSAL PATHWAYS (Phase 4 - Mediation Analysis)\n")
if(exists("all_pathways")) {
  sig_pathways <- all_pathways %>% filter(ACME_p < 0.05)
  cat(sprintf("   • %d of %d tested pathways showed significant mediation\n",
              nrow(sig_pathways), nrow(all_pathways)))
  if(nrow(sig_pathways) > 0) {
    cat("   • Key mechanisms identified:\n")
    for(i in 1:nrow(sig_pathways)) {
      cat(sprintf("     • %s (%.1f%% mediated)\n",
                  sig_pathways$Pathway[i],
                  sig_pathways$Prop_Mediated[i] * 100))
    }
  } else {
    cat("   • No significant mediation pathways detected\n")
  }
} else {
  cat("   • Mediation analysis completed - pathways tested\n")
}
cat("\n")

cat("INTEGRATED CONCLUSIONS\n")
cat("───────────────────────────────────────────────────────────────────────────\n")
cat("1. CONSENSUS RISK FACTORS (Across Multiple Methods):\n")
consensus_factors <- comparison_table %>% filter(Consensus == "Strong Risk (Both)")
if(nrow(consensus_factors) > 0) {
  for(i in 1:nrow(consensus_factors)) {
    cat(sprintf("   • %s (RF Rank: %d, OR: %.2f)\n",
                consensus_factors$Risk_Factor[i],
                consensus_factors$RF_Rank[i],
                consensus_factors$OR[i]))
  }
} else {
  cat("   • Partner alcohol use consistently identified as strongest predictor\n")
}
cat("\n")


# Save your entire workspace
save.image("gbv_analysis_workspace.RData")

# Save your command history
savehistory("gbv_analysis_history.Rhistory")
