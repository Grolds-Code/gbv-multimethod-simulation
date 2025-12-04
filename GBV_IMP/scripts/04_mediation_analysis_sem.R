required_packages <- c("tidyverse", "mediation", "lavaan", "DiagrammeR", "ggplot2")

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste(" Installing:", pkg, "\n"))
      install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
    }
  }
}

tryCatch({
  install_if_missing(required_packages)
  library(tidyverse)
  library(mediation)
  library(lavaan)
  library(DiagrammeR)
  library(ggplot2)
  cat("All libraries loaded successfully.\n\n")
}, error = function(e) {
  cat("❌ ERROR in library loading:\n")
  stop(e)
})




# loading data fromphase1

if(!exists("data_sim")) {
  if(file.exists("outputs/phase1_dataset_clean.csv")) {
    data_sim <- read.csv("outputs/phase1_dataset_clean.csv")
    cat("data loaded from Phase 1 output file\n")
  } else {
    stop("❌ ERROR: Phase 1 data not found. Please run Phase 1 first.")
  }
} else {
  cat("ata already in environment\n")
}

# Ensuring numeric outcome for mediation analysis
data_sim$any_violence_num <- as.numeric(data_sim$any_violence)
data_sim$partner_alcohol_num <- as.numeric(data_sim$partner_alcohol)
data_sim$childhood_trauma_num <- as.numeric(data_sim$childhood_trauma)

cat("   • Sample size:", nrow(data_sim), "\n\n")





# Defining candidate pathways based on theory

cat("CANDIDATE PATHWAYS TO TEST:\n")

cat("1. Partner Alcohol → Marital Conflict → GBV\n")
cat("   Theory: Alcohol use increases conflict, which increases violence\n\n")

cat("2. Childhood Trauma → Mental Health → GBV\n")
cat("   Theory: Trauma affects mental health, which influences violence risk\n\n")

cat("3. Community Patriarchy → Decision Power → GBV\n")
cat("   Theory: Patriarchal norms reduce women's power, increasing risk\n\n")

cat("4. Mental Health → Communication → Marital Conflict → GBV\n")
cat("   Theory: Mental health affects communication, which affects conflict\n\n")





# PATHWAY 1: Partner Alcohol → Marital Conflict → GBV

# Step 1: Model for mediator (M ~ X)
model_m1 <- lm(marital_conflict ~ partner_alcohol_num + age + 
                 education + wealth_quintile,
               data = data_sim)

cat("Step 1 - Predictor → Mediator:\n")
cat(sprintf("Partner Alcohol → Marital Conflict: β = %.3f, p = %.4f\n", 
            summary(model_m1)$coefficients["partner_alcohol_num", "Estimate"],
            summary(model_m1)$coefficients["partner_alcohol_num", "Pr(>|t|)"]))
cat("\n")

# Step 2: Model for outcome (Y ~ X + M)
model_y1 <- glm(any_violence_num ~ partner_alcohol_num + marital_conflict + 
                  age + education + wealth_quintile,
                data = data_sim,
                family = binomial(link = "logit"))

cat("Step 2 - Direct and Mediated Effects:\n")
cat(sprintf("Partner Alcohol → GBV (direct): β = %.3f, p = %.4f\n",
            summary(model_y1)$coefficients["partner_alcohol_num", "Estimate"],
            summary(model_y1)$coefficients["partner_alcohol_num", "Pr(>|z|)"]))
cat(sprintf("Marital Conflict → GBV: β = %.3f, p = %.4f\n",
            summary(model_y1)$coefficients["marital_conflict", "Estimate"],
            summary(model_y1)$coefficients["marital_conflict", "Pr(>|z|)"]))
cat("\n")

# Step 3: Mediation analysis with bootstrapping
cat("Step 3 - Mediation analysis with 1000 bootstrap samples...\n")
cat("(This may take 30-60 seconds)\n\n")

set.seed(456)
med_pathway1 <- mediate(model_m1, model_y1, 
                        treat = "partner_alcohol_num", 
                        mediator = "marital_conflict",
                        boot = TRUE, 
                        sims = 1000)

cat("PATHWAY 1 RESULTS:\n")

summary(med_pathway1)
cat("\n")

# Extracing key estimates
pathway1_results <- data.frame(
  Pathway = "Partner Alcohol → Marital Conflict → GBV",
  ACME = med_pathway1$d0,
  ACME_CI_lower = med_pathway1$d0.ci[1],
  ACME_CI_upper = med_pathway1$d0.ci[2],
  ACME_p = med_pathway1$d0.p,
  ADE = med_pathway1$z0,
  ADE_CI_lower = med_pathway1$z0.ci[1],
  ADE_CI_upper = med_pathway1$z0.ci[2],
  ADE_p = med_pathway1$z0.p,
  Total_Effect = med_pathway1$tau.coef,
  Prop_Mediated = med_pathway1$n0
)

cat("INTERPRETATION:\n")
cat(sprintf("Total Effect: %.4f (p = %.4f)\n", 
            pathway1_results$Total_Effect, 
            med_pathway1$tau.p))
cat(sprintf("Direct Effect (ADE): %.4f (95%% CI: %.4f to %.4f)\n",
            pathway1_results$ADE,
            pathway1_results$ADE_CI_lower,
            pathway1_results$ADE_CI_upper))
cat(sprintf("Indirect Effect (ACME): %.4f (95%% CI: %.4f to %.4f)\n",
            pathway1_results$ACME,
            pathway1_results$ACME_CI_lower,
            pathway1_results$ACME_CI_upper))
cat(sprintf("Proportion Mediated: %.1f%%\n", pathway1_results$Prop_Mediated * 100))

if(pathway1_results$ACME_p < 0.05) {
  if(abs(pathway1_results$Prop_Mediated) > 0.5) {
    cat("STRONG mediation detected (>50% mediated)\n")
  } else {
    cat("PARTIAL mediation detected\n")
  }
} else {
  cat("❌ No significant mediation\n")
}
cat("\n\n")




# PATHWAY 2: Childhood Trauma → Mental Health → GBV

# Step 1: Model for mediator
model_m2 <- lm(mental_health_score ~ childhood_trauma_num + age + 
                 education + wealth_quintile,
               data = data_sim)

cat("Step 1 - Predictor → Mediator:\n")
cat(sprintf("Childhood Trauma → Mental Health: β = %.3f, p = %.4f\n",
            summary(model_m2)$coefficients["childhood_trauma_num", "Estimate"],
            summary(model_m2)$coefficients["childhood_trauma_num", "Pr(>|t|)"]))
cat("\n")

# Step 2: Model for outcome
model_y2 <- glm(any_violence_num ~ childhood_trauma_num + mental_health_score + 
                  age + education + wealth_quintile,
                data = data_sim,
                family = binomial(link = "logit"))

cat("Step 2 - Direct and Mediated Effects:\n")
cat(sprintf("Childhood Trauma → GBV (direct): β = %.3f, p = %.4f\n",
            summary(model_y2)$coefficients["childhood_trauma_num", "Estimate"],
            summary(model_y2)$coefficients["childhood_trauma_num", "Pr(>|z|)"]))
cat(sprintf("Mental Health → GBV: β = %.3f, p = %.4f\n",
            summary(model_y2)$coefficients["mental_health_score", "Estimate"],
            summary(model_y2)$coefficients["mental_health_score", "Pr(>|z|)"]))
cat("\n")

# Step 3: Mediation analysis
cat("Step 3 - Mediation analysis with 1000 bootstrap samples...\n\n")

set.seed(789)
med_pathway2 <- mediate(model_m2, model_y2,
                        treat = "childhood_trauma_num",
                        mediator = "mental_health_score",
                        boot = TRUE,
                        sims = 1000)

cat("PATHWAY 2 RESULTS:\n")

summary(med_pathway2)
cat("\n")

# Extracting results
pathway2_results <- data.frame(
  Pathway = "Childhood Trauma → Mental Health → GBV",
  ACME = med_pathway2$d0,
  ACME_CI_lower = med_pathway2$d0.ci[1],
  ACME_CI_upper = med_pathway2$d0.ci[2],
  ACME_p = med_pathway2$d0.p,
  ADE = med_pathway2$z0,
  ADE_CI_lower = med_pathway2$z0.ci[1],
  ADE_CI_upper = med_pathway2$z0.ci[2],
  ADE_p = med_pathway2$z0.p,
  Total_Effect = med_pathway2$tau.coef,
  Prop_Mediated = med_pathway2$n0
)

cat("INTERPRETATION:\n")
cat(sprintf("• Total Effect: %.4f (p = %.4f)\n",
            pathway2_results$Total_Effect,
            med_pathway2$tau.p))
cat(sprintf("• Direct Effect (ADE): %.4f (95%% CI: %.4f to %.4f)\n",
            pathway2_results$ADE,
            pathway2_results$ADE_CI_lower,
            pathway2_results$ADE_CI_upper))
cat(sprintf("• Indirect Effect (ACME): %.4f (95%% CI: %.4f to %.4f)\n",
            pathway2_results$ACME,
            pathway2_results$ACME_CI_lower,
            pathway2_results$ACME_CI_upper))
cat(sprintf("• Proportion Mediated: %.1f%%\n", pathway2_results$Prop_Mediated * 100))

if(pathway2_results$ACME_p < 0.05) {
  if(abs(pathway2_results$Prop_Mediated) > 0.5) {
    cat("STRONG mediation detected (>50% mediated)\n")
  } else {
    cat("PARTIAL mediation detected\n")
  }
} else {
  cat("❌ No significant mediation\n")
}
cat("\n\n")





#  PATHWAY 3: Community Patriarchy → Decision Power → GBV

# Step 1: Model for mediator
model_m3 <- lm(decision_power ~ community_patriarchy + age + 
                 education + wealth_quintile,
               data = data_sim)

cat("Step 1 - Predictor → Mediator:\n")
cat(sprintf("Community Patriarchy → Decision Power: β = %.3f, p = %.4f\n",
            summary(model_m3)$coefficients["community_patriarchy", "Estimate"],
            summary(model_m3)$coefficients["community_patriarchy", "Pr(>|t|)"]))
cat("\n")

# Step 2: Model for outcome
model_y3 <- glm(any_violence_num ~ community_patriarchy + decision_power + 
                  age + education + wealth_quintile,
                data = data_sim,
                family = binomial(link = "logit"))

cat("Step 2 - Direct and Mediated Effects:\n")
cat(sprintf("Community Patriarchy → GBV (direct): β = %.3f, p = %.4f\n",
            summary(model_y3)$coefficients["community_patriarchy", "Estimate"],
            summary(model_y3)$coefficients["community_patriarchy", "Pr(>|z|)"]))
cat(sprintf("Decision Power → GBV: β = %.3f, p = %.4f\n",
            summary(model_y3)$coefficients["decision_power", "Estimate"],
            summary(model_y3)$coefficients["decision_power", "Pr(>|z|)"]))
cat("\n")

# Step 3: Mediation analysis
cat("Step 3 - Mediation analysis with 1000 bootstrap samples...\n\n")

set.seed(101)
med_pathway3 <- mediate(model_m3, model_y3,
                        treat = "community_patriarchy",
                        mediator = "decision_power",
                        boot = TRUE,
                        sims = 1000)

cat("PATHWAY 3 RESULTS:\n")

summary(med_pathway3)
cat("\n")

# Extracting results
pathway3_results <- data.frame(
  Pathway = "Community Patriarchy → Decision Power → GBV",
  ACME = med_pathway3$d0,
  ACME_CI_lower = med_pathway3$d0.ci[1],
  ACME_CI_upper = med_pathway3$d0.ci[2],
  ACME_p = med_pathway3$d0.p,
  ADE = med_pathway3$z0,
  ADE_CI_lower = med_pathway3$z0.ci[1],
  ADE_CI_upper = med_pathway3$z0.ci[2],
  ADE_p = med_pathway3$z0.p,
  Total_Effect = med_pathway3$tau.coef,
  Prop_Mediated = med_pathway3$n0
)

cat("INTERPRETATION:\n")
cat(sprintf("Total Effect: %.4f (p = %.4f)\n",
            pathway3_results$Total_Effect,
            med_pathway3$tau.p))
cat(sprintf("Direct Effect (ADE): %.4f (95%% CI: %.4f to %.4f)\n",
            pathway3_results$ADE,
            pathway3_results$ADE_CI_lower,
            pathway3_results$ADE_CI_upper))
cat(sprintf("Indirect Effect (ACME): %.4f (95%% CI: %.4f to %.4f)\n",
            pathway3_results$ACME,
            pathway3_results$ACME_CI_lower,
            pathway3_results$ACME_CI_upper))
cat(sprintf("Proportion Mediated: %.1f%%\n", pathway3_results$Prop_Mediated * 100))

if(pathway3_results$ACME_p < 0.05) {
  if(abs(pathway3_results$Prop_Mediated) > 0.5) {
    cat("STRONG mediation detected (>50% mediated)\n")
  } else {
    cat("PARTIAL mediation detected\n")
  }
} else {
  cat("❌ No significant mediation\n")
}
cat("\n\n")





# all pathway results 

all_pathways <- rbind(pathway1_results, pathway2_results, pathway3_results)

# again, let's check what columns actually exist
cat("Available columns in all_pathways:\n")
print(names(all_pathways))
cat("\n")

all_pathways <- all_pathways %>%
  mutate(
    Mediation_Status = case_when(
      ACME_p >= 0.05 ~ "Not Significant",
      abs(Prop_Mediated) > 0.5 ~ "Strong Mediation",
      TRUE ~ "Partial Mediation"
    ),
    Significance = ifelse(ACME_p < 0.001, "***",
                          ifelse(ACME_p < 0.01, "**",
                                 ifelse(ACME_p < 0.05, "*", "NS")))
  )

cat("PATHWAY SUMMARY TABLE:\n")


# Using the actual column names that exist
print(all_pathways)
cat("\n")





#visualization

cat("Step 3: Creating pathway visualizations...\n")


if(!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
}

# Plot 1: Mediation effect sizes
p1 <- ggplot(all_pathways, aes(x = reorder(Pathway, ACME), y = ACME, fill = ACME)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = ACME_CI_lower, ymax = ACME_CI_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_flip() +
  labs(
    title = "Mediation Analysis: Indirect Effects (ACME)",
    subtitle = "Average Causal Mediation Effects with 95% Confidence Intervals",
    x = "",
    y = "Indirect Effect Size (ACME)",
    fill = "Effect Size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave("figures/phase4_mediation_effects.png", p1, width = 12, height = 6, dpi = 300)
cat("Mediation effects plot saved\n")

# Plot 2: Proportion mediated
p2 <- ggplot(all_pathways, aes(x = reorder(Pathway, Prop_Mediated), 
                               y = Prop_Mediated * 100,
                               fill = Prop_Mediated)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(Prop_Mediated * 100, 1), "%")), 
            hjust = -0.2, size = 4) +
  scale_fill_gradient(low = "white", high = "#2ecc71") +
  coord_flip() +
  labs(
    title = "Proportion of Total Effect Mediated by Each Pathway",
    subtitle = "Dashed line indicates 50% mediation threshold",
    x = "",
    y = "Proportion Mediated (%)",
    fill = "Proportion (%)"
  ) +
  ylim(0, max(all_pathways$Prop_Mediated * 100) * 1.15) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave("figures/phase4_proportion_mediated.png", p2, width = 12, height = 6, dpi = 300)
cat("proportion mediated plot saved\n")

# Plot 3: Simple comparison plot
comparison_data <- data.frame(
  Pathway = rep(all_pathways$Pathway, 2),
  Effect_Type = rep(c("Direct (ADE)", "Indirect (ACME)"), each = nrow(all_pathways)),
  Effect_Size = c(all_pathways$ADE, all_pathways$ACME)
)

p3 <- ggplot(comparison_data, aes(x = Pathway, y = Effect_Size, fill = Effect_Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Direct (ADE)" = "#3498db", "Indirect (ACME)" = "#e74c3c")) +
  coord_flip() +
  labs(
    title = "Direct vs. Indirect Effects by Pathway",
    subtitle = "ADE = Average Direct Effect, ACME = Average Causal Mediation Effect",
    x = "",
    y = "Effect Size",
    fill = "Effect Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave("figures/phase4_direct_indirect.png", p3, width = 12, height = 6, dpi = 300)
cat("direct vs indirect effects plot saved\n\n")


# interpretation summary

cat(" PHASE 4 INTERPRETATION SUMMARY\n")


cat("pathway Analysis Phase Complete!\n\n")

cat("KEY FINDINGS:\n")


# significant mediations
sig_mediations <- sum(all_pathways$ACME_p < 0.05)
strong_mediations <- sum(all_pathways$Mediation_Status == "Strong Mediation")

cat(sprintf("1. TESTED PATHWAYS: %d\n", nrow(all_pathways)))
cat(sprintf("Significant mediations: %d\n", sig_mediations))
cat(sprintf(" Strong mediations (>50%%): %d\n", strong_mediations))
cat("\n")

if(sig_mediations > 0) {
  cat("2. SIGNIFICANT PATHWAYS:\n")
  sig_pathways <- all_pathways %>% filter(ACME_p < 0.05)
  for(i in 1:nrow(sig_pathways)) {
    cat(sprintf("   • %s\n", sig_pathways$Pathway[i]))
    cat(sprintf("     - Indirect effect: %.4f (p = %.4f)\n", 
                sig_pathways$ACME[i], sig_pathways$ACME_p[i]))
    cat(sprintf("     - Proportion mediated: %.1f%%\n", 
                sig_pathways$Prop_Mediated[i] * 100))
  }
  cat("\n")
}

cat("3. STRONGEST PATHWAY:\n")
cat(sprintf("    %s\n", strongest_pathway))
cat(sprintf("    Explains %.1f%% of total effect through mediation\n",
            all_pathways$Prop_Mediated[strongest_pathway_idx] * 100))
cat("\n")
