# Figures for Article
# This script generates all figures for the article using the combined participants 
# data from both Study 1 and Study 2.

# ==============================================================================
# Imports and Configuration
# ==============================================================================

# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Suppress warnings for cleaner output
options(warn = -1)

# Set plotting theme
theme_set(theme_bw() + 
          theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
                panel.grid.minor = element_blank(),
                text = element_text(size = 10),
                axis.title = element_text(size = 11),
                plot.title = element_text(size = 12),
                axis.text = element_text(size = 10),
                legend.text = element_text(size = 10),
                legend.title = element_blank()))

# ==============================================================================
# Data Loading
# ==============================================================================

# Define paths
# Assume script is run from the analysis directory or code directory
current_dir <- getwd()
if (basename(current_dir) == "analysis") {
  BASE_DIR <- dirname(current_dir)
} else if (basename(current_dir) == "code") {
  BASE_DIR <- current_dir
} else {
  # Try to find the code directory
  BASE_DIR <- current_dir
}
DATA_DIR <- file.path(BASE_DIR, "data")

# Create figures directory if it doesn't exist
FIGURES_DIR <- file.path(BASE_DIR, "figures")
if (!dir.exists(FIGURES_DIR)) {
  dir.create(FIGURES_DIR, recursive = TRUE)
  cat(sprintf("Created figures directory: %s\n", FIGURES_DIR))
}

# Load the combined participants file (CSV has single header row)
combined_file <- file.path(DATA_DIR, "combined_participants_S1&S2.csv")
df_all <- read_csv(combined_file, show_col_types = FALSE)

cat(sprintf("Total participants in combined file: %d\n", nrow(df_all)))

# Convert key columns to appropriate types
df_all$id <- as.character(df_all$id)
df_all$study <- as.character(df_all$study)
df_all$task <- as.character(df_all$task)
df_all$group <- as.numeric(df_all$group)

# Handle exclusions
# The 'exclude' column indicates if participant should be excluded from entire study
if ("exclude" %in% names(df_all)) {
  # Check what values indicate exclusion (likely True/1 for excluded)
  cat("\nExclusion column value counts:\n")
  print(table(df_all$exclude))
  
  # Filter out excluded participants
  # Assuming True, 1, or 'True' means excluded
  exclude_values <- toupper(as.character(df_all$exclude))
  df_all_included <- df_all %>%
    filter(!exclude_values %in% c("TRUE", "1", "YES"))
  
  cat(sprintf("Participants after exclusions: %d\n", nrow(df_all_included)))
} else {
  cat("Warning: 'exclude' column not found. Using all participants.\n")
  df_all_included <- df_all
}

# Separate by study for convenience
df_s1 <- df_all_included %>%
  filter(toupper(as.character(study)) %in% c("S1", "1"))

df_s2 <- df_all_included %>%
  filter(toupper(as.character(study)) %in% c("S2", "2"))

cat(sprintf("\nStudy 1 participants (after exclusions): %d\n", nrow(df_s1)))
cat(sprintf("Study 2 participants (after exclusions): %d\n", nrow(df_s2)))

# Check group distribution for both studies
cat("\nStudy 1 group distribution:\n")
print(table(df_s1$group))
cat("\nStudy 2 group distribution:\n")
print(table(df_s2$group))
cat("(0 = Control, 1 = Faking Low/Introversion, 2 = Faking High/Extraversion)\n")

# Check task distribution
cat("\nStudy 1 task distribution:\n")
print(table(df_s1$task))
cat("\nStudy 2 task distribution:\n")
print(table(df_s2$task))

# Store dataframes for use in figures
# Main dataframe with all included participants
df <- df_all_included

# Study-specific dataframes
df_s1_fig <- df_s1
df_s2_fig <- df_s2

cat(sprintf("\nDataframes ready for figure creation:\n"))
cat(sprintf("  df: All participants (N = %d)\n", nrow(df)))
cat(sprintf("  df_s1_fig: Study 1 participants (N = %d)\n", nrow(df_s1_fig)))
cat(sprintf("  df_s2_fig: Study 2 participants (N = %d)\n", nrow(df_s2_fig)))

# ==============================================================================
# Figure 2: Implicit-Explicit Correlations at Time 1 (Study 1)
# ==============================================================================

# Filter to Study 1, Time 1 data
df_s1_t1 <- df_s1_fig

# Prepare IAT data (WT = IAT)
iat_data <- df_s1_t1 %>%
  filter(toupper(task) == "IAT")

iat_implicit <- as.numeric(iat_data$t1_dscore)
iat_explicit <- as.numeric(iat_data$t1_ques)

# Remove missing values
iat_pairs <- data.frame(
  implicit = iat_implicit,
  explicit = iat_explicit
) %>%
  filter(!is.na(implicit) & !is.na(explicit))

# Prepare qIAT data
qiat_data <- df_s1_t1 %>%
  filter(toupper(task) == "QIAT")

qiat_implicit <- as.numeric(qiat_data$t1_dscore)
qiat_explicit <- as.numeric(qiat_data$t1_ques)

# Remove missing values
qiat_pairs <- data.frame(
  implicit = qiat_implicit,
  explicit = qiat_explicit
) %>%
  filter(!is.na(implicit) & !is.na(explicit))

# Create figure
p2 <- ggplot() +
  # Plot IAT (WT) - orange
  geom_point(data = iat_pairs, aes(x = implicit, y = explicit), 
             alpha = 0.5, color = "orange", size = 1.5) +
  # Plot qIAT - blue
  geom_point(data = qiat_pairs, aes(x = implicit, y = explicit), 
             alpha = 0.5, color = "blue", size = 1.5) +
  # Add regression lines
  geom_smooth(data = iat_pairs, aes(x = implicit, y = explicit), 
              method = "lm", se = FALSE, color = "orange", linewidth = 0.8) +
  geom_smooth(data = qiat_pairs, aes(x = implicit, y = explicit), 
              method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  # Labels and formatting
  xlab("Implicit Score") +
  ylab("Explicit Score (questionnaire)") +
  xlim(-2.5, 2.5) +
  ylim(8, 51) +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "", values = c("IAT" = "orange", "qIAT" = "blue"),
                     labels = c("IAT", "qIAT")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

# Add legend manually since we're using separate geoms
p2 <- p2 + 
  annotate("point", x = -2.3, y = 50, color = "orange", size = 3, alpha = 1) +
  annotate("text", x = -2.1, y = 50, label = "IAT", hjust = 0, size = 3.5) +
  annotate("point", x = -2.3, y = 47, color = "blue", size = 3, alpha = 1) +
  annotate("text", x = -2.1, y = 47, label = "qIAT", hjust = 0, size = 3.5)

print(p2)

# Save figure
ggsave(file.path(FIGURES_DIR, "figure2.png"), p2, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat(sprintf("Saved Figure 2 to: %s\n", file.path(FIGURES_DIR, "figure2.png")))

# Print correlation statistics
cat("\nCorrelation Statistics:\n")
if (nrow(iat_pairs) > 1) {
  iat_corr_test <- cor.test(iat_pairs$implicit, iat_pairs$explicit)
  iat_corr <- iat_corr_test$estimate
  iat_p <- iat_corr_test$p.value
  cat(sprintf("IAT: r = %.3f, p = %.4f, n = %d\n", iat_corr, iat_p, nrow(iat_pairs)))
}
if (nrow(qiat_pairs) > 1) {
  qiat_corr_test <- cor.test(qiat_pairs$implicit, qiat_pairs$explicit)
  qiat_corr <- qiat_corr_test$estimate
  qiat_p <- qiat_corr_test$p.value
  cat(sprintf("qIAT: r = %.3f, p = %.4f, n = %d\n", qiat_corr, qiat_p, nrow(qiat_pairs)))
}

# ==============================================================================
# Figure 3: Mean Normalized Score Differences (Time 2 - Time 1) by Group (Study 1)
# ==============================================================================

# Filter to Study 1
df_s1_fig3 <- df_s1_fig

# Group mapping (0 = Control, 1 = Faking Low, 2 = Faking High)
group_names <- c("0" = "Control", "1" = "Faking Low", "2" = "Faking High")

# Prepare data for plotting
# We need to compute normalized differences (T2_norm - T1_norm) for each measure

# 1. Questionnaire: Compute normalized scores using T1 baseline
t1_ques_all <- as.numeric(df_s1_fig3$t1_ques)
t1_ques_all <- t1_ques_all[!is.na(t1_ques_all)]
t1_ques_mean <- mean(t1_ques_all, na.rm = TRUE)
t1_ques_sd <- sd(t1_ques_all, na.rm = TRUE)

df_s1_fig3$t1_ques_norm <- (as.numeric(df_s1_fig3$t1_ques) - t1_ques_mean) / t1_ques_sd
df_s1_fig3$t2_ques_norm <- (as.numeric(df_s1_fig3$t2_ques) - t1_ques_mean) / t1_ques_sd
df_s1_fig3$ques_diff <- df_s1_fig3$t2_ques_norm - df_s1_fig3$t1_ques_norm

# 2. IAT: Use normalized D-scores (t1_com_d_norm and t2_com_d_norm)
iat_data <- df_s1_fig3 %>%
  filter(toupper(task) == "IAT")

iat_data$iat_diff <- (as.numeric(iat_data$t2_com_d_norm) - 
                      as.numeric(iat_data$t1_com_d_norm))

# 3. qIAT: Use normalized D-scores (t1_com_d_norm and t2_com_d_norm)
qiat_data <- df_s1_fig3 %>%
  filter(toupper(task) == "QIAT")

qiat_data$qiat_diff <- (as.numeric(qiat_data$t2_com_d_norm) - 
                       as.numeric(qiat_data$t1_com_d_norm))

# Compute means for each group and measure
results <- list()

for (group_num in c(0, 1, 2)) {
  group_name <- group_names[as.character(group_num)]
  
  # Questionnaire (all participants have questionnaire data)
  ques_group <- df_s1_fig3 %>%
    filter(group == group_num)
  ques_diff_mean <- mean(ques_group$ques_diff, na.rm = TRUE)
  results[[length(results) + 1]] <- data.frame(
    Group = group_name,
    Measure = "Questionnaire",
    Mean_Diff = ques_diff_mean
  )
  
  # IAT (only participants with IAT task)
  iat_group <- iat_data %>%
    filter(group == group_num)
  if (nrow(iat_group) > 0) {
    iat_diff_mean <- mean(iat_group$iat_diff, na.rm = TRUE)
    results[[length(results) + 1]] <- data.frame(
      Group = group_name,
      Measure = "IAT",
      Mean_Diff = iat_diff_mean
    )
  }
  
  # qIAT (only participants with qIAT task)
  qiat_group <- qiat_data %>%
    filter(group == group_num)
  if (nrow(qiat_group) > 0) {
    qiat_diff_mean <- mean(qiat_group$qiat_diff, na.rm = TRUE)
    results[[length(results) + 1]] <- data.frame(
      Group = group_name,
      Measure = "qIAT",
      Mean_Diff = qiat_diff_mean
    )
  }
}

# Convert to DataFrame
results_df <- bind_rows(results)

# Pivot for easier plotting
pivot_df <- results_df %>%
  pivot_wider(names_from = Measure, values_from = Mean_Diff) %>%
  select(Group, qIAT, IAT, Questionnaire) %>%
  arrange(match(Group, c("Faking High", "Faking Low", "Control")))

# Create figure
# Prepare data for ggplot
plot_data <- results_df %>%
  mutate(Group = factor(Group, levels = c("Faking High", "Faking Low", "Control")),
         Measure = factor(Measure, levels = c("qIAT", "IAT", "Questionnaire")))

# Define colors: qIAT (blue), IAT (orange), Questionnaire (dark green)
colors <- c("qIAT" = "blue", "IAT" = "orange", "Questionnaire" = "darkgreen")

p3 <- ggplot(plot_data, aes(x = Group, y = Mean_Diff, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = colors) +
  xlab("Experimental Group") +
  ylab("Mean of Difference Between Normalized Scores") +
  ylim(-2, 2.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(limits = c("Faking High", "Faking Low", "Control"))

print(p3)

# Save figure
ggsave(file.path(FIGURES_DIR, "figure3.png"), p3, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat(sprintf("Saved Figure 3 to: %s\n", file.path(FIGURES_DIR, "figure3.png")))

# Print summary statistics
cat("\nMean Differences (T2_norm - T1_norm) by Group and Measure:\n")
pivot_df_rounded <- pivot_df %>%
  mutate(across(where(is.numeric), round, 3))
print(pivot_df_rounded)

# ==============================================================================
# Figure 4 (Study 2): Implicit-Explicit Correlations at Time 1
# ==============================================================================

# Filter to Study 2, Time 1 data
df_s2_t1 <- df_s2_fig

# Prepare IAT data
iat_data <- df_s2_t1 %>%
  filter(toupper(task) == "IAT")

iat_implicit <- as.numeric(iat_data$t1_dscore)
iat_explicit <- as.numeric(iat_data$t1_ques)

# Remove missing values
iat_pairs <- data.frame(
  implicit = iat_implicit,
  explicit = iat_explicit
) %>%
  filter(!is.na(implicit) & !is.na(explicit))

# Prepare qIAT data
qiat_data <- df_s2_t1 %>%
  filter(toupper(task) == "QIAT")

qiat_implicit <- as.numeric(qiat_data$t1_dscore)
qiat_explicit <- as.numeric(qiat_data$t1_ques)

# Remove missing values
qiat_pairs <- data.frame(
  implicit = qiat_implicit,
  explicit = qiat_explicit
) %>%
  filter(!is.na(implicit) & !is.na(explicit))

# Create figure
p4 <- ggplot() +
  # Plot IAT - orange
  geom_point(data = iat_pairs, aes(x = implicit, y = explicit), 
             alpha = 0.5, color = "orange", size = 1.5) +
  # Plot qIAT - blue
  geom_point(data = qiat_pairs, aes(x = implicit, y = explicit), 
             alpha = 0.5, color = "blue", size = 1.5) +
  # Add regression lines
  geom_smooth(data = iat_pairs, aes(x = implicit, y = explicit), 
              method = "lm", se = FALSE, color = "orange", linewidth = 0.8) +
  geom_smooth(data = qiat_pairs, aes(x = implicit, y = explicit), 
              method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  # Labels and formatting
  xlab("Implicit Score") +
  ylab("Explicit Score (questionnaire)") +
  xlim(-2.5, 2.5) +
  ylim(8, 52) +
  theme(legend.position = "left",
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank())

# Add legend manually
p4 <- p4 + 
  annotate("point", x = -2.3, y = 51, color = "orange", size = 3, alpha = 1) +
  annotate("text", x = -2.1, y = 51, label = "IAT", hjust = 0, size = 3.5) +
  annotate("point", x = -2.3, y = 48, color = "blue", size = 3, alpha = 1) +
  annotate("text", x = -2.1, y = 48, label = "qIAT", hjust = 0, size = 3.5)

print(p4)

# Save figure
ggsave(file.path(FIGURES_DIR, "figure4.png"), p4, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat(sprintf("Saved Figure 4 to: %s\n", file.path(FIGURES_DIR, "figure4.png")))

# Print correlation statistics
cat("\nCorrelation Statistics (Study 2):\n")
if (nrow(iat_pairs) > 1) {
  iat_corr_test <- cor.test(iat_pairs$implicit, iat_pairs$explicit)
  iat_corr <- iat_corr_test$estimate
  iat_p <- iat_corr_test$p.value
  cat(sprintf("IAT: r = %.3f, p = %.4f, n = %d\n", iat_corr, iat_p, nrow(iat_pairs)))
}
if (nrow(qiat_pairs) > 1) {
  qiat_corr_test <- cor.test(qiat_pairs$implicit, qiat_pairs$explicit)
  qiat_corr <- qiat_corr_test$estimate
  qiat_p <- qiat_corr_test$p.value
  cat(sprintf("qIAT: r = %.3f, p = %.4f, n = %d\n", qiat_corr, qiat_p, nrow(qiat_pairs)))
}

# ==============================================================================
# Figure 5: Mean Normalized Score Differences (Time 2 - Time 1) by Group (Study 2)
# ==============================================================================

# Filter to Study 2
df_s2_fig5 <- df_s2_fig

# Group mapping (0 = Control, 1 = Faking Low, 2 = Faking High)
group_names <- c("0" = "Control", "1" = "Faking Low", "2" = "Faking High")

# Prepare data for plotting
# We need to compute normalized differences (T2_norm - T1_norm) for each measure

# 1. Questionnaire: Compute normalized scores using T1 baseline
t1_ques_all <- as.numeric(df_s2_fig5$t1_ques)
t1_ques_all <- t1_ques_all[!is.na(t1_ques_all)]
t1_ques_mean <- mean(t1_ques_all, na.rm = TRUE)
t1_ques_sd <- sd(t1_ques_all, na.rm = TRUE)

df_s2_fig5$t1_ques_norm <- (as.numeric(df_s2_fig5$t1_ques) - t1_ques_mean) / t1_ques_sd
df_s2_fig5$t2_ques_norm <- (as.numeric(df_s2_fig5$t2_ques) - t1_ques_mean) / t1_ques_sd
df_s2_fig5$ques_diff <- df_s2_fig5$t2_ques_norm - df_s2_fig5$t1_ques_norm

# 2. IAT: Use normalized D-scores (t1_com_d_norm and t2_com_d_norm)
iat_data <- df_s2_fig5 %>%
  filter(toupper(task) == "IAT")

iat_data$iat_diff <- (as.numeric(iat_data$t2_com_d_norm) - 
                      as.numeric(iat_data$t1_com_d_norm))

# 3. qIAT: Use normalized D-scores (t1_com_d_norm and t2_com_d_norm)
qiat_data <- df_s2_fig5 %>%
  filter(toupper(task) == "QIAT")

qiat_data$qiat_diff <- (as.numeric(qiat_data$t2_com_d_norm) - 
                        as.numeric(qiat_data$t1_com_d_norm))

# Compute means for each group and measure
results <- list()

for (group_num in c(0, 1, 2)) {
  group_name <- group_names[as.character(group_num)]
  
  # Questionnaire (all participants have questionnaire data)
  ques_group <- df_s2_fig5 %>%
    filter(group == group_num)
  ques_diff_mean <- mean(ques_group$ques_diff, na.rm = TRUE)
  results[[length(results) + 1]] <- data.frame(
    Group = group_name,
    Measure = "Questionnaire",
    Mean_Diff = ques_diff_mean
  )
  
  # IAT (only participants with IAT task)
  iat_group <- iat_data %>%
    filter(group == group_num)
  if (nrow(iat_group) > 0) {
    iat_diff_mean <- mean(iat_group$iat_diff, na.rm = TRUE)
    results[[length(results) + 1]] <- data.frame(
      Group = group_name,
      Measure = "IAT",
      Mean_Diff = iat_diff_mean
    )
  }
  
  # qIAT (only participants with qIAT task)
  qiat_group <- qiat_data %>%
    filter(group == group_num)
  if (nrow(qiat_group) > 0) {
    qiat_diff_mean <- mean(qiat_group$qiat_diff, na.rm = TRUE)
    results[[length(results) + 1]] <- data.frame(
      Group = group_name,
      Measure = "qIAT",
      Mean_Diff = qiat_diff_mean
    )
  }
}

# Convert to DataFrame
results_df <- bind_rows(results)

# Pivot for easier plotting
pivot_df <- results_df %>%
  pivot_wider(names_from = Measure, values_from = Mean_Diff) %>%
  select(Group, qIAT, IAT, Questionnaire) %>%
  arrange(match(Group, c("Faking High", "Faking Low", "Control")))

# Create figure
# Prepare data for ggplot
plot_data <- results_df %>%
  mutate(Group = factor(Group, levels = c("Faking High", "Faking Low", "Control")),
         Measure = factor(Measure, levels = c("qIAT", "IAT", "Questionnaire")))

# Define colors: qIAT (blue), IAT (orange), Questionnaire (dark green)
colors <- c("qIAT" = "blue", "IAT" = "orange", "Questionnaire" = "darkgreen")

p5 <- ggplot(plot_data, aes(x = Group, y = Mean_Diff, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = colors) +
  xlab("Experimental Group") +
  ylab("Mean of Difference Between Normalized Scores") +
  ylim(-2.0, 2.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(limits = c("Faking High", "Faking Low", "Control"))

print(p5)

# Save figure
ggsave(file.path(FIGURES_DIR, "figure5.png"), p5, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat(sprintf("Saved Figure 5 to: %s\n", file.path(FIGURES_DIR, "figure5.png")))

# Print summary statistics
cat("\nMean Differences (T2_norm - T1_norm) by Group and Measure (Study 2):\n")
pivot_df_rounded <- pivot_df %>%
  mutate(across(where(is.numeric), round, 3))
print(pivot_df_rounded)
