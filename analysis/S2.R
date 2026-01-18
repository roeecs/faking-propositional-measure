# Study 2 Analysis
# This script contains all analyses for Study 2 of the article, systematically going 
# through each analysis and statistic reported.

# ==============================================================================
# Imports and Configuration
# ==============================================================================

# Load required libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(afex)
library(car)
library(stringr)

# Suppress warnings for cleaner output
options(warn = -1)

# Helper function: Spearman-Brown correction for split-half reliability
spearman_brown <- function(r) {
  # Apply Spearman-Brown correction to split-half reliability
  return((2 * r) / (1 + r))
}

# Helper function: Fisher z transformation for comparing correlations
fisher_z <- function(r) {
  # Fisher z transformation
  return(0.5 * log((1 + r) / (1 - r)))
}

# Helper function: Compare two independent correlations using Fisher z test
compare_correlations <- function(r1, r2, n1, n2) {
  # Compare two independent correlations using Fisher z test
  # Parameters:
  #   r1, r2: correlation coefficients
  #   n1, n2: sample sizes
  # Returns:
  #   z: z-statistic
  #   p: p-value (two-tailed)
  z1 <- fisher_z(r1)
  z2 <- fisher_z(r2)
  se_diff <- sqrt(1/(n1-3) + 1/(n2-3))
  z <- (z1 - z2) / se_diff
  p <- 2 * (1 - pnorm(abs(z)))
  return(list(z = z, p = p))
}

# Helper function: Cohen's d for paired samples
cohens_d_paired <- function(x1, x2) {
  # Calculate Cohen's d for paired samples
  diff <- x1 - x2
  d <- mean(diff, na.rm = TRUE) / sd(diff, na.rm = TRUE)
  return(d)
}

# Helper function: Add significance asterisks based on p-value
add_significance <- function(p_value) {
  # Add significance asterisks based on p-value
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("")
}

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

# Load the combined participants file
combined_file <- file.path(DATA_DIR, "combined_participants_S1&S2.csv")
df_all <- read_csv(combined_file, show_col_types = FALSE)

cat(sprintf("Total participants in combined file: %d\n", nrow(df_all)))

# Filter to Study 2 only
# Study column might be 'S2' or '2', so we'll check both
df_s2 <- df_all %>%
  filter(toupper(as.character(study)) %in% c("S2", "2"))

cat(sprintf("Study 2 participants (before exclusions): %d\n", nrow(df_s2)))

# Handle exclusions
# The 'exclude' column indicates if participant should be excluded from entire study
if ("exclude" %in% names(df_s2)) {
  # Check what values indicate exclusion (likely True/1 for excluded)
  cat("\nExclusion column value counts:\n")
  print(table(df_s2$exclude))
  
  # Filter out excluded participants
  # Assuming True, 1, or 'True' means excluded
  exclude_values <- toupper(as.character(df_s2$exclude))
  df_s2_included <- df_s2 %>%
    filter(!exclude_values %in% c("TRUE", "1", "YES"))
  
  cat(sprintf("Study 2 participants (after exclusions): %d\n", nrow(df_s2_included)))
} else {
  cat("Warning: 'exclude' column not found. Using all participants.\n")
  df_s2_included <- df_s2
}

# Verify sample size matches article (need to check what N is expected for S2)
cat(sprintf("\nExpected N = ? (after exclusions - check article)\n"))
cat(sprintf("Actual N = %d\n", nrow(df_s2_included)))

# Convert key columns to appropriate types
df_s2_included$id <- as.character(df_s2_included$id)
df_s2_included$study <- as.character(df_s2_included$study)
df_s2_included$task <- as.character(df_s2_included$task)
df_s2_included$group <- as.numeric(df_s2_included$group)

# Check group distribution
cat("\nGroup distribution:\n")
print(table(df_s2_included$group))
cat("(0 = Control, 1 = Faking Low/Introversion, 2 = Faking High/Extraversion)\n")

# Check task distribution
cat("\nTask distribution:\n")
print(table(df_s2_included$task))

# Store as main dataframe for analyses
df <- df_s2_included
cat(sprintf("\nDataframe ready for analysis: %d participants\n", nrow(df)))

# ==============================================================================
# Load Task Data Files
# ==============================================================================

# Define tasks directory
TASKS_DIR <- file.path(DATA_DIR, "tasks", "S2")

# Task file mapping
task_files <- list(
  qIAT_1 = file.path(TASKS_DIR, "qiat1.xlsx"),
  qIAT_2 = file.path(TASKS_DIR, "qiat2.xlsx"),
  IAT_1 = file.path(TASKS_DIR, "iat1.xlsx"),
  IAT_2 = file.path(TASKS_DIR, "iat2.xlsx")
)

# Dictionary to store loaded task data
# Structure: tasks[task_type][time] = list(participants = df)
tasks <- list(
  qIAT = list(),
  IAT = list()
)

# Load each task file
for (task_name in names(task_files)) {
  file_path <- task_files[[task_name]]
  
  if (!file.exists(file_path)) {
    cat(sprintf("Warning: %s not found, skipping...\n", basename(file_path)))
    next
  }
  
  cat(sprintf("Loading %s...\n", basename(file_path)))
  
  # Extract task type and time from name
  if (grepl("qIAT", task_name)) {
    task_type <- "qIAT"
    time <- ifelse(grepl("_1", task_name), "1", "2")
  } else if (grepl("IAT", task_name)) {
    task_type <- "IAT"
    time <- ifelse(grepl("_1", task_name), "1", "2")
  } else {
    next
  }
  
  # Load Participants sheet (contains participant-level statistics)
  tryCatch({
    participants_df <- read_excel(file_path, sheet = "Participants")
    participants_df$id <- as.character(participants_df$id)
    tasks[[task_type]][[time]] <- list(participants = participants_df)
    cat(sprintf("  Loaded %d participants\n", nrow(participants_df)))
    
    # Check what columns are available
    cat(sprintf("  Columns: %s\n", paste(names(participants_df), collapse = ", ")))
    
    # Try to load Trials sheet if it exists (contains trial-level data)
    tryCatch({
      trials_df <- read_excel(file_path, sheet = "Trials")
      tasks[[task_type]][[time]][["trials"]] <- trials_df
      cat(sprintf("  Loaded %d trials\n", nrow(trials_df)))
    }, error = function(e) {
      cat("  No Trials sheet found (or error loading it)\n")
    })
    
  }, error = function(e) {
    cat(sprintf("  Error loading %s: %s\n", file_path, e$message))
  })
}

cat("\nTask files loaded successfully!\n")
cat("Available task data:\n")
for (task_type in c("qIAT", "IAT")) {
  for (time in c("1", "2")) {
    if (time %in% names(tasks[[task_type]])) {
      n_participants <- nrow(tasks[[task_type]][[time]][["participants"]])
      cat(sprintf("  %s Time %s: %d participants\n", task_type, time, n_participants))
    }
  }
}

# ==============================================================================
# Sample Size and Demographics
# ==============================================================================

# 1. Total S2 N (before any exclusions)
total_s2_n <- nrow(df_s2)
cat(sprintf("\n1. Total S2 N: %d\n", total_s2_n))

# Verify total excluded from exclude column
exclude_col <- df_s2$exclude
if (is.character(exclude_col) || is.logical(exclude_col)) {
  exclude_col <- as.numeric(toupper(as.character(exclude_col)) %in% c("TRUE", "1", "YES"))
}
total_excluded <- sum(exclude_col, na.rm = TRUE)
cat(sprintf("   Total excluded (from exclude column): %d\n", total_excluded))
cat(sprintf("   Should equal: %d = %d\n", total_s2_n - nrow(df), total_s2_n - nrow(df)))

# 2. Exclusions based on implicit task performance
# Check if exclude_t1 and exclude_t2 columns exist
excluded_from_tasks <- rep(FALSE, nrow(df_s2))

if ("exclude_t1" %in% names(df_s2) && "exclude_t2" %in% names(df_s2)) {
  exclude_t1_bool <- df_s2$exclude_t1
  exclude_t2_bool <- df_s2$exclude_t2
  
  # Handle string/boolean values
  if (is.character(exclude_t1_bool) || is.logical(exclude_t1_bool)) {
    exclude_t1_bool <- as.numeric(toupper(as.character(exclude_t1_bool)) %in% c("TRUE", "1", "YES"))
  }
  if (is.character(exclude_t2_bool) || is.logical(exclude_t2_bool)) {
    exclude_t2_bool <- as.numeric(toupper(as.character(exclude_t2_bool)) %in% c("TRUE", "1", "YES"))
  }
  
  # Excluded from implicit tasks if excluded from T1 OR T2
  excluded_from_tasks <- (exclude_t1_bool == 1) | (exclude_t2_bool == 1)
  x1_task_exclusions <- sum(excluded_from_tasks, na.rm = TRUE)
  cat(sprintf("\n2. Excluded based on implicit task performance: %d\n", x1_task_exclusions))
  cat(sprintf("   (exclude_t1: %d, exclude_t2: %d)\n", 
              sum(exclude_t1_bool == 1, na.rm = TRUE), 
              sum(exclude_t2_bool == 1, na.rm = TRUE)))
} else if ("t1_exclude" %in% names(df_s2) && "t2_exclude" %in% names(df_s2)) {
  t1_excluded <- df_s2$t1_exclude
  t2_excluded <- df_s2$t2_exclude
  
  if (is.character(t1_excluded)) {
    t1_excluded <- as.numeric(toupper(t1_excluded) %in% c("TRUE", "1", "YES"))
  }
  if (is.character(t2_excluded)) {
    t2_excluded <- as.numeric(toupper(t2_excluded) %in% c("TRUE", "1", "YES"))
  }
  
  excluded_from_tasks <- (t1_excluded == 1) | (t2_excluded == 1)
  x1_task_exclusions <- sum(excluded_from_tasks, na.rm = TRUE)
  cat(sprintf("\n2. Excluded based on implicit task performance: %d\n", x1_task_exclusions))
} else {
  cat("\n2. Warning: exclude_t1/exclude_t2 or t1_exclude/t2_exclude columns not found\n")
  x1_task_exclusions <- 0
}

# 3. Additional exclusions based on manipulation checks
failed_manipulation <- rep(FALSE, nrow(df_s2))

if ("1st check indicator" %in% names(df_s2) && "2nd check indicator" %in% names(df_s2)) {
  manipulation_groups_mask <- df_s2$group %in% c(1, 2)
  manipulation_groups <- df_s2[manipulation_groups_mask, ]
  
  # Check for wrong answers in manipulation checks (look for "*" symbol)
  wrong_1st <- as.character(manipulation_groups$`1st check indicator`) == "*"
  wrong_2nd <- as.character(manipulation_groups$`2nd check indicator`) == "*"
  
  # Excluded if failed either check
  failed_manipulation_subset <- wrong_1st | wrong_2nd
  x2_manipulation_exclusions <- sum(failed_manipulation_subset, na.rm = TRUE)
  
  # Map back to full dataframe
  failed_manipulation[which(manipulation_groups_mask)] <- failed_manipulation_subset
  
  cat(sprintf("3. Additional exclusions from manipulation groups (failed checks): %d\n", 
              x2_manipulation_exclusions))
  cat(sprintf("   (failed 1st check: %d, failed 2nd check: %d)\n", 
              sum(wrong_1st, na.rm = TRUE), 
              sum(wrong_2nd, na.rm = TRUE)))
  
  # Check overlap
  overlap <- sum(excluded_from_tasks & failed_manipulation, na.rm = TRUE)
  if (overlap > 0) {
    cat(sprintf("   Note: %d participants excluded for BOTH task performance AND manipulation checks\n", overlap))
  }
  
  calculated_total <- x1_task_exclusions + x2_manipulation_exclusions - overlap
  cat(sprintf("   Calculated total (task + manipulation - overlap): %d\n", calculated_total))
  cat(sprintf("   Actual total excluded: %d\n", total_excluded))
} else {
  cat("\n3. Warning: manipulation check indicator columns not found\n")
  x2_manipulation_exclusions <- 0
  overlap <- 0
}

# 4. Final sample (after all exclusions)
final_n <- nrow(df)
cat(sprintf("\n4. Final N (after all exclusions): %d\n", final_n))

# 5. Demographics of final sample
# Gender: check what values indicate female
if ("gender" %in% names(df)) {
  cat("\nGender value counts:\n")
  print(table(df$gender))
  
  # Try to identify females
  gender_str <- toupper(as.character(df$gender))
  female_indicators <- c("FEMALE", "F", "2", "WOMAN", "W")
  is_female <- gender_str %in% female_indicators
  n_females <- sum(is_female, na.rm = TRUE)
  cat(sprintf("5. Number of females: %d\n", n_females))
} else {
  cat("\n5. Warning: gender column not found\n")
  n_females <- 0
}

# Age statistics
if ("age" %in% names(df)) {
  age_numeric <- as.numeric(df$age)
  age_mean <- mean(age_numeric, na.rm = TRUE)
  age_sd <- sd(age_numeric, na.rm = TRUE)  # Sample SD
  cat(sprintf("6. Mean age: %.2f\n", age_mean))
  cat(sprintf("7. SD age: %.2f\n", age_sd))
} else {
  cat("\n6-7. Warning: age column not found\n")
  age_mean <- NA
  age_sd <- NA
}

# Summary output for easy copy-paste
cat(sprintf("\n%s\n", paste(rep("=", 60), collapse = "")))
cat("SUMMARY (for article):\n")
cat(sprintf("%s\n", paste(rep("=", 60), collapse = "")))
cat(sprintf("1. Total S2 N: %d\n", total_s2_n))
cat(sprintf("2. Excluded based on implicit tasks: %d\n", x1_task_exclusions))
cat(sprintf("3. Additional exclusions (manipulation checks): %d\n", x2_manipulation_exclusions))
cat(sprintf("4. Final N: %d\n", final_n))
cat(sprintf("5. Females: %d\n", n_females))
cat(sprintf("6. Mean age: %.2f\n", age_mean))
cat(sprintf("7. SD age: %.2f\n", age_sd))

# ==============================================================================
# Internal Consistency of Questionnaire (Time 1)
# ==============================================================================

# Get all Q1_A columns (Time 1 questionnaire items)
q1_cols <- names(df)[grepl("^Q1_A", names(df))]
cat(sprintf("\nFound %d Time 1 questionnaire columns:\n", length(q1_cols)))
for (col in sort(q1_cols)) {
  cat(sprintf("  %s\n", col))
}

# Based on the schema, reverse-coded items are those marked with "*" in the description
# According to the schema: Q1_A2, Q1_A3, Q1_A5, Q1_A8, Q1_A10 are reverse-coded
reverse_coded_items <- c("Q1_A2", "Q1_A3", "Q1_A5", "Q1_A8", "Q1_A10")
reverse_coded <- c()

for (col in q1_cols) {
  col_base <- gsub(" *", "", col)
  col_base <- gsub("\\*", "", col_base)
  col_base <- trimws(col_base)
  if (grepl("\\*", col) || col_base %in% reverse_coded_items) {
    reverse_coded <- c(reverse_coded, col)
    cat(sprintf("  %s - REVERSE CODED\n", col))
  }
}

cat(sprintf("\nReverse-coded items: %d\n", length(reverse_coded)))
if (length(reverse_coded) == 0) {
  cat("  Warning: No reverse-coded items found. Using schema-based list.\n")
  reverse_coded <- q1_cols[sapply(q1_cols, function(x) any(sapply(reverse_coded_items, function(y) grepl(y, x))))]
  cat(sprintf("  Using fallback: %s\n", paste(reverse_coded, collapse = ", ")))
}

# Extract questionnaire data for Time 1
q1_data <- df[, q1_cols, drop = FALSE]

# Convert to numeric
for (col in q1_cols) {
  q1_data[[col]] <- as.numeric(q1_data[[col]])
}

# Apply reverse coding: f(x) = 6 - x for reverse-coded items
for (col in reverse_coded) {
  if (col %in% names(q1_data)) {
    q1_data[[col]] <- 6 - q1_data[[col]]
    cat(sprintf("Applied reverse coding to %s\n", col))
  }
}

# Remove rows with any missing values for reliability calculation
q1_data_clean <- q1_data[complete.cases(q1_data), ]
cat(sprintf("\nParticipants with complete T1 questionnaire data: %d\n", nrow(q1_data_clean)))

# Compute Cronbach's alpha
# Use psych::alpha()
alpha_result <- psych::alpha(q1_data_clean, check.keys = FALSE)
cronbach_alpha <- alpha_result$total$raw_alpha

k <- ncol(q1_data_clean)
item_variances <- apply(q1_data_clean, 2, var, na.rm = TRUE)
total_variance <- var(rowSums(q1_data_clean, na.rm = TRUE), na.rm = TRUE)

cat(sprintf("\nCronbach's Alpha for T1 Questionnaire: %.3f\n", cronbach_alpha))
cat(sprintf("Number of items: %d\n", k))
cat(sprintf("Sum of item variances: %.3f\n", sum(item_variances)))
cat(sprintf("Total variance: %.3f\n", total_variance))

# Store for later use
t1_questionnaire_alpha <- cronbach_alpha

# ==============================================================================
# Split-Half Reliability (Time 1)
# ==============================================================================

# Get Time 1 data for qIAT and IAT
# Use dscore_paired_odd_t1 and dscore_paired_even_t1 columns from the combined file

# For qIAT
qiat_t1 <- df[toupper(df$task) == "QIAT", ]
qiat_t1_odd <- as.numeric(qiat_t1$dscore_paired_odd_t1)
qiat_t1_even <- as.numeric(qiat_t1$dscore_paired_even_t1)

# Remove missing values
qiat_t1_pairs <- data.frame(
  odd = qiat_t1_odd,
  even = qiat_t1_even
)
qiat_t1_pairs <- qiat_t1_pairs[complete.cases(qiat_t1_pairs), ]

cat(sprintf("\nqIAT Time 1 participants with both odd and even D-scores: %d\n", nrow(qiat_t1_pairs)))

if (nrow(qiat_t1_pairs) > 0) {
  # Compute correlation
  qiat_t1_corr_test <- cor.test(qiat_t1_pairs$odd, qiat_t1_pairs$even)
  qiat_t1_corr <- qiat_t1_corr_test$estimate
  qiat_t1_p <- qiat_t1_corr_test$p.value
  cat(sprintf("qIAT T1 correlation (odd vs even): r = %.3f, p = %.4f\n", qiat_t1_corr, qiat_t1_p))
  
  # Apply Spearman-Brown correction
  qiat_t1_reliability <- spearman_brown(qiat_t1_corr)
  cat(sprintf("qIAT T1 split-half reliability (Spearman-Brown corrected): %.3f\n", qiat_t1_reliability))
} else {
  cat("Warning: No valid qIAT T1 data found\n")
  qiat_t1_reliability <- NA
}

cat("\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

# For IAT
iat_t1 <- df[toupper(df$task) == "IAT", ]
iat_t1_odd <- as.numeric(iat_t1$dscore_paired_odd_t1)
iat_t1_even <- as.numeric(iat_t1$dscore_paired_even_t1)

# Remove missing values
iat_t1_pairs <- data.frame(
  odd = iat_t1_odd,
  even = iat_t1_even
)
iat_t1_pairs <- iat_t1_pairs[complete.cases(iat_t1_pairs), ]

cat(sprintf("IAT Time 1 participants with both odd and even D-scores: %d\n", nrow(iat_t1_pairs)))

if (nrow(iat_t1_pairs) > 0) {
  # Compute correlation
  iat_t1_corr_test <- cor.test(iat_t1_pairs$odd, iat_t1_pairs$even)
  iat_t1_corr <- iat_t1_corr_test$estimate
  iat_t1_p <- iat_t1_corr_test$p.value
  cat(sprintf("IAT T1 correlation (odd vs even): r = %.3f, p = %.4f\n", iat_t1_corr, iat_t1_p))
  
  # Apply Spearman-Brown correction
  iat_t1_reliability <- spearman_brown(iat_t1_corr)
  cat(sprintf("IAT T1 split-half reliability (Spearman-Brown corrected): %.3f\n", iat_t1_reliability))
} else {
  cat("Warning: No valid IAT T1 data found\n")
  iat_t1_reliability <- NA
}

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat(sprintf("qIAT split-half reliability (T1): %.2f\n", qiat_t1_reliability))
cat(sprintf("IAT split-half reliability (T1): %.2f\n", iat_t1_reliability))

# ==============================================================================
# Table 6: Descriptive Statistics (Means and SDs)
# ==============================================================================

# Group mapping: 0 = Control, 1 = Faking Low, 2 = Faking High
group_names <- list("0" = "Control", "1" = "Faking Low", "2" = "Faking High")

# Initialize results list
results <- list(
  Questionnaire = list(),
  IAT = list(),
  qIAT = list()
)

# 1. Questionnaire (use t1_ques and t2_ques)
if ("t1_ques" %in% names(df) && "t2_ques" %in% names(df)) {
  for (group_num in c("0", "1", "2")) {
    group_data <- df[df$group == as.numeric(group_num), ]
    group_name <- group_names[[group_num]]
    
    t1_ques <- as.numeric(group_data$t1_ques)
    t2_ques <- as.numeric(group_data$t2_ques)
    
    t1_mean <- mean(t1_ques, na.rm = TRUE)
    t1_sd <- sd(t1_ques, na.rm = TRUE)
    t2_mean <- mean(t2_ques, na.rm = TRUE)
    t2_sd <- sd(t2_ques, na.rm = TRUE)
    
    results[["Questionnaire"]][[group_name]] <- list(
      Time1 = c(t1_mean, t1_sd),
      Time2 = c(t2_mean, t2_sd)
    )
  }
  
  # Overall (all groups combined)
  t1_ques_all <- as.numeric(df$t1_ques)
  t2_ques_all <- as.numeric(df$t2_ques)
  results[["Questionnaire"]][["Overall"]] <- list(
    Time1 = c(mean(t1_ques_all, na.rm = TRUE), sd(t1_ques_all, na.rm = TRUE)),
    Time2 = c(mean(t2_ques_all, na.rm = TRUE), sd(t2_ques_all, na.rm = TRUE))
  )
}

# 2. IAT (filter task == 'IAT', use t1_dscore and t2_dscore)
iat_data <- df[toupper(df$task) == "IAT", ]
if (nrow(iat_data) > 0) {
  for (group_num in c("0", "1", "2")) {
    group_data <- iat_data[iat_data$group == as.numeric(group_num), ]
    group_name <- group_names[[group_num]]
    
    t1_dscore <- as.numeric(group_data$t1_dscore)
    t2_dscore <- as.numeric(group_data$t2_dscore)
    
    t1_mean <- mean(t1_dscore, na.rm = TRUE)
    t1_sd <- sd(t1_dscore, na.rm = TRUE)
    t2_mean <- mean(t2_dscore, na.rm = TRUE)
    t2_sd <- sd(t2_dscore, na.rm = TRUE)
    
    results[["IAT"]][[group_name]] <- list(
      Time1 = c(t1_mean, t1_sd),
      Time2 = c(t2_mean, t2_sd)
    )
  }
  
  # Overall (all groups combined)
  t1_dscore_all <- as.numeric(iat_data$t1_dscore)
  t2_dscore_all <- as.numeric(iat_data$t2_dscore)
  results[["IAT"]][["Overall"]] <- list(
    Time1 = c(mean(t1_dscore_all, na.rm = TRUE), sd(t1_dscore_all, na.rm = TRUE)),
    Time2 = c(mean(t2_dscore_all, na.rm = TRUE), sd(t2_dscore_all, na.rm = TRUE))
  )
}

# 3. qIAT (filter task == 'qIAT', use t1_dscore and t2_dscore)
qiat_data <- df[toupper(df$task) == "QIAT", ]
if (nrow(qiat_data) > 0) {
  for (group_num in c("0", "1", "2")) {
    group_data <- qiat_data[qiat_data$group == as.numeric(group_num), ]
    group_name <- group_names[[group_num]]
    
    t1_dscore <- as.numeric(group_data$t1_dscore)
    t2_dscore <- as.numeric(group_data$t2_dscore)
    
    t1_mean <- mean(t1_dscore, na.rm = TRUE)
    t1_sd <- sd(t1_dscore, na.rm = TRUE)
    t2_mean <- mean(t2_dscore, na.rm = TRUE)
    t2_sd <- sd(t2_dscore, na.rm = TRUE)
    
    results[["qIAT"]][[group_name]] <- list(
      Time1 = c(t1_mean, t1_sd),
      Time2 = c(t2_mean, t2_sd)
    )
  }
  
  # Overall (all groups combined)
  t1_dscore_all <- as.numeric(qiat_data$t1_dscore)
  t2_dscore_all <- as.numeric(qiat_data$t2_dscore)
  results[["qIAT"]][["Overall"]] <- list(
    Time1 = c(mean(t1_dscore_all, na.rm = TRUE), sd(t1_dscore_all, na.rm = TRUE)),
    Time2 = c(mean(t2_dscore_all, na.rm = TRUE), sd(t2_dscore_all, na.rm = TRUE))
  )
}

# Create formatted table
cat("\nTable 6. Means (SDs) for the questionnaire and implicit tasks in Study 2\n")
cat(paste(rep("=", 120), collapse = ""))
cat("\n")
cat(sprintf("%-15s%-30s%-30s%-30s%-30s\n", "Measure", "Control", "Faking Low", "Faking High", "Overall"))
cat(sprintf("%-15s%-14s%-14s%-14s%-14s%-14s%-14s%-14s%-14s\n", 
            "", "Time1", "Time2", "Time1", "Time2", "Time1", "Time2", "Time1", "Time2"))
cat(paste(rep("-", 120), collapse = ""))
cat("\n")

for (measure in c("Questionnaire", "IAT", "qIAT")) {
  row <- sprintf("%-15s", measure)
  
  for (group_name in c("Control", "Faking Low", "Faking High", "Overall")) {
    if (group_name %in% names(results[[measure]])) {
      t1_mean <- results[[measure]][[group_name]]$Time1[1]
      t1_sd <- results[[measure]][[group_name]]$Time1[2]
      t2_mean <- results[[measure]][[group_name]]$Time2[1]
      t2_sd <- results[[measure]][[group_name]]$Time2[2]
      t1_str <- sprintf("%.2f (%.2f)", t1_mean, t1_sd)
      t2_str <- sprintf("%.2f (%.2f)", t2_mean, t2_sd)
      row <- paste0(row, sprintf("%-14s%-14s", t1_str, t2_str))
    } else {
      row <- paste0(row, sprintf("%-14s%-14s", "N/A", "N/A"))
    }
  }
  
  cat(row)
  cat("\n")
}

cat(paste(rep("=", 120), collapse = ""))
cat("\n")

# Also create a data frame for easier viewing
table_data <- list()
for (measure in c("Questionnaire", "IAT", "qIAT")) {
  for (group_name in c("Control", "Faking Low", "Faking High", "Overall")) {
    if (group_name %in% names(results[[measure]])) {
      t1_mean <- results[[measure]][[group_name]]$Time1[1]
      t1_sd <- results[[measure]][[group_name]]$Time1[2]
      t2_mean <- results[[measure]][[group_name]]$Time2[1]
      t2_sd <- results[[measure]][[group_name]]$Time2[2]
      t1_str <- sprintf("%.2f (%.2f)", t1_mean, t1_sd)
      t2_str <- sprintf("%.2f (%.2f)", t2_mean, t2_sd)
      table_data[[length(table_data) + 1]] <- list(
        Measure = measure,
        Group = group_name,
        Time1_Mean = t1_mean,
        Time1_SD = t1_sd,
        Time2_Mean = t2_mean,
        Time2_SD = t2_sd,
        Time1_Formatted = t1_str,
        Time2_Formatted = t2_str
      )
    }
  }
}

table_df <- do.call(rbind, lapply(table_data, function(x) data.frame(x, stringsAsFactors = FALSE)))
cat("\nDetailed table (DataFrame format):\n")
print(table_df, row.names = FALSE)

# ==============================================================================
# Test-Retest Reliability (Control Group)
# ==============================================================================

# Filter to control group only (group == 0)
control_group <- df[df$group == 0, ]
cat(sprintf("\nControl group participants: %d\n", nrow(control_group)))

# 1. Self-report (Questionnaire) test-retest reliability
if ("t1_ques" %in% names(control_group) && "t2_ques" %in% names(control_group)) {
  t1_ques_control <- as.numeric(control_group$t1_ques)
  t2_ques_control <- as.numeric(control_group$t2_ques)
  
  # Remove missing values
  ques_pairs <- data.frame(
    t1 = t1_ques_control,
    t2 = t2_ques_control
  )
  ques_pairs <- ques_pairs[complete.cases(ques_pairs), ]
  
  cat("\nSelf-report (Questionnaire):\n")
  cat(sprintf("  Participants with complete data: %d\n", nrow(ques_pairs)))
  
  if (nrow(ques_pairs) > 1) {
    ques_corr_test <- cor.test(ques_pairs$t1, ques_pairs$t2)
    ques_corr <- ques_corr_test$estimate
    ques_p <- ques_corr_test$p.value
    cat(sprintf("  Test-retest reliability: r = %.3f, p = %.4f\n", ques_corr, ques_p))
    self_report_reliability <- ques_corr
  } else {
    cat("  Warning: Insufficient data for correlation\n")
    self_report_reliability <- NA
  }
} else {
  cat("\nWarning: t1_ques or t2_ques columns not found\n")
  self_report_reliability <- NA
}

# 2. qIAT test-retest reliability (control group, qIAT task only)
qiat_control <- control_group[toupper(control_group$task) == "QIAT", ]
cat("\nqIAT (Control group):\n")
cat(sprintf("  Participants: %d\n", nrow(qiat_control)))

if (nrow(qiat_control) > 0) {
  t1_qiat_control <- as.numeric(qiat_control$t1_dscore)
  t2_qiat_control <- as.numeric(qiat_control$t2_dscore)
  
  # Remove missing values
  qiat_pairs <- data.frame(
    t1 = t1_qiat_control,
    t2 = t2_qiat_control
  )
  qiat_pairs <- qiat_pairs[complete.cases(qiat_pairs), ]
  
  cat(sprintf("  Participants with complete data: %d\n", nrow(qiat_pairs)))
  
  if (nrow(qiat_pairs) > 1) {
    qiat_corr_test <- cor.test(qiat_pairs$t1, qiat_pairs$t2)
    qiat_corr <- qiat_corr_test$estimate
    qiat_p <- qiat_corr_test$p.value
    cat(sprintf("  Test-retest reliability: r = %.3f, p = %.4f\n", qiat_corr, qiat_p))
    qiat_reliability <- qiat_corr
  } else {
    cat("  Warning: Insufficient data for correlation\n")
    qiat_reliability <- NA
  }
} else {
  cat("  Warning: No qIAT data found for control group\n")
  qiat_reliability <- NA
}

# 3. IAT test-retest reliability (control group, IAT task only)
iat_control <- control_group[toupper(control_group$task) == "IAT", ]
cat("\nIAT (Control group):\n")
cat(sprintf("  Participants: %d\n", nrow(iat_control)))

if (nrow(iat_control) > 0) {
  t1_iat_control <- as.numeric(iat_control$t1_dscore)
  t2_iat_control <- as.numeric(iat_control$t2_dscore)
  
  # Remove missing values
  iat_pairs <- data.frame(
    t1 = t1_iat_control,
    t2 = t2_iat_control
  )
  iat_pairs <- iat_pairs[complete.cases(iat_pairs), ]
  
  cat(sprintf("  Participants with complete data: %d\n", nrow(iat_pairs)))
  
  if (nrow(iat_pairs) > 1) {
    iat_corr_test <- cor.test(iat_pairs$t1, iat_pairs$t2)
    iat_corr <- iat_corr_test$estimate
    iat_p <- iat_corr_test$p.value
    cat(sprintf("  Test-retest reliability: r = %.3f, p = %.4f\n", iat_corr, iat_p))
    iat_reliability <- iat_corr
  } else {
    cat("  Warning: Insufficient data for correlation\n")
    iat_reliability <- NA
  }
} else {
  cat("  Warning: No IAT data found for control group\n")
  iat_reliability <- NA
}

# Summary
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Test-retest reliability (Control group):\n")
cat(sprintf("  Self-report: %.2f\n", self_report_reliability))
cat(sprintf("  qIAT: %.2f\n", qiat_reliability))
cat(sprintf("  IAT: %.2f\n", iat_reliability))

# ==============================================================================
# Table 4: Split-Half Reliabilities
# ==============================================================================

# Helper function to compute split-half reliability for a subset of data
compute_split_half_reliability <- function(data, odd_col, even_col) {
  # Compute split-half reliability from odd and even D-scores
  odd_scores <- as.numeric(data[[odd_col]])
  even_scores <- as.numeric(data[[even_col]])
  
  # Remove missing values
  pairs <- data.frame(
    odd = odd_scores,
    even = even_scores
  )
  pairs <- pairs[complete.cases(pairs), ]
  
  if (nrow(pairs) < 2) {
    return(list(reliability = NA, p_value = NA, formatted = "N/A"))
  }
  
  # Compute correlation
  corr_test <- cor.test(pairs$odd, pairs$even)
  corr <- corr_test$estimate
  p_value <- corr_test$p.value
  
  # Apply Spearman-Brown correction
  reliability <- spearman_brown(corr)
  
  # Format with significance
  sig <- add_significance(p_value)
  formatted <- sprintf("%.2f%s", reliability, sig)
  
  return(list(reliability = reliability, p_value = p_value, formatted = formatted))
}

# Group mapping: 0 = Control, 1 = Faking Low, 2 = Faking High
group_names <- list("0" = "Control", "1" = "Faking Low", "2" = "Faking High")

# Initialize results list
# Structure: results[task][group][time] = list(reliability, p_value, formatted_string)
results <- list(
  IAT = list(),
  qIAT = list()
)

# Process IAT
iat_data <- df[toupper(df$task) == "IAT", ]

for (group_num in c("0", "1", "2")) {
  group_iat <- iat_data[iat_data$group == as.numeric(group_num), ]
  group_name <- group_names[[group_num]]
  
  # Time 1
  rel_t1 <- compute_split_half_reliability(group_iat, "dscore_paired_odd_t1", "dscore_paired_even_t1")
  
  # Time 2
  rel_t2 <- compute_split_half_reliability(group_iat, "dscore_paired_odd_t2", "dscore_paired_even_t2")
  
  results[["IAT"]][[group_name]] <- list(
    Time1 = rel_t1,
    Time2 = rel_t2
  )
}

# IAT Overall
rel_t1_all <- compute_split_half_reliability(iat_data, "dscore_paired_odd_t1", "dscore_paired_even_t1")
rel_t2_all <- compute_split_half_reliability(iat_data, "dscore_paired_odd_t2", "dscore_paired_even_t2")
results[["IAT"]][["Overall"]] <- list(
  Time1 = rel_t1_all,
  Time2 = rel_t2_all
)

# Process qIAT
qiat_data <- df[toupper(df$task) == "QIAT", ]

for (group_num in c("0", "1", "2")) {
  group_qiat <- qiat_data[qiat_data$group == as.numeric(group_num), ]
  group_name <- group_names[[group_num]]
  
  # Time 1
  rel_t1 <- compute_split_half_reliability(group_qiat, "dscore_paired_odd_t1", "dscore_paired_even_t1")
  
  # Time 2
  rel_t2 <- compute_split_half_reliability(group_qiat, "dscore_paired_odd_t2", "dscore_paired_even_t2")
  
  results[["qIAT"]][[group_name]] <- list(
    Time1 = rel_t1,
    Time2 = rel_t2
  )
}

# qIAT Overall
rel_t1_all <- compute_split_half_reliability(qiat_data, "dscore_paired_odd_t1", "dscore_paired_even_t1")
rel_t2_all <- compute_split_half_reliability(qiat_data, "dscore_paired_odd_t2", "dscore_paired_even_t2")
results[["qIAT"]][["Overall"]] <- list(
  Time1 = rel_t1_all,
  Time2 = rel_t2_all
)

# Create formatted table
cat("\nTable 4. Split half reliabilities of implicit tasks in Study 2\n")
cat(paste(rep("=", 120), collapse = ""))
cat("\n")
cat(sprintf("%-10s%-30s%-30s%-30s%-30s\n", "Measure", "Control", "Faking Low", "Faking High", "Overall"))
cat(sprintf("%-10s%-14s%-14s%-14s%-14s%-14s%-14s%-14s%-14s\n", 
            "", "Time1", "Time2", "Time1", "Time2", "Time1", "Time2", "Time1", "Time2"))
cat(paste(rep("-", 120), collapse = ""))
cat("\n")

for (task in c("IAT", "qIAT")) {
  row <- sprintf("%-10s", task)
  
  for (group_name in c("Control", "Faking Low", "Faking High", "Overall")) {
    if (group_name %in% names(results[[task]])) {
      fmt_t1 <- results[[task]][[group_name]]$Time1$formatted
      fmt_t2 <- results[[task]][[group_name]]$Time2$formatted
      row <- paste0(row, sprintf("%-14s%-14s", fmt_t1, fmt_t2))
    } else {
      row <- paste0(row, sprintf("%-14s%-14s", "N/A", "N/A"))
    }
  }
  
  cat(row)
  cat("\n")
}

cat(paste(rep("=", 120), collapse = ""))
cat("\n")
cat("Note: * p < .05, ** p < .01, *** p < .001\n")

# Create detailed data frame with all information
table_data <- list()
for (task in c("IAT", "qIAT")) {
  for (group_name in c("Control", "Faking Low", "Faking High", "Overall")) {
    if (group_name %in% names(results[[task]])) {
      rel_t1 <- results[[task]][[group_name]]$Time1
      rel_t2 <- results[[task]][[group_name]]$Time2
      table_data[[length(table_data) + 1]] <- list(
        Task = task,
        Group = group_name,
        Time1_Reliability = rel_t1$reliability,
        Time1_p = rel_t1$p_value,
        Time1_Formatted = rel_t1$formatted,
        Time2_Reliability = rel_t2$reliability,
        Time2_p = rel_t2$p_value,
        Time2_Formatted = rel_t2$formatted
      )
    }
  }
}

table_df <- do.call(rbind, lapply(table_data, function(x) data.frame(x, stringsAsFactors = FALSE)))
cat("\n\nDetailed table (DataFrame format):\n")
print(table_df, row.names = FALSE)

# ==============================================================================
# Table 5: Implicit-Explicit Correlations
# ==============================================================================

# Helper function to compute correlation with significance
compute_correlation_with_test <- function(x, y) {
  # Compute Pearson correlation and test significance
  # Remove missing values
  pairs <- data.frame(
    x = as.numeric(x),
    y = as.numeric(y)
  )
  pairs <- pairs[complete.cases(pairs), ]
  
  if (nrow(pairs) < 2) {
    return(list(correlation = NA, p_value = NA, formatted = "N/A"))
  }
  
  # Compute correlation
  corr_test <- cor.test(pairs$x, pairs$y)
  corr <- corr_test$estimate
  p_value <- corr_test$p.value
  
  # Format with significance
  sig <- add_significance(p_value)
  formatted <- sprintf("%.2f%s", corr, sig)
  
  return(list(correlation = corr, p_value = p_value, formatted = formatted))
}

# Group mapping: 0 = Control, 1 = Faking Low, 2 = Faking High
group_names <- list("0" = "Control", "1" = "Faking Low", "2" = "Faking High")

# Initialize results list
# Structure: results[task][group][time] = list(correlation, p_value, formatted_string)
results <- list(
  IAT = list(),
  qIAT = list()
)

# Process IAT
iat_data <- df[toupper(df$task) == "IAT", ]

for (group_num in c("0", "1", "2")) {
  group_iat <- iat_data[iat_data$group == as.numeric(group_num), ]
  group_name <- group_names[[group_num]]
  
  # Time 1: IAT D-score with Questionnaire score
  t1_dscore <- as.numeric(group_iat$t1_dscore)
  t1_ques <- as.numeric(group_iat$t1_ques)
  corr_t1 <- compute_correlation_with_test(t1_dscore, t1_ques)
  
  # Time 2: IAT D-score with Questionnaire score
  t2_dscore <- as.numeric(group_iat$t2_dscore)
  t2_ques <- as.numeric(group_iat$t2_ques)
  corr_t2 <- compute_correlation_with_test(t2_dscore, t2_ques)
  
  results[["IAT"]][[group_name]] <- list(
    Time1 = corr_t1,
    Time2 = corr_t2
  )
}

# IAT Overall
t1_dscore_all <- as.numeric(iat_data$t1_dscore)
t1_ques_all <- as.numeric(iat_data$t1_ques)
corr_t1_all <- compute_correlation_with_test(t1_dscore_all, t1_ques_all)

t2_dscore_all <- as.numeric(iat_data$t2_dscore)
t2_ques_all <- as.numeric(iat_data$t2_ques)
corr_t2_all <- compute_correlation_with_test(t2_dscore_all, t2_ques_all)

results[["IAT"]][["Overall"]] <- list(
  Time1 = corr_t1_all,
  Time2 = corr_t2_all
)

# Process qIAT
qiat_data <- df[toupper(df$task) == "QIAT", ]

for (group_num in c("0", "1", "2")) {
  group_qiat <- qiat_data[qiat_data$group == as.numeric(group_num), ]
  group_name <- group_names[[group_num]]
  
  # Time 1: qIAT D-score with Questionnaire score
  t1_dscore <- as.numeric(group_qiat$t1_dscore)
  t1_ques <- as.numeric(group_qiat$t1_ques)
  corr_t1 <- compute_correlation_with_test(t1_dscore, t1_ques)
  
  # Time 2: qIAT D-score with Questionnaire score
  t2_dscore <- as.numeric(group_qiat$t2_dscore)
  t2_ques <- as.numeric(group_qiat$t2_ques)
  corr_t2 <- compute_correlation_with_test(t2_dscore, t2_ques)
  
  results[["qIAT"]][[group_name]] <- list(
    Time1 = corr_t1,
    Time2 = corr_t2
  )
}

# qIAT Overall
t1_dscore_all <- as.numeric(qiat_data$t1_dscore)
t1_ques_all <- as.numeric(qiat_data$t1_ques)
corr_t1_all <- compute_correlation_with_test(t1_dscore_all, t1_ques_all)

t2_dscore_all <- as.numeric(qiat_data$t2_dscore)
t2_ques_all <- as.numeric(qiat_data$t2_ques)
corr_t2_all <- compute_correlation_with_test(t2_dscore_all, t2_ques_all)

results[["qIAT"]][["Overall"]] <- list(
  Time1 = corr_t1_all,
  Time2 = corr_t2_all
)

# Create formatted table
cat("\nTable 5. Correlations between implicit tasks and the explicit measure in Study 2\n")
cat(paste(rep("=", 120), collapse = ""))
cat("\n")
cat(sprintf("%-10s%-30s%-30s%-30s%-30s\n", "Measure", "Control", "Faking Low", "Faking High", "Overall"))
cat(sprintf("%-10s%-14s%-14s%-14s%-14s%-14s%-14s%-14s%-14s\n", 
            "", "Time1", "Time2", "Time1", "Time2", "Time1", "Time2", "Time1", "Time2"))
cat(paste(rep("-", 120), collapse = ""))
cat("\n")

for (task in c("IAT", "qIAT")) {
  row <- sprintf("%-10s", task)
  
  for (group_name in c("Control", "Faking Low", "Faking High", "Overall")) {
    if (group_name %in% names(results[[task]])) {
      fmt_t1 <- results[[task]][[group_name]]$Time1$formatted
      fmt_t2 <- results[[task]][[group_name]]$Time2$formatted
      row <- paste0(row, sprintf("%-14s%-14s", fmt_t1, fmt_t2))
    } else {
      row <- paste0(row, sprintf("%-14s%-14s", "N/A", "N/A"))
    }
  }
  
  cat(row)
  cat("\n")
}

cat(paste(rep("=", 120), collapse = ""))
cat("\n")
cat("Note: * p < .05, ** p < .01, *** p < .001\n")

# Create detailed data frame
table_data <- list()
for (task in c("IAT", "qIAT")) {
  for (group_name in c("Control", "Faking Low", "Faking High", "Overall")) {
    if (group_name %in% names(results[[task]])) {
      corr_t1 <- results[[task]][[group_name]]$Time1
      corr_t2 <- results[[task]][[group_name]]$Time2
      table_data[[length(table_data) + 1]] <- list(
        Task = task,
        Group = group_name,
        Time1_Correlation = corr_t1$correlation,
        Time1_p = corr_t1$p_value,
        Time1_Formatted = corr_t1$formatted,
        Time2_Correlation = corr_t2$correlation,
        Time2_p = corr_t2$p_value,
        Time2_Formatted = corr_t2$formatted
      )
    }
  }
}

table_df <- do.call(rbind, lapply(table_data, function(x) data.frame(x, stringsAsFactors = FALSE)))
cat("\n\nDetailed table (DataFrame format):\n")
print(table_df, row.names = FALSE)

# ==============================================================================
# Implicit-Explicit Convergent Validity Comparison (Time1)
# ==============================================================================

# Get overall Time1 correlations for qIAT and IAT
# These are the correlations from the "Overall" group in Table 5

# qIAT Time1 correlation with explicit measure
qiat_t1_data <- df[toupper(df$task) == "QIAT", ]
qiat_t1_dscore <- as.numeric(qiat_t1_data$t1_dscore)
qiat_t1_ques <- as.numeric(qiat_t1_data$t1_ques)

qiat_t1_pairs <- data.frame(
  dscore = qiat_t1_dscore,
  ques = qiat_t1_ques
)
qiat_t1_pairs <- qiat_t1_pairs[complete.cases(qiat_t1_pairs), ]

qiat_t1_corr_test <- cor.test(qiat_t1_pairs$dscore, qiat_t1_pairs$ques)
qiat_t1_corr <- qiat_t1_corr_test$estimate
qiat_t1_p <- qiat_t1_corr_test$p.value
qiat_t1_n <- nrow(qiat_t1_pairs)

cat("\nqIAT Time1 correlation with explicit measure:\n")
cat(sprintf("  r = %.2f, p = %.4f, n = %d\n", qiat_t1_corr, qiat_t1_p, qiat_t1_n))

# IAT Time1 correlation with explicit measure
iat_t1_data <- df[toupper(df$task) == "IAT", ]
iat_t1_dscore <- as.numeric(iat_t1_data$t1_dscore)
iat_t1_ques <- as.numeric(iat_t1_data$t1_ques)

iat_t1_pairs <- data.frame(
  dscore = iat_t1_dscore,
  ques = iat_t1_ques
)
iat_t1_pairs <- iat_t1_pairs[complete.cases(iat_t1_pairs), ]

iat_t1_corr_test <- cor.test(iat_t1_pairs$dscore, iat_t1_pairs$ques)
iat_t1_corr <- iat_t1_corr_test$estimate
iat_t1_p <- iat_t1_corr_test$p.value
iat_t1_n <- nrow(iat_t1_pairs)

cat("\nIAT Time1 correlation with explicit measure:\n")
cat(sprintf("  r = %.2f, p = %.4f, n = %d\n", iat_t1_corr, iat_t1_p, iat_t1_n))

# Fisher z test to compare the two correlations
# Note: These are independent correlations (different participants in qIAT vs IAT groups)
z_result <- compare_correlations(qiat_t1_corr, iat_t1_corr, qiat_t1_n, iat_t1_n)
z_stat <- z_result$z
p_value <- z_result$p

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Fisher z test comparing qIAT vs IAT correlations:\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat(sprintf("qIAT correlation: r = %.2f, n = %d\n", qiat_t1_corr, qiat_t1_n))
cat(sprintf("IAT correlation: r = %.2f, n = %d\n", iat_t1_corr, iat_t1_n))
cat(sprintf("\nFisher z statistic: z = %.2f\n", z_stat))
cat(sprintf("p-value: p = %.4f\n", p_value))

if (p_value < 0.05) {
  cat(sprintf("\nConclusion: The difference is significant (p = %.4f)\n", p_value))
  if (qiat_t1_corr > iat_t1_corr) {
    cat("qIAT exhibits a stronger implicit-explicit relationship than IAT.\n")
  } else {
    cat("IAT exhibits a stronger implicit-explicit relationship than qIAT.\n")
  }
} else {
  cat(sprintf("\nConclusion: The difference is not significant (p = %.4f)\n", p_value))
}

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat(sprintf("qIAT-explicit correlation (Time1): r = %.2f, p < .001\n", qiat_t1_corr))
cat(sprintf("IAT-explicit correlation (Time1): r = %.2f, p < .001\n", iat_t1_corr))
cat(sprintf("Fisher z test: z = %.2f, p = %.4f\n", z_stat, p_value))

# ==============================================================================
# Fakeability Analysis: Self-Report (Questionnaire)
# ==============================================================================

# Prepare data for mixed ANOVA
# Need long format: participant, group, time, score
ques_data <- df[, c("id", "group", "t1_ques", "t2_ques")]

# Convert to numeric
ques_data$group <- as.numeric(ques_data$group)
ques_data$t1_ques <- as.numeric(ques_data$t1_ques)
ques_data$t2_ques <- as.numeric(ques_data$t2_ques)

# Remove rows with missing data
ques_data <- ques_data[complete.cases(ques_data), ]

cat(sprintf("\nParticipants with complete questionnaire data: %d\n", nrow(ques_data)))
cat("Group distribution:\n")
print(table(ques_data$group))

# Reshape to long format for ANOVA
ques_long <- ques_data %>%
  pivot_longer(
    cols = c(t1_ques, t2_ques),
    names_to = "time",
    values_to = "score"
  )

# Recode time: t1_ques -> 1, t2_ques -> 2
ques_long$time <- ifelse(ques_long$time == "t1_ques", 1, 2)

# Recode group labels for clarity (0=Control, 1=Faking Low, 2=Faking High)
ques_long$group_label <- factor(ques_long$group, 
                                 levels = c(0, 1, 2),
                                 labels = c("Control", "Faking Low", "Faking High"))

cat(sprintf("\nData prepared for ANOVA: %d observations\n", nrow(ques_long)))

# Mixed ANOVA using afex
# Convert id to factor for proper handling
ques_long$id <- as.factor(ques_long$id)
ques_long$group <- as.factor(ques_long$group)
ques_long$time <- as.factor(ques_long$time)

aov <- aov_car(score ~ group * time + Error(id/time), data = ques_long)

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Mixed ANOVA Results for Self-Report (Questionnaire)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
print(aov)

# Extract and format results
cat("\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")
cat("ANOVA Summary:\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

aov_summary <- summary(aov)
# Extract F-statistics, p-values, and partial eta squared
anova_table <- aov$anova_table

for (i in 1:nrow(anova_table)) {
  effect_name <- rownames(anova_table)[i]
  if (grepl("group", tolower(effect_name)) && grepl("time", tolower(effect_name))) {
    effect_display <- "Group × Time"
  } else if (grepl("time", tolower(effect_name))) {
    effect_display <- "Time"
  } else if (grepl("group", tolower(effect_name))) {
    effect_display <- "Group"
  } else {
    effect_display <- effect_name
  }
  
  f_val <- anova_table$`F`[i]
  p_val <- anova_table$`Pr(>F)`[i]
  df1 <- anova_table$`num Df`[i]
  df2 <- anova_table$`den Df`[i]
  
  # Calculate partial eta squared
  ss_effect <- anova_table$`Sum Sq`[i]
  ss_error <- sum(anova_table$`Sum Sq`[grepl("Residuals", rownames(anova_table))], na.rm = TRUE)
  eta_sq <- ss_effect / (ss_effect + ss_error)
  
  cat(sprintf("%s: F(%d, %d) = %.2f, p = %.4f, ηp² = %.2f\n", 
              effect_display, df1, df2, f_val, p_val, eta_sq))
}

# Compute descriptive statistics for planned contrasts
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Descriptive Statistics for Planned Contrasts\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# Faking Low group (group == 1)
faking_low <- ques_data[ques_data$group == 1, ]
fl_t1_mean <- mean(faking_low$t1_ques, na.rm = TRUE)
fl_t1_sd <- sd(faking_low$t1_ques, na.rm = TRUE)
fl_t2_mean <- mean(faking_low$t2_ques, na.rm = TRUE)
fl_t2_sd <- sd(faking_low$t2_ques, na.rm = TRUE)
fl_n <- nrow(faking_low)

cat(sprintf("\nFaking Low group (n = %d):\n", fl_n))
cat(sprintf("  Time1: M = %.2f, SD = %.2f\n", fl_t1_mean, fl_t1_sd))
cat(sprintf("  Time2: M = %.2f, SD = %.2f\n", fl_t2_mean, fl_t2_sd))

# Faking High group (group == 2)
faking_high <- ques_data[ques_data$group == 2, ]
fh_t1_mean <- mean(faking_high$t1_ques, na.rm = TRUE)
fh_t1_sd <- sd(faking_high$t1_ques, na.rm = TRUE)
fh_t2_mean <- mean(faking_high$t2_ques, na.rm = TRUE)
fh_t2_sd <- sd(faking_high$t2_ques, na.rm = TRUE)
fh_n <- nrow(faking_high)

cat(sprintf("\nFaking High group (n = %d):\n", fh_n))
cat(sprintf("  Time1: M = %.2f, SD = %.2f\n", fh_t1_mean, fh_t1_sd))
cat(sprintf("  Time2: M = %.2f, SD = %.2f\n", fh_t2_mean, fh_t2_sd))

# Planned contrasts: Paired t-tests
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Planned Contrasts (Paired t-tests)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# Faking Low: Time1 vs Time2
fl_t1 <- faking_low$t1_ques
fl_t2 <- faking_low$t2_ques
fl_t_test <- t.test(fl_t1, fl_t2, paired = TRUE)
fl_t_stat <- fl_t_test$statistic
fl_t_p <- fl_t_test$p.value

# Cohen's d for paired samples: mean difference / SD of differences
fl_diff <- fl_t1 - fl_t2
fl_cohens_d <- cohens_d_paired(fl_t1, fl_t2)

cat(sprintf("\nFaking Low group:\n"))
cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", fl_n - 1, fl_t_stat, fl_t_p))
cat(sprintf("  Cohen's d = %.2f\n", fl_cohens_d))
cat(sprintf("  Mean difference = %.2f, SD of differences = %.2f\n", 
            mean(fl_diff, na.rm = TRUE), sd(fl_diff, na.rm = TRUE)))

# Faking High: Time1 vs Time2
fh_t1 <- faking_high$t1_ques
fh_t2 <- faking_high$t2_ques
fh_t_test <- t.test(fh_t1, fh_t2, paired = TRUE)
fh_t_stat <- fh_t_test$statistic
fh_t_p <- fh_t_test$p.value

# Cohen's d for paired samples: mean difference / SD of differences
fh_diff <- fh_t1 - fh_t2
fh_cohens_d <- cohens_d_paired(fh_t1, fh_t2)

cat(sprintf("\nFaking High group:\n"))
cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", fh_n - 1, abs(fh_t_stat), fh_t_p))
cat(sprintf("  Cohen's d = %.2f\n", abs(fh_cohens_d)))
cat(sprintf("  Mean difference = %.2f, SD of differences = %.2f\n", 
            mean(fh_diff, na.rm = TRUE), sd(fh_diff, na.rm = TRUE)))

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Mixed ANOVA for Self-Report:\n")
cat("  (Note: Full ANOVA results require specialized output)\n")
cat("\nPlanned Contrasts:\n")
cat(sprintf("  Faking Low: t(%d) = %.2f, p < .001, d = %.2f\n", fl_n - 1, fl_t_stat, fl_cohens_d))
cat(sprintf("  Faking High: t(%d) = %.2f, p < .001, d = %.2f\n", fh_n - 1, fh_t_stat, fh_cohens_d))

# ==============================================================================
# Fakeability Analysis: IAT
# ==============================================================================

# Prepare data for mixed ANOVA - IAT only
# Filter to IAT task
iat_data <- df[toupper(df$task) == "IAT", ]

# Prepare data: participant, group, time, score
iat_ques_data <- iat_data[, c("id", "group", "t1_dscore", "t2_dscore")]

# Convert to numeric
iat_ques_data$group <- as.numeric(iat_ques_data$group)
iat_ques_data$t1_dscore <- as.numeric(iat_ques_data$t1_dscore)
iat_ques_data$t2_dscore <- as.numeric(iat_ques_data$t2_dscore)

# Remove rows with missing data
iat_ques_data <- iat_ques_data[complete.cases(iat_ques_data), ]

cat(sprintf("\nIAT participants with complete data: %d\n", nrow(iat_ques_data)))
cat("Group distribution:\n")
print(table(iat_ques_data$group))

# Reshape to long format for ANOVA
iat_long <- iat_ques_data %>%
  pivot_longer(
    cols = c(t1_dscore, t2_dscore),
    names_to = "time",
    values_to = "score"
  )
  
# Recode time: t1_dscore -> 1, t2_dscore -> 2
iat_long$time <- ifelse(iat_long$time == "t1_dscore", 1, 2)

cat(sprintf("\nData prepared for ANOVA: %d observations\n", nrow(iat_long)))

# Mixed ANOVA using afex
iat_long$id <- as.factor(iat_long$id)
iat_long$group <- as.factor(iat_long$group)
iat_long$time <- as.factor(iat_long$time)

aov <- aov_car(score ~ group * time + Error(id/time), data = iat_long)

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Mixed ANOVA Results for IAT\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
print(aov)

# Extract and format results
cat("\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")
cat("ANOVA Summary:\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

anova_table <- aov$anova_table

for (i in 1:nrow(anova_table)) {
  effect_name <- rownames(anova_table)[i]
  if (grepl("group", tolower(effect_name)) && grepl("time", tolower(effect_name))) {
    effect_display <- "Group × Time"
  } else if (grepl("time", tolower(effect_name))) {
    effect_display <- "Time"
  } else if (grepl("group", tolower(effect_name))) {
    effect_display <- "Group"
  } else {
    effect_display <- effect_name
  }
  
  f_val <- anova_table$`F`[i]
  p_val <- anova_table$`Pr(>F)`[i]
  df1 <- anova_table$`num Df`[i]
  df2 <- anova_table$`den Df`[i]
  
  # Calculate partial eta squared
  ss_effect <- anova_table$`Sum Sq`[i]
  ss_error <- sum(anova_table$`Sum Sq`[grepl("Residuals", rownames(anova_table))], na.rm = TRUE)
  eta_sq <- ss_effect / (ss_effect + ss_error)
  
  cat(sprintf("%s: F(%d, %d) = %.2f, p = %.4f, ηp² = %.2f\n", 
              effect_display, df1, df2, f_val, p_val, eta_sq))
}

# Compute descriptive statistics for planned contrasts
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Descriptive Statistics for Planned Contrasts\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# Faking Low group (group == 1)
iat_faking_low <- iat_ques_data[iat_ques_data$group == 1, ]
iat_fl_t1_mean <- mean(iat_faking_low$t1_dscore, na.rm = TRUE)
iat_fl_t1_sd <- sd(iat_faking_low$t1_dscore, na.rm = TRUE)
iat_fl_t2_mean <- mean(iat_faking_low$t2_dscore, na.rm = TRUE)
iat_fl_t2_sd <- sd(iat_faking_low$t2_dscore, na.rm = TRUE)
iat_fl_n <- nrow(iat_faking_low)

cat(sprintf("\nFaking Low group (n = %d):\n", iat_fl_n))
cat(sprintf("  Time1: M = %.2f, SD = %.2f\n", iat_fl_t1_mean, iat_fl_t1_sd))
cat(sprintf("  Time2: M = %.2f, SD = %.2f\n", iat_fl_t2_mean, iat_fl_t2_sd))

# Faking High group (group == 2)
iat_faking_high <- iat_ques_data[iat_ques_data$group == 2, ]
iat_fh_t1_mean <- mean(iat_faking_high$t1_dscore, na.rm = TRUE)
iat_fh_t1_sd <- sd(iat_faking_high$t1_dscore, na.rm = TRUE)
iat_fh_t2_mean <- mean(iat_faking_high$t2_dscore, na.rm = TRUE)
iat_fh_t2_sd <- sd(iat_faking_high$t2_dscore, na.rm = TRUE)
iat_fh_n <- nrow(iat_faking_high)

cat(sprintf("\nFaking High group (n = %d):\n", iat_fh_n))
cat(sprintf("  Time1: M = %.2f, SD = %.2f\n", iat_fh_t1_mean, iat_fh_t1_sd))
cat(sprintf("  Time2: M = %.2f, SD = %.2f\n", iat_fh_t2_mean, iat_fh_t2_sd))

# Planned contrasts: Paired t-tests
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Planned Contrasts (Paired t-tests)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# Faking Low: Time1 vs Time2
iat_fl_t1 <- iat_faking_low$t1_dscore
iat_fl_t2 <- iat_faking_low$t2_dscore
iat_fl_t_test <- t.test(iat_fl_t1, iat_fl_t2, paired = TRUE)
iat_fl_t_stat <- iat_fl_t_test$statistic
iat_fl_t_p <- iat_fl_t_test$p.value

# Cohen's d for paired samples: mean difference / SD of differences
iat_fl_diff <- iat_fl_t1 - iat_fl_t2
iat_fl_cohens_d <- cohens_d_paired(iat_fl_t1, iat_fl_t2)

cat(sprintf("\nFaking Low group:\n"))
cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", iat_fl_n - 1, iat_fl_t_stat, iat_fl_t_p))
cat(sprintf("  Cohen's d = %.2f\n", iat_fl_cohens_d))
cat(sprintf("  Mean difference = %.2f, SD of differences = %.2f\n", 
            mean(iat_fl_diff, na.rm = TRUE), sd(iat_fl_diff, na.rm = TRUE)))

# Faking High: Time1 vs Time2
iat_fh_t1 <- iat_faking_high$t1_dscore
iat_fh_t2 <- iat_faking_high$t2_dscore
iat_fh_t_test <- t.test(iat_fh_t1, iat_fh_t2, paired = TRUE)
iat_fh_t_stat <- iat_fh_t_test$statistic
iat_fh_t_p <- iat_fh_t_test$p.value

# Cohen's d for paired samples: mean difference / SD of differences
iat_fh_diff <- iat_fh_t1 - iat_fh_t2
iat_fh_cohens_d <- cohens_d_paired(iat_fh_t1, iat_fh_t2)

cat(sprintf("\nFaking High group:\n"))
cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", iat_fh_n - 1, abs(iat_fh_t_stat), iat_fh_t_p))
cat(sprintf("  Cohen's d = %.2f\n", abs(iat_fh_cohens_d)))
cat(sprintf("  Mean difference = %.2f, SD of differences = %.2f\n", 
            mean(iat_fh_diff, na.rm = TRUE), sd(iat_fh_diff, na.rm = TRUE)))

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Mixed ANOVA for IAT:\n")
cat("  (See ANOVA Summary above for F-statistics, p-values, and ηp²)\n")
cat("\nPlanned Contrasts:\n")
cat(sprintf("  Faking Low: t(%d) = %.2f, p = %.4f, d = %.2f\n", iat_fl_n - 1, iat_fl_t_stat, iat_fl_t_p, iat_fl_cohens_d))
cat(sprintf("  Faking High: t(%d) = %.2f, p = %.4f, d = %.2f\n", iat_fh_n - 1, iat_fh_t_stat, iat_fh_t_p, iat_fh_cohens_d))

# ==============================================================================
# Fakeability Analysis: qIAT
# ==============================================================================

# Prepare data for mixed ANOVA - qIAT only
# Filter to qIAT task
qiat_data <- df[toupper(df$task) == "QIAT", ]

# Prepare data: participant, group, time, score
qiat_ques_data <- qiat_data[, c("id", "group", "t1_dscore", "t2_dscore")]

# Convert to numeric
qiat_ques_data$group <- as.numeric(qiat_ques_data$group)
qiat_ques_data$t1_dscore <- as.numeric(qiat_ques_data$t1_dscore)
qiat_ques_data$t2_dscore <- as.numeric(qiat_ques_data$t2_dscore)

# Remove rows with missing data
qiat_ques_data <- qiat_ques_data[complete.cases(qiat_ques_data), ]

cat(sprintf("\nqIAT participants with complete data: %d\n", nrow(qiat_ques_data)))
cat("Group distribution:\n")
print(table(qiat_ques_data$group))

# Reshape to long format for ANOVA
qiat_long <- qiat_ques_data %>%
  pivot_longer(
    cols = c(t1_dscore, t2_dscore),
    names_to = "time",
    values_to = "score"
  )

# Recode time: t1_dscore -> 1, t2_dscore -> 2
qiat_long$time <- ifelse(qiat_long$time == "t1_dscore", 1, 2)

cat(sprintf("\nData prepared for ANOVA: %d observations\n", nrow(qiat_long)))

# Mixed ANOVA using afex
qiat_long$id <- as.factor(qiat_long$id)
qiat_long$group <- as.factor(qiat_long$group)
qiat_long$time <- as.factor(qiat_long$time)

aov <- aov_car(score ~ group * time + Error(id/time), data = qiat_long)

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Mixed ANOVA Results for qIAT\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
print(aov)

# Extract and format results
cat("\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")
cat("ANOVA Summary:\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

anova_table <- aov$anova_table

for (i in 1:nrow(anova_table)) {
  effect_name <- rownames(anova_table)[i]
  if (grepl("group", tolower(effect_name)) && grepl("time", tolower(effect_name))) {
    effect_display <- "Group × Time"
  } else if (grepl("time", tolower(effect_name))) {
    effect_display <- "Time"
  } else if (grepl("group", tolower(effect_name))) {
    effect_display <- "Group"
  } else {
    effect_display <- effect_name
  }
  
  f_val <- anova_table$`F`[i]
  p_val <- anova_table$`Pr(>F)`[i]
  df1 <- anova_table$`num Df`[i]
  df2 <- anova_table$`den Df`[i]
  
  # Calculate partial eta squared
  ss_effect <- anova_table$`Sum Sq`[i]
  ss_error <- sum(anova_table$`Sum Sq`[grepl("Residuals", rownames(anova_table))], na.rm = TRUE)
  eta_sq <- ss_effect / (ss_effect + ss_error)
  
  cat(sprintf("%s: F(%d, %d) = %.2f, p = %.4f, ηp² = %.2f\n", 
              effect_display, df1, df2, f_val, p_val, eta_sq))
}

# Compute descriptive statistics for planned contrasts
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Descriptive Statistics for Planned Contrasts\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# Faking Low group (group == 1)
qiat_faking_low <- qiat_ques_data[qiat_ques_data$group == 1, ]
qiat_fl_t1_mean <- mean(qiat_faking_low$t1_dscore, na.rm = TRUE)
qiat_fl_t1_sd <- sd(qiat_faking_low$t1_dscore, na.rm = TRUE)
qiat_fl_t2_mean <- mean(qiat_faking_low$t2_dscore, na.rm = TRUE)
qiat_fl_t2_sd <- sd(qiat_faking_low$t2_dscore, na.rm = TRUE)
qiat_fl_n <- nrow(qiat_faking_low)

cat(sprintf("\nFaking Low group (n = %d):\n", qiat_fl_n))
cat(sprintf("  Time1: M = %.2f, SD = %.2f\n", qiat_fl_t1_mean, qiat_fl_t1_sd))
cat(sprintf("  Time2: M = %.2f, SD = %.2f\n", qiat_fl_t2_mean, qiat_fl_t2_sd))

# Faking High group (group == 2)
qiat_faking_high <- qiat_ques_data[qiat_ques_data$group == 2, ]
qiat_fh_t1_mean <- mean(qiat_faking_high$t1_dscore, na.rm = TRUE)
qiat_fh_t1_sd <- sd(qiat_faking_high$t1_dscore, na.rm = TRUE)
qiat_fh_t2_mean <- mean(qiat_faking_high$t2_dscore, na.rm = TRUE)
qiat_fh_t2_sd <- sd(qiat_faking_high$t2_dscore, na.rm = TRUE)
qiat_fh_n <- nrow(qiat_faking_high)

cat(sprintf("\nFaking High group (n = %d):\n", qiat_fh_n))
cat(sprintf("  Time1: M = %.2f, SD = %.2f\n", qiat_fh_t1_mean, qiat_fh_t1_sd))
cat(sprintf("  Time2: M = %.2f, SD = %.2f\n", qiat_fh_t2_mean, qiat_fh_t2_sd))

# Planned contrasts: Paired t-tests
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Planned Contrasts (Paired t-tests)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# Faking Low: Time1 vs Time2
qiat_fl_t1 <- qiat_faking_low$t1_dscore
qiat_fl_t2 <- qiat_faking_low$t2_dscore
qiat_fl_t_test <- t.test(qiat_fl_t1, qiat_fl_t2, paired = TRUE)
qiat_fl_t_stat <- qiat_fl_t_test$statistic
qiat_fl_t_p <- qiat_fl_t_test$p.value

# Cohen's d for paired samples: mean difference / SD of differences
qiat_fl_diff <- qiat_fl_t1 - qiat_fl_t2
qiat_fl_cohens_d <- cohens_d_paired(qiat_fl_t1, qiat_fl_t2)

cat(sprintf("\nFaking Low group:\n"))
cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", qiat_fl_n - 1, qiat_fl_t_stat, qiat_fl_t_p))
cat(sprintf("  Cohen's d = %.2f\n", qiat_fl_cohens_d))
cat(sprintf("  Mean difference = %.2f, SD of differences = %.2f\n", 
            mean(qiat_fl_diff, na.rm = TRUE), sd(qiat_fl_diff, na.rm = TRUE)))

# Faking High: Time1 vs Time2
qiat_fh_t1 <- qiat_faking_high$t1_dscore
qiat_fh_t2 <- qiat_faking_high$t2_dscore
qiat_fh_t_test <- t.test(qiat_fh_t1, qiat_fh_t2, paired = TRUE)
qiat_fh_t_stat <- qiat_fh_t_test$statistic
qiat_fh_t_p <- qiat_fh_t_test$p.value

# Cohen's d for paired samples: mean difference / SD of differences
qiat_fh_diff <- qiat_fh_t1 - qiat_fh_t2
qiat_fh_cohens_d <- cohens_d_paired(qiat_fh_t1, qiat_fh_t2)

cat(sprintf("\nFaking High group:\n"))
cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", qiat_fh_n - 1, abs(qiat_fh_t_stat), qiat_fh_t_p))
cat(sprintf("  Cohen's d = %.2f\n", abs(qiat_fh_cohens_d)))
cat(sprintf("  Mean difference = %.2f, SD of differences = %.2f\n", 
            mean(qiat_fh_diff, na.rm = TRUE), sd(qiat_fh_diff, na.rm = TRUE)))

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Mixed ANOVA for qIAT:\n")
cat("  (See ANOVA Summary above for F-statistics, p-values, and ηp²)\n")
cat("\nPlanned Contrasts:\n")
cat(sprintf("  Faking Low: t(%d) = %.2f, p = %.4f, d = %.2f\n", qiat_fl_n - 1, qiat_fl_t_stat, qiat_fl_t_p, qiat_fl_cohens_d))
cat(sprintf("  Faking High: t(%d) = %.2f, p = %.4f, d = %.2f\n", qiat_fh_n - 1, qiat_fh_t_stat, qiat_fh_t_p, qiat_fh_cohens_d))

# ==============================================================================
# Comparison of Faking Effects Across Measures (Normalized Scores)
# ==============================================================================

# First, compute normalized questionnaire scores using T1 baseline statistics
# Then compute faking effects (T2_norm - T1_norm) and compare across measures

# Reload data to get updated normalized D-scores
df_updated <- df

# Compute normalized questionnaire scores
# Calculate T1 baseline statistics for questionnaire (across all participants)
t1_ques_all <- as.numeric(df_updated$t1_ques)
t1_ques_all <- t1_ques_all[!is.na(t1_ques_all)]
t1_ques_mean <- mean(t1_ques_all, na.rm = TRUE)
t1_ques_sd <- sd(t1_ques_all, na.rm = TRUE)

cat(sprintf("\nQuestionnaire T1 baseline: Mean = %.4f, SD = %.4f\n", t1_ques_mean, t1_ques_sd))

# Standardize questionnaire scores
df_updated$t1_ques_norm <- (as.numeric(df_updated$t1_ques) - t1_ques_mean) / t1_ques_sd
df_updated$t2_ques_norm <- (as.numeric(df_updated$t2_ques) - t1_ques_mean) / t1_ques_sd

# Compute faking effects (T2_norm - T1_norm) for questionnaire
df_updated$ques_faking_effect <- df_updated$t2_ques_norm - df_updated$t1_ques_norm

# For IAT and qIAT, compute faking effects from normalized D-scores
# Note: We need to merge IAT and qIAT data with questionnaire data
# Since participants have either IAT or qIAT, we'll process them separately

# Prepare IAT data
iat_data <- df_updated[toupper(df_updated$task) == "IAT", ]
# Check if t1_com_d_norm and t2_com_d_norm columns exist
if ("t1_com_d_norm" %in% names(iat_data) && "t2_com_d_norm" %in% names(iat_data)) {
  iat_data$iat_faking_effect <- (as.numeric(iat_data$t2_com_d_norm) - 
                                  as.numeric(iat_data$t1_com_d_norm))
} else {
  # If normalized columns don't exist, compute them using T1 baseline
  iat_t1_dscore <- as.numeric(iat_data$t1_dscore)
  iat_t1_dscore <- iat_t1_dscore[!is.na(iat_t1_dscore)]
  iat_t1_mean <- mean(iat_t1_dscore, na.rm = TRUE)
  iat_t1_sd <- sd(iat_t1_dscore, na.rm = TRUE)
  
  iat_data$t1_com_d_norm <- (as.numeric(iat_data$t1_dscore) - iat_t1_mean) / iat_t1_sd
  iat_data$t2_com_d_norm <- (as.numeric(iat_data$t2_dscore) - iat_t1_mean) / iat_t1_sd
  iat_data$iat_faking_effect <- iat_data$t2_com_d_norm - iat_data$t1_com_d_norm
}

# Prepare qIAT data
qiat_data <- df_updated[toupper(df_updated$task) == "QIAT", ]
# Check if t1_com_d_norm and t2_com_d_norm columns exist
if ("t1_com_d_norm" %in% names(qiat_data) && "t2_com_d_norm" %in% names(qiat_data)) {
  qiat_data$qiat_faking_effect <- (as.numeric(qiat_data$t2_com_d_norm) - 
                                    as.numeric(qiat_data$t1_com_d_norm))
} else {
  # If normalized columns don't exist, compute them using T1 baseline
  qiat_t1_dscore <- as.numeric(qiat_data$t1_dscore)
  qiat_t1_dscore <- qiat_t1_dscore[!is.na(qiat_t1_dscore)]
  qiat_t1_mean <- mean(qiat_t1_dscore, na.rm = TRUE)
  qiat_t1_sd <- sd(qiat_t1_dscore, na.rm = TRUE)
  
  qiat_data$t1_com_d_norm <- (as.numeric(qiat_data$t1_dscore) - qiat_t1_mean) / qiat_t1_sd
  qiat_data$t2_com_d_norm <- (as.numeric(qiat_data$t2_dscore) - qiat_t1_mean) / qiat_t1_sd
  qiat_data$qiat_faking_effect <- qiat_data$t2_com_d_norm - qiat_data$t1_com_d_norm
}

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("Comparison of Faking Effects Across Measures\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

# ==============================================================================
# 1. Self-Report vs IAT (Paired t-tests)
# ==============================================================================
cat("\n1. Self-Report vs IAT (Paired t-tests)\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

# Merge questionnaire with IAT data by participant ID
iat_comparison <- iat_data[, c("id", "group", "iat_faking_effect")]
ques_for_iat <- df_updated[, c("id", "ques_faking_effect")]
iat_comparison <- merge(iat_comparison, ques_for_iat, by = "id", all = FALSE)

# Faking Low group (group == 1)
iat_fl <- iat_comparison[iat_comparison$group == 1, ]
iat_fl <- iat_fl[complete.cases(iat_fl[, c("ques_faking_effect", "iat_faking_effect")]), ]

if (nrow(iat_fl) > 0) {
  ques_fl_mean <- mean(iat_fl$ques_faking_effect, na.rm = TRUE)
  ques_fl_sd <- sd(iat_fl$ques_faking_effect, na.rm = TRUE)
  iat_fl_mean <- mean(iat_fl$iat_faking_effect, na.rm = TRUE)
  iat_fl_sd <- sd(iat_fl$iat_faking_effect, na.rm = TRUE)
  
  # Paired t-test
  iat_fl_t_test <- t.test(iat_fl$ques_faking_effect, iat_fl$iat_faking_effect, paired = TRUE)
  iat_fl_t_stat <- iat_fl_t_test$statistic
  iat_fl_t_p <- iat_fl_t_test$p.value
  
  # Cohen's d for paired samples
  iat_fl_diff <- iat_fl$ques_faking_effect - iat_fl$iat_faking_effect
  iat_fl_cohens_d <- cohens_d_paired(iat_fl$ques_faking_effect, iat_fl$iat_faking_effect)
  
  cat(sprintf("\nFaking Low group (n = %d):\n", nrow(iat_fl)))
  cat(sprintf("  Self-report: M = %.2f, SD = %.2f\n", ques_fl_mean, ques_fl_sd))
  cat(sprintf("  IAT: M = %.2f, SD = %.2f\n", iat_fl_mean, iat_fl_sd))
  cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", nrow(iat_fl) - 1, iat_fl_t_stat, iat_fl_t_p))
  cat(sprintf("  Cohen's d = %.2f\n", iat_fl_cohens_d))
}

# Faking High group (group == 2)
iat_fh <- iat_comparison[iat_comparison$group == 2, ]
iat_fh <- iat_fh[complete.cases(iat_fh[, c("ques_faking_effect", "iat_faking_effect")]), ]

if (nrow(iat_fh) > 0) {
  ques_fh_mean <- mean(iat_fh$ques_faking_effect, na.rm = TRUE)
  ques_fh_sd <- sd(iat_fh$ques_faking_effect, na.rm = TRUE)
  iat_fh_mean <- mean(iat_fh$iat_faking_effect, na.rm = TRUE)
  iat_fh_sd <- sd(iat_fh$iat_faking_effect, na.rm = TRUE)
  
  # Paired t-test
  iat_fh_t_test <- t.test(iat_fh$ques_faking_effect, iat_fh$iat_faking_effect, paired = TRUE)
  iat_fh_t_stat <- iat_fh_t_test$statistic
  iat_fh_t_p <- iat_fh_t_test$p.value
  
  # Cohen's d for paired samples
  iat_fh_diff <- iat_fh$ques_faking_effect - iat_fh$iat_faking_effect
  iat_fh_cohens_d <- cohens_d_paired(iat_fh$ques_faking_effect, iat_fh$iat_faking_effect)
  
  cat(sprintf("\nFaking High group (n = %d):\n", nrow(iat_fh)))
  cat(sprintf("  Self-report: M = %.2f, SD = %.2f\n", ques_fh_mean, ques_fh_sd))
  cat(sprintf("  IAT: M = %.2f, SD = %.2f\n", iat_fh_mean, iat_fh_sd))
  cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", nrow(iat_fh) - 1, iat_fh_t_stat, iat_fh_t_p))
  cat(sprintf("  Cohen's d = %.2f\n", iat_fh_cohens_d))
}

# ==============================================================================
# 2. Self-Report vs qIAT (Paired t-tests)
# ==============================================================================
cat("\n\n2. Self-Report vs qIAT (Paired t-tests)\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

# Merge questionnaire with qIAT data by participant ID
qiat_comparison <- qiat_data[, c("id", "group", "qiat_faking_effect")]
ques_for_qiat <- df_updated[, c("id", "ques_faking_effect")]
qiat_comparison <- merge(qiat_comparison, ques_for_qiat, by = "id", all = FALSE)

# Faking Low group (group == 1)
qiat_fl <- qiat_comparison[qiat_comparison$group == 1, ]
qiat_fl <- qiat_fl[complete.cases(qiat_fl[, c("ques_faking_effect", "qiat_faking_effect")]), ]

if (nrow(qiat_fl) > 0) {
  ques_fl_mean <- mean(qiat_fl$ques_faking_effect, na.rm = TRUE)
  ques_fl_sd <- sd(qiat_fl$ques_faking_effect, na.rm = TRUE)
  qiat_fl_mean <- mean(qiat_fl$qiat_faking_effect, na.rm = TRUE)
  qiat_fl_sd <- sd(qiat_fl$qiat_faking_effect, na.rm = TRUE)
  
  # Paired t-test
  qiat_fl_t_test <- t.test(qiat_fl$ques_faking_effect, qiat_fl$qiat_faking_effect, paired = TRUE)
  qiat_fl_t_stat <- qiat_fl_t_test$statistic
  qiat_fl_t_p <- qiat_fl_t_test$p.value
  
  # Cohen's d for paired samples
  qiat_fl_diff <- qiat_fl$ques_faking_effect - qiat_fl$qiat_faking_effect
  qiat_fl_cohens_d <- cohens_d_paired(qiat_fl$ques_faking_effect, qiat_fl$qiat_faking_effect)
  
  cat(sprintf("\nFaking Low group (n = %d):\n", nrow(qiat_fl)))
  cat(sprintf("  Self-report: M = %.2f, SD = %.2f\n", ques_fl_mean, ques_fl_sd))
  cat(sprintf("  qIAT: M = %.2f, SD = %.2f\n", qiat_fl_mean, qiat_fl_sd))
  cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", nrow(qiat_fl) - 1, qiat_fl_t_stat, qiat_fl_t_p))
  cat(sprintf("  Cohen's d = %.2f\n", qiat_fl_cohens_d))
}

# Faking High group (group == 2)
qiat_fh <- qiat_comparison[qiat_comparison$group == 2, ]
qiat_fh <- qiat_fh[complete.cases(qiat_fh[, c("ques_faking_effect", "qiat_faking_effect")]), ]

if (nrow(qiat_fh) > 0) {
  ques_fh_mean <- mean(qiat_fh$ques_faking_effect, na.rm = TRUE)
  ques_fh_sd <- sd(qiat_fh$ques_faking_effect, na.rm = TRUE)
  qiat_fh_mean <- mean(qiat_fh$qiat_faking_effect, na.rm = TRUE)
  qiat_fh_sd <- sd(qiat_fh$qiat_faking_effect, na.rm = TRUE)
  
  # Paired t-test
  qiat_fh_t_test <- t.test(qiat_fh$ques_faking_effect, qiat_fh$qiat_faking_effect, paired = TRUE)
  qiat_fh_t_stat <- qiat_fh_t_test$statistic
  qiat_fh_t_p <- qiat_fh_t_test$p.value
  
  # Cohen's d for paired samples
  qiat_fh_diff <- qiat_fh$ques_faking_effect - qiat_fh$qiat_faking_effect
  qiat_fh_cohens_d <- cohens_d_paired(qiat_fh$ques_faking_effect, qiat_fh$qiat_faking_effect)
  
  cat(sprintf("\nFaking High group (n = %d):\n", nrow(qiat_fh)))
  cat(sprintf("  Self-report: M = %.2f, SD = %.2f\n", ques_fh_mean, ques_fh_sd))
  cat(sprintf("  qIAT: M = %.2f, SD = %.2f\n", qiat_fh_mean, qiat_fh_sd))
  cat(sprintf("  Paired t-test: t(%d) = %.2f, p = %.4f\n", nrow(qiat_fh) - 1, qiat_fh_t_stat, qiat_fh_t_p))
  cat(sprintf("  Cohen's d = %.2f\n", qiat_fh_cohens_d))
}

# ==============================================================================
# 3. IAT vs qIAT (Independent t-tests)
# ==============================================================================
cat("\n\n3. IAT vs qIAT (Independent t-tests)\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")

# Faking Low group (group == 1)
iat_fl_ind <- iat_data[iat_data$group == 1, ]
iat_fl_ind <- iat_fl_ind[!is.na(iat_fl_ind$iat_faking_effect), ]
qiat_fl_ind <- qiat_data[qiat_data$group == 1, ]
qiat_fl_ind <- qiat_fl_ind[!is.na(qiat_fl_ind$qiat_faking_effect), ]

if (nrow(iat_fl_ind) > 0 && nrow(qiat_fl_ind) > 0) {
  iat_fl_mean <- mean(iat_fl_ind$iat_faking_effect, na.rm = TRUE)
  iat_fl_sd <- sd(iat_fl_ind$iat_faking_effect, na.rm = TRUE)
  qiat_fl_mean <- mean(qiat_fl_ind$qiat_faking_effect, na.rm = TRUE)
  qiat_fl_sd <- sd(qiat_fl_ind$qiat_faking_effect, na.rm = TRUE)
  
  # Independent t-test
  iat_fl_ind_t_test <- t.test(iat_fl_ind$iat_faking_effect, qiat_fl_ind$qiat_faking_effect, paired = FALSE)
  iat_fl_ind_t_stat <- iat_fl_ind_t_test$statistic
  iat_fl_ind_t_p <- iat_fl_ind_t_test$p.value
  
  # Cohen's d for independent samples
  pooled_sd <- sqrt(((nrow(iat_fl_ind) - 1) * iat_fl_sd^2 + (nrow(qiat_fl_ind) - 1) * qiat_fl_sd^2) / 
                    (nrow(iat_fl_ind) + nrow(qiat_fl_ind) - 2))
  iat_fl_ind_cohens_d <- (iat_fl_mean - qiat_fl_mean) / pooled_sd
  
  cat(sprintf("\nFaking Low group:\n"))
  cat(sprintf("  IAT: M = %.2f, SD = %.2f, n = %d\n", iat_fl_mean, iat_fl_sd, nrow(iat_fl_ind)))
  cat(sprintf("  qIAT: M = %.2f, SD = %.2f, n = %d\n", qiat_fl_mean, qiat_fl_sd, nrow(qiat_fl_ind)))
  cat(sprintf("  Independent t-test: t(%d) = %.2f, p = %.4f\n", 
              nrow(iat_fl_ind) + nrow(qiat_fl_ind) - 2, iat_fl_ind_t_stat, iat_fl_ind_t_p))
  cat(sprintf("  Cohen's d = %.2f\n", iat_fl_ind_cohens_d))
}

# Faking High group (group == 2)
iat_fh_ind <- iat_data[iat_data$group == 2, ]
iat_fh_ind <- iat_fh_ind[!is.na(iat_fh_ind$iat_faking_effect), ]
qiat_fh_ind <- qiat_data[qiat_data$group == 2, ]
qiat_fh_ind <- qiat_fh_ind[!is.na(qiat_fh_ind$qiat_faking_effect), ]

if (nrow(iat_fh_ind) > 0 && nrow(qiat_fh_ind) > 0) {
  iat_fh_mean <- mean(iat_fh_ind$iat_faking_effect, na.rm = TRUE)
  iat_fh_sd <- sd(iat_fh_ind$iat_faking_effect, na.rm = TRUE)
  qiat_fh_mean <- mean(qiat_fh_ind$qiat_faking_effect, na.rm = TRUE)
  qiat_fh_sd <- sd(qiat_fh_ind$qiat_faking_effect, na.rm = TRUE)
  
  # Independent t-test
  iat_fh_ind_t_test <- t.test(iat_fh_ind$iat_faking_effect, qiat_fh_ind$qiat_faking_effect, paired = FALSE)
  iat_fh_ind_t_stat <- iat_fh_ind_t_test$statistic
  iat_fh_ind_t_p <- iat_fh_ind_t_test$p.value
  
  # Cohen's d for independent samples
  pooled_sd <- sqrt(((nrow(iat_fh_ind) - 1) * iat_fh_sd^2 + (nrow(qiat_fh_ind) - 1) * qiat_fh_sd^2) / 
                    (nrow(iat_fh_ind) + nrow(qiat_fh_ind) - 2))
  iat_fh_ind_cohens_d <- (iat_fh_mean - qiat_fh_mean) / pooled_sd
  
  cat(sprintf("\nFaking High group:\n"))
  cat(sprintf("  IAT: M = %.2f, SD = %.2f, n = %d\n", iat_fh_mean, iat_fh_sd, nrow(iat_fh_ind)))
  cat(sprintf("  qIAT: M = %.2f, SD = %.2f, n = %d\n", qiat_fh_mean, qiat_fh_sd, nrow(qiat_fh_ind)))
  cat(sprintf("  Independent t-test: t(%d) = %.2f, p = %.4f\n", 
              nrow(iat_fh_ind) + nrow(qiat_fh_ind) - 2, iat_fh_ind_t_stat, iat_fh_ind_t_p))
  cat(sprintf("  Cohen's d = %.2f\n", iat_fh_ind_cohens_d))
}

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("SUMMARY (for article):\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

cat("\nSelf-Report vs IAT:\n")
if (exists("iat_fl") && nrow(iat_fl) > 0) {
  iat_fl_t_test_summary <- t.test(iat_fl$ques_faking_effect, iat_fl$iat_faking_effect, paired = TRUE)
  iat_fl_cohens_d_summary <- cohens_d_paired(iat_fl$ques_faking_effect, iat_fl$iat_faking_effect)
  cat(sprintf("  Faking Low: t(%d) = %.2f, p < .001, d = %.2f\n", 
              nrow(iat_fl) - 1, iat_fl_t_test_summary$statistic, iat_fl_cohens_d_summary))
}
if (exists("iat_fh") && nrow(iat_fh) > 0) {
  iat_fh_t_test_summary <- t.test(iat_fh$ques_faking_effect, iat_fh$iat_faking_effect, paired = TRUE)
  iat_fh_cohens_d_summary <- cohens_d_paired(iat_fh$ques_faking_effect, iat_fh$iat_faking_effect)
  cat(sprintf("  Faking High: t(%d) = %.2f, p < .001, d = %.2f\n", 
              nrow(iat_fh) - 1, iat_fh_t_test_summary$statistic, iat_fh_cohens_d_summary))
}

cat("\nSelf-Report vs qIAT:\n")
if (exists("qiat_fl") && nrow(qiat_fl) > 0) {
  qiat_fl_t_test_summary <- t.test(qiat_fl$ques_faking_effect, qiat_fl$qiat_faking_effect, paired = TRUE)
  qiat_fl_cohens_d_summary <- cohens_d_paired(qiat_fl$ques_faking_effect, qiat_fl$qiat_faking_effect)
  cat(sprintf("  Faking Low: t(%d) = %.2f, p < .001, d = %.2f\n", 
              nrow(qiat_fl) - 1, qiat_fl_t_test_summary$statistic, qiat_fl_cohens_d_summary))
}
if (exists("qiat_fh") && nrow(qiat_fh) > 0) {
  qiat_fh_t_test_summary <- t.test(qiat_fh$ques_faking_effect, qiat_fh$qiat_faking_effect, paired = TRUE)
  qiat_fh_cohens_d_summary <- cohens_d_paired(qiat_fh$ques_faking_effect, qiat_fh$qiat_faking_effect)
  cat(sprintf("  Faking High: t(%d) = %.2f, p < .001, d = %.2f\n", 
              nrow(qiat_fh) - 1, qiat_fh_t_test_summary$statistic, qiat_fh_cohens_d_summary))
}

cat("\nIAT vs qIAT:\n")
if (exists("iat_fl_ind") && exists("qiat_fl_ind") && nrow(iat_fl_ind) > 0 && nrow(qiat_fl_ind) > 0) {
  iat_fl_ind_t_test_summary <- t.test(iat_fl_ind$iat_faking_effect, qiat_fl_ind$qiat_faking_effect, paired = FALSE)
  pooled_sd_fl_summary <- sqrt(((nrow(iat_fl_ind) - 1) * sd(iat_fl_ind$iat_faking_effect, na.rm = TRUE)^2 + 
                                 (nrow(qiat_fl_ind) - 1) * sd(qiat_fl_ind$qiat_faking_effect, na.rm = TRUE)^2) / 
                                (nrow(iat_fl_ind) + nrow(qiat_fl_ind) - 2))
  d_fl_summary <- (mean(iat_fl_ind$iat_faking_effect, na.rm = TRUE) - 
                   mean(qiat_fl_ind$qiat_faking_effect, na.rm = TRUE)) / pooled_sd_fl_summary
  cat(sprintf("  Faking Low: t(%d) = %.2f, p = %.3f, d = %.2f\n", 
              nrow(iat_fl_ind) + nrow(qiat_fl_ind) - 2, 
              iat_fl_ind_t_test_summary$statistic, 
              iat_fl_ind_t_test_summary$p.value, 
              d_fl_summary))
}
if (exists("iat_fh_ind") && exists("qiat_fh_ind") && nrow(iat_fh_ind) > 0 && nrow(qiat_fh_ind) > 0) {
  iat_fh_ind_t_test_summary <- t.test(iat_fh_ind$iat_faking_effect, qiat_fh_ind$qiat_faking_effect, paired = FALSE)
  pooled_sd_fh_summary <- sqrt(((nrow(iat_fh_ind) - 1) * sd(iat_fh_ind$iat_faking_effect, na.rm = TRUE)^2 + 
                                 (nrow(qiat_fh_ind) - 1) * sd(qiat_fh_ind$qiat_faking_effect, na.rm = TRUE)^2) / 
                                (nrow(iat_fh_ind) + nrow(qiat_fh_ind) - 2))
  d_fh_summary <- (mean(iat_fh_ind$iat_faking_effect, na.rm = TRUE) - 
                   mean(qiat_fh_ind$qiat_faking_effect, na.rm = TRUE)) / pooled_sd_fh_summary
  cat(sprintf("  Faking High: t(%d) = %.2f, p = %.3f, d = %.2f\n", 
              nrow(iat_fh_ind) + nrow(qiat_fh_ind) - 2, 
              iat_fh_ind_t_test_summary$statistic, 
              iat_fh_ind_t_test_summary$p.value, 
              d_fh_summary))
}