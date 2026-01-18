# Faking Article - Code Repository

This repository contains the code, materials, data, and analyses for a research study on faking behavior in implicit association tests (IAT) and questionnaire-based IAT (qIAT) measures of extraversion.

## Project Structure

---

## `/analysis` Directory

Contains R scripts for data analysis and figure generation.

### `S1.R`
**Study 1 Replication Analysis** - Comprehensive analysis script that replicates all results from Study 1 of the article. The script systematically goes through each analysis and statistic reported, including:
- Data loading and preprocessing
- Participant exclusions
- Statistical tests (t-tests, correlations, etc.)
- Reliability analyses (split-half with Spearman-Brown correction)
- Effect size calculations (Cohen's d)
- Correlation comparisons using Fisher z transformation
- All reported statistics and results for Study 1

### `S2.R`
**Study 2 Analysis** - Comprehensive analysis script containing all analyses for Study 2 of the article. Similar structure to S1.R, including:
- Data loading and preprocessing for Study 2
- Participant exclusions
- Statistical analyses specific to Study 2
- Reliability and validity assessments
- Effect size calculations
- All reported statistics and results for Study 2

### `figures.R`
**Figures for Article** - R script dedicated to generating all figures for the article using combined participants data from both Study 1 and Study 2. Includes:
- Data loading and preparation for visualization
- ggplot2 figure generation with publication-quality settings
- Correlation plots
- Group comparison visualizations
- All figures reported in the article

---

## `/data` Directory

Contains raw and processed data files.

### Root Data Files

#### `combined_participants_S1&S2.csv`
Combined participant data file containing data from both Study 1 and Study 2. Includes participant-level variables, task performance metrics, exclusion flags, and group assignments.

#### `combined_participants_S1&S2.txt`
Details for each column in the combined CSV file.

### `/data/raw` Directory

#### `S1.csv`
Raw data file for Study 1 participants,  exported directly from Qualtrics.

#### `S2.csv`
Raw data file for Study 2 participants, exported directly from Qualtrics.

### `/data/tasks` Directory

Contains processed task data files organized by study.

#### `/data/tasks/S1/` and `/data/tasks/S2/`
Each study directory contains Excel files with processed task data:
- **`iat1.xlsx`** and **`iat2.xlsx`**: Processed IAT (Implicit Association Test) data files. Each file contains two sheets:
  - `Participants`: Participant-level aggregated data including D-scores, exclusion criteria, and performance metrics
  - `Trials`: Trial-level data with individual response latencies, correctness, and trial characteristics
- **`qiat1.xlsx`** and **`qiat2.xlsx`**: Processed qIAT (questionnaire-based IAT) data files with the same structure as IAT files

---

## `/materials` Directory

Contains experimental materials, task scripts, and analysis tools.

### Text Files

#### `Faking Materials.txt`
Contains the experimental manipulation instructions for the faking conditions:
- **Faking High Condition**: Instructions asking participants to present themselves as extraverted (for a desirable job scenario)
- **Faking Low Condition**: Instructions asking participants to present themselves as introverted (to avoid an undesirable job scenario)
- **Manipulation Checks**: Questions to verify participants understood the faking instructions

#### `Measures Materials.txt`
Contains all questionnaire and task items used in the study:
- **Questionnaire Items**: Extraversion and introversion items (5 each) for self-report measures
- **qIAT Items**: 
  - Extraversion items (non-reversed, 5 items)
  - Extraversion items (reversed, 5 items)
  - True/False items (False condition, 5 items)
  - True/False items (True condition, 5 items)
- **IAT Items**:
  - Extraversion attribute words (5 items: sociable, talkative, active, impulsive, outgoing)
  - Introversion attribute words (5 items: shy, reticent, passive, deliberate, reserved)
  - Self category words (5 items: I, me, mine, self, myself)
  - Others category words (5 items: others, they, them, their, theirs)

### `/materials/IAT` Directory

#### `extraversion_IAT.js`
JavaScript file used to generate the IAT task for measuring extraversion.

### `/materials/qIAT` Directory

#### `qIAT.js`
Core JavaScript framework file used to generate the qIAT task.

#### `extraversion_qIAT.js`
JavaScript file used to generate the qIAT task, specifically for measuring extraversion. Extends the base `qIAT.js` framework and defines the four categories (non-reversed extraversion items, reversed extraversion items, false statements, and true statements) used in the extraversion qIAT.

#### `qiat_analyze.py`
Python script for parsing and analyzing qIAT data. Processes raw qIAT data files, applies exclusion criteria, calculates D-scores for various trial splits, and outputs results to Excel files with "Participants" and "Trials" sheets.

---

## Usage Notes

### Running Analyses

1. **For Study 1**: Run `analysis/S1.R` in R or RStudio
2. **For Study 2**: Run `analysis/S2.R` in R or RStudio
3. **For Figures**: Run `analysis/figures.R` in R or RStudio

### Processing Task Data

To process raw qIAT data:
```python
from materials.qIAT.qiat_analyze import parse_and_analyze

# Process a data file
output_file = parse_and_analyze(
    file_path='path/to/data.csv',
    qiat_column_name='qIAT',  # Column name containing qIAT data
    id_column_name='ResponseId',  # Column name for participant IDs
    task_name='qIAT',  # Output file prefix
    exclusion_too_fast=300,
    exclusion_max_fast_perc=0.1,
    exclusion_max_error_perc=0.3,
    exclusion_max_latency=30000,
    analyze_max_latency=10000,
    analyze_min_latency=400
)
```

### Task Implementation

The IAT and qIAT tasks are designed to be embedded in Qualtrics surveys. The JavaScript files use the MinnoJS framework and should be integrated according to Qualtrics survey flow requirements.

---

## Data Structure

### Participant Data Columns (Typical)
- `id`: Participant identifier
- `study`: Study identifier (S1 or S2)
- `task`: Task type (IAT or qIAT)
- `group`: Experimental group assignment
- `condition`: Condition assignment (0 or 1)
- `exclude`: Exclusion flag
- `dscore`: Overall D-score
- `dscore_even`, `dscore_odd`: Split-half D-scores
- `percFast`, `percError`: Performance metrics
- Additional task-specific metrics

### Trial Data Columns (Typical)
- `id`: Participant identifier
- `condition`: Condition assignment
- `trialN`: Trial number
- `block`: Block number
- `stimulus`: Stimulus identifier
- `latency`: Response latency in milliseconds
- `correct`: Correctness (0 or 1)
- Additional trial characteristics

---

## Notes

- The project uses chunked processing for large datasets to manage memory efficiently
- Exclusion criteria can be customized in the analysis functions
- The qIAT analysis script supports both CSV and Excel input formats
- All statistical analyses use standard R statistical libraries
- Figures are generated with ggplot2 using publication-quality settings (300 DPI)

---

## Contact

For questions about the code or analyses, please refer to the article or contact the corresponding author.
