# R Script for Identifying Conflicting Party Names and Codes
## Author: Samantha Goldstein
## Email: srgoldst@umich.edu

## Note: Ensure your current working directory contains the CLEA file. 

## Step 1: Load and install packages

# Package names
packages <- c("readxl", "tidyverse", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

## Step 2: Read in CLEA file and filter by country
file <- read_excel("pty_names_and_codes_with_month_and_release.xlsx")

Sri_Lanka = file %>% filter(ctr_n=="Sri Lanka")

# Step 3: Identify cases with the same party code, but different party names
same_pty_diff_pty_n = Sri_Lanka %>% group_by(pty, pty_n) %>% summarize(n=n()) %>% ungroup() %>% group_by(pty) %>% filter(n()>1)

# Step 4: Identify cases with the same party code, but different party names occuring in the same month and year
same_pty_diff_pty_n_same_mn_yr = Sri_Lanka %>% mutate(pty_n = stringr::str_to_lower(pty_n)) %>% group_by(pty, pty_n, mn, yr) %>% filter(n()>1)

# Step 5: Identify cases with same party name, but different party codes
diff_pty_same_pty_n = Sri_Lanka %>%  mutate(pty_n = stringr::str_to_lower(pty_n)) %>% group_by(pty, pty_n) %>% summarize(n=n()) %>% ungroup() %>% group_by(pty_n) %>% filter(n()>1) %>% arrange(pty_n)

# Step 6 (Optional): Check duplicates for all countries 
ctr_dups = file %>% group_by(ctr_n, pty, pty_n, mn, yr) %>% summarize(n=n()) %>% filter(n>1)