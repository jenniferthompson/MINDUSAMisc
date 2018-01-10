################################################################################
## Describe AUDIT scores for ancillary project
################################################################################

library(tidyverse)

## Load data
status_df <- readRDS("../MINDUSADataMgmt/analysisdata/rds/ptstatus.rds")
adm_df <- readRDS("../MINDUSADataMgmt/analysisdata/rds/admission.rds")

## AUDIT is calculated two ways, among all patients. We want four histograms:
## - Consented patients + randomized only
## - Missing = 0, missing = NA

## Create a single data.frame with all these
consented_pts <- status_df %>% filter(consented & !excluded_ever) %>% pull(id)
rand_pts <- status_df %>% filter(randomized) %>% pull(id)

## Set up arguments for pmap
pmap_args <- list(
  "ids" = list(consented_pts, consented_pts, rand_pts, rand_pts),
  "pt_grp" = c(rep("Consented Patients", 2), rep("Randomized Patients", 2)),
  "audit_var" = rep(str_subset(names(adm_df), "^audit"), 2),
  "audit_type" = rep(
    c("Any missing questions -> total score = NA", "Missing questions scored as 0"),
    2
  )
)

## Function to create df with AUDIT variable for given IDs
## Input = list where first element = ID, second = variable name
create_audit_df <- function(ids, pt_grp, audit_var, audit_type){
  adm_df %>%
    filter(id %in% ids) %>%
    select(id, one_of(audit_var)) %>%
    rename_at(vars(one_of(audit_var)), funs(gsub("\\_.+$", "", .))) %>%
    mutate(audit_type = audit_type, pt_grp = pt_grp)
}

audit_df <- pmap_df(pmap_args, create_audit_df)

ggplot(data = audit_df, aes(x = audit)) +
  facet_grid(pt_grp ~ audit_type) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "AUDIT Scores at ICU Admission",
    y = "Frequency"
  )

## How many had 8 or more?
audit_df %>%
  group_by(pt_grp, audit_type) %>%
  summarise(n_pts = n(),
            n_scores = sum(!is.na(audit)),
            over_8 = sum(audit >= 8, na.rm = TRUE))
