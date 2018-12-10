source(here("scripts/01_setup.R"))
source(here("scripts/02_read_ncqa_data.R"))
source(here("scripts/03_read_KFF_data.R"))
source(here("scripts/10_clean_NCQA_data.R"))
source(here("scripts/20_viz_distributions.R"))
source(here("scripts/30_setup_choropleths.R"))
source(here("scripts/31_map_choropleths.R"))
source(here("scripts/40_join_KFF_NCQA.R"))
source(here("scripts/50_correlations.R"))
source(here("scripts/51_modeling.R"))
source(here("scripts/50_predictions.R"))
library(ezknitr)
ezknit(file = here("Rmd_documents/60_word_output.R"), wd = here(),
       out_dir = here("output_documents"), out_suffix = "docx",
       fig_dir = here(), keep_html = F)
ezknit(file = here("Rmd_documents/61_ppt_output.R"), wd = here(),
       out_dir = here("output_documents"), out_suffix = "pptx",
       fig_dir = here(), keep_html = F)
