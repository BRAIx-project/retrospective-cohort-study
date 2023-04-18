library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(gt)
library(animate)
`%+%` <- paste0

source("helpers/cohorts.R")
source("helpers/database.R")
source("helpers/episodes.R")
source("helpers/groups.R")

source("helpers/data.R")

source("helpers/statistics.R")
source("helpers/plot.R")
source("helpers/agents.R")
source("helpers/scenario.R")
source("helpers/flowchart.R")

source("helpers/disagreement.R")
source("helpers/log.R")
source("helpers/optimpp.R")
source("helpers/flowchart_summary.R")

source("helpers/threshold.R")
source("helpers/economics.R")
source("helpers/economics_tables.R")
source("helpers/analysis.R")
set.seed(20220802)
