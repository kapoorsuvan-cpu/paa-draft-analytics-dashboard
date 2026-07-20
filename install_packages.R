required <- c(
  "tidyverse", "stringi", "cfbfastR", "nflreadr",
  "tidymodels", "finetune", "glmnet", "ranger",
  "xgboost", "themis"
)

missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
