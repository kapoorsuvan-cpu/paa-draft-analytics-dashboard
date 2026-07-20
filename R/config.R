# Run all scripts from the project root.
PROJECT_ROOT <- normalizePath(".", mustWork = FALSE)
RAW_DIR <- file.path(PROJECT_ROOT, "data", "raw")
PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
MODEL_DIR <- file.path(PROJECT_ROOT, "models")
OUTPUT_DIR <- file.path(PROJECT_ROOT, "outputs")

invisible(lapply(
  c(RAW_DIR, PROCESSED_DIR, MODEL_DIR, OUTPUT_DIR),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

HISTORICAL_JUNIOR_YEARS <- 2014:2023
CURRENT_STAT_YEAR <- 2025L
TARGET_DRAFT_YEAR <- 2027L
DRAFT_LOOKAHEAD_YEARS <- 2L
UNDRAFTED_PICK <- 300L

POSITIONS <- c(
  "QB", "RB", "WR", "TE", "OT", "OG", "C",
  "EDGE", "IDL", "ILB", "CB", "SAF", "K", "P"
)

ROUND_TIER_LEVELS <- c("R1", "R2_3", "R4_5", "R6_7")
ROUND_TIER_MIDPOINTS <- c(
  R1 = 16.5, R2_3 = 66.5, R4_5 = 144.5, R6_7 = 224.5
)

# Data-quality filters. Sparse variables are removed only when enough stronger
# variables remain. Position fallback features are retained automatically.
MAX_FEATURE_MISSING <- 0.65
MAX_FEATURE_ZERO_SHARE <- 0.985
MIN_FEATURE_UNIQUE <- 3L
MAX_POSITION_FEATURES <- 70L

MIN_POSITION_ROWS <- 45L
MIN_DRAFTED_ROWS <- 15L
MIN_ROUND_ROWS <- 180L

HOLDOUT_YEAR_COUNT <- 2L
CV_FOLDS <- 5L
TUNING_GRID_SIZE <- 12L
SEED <- 2027L

# The final displayed draft status uses a threshold learned from out-of-fold
# training predictions for each position. This value is only a fallback.
DEFAULT_DRAFT_PROBABILITY_THRESHOLD <- 0.35

# Probability blend for the pooled direct multiclass and cumulative ordinal
# round models. Evaluation reports both components and the blend.
ORDINAL_BLEND_WEIGHT <- 0.60

# TabFM is blended conservatively with the existing direct/ordinal ensemble.
# Set RUN_TABFM to FALSE when the optional Python environment is unavailable.
RUN_TABFM <- TRUE
TABFM_PYTHON <- file.path(PROJECT_ROOT, ".venv-tabfm", "bin", "python")
TABFM_BLEND_WEIGHT <- 0.50
TABFM_ESTIMATORS <- 8L

RUN_ROLLING_VALIDATION <- FALSE
