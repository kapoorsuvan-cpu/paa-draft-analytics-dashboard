# Rising-Senior NFL Draft Round Projection — v9

This project predicts where **2025 college juniors, who become rising seniors for
the 2026 season**, may be selected in the **2027 NFL Draft**.

## Main improvements

- Uses sophomore and junior seasons separately.
- Creates sophomore-to-junior deltas and growth rates.
- Adds recruiting, height, weight, school draft history, conference level,
  missing-data indicators, coverage indicators, and position-year percentiles.
- Uses fallback player-season matching when a transfer causes a school mismatch.
- Uses layered draft matching with school and position confidence.
- Removes highly missing, near-constant, and almost-all-zero features only when
  sufficient stronger features remain.
- Keeps position-specific drafted/undrafted models.
- Uses both:
  - a direct four-class round-tier model; and
  - cumulative ordinal models that respect round order.
- Blends the direct and ordinal round probabilities.
- Learns position-specific draft-probability thresholds from out-of-fold
  training predictions instead of using a fixed 15% threshold.
- Reports majority baseline, exact tier accuracy, within-one-tier accuracy,
  macro F1, balanced accuracy, ordinal MAE, weighted kappa, and log loss.
- Exact pick remains exploratory and does not determine the displayed round.

## Target tiers

- `R1`
- `R2_3`
- `R4_5`
- `R6_7`

## Setup

```bash
cd /Users/suvankapoor/Downloads/nfl_draft_model_round_accuracy_v9

Rscript install_packages.R

# One-time setup for Google's TabFM (Python 3.12, isolated environment)
./setup_tabfm.sh

export CFBD_API_KEY='YOUR_NEW_CFBD_KEY'

Rscript run_pipeline.R 2>&1 | tee pipeline_log.txt
```

TabFM is evaluated on the same temporal holdout and conservatively blended into
the round-tier probabilities. Its standalone and augmented results appear in
`outputs/round_tier_holdout_metrics.csv`. Set `RUN_TABFM <- FALSE` in
`R/config.R` to run the original R-only pipeline.

Wait until the terminal prompt returns before entering another command.

## Experimental rising-junior model

Run the separate sophomore-cutoff experiment after the main pipeline and TabFM
environment are available:

```bash
Rscript R/08_run_sophomore_experiment.R
```

This re-runs the correlation and data-quality feature selection by position,
trains new drafted/undrafted classifiers, trains direct and ordinal conditional
round models, blends TabFM probabilities, validates on the 2022-2023 temporal
holdout, and scores players listed as sophomores in 2025. It estimates eventual
draft outcomes; it does not predict which players will enter the next draft
class. The early cutoff also cannot anticipate many later breakouts, role
changes, transfers, injuries, testing results, or development.

Key experimental outputs:

```text
outputs/sophomore_position_feature_correlations.csv
outputs/sophomore_position_feature_data_quality.csv
outputs/sophomore_draft_probability_holdout_metrics.csv
outputs/sophomore_round_tier_holdout_metrics.csv
outputs/rising_juniors_2026_experimental_draft_board.csv
```

## Main outputs

```text
outputs/draft_probability_holdout_metrics.csv
outputs/draft_probability_holdout_predictions.csv
outputs/round_tier_holdout_metrics.csv
outputs/round_tier_holdout_predictions.csv
outputs/round_tier_confusion_matrix.csv
outputs/round_tier_metrics_by_year.csv
outputs/exploratory_pick_holdout_metrics.csv
outputs/position_feature_data_quality.csv
outputs/historical_data_quality_summary.csv
outputs/draft_match_quality_summary.csv
outputs/rising_seniors_2026_projected_2027_draft_board.csv
outputs/rising_seniors_2026_projected_draftable_board.csv
```

## Important interpretation

The displayed round is conditional on the player clearing a draft-probability
threshold learned for that position. The output also includes all four tier
probabilities so close calls remain exploratory rather than falsely precise.

## Manual draft-match overrides

If a known drafted player does not match correctly, add a row to:

```text
manual_draft_matches.csv
```

Then rerun from `R/02_build_junior_dataset.R` onward.
