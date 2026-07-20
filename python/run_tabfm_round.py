#!/usr/bin/env python3
"""Run Google TabFM for the temporally held-out and current round-tier rows."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
from tabfm import TabFMClassifier
from tabfm import tabfm_v1_0_0_jax as tabfm_release


TARGET = "round_tier"
ID = ".tabfm_row_id"
LEVELS = ["R1", "R2_3", "R4_5", "R6_7"]


def predict(model, train_path: Path, test_path: Path, estimators: int) -> pd.DataFrame:
    train = pd.read_csv(train_path)
    test = pd.read_csv(test_path)
    feature_cols = [c for c in train.columns if c not in (TARGET, ID)]
    missing = sorted(set(feature_cols) - set(test.columns))
    if missing:
        raise ValueError(f"Test data is missing TabFM features: {missing}")

    classifier = TabFMClassifier(
        model=model,
        n_estimators=estimators,
        batch_size=1,
        random_state=2027,
        verbose=True,
    )
    classifier.fit(train[feature_cols], train[TARGET].astype(str).to_numpy())
    probability = np.asarray(classifier.predict_proba(test[feature_cols]))
    by_class = dict(zip(classifier.classes_, probability.T))
    output = pd.DataFrame({ID: test[ID].astype(int)})
    for level in LEVELS:
        output[f"prob_{level}"] = by_class.get(level, np.zeros(len(test)))
    rowsums = output[[f"prob_{x}" for x in LEVELS]].sum(axis=1)
    output.loc[:, [f"prob_{x}" for x in LEVELS]] = output[
        [f"prob_{x}" for x in LEVELS]
    ].div(rowsums, axis=0)
    return output


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--exchange-dir", type=Path, required=True)
    parser.add_argument("--estimators", type=int, default=8)
    args = parser.parse_args()
    exchange = args.exchange_dir

    model = tabfm_release.load()
    holdout = predict(
        model,
        exchange / "round_train.csv",
        exchange / "round_holdout.csv",
        args.estimators,
    )
    holdout.to_csv(exchange / "round_holdout_probabilities.csv", index=False)

    current = predict(
        model,
        exchange / "round_full_history.csv",
        exchange / "round_current.csv",
        args.estimators,
    )
    current.to_csv(exchange / "round_current_probabilities.csv", index=False)


if __name__ == "__main__":
    main()
