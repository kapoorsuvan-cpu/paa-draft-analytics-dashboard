#!/bin/sh
set -eu

cd "$(dirname "$0")"
uv venv --python 3.12 .venv-tabfm
uv pip install --python .venv-tabfm/bin/python 'tabfm[jax]==1.0.0' pandas
echo "TabFM environment is ready. Model weights download on the first run."
