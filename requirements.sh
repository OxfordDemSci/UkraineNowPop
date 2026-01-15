#!/usr/bin/env bash
set -euo pipefail

# usage: ./install-requirements.sh /path/to/repo
REPO_PATH="${1:-}"
if [[ -z "$REPO_PATH" ]]; then
  echo "Usage: $0 /path/to/repo"
  exit 2
fi

if [[ ! -d "$REPO_PATH" ]]; then
  echo "Error: repo path '$REPO_PATH' does not exist."
  exit 3
fi

# make sure sudo is available
if ! command -v sudo >/dev/null 2>&1; then
  echo "Error: 'sudo' not found. Run the apt steps as root or install sudo."
  exit 4
fi

# ask for sudo once (will prompt for password)
sudo -v

export DEBIAN_FRONTEND=noninteractive

# run apt commands under sudo only
sudo apt-get update -y

sudo apt-get install -y --no-install-recommends \
  build-essential \
  python3.12 \
  python3.12-venv \
  python3-pip \
  python3-dev \
  curl

sudo apt-get install -y --no-install-recommends \
  libssl-dev \
  libudunits2-dev \
  unixodbc-dev \
  libpq-dev \
  libfontconfig1-dev \
  libcurl4-openssl-dev \
  libgdal-dev \
  libgit2-dev \
  libharfbuzz-dev \
  libfribidi-dev

# clean apt caches (sudo)
sudo apt-get clean
sudo rm -rf /var/lib/apt/lists/*

# ---- python steps (run as current user) ----#
cd "$REPO_PATH"

# create venv if missing (use python3.12 explicitly)
if [[ ! -d "venv" ]]; then
  python3.12 -m venv ./venv
fi

# activate venv
# shellcheck disable=SC1091
source ./venv/bin/activate

# upgrade pip/build tools (inside venv)
python -m pip install --upgrade pip setuptools wheel

# install psycopg2 (do not include in requirements.txt because it fails in Docker containers)
python -m pip install psycopg2

# install from requirements
if [[ -f "requirements.txt" ]]; then
  python -m pip install -r requirements.txt
else
  echo "Warning: requirements.txt not found in $REPO_PATH"
fi

# install the github package 
python -m pip install --no-deps --no-build-isolation git+https://github.com/GISRedeDev/PyNetworkFriction

# init alembic only if missing 
if [[ ! -d "alembic" && ! -f "alembic.ini" ]]; then
  alembic init alembic
else
  echo "alembic already initialized; skipping 'alembic init'."
fi

# ---- R dependencies ----#
if command -v Rscript >/dev/null 2>&1; then
  Rscript requirements.R || echo "Rscript finished with errors"
else
  echo "Rscript not found; skipping R package install. Install R (r-base) if needed."
fi
