#!/usr/bin/env bash
# install.sh — set up the Docker sandbox for Codex.
# Delegates to .agents-sandbox/install.sh (shared library).
#
# Usage: bash .codex/sandbox/install.sh [--force] [install-dir]

source "$(git rev-parse --show-toplevel)/.agents-sandbox/install.sh"
install_sandbox codex "$@"
