#!/usr/bin/env bash
# Wrapper — pipes stdin to the shared emit script.
exec bash "$(dirname "$0")/../emit-workspace-commands.sh"
