#!/usr/bin/env bash
# manifest.sh — declarative spec of which workspace-* skills are cached
# in this repo, and where their canonical impl lives on the host.
#
# Source this file from install.sh / sync.sh — it defines:
#
#   CACHED_SKILLS  array of "name|impl-source-path" entries.
#
# `name`          basename installed under $HOME/.claude/skills/<name>.
#                 May be a directory (the usual SKILL.md + run.sh layout)
#                 or a single file (e.g. emit-workspace-commands.sh).
# `impl-source`   absolute path on the host to the canonical impl.  May
#                 reference $HOME.  When sync.sh runs and this path
#                 exists, its contents are mirrored into this directory
#                 under <name>; that mirrored copy is the persisted
#                 cache that ships with the repo and is what install.sh
#                 falls back to when the live impl isn't on the host.

CACHED_SKILLS=(
  "workspace-annotate|$HOME/.config/doom-worktrees/copy-to-clipboard-skill/.claude/skills/workspace-annotate"
  "workspace-merge|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace-merge"
  "workspace-status|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace-status"
  "workspace-update|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace-update"
  "generate-workspace|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/generate-workspace"
  "emit-workspace-commands.sh|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/emit-workspace-commands.sh"
)
