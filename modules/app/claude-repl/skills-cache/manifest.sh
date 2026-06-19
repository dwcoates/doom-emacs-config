#!/usr/bin/env bash
# manifest.sh — declarative spec of which workspace-* skills are managed,
# and where each skill's canonical impl lives on the host.
#
# Source this file from install.sh — it defines:
#
#   CACHED_SKILLS  array of "name|impl-source-path" entries.  (The name is
#                  retained for back-compat; there is no cache anymore.)
#
# `name`          basename installed under $HOME/.claude/skills/<name>.
#                 May be a directory (the usual SKILL.md + run.sh layout)
#                 or a single file (e.g. emit-workspace-commands.sh).
# `impl-source`   absolute path on the host to the CANONICAL impl.  May
#                 reference $HOME.  install.sh symlinks $HOME/.claude/
#                 skills/<name> straight to this path — there is NO cache
#                 fallback.  If the impl is absent, install FAILS HARD
#                 rather than serve a stale copy.  Most impls live in the
#                 explanation-engine repo; workspace-annotate is the lone
#                 exception, sourced from a doom-worktree.

CACHED_SKILLS=(
  "workspace-annotate|$HOME/.config/doom-worktrees/copy-to-clipboard-skill/.claude/skills/workspace-annotate"
  "workspace-merge|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace-merge"
  "workspace-status|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace-status"
  "workspace-update|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace-update"
  "generate-workspace|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/generate-workspace"
  "emit-workspace-commands.sh|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/emit-workspace-commands.sh"
  "build-skill|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/build-skill"
)
