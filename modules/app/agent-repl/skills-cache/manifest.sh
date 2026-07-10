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
#                 rather than serve a stale copy.  Every impl here lives in
#                 the explanation-engine repo.
#
# NON-MAIN-WORKTREE RULE: an impl-source MUST NOT point into a non-main
# git worktree (e.g. a transient $HOME/.config/doom-worktrees/* checkout).
# Such a path dangles the moment the worktree is pruned.  install.sh
# rejects any such entry hard.  The folded `/workspace-*' skills
# (workspace-merge, workspace-status, workspace-update, generate-workspace)
# now live inside the single `/workspace' skill and are no longer managed
# here; `workspace-annotate' was removed for lacking a main-worktree home.

CACHED_SKILLS=(
  "workspace|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/workspace"
  "build-skill|$HOME/workspace/ChessCom/explanation-engine/.claude/skills/build-skill"
)
