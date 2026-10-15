---
name: arrange-sidebar
description: Read and organize the agent-repl sidebar's repository grouping — its repositories' order, each repository's named subsections, and which subsection each workspace sits in — which the Emacs tab bar's order follows. Creates, renames, deletes and reorders subsections, reorders repositories, and files workspaces into a subsection in bulk. Use when the user asks to organize, group, sort or tidy the sidebar or the tab bar, or after creating several child workspaces that belong together under one named subsection.
allowed-tools: Bash(<skill_base_dir>/run.sh:*)
lineage_root: user.dodge.skills.arrange-sidebar
---

## What This Skill Does

Reads the sidebar's arrangement and applies changes to it, one change per call. The sidebar and the Emacs tab bar both follow the arrangement at once: repositories in order, inside each its unsectioned workspaces, then each subsection's. A workspace's children always sit with it, so only a workspace with no parent is ever placed.

## Arguments

| Argument | Behaviour |
|---|---|
| `<goal>` | What to arrange, in the user's words (e.g. "group the three auth workspaces under 'auth'"). |
| (none) | Organize the sidebar as the conversation has asked. |

## Changes

Each change is passed to `--arrange` as one JSON object. Refs (`{"id": …, "dir": …}` for workspaces and repositories, `{"id": …}` for subsections) are copied verbatim from `--read`'s output.

| Change | JSON |
|---|---|
| Create a subsection (lands last in its repository) | `{"createSubsection":{"repository":<repo ref>,"name":"<name>"}}` |
| Rename a subsection | `{"renameSubsection":{"subsection":<sub ref>,"name":"<name>"}}` |
| Delete a subsection (its workspaces return to the repository) | `{"deleteSubsection":{"subsection":<sub ref>}}` |
| Move a subsection | `{"moveSubsection":{"subsection":<sub ref>,"before":<sub ref>}}` or `…,"end":{}}` |
| Move a repository | `{"moveRepository":{"repository":<repo ref>,"before":<repo ref>}}` or `…,"end":{}}` |
| File workspaces, in the order given, as one block | `{"placeWorkspaces":{"workspaces":[<ws ref>,…],"subsection":<sub ref>,"end":{}}}` — use `"noSubsection":{}` instead of `"subsection"` to file them back in no subsection, and `"before":<ws ref>` instead of `"end"` to put them before a workspace already there |

## Steps

1. Read the arrangement.
  - Call `<skill_base_dir>/run.sh --read`.
    - `EXIT CODE 0:` stdout is the arrangement. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.

2. Plan the changes.
  - List, in order, the changes from the **Changes** table that reach the goal, using only refs from step 1.
  - A workspace listed with a `parent` is never placed on its own; place its root instead.
  - If the goal is ambiguous about which workspaces or names are meant, ask the user and STOP until they answer.

3. Apply each change, in order.
  - Call `<skill_base_dir>/run.sh --arrange '<change JSON>'`.
    - `EXIT CODE 0:` Applied. A `createSubsection` answer carries the new subsection's ref under `created`; use it in later changes. Continue to the next change, then to step 4.
    - `EXIT CODE 1:` Refused; nothing changed. Re-run step 1, re-plan the remaining changes from the fresh arrangement, and continue. If the same change is refused twice, surface the printed refusal and STOP.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.

4. Confirm.
  - Call `<skill_base_dir>/run.sh --read` and report the resulting arrangement in a few lines: each changed repository, its subsections, and the workspaces in each.
    - `EXIT CODE 0:` Done.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.

## Notes

- **CRITICAL NOTE: Refs come only from `--read`.** NEVER build a ref from a path, a branch, or a name.
- **CRITICAL NOTE: One change per `--arrange` call.** File several workspaces with ONE `placeWorkspaces` change rather than one call each.
- **IMPORTANT NOTE: Arranging moves nothing on disk.** It changes only the order and grouping the sidebar and the tab bar draw; it never closes, merges or edits a workspace.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
