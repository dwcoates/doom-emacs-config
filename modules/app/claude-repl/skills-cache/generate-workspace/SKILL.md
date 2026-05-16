---
name: workspace-generation
description: Generate git branch/worktree names for planned work. Use when the user describes work they want to do and wants workspace or branch names generated, asks to "/workspace-generation", or wants to plan out branches for upcoming tasks.
---

# Workspace Generation

The user will describe work they want to do across one or more workspaces in plain English. Your job is to generate practical git branch/worktree names for each workspace, write them to a JSON file, and confirm. A downstream text editor is going to use this file you generate to create workspaces for itself (which will have a corresponding branch/worktree it'll create for them as well).

Do NOT attempt to generate git branches or worktrees yourself in git. Under NO circumstances. The handling of branch/worktree generation is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code or any other files or mutating effects should be done, either.

## Gathering Context via GNS

When the user's request references external resources (Slack messages, GitHub PRs, etc.), use the `gns` CLI to fetch context before generating workspaces. Use `gns --help` and `gns <subcommand> --help` for full details beyond what's listed here.

### Slack

Read a Slack conversation (root message + all replies) given a link:
```bash
gns slack convo <slack_link> --all --json
```

Parse a Slack link into channel ID and timestamps:
```bash
gns slack link <slack_url> --json
```

Read thread replies by channel and timestamp:
```bash
gns slack thread <channel_id> <thread_ts> --all --json
```

Search Slack messages:
```bash
gns slack search "<query>" --count 20 --json
```

Look up a Slack user by ID:
```bash
gns slack user get <user_id> --json
```

Look up a Slack channel by name:
```bash
gns slack channel lookup <name> --json
```

Get your own identity:
```bash
gns whoami --json
```

### GitHub

Use the `gh` CLI for GitHub operations:
```bash
gh pr view <number> --json title,body,url,files
gh issue view <number> --json title,body,url
```

### Knowledge Base

Search the company knowledge base:
```bash
gns search "<query>" --limit 10 --json
```

### Following Links

When analyzing Slack threads, **follow all links exhaustively**:
- **Slack links** (`chesscom.slack.com/archives/...`): use `gns slack convo <link>`
- **GitHub PR/issue links** (`github.com/org/repo/pull/N`): use `gh pr view` or `gh issue view`
- **Jira links**: extract the ticket ID for branch naming
- **Other URLs**: use `WebFetch` if available, or note them for the workspace prompt

## Steps

1. **Interpret** the user's description as a description of the branches or a description of the process to generate the branch names. 
  - **EXAMPLE**: "one for each of the skipped tests listed in the test suite output" should be interpreted as the following process by you
    - first -> identify how to run the corresponding tests
    - second -> run the tests
    - third -> enumerate the items found in the output
    - fourth -> spin up one agent item, and determine the corresponding branch names
  - **NOTE**: attempt to spin up agents dedicated to each soon-to-be branch name when possible

2. **Generate branch names** for each workspace:
  - Branch Names should be short, lowercase, hyphen-separated slugs — not long descriptions. 
    - E.g., Longer descriptions take up lots of space in the editor! Should not be longer than 3 words (after the prefixes). 
  - Branch names should start with `DWC/` prefix. 
    - E.g., `DWC/hello-world`
  - If the user supplied a Jira ticket, be sure to include it in the name as a prefix using the format `DWC/<ticket-id>/<feature-name>` 
    - E.g., `DWC/CV-100/fix-login`. 
  - **Do NOT append a random hash suffix yourself.** Emit the bare, human-readable name (e.g. `DWC/fix-login`). The downstream consumer (Emacs) detects collisions against existing workspaces, on-disk worktrees, git branches, and start tags, and only when a collision is found does it append a random 3-letter lowercase suffix (e.g. `-abc`). When there is no collision, the on-disk name matches what you emit verbatim. Never call `openssl`, `uuidgen`, `tr`, etc. to mint your own suffix — disambiguation is exclusively the downstream consumer's responsibility, and only fires on actual collision.

3. **Determine commands**: Build an array of typed command objects.
  - Always emit one `"create"` entry per workspace.
  - If the user **explicitly** asked to send a message to the generated workspaces, attach it as an inline `"prompt"` field on each `"create"` entry. Do **not** emit separate `"prompt"` entries for newly created workspaces — the inline form is preferred.
  - Separate `"prompt"` entries (without a `"create"`) are only for targeting **existing** workspaces by name.
  - If the user specifies a priority for a workspace, include a `"priority"` field on the `"create"` entry. Valid values are `"p05"`, `"p1"`, `"p2"`, `"p3"`. This displays a priority badge image in the tab-bar. The field is optional — omit it if no priority is specified.
  - If the caller specifies a base ref to branch from, include a `"base_commit"` field on the `"create"` entry (e.g. `"HEAD"`, `"origin/master"`, a SHA, or any other git ref). When omitted, the downstream consumer defaults to `HEAD` for forks and `origin/master` otherwise.
    - **Implicit base from source workspace**: when a `[source-ws:<name> path:<dir>]` tag is present AND the caller did NOT explicitly specify a base ref AND `fork_from` is NOT set, resolve the source workspace's current HEAD with `git -C <dir> rev-parse HEAD` and emit the resulting SHA as `"base_commit"` on every `create` entry whose `git_root` came from that tag. This makes the new workspace branch from the commit the invoking workspace is on, instead of silently falling through to `origin/master`.
    - Pin to the SHA (not the literal `"HEAD"`) so the new worktree is deterministic even if the source workspace advances between dispatch and worktree creation.
    - Do NOT override an explicit user-supplied `base_commit`.
    - Do NOT emit `base_commit` when `fork_from` is set — downstream ignores it and the combination is misleading.
    - If `git -C <dir> rev-parse HEAD` fails, stop and surface the error to the user — do not silently fall through to `origin/master`.
  - If the caller specifies a fork source, include a `"fork_from"` field naming the source workspace (the new worktree branches from HEAD and resumes that workspace's Claude session via `--fork-session`). When `fork_from` is set, the downstream consumer ignores any explicit `base_commit` and uses HEAD. If `fork_from` resolution fails (unknown workspace, no active session), the workspace is NOT created — there is no silent fallback.
  - **Always include a `"git_root"` field on every `"create"` entry** — it is required by the downstream consumer, which does not fall back to ambient context. Resolution order:
    1. If the user links a GitHub PR/issue or names a specific repo, use the local checkout path (e.g. `~/workspace/ChessCom/explanation-engine`).
    2. If a `[source-ws:<name> path:<dir>]` tag is present in the user's message (always injected when invoked from the claude-repl input buffer), use `<dir>` directly as `git_root`. Fail loudly if the tag is malformed or the path is empty — do not guess.
    3. If neither of the above applies, ask the user which repo to use before proceeding.

    Use `~` literals where convenient; they are expanded downstream.

   Example — create with initial prompt (inline, preferred):
   ```json
   [
     {"type": "create", "name": "DWC/feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "prompt": "hello world"},
     {"type": "create", "name": "DWC/feature-two", "git_root": "~/workspace/ChessCom/explanation-engine", "prompt": "hello world"}
   ]
   ```

   Example — create without prompt:
   ```json
   [
     {"type": "create", "name": "DWC/feature-one", "git_root": "~/workspace/ChessCom/explanation-engine"},
     {"type": "create", "name": "DWC/feature-two", "git_root": "~/workspace/ChessCom/explanation-engine"}
   ]
   ```

   Example — create with priority:
   ```json
   [
     {"type": "create", "name": "DWC/urgent-fix", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p05"},
     {"type": "create", "name": "DWC/feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p1"},
     {"type": "create", "name": "DWC/nice-to-have", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p3"}
   ]
   ```

   Example — create with prompt and priority:
   ```json
   [
     {"type": "create", "name": "DWC/fix-release-pipeline", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p1", "prompt": "triage failing release job"}
   ]
   ```

   Example — create branched from HEAD with an explicit `base_commit` (no fork):
   ```json
   [
     {"type": "create", "name": "DWC/follow-up", "git_root": "~/workspace/ChessCom/explanation-engine", "base_commit": "HEAD", "prompt": "extend the change just made on this branch"}
   ]
   ```

   Example — fork an existing workspace's Claude session into a new worktree:
   ```json
   [
     {"type": "create", "name": "DWC/parallel-attempt", "git_root": "~/workspace/ChessCom/explanation-engine", "fork_from": "feature-one", "prompt": "try the alternative approach"}
   ]
   ```

   Example — prompt existing workspaces only:
   ```json
   [
     {"type": "prompt", "workspace": "DWC/feature-one", "prompt": "hello world"}
   ]
   ```

4. **Write the commands** by piping the JSON array into the skill's `run.sh`. `run.sh` is the single source of truth for writing the payload atomically to `~/.claude/output/workspace_commands_<uuid>.json`. It does not mutate `name` fields — collision disambiguation lives downstream (Emacs), not in `run.sh`. Do not write the file yourself with `mktemp`/`mv` — always go through `run.sh`.
   ```bash
   cat <<'EOF' | <skill_base_dir>/run.sh
   [
     {"type": "create", "name": "DWC/feature-one", "git_root": "~/workspace/ChessCom/explanation-engine"},
     {"type": "create", "name": "DWC/feature-two", "git_root": "~/workspace/ChessCom/explanation-engine"}
   ]
   EOF
   ```
   `<skill_base_dir>` is the base directory of this skill as injected by the harness at invocation time (it appears in the system reminder above the skill's instructions).

   **`run.sh` is FIRE-AND-FORGET.** After invoking it, you are DONE with the dispatch step — there is no verification phase, no post-write check, no inspection of `~/.claude/output/`, no second call. The exit code of `run.sh` is the sole and authoritative signal:
   - Exit 0 → the write succeeded. Trust it unconditionally and move on. Do NOT `ls`, `cat`, `stat`, `find`, `test -f`, `[ -e ... ]`, or otherwise look at `~/.claude/output/` to "confirm." The downstream consumer (Emacs) watches that directory and deletes each file the instant it is dispatched, so any post-write inspection will almost always show your file missing — that is normal, not a failure. **Missing file ≠ failure.**
   - Exit non-zero → stop immediately and surface the error to the user verbatim. Do not retry, do not adapt, do not work around, do not "re-run with stderr captured."
   - Do NOT re-invoke `run.sh` for any reason — not to retry, not to verify, not to debug, not to "fix" anything. A second call dispatches duplicate workspaces with the same bare names, and downstream disambiguation will silently fork them apart — but the duplicates are not desired.
   - Do NOT attempt to fix, infer, or self-correct any aspect of the dispatch process. The pipeline is correct; assume your understanding of it is the unreliable part.

6. **Tell the user** the workspace names that were written and that the workspaces will be created automatically. If prompt commands were included, mention that the prompts will be dispatched once each session is ready.
