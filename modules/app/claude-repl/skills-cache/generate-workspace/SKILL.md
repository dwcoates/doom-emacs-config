---
name: workspace-generation
description: Generate git branch/worktree names for planned work. Use when the user describes work they want to do and wants workspace or branch names generated, asks to "/workspace-generation", or wants to plan out branches for upcoming tasks.
---

# Workspace Generation

The user will describe work they want to do across one or more workspaces in plain English. Your job is to generate practical git branch/worktree names for each workspace, write them to a JSON file, and confirm. A downstream text editor is going to use this file you generate to create workspaces for itself (which will have a corresponding branch/worktree it'll create for them as well).

Do NOT attempt to generate git branches or worktrees yourself in git. Under NO circumstances. The handling of branch/worktree generation is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code or any other files or mutating effects should be done, either.

## Scope: no investigation, only synthesis

This skill is strictly about turning the user's request into the dispatch JSON. The dispatch JSON is merely a concise description of the workspace to be created, and the near entirey of the work is to be done by the downstream workspaces created from the dispatch JSON itself. Thus, any actual investigation — running tests, walking source files, reading whole Slack threads or PR diffs, spidering through linked resources, reproducing bugs, exploring the codebase — is OUT OF SCOPE here and belongs to the *generated* workspace's session, not this one. For example, running this skill on a given slack thread should simply create dispatch JSON with a prompt for "investigate Slack thread xyz \<perhaps also with some additional context specified by the user that is important to include in the prompt\>"

Concretely:

- **Allowed**, because it directly informs branch naming or the initial prompt: parsing the user's plain-English request, fetching the *root* Slack message or GH ticket title (one shallow call, not a recursive crawl), reading at most a few lines of an obviously-cited file when the branch slug would otherwise be a guess.
- **Not allowed**, even when it might be useful: running tests, executing scripts, reading large file ranges, fetching every reply in a thread, following every link in a thread, expanding linked PRs, walking blame, building dependency graphs, or any other multi-step exploration. Each of these can take minutes and is exactly what the downstream workspace is designed to do.
- **The escape hatch**: when work would require investigation to *do correctly*, encode the investigation as instructions in the `"prompt"` field of the generated `"create"` entry so the spawned workspace performs it after launch. The spawned session has the time and tools to do this properly; this session does not.

The skill MUST finish quickly. If you find yourself reaching for a second or third `gns slack convo`, a `gh pr view --json files`, a `WebFetch`, or a code read past a few lines, stop and route that work into the workspace prompt instead.

## Gathering Context via GNS

When the user's request references external resources (Slack messages, GitHub PRs, etc.), use the `gns` CLI to fetch the *minimum* context needed to draft branch names and an initial prompt. Use `gns --help` and `gns <subcommand> --help` for full details beyond what's listed here. Per the **Scope** section above, fetches should be shallow — typically a single root-message lookup, not a full conversation crawl.

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

When a link appears in the user's request, do the *minimum* lookup needed to draft branch names and an initial prompt — never an exhaustive crawl. Per the **Scope** section, deeper investigation belongs to the generated workspace, not to this one. Recommended posture per link type:

- **Slack links** (`chesscom.slack.com/archives/...`): one `gns slack convo <link>` is fine for the root message, but do not chase every link inside that conversation. Reference the link in the workspace prompt and let the spawned session pull what it needs.
- **GitHub PR/issue links** (`github.com/org/repo/pull/N`): one `gh pr view` for title and body is fine. Do NOT fetch `--json files`, walk the diff, or pull blame.
- **Jira links**: extract the ticket ID for branch naming and stop.
- **Other URLs**: include them verbatim in the workspace prompt rather than fetching them here.

## Steps

1. **Interpret** the user's description as a description of the branches or a description of the process to generate the branch names.
  - When the user enumerates the branches explicitly (e.g. by name or by a small fixed list), use that enumeration directly.
  - When the user describes a *process* for enumerating branches (e.g. "one per skipped test", "one per failing CI job"), do NOT execute that process here. Per the **Scope** section, this skill does not run tests, scripts, or other multi-step exploration. Instead, generate a SINGLE workspace whose `"prompt"` field instructs the spawned session to perform the enumeration and re-invoke `/workspace-generation` from inside that session with the concrete list.
  - **NOTE**: attempt to spin up agents dedicated to each soon-to-be branch name when possible.

2. **Generate branch names** for each workspace:
  - Branch Names should be short, lowercase, hyphen-separated slugs — not long descriptions.
    - E.g., Longer descriptions take up lots of space in the editor! Should not be longer than 3 words.
  - **Do NOT include a user prefix yourself.** Emit the bare slug (e.g. `hello-world`). Prefixing is handled downstream.
  - If the user supplied a Jira ticket, include it as a sub-prefix using the format `<ticket-id>/<feature-name>` (e.g. `CV-100/fix-login`).
  - **Do NOT append a random hash suffix yourself.** Disambiguator suffixes are handled downstream. Never call `openssl`, `uuidgen`, `tr`, etc. to mint your own.

3. **Determine commands**: Build an array of typed command objects.
  - Always emit one `"create"` entry per workspace.
  - If the user **explicitly** asked to send a message to the generated workspaces, attach it as an inline `"prompt"` field on each `"create"` entry. Do **not** emit separate `"prompt"` entries for newly created workspaces — the inline form is preferred.
  - Separate `"prompt"` entries (without a `"create"`) are only for targeting **existing** workspaces by name.
  - If the user specifies a priority for a workspace, include a `"priority"` field on the `"create"` entry. Valid values are `"p05"`, `"p1"`, `"p2"`, `"p3"`. This displays a priority badge image in the tab-bar. The field is optional — omit it if no priority is specified.
  - If the caller specifies a base ref to branch from, include a `"base_commit"` field on the `"create"` entry (e.g. `"HEAD"`, `"origin/master"`, a SHA, or any other git ref). When omitted on a non-fork create, the default base is resolved downstream. Forks (`fork_from` set) skip this entirely.
    - **Implicit base from source workspace**: when a `[source-ws:<name> path:<dir>]` tag is present AND the caller did NOT explicitly specify a base ref AND `fork_from` is NOT set, resolve the source workspace's current HEAD with `git -C <dir> rev-parse HEAD` and emit the resulting SHA as `"base_commit"` on every `create` entry whose `git_root` came from that tag.
    - Pin to the SHA so the new worktree is deterministic even if the source workspace advances between dispatch and worktree creation.
    - Do NOT override an explicit user-supplied `base_commit`.
    - Do NOT emit `base_commit` when `fork_from` is set.
    - If `git -C <dir> rev-parse HEAD` fails, stop and surface the error to the user.
  - If the caller specifies a fork source, include a `"fork_from"` field naming the source workspace (the new worktree branches from HEAD and resumes that workspace's Claude session via `--fork-session`). When `fork_from` is set, the downstream consumer ignores any explicit `base_commit` and uses HEAD. If `fork_from` resolution fails (unknown workspace, no active session), the workspace is NOT created — there is no silent fallback.
  - **Always include a `"git_root"` field on every `"create"` entry** — it is required by the downstream consumer, which does not fall back to ambient context. Resolution order:
    1. If the user links a GitHub PR/issue or names a specific repo, use the local checkout path (e.g. `~/workspace/ChessCom/explanation-engine`).
    2. If a `[source-ws:<name> path:<dir>]` tag is present in the user's message (always injected when invoked from the claude-repl input buffer), use `<dir>` directly as `git_root`. Fail loudly if the tag is malformed or the path is empty — do not guess.
    3. If neither of the above applies, ask the user which repo to use before proceeding.

    Use `~` literals where convenient; they are expanded downstream.

   Example — create with initial prompt (inline, preferred):
   ```json
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "prompt": "hello world"},
     {"type": "create", "name": "feature-two", "git_root": "~/workspace/ChessCom/explanation-engine", "prompt": "hello world"}
   ]
   ```

   Example — create without prompt:
   ```json
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine"},
     {"type": "create", "name": "feature-two", "git_root": "~/workspace/ChessCom/explanation-engine"}
   ]
   ```

   Example — create with priority:
   ```json
   [
     {"type": "create", "name": "urgent-fix", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p05"},
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p1"},
     {"type": "create", "name": "nice-to-have", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p3"}
   ]
   ```

   Example — create with prompt and priority:
   ```json
   [
     {"type": "create", "name": "fix-release-pipeline", "git_root": "~/workspace/ChessCom/explanation-engine", "priority": "p1", "prompt": "triage failing release job"}
   ]
   ```

   Example — create branched from HEAD with an explicit `base_commit` (no fork):
   ```json
   [
     {"type": "create", "name": "follow-up", "git_root": "~/workspace/ChessCom/explanation-engine", "base_commit": "HEAD", "prompt": "extend the change just made on this branch"}
   ]
   ```

   Example — fork an existing workspace's Claude session into a new worktree:
   ```json
   [
     {"type": "create", "name": "parallel-attempt", "git_root": "~/workspace/ChessCom/explanation-engine", "fork_from": "feature-one", "prompt": "try the alternative approach"}
   ]
   ```

   Example — prompt existing workspaces only (pass the workspace name verbatim, with whatever prefix it already has):
   ```json
   [
     {"type": "prompt", "workspace": "DWC/feature-one", "prompt": "hello world"}
   ]
   ```

4. **Dispatch the commands** by piping the JSON array into the skill's `run.sh`. Do not write any file yourself — always go through `run.sh`.
   ```bash
   cat <<'EOF' | <skill_base_dir>/run.sh
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine"},
     {"type": "create", "name": "feature-two", "git_root": "~/workspace/ChessCom/explanation-engine"}
   ]
   EOF
   ```
   `<skill_base_dir>` is the base directory of this skill as injected by the harness at invocation time (it appears in the system reminder above the skill's instructions).

   **`run.sh` is FIRE-AND-FORGET.** After invoking it, you are DONE with the dispatch step — there is no verification phase, no post-dispatch check, no second call. The exit code of `run.sh` is the sole and authoritative signal:
   - Exit 0 → the dispatch succeeded. Trust it unconditionally and move on. The exit code is delivered to you directly by the tool harness — you do NOT need to print it, capture it, `echo $?` it, or otherwise observe it to know its value. **Empty stdout on exit 0 is the EXPECTED success signature**, not an ambiguous result demanding verification. Do NOT `ls`, `cat`, `stat`, `find`, `test -f`, `[ -e ... ]`, or otherwise inspect anything to "confirm." Any post-dispatch inspection will likely show no trace — that is normal, not a failure. **Missing artifact ≠ failure. Empty stdout ≠ failure.**
   - Exit non-zero → stop immediately and surface the error to the user verbatim. Do not retry, do not adapt, do not work around, do not "re-run with stderr captured."
   - Do NOT re-invoke `run.sh` for any reason — not to retry, not to verify, not to debug, not to "fix" anything, **and specifically not to print or capture the exit code**. Appending `echo "Exit code: $?"`, `; echo $?`, `&& echo ok`, `|| echo fail`, or any other shell wrapper to a second `run.sh` invocation is FORBIDDEN: each invocation produces a fresh random suffix, so the second call silently dispatches a DUPLICATE workspace with a different name. The exit code is already in your hands via the tool result — re-running the command to "see" it is the canonical foot-gun this skill exists to prevent. Re-invocation is unsafe; there is no idempotency safety net.
   - Do NOT attempt to fix, infer, or self-correct any aspect of the dispatch process. The pipeline is correct; assume your understanding of it is the unreliable part.

6. **Tell the user** the workspace names that were written and that the workspaces will be created automatically. If prompt commands were included, mention that the prompts will be dispatched once each session is ready.
