---
name: workspace-generation
description: Generate git branch/worktree names for planned work. Use when the user describes work they want to do and wants workspace or branch names generated, asks to "/workspace-generation", or wants to plan out branches for upcoming tasks. When the user says "one shot" / "one-shot" (or passes `--one-shot`), the spawned workspace is told to open and merge its own PR when done.
---

# Workspace Generation

The user will describe work they want to do across one or more workspaces in plain English. Your job is to generate practical git branch/worktree names for each workspace, write them to a JSON file, and confirm. A downstream text editor is going to use this file you generate to create workspaces for itself (which will have a corresponding branch/worktree it'll create for them as well).

## Arguments

The primary input is the user's plain-English description of the work (interpreted per the **Steps** below). The following flags/tokens may also appear in the request:

| Argument | Behaviour |
|---|---|
| `--master` | Base every generated `create` entry on the repo's resolved main branch (`origin/<main>`) instead of the source workspace's current HEAD. Ignored when `fork_from` is set; never overrides an explicit caller-supplied base ref. See Step 2 and Step 5. |
| `--one-shot` (also triggered by the user saying "one shot" / "one-shot") | Augment every generated `create` entry's prompt with a trailing instruction telling the spawned workspace to open and merge its PR once the work is finished (`When finished with the work, run /create-or-update-pr --self-certified --rebase --add-to-merge-queue --patch to open the PR and merge it.`). The augmentation is performed deterministically by `run.sh`; you opt in by exporting `CLAUDE_WORKSPACE_ONE_SHOT=1` when piping the dispatch JSON into `run.sh`. Composes with an existing inline prompt (appended after a blank line) or synthesizes a prompt carrying just the instruction when none was supplied. Prompt-only entries and explicit-prompt mode are never touched. See Step 5 and Step 6. |
| `[source-ws:<name> path:<dir>]` | Explicit source-workspace tag identifying the initiating workspace and its repo path. See Step 1. |
| `EXPLICIT_PROMPT_FOR_WORKSPACE` … `END_EXPLICIT_PROMPT_FOR_WORKSPACE` | Verbatim-prompt block whose body becomes the `create` entry's `prompt` unchanged. See **Explicit prompt mode**. |

> ⛔ **NEVER manually create git worktrees or branches.**
> This prohibition is absolute and applies to this skill and to every agent acting on its behalf.
> Commands such as `git worktree add`, `git checkout -b`, `git switch -c`, or any other git
> command that creates a branch or worktree are **strictly forbidden** here.
> Worktrees and branches MUST be created exclusively through the workspace-generation dispatch
> machinery — that is, by emitting the dispatch JSON and piping it through `run.sh`.
> There are no exceptions. Manual worktree/branch creation bypasses the downstream pipeline,
> produces state the editor cannot track, and is the root cause of real production incidents.

Do NOT attempt to generate git branches or worktrees yourself in git. Under NO circumstances. The handling of branch/worktree generation is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code or any other files or mutating effects should be done, either.

## Scope: no investigation, only synthesis

This skill is strictly about turning the user's request into the dispatch JSON. The dispatch JSON is merely a concise description of the workspace to be created, and the near entirey of the work is to be done by the downstream workspaces created from the dispatch JSON itself. Thus, any actual investigation — running tests, walking source files, reading whole Slack threads or PR diffs, spidering through linked resources, reproducing bugs, exploring the codebase — is OUT OF SCOPE here and belongs to the *generated* workspace's session, not this one. For example, running this skill on a given slack thread should simply create dispatch JSON with a prompt for "investigate Slack thread xyz \<perhaps also with some additional context specified by the user that is important to include in the prompt\>"

Concretely:

- **Allowed**, because it directly informs branch naming or the initial prompt: parsing the user's plain-English request, fetching the *root* Slack message or GH ticket title (one shallow call, not a recursive crawl), reading at most a few lines of an obviously-cited file when the branch slug would otherwise be a guess.
- **Not allowed**, even when it might be useful: running tests, executing scripts, reading large file ranges, fetching every reply in a thread, following every link in a thread, expanding linked PRs, walking blame, building dependency graphs, or any other multi-step exploration. Each of these can take minutes and is exactly what the downstream workspace is designed to do.
- **The escape hatch**: when work would require investigation to *do correctly*, encode the investigation as instructions in the `"prompt"` field of the generated `"create"` entry so the spawned workspace performs it after launch. The spawned session has the time and tools to do this properly; this session does not.

The skill MUST finish quickly. If you find yourself reaching for a second or third `gns slack convo`, a `gh pr view --json files`, a `WebFetch`, or a code read past a few lines, stop and route that work into the workspace prompt instead.

## Explicit prompt mode

When the caller supplies the spawned workspace's prompt verbatim, the skill performs ZERO interpretation of it — just package it through into the dispatch JSON. Use this mode whenever the caller has already drafted the prompt themselves (e.g. an automated trigger that built the prompt before invoking the skill, like agent-slackbot's `personal-priority.sh`).

The caller signals explicit-prompt mode by including a single delimited block in their request:

```
EXPLICIT_PROMPT_FOR_WORKSPACE
<verbatim prompt content>
END_EXPLICIT_PROMPT_FOR_WORKSPACE
```

When this block is present:

- Use the block body VERBATIM as the `"prompt"` field of the generated `"create"` entry. No editing, no rewrapping, no summarizing, no condensing — its whitespace, punctuation, and inline directives all carry through unchanged.
- Do NOT follow any links, URLs, references, or directives INSIDE the block. They are instructions for the spawned workspace, not for this session — do not fetch any Slack thread, GitHub PR/issue, or other URL that appears inside the block.
- Emit exactly ONE `"create"` entry carrying this prompt. Do not split it across multiple workspaces and do not duplicate it.
- All other create-entry fields (`name`, `git_root`, `source_ws`, `base_commit`, `fork_from`, `priority`) come from caller-supplied context OUTSIDE the block (e.g. lines like `git_root: ~/path/to/repo` or `slug hint: <slug>`), NEVER from interpreting the block's content. `source_ws` resolution still follows Step 1 (explicit tag or current-workspace fallback) — explicit-prompt mode does not exempt the source-ws invariant.
- Branch slug generation per Step 4 still applies, but base it on the caller's slug hint or other non-block context — not on the block's content.
- If more than one `EXPLICIT_PROMPT_FOR_WORKSPACE` block is present, stop and surface the error to the caller. This skill does not multiplex multiple verbatim prompts.
- The `--one-shot` argument does NOT apply in explicit-prompt mode — the verbatim guarantee wins. Do NOT export `CLAUDE_WORKSPACE_ONE_SHOT` when dispatching an explicit-prompt block, so the block body reaches the workspace exactly as supplied. If the caller wants the open-and-merge instruction, they must include it inside the block themselves.

## Handing off uncommitted changes via the stash (`stash-as-transfer-mechanism`)

When the source workspace has uncommitted working-tree edits that the spawned workspace should start from, you can transfer them through the git stash without committing the WIP, sharing a patch file, or copying files by hand. Git worktrees attached to the same repository share one underlying object store — including the stash list — so a stash created in the source workspace is reachable from the spawned worktree.

The flow:

1. In the **source** workspace, park the relevant changes and capture the stash's stable SHA:
   ```bash
   git stash push -m "handoff: feature-one wip" -- path/to/file.cpp path/to/other.h
   git rev-parse stash@{0}    # → e.g. 2bf11a4d... — copy this SHA
   ```
2. Embed that SHA in the spawned workspace's `"prompt"` so the materialization is its first step:
   ```
   First, run `git stash apply 2bf11a4d` to materialize the parked working-tree changes, then <the actual task>.
   ```
3. The **spawned** workspace runs `git stash apply 2bf11a4d` as its first action. `apply` (not `pop`) materializes the changes into its own worktree while leaving the stash entry intact, so the source workspace's stash is undisturbed.

**Always use the stash's SHA, never `stash@{N}`.** `stash@{N}` is a positional index, not a stable handle — any subsequent `git stash push` in the source workspace shifts every index by one, so `stash@{0}` in the prompt may point at a different (or nonexistent) stash by the time the spawned workspace runs. The SHA from `git rev-parse stash@{0}` is stable and is the only reliable way to refer to the parked changes from another worktree's prompt.

This is the workspace-creation-time path. For handing uncommitted changes to an **already-running** workspace, the `workspace-update` skill documents the equivalent flow.

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

1. **Resolve source-ws — MANDATORY FIRST STEP, no exceptions.** Every dispatch MUST carry a known source workspace before any other context-gathering or JSON-building begins. The source workspace identifies which workspace initiated this generation and supplies the canonical repo path for `git_root`.
  - Resolution order:
    a. **Explicit tag.** If the user's message contains `[source-ws:<name> path:<dir>]`, parse it and use `<name>` as the source workspace name and `<dir>` as its absolute path. Fail loudly if the tag is malformed or either component is empty — do NOT guess.
    b. **Current-workspace fallback.** Otherwise, default to the current workspace by running both:
       - `git rev-parse --abbrev-ref HEAD` for the source workspace name.
       - `git rev-parse --show-toplevel` for the source workspace path.
       Surface to the user in the post-dispatch summary (Step 7) that the fallback fired and which values were derived — the agent's choice MUST be visible, never silent.
    c. **No git context.** If neither (a) nor (b) yields a workspace name and path (e.g. invocation is outside any git repo and no tag was supplied), STOP and surface the error. Do NOT invent values. Do NOT proceed to later steps.
  - Record the resolved `(source_ws_name, source_ws_path)` for use by all subsequent steps. Every `"create"` entry emitted by Step 5 carries this pair as a mandatory `"source_ws"` field.
  - **CRITICAL**: this resolution is non-negotiable. The dispatch is REJECTED downstream if a `create` entry is missing `source_ws.name` or `source_ws.path`.

2. **Resolve the main branch — ONLY when the request includes `--master`.** Skip this step entirely when `--master` is absent.
  - Determine the `git_root` the `create` entries will use: default `source_ws.path` from Step 1, or the linked repo's checkout path when the request targets a distinct repo (same `git_root` resolution as Step 5).
  - Call the skill's `run.sh` in read-only resolve mode to obtain the main branch name:
    ```bash
    <skill_base_dir>/run.sh --resolve-master <git_root>
    ```
  - React to the exit code:
    - **EXIT CODE 0**: stdout is the resolved main branch name (e.g. `master` or `main`). Record it for Step 5.
    - **Any non-zero exit**: STOP and surface the error to the user verbatim. Do NOT guess a branch name, do NOT fall back to `master`/`main`, and do NOT proceed to later steps.
  - **This `--resolve-master` call is READ-ONLY and is EXEMPT from the no-reinvocation rule** that governs the dispatch call (Step 6). It performs no dispatch and mutates nothing, so calling — or re-calling — it is always safe. The no-reinvocation prohibition applies ONLY to the fire-and-forget dispatch verb in Step 6.

3. **Interpret** the user's description as a description of the branches or a description of the process to generate the branch names.
  - When the user enumerates the branches explicitly (e.g. by name or by a small fixed list), use that enumeration directly.
  - When the user describes a *process* for enumerating branches (e.g. "one per skipped test", "one per failing CI job"), do NOT execute that process here. Per the **Scope** section, this skill does not run tests, scripts, or other multi-step exploration. Instead, generate a SINGLE workspace whose `"prompt"` field instructs the spawned session to perform the enumeration and re-invoke `/workspace-generation` from inside that session with the concrete list.
  - **NOTE**: attempt to spin up agents dedicated to each soon-to-be branch name when possible.

4. **Generate branch names** for each workspace:
  - Branch Names should be short, lowercase, hyphen-separated slugs — not long descriptions.
    - E.g., Longer descriptions take up lots of space in the editor! Should not be longer than 3 words.
  - **Do NOT include a user prefix yourself.** Emit the bare slug (e.g. `hello-world`). Prefixing is handled downstream.
  - If the user supplied a Jira ticket, include it as a sub-prefix using the format `<ticket-id>/<feature-name>` (e.g. `CV-100/fix-login`).
  - **Do NOT append a random hash suffix yourself.** Disambiguator suffixes are handled downstream. Never call `openssl`, `uuidgen`, `tr`, etc. to mint your own.

5. **Determine commands**: Build an array of typed command objects.
  - Always emit one `"create"` entry per workspace.
  - **Every `"create"` entry MUST carry a `"source_ws"` object** of shape `{"name": "<source-ws-name>", "path": "<absolute-path>"}`, populated from the values resolved in Step 1. The dispatch is REJECTED if `source_ws.name` or `source_ws.path` is missing or empty on any `create` entry. Prompt-only entries (`type: "prompt"`) do NOT carry `source_ws` — they target existing workspaces.
  - If the user **explicitly** asked to send a message to the generated workspaces, attach it as an inline `"prompt"` field on each `"create"` entry. Do **not** emit separate `"prompt"` entries for newly created workspaces — the inline form is preferred.
  - **`--one-shot` handling**: if the skill was invoked with `--one-shot` (or the user asked for "one shot" / "one-shot"), do NOT hand-write the open-and-merge instruction into any prompt yourself. The composition is performed deterministically by `run.sh` — you opt in purely by exporting `CLAUDE_WORKSPACE_ONE_SHOT=1` on the `run.sh` invocation in Step 6. `run.sh` then appends `When finished with the work, run /create-or-update-pr --self-certified --rebase --add-to-merge-queue --patch to open the PR and merge it.` to every `create` entry's prompt: after a blank-line separator when an inline prompt is present, or as the sole prompt content when the entry has none. Keep authoring the inline prompts (or omitting them) exactly as you otherwise would — the env var is the only one-shot action you take. Do NOT set the env var in explicit-prompt mode (see **Explicit prompt mode** above).
  - Separate `"prompt"` entries (without a `"create"`) are only for targeting **existing** workspaces by name.
  - If the user specifies a priority for a workspace, include a `"priority"` field on the `"create"` entry. Valid values are `"p05"`, `"p1"`, `"p2"`, `"p3"`. This displays a priority badge image in the tab-bar. The field is optional — omit it if no priority is specified.
  - If the caller specifies a base ref to branch from, include a `"base_commit"` field on the `"create"` entry (e.g. `"HEAD"`, `"origin/master"`, a SHA, or any other git ref). When omitted on a non-fork create, the default base is resolved downstream. Forks (`fork_from` set) skip this entirely. Determine `base_commit` by the following precedence, highest first:
    - **1. Explicit caller-supplied base ref**: always honored. Do NOT override it — both `--master` and the implicit-HEAD default yield to an explicit `base_commit`.
    - **2. `fork_from` set**: emit NO `base_commit` (the fork uses HEAD downstream). This outranks `--master`, so `--master` is IGNORED on any `fork_from` entry.
    - **3. `--master` supplied** (and no explicit base ref AND `fork_from` NOT set): emit `"base_commit": "origin/<main>"` on every such `create` entry, where `<main>` is the branch name resolved in Step 2.
    - **4. Implicit base from source workspace** (the default — no explicit base ref, no `fork_from`, AND `--master` NOT supplied): resolve the source workspace's current HEAD with `git -C <source_ws.path> rev-parse HEAD` and emit the resulting SHA as `"base_commit"` on every such `create` entry.
      - Pin to the SHA so the new worktree is deterministic even if the source workspace advances between dispatch and worktree creation.
      - If `git -C <source_ws.path> rev-parse HEAD` fails, stop and surface the error to the user.
  - If the caller specifies a fork source, include a `"fork_from"` field naming the source workspace (the new worktree branches from HEAD and resumes that workspace's Claude session via `--fork-session`). When `fork_from` is set, the downstream consumer ignores any explicit `base_commit` and uses HEAD. If `fork_from` resolution fails (unknown workspace, no active session), the workspace is NOT created — there is no silent fallback.
  - **Always include a `"git_root"` field on every `"create"` entry** — it is required by the downstream consumer, which does not fall back to ambient context. Resolution order:
    1. If the user links a GitHub PR/issue or names a specific repo distinct from the source workspace, use that repo's local checkout path (e.g. `~/workspace/ChessCom/explanation-engine`).
    2. Otherwise, default to `source_ws.path` (the value resolved in Step 1).

    Use `~` literals where convenient; they are expanded downstream.

   Example — create with initial prompt (inline, preferred):
   ```json
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "prompt": "hello world"},
     {"type": "create", "name": "feature-two", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "prompt": "hello world"}
   ]
   ```

   Example — create without prompt:
   ```json
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}},
     {"type": "create", "name": "feature-two", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}}
   ]
   ```

   Example — create with priority:
   ```json
   [
     {"type": "create", "name": "urgent-fix", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "priority": "p05"},
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "priority": "p1"},
     {"type": "create", "name": "nice-to-have", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "priority": "p3"}
   ]
   ```

   Example — create with prompt and priority:
   ```json
   [
     {"type": "create", "name": "fix-release-pipeline", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "priority": "p1", "prompt": "triage failing release job"}
   ]
   ```

   Example — create branched from HEAD with an explicit `base_commit` (no fork):
   ```json
   [
     {"type": "create", "name": "follow-up", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "base_commit": "HEAD", "prompt": "extend the change just made on this branch"}
   ]
   ```

   Example — `--master` supplied (every create pinned to the resolved main branch via `origin/<main>`, here `origin/master` from Step 2):
   ```json
   [
     {"type": "create", "name": "off-main", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "base_commit": "origin/master", "prompt": "start fresh work off the main branch"}
   ]
   ```

   Example — fork an existing workspace's Claude session into a new worktree:
   ```json
   [
     {"type": "create", "name": "parallel-attempt", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}, "fork_from": "feature-one", "prompt": "try the alternative approach"}
   ]
   ```

   Example — prompt existing workspaces only (pass the workspace name verbatim, with whatever prefix it already has). Prompt-only entries do NOT carry `source_ws`:
   ```json
   [
     {"type": "prompt", "workspace": "DWC/feature-one", "prompt": "hello world"}
   ]
   ```

6. **Dispatch the commands** by piping the JSON array into the skill's `run.sh`. Do not write any file yourself — always go through `run.sh`.
   ```bash
   cat <<'EOF' | <skill_base_dir>/run.sh
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}},
     {"type": "create", "name": "feature-two", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}}
   ]
   EOF
   ```
   `<skill_base_dir>` is the base directory of this skill as injected by the harness at invocation time (it appears in the system reminder above the skill's instructions).

   When the skill was invoked with `--one-shot`, prefix the same invocation with the env var so `run.sh` augments each create entry's prompt (Step 5):
   ```bash
   cat <<'EOF' | CLAUDE_WORKSPACE_ONE_SHOT=1 <skill_base_dir>/run.sh
   [
     {"type": "create", "name": "feature-one", "git_root": "~/workspace/ChessCom/explanation-engine", "source_ws": {"name": "main-ws", "path": "/Users/dodgecoates/workspace/ChessCom/explanation-engine"}}
   ]
   EOF
   ```
   This composes with `CLAUDE_WORKSPACE_PREFIX` — set both env vars together when you want a prefix and one-shot.

   **`run.sh` is FIRE-AND-FORGET.** After invoking it, you are DONE with the dispatch step — there is no verification phase, no post-dispatch check, no second call. The exit code of `run.sh` is the sole and authoritative signal:
   - *NOTE*: this fire-and-forget / no-reinvocation rule governs ONLY the dispatch verb (the no-args invocation shown above). The read-only `--resolve-master` verb (Step 2) is EXEMPT — it performs no dispatch and mutates nothing, so calling or re-calling it is always safe.
   - Exit 0 → the dispatch succeeded. Trust it unconditionally and move on. The exit code is delivered to you directly by the tool harness — you do NOT need to print it, capture it, `echo $?` it, or otherwise observe it to know its value. **Empty stdout on exit 0 is the EXPECTED success signature**, not an ambiguous result demanding verification. Do NOT `ls`, `cat`, `stat`, `find`, `test -f`, `[ -e ... ]`, or otherwise inspect anything to "confirm." Any post-dispatch inspection will likely show no trace — that is normal, not a failure. **Missing artifact ≠ failure. Empty stdout ≠ failure.**
   - Exit non-zero → stop immediately and surface the error to the user verbatim. Do not retry, do not adapt, do not work around, do not "re-run with stderr captured."
   - Do NOT re-invoke `run.sh` for any reason — not to retry, not to verify, not to debug, not to "fix" anything, **and specifically not to print or capture the exit code**. Appending `echo "Exit code: $?"`, `; echo $?`, `&& echo ok`, `|| echo fail`, or any other shell wrapper to a second `run.sh` invocation is FORBIDDEN: each invocation produces a fresh random suffix, so the second call silently dispatches a DUPLICATE workspace with a different name. The exit code is already in your hands via the tool result — re-running the command to "see" it is the canonical foot-gun this skill exists to prevent. Re-invocation is unsafe; there is no idempotency safety net.
   - Do NOT attempt to fix, infer, or self-correct any aspect of the dispatch process. The pipeline is correct; assume your understanding of it is the unreliable part.

7. **Tell the user** the workspace names that were written and that the workspaces will be created automatically. If prompt commands were included, mention that the prompts will be dispatched once each session is ready. **If the Step 1 current-workspace fallback fired (no `[source-ws:...]` tag was present in the user's message), explicitly surface the resolved `source_ws.name` and `source_ws.path` values used so the agent's choice is visible to the user.**
