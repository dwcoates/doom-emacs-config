<!-- BEGIN CURRENT HANDOFF STATUS -->
# CURRENT HANDOFF STATUS

- The test-perf-2 work is complete and green at `a03aa1a8963285429c8cb4fd7b008f1fcf28d957`.
- `master` is at `fb8dc146b` (one owner-requested commit on top of `86170d65`, tagged `temp-drop-merge-queue`; leave it in place) and contains none of the test-perf-2 work.
- The prior manual cherry-pick attempt was aborted, and its dedicated master-integration worktree was removed.
- Two merge-queue attempts were quarantined; the latest command is `workspace_commands_80d3dd30a99da3003b8f41088bc76e80.json`.
- test-perf-2 has prerequisite history beginning at `6a3203885`, not merely the 17 commits starting at `e60ebea7f`; the next owner must land the entire intended branch history safely.
- Full test result: 184.8s wall, 1094.4s aggregate CPU, 86% utilization, 256 units; runnable suites were green. `e2e-emacs` was declined because Docker was unavailable.
- After the tests land, all remaining feature/TODO work from the verbatim prescription remains unimplemented, including desktop notifications, spellcheck, and ship-gns stale live work.

<!-- END CURRENT HANDOFF STATUS -->

# ADDITIONAL OWNER INSTRUCTIONS (2026-10-02 session)

These were given after the verbatim instructions below and override them where they differ.

- Start only after the owner's usage limits reset at 4am on 2026-10-02, or when the owner says to start.
- The verbatim prescription was first given to a different agent, which did badly. It never committed on master, but it did the test-perf-2 work, so the implementation on that branch may be suspect.
  - Review its commits against `modules/app/agent-repl/docs/investigations/2026-10-01-test-parallelism-handoff-plan.md`.
  - Rerun `modules/app/agent-repl/bin/test-all.sh` yourself before landing it, rather than relying on its reported green run.
- Land the work on master by cherry-picking directly onto master, as the verbatim instructions say. This applies to all of it, including test-perf-2 (its whole history from `6a3203885`), even though the quoted test-perf-2 prompt below says otherwise.
- `~/.config/doom` (the main checkout) is checked out on `master`, so commits cherry-picked there land on master.
  - Subagents work in their own git worktrees, never in the main checkout.
- Model mapping:
  - "opus" means the `opus-medium` subagent type (Opus 5.5).
  - "sonnet" means the `sonnet-medium` subagent type.
# Verbatim session instructions

===== OPENING REPOSITORY INSTRUCTIONS =====

# AGENTS.md instructions for /Users/dodgecoates/.config/doom

<INSTRUCTIONS>
# Agents 

Always explain at the end of your response if there were changes whether or not the changes are persistent after unloading then. That is, whether resetting them and the re-loading will undo all the runtime changes. For example, if your changes add to some hook, then resetting your changes and reloading will not un-add that thing to the hook -- i'll still be there -- and thus the changes are "persistent after unloading"

Begin the persistence message with ✅ if the changes are not persistent after reloading (i.e. a reload fully resets them), or ❌ if any changes persist after reloading. When using ❌, briefly summarize which specific changes persist and why (e.g. "file written to disk", "entry added to hook").

## Related repo: explanation-engine (CEE) — chess.com work

- **Location**: `~/workspace/ChessCom/explanation-engine`, with worktrees under `~/workspace/ChessCom/explanation-engine-worktrees/`.
- **What it is**: the ChessCom Chess Explanation Engine ("CEE"), a chess.com work repo — the chess analysis/explanation backend (`services/` engine services, `sdks/lang/go/` Go SDK, the embeddable `apps/cee-web-widget/`, and the source of many agent-repl skills in its `.claude/skills/`).
- **Workspaces**: you may be asked to (or may want to) generate workspaces for it via the workspace-generation skill; its generated worktrees land under `explanation-engine-worktrees/`.

## There is NO such thing as a "pre-existing" error

**Under NO circumstances — ever, in any universe — does the concept of a "pre-existing error" exist in this repository. ALL errors need fixing.**

- An error encountered while working here is an error to fix, full stop. Provenance carries ZERO exculpatory weight: "already broken on the base branch", "not introduced by my change", "pre-dates this work", "artifact of the environment", and every similar framing are all invalid reasons to leave anything red.
- This covers every kind of error without exception: test failures, build breaks, byte-compile errors, lint errors, runtime bugs, tooling errors — anything that reports as an error while you work.
- Fix errors in the same body of work that surfaced them. An error unrelated to the main change gets its own commit; it never gets a mention-and-move-on.
- The ONLY alternative to fixing an error outright is an explicit escalation to the user, and only when the correct fix genuinely hinges on a decision that is the user's to make. Silently tolerating an error — with or without a provenance excuse — is never an option.
- Investigating when an error was introduced is fine as a debugging aid (it pinpoints the culprit commit); using the answer as a reason not to fix is not.

## Claude REPL

Before starting any work under `modules/app/agent-repl/`, read and follow the complete metaprompt at `<current-repository-root>/modules/app/agent-repl/metaprompt.md` (full repository-relative path: `modules/app/agent-repl/metaprompt.md`). Always resolve it from the current worktree's repository root so the instructions match the revision being changed.

### HARD GATE: never edit the top-level doomdir `config.el` without explicit permission

**Top-level `config.el` (i.e. `$DOOMDIR/config.el`, the file at the repo root) is off-limits to agents by default.** Before making ANY edit to that file — even a one-character edit, even refactoring an existing block, even "while you're already in there" — STOP and ask the user for explicit permission, in this turn, for this specific edit. Implicit permission (e.g. a previous session, a related task, a generic "fix this bug") does not count. Permission must be explicit and per-edit.

Why this is a hard gate rather than a soft preference:

- Workspace-merge reload (the `M-x doom/reload`-equivalent fired by the merge sentinel) reloads `modules/app/agent-repl/*.el` but does **not** re-evaluate the top-level `config.el`.
  - Top-level edits silently fail to take effect in the running Emacs until the user manually restarts or does a full reload.
  - The agent and the user then waste real time debugging "why didn't my fix land" when the fix is correct but unloaded.
- Top-level `config.el` is the user's personal scope (`+dwc/...`).
  - Anything agent-repl-related that lives there is almost certainly mis-placed and should be inside the module.
  - Editing it conflates user-scope and package-scope, which makes future extraction harder.

If you find yourself about to edit top-level `config.el` for a agent-repl-related concern (anything touching `tab-bar`, `persp-mode`, `+workspace-*`, `safe-persp-name`, claude faces, claude commands, claude keybindings, claude hooks, etc.), STOP and:

1. Do not edit `config.el`.
2. Surface the situation to the user in your next message. Include:
   - Exactly what change you were about to make and on which lines.
   - Why the change wants to live in `config.el` (e.g. it reads a `+dwc/` toggle, it depends on a `(after! persp-mode ...)` block, it sets `tab-bar-format`, etc.).
   - A concrete proposal for extracting the existing surrounding code into `modules/app/agent-repl/` so the new change can live in the module too.
   - A brief justification: why extraction is the right move, what it unblocks, and what the risk is of leaving it in `config.el`.
3. Wait for the user to decide between: (a) approve the top-level edit anyway, (b) approve the extraction, (c) something else.

**Never put new agent-repl code in the top-level doomdir `config.el`.** All agent-repl code — defuns, advice, hooks, keybindings, magit integration — lives under `modules/app/agent-repl/lisp/*.el`. The top-level `config.el` is not reloaded by `M-x doom/reload-lisp-config` the same way the module is, and instrumentation added there routinely fails to take effect, wasting debugging cycles.

When adding a new concern:

1. Pick the right sub-file (`core.el`, `panels.el`, `status.el`, `session.el`, `sentinel.el`, `worktree.el`, `input.el`, `keybindings.el`, `magit.el`, etc.) or create a new one.
2. Put it in `modules/app/agent-repl/lisp/` and register it in `modules/app/agent-repl/config.el` via `(agent-repl--load-module "NAME")` — `config.el` stays at the module root (Doom resolves it there) and loads each name as `lisp/NAME`.
3. If the feature bridges agent-repl with another package (e.g. magit), put it in a dedicated integration file like `magit.el` rather than in the doomdir `config.el` under `(after! PACKAGE ...)`.
4. Leader-key bindings that reference a `agent-repl-*` symbol (e.g. overriding `SPC p p` to `agent-repl-switch-to-project`) belong in `keybindings.el`, not in the doomdir `config.el` — even when they shadow a pre-existing `+dwc/` binding there.

Naming: internals use `agent-repl--` prefix, public entry points use `agent-repl-` prefix. User-facing commands triggered by leader keybindings may keep the `+dwc/` prefix when they were moved out of the doomdir `config.el` and remain user-scope entry points.

**Never add `+dwc/` functions or variables inside the agent-repl module.** If a feature needs state that currently lives in `config.el` (e.g. `+dwc/workspace-history`), define a `agent-repl--` equivalent inside the module and wire it up there. The `+dwc/` namespace belongs to the doomdir config layer; the module must be self-contained and not introduce new `+dwc/` symbols.

## Workspace state encapsulation — go through `workspace.el`

The `agent-repl--workspaces` hash table is owned by `modules/app/agent-repl/lisp/workspace.el`. **All new code touching workspace state must go through `workspace.el`'s wrapper API**: `agent-repl--ws-get`, `agent-repl--ws-put`, `agent-repl--ws-del`, `agent-repl--ws-live-p`, `agent-repl--ws-known-p`, `agent-repl--ws-tombstoned-p`, `agent-repl--ws-open-p`, `agent-repl--ws-render-status`, `agent-repl--live-ws-names`, `agent-repl--ws-require-known`.

**Forbidden in any file other than `workspace.el`:** direct `gethash`, `puthash`, `maphash`, `hash-table-keys`, `hash-table-count`, `remhash`, or `clrhash` against `agent-repl--workspaces`. Doc-strings and `:argument` references to the symbol name are fine; runtime hash ops are not.

Two exceptions are grandfathered inside `core.el` itself: `agent-repl--ws-id-cached` and `agent-repl--format-ws-metadata` directly read the hash because they are logging primitives that `--ws-put` calls on stub-create — routing them through the wrapper would create a logging-to-workspace cycle. These exceptions live inside the encapsulation boundary, not outside it; do not add new ones.

**Render-state unification:** every renderer (tab-bar composed-state via `--ws-display-state`/`--ws-bracket-state`, project picker emoji) MUST call `agent-repl--ws-render-status` to determine what state to display. Never re-derive status from `:claude-state` / `:repl-state` / `:merging` / `:merge-completed` in a renderer — that's what the unification eliminated. New visual states added to the system go in `--ws-render-status`'s `cond` (with a documented precedence comment) and in `agent-repl-ws-state-icons`; renderers automatically pick them up.

Per-diff audit (mandatory on every diff touching `modules/app/agent-repl/`): grep the diff for `(gethash` / `(puthash` / `(maphash` against `agent-repl--workspaces` in any file other than `workspace.el`. If you find one, extract a wrapper into `workspace.el` first; only then write the calling logic.

## NEVER manipulate third-party internals from a high-level layer

**ABSOLUTE RULE: a high-level layer in `modules/app/agent-repl/` must never touch a third-party dependency's internals directly. Every third-party API call must go through a dedicated wrapper module that owns the integration boundary.** "Third-party" here means anything outside `modules/app/agent-repl/` itself — external Emacs packages, Doom helpers, language-tool integrations, shell programs, etc.

The shape is always the same, regardless of which dependency is in play:

- One agent-repl file owns the integration boundary for a given dependency.
- Every other agent-repl file expresses its intent in terms of agent-repl semantics (verbs like "register workspace", "render this status", "tag this commit") and routes through the owning file's wrapper API.
- The dependency-specific bookkeeping (data-structure shape, naming conventions, undocumented-behavior workarounds, version skew) lives inside the owning file, never sprinkled across the high-level layer.

If a high-level call site needs a behavior the owning file does not yet expose, STOP and surface to the user with:

1. The exact third-party API the code wants to call.
2. The call site (file:line) that would call it.
3. A proposed wrapper name + signature in the owning file, or the owning file itself if no owner exists yet.
4. A note on whether any existing wrapper looks close enough to extend.

Wait for the user to decide whether to extend an existing wrapper, add a new one, or take a different approach. Do NOT silently sprinkle the third-party call at the high-level site, even "just this once".

Why this is absolute:

- Direct third-party calls at high-level sites are abstraction leaks. High-level code is supposed to express agent-repl semantics, not the dependency's internal bookkeeping.

- Leaks create load-bearing dependencies on a third party's undocumented or version-specific behavior. The fix for any one such quirk then has to be applied at N call sites instead of one wrapper.

- They also pin agent-repl to the current dependency choice. A future swap (different workspace backend, different git frontend, different terminal emulator) touches N files instead of one.

- They defeat the testing model. The wrappers ARE the mock boundary — without them, every call site has to be mocked independently, and tests of high-level logic end up encoding dependency-specific behavior that drifts when the dependency upgrades.

Examples of integration boundaries already established or in progress:

- Persp-mode / Doom-workspace API (`persp-names-cache`, `persp-add-new`, `+workspace/new`, `+workspace/kill`, `+workspace-list-names`, etc.) → owned by `workspace.el`.
- Shell / subprocess invocations (`git`, `gh`, `claude -p`, etc.) → owned via the `agent-repl--<resource>-<verb>` wrapper convention documented under "No External Processes or External State in Tests".
- (Future) magit, vterm, projectile integration points should follow the same pattern when they grow non-trivial bookkeeping.

Pre-existing call sites are grandfathered until they migrate. New ones are not, and migration of the existing ones is in progress (see commit history for the current wave).

## Claude REPL instrumentation

New code added to the agent-repl module must include instrumentation via `agent-repl--log`. Every dynamic aspect of the call site must be included in the log message — variable values, resolved paths, computed flags, branch outcomes, etc. The goal is that a log trace alone should be sufficient to diagnose any behavioral issue without needing to add instrumentation after the fact.

**Always thread WS through `agent-repl--log` / `agent-repl--log-verbose`.** If a workspace is in lexical scope, or derivable via `(+workspace-current-name)` or the buffer-local `agent-repl--owning-workspace`, pass it as the first argument. `nil` WS omits the canonical workspace and session identity fields that disambiguate identical records across workspaces. Only pass `nil` when the call site is genuinely workspace-agnostic (load-time sentinel init, workspace-resolving helpers, pure file/directory utilities). If threading WS into a function changes its signature, do it — add an optional `ws` parameter dedicated to diagnostics if purity needs to be preserved otherwise.

**Diagnostic output must land in the canonical logfile, never in a dedicated buffer as the sole record.** Route subprocess stdout/stderr, captured shell output, resolver responses, and any other instrumentation through `agent-repl--log` / `agent-repl--log-verbose` so it inherits the canonical structured identity fields and survives session restarts. Side buffers are not durable or available to the log-discovery workflow, and they require a human to already know the buffer name to inspect them. A side buffer is acceptable *in addition* for live interactive inspection — never *instead of* a log entry. If you find yourself writing `(get-buffer-create "*agent-repl-...*")` to hold diagnostics, stop and log instead.

**Every bug fix MUST add instrumentation.** A bug that reached production is itself proof the existing logging could not pinpoint it, so any fix must leave behind the `agent-repl--log` coverage (decision inputs, computed flags, branch taken, error captured) that would have isolated the root cause from a trace alone — never fix a bug without also closing the instrumentation gap it exposed.

**Instrument every new or changed code path.** Skip only for "extremely hot" paths — code that fires more than ~once per second across all workspaces (per-keystroke handlers, per-timer-tick callbacks, redisplay hooks, char-output filters) where the file-write itself would multiply log volume and bury other events. For merely-frequent-but-load-bearing paths, use `agent-repl--log-verbose` (echo-gated, always file-written) instead of omitting the log. Default to logging; "no log" is the harder choice and warrants a one-line comment naming the firing frequency and why instrumentation would be counterproductive.

## Skill symlinks: always target the MAIN worktree, never a linked one

The skill installer (`.claude/install.sh` + `modules/app/agent-repl/skills-cache/manifest.sh`) symlinks each managed skill into `~/.claude/skills/`. **Every symlink MUST point into the MAIN worktree of its repo, never a transient linked worktree.**

- A linked worktree (anything under `~/.config/doom-worktrees/*`, agent worktrees, etc.) is ephemeral — its path dangles the moment the worktree is pruned, leaving a broken skill symlink.
- `install.sh` resolves the main worktree via `git worktree list --porcelain` (its first `worktree` entry) and sources repo-local skills from there, regardless of which worktree the script is invoked from.
- `install.sh` **fails hard** (`_impl_in_nonmain_worktree`) when any manifest impl path resolves inside a non-main worktree. Do not add such an entry to `manifest.sh`.
- A skill with no home in the main worktree is NOT eligible to be managed. Re-home it (commit it into `modules/app/agent-repl/skills/` for repo-local skills, or its repo's main checkout for external skills) before adding it back.

The workspace-command skills (`workspace-merge`, `workspace-status`, `workspace-update`, `generate-workspace`) were folded into the single `/workspace` skill — manage `workspace`, not the removed per-command names.

## Debugging skills: when to use each

The complete agent-repl investigation workflow lives in
`<current-repository-root>/modules/app/agent-repl/skills/debug-emacs-agent-repl/SKILL.md`.
Invoke `/debug-emacs-agent-repl` immediately for
any question about current or historical editor, workspace, daemon, shim,
webapp, sidecar, store, state, deployment, logging, or coverage behavior. The
skill owns evidence routing, health and readiness checks, identity correlation,
SQL investigation, log discovery, and telemetry-deficiency review. Do not
duplicate its paths or procedures here and do not replace them with ad hoc
`grep`, `cat`, `sqlite3`, or `emacsclient` probes.

Use `/runtime-eval-code` when the debug skill routes the investigation to live
Emacs state or third-party `*Messages*` output. Use `/profile` for time-boxed
performance capture and `/workspace-profile` when the user needs manual
profiler control.

## No Silent Fallbacks — Fail Hard on Invariant Violations

**ABSOLUTE RULE: Do not introduce ANY "fallback" behavior.** Under no circumstances — without **explicit, per-case permission from the user**, and only when the fallback is *absolutely* necessary — may code fall back to an alternative value, default, or code path when the primary input/lookup/precondition is missing or fails. **Always** prefer a loud error and a hard failure. Assume the answer is "no fallback" and propose the failure mode to the user; wait for explicit approval before writing any fallback. Do not suggest a fallback unless asked, and do not smuggle one in under names like "default", "graceful degradation", "sensible behavior when …", or "keep existing callers working".

**Never silently fall back, skip, or no-op when a precondition fails.** If an invariant is violated, **immediately fail loudly** with a `agent-repl--log` entry AND user-visible feedback (`user-error`, `error`, or at minimum a `message` that reaches the echo area). **Never fall back to an alternative code path** — the operation must abort entirely.

**Do not commit state changes before the failure point.** If an operation involves multiple steps (e.g., resolve session ID, then create worktree), validate all preconditions before mutating any state. If validation fails partway through, no workspace should be created, no timers scheduled, no hash table entries written. The system state must remain unchanged on failure. Always commit concrete changes when finished. Commit frequently.

Silent fallbacks create mysterious "stuck" states (the slash-passthrough bug was one — vterm lookup failed silently, keystrokes piled onto a hidden stack, the user saw no effect and had no signal that anything was wrong). A loud failure surfaces itself and can be diagnosed; a silent fallback just leaves the user guessing. A "degraded but working" fallback is even worse — the user sees *something* happen, assumes it worked, and only discovers the problem much later (e.g., a workspace branched from origin/master instead of the requested fork source).

Anti-patterns to reject:
- `(when-let ((x (lookup))) BODY)` where BODY is user-expected behavior. If `lookup` returns nil, the caller needs to know.
- State mutations that run regardless of whether the upstream operation succeeded (e.g., pushing onto a local stack after a failed forward).
- `(ignore-errors ...)` without a companion log of what was swallowed.
- `or`-chained defaults that mask missing data: `(or (ws-get :vterm) (default-vterm))`.
- Early returns that hide a failed precondition instead of signaling it.
- Returning nil from a resolution function and letting the caller silently degrade to a default (e.g., resolving a fork source to nil and falling back to origin/master).
- `if`/`cond` branches that pick an ambient value when an explicit input is absent (e.g., "prefer cmd arg X; else resolve X from current buffer/workspace"). If X is required, demand X — do not synthesize it.
- Comments or commit messages that contain the phrases "fall back", "falls back", "fallback", "default to", or "for backwards compatibility" describing runtime behavior. Treat these as review blockers.

The only acceptable silent no-op is one whose contract **explicitly requests** it: a best-effort cleanup where failure is known to be recoverable, or a `lookup-or-nil`-style query function. In those cases, document the contract in the docstring so callers know what they're getting. Even then, prefer an explicit error over a silent no-op unless the user has signed off on the recoverable-failure semantics.

When in doubt: fail loudly. When a precondition fails: abort entirely. When tempted to write a fallback: stop, surface the situation to the user, and wait for explicit authorization.

## Testing

After any changes to `modules/app/agent-repl/`, run the single full local
verification entry point:

```bash
modules/app/agent-repl/bin/test-all.sh
```

It runs the shell harnesses, the full ERT safety wrapper, every authored Go
module with statement coverage, both TypeScript projects with statement and
branch coverage, and proto validation. It reports wall-clock duration for each
suite. Every suite must pass.

`--suites a,b,c` narrows the run to a subset, and an unknown name is a hard
error rather than a silently empty run. It exists for the MERGE GATE, which
derives the subset from the paths a merge touches
(`daemon/internal/workspace/merge/suiteselect.go`) so a webapp-only merge is not
denied by a suite that shares no code with it. Local verification should still
be argument-less: a human does not always know what their change can reach, and
an unmapped path is exactly the case the gate answers by running everything.

The ERT safety wrapper remains available for a focused selector:

```bash
.claude/safe-test-run.sh "^foo"
```

The wrapper drops a checkpoint tag at `HEAD` before invoking ert, then diffs
the pre/post git state (HEAD, all refs/heads, all refs/tags, working-tree
status). On any drift the checkpoint tag is **preserved** and the script
prints the exact `git reset --hard <tag>` command to roll back; on a clean run
the tag is auto-removed. Exit codes: `0` clean, `1` test failures, `2` drift
detected, `3` environment error.

### Canonical test timing history

`modules/app/agent-repl/test_time.csv` is the canonical per-suite timing
history. Normal feature-branch verification must not mutate it. After a commit
lands on `master`, run:

```bash
modules/app/agent-repl/bin/test-all.sh --record
```

The command appends timings only after every suite passes and only if the
branch and commit remain unchanged throughout the run. Commit the resulting
`test_time.csv` update as a follow-up timing record.

Review the command's regression report and the newest CSV rows before
declaring the master work complete. A big regression means the current suite
is both at least 25% slower and at least 1.0 second slower than the average of
its five most recent entries on the same branch. The script prints
`TIMING REGRESSION` for that condition once at least three prior entries exist.
Surface every such regression to the user with the current duration, recent
average, percentage increase, and absolute increase. Investigate and fix it
unless the correct resolution requires a user decision.

You may invoke ert directly if you have a specific reason:

```bash
emacs -batch -Q -l ert -l modules/app/agent-repl/lisp/test-agent-repl.el -f ert-run-tests-batch-and-exit
```

A repo-checked-in pre-commit hook enforces this automatically: when agent-repl
Elisp, Go, TypeScript, proto, or local verification infrastructure is staged,
the unified `modules/app/agent-repl/bin/test-all.sh` suite runs without
recording timing history, and any failure blocks the commit. Direct commits on
`master` are gated too; only automated cherry-pick replay is skipped because
the source commit was already gated. The hook lives at `.githooks/pre-commit`.
Install once per clone:

```bash
git config core.hooksPath .githooks
```

Never use `git commit --no-verify` to bypass it; fix the failures instead.

**Zero tolerance for test failures.** Every test failure is a real bug that must be fixed before your work is done. **There is NO such thing as a "pre-existing" failure — not under ANY circumstances, EVER.** Do not investigate whether a failure predates your work. Do not check git history. Do not stash, checkout, or touch git state to "verify" it was already broken. Do not rationalize, dismiss, categorize, defer, or explain away any test failure for any reason whatsoever. If a test fails, fix it. Every failing test is your responsibility the moment you observe it. Never report work as complete while any test is failing.

## No External Processes or External State in Tests

**ABSOLUTE RULE: Tests must NEVER invoke an external process and must NEVER mutate any external state.** This includes — but is not limited to — `git`, `gh`, `curl`, `claude`, `pbcopy`, `pbpaste`, `osascript`, `xcrun`, `find`, `xargs`, `ssh`, `make`, `npm`, any user-installed binary, the system clipboard, the desktop notification system, environment variables outside the test's own dynamic let-binding, and any filesystem path outside `temporary-file-directory`. **Tests are pure elisp.** No subprocess. No `call-process`, `start-process`, `process-file`, `shell-command`, `shell-command-to-string`, `make-process`, `async-shell-command`, `vterm`, `eshell-command`. No `write-region` or `make-directory` outside of `temporary-file-directory`, and even those should be a last resort — prefer in-memory state.

**Why:**

- Tests that shell out are slow, flaky, and platform-dependent.

- Tests that mutate external state (e.g., creating real git branches, writing real files) pollute the developer's machine and other tests.
  - A real recovered incident: a test suite that ran `git -C $TEMP init` and `git -C $TEMP commit` somehow ended up writing branches like `branch-a`, `branch-b`, `feature-x`, `trunk`, `child`, `bad-1` into the developer's actual repo's `.git/refs/heads/`, leaving the worktree in a half-cherry-picked state.
  - The root cause is fundamentally that the tests were invoking real `git` at all — once the boundary is crossed, the blast radius is impossible to bound by inspection.

- Tests that depend on installed binaries fail differently on CI, in containers, on a coworker's machine, and after a tool upgrade.

- Mocked tests run in milliseconds, are deterministic, and document the exact contract between production code and the external boundary.

**Required pattern:**

1. Every external-process or external-state call in production code is wrapped by a dedicated single-purpose elisp function.
  - The wrapper does ONE thing: invoke the external call.
  - The wrapper does NOT contain conditional logic, parsing, retries, formatting, or any other business logic — that belongs in callers that the test exercises directly without mocking.

2. Tests stub the wrapper via `cl-letf` (or equivalent) and supply fixture return values.
  - Tests assert against the production lisp behavior, NOT the external system's behavior.
  - The test never asserts "git created a branch"; it asserts "the production function, given this mocked git output, returned this value / called this other wrapper with these args".

3. **Wrapper naming convention:** `agent-repl--<resource>-<verb>[-<noun>]` where `<resource>` names the external thing the wrapper boundaries (`git`, `gh`, `curl`, `claude`, `pbcopy`, `clipboard`, `notify`, etc.).
  - The leading `agent-repl--<resource>-` prefix is the **signal** to a future contributor: "this is the external boundary; mock me in tests, do not let it run for real."
  - Existing examples: `agent-repl--git-string`, `agent-repl--git-string-quiet`, `agent-repl--git-exit-code`, `agent-repl--async-git` (these are the canonical reference and grandfathered).
  - For a new external, follow the same shape: `agent-repl--gh-string`, `agent-repl--curl-string`, `agent-repl--pbcopy-write`, `agent-repl--notify-send`, etc.

4. **No external call may exist in production code outside such a wrapper.** If you find a bare `(shell-command-to-string "git ...")` or `(call-process "gh" ...)` in production code, extract it into a wrapper first; only then write the calling logic.

5. **No external call may exist in test code at all.** Not even via a "test-only helper" macro. If a test helper macro currently shells out (e.g., a `with-temp-git-repo` macro that runs `git init`), it is an anti-pattern and must be replaced with a fixture-data approach that mocks the relevant `agent-repl--git-*` wrapper.

**Prohibited anti-patterns:**

- `(call-process "git" nil nil nil "-C" repo "init")` or any sibling thereof inside a test file or test helper.
- `(shell-command-to-string "git ...")` inside a test file.
- "Temp git repo" macros that build up state via real `git` invocations — even when scoped via `-C $TEMP`. The blast radius is bigger than it looks; see the incident note above.
- Tests that depend on the test runner's CWD being inside a git repo (e.g., `(agent-repl--git-string "rev-parse" "--show-toplevel")` without binding `default-directory`).
- Tests that mutate `~/.claude/` or any path under `$HOME` other than `temporary-file-directory`.
- "Integration tests" that opt out of this rule by running real external processes. There is no integration-test escape hatch in this codebase; if integration coverage is needed, it lives outside the ERT suite and is run manually, never by the pre-commit hook.

**When you encounter a test that violates this rule:** stop, refactor the production code to introduce the wrapper if it doesn't already exist, then rewrite the test to mock the wrapper. Do not add new tests that perpetuate the pattern.

### We test lisp, not external code

The corollary to "no external processes in tests" is: **tests exist to cover elisp behavior, not external program behavior.** If a candidate test would exclusively exercise a non-elisp artifact (a shell script, an installed binary's command-line surface, a remote service, etc.), it does NOT belong in the ERT suite.

Concretely: do not add a test whose body is "spawn the external thing, then assert on what it did." Mocking the external call in that situation reduces the test to asserting nothing of value (the entirety of the contract under test lives outside lisp). The right path is:

- If lisp dispatches to the external thing, test the lisp dispatch — mock the wrapper and assert the dispatch logic.

- If you genuinely want to cover the external thing's behavior, write that coverage as a separate runner (a `make test-install-bash` target, a CI job, a hand-run harness). It does not belong in the ERT batch invoked by the pre-commit hook.

- If the external thing has been hard to test for a long time, that is a signal to port its essential logic into elisp — not a license to add an integration test to the ERT suite.

This corollary is the policy reason `test-install.el` no longer ships its `bash install.sh`-spawning tests, and it is the reason no future revision of the suite should reintroduce them.

### How the rule is enforced

**The agent's per-diff audit is the SOLE primary enforcement.** There is no static lint, no regex grep, no commit-time backstop that catches raw external calls in production code. If you (the agent) miss extracting a wrapper, the failure mode is silent — a test reaches the raw `shell-command-to-string`/`call-process`/`start-process` call, real `git`/`gh`/etc. runs, no alarm fires.

This is deliberate. A backstop would create moral hazard: the agent learns to rely on it ("the lint will catch me") and the primary obligation erodes. The system instead trusts the agent to do its job correctly, and the cost of a miss is borne immediately by the resulting silent state pollution — which is exactly the failure class that motivated this policy in the first place.

**Required agent workflow on every diff that touches `modules/app/agent-repl/`:**

1. *
</INSTRUCTIONS><environment_context>
  <cwd>/Users/dodgecoates/.config/doom</cwd>
  <shell>zsh</shell>
  <current_date>2026-10-01</current_date>
  <timezone>America/New_York</timezone>
  <filesystem><workspace_roots><root>/Users/dodgecoates/.config/doom</root></workspace_roots><permission_profile type="managed"><file_system type="restricted"><entry access="read"><special>:root</special></entry><entry access="write"><path>/Users/dodgecoates/.config/doom</path></entry><entry access="write"><special>:slash_tmp</special></entry><entry access="write"><special>:tmpdir</special></entry><entry access="read"><path>/Users/dodgecoates/.config/doom/.git</path></entry><entry access="read"><path>/Users/dodgecoates/.config/doom/.agents</path></entry><entry access="read"><path>/Users/dodgecoates/.config/doom/.codex</path></entry></file_system></permission_profile></filesystem>
</environment_context>

===== ORCHESTRATION AND FEATURE REQUEST =====

for the following, and indeed, the rest of the session and in perpetuity, make all code implementation changes with opus, and, if necessary, rote changes or reading with sonnet. You can resolve all conflicts yourself, though. You are an orchestrator, and thus code writing and rote tasks should be done by other the other models mentioned, but you have good context for the changes being made holistically, and thus can do conflict resolution as you fold in subagents work. All changes should land directly on master as commits, but the first commit you make should be tagged with something that identifies it as the first commit done by you. prefer using medium effort for subagents as well. Also, prefer updating existing agents that are working on similar or related areas of code over spawning new agents within reason. 

let's make some agent-repl changes:

- we should remove the "quiet-stretch" webapp footer activity updates entirely: they are too chatty and add next to no information, so all activity updates falling intot his category should be simply remvoed. they are things like the tool calls being made or whatever right after they are made and before the next update in the feed lands. not useful and hsould be removed
- the usage (weekly+5hr) enduring footer activity blue separator "|" should have 3 spaces on either side of it (i believe i only has one currently) and shoudl be bold/a bit thicker
- the topbar "persistent-wifi-mode-toggle" should be blue circle background (when the persistent-wifi mode is on) should have a black backgroudn instead of blue. the green foreground for the wifi symbol should also be slightly brighter. also, it seems that the wifi symbol is VERY slightly too far right within that circle (like a pixel or too), and the circle itself (along with the wifi symbol) seems a bit closer to the token chip on the left than the warninig/error chip on the right, so it should be centered that way too. also, the icon itself in the topbar should be clickable, and clicking it should toggle the persistent-wifi-mode just as agent-repl-persistent-wifi-mode-toggle in emacs does (same codepath)
- sometimes, especially when under CPU load, switching workspaces with M-{ or M-} is glitchy, so an investigation into the potential reasons and fixes for that shoul be produced by some agent
- there should be an effort selector in the topbar, between the model and permission mode selectors, and of the same format (dropdown, font, sapcing, etc). It should have a little disclaimer in the dropdown that briefly says "will cause token cache misses". also, the existing model selector dropdown should hav ethe same disclaimer in the dropdown. actually, scratch that about being in the dropdown, it should be a tooltip visible on hover, along with a very brief explanation of what it does (changes model used by workspace for rest of sesssion; changes agent effort for rest of session in worksapce) 
- tab-bar entires in emacs workworkspaces should change from full->partial within one second instead of five iff you're viewing the workspace when the final reponse (green-obrder bubble) lands in the feed OR if the workspace is otherwise in background status (that is, it's "done" and thus green, but also in bakcground status, so when marked "viewed" (aka partial background) it will switch to yellow, and in that case, it's also instead on a one second timer)
- when emacs starts up, or when the daemon bounces while emacs is still live, workspaces should always be started. I notice sometimes the bounced workspaces have a totally blank feed (even though there was an extensive converstation there before bouncing) and i have to send a prompt, at which point the feed shows a blue "starting up" badge for a moment before rendering the feed. The feed (or rather, the most recent page of the feed) should be automatically rendered when the workspace bounce or its backend(s) bounce, always. there shoulndt even be a "starting up" badge at all, because it should be strucuturally required to have started up when the workspaces webpage was restarted or opened or whatever
- after compaction in the feed, the compaction summary bubble should be automatically opened. curently there's a "<chevron> Summary" which is closed by default, and i'm saying that chevron should be open by default, and indeed, there should eb no chevron at all, and no "<chevron> Summary"; the orange-border summary bubble should just be there visible under the orange compaction bar as though the chevron were opened. it should default as usual though to unexpanded bubble form (so as it does today when the chevron is opened for the Summary
- the spacing in the topbar between the chips/dropdown titles on the right hand corner (model selection, permission mode selection, fast mode selection, etc) should all be made a bit closer (say, 30% closer) and uniform (all righthand chips should hav ethe same spacing between them). also, it looks like the text for the dropdown titles on the right side of the toolbar seem to be slightly larger than the text for the login information on the left side of the topbar. if that's the case, the text on right rightside of the topbar (i mean in the actual topbar, like model name, not the text in the dropdowns themselves, but what's shown in the topbar. that's what i mean about the "dropdown titles"). if the text on the right isn't a bit bigger than on the left, then i'm just mistaken, and you can ignore that ibt (looks like it's slightly bigger to my eye, but i migth be mistaken. i think *definitely* the token count text there that's yellow is too big, at least). also, the topbar yellow token text should get the same gradiant logic (preferrably the same exact code) as what's used to color the token count in the footer *activity section* (NOT the footer itself, which is checking raw token values. we want to work with PERCENTAGE of the max contenxt window size, which hopefully we can derive programmatically from the vendor/SDK, but if not, we can assume to be 1 million tokens. so the percentage of 1 million tokens the value displayed there in the topbar should the color exactly as the percentage dictates the collor in the footer activity session for the enduring weekly/5hr usage percentages)
- switching away from a workspace and back to it should automatically instantly scroll to the very bottom of the feed (such that feed autoscroll is reactivated)
- when selecting text manually with the cursor in the webapp, two things must be true: all text must be supported, everywhere in the webapp (or any bubble or anything), and copying must work for both 'y' (vim style yank, should work already), ctrl+c and cmd+c
- the emacs input window for agent-repl (where you send prompts) should be one line shorter by default
- cleared and compacted workspaces should have their tab-bar/sidebar viewed status (full vs partial) be instantly updated to partial (rather than having to wait for 5 seconds or 1 seoncd in the sepcial case mentioned above)


if you have any quesations for me, ask them up front, because i would like for you to implement all changes without my further infvolvement after you actually begin dispatching. as for protobuf changes, you're free to make them, and first read /create-or-update-protobufs to get the conventions for protobuf changes, but to not follow the bit in that skill about asking me to approve or iterate on changes; you make the changes, and by "you" i mean literally you -- no subagents either should touch the protobufs, and if they feel the need to, they should ask you, and you should make them if you feel it's necessary, or tell them why it's not. only surface things to me if there's actual ambiguity you cannot resolve yourself, or if there are UI/UX changes that seem to eb implied or necessary that i havent myself already prescribed specifically.

===== EXTENDED SKILL CATALOG CORRECTION =====

the required protobuf skill can be found at ~/.claude/skills, and indeed, you sohuld bear in mind the existence of all skills there (dont read them NECESSARILY, but the directory and markdown description headers should be known to you in case you later determine they might be useful to read into context)

===== PRE-IMPLEMENTATION QUESTION GATE =====

before you bein any impleemntation work, please circle back to me with questions. i dont want you to start until you're sure you understand the problesm enough to begin work confidently

===== ANSWERS, WORKTREE RULE, AND CHERRY-PICK INSTRUCTION =====

QUESTIONS TO WHICH THE ANSWERS BELOW CORRESPOND

Response (👀 no code changes made)

1 ❓ Questions before implementation
├── 1.1 Should quiet-stretch activity be removed end to end
│   └── That means deleting its protobuf shape and daemon production as well as webapp rendering and hold logic
├── 1.2 Should the effort selector show `—` when the session has not reported a current effort
│   ├── Unsupported models would show `—` with no dropdown
│   └── Supported models would offer only their declared effort levels
├── 1.3 Should changing effort create a visible `/effort <level>` user row in the feed
│   └── If not then I will add a dedicated typed command path that changes session effort without fabricating a user message
├── 1.4 Should `y` copy only when the browser has a nonempty text selection
│   └── With no selection the webapp would leave `y` alone
├── 1.5 Should return-to-workspace scrolling apply to every switch path
│   └── That includes Emacs workspace cycling plus webapp sidebar selection
└── 1.6 Please confirm the viewed-state timing interpretation
    ├── Final response while currently viewing the workspace becomes partial after one second
    ├── Final response in a background workspace stays full until visited then becomes partial after one second
    └── Successful clear or compaction becomes partial immediately

2 🔎 Workspace-switch investigation
├── 2.1 A stale zero-delay activation timer can repaint and focus the prior workspace under CPU load
├── 2.2 Every switch also performs the departing frame snapshot twice
└── 2.3 The proposed structural fix coalesces activations by generation and removes the redundant snapshot

3 ⏸️ Current state
├── 3.1 All implementation agents are stopped
├── 3.2 No code or protobuf files were changed
└── 3.3 ❌ The new Git branch `astra-agent-repl-ui-lifecycle-20261001` persists on disk after reloading although the working files remain unchanged

THE ANSWERS TO THE QUESTION BY THE USER

1.1 - yes, e2e
1.2 - should show whatever the defauhlt is, programmatically determined by daemon at the given workspaces initialization by checking the canonical claude file location for that information
1.3 - no, should not create a visible row in feed
1.4 - yeah, only when selection is nonempty and the webapp is focused. same for C-c and Cmd-c
1.5 - only when switching between workspaces within emacs, but not when switching back to emacs (from another application) to view the workspace already open. doesn't amtter whehter its done via emacs workspace switching (there's a number of ways to do it) or by switching by clicking the sidebar. indeed, it should be an invariant handled in the shared protobuf RPC (if emacs and webapp share an rcp for switching worksapces, not sure if they do)
1.6 - yes, but your working is slightly weird. it's not the "final response" that becomes partial or stays full, it's the viewed representation in tab-bar entries and sidebar entries. but yes

2.1 - let's fix that
2.2 - should be once, right? 
2.3 - sounds great, lets do it

have all your subagent work done in git worktrees. as they resolve, you can cherry pick their commits ont master (not do any merges or anything), resolving conflicts along the way. 

