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

**Never put new agent-repl code in the top-level doomdir `config.el`.** All agent-repl code — defuns, advice, hooks, keybindings, magit integration — lives under `modules/app/agent-repl/*.el`. The top-level `config.el` is not reloaded by `M-x doom/reload-lisp-config` the same way the module is, and instrumentation added there routinely fails to take effect, wasting debugging cycles.

When adding a new concern:

1. Pick the right sub-file (`core.el`, `panels.el`, `status.el`, `session.el`, `sentinel.el`, `worktree.el`, `input.el`, `keybindings.el`, `magit.el`, etc.) or create a new one.
2. If creating a new file, register it in `modules/app/agent-repl/config.el` via `(agent-repl--load-module "NAME")`.
3. If the feature bridges agent-repl with another package (e.g. magit), put it in a dedicated integration file like `magit.el` rather than in the doomdir `config.el` under `(after! PACKAGE ...)`.
4. Leader-key bindings that reference a `agent-repl-*` symbol (e.g. overriding `SPC p p` to `agent-repl-switch-to-project`) belong in `keybindings.el`, not in the doomdir `config.el` — even when they shadow a pre-existing `+dwc/` binding there.

Naming: internals use `agent-repl--` prefix, public entry points use `agent-repl-` prefix. User-facing commands triggered by leader keybindings may keep the `+dwc/` prefix when they were moved out of the doomdir `config.el` and remain user-scope entry points.

**Never add `+dwc/` functions or variables inside the agent-repl module.** If a feature needs state that currently lives in `config.el` (e.g. `+dwc/workspace-history`), define a `agent-repl--` equivalent inside the module and wire it up there. The `+dwc/` namespace belongs to the doomdir config layer; the module must be self-contained and not introduce new `+dwc/` symbols.

## Workspace state encapsulation — go through `workspace.el`

The `agent-repl--workspaces` hash table is owned by `modules/app/agent-repl/workspace.el`. **All new code touching workspace state must go through `workspace.el`'s wrapper API**: `agent-repl--ws-get`, `agent-repl--ws-put`, `agent-repl--ws-del`, `agent-repl--ws-live-p`, `agent-repl--ws-known-p`, `agent-repl--ws-tombstoned-p`, `agent-repl--ws-open-p`, `agent-repl--ws-render-status`, `agent-repl--live-ws-names`, `agent-repl--ws-require-known`.

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

**Always thread WS through `agent-repl--log` / `agent-repl--log-verbose`.** If a workspace is in lexical scope, or derivable via `(+workspace-current-name)` or the buffer-local `agent-repl--owning-workspace`, pass it as the first argument. `nil` WS silently drops the `{ws=... id=... dir=... cst=... rst=... env=... vt=... in=... ...}` metadata block — that block is exactly what disambiguates 30 identical flood lines across N workspaces. Only pass `nil` when the call site is genuinely workspace-agnostic (load-time sentinel init, workspace-resolving helpers, pure file/directory utilities). If threading WS into a function changes its signature, do it — add an optional `ws` parameter dedicated to diagnostics if purity needs to be preserved otherwise.

**Diagnostic output must land in the logfile, never in a dedicated buffer as the sole record.** Route subprocess stdout/stderr, captured shell output, resolver responses, and any other instrumentation through `agent-repl--log` / `agent-repl--log-verbose` so it inherits the standard timestamp + `{ws=...}` metadata and survives session restarts. Side buffers are not greppable from `~/.claude/emacs/doom-agent-repl.log`, do not persist after Emacs exits, and require a human to already know the buffer name to inspect them. A side buffer is acceptable *in addition* for live interactive inspection — never *instead of* a log entry. If you find yourself writing `(get-buffer-create "*agent-repl-...*")` to hold diagnostics, stop and log instead.

**Every bug fix MUST add instrumentation.** A bug that reached production is itself proof the existing logging could not pinpoint it, so any fix must leave behind the `agent-repl--log` coverage (decision inputs, computed flags, branch taken, error captured) that would have isolated the root cause from a trace alone — never fix a bug without also closing the instrumentation gap it exposed.

**Instrument every new or changed code path.** Skip only for "extremely hot" paths — code that fires more than ~once per second across all workspaces (per-keystroke handlers, per-timer-tick callbacks, redisplay hooks, char-output filters) where the file-write itself would multiply log volume and bury other events. For merely-frequent-but-load-bearing paths, use `agent-repl--log-verbose` (echo-gated, always file-written) instead of omitting the log. Default to logging; "no log" is the harder choice and warrants a one-line comment naming the firing frequency and why instrumentation would be counterproductive.

## Skill symlinks: always target the MAIN worktree, never a linked one

The skill installer (`.claude/install.sh` + `modules/app/agent-repl/skills-cache/manifest.sh`) symlinks each managed skill into `~/.claude/skills/`. **Every symlink MUST point into the MAIN worktree of its repo, never a transient linked worktree.**

- A linked worktree (anything under `~/.config/doom-worktrees/*`, sandbox worktrees, etc.) is ephemeral — its path dangles the moment the worktree is pruned, leaving a broken skill symlink.
- `install.sh` resolves the main worktree via `git worktree list --porcelain` (its first `worktree` entry) and sources repo-local skills from there, regardless of which worktree the script is invoked from.
- `install.sh` **fails hard** (`_impl_in_nonmain_worktree`) when any manifest impl path resolves inside a non-main worktree. Do not add such an entry to `manifest.sh`.
- A skill with no home in the main worktree is NOT eligible to be managed. Re-home it (commit it into `modules/app/agent-repl/skills/` for repo-local skills, or its repo's main checkout for external skills) before adding it back.

The workspace-command skills (`workspace-merge`, `workspace-status`, `workspace-update`, `generate-workspace`) were folded into the single `/workspace` skill — manage `workspace`, not the removed per-command names.

## Debugging skills: when to use each

The doom-agent-repl ecosystem ships several Claude Code skills for debugging. Pick by the *kind* of evidence you need.

**Proactive on any state question — no prompting required.** The moment the user asks anything about the *current* state of the editor, a workspace, the daemon, or the REPL — "what's going on with X", "why is Y stuck/dead", "what state is Z in", "is the session alive" — reach for these two skills *immediately and without being asked*. A state question IS the trigger; invoking them is the first step of answering, never a follow-up you offer. Do not hand-roll ad-hoc `grep`/`cat`/`emacsclient` probes when a skill covers the evidence.

- `/debug-logs` — **read state/history that already exists on disk.** Reads `~/.claude/emacs/doom-agent-repl.log` and per-workspace `memory-state.el` snapshots. Reach for it first whenever the moment of interest has already passed (a workspace that went stuck, a crash-restart loop, a recorded agent/repl state) — anything answerable from what was already logged or snapshotted.
- `/runtime-eval-code` — **inspect or mutate *live* editor state** by sending elisp to the running Emacs, scoped to the current workspace. Reach for it when the answer is a value/predicate/buffer-state no logger captured, when you need to confirm what the editor believes *right now*, or when the log is silent because the signal lives in `*Messages*` or 3rd-party output.

The two compose: `/debug-logs` establishes what happened, `/runtime-eval-code` confirms the live state now. The fuller reference for each skill (plus the profiling skills) follows.

- `/debug-logs` — read history that already exists.
  - Use for any agent-repl logic/state bug whose timestamp you can pin down.
  - Reads `~/.claude/emacs/doom-agent-repl.log` and per-workspace `memory-state.el` snapshots.
  - First stop for reproducible bugs originating in `modules/app/agent-repl/`.

- `/runtime-eval-code` — inspect or mutate live editor state by sending elisp to the running Emacs, scoped to the current workspace.
  - Use when you need a value, predicate, or state inspection not captured by any logger.
  - Use to dump `*Messages*` to disk when the bug is signaled by 3rd-party output that the agent-repl log does NOT capture (magit/transient/doom-core warnings, byte-compile errors during refactor or dep bumps, package-init failures). The dump snippet and its grep recipes live in the `/runtime-eval-code` skill itself; `debug-logs` §9 just points there.
  - Use to drive a specific elisp snippet during a profiling session.

- `/profile` — capture a fresh sample with auto-stop.
  - Use when the symptom is performance (slow, hitching, hot path suspected).
  - Schedules a wakeup so the profiler stops automatically; analysis lands as a follow-up message.

- `/workspace-profile` — manual profiler toggle.
  - Use when you want to start/stop on your own cadence rather than time-boxed.

Rule of thumb: read first (`/debug-logs`), then inspect live (`/runtime-eval-code`), then measure (`/profile`).

When build or compilation errors surface during refactor or dep-bump work, the failing output typically lives in `*Messages*` — not in the agent-repl log file. Use `/runtime-eval-code` to dump `*Messages*` (its SKILL.md has the snippet and grep recipes), then read the dump.

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

After any changes to `modules/app/agent-repl/`, always run the agent-repl test suite. Prefer the safety-net wrapper:

```bash
.claude/safe-test-run.sh           # full suite
.claude/safe-test-run.sh "^foo"    # ert selector regex
```

The wrapper drops a checkpoint tag at `HEAD` before invoking ert, then diffs the pre/post git state (HEAD, all refs/heads, all refs/tags, working-tree status). On any drift the checkpoint tag is **preserved** and the script prints the exact `git reset --hard <tag>` command to roll back; on a clean run the tag is auto-removed. Exit codes: `0` clean, `1` test failures, `2` drift detected, `3` environment error.

You may invoke ert directly if you have a specific reason:

```bash
emacs -batch -Q -l ert -l modules/app/agent-repl/test-agent-repl.el -f ert-run-tests-batch-and-exit
```

A repo-checked-in pre-commit hook enforces this automatically: when any `modules/app/agent-repl/**.el` file is staged, the suite runs and a failure blocks the commit. The hook lives at `.githooks/pre-commit`. Install once per clone:

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

1. **Audit step (mandatory, explicit).** Before staging the commit, search the diff for every occurrence of `call-process`, `start-process`, `make-process`, `shell-command`, `shell-command-to-string`, `async-shell-command`, `process-file` — in production files (NOT test files; NOT the wrapper-definition file core.el).

2. **Classify each hit.** Each one is either:
  - (i) Inside a wrapper definition listed in `agent-repl--external-boundary-functions' (acceptable — the wrapper IS the external boundary).
  - (ii) Raw production code that needs to be extracted into a new wrapper (NOT acceptable; refactor before committing).

3. **No exceptions.** Phrases like "this is just for X, it's safe" are never valid. If you find yourself reasoning "this one is fine because…", stop and extract the wrapper anyway.

There IS one secondary safety net, but it covers a DIFFERENT failure mode:

#### Runtime guards (catch unmocked-WRAPPER test paths only)

`modules/app/agent-repl/test-helpers.el' iterates the registry at load time and replaces every wrapper's function cell with a guard that errors with `EXTERNAL BOUNDARY UNMOCKED: ...' if invoked. Tests `cl-letf' over the guard to install their fixture; tests that forget fail loudly.

**test-helpers.el is batch-only.** Every dangerous load-time side effect in it (guard install, `AGENT_REPL_STATE_DIR` redirect, module reload under stubbed `file-notify-add-watch`, log/merge/defer overrides) is gated on `noninteractive`:

- Loading test-helpers.el in an interactive session is an inert no-op that announces itself via `display-warning` — hot-reloading test files into a live Emacs is safe.
- `agent-repl-test--install-external-guards' refuses (signals) outside batch.
- If a guard nevertheless leaks into an interactive session, invoking it warns and delegates to the captured original instead of erroring, so the live session keeps working; in batch it errors exactly as documented above.
- Motivating incident: test-helpers.el hot-loaded into the main Emacs replaced the live wrappers, and the web frontend failed with "daemon at 127.0.0.1:8787 never became ready: EXTERNAL BOUNDARY UNMOCKED".
- Coverage for the gating lives in `test-test-helpers.el`.

This catches: a test that exercises a code path reaching a registered wrapper without mocking it. The test sees a loud error pointing at exactly which wrapper needs the `cl-letf` binding.

This does NOT catch: a raw `(shell-command-to-string "git ...")` in production code that bypasses every registered wrapper. That class of bug is the agent's audit step's responsibility, and there is no automated tripwire.

#### Wrapper registry (single source of truth)

`agent-repl--external-boundary-functions' (defvar in `modules/app/agent-repl/core.el') is the canonical list. The runtime guards iterate it. When you introduce a new wrapper, you MUST add it to that list in the same commit — otherwise the guard never installs for it and tests can silently shell out.

### Maintainer rule for new external wrappers

When you wrap a new external binary `X`:

1. Define `agent-repl--X-string` (or `--X-string-quiet`, etc.) in core.el. The body does NOTHING but invoke the external thing — no conditional logic, no parsing, no retries.

2. Add the wrapper symbol to `agent-repl--external-boundary-functions' in the SAME commit. Skipping this step means no guard, no protection, silent leak.

3. Update any existing production sites that already call `X` raw to route through the new wrapper.

4. Update or add tests; per the policy they `cl-letf` the wrapper, not `shell-command-to-string`.

5. Re-run the agent audit step (mandatory step 1 above) on the final diff before committing, including the new files you just wrote.

## Daemon bounce policy (claude-repld)

Agents may bounce the resident claude-repld to deploy a rebuilt binary — clients are expected to reconnect — under these rules:

- **Never bounce while a turn is in flight.** `agent-repl--frontend-stop-daemon' (and therefore `agent-repl-frontend-daemon-restart') refuses with "turn in flight" when any session reports `turn_active`; retry when idle. Do not work around the refusal with a direct `kill` — if you must kill directly (Emacs unavailable), check `GET /sessions` for `turn_active` yourself first.

- **Prefer the Emacs restart path** (`agent-repl-frontend-daemon-restart` via emacsclient): it serializes concurrent bounces, runs build-if-stale, and its refusal semantics are the policy.

- **Never hand-spawn a daemon on the configured addr.** Emacs owns the resident daemon's lifecycle and now ADOPTS a foreign daemon it finds answering on the port; a second spawn just bind-fails and dies.

- **Verify against an ephemeral daemon, not the resident one**: spin up your build on a random port with a scratch state dir (the e2e suite works this way), test, kill. The resident daemon is the user's.

- After any bounce, the client-side reattach loop (`agent-repl--frontend-reattach-check`) re-ensures vanished sessions (resume + transcript replay) and remounts webviews; repeated reattach failure against an answering daemon surfaces as a version-mismatch warning rather than retrying forever.

## Paren Checking

To verify parenthesis balance in an `.el` file (skipping strings and comments):

```bash
python3 .claude/check-parens.py <file.el>
```

Handy as a quick sanity check before invoking the byte-compiler.

## No Redundant Mechanisms

Never maintain two mechanisms for the same thing. Redundancy adds complexity, obscures which path is authoritative, and creates subtle divergence bugs. If a new approach replaces an old one, **delete the old one** — do not keep it "as a fallback." If the new approach isn't trusted enough to stand alone, it's not ready to ship.

Example: Claude Code hooks (`session_start`, `stop`, `prompt_submit`) are the sole source of session IDs and lifecycle events. Do not also scan `~/.claude/sessions/` files, watch terminal titles for readiness, or poll for state that hooks already deliver. One mechanism, one source of truth.

## No Duplicated or Mirrored Code — Always Extract Shared Helpers

**ABSOLUTE RULE: Never duplicate, mirror, or copy-paste code when extraction into a shared helper is possible. Always extract.** This applies to function bodies, prompt/template strings, defconst content, conditional branches, repeated `let*` blocks, parallel test-setup boilerplate — anything. If two call sites share more than trivial structure, the shared structure belongs in a helper, and the call sites become thin dispatches that vary only in their parameters.

The bar is intentionally low: if a future reader would look at two functions and think "these look almost identical," they should be one function with parameters. "Almost identical" is the smell — do not let it ship.

Why this is absolute:
- Mirrored code drifts. Every "almost identical" pair becomes a "subtly divergent" pair within a few edits — one site picks up a fix or a new arg, the other doesn't, and the bug is invisible until it bites.
- Mirrored code multiplies the test surface. Two parallel functions need two parallel test suites; one helper needs one test suite plus thin per-caller tests.
- Mirrored code obscures intent. A reader cannot tell whether the duplication is intentional (different requirements) or accidental (lazy copy-paste). Extraction forces that decision to be explicit at the parameter list.
- Mirrored code creates fertile ground for the "fix one, forget the other" class of bug — which is doubly bad in conjunction with the No-Silent-Fallbacks rule, because both sites continue silently doing the wrong thing.

Required process when adding a new variant of an existing pattern:

1. **Before writing the new variant, identify the existing one.** Read it. Look for what would differ vs. what would stay the same.
2. **If anything stays the same, extract first.** Pull the shared body into a private helper (`agent-repl--<verb>-<noun>`). Make the differing parts parameters. Then rewrite the existing call site through the helper *as a separate refactor commit*, run the tests to prove the refactor is behavior-preserving, and only then add the new variant on top.
3. **Test the helper directly,** in addition to the wrappers. The wrappers are thin and tested via end-to-end behavior; the helper carries the contract and deserves its own focused unit tests for the contract (validation, interpolation, edge cases).
4. **The wrappers must be trivial after extraction.** Each wrapper should be ~3–8 lines: docstring + `(interactive)` + a single call into the helper with literal arguments. If the wrapper is doing anything else, push that into the helper too.

Anti-patterns to reject:
- Two `defun`s whose bodies are 80%+ the same and differ only in 2–3 literal values (path, label, prompt string).
- Two `defconst`s whose strings share more than a sentence of template structure and differ only in interpolated tokens — extract a builder function and call it from both `defconst`s.
- "I'll just copy this and tweak it" as a working assumption when implementing a variant — that *is* the moment to extract.
- A new variant added without first refactoring the original through a shared helper.
- Comments like "mirrors X" / "parallel to X" / "based on X" used as a substitute for actually sharing code with X — those comments are a confession that the code should have been extracted.
- Test files where two `ert-deftest`s differ only in which function is called and which expected literal is asserted — extract a helper, parameterize, or write a single table-driven test.

The ONLY acceptable reason to leave near-duplication in place is that the user has been told what the shared helper would look like and has *explicitly* opted to keep the duplication for a stated reason. Default to extraction; ask if unsure.

Example (this is the canonical reference): the one-shot creators `agent-repl-create-doom-oneshot-workspace` and `agent-repl-create-explanation-engine-oneshot-workspace` both dispatch through `agent-repl--create-pinned-oneshot-workspace`. The success-suffix constants `agent-repl--oneshot-merge-suffix` and `agent-repl--oneshot-create-pr-suffix` are both built via `agent-repl--build-oneshot-success-suffix`. Any future `agent-repl-create-<repo>-oneshot-workspace` MUST dispatch through the same helper — do not start a third copy.

## Fullscreen panels — one consolidated entry point

**There is a single canonical operation for showing the Claude REPL fullscreen: `agent-repl--enter-fullscreen` in `panels.el`.** It always shows BOTH panels (vterm output AND input) and saves the prior layout as the workspace's `:fullscreen-config` so `agent-repl-toggle-fullscreen` can restore it. Every place with a concept of "go fullscreen" routes through it:

- `agent-repl-toggle-fullscreen` — its go-fullscreen branch calls `--enter-fullscreen` (the toggle adds the restore/no-poison logic around it).
- `agent-repl--drain-pending-fullscreen` — the `/workspace-generation` path calls `--enter-fullscreen` directly, NOT the toggle (the toggle would read the all-Claude-buffers layout as already-fullscreen via `agent-repl--fullscreen-p` and skip, leaving the input panel unshown).
- `agent-repl--maybe-fullscreen-on-switch` — the switch-to-workspace path reuses `agent-repl-toggle-fullscreen` (so the splitscreen layout is saved for a later `SPC w f` restore), and therefore funnels through `--enter-fullscreen` transitively.

When adding any new fullscreen entry point, call `agent-repl--enter-fullscreen` — never re-implement the save-config + sweep-non-panel-windows dance, and never show only one panel.

## Comment Non-Obvious Code

**ALWAYS comment any change whose reasoning isn't immediately obvious from the code itself — even if it's only slightly non-obvious.** The bar is low on purpose: if a future reader (or a future you) would have to re-derive *why* the line is shaped the way it is, leave a comment that says why. Examples that always warrant a `WHY:` comment:

- Load-bearing side effects of a call (e.g. a function called primarily for one purpose whose secondary effect is depended on elsewhere — name the dependent site).
- Reliance on an external package's undocumented or implicit behavior.
- Ordering constraints between statements that are not enforced by data flow.
- A choice between two plausible approaches where the rejected alternative has a subtle failure mode.
- A guard, fallback, or `ignore-errors` whose absence would cause a specific concrete bug — name the bug.

The comment must explain *why*, not *what*. "Calls foo before bar" describes the code; "foo must run first because bar reads state foo writes via hook X" is the comment. If you can delete the comment without losing information a reader needs, it shouldn't have been written; if a reader would have to git-blame or grep to understand the line, the comment is required.

## Sandbox-portable paths in `settings.json`

When adding or updating hooks/permissions that reference a home-relative path, use `~` rather than a hardcoded host home like `/Users/dodgecoates`, so the path resolves in both the host and the `DOOM_SANDBOX` container (whose `$HOME` is `/home/claude`).

## Cross-worktree code handoff — `git stash create`, not `push`

To hand uncommitted edits to another worktree of this repo (e.g. a spawned workspace), share a stash by SHA and have the receiver run `git stash apply <SHA>`. Prefer `git stash create` (`SHA=$(git stash create "msg")`): it writes neither the working tree nor the shared `refs/stash` stack, so the source keeps its edits and a concurrent worktree's `git stash pop` cannot collide with the handoff.

## Git

When asked to make changes, commit your work when done. Commit freely and often. **Never** rebase, pull, merge, push, or run any other mutating git commands without explicit instruction from the user.

## AGENTS.md Updates

Keep entries minimal — one short sentence or a brief code block per rule.
Prefer adding a new entry under an existing relevant section over creating a new top-level heading.

# Agent Guidelines for agent-repl Development

## Wire-protocol breaking changes require user approval

The agent-shim protobuf protocol shared by the agent-repl daemon and the shim ecosystem (the per-vendor agent-shim such as `claude/shim`, its `claude/shim-sidecar` JSONL reader, and the `shim-store`) may evolve freely in backward-compatible ways, but any BREAKING change to message shapes or semantics requires explicit user approval first — never break the contract silently, even though there are no external consumers.

## The `workspace` wire field is a CWD, never a display name

Every session-routed frontend command (`submitPrompt`, `interrupt`, `permissionAnswer`) is keyed by the session's **absolute cwd**: `SessionLocator.Locate` matches registry records on `rec.CWD == workspace`, and `sessiondrv` maps live drivers under that same string. Emacs keys everything by the **persp name** (`"doom"`), so a ws-keyed command must resolve its wire key through `agent-repl--frontend-ws-command-key` (`:project-dir`) — never pass the `ws` name straight to `agent-repl--uds-send-command`. The 2026-07-25 regression shipped the name and every prompt NACKed as `workspace "doom" has no live session to drive`, which reads as a dead session rather than a wire-contract violation; `checkWorkspaceKey` in `frontendcmd.go` now refuses a non-absolute key by name.

## Stand down a SPECIFIC session with `HibernateSession`, not `Hibernate`

Several registry records can share one workspace cwd (a stale duplicate, a superseded resume, an orphan awaiting reap). `sessiondrv.Manager.Hibernate(cwd)` SIGTERMs whichever shim is live for that cwd, so using it to stop one record's shim kills whatever session currently owns the workspace — on 2026-07-25 reaping an orphan killed the healthy session created 175ms earlier. Delete and supersede therefore call `HibernateSession(cwd, sessionID)`, which returns `ErrNotLiveSession` and stops nothing when a different session drives the workspace. `Hibernate` stays correct only for genuinely workspace-scoped teardown (idle sweep, daemon shutdown).

## The protocol schema is TREATED as vendor-agnostic

The `agentshim.data.v1` shapes were derived from the Claude harness, so the schema is not factually vendor-agnostic — but it is BELIEVED and TREATED as vendor-agnostic everywhere: no consumer special-cases a vendor. When adding a new vendor (e.g. a `codex-shim`), RESOLVE any incongruity by revising the API (a breaking change is the expected remedy, gated on the approval rule above) — never by bolting vendor side-channels onto the protocol. See `modules/app/agent-repl/proto/AGENTS.md`.

## The shim ecosystem exclusively facilitates agent-backend interaction

The agent-shim (per vendor: `claude/shim`, a future `codex/shim`, …), the `claude/shim-sidecar`, and the `shim-store` exist for exactly one purpose: facilitating interaction with the agent vendor's backend and surfacing it as agent-shim protobuf messages. Frontend serving, merge/workspace state, and render-state derivation never live in the shim ecosystem — those belong to the daemon.

## GUI nomenclature: tail status rows

The animated `thinking…`/`working…`/`retrying…`/`interrupting…`/`monitoring…` indicators at the bottom of the webapp feed are **tail status rows** (`webapp/src/render.ts`); the in-flight precedence set (interrupting > compacting > retrying > working) is the **bucket-1 tail** (`tailStatusRow`), `monitoring…` is its idle-but-async-live fallback, and each animates via `animatedEllipsis()`.

## GUI nomenclature: the central column

The **central column** is the framed strip of the webapp feed the conversation renders inside — the `--agent-bubble-cap` width (75% of `#feed`, centered, an eighth of blank margin each side; `webapp/src/styles.css`), NOT the full width of the encompassing Emacs window. Its **rails** are the column's left and right edges: the assistant response bubble's left edge hugs the left rail and the user prompt bubble's right edge hugs the right rail, while the full-width tool cards (Agent, Bash, Read, Skill, …) center between them at `--tool-card-cap`. Anything spanning the conversation (e.g. the `/clear` red rule and `/compact` label dividers) is confined to this column, not the window.

## GUI nomenclature: streaming response elements

A **streaming response element** is any response element — bubble or badge — that can be expanded to show a continuous stream of update information specific to an asynchronous process. The element is the expandable UNIT (a fold or a badge), not its host bubble; one bubble can carry several at once.

The consolidated architecture behind every such element is three orthogonal pieces (`webapp/src/stream-member.ts`, `webapp/src/render.ts`):

- **Member** — the model. `resolveMember` is the ONE place a call's streaming nature is decided: its effective source (daemon-classified or synthesized from the spawn announcement), ONE status (`running`/`done`/`error`/`killed`, notification status surfaced), ONE tail (ws-streamed over polled), and a `BodySpec` list in Shape A stacking order (child feed above the detached stream — a dual-body card stacks two folds, never a merged panel).
- **Face** — the collapsed identity. One status→badge vocabulary (`memberBadge`), one label truncation rule (`capLabel`), the poll-fed elapsed, and the shared top-right `Stop` button (bare label, prompt-mediated caveat in the tooltip), on both geometries: the card head row and the amber catalog pill.
- **Panel** — the expansion. Every body renders through `MemberFold` into the same `.agent-panel` inset, dispatched per `BodySpec` (`child-feed` → the partitioned child feed, `jsonl-transcript` → nested bubbles, `jsonl-journal` → rows, `raw` → a `<pre>`), with the depth cap, cycle guard, item cap, and dropped-notice enforced there alone. A catalog badge's expansion mounts the member's OWN `ToolCard` inside a Panel — badge context and feed context are one code path and cannot diverge.

Membership rulings from the deliberation: a `TaskCreate` card's update history is a member via its child-feed body (task-id claims, `partition.ts`); the gns-sockets fold shares the fold dress with a rehosted-items body (`gns.ts`); an announcement-less tail is a raw member wearing the same dress (nothing renders zero-click inline); a thinking disclosure is NOT a member (it streams the model's own turn, not a detached process). The standing regression gallery for all of this is the catalogue page (`webapp/catalogue.html`, scenarios in `webapp/src/catalogue.ts`).

## Debugging Vexing / Non-Obvious Bugs

When facing a bug that resists immediate root-cause identification, **do not speculate indefinitely**. Instead, shift to an instrumentation-first approach:

1. **Ask: "What logging can I add to narrow the root cause after reproduction?"**
   Design targeted log statements that distinguish between competing hypotheses. Each log point should eliminate at least one theory.

2. **Always use the existing logging helper.**
   All debug logging must go through `agent-repl--log` (defined in `core.el`). Never use raw `(message ...)` for debug instrumentation. The helper provides:
   - Timestamped output (`HH:MM:SS.mmm [agent-repl] {ws=... id=... ...} ...`)
   - Automatic workspace metadata (all plist keys) when `ws` is non-nil
   - Gated by `agent-repl-debug` (nil = off, t = on, 'verbose = high-frequency)
   - Use `agent-repl--log-verbose` for high-frequency events (per-keystroke, per-timer-tick, git-diff sentinels)
   - Signature: `(agent-repl--log WS "context: key=%s" value)` — WS is the workspace name string, or nil for workspace-free contexts
   - When a function has `ws` in scope (parameter or local), always pass it. Pure helpers with no ws pass nil.

3. **Prioritize "smoking gun" instrumentation.**
   Identify the single log point that would most directly distinguish between hypotheses. Add that first. For example, if the question is "is function A or function B being called?", a log at the top of each immediately resolves it.

4. **Wrap risky calls with `condition-case` logging.**
   When a theory involves a call erroring and preventing cleanup, wrap it:
   ```elisp
   (condition-case err
       (risky-call)
     (error (agent-repl--log ws "context: risky-call failed: %S" err)))
   ```
   This both catches the error for logging and prevents it from silently breaking downstream cleanup.

5. **Log buffer and state context.**
   For bugs involving wrong-buffer or stale-state theories, log `(current-buffer)`, relevant buffer-local variables, and mode states at the instrumentation point.

6. **Toggling logging at runtime.**
   Use `M-x agent-repl-debug/toggle-logging` to cycle standard logging on/off. With a prefix argument (`C-u M-x agent-repl-debug/toggle-logging`), it toggles verbose mode instead. Verbose mode includes high-frequency events (1-second timer ticks, git-diff sentinels, window changes, resolve-root). Standard mode logs only meaningful state transitions and user-initiated actions.

   **Suggest verbose mode to the user when investigating hot-path issues** (vterm redraws, overlay churn, mode-line refresh, window-config changes, async refresh ticks). Some call sites are *fully* gated on verbose (file write and echo both skipped) because they fire too often to leave on by default — notably `vterm-color-advice` in `overlay.el`, which previously emitted ~88% of the entire logfile. If the log appears silent for a suspected hot path, that gating is usually why; ask the user to enable verbose with `C-u M-x agent-repl-debug/toggle-logging` and reproduce.

7. **Persistent logfile (`~/.claude/doom-agent-repl.log`).**
   By default, all log output is also appended to `~/.claude/doom-agent-repl.log`. This file persists across Emacs sessions and is the primary artifact for debugging and coordinating with the user. When investigating a bug or answering a user question about recent behavior, **read `~/.claude/doom-agent-repl.log` first** — it contains the full timestamped trace of agent-repl activity. Use `M-x agent-repl-debug/toggle-log-to-file` to disable or re-enable file logging at runtime. Ensure that the user is cognizant of it's existence during tricky debug problems, and of how to enable/disable it. 

8. **Choosing standard vs verbose.**
   Events that fire on every timer tick, every window change, or every keystroke MUST use `agent-repl--log-verbose`. Events that fire on discrete user actions or state transitions use `agent-repl--log`. Rule of thumb: if it fires more than once per second across all workspaces, it's verbose.

## Worker-thread AppKit trap — nothing that reaches AppKit may run off the main thread on macOS

**The invariant:** a `(make-thread ...)` worker must never execute anything that can reach AppKit. On the NS build that means **two** distinct families, and both have wedged Emacs in production:

1. **Waiting on a subprocess** — `accept-process-output`, `call-process`, `shell-command-to-string`, anything routing through `wait_reading_process_output` → `ns_select_1` → `[NSApp run]`. (The `ns_select_1` analysis below.)

2. **Anything that can trigger a REDISPLAY** — redisplay calls `gui_consider_frame_title` → `-[NSWindow setTitle:]`. The non-obvious members of this family are process/buffer teardown:
  - `delete-process` — a status change redisplays (`redisplay_preserve_echo_area`).
  - `kill-buffer` on a buffer that still owns a live process — implicitly calls `delete-process`.
  - `message`, and modifying a buffer that is displayed in a window.

  Failure mode: AppKit raises an uncaught ObjC exception off-main → `objc_exception_throw` → `std::terminate` → `abort` → Emacs's fatal-signal handler. The worker then sits suspended in that handler **still holding the global Lisp lock**, so the main thread deadlocks forever on its next Lisp form (it appears as a 0%-CPU hang, blocked in `really_call_select` → `pthread_mutex_firstfit_lock_wait`). This wedged Emacs on 2026-07-12: a declined cherry-pick auto-resolve killed its resolver buffer on the merge worker thread.

**Required helpers** (all in `modules/app/agent-repl/worktree.el`) — never hand-roll these in worker-reachable code:

| Need | Use | Never |
|---|---|---|
| Wait for a subprocess | `agent-repl--wait-for-process-exit` / `agent-repl--spawn-and-wait` | `accept-process-output`, `call-process` |
| Kill a process | `agent-repl--kill-process-safely` | bare `delete-process` |
| Kill a (possibly process-owning) buffer | `agent-repl--kill-buffer-safely` | bare `kill-buffer` |
| Any UI op (perspective switch, magit, window config, workspace close) | `agent-repl--defer-to-main-thread` | calling it directly |
| Protect a main-thread-only entry point (e.g. a synchronous HTTP boundary) | `agent-repl--assert-main-thread` at its top | trusting call sites to stay on main |

**Per-diff audit (mandatory when touching merge/worker paths):** grep the diff for `delete-process`, `kill-buffer`, `accept-process-output`, `message`, `magit-`, `display-buffer`, `switch-to-buffer`. For each hit, ask *"can this run on the merge worker thread?"* — the worker entry point is the `make-thread` in `agent-repl--workspace-merge-async`, so anything reachable from `agent-repl--dispatch-merge-handler` qualifies. If it can, route it through the table above.

### The structural cause (Emacs 30.2 source)

On macOS, `wait_reading_process_output` unconditionally routes through `ns_select` at `process.c:5753`:

```c
#elif defined HAVE_NS
        /* And NS builds call thread_select in ns_select. */
        nfds = ns_select (max_desc + 1, &Available, ...);
```

`ns_select` calls `ns_select_1` in `nsterm.m`. `ns_select_1` has a non-main-thread check at line 4876 — but it only picks which `thread_select` variant to call. The function then falls through to:

```c
// nsterm.m:4951-4962
block_input ();
ns_init_events (&event);
[NSApp run];              // ← MAIN-THREAD-ONLY AppKit API
ns_finish_events ();
...
unblock_input ();
```

`[NSApp run]` is documented main-thread-only. Calling it from a worker thread is undefined behavior on macOS and produces two failure modes we've seen in practice:

1. **Long monopolization**: the worker thread gets stuck servicing NSCFString work AppKit isn't thread-safe for, holding the global Lisp lock (more accurately the select-serialization side of it) for tens of seconds while the main thread starves in `wait_reading_process_output → really_call_select → pthread_mutex_firstfit_lock_slow`. Symptom: Emacs feels frozen during a workspace merge.

2. **Hard abort**: when `unblock_input` runs on the worker thread and finds `interrupt_input_blocked` underflowed (it's a process-global counter that the main thread also touches), Emacs aborts via `emacs_abort`. Crash report shows the worker thread crashed in `unblock_input + 44`, called from `ns_select_1 + 1084`.

### The pattern that works

```elisp
(make-thread
 (lambda ()
   (let* ((proc (start-process "my-task" buf "my-cmd" ...))
          (status (agent-repl--wait-for-process-exit
                   proc TIMEOUT "my-tag" target-ws)))
     ;; ... use status ...
     )))
```

Or — if you need spawn + wait + log + buffer-cleanup as one unit — use the higher-level helper:

```elisp
(agent-repl--spawn-and-wait
 cmd out-buf
 :process-name "my-task"
 :timeout 30
 :log-tag "my-task"
 :log-ws ws
 :extract #'agent-repl--extract-buffer-whole       ; or skip-header-comments
 :on-completed (lambda (status output) ...)        ; optional
 :keep-buffer nil)                                  ; t to preserve OUT-BUF
```

Both helpers dispatch on `current-thread`:

- **Main thread**: legacy `accept-process-output` busy-wait. Safe because the main thread is the only one allowed to drive `[NSApp run]`. Preserved so ert tests (which run on main) and any direct main-thread caller behave identically to before.
- **Worker thread**: process sentinel + condition variable. The worker blocks on `condition-wait`, which **does** release the global Lisp lock (unlike `accept-process-output` via `ns_select_1`). The sentinel fires on the main thread (legal location for `[NSApp run]`) when the process exits, signals the condvar, and the worker wakes up to return.

### What to avoid

- ❌ `(accept-process-output proc TIMEOUT)` inside `(make-thread ...)` — even with `JUST-THIS-ONE=t`, the syscall still routes through `ns_select_1`. The flag only restricts which process's filters fire, not which select implementation is used.
- ❌ `(call-process ...)`, `(shell-command-to-string ...)`, `(process-file ...)` inside `(make-thread ...)` when the call captures stdout — same path, same trap.
- ❌ `(call-process ...)` with `destination=nil` (output discarded, e.g. `(apply #'call-process "git" nil nil nil "-C" root args)`) inside `(make-thread ...)`. It does NOT trap in `ns_select_1` (Emacs reads no pipes, so it never reaches `wait_reading_process_output`), but it is still SYNCHRONOUS and holds the global Lisp lock for the child's ENTIRE runtime — a worker thread executing C `call-process` never yields the lock until the child exits, so the main thread cannot run Lisp and the UI freezes for as long as the subprocess runs. This is the 2026-06-12 merge hang: the `onto-master` handler's `git fetch` and the cee-agent reinstall-and-bounce script (both destination-nil) starved the main-thread heartbeat for the whole subprocess duration. An earlier revision of this doc wrongly called destination-nil "fine"; it avoids the trap but not the freeze. Use the sentinel + condvar wait (`--wait-for-process-exit`, which DOES release the lock via `condition-wait`) instead. `agent-repl--git-exit-code` dispatches to that helper automatically on worker threads.
- ❌ Polling via `sit-for` / `sleep-for` in a worker thread — same path.

### What's fine

- ✓ Calling `accept-process-output` from the main thread (the historical default; the main-thread branch of `--wait-for-process-exit` does this).
- ✓ Calling `call-process` from the main thread, with the understanding that it blocks the UI for the duration.
- ✓ Async `make-process` + `:sentinel` / `:filter` from any thread — sentinels and filters fire on whichever thread is currently servicing events (usually main), not the caller of `make-process`.

### Subprocess spawns: PIPE, never pty — the git-pager stall

**Every synchronous subprocess the module spawns MUST use a pipe (`make-process :connection-type 'pipe`), never a pty.** `start-process` and `make-process` default to `process-connection-type` = t (a pty), and children change behavior on a terminal: `git log` (and any paginating git subcommand) launches its pager, `less` waits forever for a keypress nobody will send, and the call burns its full 60s `--capture-process-output` timeout. This was the trigger for every 2026-07-18 freeze: 60s-per-call merge stalls, main-thread UI freezes when the caller was a timer, and the timeout path feeding the worker-thread `kill-buffer` deadlock below. `agent-repl--capture-process-output` now spawns both branches on pipes; keep it that way, and spawn any NEW synchronous capture through it rather than hand-rolling.

### Buffer teardown can BE process teardown — the timeout-path deadlock

`kill-buffer` on a buffer that still owns a live process implicitly runs `delete-process` on the calling thread. On the merge worker that means redisplay → `-[NSThemeFrame setTitle:]` → ObjC exception → `abort`, and the worker dies in the fatal-signal handler **still holding the global Lisp lock** — the main thread then deadlocks on its next select (0% CPU, only `kill -9` recovers; observed 2026-07-12 and again 2026-07-18). The subtle case is CLEANUP code: `--capture-process-output`'s unwind-protect kills its stdout buffer, and on the timeout path the child is often still alive because `--kill-process-safely` deferred the real `delete-process` to the main thread. Route every buffer kill that can run on a worker through `agent-repl--kill-buffer-safely` — including `unwind-protect` cleanup forms and process sentinels, not just the obvious teardown paths.

### Indirect chains — the trap does not require a direct call

The per-diff audit greps for direct calls, but the 2026-07-18 freeze reached `[NSApp run]` through FIVE frames of indirection, none of which looked like a wait: merge worker → `load-file` (config reload) → top-level watcher re-arm (`agent-repl--dir-watcher-register`) → `file-notify-rm-watch` (synchronously fires the handler's `stopped` branch) → notification drain → the then-existing frontend HTTP call (`url-retrieve-synchronously` → `accept-process-output`; the HTTP client is gone now, but its UDS replacement pumps through the same `accept-process-output`). Symptom: ~90% of CPU samples in `-[NSApplication reportException:]` logging AppKit exceptions from the worker, main thread deadlocked in `really_call_select` — recoverable only by `kill -9`.

Two standing defenses, both born from that incident:

1. **`load` of module files is itself a worker-thread hazard** (top-level forms run watcher teardown/re-arm and drains). The merge success path routes its config reload through `agent-repl--merge-finalize-on-main`; never `load-file` on a worker.
2. **Main-thread-only entry points defend themselves**: the blocking frontend UDS waits (`agent-repl--frontend-wait-ready`, `agent-repl--frontend-await-uds`) open with `agent-repl--assert-main-thread`, converting any future smuggled chain into an ordinary error instead of a deadlock. The original such guard sat on `agent-repl--frontend-http-request`, which the S9 sentinel endgame deleted along with the rest of Emacs's daemon HTTP client; the hazard moved to the `accept-process-output` pumps that replaced it. Add the same assert to any new synchronous boundary that can reach `ns_select_1`.
