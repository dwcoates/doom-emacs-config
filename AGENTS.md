# Agents 

Always explain at the end of your response if there were changes whether or not the changes are persistent after unloading then. That is, whether resetting them and the re-loading will undo all the runtime changes. For example, if your changes add to some hook, then resetting your changes and reloading will not un-add that thing to the hook -- i'll still be there -- and thus the changes are "persistent after unloading"

Begin the persistence message with ✅ if the changes are not persistent after reloading (i.e. a reload fully resets them), or ❌ if any changes persist after reloading. When using ❌, briefly summarize which specific changes persist and why (e.g. "file written to disk", "entry added to hook").

## Claude REPL

**Never put claude-repl code in the top-level doomdir `config.el`.** All claude-repl code — defuns, advice, hooks, keybindings, magit integration — lives under `modules/app/claude-repl/*.el`. The top-level `config.el` is not reloaded by `M-x doom/reload-lisp-config` the same way the module is, and instrumentation added there routinely fails to take effect, wasting debugging cycles.

When adding a new concern:

1. Pick the right sub-file (`core.el`, `panels.el`, `status.el`, `session.el`, `sentinel.el`, `worktree.el`, `input.el`, `keybindings.el`, `magit.el`, etc.) or create a new one.
2. If creating a new file, register it in `modules/app/claude-repl/config.el` via `(claude-repl--load-module "NAME")`.
3. If the feature bridges claude-repl with another package (e.g. magit), put it in a dedicated integration file like `magit.el` rather than in the doomdir `config.el` under `(after! PACKAGE ...)`.

Naming: internals use `claude-repl--` prefix, public entry points use `claude-repl-` prefix. User-facing commands triggered by leader keybindings may keep the `+dwc/` prefix when they were moved out of the doomdir `config.el` and remain user-scope entry points.

**Never add `+dwc/` functions or variables inside the claude-repl module.** If a feature needs state that currently lives in `config.el` (e.g. `+dwc/workspace-history`), define a `claude-repl--` equivalent inside the module and wire it up there. The `+dwc/` namespace belongs to the doomdir config layer; the module must be self-contained and not introduce new `+dwc/` symbols.

## Claude REPL instrumentation

New code added to the claude-repl module must include instrumentation via `claude-repl--log`. Every dynamic aspect of the call site must be included in the log message — variable values, resolved paths, computed flags, branch outcomes, etc. The goal is that a log trace alone should be sufficient to diagnose any behavioral issue without needing to add instrumentation after the fact.

**Always thread WS through `claude-repl--log` / `claude-repl--log-verbose`.** If a workspace is in lexical scope, or derivable via `(+workspace-current-name)` or the buffer-local `claude-repl--owning-workspace`, pass it as the first argument. `nil` WS silently drops the `{ws=... id=... dir=... cst=... rst=... env=... vt=... in=... ...}` metadata block — that block is exactly what disambiguates 30 identical flood lines across N workspaces. Only pass `nil` when the call site is genuinely workspace-agnostic (load-time sentinel init, workspace-resolving helpers, pure file/directory utilities). If threading WS into a function changes its signature, do it — add an optional `ws` parameter dedicated to diagnostics if purity needs to be preserved otherwise.

## No Silent Fallbacks — Fail Hard on Invariant Violations

**Never silently fall back, skip, or no-op when a precondition fails.** If an invariant is violated, **immediately fail loudly** with a `claude-repl--log` entry AND user-visible feedback (`user-error`, `error`, or at minimum a `message` that reaches the echo area). **Never fall back to an alternative code path** — the operation must abort entirely.

**Do not commit state changes before the failure point.** If an operation involves multiple steps (e.g., resolve session ID, then create worktree), validate all preconditions before mutating any state. If validation fails partway through, no workspace should be created, no timers scheduled, no hash table entries written. The system state must remain unchanged on failure.

Silent fallbacks create mysterious "stuck" states (the slash-passthrough bug was one — vterm lookup failed silently, keystrokes piled onto a hidden stack, the user saw no effect and had no signal that anything was wrong). A loud failure surfaces itself and can be diagnosed; a silent fallback just leaves the user guessing. A "degraded but working" fallback is even worse — the user sees *something* happen, assumes it worked, and only discovers the problem much later (e.g., a workspace branched from origin/master instead of the requested fork source).

Anti-patterns to reject:
- `(when-let ((x (lookup))) BODY)` where BODY is user-expected behavior. If `lookup` returns nil, the caller needs to know.
- State mutations that run regardless of whether the upstream operation succeeded (e.g., pushing onto a local stack after a failed forward).
- `(ignore-errors ...)` without a companion log of what was swallowed.
- `or`-chained defaults that mask missing data: `(or (ws-get :vterm) (default-vterm))`.
- Early returns that hide a failed precondition instead of signaling it.
- Returning nil from a resolution function and letting the caller silently degrade to a default (e.g., resolving a fork source to nil and falling back to origin/master).

The only acceptable silent no-op is one whose contract **explicitly requests** it: a best-effort cleanup where failure is known to be recoverable, or a `lookup-or-nil`-style query function. In those cases, document the contract in the docstring so callers know what they're getting.

When in doubt: fail loudly. When a precondition fails: abort entirely.

## Testing

After any changes to `modules/app/claude-repl/`, always run the claude-repl test suite:

```bash
emacs -batch -Q -l ert -l modules/app/claude-repl/test-claude-repl.el -f ert-run-tests-batch-and-exit
```

**Zero tolerance for test failures.** Every test failure is a real bug that must be fixed before your work is done. **There is NO such thing as a "pre-existing" failure — not under ANY circumstances, EVER.** Do not investigate whether a failure predates your work. Do not check git history. Do not stash, checkout, or touch git state to "verify" it was already broken. Do not rationalize, dismiss, categorize, defer, or explain away any test failure for any reason whatsoever. If a test fails, fix it. Every failing test is your responsibility the moment you observe it. Never report work as complete while any test is failing.

## Paren Checking

To verify parenthesis balance in an `.el` file (skipping strings and comments):

```bash
python3 .claude/check-parens.py <file.el>
```

## No Redundant Mechanisms

Never maintain two mechanisms for the same thing. Redundancy adds complexity, obscures which path is authoritative, and creates subtle divergence bugs. If a new approach replaces an old one, **delete the old one** — do not keep it "as a fallback." If the new approach isn't trusted enough to stand alone, it's not ready to ship.

Example: Claude Code hooks (`session_start`, `stop`, `prompt_submit`) are the sole source of session IDs and lifecycle events. Do not also scan `~/.claude/sessions/` files, watch terminal titles for readiness, or poll for state that hooks already deliver. One mechanism, one source of truth.

## Git

When asked to make changes, commit your work when done. Commit freely and often. **Never** rebase, pull, merge, push, or run any other mutating git commands without explicit instruction from the user.

## AGENTS.md Updates

Keep entries minimal — one short sentence or a brief code block per rule.

# Agent Guidelines for claude-repl Development

## Debugging Vexing / Non-Obvious Bugs

When facing a bug that resists immediate root-cause identification, **do not speculate indefinitely**. Instead, shift to an instrumentation-first approach:

1. **Ask: "What logging can I add to narrow the root cause after reproduction?"**
   Design targeted log statements that distinguish between competing hypotheses. Each log point should eliminate at least one theory.

2. **Always use the existing logging helper.**
   All debug logging must go through `claude-repl--log` (defined in `core.el`). Never use raw `(message ...)` for debug instrumentation. The helper provides:
   - Timestamped output (`HH:MM:SS.mmm [claude-repl] {ws=... id=... ...} ...`)
   - Automatic workspace metadata (all plist keys) when `ws` is non-nil
   - Gated by `claude-repl-debug` (nil = off, t = on, 'verbose = high-frequency)
   - Use `claude-repl--log-verbose` for high-frequency events (per-keystroke, per-timer-tick, git-diff sentinels)
   - Signature: `(claude-repl--log WS "context: key=%s" value)` — WS is the workspace name string, or nil for workspace-free contexts
   - When a function has `ws` in scope (parameter or local), always pass it. Pure helpers with no ws pass nil.

3. **Prioritize "smoking gun" instrumentation.**
   Identify the single log point that would most directly distinguish between hypotheses. Add that first. For example, if the question is "is function A or function B being called?", a log at the top of each immediately resolves it.

4. **Wrap risky calls with `condition-case` logging.**
   When a theory involves a call erroring and preventing cleanup, wrap it:
   ```elisp
   (condition-case err
       (risky-call)
     (error (claude-repl--log ws "context: risky-call failed: %S" err)))
   ```
   This both catches the error for logging and prevents it from silently breaking downstream cleanup.

5. **Log buffer and state context.**
   For bugs involving wrong-buffer or stale-state theories, log `(current-buffer)`, relevant buffer-local variables, and mode states at the instrumentation point.

6. **Toggling logging at runtime.**
   Use `M-x claude-repl-debug/toggle-logging` to cycle standard logging on/off. With a prefix argument (`C-u M-x claude-repl-debug/toggle-logging`), it toggles verbose mode instead. Verbose mode includes high-frequency events (1-second timer ticks, git-diff sentinels, window changes, resolve-root). Standard mode logs only meaningful state transitions and user-initiated actions.

7. **Persistent logfile (`~/.claude/doom-claude-repl.log`).**
   By default, all log output is also appended to `~/.claude/doom-claude-repl.log`. This file persists across Emacs sessions and is the primary artifact for debugging and coordinating with the user. When investigating a bug or answering a user question about recent behavior, **read `~/.claude/doom-claude-repl.log` first** — it contains the full timestamped trace of claude-repl activity. Use `M-x claude-repl-debug/toggle-log-to-file` to disable or re-enable file logging at runtime.

8. **Choosing standard vs verbose.**
   Events that fire on every timer tick, every window change, or every keystroke MUST use `claude-repl--log-verbose`. Events that fire on discrete user actions or state transitions use `claude-repl--log`. Rule of thumb: if it fires more than once per second across all workspaces, it's verbose.
