# Agents 

Always explain at the end of your response if there were changes whether or not the changes are persistent after unloading then. That is, whether resetting them and the re-loading will undo all the runtime changes. For example, if your changes add to some hook, then resetting your changes and reloading will not un-add that thing to the hook -- i'll still be there -- and thus the changes are "persistent after unloading"

Begin the persistence message with ✅ if the changes are not persistent after reloading (i.e. a reload fully resets them), or ❌ if any changes persist after reloading. When using ❌, briefly summarize which specific changes persist and why (e.g. "file written to disk", "entry added to hook").

## Claude REPL

**Never put claude-repl code in the top-level doomdir `config.el`.** All claude-repl code — defuns, advice, hooks, keybindings, magit integration — lives under `modules/app/claude-repl/*.el`. The top-level `config.el` is not reloaded by `M-x doom/reload-lisp-config` the same way the module is, and instrumentation added there routinely fails to take effect, wasting debugging cycles.

When adding a new concern:

1. Pick the right sub-file (`core.el`, `panels.el`, `status.el`, `session.el`, `sentinel.el`, `worktree.el`, `input.el`, `keybindings.el`, `magit.el`, etc.) or create a new one.
2. If creating a new file, register it in `modules/app/claude-repl/config.el` via `(claude-repl--load-module "NAME")`.
3. If the feature bridges claude-repl with another package (e.g. magit), put it in a dedicated integration file like `magit.el` rather than in the doomdir `config.el` under `(after! PACKAGE ...)`.
4. Leader-key bindings that reference a `claude-repl-*` symbol (e.g. overriding `SPC p p` to `claude-repl-switch-to-project`) belong in `keybindings.el`, not in the doomdir `config.el` — even when they shadow a pre-existing `+dwc/` binding there.

Naming: internals use `claude-repl--` prefix, public entry points use `claude-repl-` prefix. User-facing commands triggered by leader keybindings may keep the `+dwc/` prefix when they were moved out of the doomdir `config.el` and remain user-scope entry points.

**Never add `+dwc/` functions or variables inside the claude-repl module.** If a feature needs state that currently lives in `config.el` (e.g. `+dwc/workspace-history`), define a `claude-repl--` equivalent inside the module and wire it up there. The `+dwc/` namespace belongs to the doomdir config layer; the module must be self-contained and not introduce new `+dwc/` symbols.

## Claude REPL instrumentation

New code added to the claude-repl module must include instrumentation via `claude-repl--log`. Every dynamic aspect of the call site must be included in the log message — variable values, resolved paths, computed flags, branch outcomes, etc. The goal is that a log trace alone should be sufficient to diagnose any behavioral issue without needing to add instrumentation after the fact.

**Always thread WS through `claude-repl--log` / `claude-repl--log-verbose`.** If a workspace is in lexical scope, or derivable via `(+workspace-current-name)` or the buffer-local `claude-repl--owning-workspace`, pass it as the first argument. `nil` WS silently drops the `{ws=... id=... dir=... cst=... rst=... env=... vt=... in=... ...}` metadata block — that block is exactly what disambiguates 30 identical flood lines across N workspaces. Only pass `nil` when the call site is genuinely workspace-agnostic (load-time sentinel init, workspace-resolving helpers, pure file/directory utilities). If threading WS into a function changes its signature, do it — add an optional `ws` parameter dedicated to diagnostics if purity needs to be preserved otherwise.

**Diagnostic output must land in the logfile, never in a dedicated buffer as the sole record.** Route subprocess stdout/stderr, captured shell output, resolver responses, and any other instrumentation through `claude-repl--log` / `claude-repl--log-verbose` so it inherits the standard timestamp + `{ws=...}` metadata and survives session restarts. Side buffers are not greppable from `~/.claude/emacs/doom-claude-repl.log`, do not persist after Emacs exits, and require a human to already know the buffer name to inspect them. A side buffer is acceptable *in addition* for live interactive inspection — never *instead of* a log entry. If you find yourself writing `(get-buffer-create "*claude-repl-...*")` to hold diagnostics, stop and log instead.

**Instrument every new or changed code path.** Skip only for "extremely hot" paths — code that fires more than ~once per second across all workspaces (per-keystroke handlers, per-timer-tick callbacks, redisplay hooks, char-output filters) where the file-write itself would multiply log volume and bury other events. For merely-frequent-but-load-bearing paths, use `claude-repl--log-verbose` (echo-gated, always file-written) instead of omitting the log. Default to logging; "no log" is the harder choice and warrants a one-line comment naming the firing frequency and why instrumentation would be counterproductive.

## No Silent Fallbacks — Fail Hard on Invariant Violations

**ABSOLUTE RULE: Do not introduce ANY "fallback" behavior.** Under no circumstances — without **explicit, per-case permission from the user**, and only when the fallback is *absolutely* necessary — may code fall back to an alternative value, default, or code path when the primary input/lookup/precondition is missing or fails. **Always** prefer a loud error and a hard failure. Assume the answer is "no fallback" and propose the failure mode to the user; wait for explicit approval before writing any fallback. Do not suggest a fallback unless asked, and do not smuggle one in under names like "default", "graceful degradation", "sensible behavior when …", or "keep existing callers working".

**Never silently fall back, skip, or no-op when a precondition fails.** If an invariant is violated, **immediately fail loudly** with a `claude-repl--log` entry AND user-visible feedback (`user-error`, `error`, or at minimum a `message` that reaches the echo area). **Never fall back to an alternative code path** — the operation must abort entirely.

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
2. **If anything stays the same, extract first.** Pull the shared body into a private helper (`claude-repl--<verb>-<noun>`). Make the differing parts parameters. Then rewrite the existing call site through the helper *as a separate refactor commit*, run the tests to prove the refactor is behavior-preserving, and only then add the new variant on top.
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

Example (this is the canonical reference): the one-shot creators `claude-repl-create-doom-oneshot-workspace` and `claude-repl-create-explanation-engine-oneshot-workspace` both dispatch through `claude-repl--create-pinned-oneshot-workspace`. The success-suffix constants `claude-repl--oneshot-merge-suffix` and `claude-repl--oneshot-create-pr-suffix` are both built via `claude-repl--build-oneshot-success-suffix`. Any future `claude-repl-create-<repo>-oneshot-workspace` MUST dispatch through the same helper — do not start a third copy.

## Comment Non-Obvious Code

**ALWAYS comment any change whose reasoning isn't immediately obvious from the code itself — even if it's only slightly non-obvious.** The bar is low on purpose: if a future reader (or a future you) would have to re-derive *why* the line is shaped the way it is, leave a comment that says why. Examples that always warrant a `WHY:` comment:

- Load-bearing side effects of a call (e.g. a function called primarily for one purpose whose secondary effect is depended on elsewhere — name the dependent site).
- Reliance on an external package's undocumented or implicit behavior.
- Ordering constraints between statements that are not enforced by data flow.
- A choice between two plausible approaches where the rejected alternative has a subtle failure mode.
- A guard, fallback, or `ignore-errors` whose absence would cause a specific concrete bug — name the bug.

The comment must explain *why*, not *what*. "Calls foo before bar" describes the code; "foo must run first because bar reads state foo writes via hook X" is the comment. If you can delete the comment without losing information a reader needs, it shouldn't have been written; if a reader would have to git-blame or grep to understand the line, the comment is required.

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

   **Suggest verbose mode to the user when investigating hot-path issues** (vterm redraws, overlay churn, mode-line refresh, window-config changes, async refresh ticks). Some call sites are *fully* gated on verbose (file write and echo both skipped) because they fire too often to leave on by default — notably `vterm-color-advice` in `overlay.el`, which previously emitted ~88% of the entire logfile. If the log appears silent for a suspected hot path, that gating is usually why; ask the user to enable verbose with `C-u M-x claude-repl-debug/toggle-logging` and reproduce.

7. **Persistent logfile (`~/.claude/doom-claude-repl.log`).**
   By default, all log output is also appended to `~/.claude/doom-claude-repl.log`. This file persists across Emacs sessions and is the primary artifact for debugging and coordinating with the user. When investigating a bug or answering a user question about recent behavior, **read `~/.claude/doom-claude-repl.log` first** — it contains the full timestamped trace of claude-repl activity. Use `M-x claude-repl-debug/toggle-log-to-file` to disable or re-enable file logging at runtime. Ensure that the user is cognizant of it's existence during tricky debug problems, and of how to enable/disable it. 

8. **Choosing standard vs verbose.**
   Events that fire on every timer tick, every window change, or every keystroke MUST use `claude-repl--log-verbose`. Events that fire on discrete user actions or state transitions use `claude-repl--log`. Rule of thumb: if it fires more than once per second across all workspaces, it's verbose.
