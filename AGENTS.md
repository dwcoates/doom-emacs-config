# Agents 

Always explain at the end of your response if there were changes whether or not the changes are persistent after unloading then. That is, whether resetting them and the re-loading will undo all the runtime changes. For example, if your changes add to some hook, then resetting your changes and reloading will not un-add that thing to the hook -- i'll still be there -- and thus the changes are "persistent after unloading"

Begin the persistence message with ✅ if the changes are not persistent after reloading (i.e. a reload fully resets them), or ❌ if any changes persist after reloading. When using ❌, briefly summarize which specific changes persist and why (e.g. "file written to disk", "entry added to hook").

## Claude REPL

All Claude REPL functionality (commands, keybindings, functions) must be added to `modules/app/claude-repl/config.el`. Never add Claude REPL features to `config.el` or any other file without first asking the user.

Functions defined in `modules/app/claude-repl/config.el` must use the `claude-repl-` prefix (public) or `claude-repl--` prefix (private). Never use `+dwc/` prefixed functions in that file — `+dwc/` functions belong in `config.el`.

## Claude REPL instrumentation

New code added to the claude-repl module must include instrumentation via `claude-repl--log`. Every dynamic aspect of the call site must be included in the log message — variable values, resolved paths, computed flags, branch outcomes, etc. The goal is that a log trace alone should be sufficient to diagnose any behavioral issue without needing to add instrumentation after the fact.

## No Silent Fallbacks

**Never silently fall back, skip, or no-op when a precondition fails.** Always fail loudly: a `claude-repl--log` entry AND user-visible feedback (a `user-error`, `error`, or at minimum a `message` that reaches the echo area).

Silent fallbacks create mysterious "stuck" states (the slash-passthrough bug was one — vterm lookup failed silently, keystrokes piled onto a hidden stack, the user saw no effect and had no signal that anything was wrong). A loud failure surfaces itself and can be diagnosed; a silent fallback just leaves the user guessing.

Anti-patterns to reject:
- `(when-let ((x (lookup))) BODY)` where BODY is user-expected behavior. If `lookup` returns nil, the caller needs to know.
- State mutations that run regardless of whether the upstream operation succeeded (e.g., pushing onto a local stack after a failed forward).
- `(ignore-errors ...)` without a companion log of what was swallowed.
- `or`-chained defaults that mask missing data: `(or (ws-get :vterm) (default-vterm))`.
- Early returns that hide a failed precondition instead of signaling it.

The only acceptable silent no-op is one whose contract **explicitly requests** it: a best-effort cleanup where failure is known to be recoverable, or a `lookup-or-nil`-style query function. In those cases, document the contract in the docstring so callers know what they're getting.

When in doubt: fail loudly.

## Testing

After any changes to `modules/app/claude-repl/`, always run the claude-repl test suite:

```bash
emacs -batch -Q -l ert -l modules/app/claude-repl/test-claude-repl.el -f ert-run-tests-batch-and-exit
```

**Zero tolerance for test failures.** Every test failure is a real bug that must be fixed before your work is done. There is no such thing as a "pre-existing" or "known" failure — if a test fails, fix it. Do not skip, ignore, rationalize, or dismiss any test failure for any reason. If you encounter a failing test, either fix the test or fix the code. Never report work as complete while any test is failing.

## Paren Checking

To verify parenthesis balance in an `.el` file (skipping strings and comments):

```bash
python3 .claude/check-parens.py <file.el>
```

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

7. **Choosing standard vs verbose.**
   Events that fire on every timer tick, every window change, or every keystroke MUST use `claude-repl--log-verbose`. Events that fire on discrete user actions or state transitions use `claude-repl--log`. Rule of thumb: if it fires more than once per second across all workspaces, it's verbose.
