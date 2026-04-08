# Agents 

Always explain at the end of your response if there were changes whether or not the changes are persistent after unloading then. That is, whether resetting them and the re-loading will undo all the runtime changes. For example, if your changes add to some hook, then resetting your changes and reloading will not un-add that thing to the hook -- i'll still be there -- and thus the changes are "persistent after unloading"

Begin the persistence message with ✅ if the changes are not persistent after reloading (i.e. a reload fully resets them), or ❌ if any changes persist after reloading. When using ❌, briefly summarize which specific changes persist and why (e.g. "file written to disk", "entry added to hook").

## Claude REPL

All Claude REPL functionality (commands, keybindings, functions) must be added to `modules/app/claude-repl/config.el`. Never add Claude REPL features to `config.el` or any other file without first asking the user.

Functions defined in `modules/app/claude-repl/config.el` must use the `claude-repl-` prefix (public) or `claude-repl--` prefix (private). Never use `+dwc/` prefixed functions in that file — `+dwc/` functions belong in `config.el`.

## Claude REPL instrumentation

New code added to the claude-repl module must include instrumentation via `claude-repl--log`. Every dynamic aspect of the call site must be included in the log message — variable values, resolved paths, computed flags, branch outcomes, etc. The goal is that a log trace alone should be sufficient to diagnose any behavioral issue without needing to add instrumentation after the fact.

## Testing

After any changes to `modules/app/claude-repl/`, always run the claude-repl test suite:

```bash
emacs -batch -Q -l ert -l modules/app/claude-repl/test-claude-repl.el -f ert-run-tests-batch-and-exit
```

## Paren Checking

To verify parenthesis balance in an `.el` file (skipping strings and comments):

```bash
python3 .claude/check-parens.py <file.el>
```

## AGENTS.md Updates

Keep entries minimal — one short sentence or a brief code block per rule.

