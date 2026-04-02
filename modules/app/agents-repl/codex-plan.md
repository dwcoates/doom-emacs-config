# Codex Backend Support Plan

## Status

Phase 0–4 complete. The module has been renamed `agents-repl` and the backend
abstraction layer is in place. What remains is wiring the backend-specific
behavior that currently still defaults to Claude.

---

## What's already done

- **Phase 0**: Renamed `claude-repl` → `agents-repl` throughout (module dir,
  all symbols, `init.el`, main `config.el`).
- **Phase 1**: Per-workspace backend infrastructure — `agents-repl-default-backend`
  defcustom, `agents-repl--ws-backend` accessor, `agents-repl-set-backend`
  interactive command, `:backend` stamped into workspace plist on session start.
- **Phase 2**: `agents-repl--backends` alist with full config for both `claude`
  and `codex` backends (start command, skip-perms flag, spinner predicate,
  interrupt fn, exempt inputs, paste-returns, permission-watch flag).
- **Phase 3**: Six functions parametrized via backend config:
  - `--start-backend` (was `--start-claude`)
  - `--title-has-spinner-p` (dispatches to `:spinner-p`)
  - `agents-repl-interrupt` (dispatches to `:interrupt`)
  - `--send-input-to-vterm` (respects `:paste-returns`)
  - `--skip-metaprompt-p` (reads `:exempt-inputs`)
  - Permission file-notify watcher (Claude-only guard)
- **Phase 4**: Buffer naming uses backend `:buffer-prefix` (`*claude-HASH*` vs
  `*codex-HASH*`); all ~10 regex/format sites updated to match both prefixes.
- **Phase 5**: `SPC j b` → `agents-repl-set-backend`.

---

## Remaining work

### 1. Ensure input buffer is also named with backend prefix

`agents-repl--ensure-input-buffer` calls `agents-repl--buffer-name "-input"` but
does not pass `ws`. Verify it picks up the correct workspace and therefore the
correct prefix (e.g. `*codex-input-HASH*`).

```elisp
;; in --ensure-input-buffer, confirm ws is passed:
(get-buffer-create (agents-repl--buffer-name "-input" ws))
```

### 2. `--kill-stale-vterm` uses `--buffer-name` without ws

```elisp
(defun agents-repl--kill-stale-vterm ()
  (when-let ((existing (get-buffer (agents-repl--buffer-name))))
    ...))
```

This will look for the wrong prefix if `agents-repl-default-backend` differs from
the workspace's actual backend. Pass `ws` through from `--ensure-vterm-buffer`.

### 3. `:permission` state guard for codex

The `:permission` workspace state and the `❓` tabline label are currently always
reachable. Since codex has no file-based permission notification, wrap the state
transitions in `--on-permission-notify` (already Claude-only) and additionally
guard the tabline `❓` render:

```elisp
(label (pcase state
         ;; Only show ❓ for claude workspaces
         (:permission (if (eq (agents-repl--ws-backend name) 'claude) "❓" num))
         (_ num)))
```

### 4. `--ensure-input-buffer` ws argument

`agents-repl--ensure-input-buffer` receives `ws` but `--buffer-name` is called
without it in at least one path. Audit the call sites.

### 5. Smoke test codex end-to-end

1. Set `agents-repl-default-backend` to `'codex` (or use `SPC j b` in a workspace).
2. Launch with `agents-repl` — expect buffer `*codex-HASH*` and `*codex-input-HASH*`.
3. Confirm startup command is `codex resume --last [--dangerously-bypass-approvals-and-sandbox]`.
4. Confirm spinner detection works (braille prefix `[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]`).
5. Confirm `C-c C-k` sends `C-c` (not `ESC ESC i`).
6. Confirm large paste (>200 chars) sends only one Return.
7. Confirm `/status` is exempt from metaprompt; `/usage` and `/login` are not.
8. Confirm `--ws-for-dir` finds the codex buffer by hash.

### 6. Delete old module directory

Once smoke tests pass, delete `modules/app/claude-repl/`.

---

## Research findings summary

| Integration | Claude Code | Codex |
|---|---|---|
| Startup | `claude -c` | `codex resume --last` |
| Skip permissions | `--dangerously-skip-permissions` | `--dangerously-bypass-approvals-and-sandbox` |
| Ready detection | First terminal title set (vterm--set-title advice) | Same mechanism works |
| Thinking detection | Non-ASCII prefix, not `✳` | Braille prefix `[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]` |
| Idle detection | `✳` prefix or plain title | No spinner prefix |
| Interrupt | `ESC ESC` + 250ms + `i` | `C-c` only |
| Slash commands | `/clear /usage /login /logout` | `/clear /status /logout` (no `/login` in TUI) |
| Paste submission | Double Return for >200 chars | Single Return |
| Permission prompts | Sentinel file in `~/.claude/workspace-notifications/` | None (in-TUI only; upstream issues #11808, #3052) |
| Input box | Bottom ~4 lines of vterm | Bottom pane (expanding); same hide-overlay approach applies |
| Session continue | `-c` flag | `resume --last` subcommand |

---

## Known gaps (no clean solution yet)

- **`:permission` workspace state for codex**: No file-based hook exists. Upstream
  openai/codex issues [#11808](https://github.com/openai/codex/issues/11808) and
  [#3052](https://github.com/openai/codex/issues/3052) track this. Until resolved,
  the `:permission` indicator simply never fires for codex sessions.
- **Hide overlay line count**: Claude's input box is ~4 lines; codex's `ChatComposer`
  expands. May need to adjust the overlay coverage heuristic for codex.
