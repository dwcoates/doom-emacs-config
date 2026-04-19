# Fallback and Default Logic Report: `panels.el`

**Total instances: 57**

---

## Section 1: `defvar` Defaults (2 instances)

| # | Line | Variable | Default |
|---|---|---|---|
| 1.1 | 330 | `claude-repl--sync-timer` | `nil` (no timer scheduled) |
| 1.2 | 333 | `claude-repl--cursor-reset-timer` | `nil` (no timer scheduled) |

---

## Section 2: `or` Expressions (3 instances)

| # | Line | Code | Chain |
|---|---|---|---|
| 2.1 | 243 | `(or ws (+workspace-current-name))` | Explicit ws -> current workspace |
| 2.2 | 492 | `(or start-cmd "?")` | Start command -> `"?"` for display |
| 2.3 | 757 | `(or (claude-repl--ws-get ws new-env) (make-...))` | Existing struct -> new struct |

---

## Section 3: `when-let` / `and` Nil-Guards (12 instances)

Key instances:

| # | Line | Context | Falls back TO |
|---|---|---|---|
| 4.1 | 9-10 | Buffer visibility check | `nil` (three failure modes indistinguishable) |
| 4.4 | 67 | `window-in-direction 'above` | Stays in current window |
| 4.5 | 89-91 | Focus input panel | Silent no-op (cursor not moved) |
| 4.7 | 198-201 | Redirect from Claude buffer | No redirect (fullscreen case) |
| 4.9 | 387-388 | Bounce from vterm | Warning message (user stranded) |
| 4.10 | 567 | Kill placeholder | No-op if already killed |
| 4.11 | 678-681 | Focus input after show | Silent no-op |

---

## Section 4: `condition-case` / `ignore-errors` (6 instances)

| # | Line | Code | Scope |
|---|---|---|---|
| 5.1 | 38 | `ignore-errors (delete-window ...)` | Panel window close |
| 5.2 | 208 | `ignore-errors (persp-frame-save-state)` | Workspace switch |
| 5.3 | 314-320 | `condition-case ... (end-of-buffer nil)` | Cursor reset |
| **5.4** | **340-342** | **`(condition-case nil ... (error nil))`** | **Blanket error suppression for sync-panels** |
| 5.5 | 597 | `ignore-errors (claude-repl--disable-hide-overlay)` | Session teardown |
| 5.6 | 688 | `ignore-errors (delete-window ...)` | Fullscreen mode |

**Instance 5.4 is the broadest error suppression in the codebase.** ANY error from `sync-panels` is silently discarded.

---

## Section 5: `if`/`when`/`cond` Guards (18 instances)

Key instances:

| # | Line | Context | Consequence |
|---|---|---|---|
| 6.3 | 108-109 | `when vterm--term` | Skip redraw if terminal not initialized |
| 6.5 | 135-143 | `resolve-vterm-buffer` | Two strategies: current buffer vs workspace lookup |
| 6.6 | 149-160 | `refresh-vterm` | Three-tier fallback: no buffer / dead / wrong mode |
| 6.13 | 418-424 | `kill-stale-vterm` | Only kills processless buffers |
| **6.15** | **544-560** | **`claude-repl` main entry** | **Five-branch state machine with `t` catch-all** |
| 6.16 | 698-713 | `toggle-fullscreen` | Three branches: saved config / live vterm / no vterm |
| 6.17 | 680, 726 | `bound-and-true-p evil-mode` | Safe without evil-mode |

---

## Section 6: Hardcoded Constants (9 instances)

| Value | Line | Purpose |
|---|---|---|
| `0.6` | 76 | Panel split ratio (60/40) |
| `-0.15` | 77 | Input panel height (15%) |
| `0.5` sec | 580 | SIGKILL delay |
| `8` chars | 787 | Session ID display length |
| `0` | 213 | `run-at-time` deferred to next event loop |
| `15` | 434, 467 | Vterm background grey |

---

## Section 7: Feature-Conditional Loading

### Instance 10.1: `modulep! :ui workspaces`
- **Line(s):** 215-219
- **Consequence:** Without the workspaces module, ALL workspace-switch lifecycle hooks are disabled. Panel orphan detection, vterm refresh, and pending-show-panels draining all silently stop.

---

## Cross-Referenced Fallbacks

- **`claude-repl--buffer-name`** (core.el): explicit ws -> `+workspace-current-name` -> `"default"`
- **`claude-repl--resolve-root`** (core.el): git-root -> project-root -> `default-directory`

---

## Most Consequential

1. **Instance 5.4** (blanket error suppression around sync-panels)
2. **Instance 4.5** (silent focus failure -- panels shown but cursor not moved)
3. **Instance 2.1** (`or ws (+workspace-current-name)` in on-close -- late resolution could target wrong workspace)
4. **Instance 10.1** (`modulep!` gate -- entire lifecycle collapses without workspaces module)
5. **Instance 4.9** (bounce-from-vterm warning -- user stranded with descriptive message)
