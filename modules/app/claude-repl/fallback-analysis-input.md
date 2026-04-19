# Fallback and Default Logic Report: `input.el`

**Total instances: 50**

---

## Section 1: `defcustom` / `defvar` / `defface` Defaults (13 instances)

| # | Line | Variable | Default | Key consequence |
|---|---|---|---|---|
| 1 | 7 | `claude-repl-skip-permissions` | `t` | Metaprompt injection active by default |
| 2 | 12 | `claude-repl-prefix-period` | `7` | Metaprompt every 7th prompt |
| 3 | 18 | `claude-repl-command-prefix` | Long safety string | Baked into formatted prefix at load time |
| 4 | 25-33 | `claude-repl--command-prefix` | Formatted at load time | **Cannot be updated by customizing `claude-repl-command-prefix` after load** |
| 5 | 39 | `claude-repl-send-postfix` | `"\n what do you think? ..."` | Forces analyze-only instruction on C-RET |
| 6 | 44 | `claude-repl-paste-delay` | `0.25` | Fixed delay, no size-based calculation |
| 7 | 49-50 | `claude-repl-header-line` | White/black/bold | No theme-specific variants |
| 8 | 537 | `claude-repl--slash-stack` (local) | `nil` | Empty slash stack |
| 9 | 185 | `claude-repl-scroll-lines` | `15` | Fixed scroll quantum |
| 10 | 291-293 | `claude-repl-metaprompt-exempt-strings` | 4 slash commands | Not extensible via customize |
| 11 | 305-308 | `claude-repl-send-posthooks` | `/clear` -> reset counter | Single-entry alist |
| 12 | 395 | `claude-repl--bracketed-paste-threshold` | `200` chars | `defconst`, not configurable |
| 13 | 55-59 | `claude-repl--backspace-commands` | 4 commands | `defconst`, not extensible |

---

## Section 2: `or` Expressions (6 instances)

| # | Line | Code | Fallback chain |
|---|---|---|---|
| 14 | 338 | `(or (claude-repl--ws-get ws :prefix-counter) 0)` | Counter -> 0 (first prompt always gets metaprompt) |
| 15 | 417 | `(or (claude-repl--ws-get ws :prefix-counter) 0)` | **Duplicate of #14** |
| 16 | 473 | `(or ws (+workspace-current-name))` | Explicit ws -> current workspace -> error |
| 17 | 479 | `(or prompt (claude-repl--read-input-buffer ws))` | Explicit prompt -> buffer contents -> nil (silent no-op) |
| 18 | 331 | `(or force (zerop (mod ...)))` | Force flag -> counter check |
| 19 | 300 | `(or (member ...) (string-match-p ...))` | Exempt list -> bare numeral check |

---

## Section 3: `if-let` No-Vterm Fallbacks (9 instances)

**Two distinct failure modes:**

### Silent (log only, no user feedback):
| # | Line | Function | Action dropped |
|---|---|---|---|
| 20 | 73-79 | `slash-intercept-backspace` | Backspace to vterm |
| 22 | 140-145 | `send-vterm-key` | Arrow key forward |
| 23 | 162-167 | `send-vterm-down` | History-down scroll |
| 24 | 170-177 | `send-vterm-up` | History-up scroll |
| 25 | 522-528 | `send-char` | Single-char send (y/n) |

### Loud (user-visible `message`):
| # | Line | Function | Action |
|---|---|---|---|
| 21 | 114-121 | `vterm-send-raw-ctrl-c` | Error message + returns nil |
| 26 | 594-600 | `slash-vterm-send` | Error message + returns nil |
| 27 | 643-655 | `slash-backspace` | Error + exits slash mode |
| 28 | 688-694 | `slash-return` | Error + exits slash mode regardless |

---

## Section 4: `when-let` Patterns (2 instances)

| # | Line | Falls back TO |
|---|---|---|
| 29 | 462-464 | `nil` (no input buffer or dead buffer) |
| 30 | 507-510 | `nil` (text silently discarded) |

---

## Section 5: Conditional Guards (10 instances)

Key instances:

- **Line 67:** Backspace command recognition via `memq`
- **Lines 68-80:** Three-level cascade: slash mode -> empty buffer -> non-empty buffer
- **Lines 357-361:** Deferred action buffer liveness check (paste pipeline)
- **Lines 474-486:** `claude-repl--send` -- `unless` blocks are log-only, NOT early returns
- **Lines 724-740:** `claude-repl--passthrough-start` -- most defensive function, three fallback paths, keystroke NEVER lost

---

## Section 6: `&optional` Parameters (4 instances)

Lines 466, 335, 325, 447 -- `prompt`, `ws`, `force-metaprompt`, `clear-p`

---

## Section 7: Other Notable Instances

### Instance 45: Slash mode toggle
- **Line(s):** 553-555
- `kill-local-variable` restores global default, losing any prior buffer-local value

### Instance 46: Workspace name fallback in error message
- **Line:** 585
- **Code:** `(or ws "<none>")`

### Instance 47: Short-circuit `and` for metaprompt decision
- **Lines:** 328-331
- Four conditions, any falsy one suppresses metaprompt

---

## Key Architectural Observations

1. **Silent vs. loud failure split**: Slash-mode functions use `claude-repl--slash-no-vterm-error` (user-visible). Non-slash functions only log. User actions are silently dropped without feedback.

2. **Duplicated counter fallback**: Instances 14 and 15 both independently fall back to `0`. Could diverge if one is changed without the other.

3. **`claude-repl--send` guard structure is unusual**: The `unless` forms on lines 480-485 are log-only, NOT early returns. The real guard is the `when` on line 486.

4. **`passthrough-start`** (Instance 40) is the most defensively coded function -- three explicit fallback paths and a race-condition guard, all ensuring the user's keystroke is never lost.

5. **Load-time bake** (Instance 4): `claude-repl--command-prefix` is formatted once at load. Post-load customization of `claude-repl-command-prefix` has no effect.

---

## Summary

| Category | Count |
|---|---|
| `defcustom`/`defvar`/`defconst`/`defface` defaults | 13 |
| `or` fallback expressions | 6 |
| `if-let` no-vterm fallbacks | 9 |
| `when-let` nil-return fallbacks | 2 |
| Guards (`when`/`unless`/`if`/`cond`) | 10 |
| `&optional` implicit nil defaults | 4 |
| Minor mode toggle / display / control-flow | 6 |
| **Total** | **50** |
