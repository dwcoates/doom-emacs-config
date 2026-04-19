# Fallback and Default Logic Report: `overlay.el`

**Total instances: 25**

---

## Section 1: Variable Defaults (6 instances)

| # | Line | Variable | Default | Consequence |
|---|---|---|---|---|
| 1 | 5 | `claude-repl--hide-overlay-line-count` | `4` | Hardcoded, not configurable |
| 2 | 8 | `claude-repl--hide-overlay-refcount` | `0` | "No sessions registered" |
| 3 | 12 | `claude-repl-hide-overlay` (local) | `nil` | "No overlay in this buffer" |
| 4 | 15-18 | `claude-repl-hide-input-box` | `nil` | **Feature is OFF by default** (gates entire overlay system) |
| 5 | 20-21 | `claude-repl--in-redraw-advice` | `nil` | "Not in redraw" (recursion guard armed) |
| 21 | 134-135 | `claude-repl--vterm-background-grey` | `15` | Very dark grey (#0f0f0f), not configurable |

---

## Section 2: Nil-Guards (8 instances)

| # | Line | Guard | Falls back TO |
|---|---|---|---|
| 6 | 29-32 | Buffer empty check | `nil` (no region to overlay) |
| 7 | 38-43 | Degenerate region check | `nil` (start >= end) |
| 8 | 49-50 | `claude-repl-hide-input-box` check | Skip overlay creation |
| 9 | 52-53 | Nil region guard | Skip overlay creation |
| 12 | 61-67 | Nil/dead overlay guard | Skip deletion, unconditionally set to nil |
| 15 | 84-86 | Nil/dead buffer guard | Skip overlay update |
| 17 | 104-105 | Recursion guard | Skip advice body |
| 18 | 107-108 | Non-Claude buffer guard | Skip overlay logic for all non-Claude vterms |

---

## Section 3: Resolution Fallback

### Instance 13: `claude-repl--resolve-overlay-target-buffer`
- **Line(s):** 73-79
- **Two-tier:** current Claude buffer -> workspace lookup -> nil

### Instance 14: `and` short-circuit
- **Line:** 79
- **Code:** `(and ws (claude-repl--ws-get ws :vterm-buffer))`

---

## Section 4: Defensive Patterns

### Instance 16: `max 0` floor on refcount
- **Line(s):** 115-116
- **Code:** `(max 0 (1- claude-repl--hide-overlay-refcount))`
- **Prevents:** Negative refcount from extra `disable` calls. Without this, advice could never be removed.

### Instance 19: `&rest _` ignoring all arguments
- **Line:** 102
- Advice resilient to `vterm--redraw` signature changes.

---

## Section 5: Around-Advice with Passthrough

### Instance 23: `claude-repl--vterm-color-advice`
- **Line(s):** 163-169
- **Always** calls original function. Only overrides result for Claude buffers requesting default background.
- **Falls back TO:** Original `vterm--get-color` result for all non-matching cases.

### Instance 24: Hardcoded index sentinel `-1`
- **Line:** 155
- Relies on vterm internal convention for default face index.

---

## Section 6: Other

- **Instance 10:** `make-overlay` hardcoded boolean arguments
- **Instance 11:** `overlay-put 'display ""` -- empty string hides text
- **Instance 20:** Toggle message: `(if claude-repl-hide-input-box "enabled" "disabled")`
- **Instance 22:** `claude-repl--grey-hex` produces only greyscale colors
- **Instance 25:** `nil` as workspace argument in 17 log call sites

---

## Summary

| Category | Count |
|---|---|
| Variable defaults | 6 |
| Nil-guards (early returns) | 8 |
| Resolution fallback | 2 |
| Defensive clamping | 1 |
| Around-advice passthrough | 1 |
| Hardcoded format/display/sentinel | 4 |
| Ignored arguments | 1 |
| Nil workspace in logs | 1 |
| Other | 1 |
| **Total** | **25** |

**Most architecturally significant:** Instance 4 (`defcustom nil`) gates the entire feature. Instance 16 (`max 0` floor) prevents unrecoverable state.
