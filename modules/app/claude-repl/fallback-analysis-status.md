# Fallback and Default Logic Report: `status.el`

**Total instances: 56**

---

## Section 1: `or` Expression Fallbacks (10 instances)

### Key instances:

| # | Line | Code | Falls back TO |
|---|---|---|---|
| 2 | 16 | `(or load-file-name buffer-file-name)` | Buffer file for interactive eval |
| 3 | 112 | `(or (claude-repl--ws-get ws :project-dir) (error ...))` | **Hard error** (crash-not-degrade) |
| **4** | **312-313** | **`(or (plist-get row key) (plist-get claude-repl--tab-default key))`** | **Default tab appearance for unlisted states** |
| 5 | 365-366 | `(or (plist-get ... :label) (number-to-string index))` | Numeric index (only `:permission` has a label) |
| 6 | 376-377 | `(or ... :face-override) '+workspace-tab-selected-face)` | Doom's default selected-tab face |
| 7 | 378-379 | `(or ... :face) '+workspace-tab-face)` | Doom's default unselected-tab face |
| 8-11 | 350-353 | `:bg`/`:fg`/`:bracket-fg`/`:weight` fallbacks | `'unspecified` or `'normal` |

---

## Section 2: Conditional Guards (21 instances)

### Critical instances:

### Instance 18: `workspace-clean-p` defaults to clean
- **Line:** 122
- **Code:** `(not (eq (claude-repl--ws-get ws :git-clean) 'dirty))`
- **Consequence:** When `:git-clean` cache is nil (not yet populated), workspace appears "clean". **Causes premature `:done` to `:idle` tab decay** before git check completes.

### Instance 21: Git exit code misinterpretation
- **Line:** 140
- **Code:** `(if (= 0 exit-code) 'clean 'dirty)`
- **Consequence:** ANY non-zero exit (including git errors 128, 129) is treated as "dirty". **Keeps tabs permanently green** on git failures.

### Instance 29: `composed-state` catch-all
- **Line:** 404
- **Code:** `(t nil)` in `cond`
- **Consequence:** Unrecognized states (including `:idle`) silently get default appearance.

### Instance 30: `update-ws-state` catch-all
- **Line(s):** 514-516
- Only `:done`+clean is actionable; everything else is a no-op.

### Instance 25: Window config traversal
- **Line:** 457
- **Code:** `(when (and wconf (proper-list-p wconf)) ...)`
- Guards against nil or malformed window configurations.

---

## Section 3: `&optional` with `names-supplied-p`

### Instance 33: `claude-repl--tabline-advice`
- **Line(s):** 426-428
- **Three-way behavior:** no arg = dynamic fetch, explicit nil = empty tabline, explicit list = use list.

---

## Section 4: Hardcoded Color Constants (12 instances)

| # | Line | Variable | Value |
|---|---|---|---|
| 34 | 209 | `--color-init-blue` | `"#3366cc"` |
| 35 | 212 | `--color-thinking-red` | `"#cc3333"` |
| 36 | 215 | `--color-done-green` | `"#1a7a1a"` |
| 37 | 218 | `--color-done-green-bright` | `"#2a8c2a"` |
| 38 | 222 | `--color-default-bracket` | `"#4477cc"` |
| 39 | 225 | `--color-selected-bg` | `"#c0c0c0"` |
| 40 | 228 | `--color-light` | `"white"` |
| 41 | 231 | `--color-dark` | `"black"` |
| 42 | 234 | `--label-permission` | `"?"` |
| 43 | 238 | `--tab-weight` | `'bold` |
| 44 | 243-252 | `--tab-default` | Full default spec |
| 45 | 254-303 | `--tab-palette` | Four-entry palette |

---

## Section 5: `defface` Defaults (4 instances)

| # | Line | Face | Colors |
|---|---|---|---|
| 46 | 320-323 | `claude-repl-tab-init` | Blue/white/bold |
| 47 | 326-329 | `claude-repl-tab-thinking` | Red/white/bold |
| 48 | 331-334 | `claude-repl-tab-done` | Dark-green/black/bold |
| 49 | 338-341 | `claude-repl-tab-permission` | Dark-green/black/bold |

---

## Section 6: Implicit Defaults (7 instances)

- **Instance 50:** `make-process :buffer nil` -- discard git output
- **Instance 53:** Timer interval `(run-with-timer 1 1 ...)` -- hardcoded 1-second poll
- **Instance 55:** Priority image double `when-let` -- three nil cases all produce no image
- **Instance 56:** Conditional image insertion -- clean omission when no image

---

## Most Consequential

1. **Instance 18** (defaults to clean) -- race condition causing premature tab decay
2. **Instance 21** (git error = dirty) -- permanently green tabs on git failures
3. **Instance 4** (palette fallback to tab-default) -- central rendering fallback
4. **Instance 29** (composed-state catch-all) -- silently collapses unrecognized states
5. **Instance 33** (three-way optional behavior) -- subtle, could trip up callers
