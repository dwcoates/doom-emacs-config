# Fallback and Default Logic Report: `doctor.el`

**Total instances: 9**

---

## Instance 1: Guarded load of `install.el` via `file-exists-p`

**Line(s):** 12-13

```elisp
(when (file-exists-p install-el)
  (load install-el nil t))
```

**Falls back TO:** Doing nothing. When the file does not exist, the entire doctor check infrastructure is silently skipped.

---

## Instance 2: `load` called with implicit defaults

**Line:** 13 -- The omitted fourth argument (MUST-SUFFIX) defaults to `nil`.

---

## Instance 3: `and` short-circuit as guarded function call

**Line(s):** 15-16

```elisp
(and (fboundp 'claude-repl--doctor-issues)
     (claude-repl--doctor-issues))
```

**Falls back TO:** `nil`. If `fboundp` returns nil, the `dolist` iterates over nil -- zero iterations, no diagnostics.

---

## Instance 4: `pcase` with catch-all `_` branch

**Line(s):** 17-21

```elisp
(pcase (car issue)
  ('error (if (fboundp 'error!)
              (error! "%s" (cdr issue))
            (warn! "FATAL: %s" (cdr issue))))
  (_      (warn! "%s" (cdr issue))))
```

**Falls back TO:** Any non-`error` level (including `warn`, `info`, nil, or malformed values) becomes a `warn!` call.

---

## Instance 5: Fallback from `error!` to `warn!`

**Line(s):** 18-20

```elisp
(if (fboundp 'error!)
    (error! "%s" (cdr issue))
  (warn! "FATAL: %s" (cdr issue)))
```

**Falls back TO:** `warn!` with `"FATAL: "` prefix when `error!` is not defined. Machine-readable severity is lost.

---

## Instance 6: `expand-file-name` relative to `load-file-name` directory

**Line(s):** 10-11

If `load-file-name` is nil (interactive evaluation), `file-name-directory` returns nil, and `expand-file-name` resolves against `default-directory` -- almost certainly wrong.

---

## Instance 7: `dolist` over a potentially nil list

**Line:** 15 -- `dolist` iterates zero times when the list is nil. "No issues found" and "could not check for issues" are indistinguishable.

---

## Instance 8: Absence of fallback for `warn!`

**Lines:** 20, 21 -- `warn!` is used unconditionally with no `fboundp` guard (unlike `error!`). Would signal `void-function` error outside `doom doctor`.

---

## Instance 9: Implicit `lexical-binding` default

**Line:** 1 -- File-local variable overrides Emacs's default dynamic binding.

---

## Summary

| # | Type | Silent? |
|---|---|---|
| 1 | `when` guard | Yes |
| 2 | Omitted `load` arg | Yes |
| 3 | `and` short-circuit | Yes |
| 4 | `pcase` catch-all `_` | Yes |
| 5 | `if` / `fboundp` guard | No (visible change) |
| 6 | Implicit Emacs default | Yes |
| 7 | `dolist` over nil | Yes |
| 8 | Absence of fallback | N/A |
| 9 | File-local variable | N/A |

**Dominant pattern: silent degradation.** Instances 1, 3, and 7 form a cascading chain where any single failure causes the entire doctor check to silently produce no output, making "no news" ambiguous between "all checks passed" and "no checks ran."
