;;; config/doom-tomorrow-night/doom-tomorrow-night-warm-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; `doom-tomorrow-night' on warm grounds, to match agent-repl's warm dark
;; webapp.  Only the two grounds change.  Each keeps the offset it had above
;; the webapp's feed (`--bg' in the webapp's styles.css), so Emacs still
;; reads a touch lighter than the webapp: the old `bg' #1d1f21 sat
;; (+12, +12, +9) above the old feed #111318, and the new `bg' sits the same
;; step above the new feed #151413.  `bg-alt' keeps its old step below `bg'.

(require 'doom-themes)
(require 'doom-tomorrow-night-derive
         (expand-file-name "derive" (file-name-directory
                                     (or load-file-name buffer-file-name))))

(defconst doom-tomorrow-night-warm-grounds
  '((bg     . "#21201c")
    (bg-alt . "#1a1814"))
  "The palette entries `doom-tomorrow-night-warm' swaps, as (VAR . GUI).")

(dolist (form (doom-tomorrow-night-derive-theme
               (doom-tomorrow-night-read-forms
                (or (locate-library "doom-tomorrow-night-theme.el" t)
                    (error "doom-tomorrow-night-warm: upstream doom-tomorrow-night-theme.el is not on the load-path")))
               'doom-tomorrow-night-warm
               doom-tomorrow-night-warm-grounds))
  (eval form t))
