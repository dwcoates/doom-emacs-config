;;; window.el --- Centralized window-management helpers for claude-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Single home for window-management primitives shared across the
;; claude-repl package.  Before this module existed, identical
;; recipes for window hardening, subset-deletion, and side-window
;; awareness were inlined into panels.el, drawer.el, magit.el,
;; session.el, and keybindings.el — drift across the copies turned
;; simple operations (e.g. `SPC g g' opening magit) into surprising
;; side-effects on siblings (e.g. the workspace drawer being
;; deleted).
;;
;; Conceptual scope:
;;
;;   • Panel registry (data describing each known panel kind).
;;   • Window finders (resolve a panel kind to a live window).
;;   • Hardening (dedicate, size-fix, delete-protect, preserve-size).
;;   • Subset deletion (`--delete-where' with side-window skipping by
;;     default — the SPC g g class of bug is fixed at this layer).
;;   • Focus + buffer-current dispatch (the two converging styles
;;     today seen in panels.el vs drawer.el).
;;   • Layout transactions (`--ensure-layout' for declarative target
;;     layouts that diff against current and apply minimum changes).
;;   • Per-workspace window-configuration save/restore.
;;   • Centralized hook install/uninstall.
;;
;; Out of scope (each lives in its own module and CALLS helpers here):
;;
;;   • Persp save/restore policy (panels.el / persp integration).
;;   • Hide-overlay (a buffer overlay, not a window).
;;   • Tab-bar shuffle.
;;   • Magit advice glue.
;;
;; Today the module ships only the side-window-aware subset-deletion
;; primitive — that single helper retires the dedicated SPC-g-g bug
;; and provides the foundation for migrating the other inlined window
;; recipes in subsequent steps.

;;; Code:

(require 'cl-lib)

;;;; --- Panel registry -----------------------------------------------------
;;
;; Reserved: populated in a follow-up step when callers are migrated.
;; The intended shape is:
;;
;;   '((:vterm  :side right :buffer-key :vterm-buffer
;;              :width-frac claude-repl-vterm-width-fraction
;;              :harden (dedicate size-fix-width  delete-protect))
;;     (:input  :side below :buffer-key :input-buffer
;;              :height-frac claude-repl-input-height-fraction
;;              :harden (dedicate size-fix-height delete-protect preserve-size))
;;     (:drawer :side left  :buffer (claude-repl-drawer-buffer-name)
;;              :width-fn   claude-repl-drawer--window-width
;;              :harden (dedicate delete-protect)))

;;;; --- Side-window awareness ---------------------------------------------

(defun claude-repl-window--side-window-p (win)
  "Return non-nil if WIN is a side window.
A side window is one created via `display-buffer-in-side-window' (or
equivalent), distinguished by a non-nil `window-side' window
parameter.  Treat these as exempt from generic layout-clearing
operations so commands that reset the main window tree (e.g.
`delete-other-windows', `+dwc/magit-status-workspace') don't trample
frame-level UI elements like the workspace drawer."
  (and (window-live-p win)
       (window-parameter win 'window-side)))

;;;; --- Per-window hardening ----------------------------------------------

(cl-defun claude-repl-window--harden
    (win &key
         dedicate
         size-fix
         delete-protect
         preserve-size
         no-other-window
         fringes)
  "Apply a hardening recipe to WIN.

Centralizes the dedicate/size-fix/delete-protect/preserve-size
combinations that panel-style windows (vterm, input, drawer) all
need.  Before this helper existed, the same four-line recipe was
inlined into 3+ call sites with subtle drift.

Each keyword is independent and may be omitted:

  :DEDICATE        non-nil → `set-window-dedicated-p WIN t'.

  :SIZE-FIX        Symbol passed to `set-window-parameter WIN
                   'window-size-fixed'.  Accepted values match
                   Emacs's `window-size-fixed' contract: `width',
                   `height', or `t' (both).  Window parameter (not
                   buffer-local) so the lock is per-window even
                   when the same buffer appears elsewhere.

  :DELETE-PROTECT  non-nil → sets `no-delete-other-windows' window
                   parameter so `delete-other-windows' refuses to
                   remove WIN.  Note: this is NOT enough on its
                   own — direct `delete-window' calls bypass this
                   parameter; the side-window-aware `--delete-where'
                   covers the rest.

  :PRESERVE-SIZE   Symbol controlling `window-preserve-size'.
                   Accepted values: `width', `height', `t' (both).
                   This is the only way to defend a window's size
                   against `window--resize-mini-window' (which
                   ignores `window-size-fixed' alone — see
                   `panels.el' input-panel comment for the gory
                   detail).

  :NO-OTHER-WINDOW non-nil → sets the `no-other-window' parameter
                   so `other-window' (keyboard nav) skips WIN.

  :FRINGES         nil (leave alone) | integer N (both fringes to
                   N px) | cons cell (LEFT . RIGHT).  Common
                   shorthand: pass 0 to hide both fringes (used by
                   the drawer to suppress the wrap-continuation
                   glyph)."
  (when (and (window-live-p win) dedicate)
    (set-window-dedicated-p win t))
  (when (and (window-live-p win) size-fix)
    (set-window-parameter win 'window-size-fixed size-fix))
  (when (and (window-live-p win) delete-protect)
    (set-window-parameter win 'no-delete-other-windows t))
  (when (and (window-live-p win) no-other-window)
    (set-window-parameter win 'no-other-window t))
  (when (and (window-live-p win) preserve-size)
    (cond
     ((eq preserve-size 'width)
      (window-preserve-size win t t))
     ((eq preserve-size 'height)
      (window-preserve-size win nil t))
     ((eq preserve-size t)
      (window-preserve-size win t t)
      (window-preserve-size win nil t))))
  (when (and (window-live-p win) fringes)
    (cond
     ((integerp fringes)
      (set-window-fringes win fringes fringes nil))
     ((and (consp fringes)
           (integerp (car fringes))
           (integerp (cdr fringes)))
      (set-window-fringes win (car fringes) (cdr fringes) nil)))))

;;;; --- Subset deletion ---------------------------------------------------

(cl-defun claude-repl-window--delete-where
    (predicate &key (skip-side-windows t) frame)
  "Delete each live window for which PREDICATE returns non-nil.

PREDICATE is called with the window as its single argument.  Returns
the list of windows that were deleted (useful for callers wanting to
verify the sweep took effect).

When SKIP-SIDE-WINDOWS is non-nil (the default), windows with a
non-nil `window-side' parameter are unconditionally preserved
regardless of PREDICATE.  This is the side-window-aware default that
prevents layout-clearing commands from destroying frame-level UI
elements like the workspace drawer.  Pass nil ONLY when the caller
genuinely wants to clear side windows too (e.g. a full-frame
fullscreen toggle that should reduce to a single panel — and even
then, consider whether the caller should instead restore the
drawer afterward).

FRAME limits the scan; defaults to the selected frame.  Deletion is
wrapped in `condition-case' so one undeletable window doesn't abort
the sweep — the error is logged via `message' and iteration
continues."
  (let ((deleted '()))
    (dolist (win (window-list frame))
      (when (and (window-live-p win)
                 (or (not skip-side-windows)
                     (not (claude-repl-window--side-window-p win)))
                 (funcall predicate win))
        (condition-case err
            (progn
              (delete-window win)
              (push win deleted))
          (error
           (message "[claude-repl] window--delete-where: could not delete %s: %S"
                    win err)))))
    (nreverse deleted)))

(provide 'claude-repl-window)
;;; window.el ends here
