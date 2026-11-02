;;; frame-border.el --- One owner of a frame's internal border -*- lexical-binding: t; -*-

;;; Commentary:

;; THE FRAME'S INTERNAL BORDER HAS ONE OWNER, this file.  The border is the
;; strip round a frame's edge (two pixels on the main frame), beneath the
;; echo area's last row among others, drawn in the frame's `internal-border'
;; face.  Two states decide its color, each recorded by the module that
;; owns it, and this file alone sets the face:
;;
;;   - a frame showing agent-repl's panels (dividers.el) wears the ground,
;;     `agent-repl-input-background', round its edge, so the strip under
;;     the echo area is the sidebar's color (it wore the theme's #1d1f21,
;;     measured 2026-11-02);
;;   - otherwise a frame whose title bar is orange while Emacs is unfocused
;;     (titlebar.el) keeps the color it had before the orange;
;;   - otherwise the border has no color of its own, and is the frame's.
;;
;; Each module once set the face itself, so whichever wrote last won: a
;; focus gain unpinned the border a frame showing panels had painted the
;; ground.  Now each records its state through a setter here, and every
;; setter recomputes the one color from both, so no order of writes can
;; leave the border wrong.  A change of the ground and a theme load each
;; sync every frame (`agent-repl-input-background-painters', input.el).

;;; Code:

(declare-function agent-repl--log "core" (ws fmt &rest args))
(defvar agent-repl-input-background)

(defconst agent-repl--frame-border-log-scope
  '(:agent-repl-central "a frame's border belongs to no one workspace")
  "The central log scope every frame border record is written under.")

(defconst agent-repl--frame-border-panels 'agent-repl-frame-border-panels
  "The frame parameter of a frame showing agent-repl's panels, non-nil.")

(defconst agent-repl--frame-border-pinned 'agent-repl-frame-border-pinned
  "The frame parameter holding the color an orange title bar pinned, or nil.")

(defun agent-repl--frame-border-color (frame)
  "Return the color FRAME's internal border wears, or `unspecified'."
  (cond ((frame-parameter frame agent-repl--frame-border-panels) agent-repl-input-background)
        ((frame-parameter frame agent-repl--frame-border-pinned))
        (t 'unspecified)))

(defun agent-repl-frame-border-sync (frame)
  "Set FRAME's internal border to the color its two states decide."
  (let ((color (agent-repl--frame-border-color frame)))
    (agent-repl--log agent-repl--frame-border-log-scope
                     "elisp.frame-border.sync frame=%S color=%S panels=%S pinned=%S"
                     frame color
                     (frame-parameter frame agent-repl--frame-border-panels)
                     (frame-parameter frame agent-repl--frame-border-pinned))
    (set-face-attribute 'internal-border frame :background color)))

(defun agent-repl--frame-border-record (frame parameter value)
  "Record FRAME's PARAMETER as VALUE and sync its border when it changed."
  (unless (equal (frame-parameter frame parameter) value)
    (set-frame-parameter frame parameter value)
    (agent-repl-frame-border-sync frame)))

(defun agent-repl-frame-border-set-panels (frame shows)
  "Record whether FRAME SHOWS agent-repl's panels, and sync its border.
dividers.el's sync calls it on every window change, so an unchanged
state does nothing."
  (agent-repl--frame-border-record frame agent-repl--frame-border-panels (and shows t)))

(defun agent-repl-frame-border-set-pinned (frame color)
  "Record COLOR as the one FRAME's orange title bar pinned, nil for none.
titlebar.el calls it as it paints and unpaints, and FRAME's border is
synced."
  (agent-repl--frame-border-record frame agent-repl--frame-border-pinned color))

(defun agent-repl-frame-border-sync-all ()
  "Sync every frame's internal border."
  (mapc #'agent-repl-frame-border-sync (frame-list)))

(defun agent-repl--frame-border-on-theme (_theme)
  "Sync every frame's border after a theme load recalculated its faces."
  (agent-repl-frame-border-sync-all))

;; A new ground repaints every frame's border (input.el).
(add-hook 'agent-repl-input-background-painters #'agent-repl-frame-border-sync-all)
(add-hook 'enable-theme-functions #'agent-repl--frame-border-on-theme)
;; A reload finds the frames already holding their states.
(agent-repl-frame-border-sync-all)

(provide 'frame-border)
;;; frame-border.el ends here
