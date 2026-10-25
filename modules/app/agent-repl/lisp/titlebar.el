;;; titlebar.el --- Emacs's title bar turns orange while Emacs is unfocused -*- lexical-binding: t; -*-

;;; Commentary:

;; WHILE EMACS IS NOT THE FOCUSED APPLICATION, THE TITLE BAR OF EVERY EMACS
;; WINDOW IS ORANGE, so Emacs's window is the one spotted at a glance among
;; the desktop's windows (macOS Mission Control shows them all at once).  A
;; focus gain takes it away.  Only the bar changes: the web view and the
;; rest of the frame keep their colors.
;;
;; macOS draws a transparent title bar (`ns-transparent-titlebar', which
;; Doom's ns-auto-titlebar turns on) in the window's background color, and
;; Emacs offers no other handle on that color than the frame's
;; `background-color'.  So an unfocused frame's background is set orange,
;; and the one other surface that would show it, the internal border, is
;; pinned to the frame's color from before.  Every buffer the frame shows
;; draws its own background, so none turns orange.
;;
;; THE FOCUSED COLOR IS NEVER STORED.  A focus gain unpins the border and
;; recalculates the default face from its spec (`face-spec-recalc', exactly
;; what a theme load does to every frame), so the bar returns to whatever
;; the theme says now.  A theme loaded while unfocused recalculates the
;; frames itself and leaves the border pinned to the old color, so the
;; theme hook unpaints every painted frame before painting afresh.
;;
;; The paint follows Emacs's focus on every event that can change what a
;; frame should show: a settled focus change, a new frame, and a theme
;; load.  Child frames (posframes) and frames on other platforms have no
;; title bar Emacs could color, and are never touched.

;;; Code:

(require 'seq)

(declare-function agent-repl--emacs-focused-p "notifications" (&optional ws))
(declare-function agent-repl-status--color-value "status" (color))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(defvar agent-repl--focus-log-scope)
(defvar agent-repl-focus-settled-hook)

(defconst agent-repl-titlebar-unfocused-color (agent-repl-status--color-value "orange")
  "The title bar's color while Emacs is unfocused.
Render-colors.json\='s dark `orange\=', the webapp\='s `--compact-rule\=', so
the editor\='s one orange is one color everywhere.")

(defconst agent-repl--titlebar-painted 'agent-repl-titlebar-painted
  "The frame parameter a frame whose title bar is orange holds non-nil.")

(defun agent-repl--titlebar-frames ()
  "Return every live frame whose title bar Emacs can color.
A top-level macOS frame: a child frame has no title bar, and no other
platform draws its title bar in the frame\='s background."
  (seq-filter (lambda (frame)
                (and (eq (framep frame) 'ns)
                     (not (frame-parameter frame 'parent-frame))))
              (frame-list)))

(defun agent-repl--titlebar-paint (frame)
  "Turn FRAME\='s title bar orange, keeping its internal border its color.
A frame already painted is left as it is."
  (unless (frame-parameter frame agent-repl--titlebar-painted)
    (let ((background (face-background 'default frame)))
      (agent-repl--log agent-repl--focus-log-scope
                       "elisp.titlebar.paint frame=%S background=%s color=%s"
                       frame background agent-repl-titlebar-unfocused-color)
      (set-face-attribute 'internal-border frame :background background)
      ;; THE FRAME KEEPS ITS LIGHT OR DARK MODE.  Setting `background-color'
      ;; has Emacs derive the frame's `background-mode' from the new color
      ;; (`frame-set-background-mode'), and the orange reads as light, so
      ;; every face with a light and a dark spec switched to its light one:
      ;; the whole theme changed while Emacs was unfocused.  The variable,
      ;; non-nil, is what that derivation takes instead of the color.
      (let ((frame-background-mode (frame-parameter frame 'background-mode)))
        (set-frame-parameter frame 'background-color agent-repl-titlebar-unfocused-color))
      (set-frame-parameter frame agent-repl--titlebar-painted t))))

(defun agent-repl--titlebar-unpaint (frame)
  "Return FRAME\='s title bar to the theme\='s color.
A frame not painted is left as it is."
  (when (frame-parameter frame agent-repl--titlebar-painted)
    (agent-repl--log agent-repl--focus-log-scope "elisp.titlebar.unpaint frame=%S" frame)
    (set-face-attribute 'internal-border frame :background 'unspecified)
    (face-spec-recalc 'default frame)
    (set-frame-parameter frame agent-repl--titlebar-painted nil)))

(defun agent-repl-titlebar-sync (&rest _)
  "Paint every title bar for Emacs\='s focus now.
Orange while Emacs is unfocused, the theme\='s color while it is focused.
The focus is read at the call, never kept.  Takes and ignores the
argument `after-make-frame-functions\=' hands it."
  (let ((focused (agent-repl--emacs-focused-p))
        (frames (agent-repl--titlebar-frames)))
    (agent-repl--log agent-repl--focus-log-scope
                     "elisp.titlebar.sync focused=%s frames=%d" (and focused t) (length frames))
    (dolist (frame frames)
      (if focused
          (agent-repl--titlebar-unpaint frame)
        (agent-repl--titlebar-paint frame)))))

(defun agent-repl--titlebar-on-theme (_theme)
  "Repaint every title bar after a theme load.
The load recalculated every frame\='s background but left a painted
frame\='s border pinned to the old theme\='s color, so each painted frame is
unpainted before the paint is taken afresh."
  (dolist (frame (agent-repl--titlebar-frames))
    (agent-repl--titlebar-unpaint frame))
  (agent-repl-titlebar-sync))

(add-hook 'agent-repl-focus-settled-hook #'agent-repl-titlebar-sync)
(add-hook 'after-make-frame-functions #'agent-repl-titlebar-sync)
(add-hook 'enable-theme-functions #'agent-repl--titlebar-on-theme)
;; An Emacs started behind another application (`open -gj') sees no focus
;; change until it is brought forward, so its frames are painted now.
(agent-repl-titlebar-sync)

(provide 'titlebar)
;;; titlebar.el ends here
