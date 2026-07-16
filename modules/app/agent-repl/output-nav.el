;;; output-nav.el --- cycle the output feed from the input box -*- lexical-binding: t; -*-

;;; Commentary:

;; Keyboard navigation of the GUI frontend's output feed, driven entirely
;; from the input buffer below it.
;;
;; The output window is not an Emacs text buffer -- it is an xwidget
;; WKWebView hosting the webapp (`frontend.el'), whose feed is DOM, so
;; there is no point to move and no overlay to hang a marker on.  The one
;; channel Emacs has into it is evaluating JavaScript against the live
;; document, exactly as the snap-to-tail hook does
;; (`agent-repl--frontend-tail-script').  So these commands do not
;; navigate anything themselves: each names a bubble class and a
;; direction, and the page's own cycle (webapp/src/nav.ts) does the work.
;;
;; Focus never moves.  The chords fire in the input buffer, Emacs injects
;; a script, the webview scrolls and marks the bubble it landed on, and
;; the user is still typing where they were.
;;
;; The class/direction vocabulary is the webapp's, mirrored here rather
;; than reinvented: one command per (class, direction) pair, all six built
;; by the same macro so a new class is a row in
;; `agent-repl-output-nav-classes' plus a keybinding, never new logic.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-get "workspace")
(declare-function agent-repl--frontend-webview-execute-script "frontend")

(defconst agent-repl-frontend-nav-hook "agentReplNavigate"
  "Name of the webapp global that cycles the feed's bubbles.
The webapp plants it on `window' at boot (`NAV_HOOK' in
webapp/src/nav.ts) -- the two names are one contract and MUST match.")

(defconst agent-repl-output-nav-classes '("prompt" "final" "tool")
  "Bubble classes the feed can be cycled by.
Mirrors `NAV_CLASSES' in webapp/src/nav.ts, which is what actually
resolves them against the DOM; a class absent there is rejected by the
hook rather than silently cycling nothing.")

(defconst agent-repl-output-nav-directions '("next" "prev")
  "Directions a cycle can step, mapped to the webapp's +1/-1.")

(defun agent-repl--output-nav-delta (direction)
  "Return the webapp's numeric step for DIRECTION.
DIRECTION is one of `agent-repl-output-nav-directions'.  An unknown
direction is a bug in this file rather than a runtime condition, so it
raises instead of defaulting to a step the caller never asked for."
  (pcase direction
    ("next" 1)
    ("prev" -1)
    (_ (error "agent-repl: unknown nav direction %S" direction))))

(defun agent-repl--output-nav-script (class direction)
  "Return the JavaScript that cycles the feed's CLASS bubbles DIRECTION.

Guarded on the hook's presence, like `agent-repl--frontend-tail-script':
a webview still navigating has not planted it yet, and that is an
expected state rather than a violated invariant -- a page that has not
booted has no feed to cycle."
  (unless (member class agent-repl-output-nav-classes)
    (error "agent-repl: unknown nav class %S" class))
  (format "window.%s && window.%s('%s', %d);"
          agent-repl-frontend-nav-hook
          agent-repl-frontend-nav-hook
          class
          (agent-repl--output-nav-delta direction)))

(defun agent-repl--output-navigate (class direction)
  "Cycle the current workspace's output feed by CLASS, stepping DIRECTION.

Signals a `user-error' when the workspace has no live webview: a closed
or never-opened output panel is an expected condition (the user can
simply not have one), not a broken invariant."
  (let* ((ws (agent-repl--ws-current-name))
         (buf (and ws (agent-repl--ws-get ws :frontend-buffer))))
    (unless (buffer-live-p buf)
      (user-error "No output window to navigate in this workspace"))
    (agent-repl--frontend-webview-execute-script
     buf (agent-repl--output-nav-script class direction))))

(defmacro agent-repl--define-output-nav-command (class direction doc)
  "Define the interactive command cycling CLASS bubbles DIRECTION.
DOC is its docstring.  One definition site for all six commands, so the
class list and the command surface cannot drift apart."
  (let ((name (intern (format "agent-repl-output-%s-%s" direction class))))
    `(defun ,name ()
       ,doc
       (interactive)
       (agent-repl--output-navigate ,class ,direction))))

(agent-repl--define-output-nav-command
 "prompt" "next"
 "Cycle to the next user prompt in the output feed, wrapping at the end.")
(agent-repl--define-output-nav-command
 "prompt" "prev"
 "Cycle to the previous user prompt in the output feed, wrapping at the start.")
(agent-repl--define-output-nav-command
 "final" "next"
 "Cycle to the next agent response bubble, wrapping at the end.")
(agent-repl--define-output-nav-command
 "final" "prev"
 "Cycle to the previous agent response bubble, wrapping at the start.")
(agent-repl--define-output-nav-command
 "tool" "next"
 "Cycle to the next tool card in the output feed, wrapping at the end.")
(agent-repl--define-output-nav-command
 "tool" "prev"
 "Cycle to the previous tool card in the output feed, wrapping at the start.")

(provide 'agent-repl-output-nav)
;;; output-nav.el ends here
