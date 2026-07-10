;;; close-panels-on-open.el --- Close Claude panels when opening a buffer/file -*- lexical-binding: t; -*-

;;; Commentary:

;; When the Claude REPL panels fill the frame and the user opens a
;; buffer or file by any of the usual means (`SPC ,', `SPC <', `SPC .',
;; `SPC f f', `SPC p f', `M-x switch-to-buffer', `M-x find-file', etc.),
;; the panels should first close — restoring the pre-panel layout — and
;; the buffer/file should then open in whatever window the close leaves
;; selected.
;;
;; "Any other means" is covered by advising the low-level primitives that
;; virtually every interactive buffer/file open funnels through rather
;; than enumerating each leader command:
;;
;;   - `switch-to-buffer'        — `SPC <', `M-x switch-to-buffer', and
;;                                 the consult/vertico workspace-buffer
;;                                 pickers behind `SPC ,' (their selection
;;                                 action calls it).
;;   - `pop-to-buffer-same-window' — the same-window display primitive
;;                                 `find-file' itself calls, plus other
;;                                 same-window opens.
;;   - `find-file'               — `SPC .', `SPC f f', and `SPC p f'
;;                                 (projectile) all route through it.
;;
;; The advice is a :before hook so the panels are gone before the wrapped
;; command picks the window it will display in.
;;
;; Three guards keep panel machinery itself unaffected:
;;
;;   1. No-op unless both panels are currently visible
;;      (`agent-repl--panels-visible-p').
;;   2. No-op when the open target is itself a Claude panel buffer — this
;;      is how the machinery places panels into their dedicated windows
;;      via `switch-to-buffer'/`pop-to-buffer-same-window' (see
;;      prevent-select.el's note on programmatic selection).
;;   3. A re-entrancy guard so the layout-restoring close — which itself
;;      switches buffers and so re-enters the advised primitives — does
;;      not recurse back into this advice.
;;
;; The close routes through `agent-repl--simple-hide-and-preserve-status'
;; — the same plain close bound to `SPC o c' — so the pre-panel layout is
;; restored (the panels fill the frame, so a bare window delete would
;; strand a panel) and `:repl-state' becomes :inactive.

;;; Code:

(require 'cl-lib)

(defcustom agent-repl-close-panels-on-open-fns
  '(switch-to-buffer
    pop-to-buffer-same-window
    find-file)
  "Functions that should close visible Claude panels before opening.
Each is advised :before with `agent-repl--close-panels-before-open'.
These three primitives are the funnel for essentially every
interactive buffer/file open — see the commentary at the top of
close-panels-on-open.el for the key-to-primitive mapping."
  :type '(repeat symbol)
  :group 'agent-repl)

(defvar agent-repl--closing-panels-for-open nil
  "Non-nil while `agent-repl--close-panels-before-open' is closing panels.
Prevents the layout-restoring close — which switches buffers and so
re-enters the advised primitives — from recursing back into the
advice.")

(defun agent-repl--open-target-is-panel-p (args)
  "Return non-nil when ARGS's first element resolves to a live Claude panel buffer.
ARGS is the argument list passed to an advised open function.  Only a
buffer object or the name of an existing buffer counts as a target — a
filename string handed to `find-file' resolves to no buffer and so is
treated as a non-panel target, which is correct because files are
never panels.  This lets the advice skip the programmatic
`switch-to-buffer'/`pop-to-buffer-same-window' calls that panel
machinery uses to place panels into their own windows."
  (let* ((arg (car args))
         (buf (cond ((bufferp arg) arg)
                    ((stringp arg) (get-buffer arg)))))
    (and buf (buffer-live-p buf) (agent-repl--claude-panel-buffer-p buf))))

(defun agent-repl--close-panels-before-open (&rest args)
  "Close Claude REPL panels before an interactive buffer/file open.
Installed as :before advice on `agent-repl-close-panels-on-open-fns'.
No-op unless both panels are currently visible and the open target is
not itself a Claude panel buffer (see
`agent-repl--open-target-is-panel-p').  Routes through
`agent-repl--simple-hide-and-preserve-status' (the `SPC o c' close)
so the pre-panel layout is restored and `:repl-state' becomes
:inactive; the wrapped open then displays in whatever window that
close leaves selected."
  (unless agent-repl--closing-panels-for-open
    (when (and (agent-repl--panels-visible-p)
               (not (agent-repl--open-target-is-panel-p args)))
      (let ((agent-repl--closing-panels-for-open t))
        (agent-repl--simple-hide-and-preserve-status)))))

(dolist (fn agent-repl-close-panels-on-open-fns)
  (advice-add fn :before #'agent-repl--close-panels-before-open))

(provide 'close-panels-on-open)

;;; close-panels-on-open.el ends here
