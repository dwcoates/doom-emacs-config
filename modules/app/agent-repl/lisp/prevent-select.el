;;; prevent-select.el --- Hide Claude panels from interactive buffer cycling -*- lexical-binding: t; -*-

;;; Commentary:

;; Make Claude panel buffers (the input composer and the webview)
;; unselectable through the interactive buffer-cycling commands.
;; Emacs's `next-buffer', `previous-buffer', and the
;; replacement-picker that runs after `kill-buffer' (and bury-buffer)
;; all consult `switch-to-prev-buffer-skip'.  We install a predicate
;; there so those paths skip Claude panels and fall through to the
;; next eligible buffer in the window's history.
;;
;; Programmatic selection (`switch-to-buffer', `display-buffer',
;; `pop-to-buffer-same-window' invoked by code) is unaffected — those
;; functions never consult the skip predicate, so the rest of the
;; agent-repl machinery that puts panels into their own dedicated
;; windows keeps working.

;;; Code:

(require 'cl-lib)

(defun agent-repl--prev-buffer-skip-agent-panel (_window buffer bury-or-kill)
  "Predicate for `switch-to-prev-buffer-skip'.
Return non-nil when BUFFER is a Claude panel buffer (the input
composer or the webview) so it is skipped by `next-buffer',
`previous-buffer', and the replacement chooser invoked after
`kill-buffer'."
  (let* ((live-p (buffer-live-p buffer))
         (ws (and live-p (agent-repl--buffer-owner buffer)))
         (panel-p (and live-p (agent-repl--agent-panel-buffer-p buffer))))
    ;; This predicate can run once for every candidate in a window's history.
    ;; Keep its branch trace behind verbose logging so buffer cycling remains
    ;; cheap outside an explicitly requested diagnostic session.
    (agent-repl--log-verbose
     ws
     "prevent-select: candidate buffer=%S live=%s panel=%s bury-or-kill=%S skip=%s"
     (and live-p (buffer-name buffer)) live-p panel-p bury-or-kill panel-p)
    panel-p))

(setq switch-to-prev-buffer-skip
      #'agent-repl--prev-buffer-skip-agent-panel)

(agent-repl--log nil
                  "prevent-select: installed switch-to-prev-buffer-skip predicate=%S"
                  switch-to-prev-buffer-skip)

(provide 'prevent-select)

;;; prevent-select.el ends here
