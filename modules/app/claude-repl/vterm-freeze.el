;;; vterm-freeze.el --- temporary vterm display freeze around scroll -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a buffer-local "freeze" mechanism for vterm buffers.  While
;; a buffer is frozen, `vterm--invalidate' is short-circuited so the
;; visible buffer text and `window-start' are not refreshed in response
;; to incoming process output.  The underlying libvterm state still
;; advances normally (the C `vterm--update' that feeds bytes into
;; libvterm is untouched), so when the freeze lifts a single
;; `vterm--invalidate' call schedules a redraw that catches the buffer
;; up to the live terminal state.
;;
;; This is wired into `claude-repl--scroll-vterm-output' (see
;; `input.el') so that pressing `S-<up>' / `S-<down>' or `C-S-j' /
;; `C-S-k' freezes the Claude vterm for `claude-repl-vterm-freeze-duration'
;; seconds.  Each additional scroll keypress re-arms the timer, so the
;; freeze extends as the user continues scrolling.  After the timer
;; fires the buffer is force-redrawn to reflect any output that arrived
;; during the freeze.

;;; Code:

(defcustom claude-repl-vterm-freeze-duration 1.0
  "Seconds to suspend vterm redraw after a scroll keypress.
Re-armed on every scroll, so successive scroll presses extend the
freeze rather than letting it lapse mid-scroll."
  :type 'number
  :group 'claude-repl)

(defvar-local claude-repl--vterm-frozen nil
  "When non-nil, `vterm--invalidate' is a no-op for the current buffer.
Set by `claude-repl--vterm-freeze-bump' and cleared by
`claude-repl--vterm-unfreeze' once the freeze timer fires.")

(defvar-local claude-repl--vterm-freeze-timer nil
  "Pending unfreeze timer for the current vterm buffer.
Holds the timer object returned by `run-with-timer'; cancelled and
replaced on every fresh scroll-bump.  Nil when no freeze is pending.")

(defun claude-repl--vterm-invalidate-advice (orig-fn &rest args)
  "Around-advice on `vterm--invalidate': skip when current buffer is frozen.
Falls through to ORIG-FN with ARGS for non-frozen buffers, so other
vterm buffers (and Claude's own vterm when not actively being
scrolled) keep their normal redraw cadence."
  (if claude-repl--vterm-frozen
      (progn
        (when (fboundp 'claude-repl--log-verbose)
          (claude-repl--log-verbose (and (fboundp '+workspace-current-name)
                                          (+workspace-current-name))
                                     "vterm-invalidate-advice: SKIP buf=%s"
                                     (buffer-name)))
        nil)
    (apply orig-fn args)))

(defun claude-repl--enable-vterm-freeze-advice ()
  "Install the freeze advice on `vterm--invalidate'.
Idempotent — `advice-add' deduplicates by function symbol so repeat
calls are harmless."
  (advice-add 'vterm--invalidate :around #'claude-repl--vterm-invalidate-advice))

(defun claude-repl--disable-vterm-freeze-advice ()
  "Remove the freeze advice from `vterm--invalidate'."
  (advice-remove 'vterm--invalidate #'claude-repl--vterm-invalidate-advice))

(defun claude-repl--vterm-unfreeze (buf)
  "Clear the freeze flag on BUF and force a single redraw.
Called by the freeze timer once `claude-repl-vterm-freeze-duration'
elapses with no further scroll-bump.  Calls `vterm--invalidate'
afterward so any output that accumulated in the libvterm state during
the freeze gets flushed into the visible buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq claude-repl--vterm-frozen nil)
      (setq claude-repl--vterm-freeze-timer nil)
      (when (fboundp 'claude-repl--log)
        (claude-repl--log (and (fboundp '+workspace-current-name)
                               (+workspace-current-name))
                          "vterm-unfreeze: buf=%s flushing" (buffer-name)))
      ;; Force a redraw to flush any output that arrived while frozen.
      ;; Guard `vterm--term' so tests / buffers without an active libvterm
      ;; state don't crash on the C-side invalidate.
      (when (and (boundp 'vterm--term) vterm--term
                 (fboundp 'vterm--invalidate))
        (vterm--invalidate)))))

(defun claude-repl--vterm-freeze-bump (buf)
  "Freeze BUF's vterm display and (re)arm its unfreeze timer.
Sets the buffer-local `claude-repl--vterm-frozen' flag, cancels any
pending unfreeze timer, and schedules a fresh one
`claude-repl-vterm-freeze-duration' seconds out.  A no-op when BUF is
dead."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq claude-repl--vterm-frozen t)
      (when (timerp claude-repl--vterm-freeze-timer)
        (cancel-timer claude-repl--vterm-freeze-timer))
      (setq claude-repl--vterm-freeze-timer
            (run-with-timer claude-repl-vterm-freeze-duration nil
                            #'claude-repl--vterm-unfreeze buf))
      (when (fboundp 'claude-repl--log)
        (claude-repl--log (and (fboundp '+workspace-current-name)
                               (+workspace-current-name))
                          "vterm-freeze-bump: buf=%s duration=%.2f"
                          (buffer-name) claude-repl-vterm-freeze-duration)))))

;; Install advice at load time.  The advice is harmless for non-Claude
;; vterm buffers because `claude-repl--vterm-frozen' defaults to nil
;; everywhere; only buffers Claude actively freezes are affected.
(claude-repl--enable-vterm-freeze-advice)

(provide 'claude-repl-vterm-freeze)
;;; vterm-freeze.el ends here
