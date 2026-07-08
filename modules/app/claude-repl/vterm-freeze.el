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
;; `C-S-k' freezes the Claude vterm.  The direction of the last scroll
;; determines whether the freeze is timed:
;;
;;   * Scrolling DOWN (toward newer output) arms an unfreeze timer for
;;     `claude-repl-vterm-freeze-duration' seconds.  The user is heading
;;     back toward the live prompt, so the freeze should lapse and let
;;     auto-scroll-to-bottom resume shortly.
;;
;;   * Scrolling UP (toward older output) freezes indefinitely with NO
;;     timer.  The user is reading back through history and must not be
;;     yanked to the bottom by incoming output until they scroll DOWN
;;     again — a subsequent DOWN scroll re-arms the timed freeze.
;;
;; Each additional scroll keypress re-evaluates this, so the freeze
;; extends as the user continues scrolling.  After a (timed) freeze
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
          (claude-repl--log-verbose (claude-repl--ws-current-name)
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
        (claude-repl--log (claude-repl--ws-current-name)
                          "vterm-unfreeze: buf=%s flushing" (buffer-name)))
      ;; Force a redraw to flush any output that arrived while frozen.
      ;; Guard `vterm--term' so tests / buffers without an active libvterm
      ;; state don't crash on the C-side invalidate.
      (when (and (boundp 'vterm--term) vterm--term
                 (fboundp 'vterm--invalidate))
        (vterm--invalidate)))))

(defun claude-repl--vterm-freeze-bump (buf &optional persist)
  "Freeze BUF's vterm display and (re)arm its unfreeze timer.
Sets the buffer-local `claude-repl--vterm-frozen' flag and cancels any
pending unfreeze timer.  When PERSIST is nil (the default, used for
DOWN scrolls) a fresh unfreeze timer is scheduled
`claude-repl-vterm-freeze-duration' seconds out.  When PERSIST is
non-nil (used for UP scrolls) NO timer is armed, so the freeze holds
indefinitely until a later DOWN scroll re-arms a timed freeze.  A no-op
when BUF is dead."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq claude-repl--vterm-frozen t)
      (when (timerp claude-repl--vterm-freeze-timer)
        (cancel-timer claude-repl--vterm-freeze-timer))
      (setq claude-repl--vterm-freeze-timer
            (unless persist
              (run-with-timer claude-repl-vterm-freeze-duration nil
                              #'claude-repl--vterm-unfreeze buf)))
      (when (fboundp 'claude-repl--log)
        (claude-repl--log (claude-repl--ws-current-name)
                          "vterm-freeze-bump: buf=%s duration=%.2f persist=%s"
                          (buffer-name) claude-repl-vterm-freeze-duration
                          (if persist "t" "nil"))))))

;; Install advice at load time.  The advice is harmless for non-Claude
;; vterm buffers because `claude-repl--vterm-frozen' defaults to nil
;; everywhere; only buffers Claude actively freezes are affected.
(claude-repl--enable-vterm-freeze-advice)

(provide 'claude-repl-vterm-freeze)
;;; vterm-freeze.el ends here
