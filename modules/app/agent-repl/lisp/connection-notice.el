;;; connection-notice.el --- Retractable daemon-connection notices -*- lexical-binding: t; -*-

;;; Commentary:

;; The ONE place Emacs tells a human that it cannot reach the agent-repl
;; daemon, and the ONE place it takes that back.
;;
;; A connection notice is different in kind from every other thing Emacs
;; says.  A classified failure on the conversation plane describes something
;; that HAPPENED — a refused command, a session that died — and stays true
;; forever after.  A connection notice describes a condition that is TRUE
;; RIGHT NOW and stops being true the moment the link returns.  Left standing
;; after the reconnect it is no longer a report, it is a lie: a `*Warnings*'
;; buffer parked over a live session, saying the daemon is unreachable while
;; the feed beside it streams.
;;
;; So every connection notice is minted here, and every one is RETRACTABLE.
;; The retraction is exact rather than textual: each notice records the
;; buffer region `display-warning' actually wrote, so taking it down removes
;; that notice and leaves every unrelated warning in the buffer untouched.
;;
;; The webapp has had this contract since `CONNECTIVITY_WINDOW_FAILURE_TYPES'
;; (webapp/src/store.ts): a connectivity window that closes RETRACTS its card
;; instead of settling it in place with a resolved stamp.  This is Emacs's
;; half of the same rule.

;;; Code:

(require 'subr-x)
(require 'warnings)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))

(defconst agent-repl-connection-notice-buffer "*Warnings*"
  "The buffer `display-warning' writes agent-repl connection notices into.
Named explicitly rather than defaulted so the retraction reaches the same
buffer the notice was written to, whatever a caller's `warning-*' bindings
happen to be.")

(defvar agent-repl--connection-notices nil
  "The standing connection notices, newest first.

Each entry is a plist of `:start' `:end' (markers bounding the region
`display-warning' wrote) and `:text' (the notice's prose, for the log).
Emptied by `agent-repl-connection-notices-retract'.")

(defvar agent-repl--connection-notice-echo nil
  "The prose of the last connection notice echoed to the echo area, or nil.
Retraction clears the echo area only when it still shows THIS text: an
echo the user has since replaced with something of their own is theirs,
and blanking it would take away a message this module never wrote.")

(defun agent-repl-connection-notice-warn (text &optional level)
  "Display TEXT as a RETRACTABLE agent-repl connection warning.

LEVEL is a `display-warning' level and defaults to `:warning'.  Returns
the recorded notice plist.

The region `display-warning' writes is captured by bracketing the call
with markers rather than by matching the text back out afterwards.  A
textual search would be defeated by the very thing that makes these
notices worth retracting: the same condition reported twice produces two
identical entries, and there would be no way to say which one this call
owns."
  (let* ((level (or level :warning))
         (buffer (get-buffer-create agent-repl-connection-notice-buffer))
         ;; Insertion type nil: text inserted AT this position goes after the
         ;; marker, so it keeps pointing at the first character of the entry
         ;; `display-warning' is about to write.
         (start (with-current-buffer buffer (copy-marker (point-max) nil))))
    (display-warning 'agent-repl text level agent-repl-connection-notice-buffer)
    (let ((notice (list :start start
                        :end (with-current-buffer buffer
                               (copy-marker (point-max) nil))
                        :text text)))
      (push notice agent-repl--connection-notices)
      (agent-repl--log nil
                       "connection-notice: RAISED level=%s standing=%d text=%s"
                       level (length agent-repl--connection-notices) text)
      notice)))

(defun agent-repl-connection-notice-echo (text)
  "Record TEXT as the connection notice currently in the echo area.

The echo area is not written here — the caller has already echoed TEXT
through its own surfacing path (`agent-repl-failure-surface' for a
classified one).  Recording it is what lets the retraction take a
transient notice down as well as a persistent one, so a reconnect does
not leave \"the daemon is unreachable\" sitting under a working session."
  (setq agent-repl--connection-notice-echo text)
  (agent-repl--log nil "connection-notice: ECHO recorded text=%s" text)
  text)

(defun agent-repl--connection-notice-delete (notice)
  "Delete NOTICE's region from its buffer and release its markers.
Returns non-nil when a region was actually removed.  A notice whose
buffer the user killed is already gone, which is the outcome the
retraction wanted."
  (let* ((start (plist-get notice :start))
         (end (plist-get notice :end))
         (buffer (marker-buffer start))
         (deleted nil))
    (when (and buffer (eq buffer (marker-buffer end)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (delete-region start end)
          (setq deleted t))))
    (set-marker start nil)
    (set-marker end nil)
    deleted))

(defun agent-repl--connection-notice-buffer-empty-p ()
  "Return non-nil when the notice buffer exists and holds nothing but space."
  (let ((buffer (get-buffer agent-repl-connection-notice-buffer)))
    (and buffer
         (with-current-buffer buffer
           (string-empty-p (string-trim (buffer-string)))))))

(defun agent-repl--connection-notice-dismiss-buffer ()
  "Bury the notice buffer and close its windows once it holds nothing.

Deleting the LAST notice out of `*Warnings*' leaves an empty window
sitting over the user's frame, which reads as \"something is still
wrong\" just as loudly as the text used to.  A buffer that still holds
warnings this module did not write is left exactly where it is."
  (when (agent-repl--connection-notice-buffer-empty-p)
    (let ((buffer (get-buffer agent-repl-connection-notice-buffer)))
      (dolist (window (get-buffer-window-list buffer nil t))
        (when (window-live-p window)
          (condition-case err
              (delete-window window)
            ;; A sole window in its frame cannot be deleted; burying the
            ;; buffer out of it is the equivalent outcome.
            (error
             (agent-repl--log nil
                              "connection-notice: window not deletable (%s) — burying instead"
                              (error-message-string err))
             (with-selected-window window (switch-to-prev-buffer window))))))
      (kill-buffer buffer)
      t)))

(defun agent-repl-connection-notices-retract (reason)
  "Take down every standing connection notice; REASON names what cleared them.

Called the instant the link is proven back — the snapshot-applied edge,
not the socket-open edge, because a socket with no state behind it has
not finished reconnecting and a notice taken down there would be taken
down early.

Returns the number of notices retracted.  Zero is the ordinary case (a
reconnect after no outage was ever reported) and is deliberately silent
in the echo area: announcing the end of something the user never saw the
start of is the noise this module exists to avoid."
  (let ((standing (length agent-repl--connection-notices))
        (echo agent-repl--connection-notice-echo))
    (when (> standing 0)
      (dolist (notice agent-repl--connection-notices)
        (agent-repl--log nil
                         "connection-notice: RETRACTED reason=%s text=%s"
                         reason (plist-get notice :text))
        (agent-repl--connection-notice-delete notice))
      (setq agent-repl--connection-notices nil)
      (agent-repl--connection-notice-dismiss-buffer)
      (agent-repl--log nil
                       "connection-notice: retracted %d standing notice(s) reason=%s"
                       standing reason))
    (when echo
      (setq agent-repl--connection-notice-echo nil)
      (when (equal (current-message) (format "agent-repl: %s" echo))
        (message nil)
        (agent-repl--log nil
                         "connection-notice: cleared the echoed notice reason=%s text=%s"
                         reason echo)))
    standing))

(provide 'connection-notice)
;;; connection-notice.el ends here
