;;; events.el --- lightweight workspace lifecycle event log -*- lexical-binding: t; -*-

;;; Commentary:

;; Records workspace create/merge events with timestamps to a small file
;; under `~/.claude-emacs/'.  The drawer renders a summary of the last 24
;; hours at its top.  Storage is trimmed to that window on every write,
;; so the file cannot grow unbounded.

;;; Code:

(require 'cl-lib)

(defcustom agent-repl-events-file
  (agent-repl--global-state-file "events.el")
  "Path to the global workspace-event log.
Lives at `~/.claude-emacs/events.el' (under `agent-repl--global-state-dir').
Stores recent `:create' and `:merge' events as a list of plists."
  :type 'file
  :group 'agent-repl)

(defconst agent-repl-events-window-seconds (* 24 60 60)
  "Time window (seconds) for the drawer's recent-events summary.")

(defvar agent-repl--events-cache nil
  "In-memory copy of the events list.  Lazily loaded from disk.")

(defvar agent-repl--events-cache-loaded nil
  "Non-nil once `--events-cache' has been hydrated from disk.")

(defun agent-repl--events-load ()
  "Hydrate `agent-repl--events-cache' from disk if not yet loaded.
Returns the cached list (possibly empty).  Malformed files reset the
cache to empty rather than signal."
  (unless agent-repl--events-cache-loaded
    (setq agent-repl--events-cache
          (when (file-exists-p agent-repl-events-file)
            (condition-case _err
                (with-temp-buffer
                  (insert-file-contents agent-repl-events-file)
                  (let ((data (read (current-buffer))))
                    (and (listp data) data)))
              (error nil))))
    (setq agent-repl--events-cache-loaded t))
  agent-repl--events-cache)

(defun agent-repl--events-prune (events &optional now)
  "Return EVENTS with entries older than the 24h window dropped.
NOW defaults to `float-time'."
  (let ((cutoff (- (or now (float-time)) agent-repl-events-window-seconds)))
    (cl-remove-if (lambda (ev)
                    (let ((ts (plist-get ev :time)))
                      (or (not (numberp ts)) (< ts cutoff))))
                  events)))

(defun agent-repl--events-save ()
  "Persist `agent-repl--events-cache' to disk, pruned to the 24h window."
  (let* ((pruned (agent-repl--events-prune agent-repl--events-cache))
         (dir (file-name-directory agent-repl-events-file)))
    (setq agent-repl--events-cache pruned)
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-file agent-repl-events-file
      (prin1 pruned (current-buffer)))))

(defun agent-repl--events-record (ws kind &optional time)
  "Record an event of KIND for workspace WS.
KIND is :create or :merge.  TIME defaults to `float-time'.
Trims the log to the 24h window on every write."
  (agent-repl--events-load)
  (let ((event (list :kind kind :ws ws :time (or time (float-time)))))
    (setq agent-repl--events-cache
          (cons event agent-repl--events-cache)))
  (agent-repl--events-save))

(defun agent-repl--events-recent (&optional now)
  "Return events from the last 24 hours, newest first.
NOW defaults to `float-time'."
  (agent-repl--events-load)
  (let ((pruned (agent-repl--events-prune agent-repl--events-cache now)))
    (sort (copy-sequence pruned)
          (lambda (a b)
            (> (or (plist-get a :time) 0)
               (or (plist-get b :time) 0))))))

(defun agent-repl--events-count-by-kind (events kind)
  "Count entries in EVENTS whose :kind equals KIND."
  (cl-count-if (lambda (ev) (eq (plist-get ev :kind) kind)) events))

(provide 'agent-repl-events)

;;; events.el ends here
