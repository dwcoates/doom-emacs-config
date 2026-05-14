;;; events.el --- lightweight workspace lifecycle event log -*- lexical-binding: t; -*-

;;; Commentary:

;; Records workspace create/merge events with timestamps to a small file
;; under `~/.claude/emacs/'.  The drawer renders a summary of the last 24
;; hours at its top.  Storage is trimmed to that window on every write,
;; so the file cannot grow unbounded.

;;; Code:

(require 'cl-lib)

(defcustom claude-repl-events-file
  (expand-file-name "events.el" "~/.claude/emacs/")
  "Path to the global workspace-event log.
Stores recent `:create' and `:merge' events as a list of plists."
  :type 'file
  :group 'claude-repl)

(defconst claude-repl-events-window-seconds (* 24 60 60)
  "Time window (seconds) for the drawer's recent-events summary.")

(defvar claude-repl--events-cache nil
  "In-memory copy of the events list.  Lazily loaded from disk.")

(defvar claude-repl--events-cache-loaded nil
  "Non-nil once `--events-cache' has been hydrated from disk.")

(defun claude-repl--events-load ()
  "Hydrate `claude-repl--events-cache' from disk if not yet loaded.
Returns the cached list (possibly empty).  Malformed files reset the
cache to empty rather than signal."
  (unless claude-repl--events-cache-loaded
    (setq claude-repl--events-cache
          (when (file-exists-p claude-repl-events-file)
            (condition-case _err
                (with-temp-buffer
                  (insert-file-contents claude-repl-events-file)
                  (let ((data (read (current-buffer))))
                    (and (listp data) data)))
              (error nil))))
    (setq claude-repl--events-cache-loaded t))
  claude-repl--events-cache)

(defun claude-repl--events-prune (events &optional now)
  "Return EVENTS with entries older than the 24h window dropped.
NOW defaults to `float-time'."
  (let ((cutoff (- (or now (float-time)) claude-repl-events-window-seconds)))
    (cl-remove-if (lambda (ev)
                    (let ((ts (plist-get ev :time)))
                      (or (not (numberp ts)) (< ts cutoff))))
                  events)))

(defun claude-repl--events-save ()
  "Persist `claude-repl--events-cache' to disk, pruned to the 24h window."
  (let* ((pruned (claude-repl--events-prune claude-repl--events-cache))
         (dir (file-name-directory claude-repl-events-file)))
    (setq claude-repl--events-cache pruned)
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-file claude-repl-events-file
      (prin1 pruned (current-buffer)))))

(defun claude-repl--events-record (ws kind &optional time)
  "Record an event of KIND for workspace WS.
KIND is :create or :merge.  TIME defaults to `float-time'.
Trims the log to the 24h window on every write."
  (claude-repl--events-load)
  (let ((event (list :kind kind :ws ws :time (or time (float-time)))))
    (setq claude-repl--events-cache
          (cons event claude-repl--events-cache)))
  (claude-repl--events-save))

(defun claude-repl--events-recent (&optional now)
  "Return events from the last 24 hours, newest first.
NOW defaults to `float-time'."
  (claude-repl--events-load)
  (let ((pruned (claude-repl--events-prune claude-repl--events-cache now)))
    (sort (copy-sequence pruned)
          (lambda (a b)
            (> (or (plist-get a :time) 0)
               (or (plist-get b :time) 0))))))

(defun claude-repl--events-count-by-kind (events kind)
  "Count entries in EVENTS whose :kind equals KIND."
  (cl-count-if (lambda (ev) (eq (plist-get ev :kind) kind)) events))

(provide 'claude-repl-events)

;;; events.el ends here
