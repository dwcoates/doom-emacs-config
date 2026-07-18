;;; dir-watcher.el --- Generic watched-directory file intake -*- lexical-binding: t; -*-

;;; Commentary:

;; One implementation of the watch-a-directory-for-request-files pattern
;; used by the workspace-commands channel (worktree.el) — and any future
;; channel of the same shape: a file-notify watch on a directory,
;; prefix-matched dispatch of created/changed/renamed files to a one-file
;; processor, a drain that catches files landed while the watch was down,
;; and a self-healing re-arm when the watched directory is deleted out
;; from under the watch (which silently invalidates the descriptor) or
;; the watch stops.
;;
;; A watcher is a plist of SYMBOLS, not values, so every read resolves at
;; use time: defcustom edits and test `let'-bindings of the
;; dir/prefix/regexp variables take effect immediately, and `cl-letf'
;; stubs of the channel's NAMED process/register/drain/handler functions
;; are honored — including by the re-arm path, which both channels' test
;; suites rely on.
;;
;; Watcher keys (every value is a symbol):
;;   :label          prefix for `agent-repl--log' lines
;;   :dir-var        variable holding the watched directory
;;   :prefix-var     variable holding the filename prefix to dispatch on
;;   :regexp-var     variable holding the drain's filename regexp
;;   :descriptor-var variable holding the file-notify descriptor
;;   :process-fn     the channel's one-file processor
;;   :register-fn    the channel's named register function
;;   :drain-fn       the channel's named drain function
;;   :handler-fn     the channel's named file-notify event handler

;;; Code:

(defun agent-repl--dir-watcher-dir (watcher)
  "Return WATCHER's directory, expanded, with a trailing slash."
  (file-name-as-directory
   (expand-file-name (symbol-value (plist-get watcher :dir-var)))))

(defun agent-repl--dir-watcher-drain (watcher)
  "Process every regexp-matching file currently in WATCHER's directory.
Catch-up path for files that landed while the file-notify watch was
down (e.g. after the directory was deleted and recreated, which
invalidates the watch).  Returns the number of files processed."
  (let* ((dir (agent-repl--dir-watcher-dir watcher))
         (files (when (file-directory-p dir)
                  (directory-files
                   dir t (symbol-value (plist-get watcher :regexp-var))))))
    (dolist (file files)
      (agent-repl--log nil "%s: drain processing file=%s"
                       (plist-get watcher :label) file)
      (funcall (plist-get watcher :process-fn) file))
    (length files)))

(defun agent-repl--dir-watcher-handle-event (watcher event)
  "Handle a file-notify EVENT for WATCHER's directory.
Dispatches to WATCHER's `:process-fn' when a prefix-matching file is
created, changed, or renamed.  When the event is `stopped' (or the
watched directory ITSELF is `deleted' — both fire when the directory is
removed and recreated, which silently invalidates the watch descriptor
and would otherwise strand every future file), re-arms via the
channel's named `:register-fn' and drains via its named `:drain-fn' so
files that landed during the gap are picked up.  A `deleted' of an
individual file inside the directory (routine cleanup after
processing) does NOT trigger the re-arm."
  (let* ((action (nth 1 event))
         ;; renamed events carry (descriptor renamed old-file new-file);
         ;; all other events carry (descriptor action file).
         (file (if (eq action 'renamed)
                   (nth 3 event)
                 (nth 2 event)))
         (label (plist-get watcher :label)))
    (agent-repl--log nil "%s: action=%s file=%s" label action file)
    (cond
     ((and (memq action '(changed created renamed))
           file
           (string-prefix-p (symbol-value (plist-get watcher :prefix-var))
                            (file-name-nondirectory file)))
      (funcall (plist-get watcher :process-fn) file))
     ((or (eq action 'stopped)
          (and (eq action 'deleted)
               file
               (string= (file-name-as-directory (expand-file-name file))
                        (agent-repl--dir-watcher-dir watcher))))
      (agent-repl--log nil "%s: watch lost (action=%s) — re-arming"
                       label action)
      (funcall (plist-get watcher :register-fn))
      (let ((n (funcall (plist-get watcher :drain-fn))))
        (agent-repl--log nil "%s: re-armed and drained %d pending file(s)"
                         label n)))
     (t
      (agent-repl--log nil "%s: skipped (wrong action or wrong prefix)"
                       label)))))

(defun agent-repl--dir-watcher-register (watcher)
  "Register a file-notify watch on WATCHER's directory.
Creates the directory when missing and tears down any existing valid
watch first to avoid duplicates on re-eval.  Events route through
WATCHER's named `:handler-fn' (resolved per event, so stubbing the
named handler takes effect on a live watch)."
  (let ((dir (agent-repl--dir-watcher-dir watcher))
        (descriptor-var (plist-get watcher :descriptor-var)))
    (make-directory dir t)
    (let ((existing (symbol-value descriptor-var)))
      (when (and existing (file-notify-valid-p existing))
        (file-notify-rm-watch existing)))
    (agent-repl--log nil "%s: registering watch on %s"
                     (plist-get watcher :label) dir)
    (set descriptor-var
         (file-notify-add-watch
          dir '(change)
          (lambda (event)
            (funcall (plist-get watcher :handler-fn) event))))))

(provide 'agent-repl-dir-watcher)
;;; dir-watcher.el ends here
