;;; threads.el --- zettel chat threads from Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; A ZETTEL CHAT THREAD is a chat whose questions and answers land in the
;; knowledge graph (docs/knowledge-graph/PLAN.md).  To the daemon it is a
;; kind of workspace, so Emacs opens one as it opens any workspace -- a
;; perspective, its page, its composer -- from the roster's thread section
;; (roster.el), but never draws it on the tab bar and never cycles to it.
;; Emacs's composer is the chat's input; the webapp draws the chat.
;;
;; WHAT THIS FILE OWNS, the three ways to a thread (owner ruling, 2026-10-24):
;;
;;   `SPC TAB t'  a new thread, from a question read in the minibuffer, as a
;;                one-shot's commission is read;
;;   `SPC TAB T'  the thread last interacted with;
;;   `SPC TAB .'  a thread picked by name.
;;
;; and the daemon's push asking for the new-thread prompt, which the
;; sidebar's new-thread button sends (`DaemonNewThreadRequested').
;;
;; A NEW THREAD OPENS (owner ruling, 2026-10-24: "on Enter the new thread
;; opens in the webapp"), unlike a new workspace: the daemon selects it, and
;; the roster's `current' moves the frame onto it once its workspace is open.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--global-state-file "core" (relative))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--when-minibuffer-free "workspace" (scope label key fn))
(declare-function agent-repl-verbs--read-required-prompt "verbs" (prompt-text refusal &optional history-file unseeded))
(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-rpc-create-thread "rpc" (conn request &rest keys))
(declare-function agent-repl-roster-threads "roster" (&optional roster))

(defconst agent-repl-threads--log-scope
  '(:agent-repl-central "a thread is created and picked from no workspace of its own")
  "The central log scope every thread command records under.")

(defconst agent-repl-threads--history-basename "thread-question-history.el"
  "The file, under the global state directory, holding new threads' questions.")

(defun agent-repl-threads--history-file ()
  "Return the file the new-thread prompt keeps its history in."
  (agent-repl--global-state-file agent-repl-threads--history-basename))

;;;; ---- A new thread ----

(defun agent-repl-thread-new ()
  "Start a zettel chat thread from a question read in the minibuffer.
Bound to `SPC TAB t\='.  The daemon names the thread after the question,
brings its session up, asks the question and selects the thread, which the
frame follows once its workspace is open."
  (interactive)
  (let ((question (agent-repl-verbs--read-required-prompt
                   "New thread: " "a thread starts from a question"
                   (agent-repl-threads--history-file) 'unseeded)))
    (agent-repl-threads--create question)))

(defun agent-repl-threads--create (question)
  "Ask the daemon to start a thread from QUESTION."
  (let ((conn (agent-repl-link-primary)))
    (unless conn
      (agent-repl--warn agent-repl-threads--log-scope "elisp.threads.create-unsent reason=no-daemon")
      (user-error "agent-repl: no daemon is connected to start the thread"))
    (agent-repl--info agent-repl-threads--log-scope "elisp.threads.create chars=%d" (length question))
    (message "[agent-repl] Starting a thread…")
    (agent-repl-rpc-create-thread
     conn (list :question question)
     :on-response #'agent-repl-threads--created
     :on-failure
     (lambda (failure)
       (agent-repl--error agent-repl-threads--log-scope "elisp.threads.create-failed failure=%S" failure)
       (message "[agent-repl] The thread was not started: the daemon could not be reached")))))

(defun agent-repl-threads--created (response)
  "Report the CreateThread RESPONSE."
  (pcase (plist-get response :arm)
    (:success
     (let* ((ref (plist-get (plist-get response :value) :thread))
            (name (file-name-nondirectory (directory-file-name (plist-get ref :dir)))))
       (agent-repl--info agent-repl-threads--log-scope "elisp.threads.created id=%s name=%s"
                         (plist-get ref :id) name)
       (message "[agent-repl] Thread %s started" name)))
    (:error
     (let* ((cause (plist-get (plist-get response :value) :cause))
            (detail (plist-get (plist-get cause :value) :detail)))
       (agent-repl--warn agent-repl-threads--log-scope "elisp.threads.create-refused arm=%S detail=%s"
                         (plist-get cause :arm) detail)
       (message "[agent-repl] The thread was not started: %s" detail)))
    (arm
     (agent-repl--error agent-repl-threads--log-scope "elisp.threads.create-unknown-arm arm=%S" arm)
     (message "[agent-repl] The daemon answered the new thread with %S" arm))))

(defun agent-repl-thread-requested ()
  "Open the new-thread prompt the daemon asked for (the sidebar's button).
Called from the daemon link's process filter, so the prompt is opened from
a timer, and held while another minibuffer is open."
  (agent-repl--info agent-repl-threads--log-scope "elisp.threads.new-requested")
  (run-at-time
   0 nil
   (lambda ()
     (agent-repl--when-minibuffer-free
      agent-repl-threads--log-scope "new-thread prompt" 'new-thread
      (lambda (_released) (call-interactively #'agent-repl-thread-new))))))

;;;; ---- Opening a thread ----

(defun agent-repl-threads--open (row trigger)
  "Switch to the workspace of the thread ROW, recording TRIGGER."
  (let* ((id (plist-get (plist-get row :thread) :id))
         (ws (agent-repl--ws-by-ref-id id)))
    (if ws
        (agent-repl-host-request-switch ws trigger)
      (agent-repl--info agent-repl-threads--log-scope "elisp.threads.open-pending id=%s name=%s"
                        id (plist-get row :name))
      (message "[agent-repl] Thread %s is still opening" (plist-get row :name)))))

(defun agent-repl-thread-recent ()
  "Open the thread last interacted with (`SPC TAB T\=')."
  (interactive)
  (let ((rows (agent-repl-roster-threads)))
    (unless rows
      (user-error "agent-repl: there is no thread yet; SPC TAB t starts one"))
    (agent-repl-threads--open (car rows) 'thread-recent)))

(defun agent-repl-thread-pick ()
  "Open a thread picked by name (`SPC TAB .\='), the most recent first."
  (interactive)
  (let ((rows (agent-repl-roster-threads)))
    (unless rows
      (user-error "agent-repl: there is no thread yet; SPC TAB t starts one"))
    (let* ((names (mapcar (lambda (row) (plist-get row :name)) rows))
           (picked (completing-read "Thread: " (lambda (string pred action)
                                                 (if (eq action 'metadata)
                                                     '(metadata (display-sort-function . identity))
                                                   (complete-with-action action names string pred)))
                                    nil t)))
      (agent-repl-threads--open
       (cl-find picked rows :key (lambda (row) (plist-get row :name)) :test #'equal)
       'thread-picker))))

(provide 'threads)

;;; threads.el ends here
