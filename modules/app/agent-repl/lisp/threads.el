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
;;   `SPC TAB T'  the thread last asked a question;
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
(declare-function agent-repl--when-minibuffer-free "workspace" (scope label key fn))
(declare-function agent-repl-verbs--read-required-prompt "verbs" (prompt-text refusal &optional history-file unseeded))
(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-rpc-create-thread "rpc" (conn request &rest keys))
(declare-function agent-repl-roster-threads "roster" (&optional roster))
(declare-function agent-repl-roster-thread-row-ws "roster" (row))
(defvar agent-repl-roster-view)
(defvar agent-repl-roster-update-functions)

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
  (let ((id (plist-get (plist-get row :thread) :id))
        (ws (agent-repl-roster-thread-row-ws row)))
    (if ws
        (agent-repl-host-request-switch ws trigger)
      (agent-repl--info agent-repl-threads--log-scope "elisp.threads.open-pending id=%s name=%s"
                        id (plist-get row :name))
      (message "[agent-repl] Thread %s is still opening" (plist-get row :name)))))

(defun agent-repl-thread-recent ()
  "Open the thread last asked a question (`SPC TAB T\=')."
  (interactive)
  (let ((rows (agent-repl-roster-threads)))
    (unless rows
      (user-error "agent-repl: there is no thread yet; SPC TAB t starts one"))
    (agent-repl-threads--open (car rows) 'thread-recent)))

;;;; ---- Opening Chats from outside Emacs (Gippity.app) ----

;; GIPPITY.APP OPENS A FILE (bin/install-gippity.sh): `open -a Emacs` on the
;; launch file, lisp/open-chats.agent-repl-chats, whose name matches the
;; regexp below. macOS holds that open until Emacs takes it, running or
;; still starting, so no launch is lost to a server that is not up yet; the
;; file's major mode opens the most recent thread, at once or, before the
;; first roster, on its arrival.

(defconst agent-repl-threads-launch-file-regexp "\\.agent-repl-chats\\'"
  "The file names whose opening is a launch of Chats from outside Emacs.")

(defvar agent-repl-threads--recent-owed nil
  "Non-nil while a launch waits for the first roster to open its thread.")

(defun agent-repl-threads--open-recent-launched ()
  "Open the most recent thread for a launch; say so when there is none."
  (let ((rows (agent-repl-roster-threads)))
    (if rows
        (agent-repl-threads--open (car rows) 'launch)
      (agent-repl--info agent-repl-threads--log-scope "elisp.threads.launch-no-thread")
      (message "[agent-repl] There is no thread yet; SPC TAB t starts one"))))

(defun agent-repl-thread-recent-when-ready ()
  "Open the most recent thread now, or once the daemon's first roster arrives."
  (if agent-repl-roster-view
      (agent-repl-threads--open-recent-launched)
    (setq agent-repl-threads--recent-owed t)
    (agent-repl--info agent-repl-threads--log-scope "elisp.threads.launch-deferred")))

(defun agent-repl-threads--launch-on-roster (&rest _)
  "Open the thread a launch owes, once, with the first roster."
  (when agent-repl-threads--recent-owed
    (setq agent-repl-threads--recent-owed nil)
    (agent-repl-threads--open-recent-launched)))

(add-hook 'agent-repl-roster-update-functions #'agent-repl-threads--launch-on-roster)

(defun agent-repl-threads-launch-mode ()
  "The mode of a launch file: drop its buffer, then open the most recent thread.
Both run once `find-file' has finished with the buffer, which a mode
function cannot kill from inside it."
  (let ((buffer (current-buffer)))
    (agent-repl--info agent-repl-threads--log-scope "elisp.threads.launched file=%s" buffer-file-name)
    (run-at-time 0 nil (lambda ()
                         (when (buffer-live-p buffer) (kill-buffer buffer))
                         (agent-repl-thread-recent-when-ready)))))

(add-to-list 'auto-mode-alist (cons agent-repl-threads-launch-file-regexp #'agent-repl-threads-launch-mode))
(with-eval-after-load 'recentf
  (defvar recentf-exclude)
  (add-to-list 'recentf-exclude agent-repl-threads-launch-file-regexp))

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
