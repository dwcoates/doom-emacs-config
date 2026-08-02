;;; test-workspace-create-client.el --- Tests for daemon workspace thin client -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-helpers.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-repl-test-workspace-create-keybindings-are-tab-n-and-N ()
  "The only declared create chords dispatch the required commands."
  (should
   (equal agent-repl--workspace-create-keybindings
          '(("n" . agent-repl-create-worktree-workspace)
            ("N" . agent-repl-create-worktree-workspace-from-origin-master)))))

(ert-deftest agent-repl-test-workspace-create-master-wrapper-dispatches-master ()
  "`SPC TAB N' delegates to the same thin client with BASE `master'."
  (let ((seen nil))
    (cl-letf (((symbol-function 'agent-repl-create-worktree-workspace)
               (lambda (base &optional source)
                 (setq seen (list base source)))))
      (agent-repl-create-worktree-workspace-from-origin-master "source")
      (should (equal seen '(master "source"))))))

;;;; ---- Tests: request composition (the single ingestion point) ----

(defmacro agent-repl-test--with-command-inbox (&rest body)
  "Run BODY with `agent-repl--output-dir' bound to a fresh temp inbox."
  (declare (indent 0))
  `(let* ((agent-repl--output-dir
           (file-name-as-directory (make-temp-file "agent-repl-inbox" t)))
          (agent-repl--workspace-create-requests
           (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory (directory-file-name agent-repl--output-dir) t))))

(defun agent-repl-test--sole-command-entry ()
  "Return the sole create entry written into the temp inbox, as an alist."
  (let ((files (directory-files agent-repl--output-dir t
                                "\\`workspace_commands_.*\\.json\\'")))
    (should (= (length files) 1))
    (let ((parsed (with-temp-buffer
                    (insert-file-contents (car files))
                    (json-parse-buffer :object-type 'alist
                                       :array-type 'list))))
      (should (= (length parsed) 1))
      (car parsed))))

(ert-deftest agent-repl-test-create-request-writes-one-command-file ()
  "`SPC TAB n' emits exactly one create command file and no wire command."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (agent-repl--ws-put "source" :project-dir "/tmp/source")
      (let ((reads '("requested-name" "look into it")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (pop reads)))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "source"))
                  ((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil))
                  ((symbol-function 'agent-repl--uds-send-command)
                   (lambda (&rest _)
                     (error "workspace creation must not use the wire"))))
          (let ((id (agent-repl-create-worktree-workspace 'head))
                (entry (agent-repl-test--sole-command-entry)))
            (should (string-prefix-p "workspace_commands_" id))
            (should (equal (alist-get 'type entry) "create"))
            (should (equal (alist-get 'name entry) "requested-name"))
            (should (equal (alist-get 'prompt entry) "look into it"))))))))

(ert-deftest agent-repl-test-create-request-from-current-carries-head-base ()
  "The from-current flavor pins `base_commit' to HEAD."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (agent-repl--ws-put "source" :project-dir "/tmp/source")
      (let ((reads '("wsname" "")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (pop reads)))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "source"))
                  ((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil)))
          (agent-repl-create-worktree-workspace 'head)
          (should (equal (alist-get 'base_commit
                                    (agent-repl-test--sole-command-entry))
                         "HEAD")))))))

(ert-deftest agent-repl-test-create-request-from-master-carries-master-base ()
  "The from-master flavor pins `base_commit' to the local trunk."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (agent-repl--ws-put "source" :project-dir "/tmp/source")
      (let ((reads '("wsname" "")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (pop reads)))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "source"))
                  ((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil)))
          (agent-repl-create-worktree-workspace-from-origin-master)
          (should (equal (alist-get 'base_commit
                                    (agent-repl-test--sole-command-entry))
                         "master")))))))

(ert-deftest agent-repl-test-create-request-carries-source-workspace-fields ()
  "The source workspace is nominated so the daemon inherits its posture."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (agent-repl--ws-put "source" :project-dir "/tmp/source")
      (let ((reads '("wsname" "")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (pop reads)))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "source"))
                  ((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil)))
          (agent-repl-create-worktree-workspace 'head)
          (let ((entry (agent-repl-test--sole-command-entry)))
            (should (equal (alist-get 'source_workspace entry) "source"))
            (should (equal (alist-get 'source_dir entry) "/tmp/source"))))))))

(ert-deftest agent-repl-test-create-request-omits-blank-prompt ()
  "A blank initial prompt emits NO `prompt' field.
An empty string would make the daemon submit a blank turn into the fresh
session, which is a visibly broken workspace rather than a quiet one."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (agent-repl--ws-put "source" :project-dir "/tmp/source")
      (let ((reads '("wsname" "   ")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (pop reads)))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "source"))
                  ((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil)))
          (agent-repl-create-worktree-workspace 'head)
          (should-not (assq 'prompt
                            (agent-repl-test--sole-command-entry))))))))

(ert-deftest agent-repl-test-create-request-emits-fork-from ()
  "A fork request carries `fork_from' and needs no base commit."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                 (lambda (_dir) nil)))
        (agent-repl--workspace-create-request
         :name "forked" :git-root "/tmp/source" :fork-from "parent"
         :source-workspace "parent" :source-dir "/tmp/source")
        (let ((entry (agent-repl-test--sole-command-entry)))
          (should (equal (alist-get 'fork_from entry) "parent"))
          (should-not (assq 'base_commit entry)))))))

(ert-deftest agent-repl-test-create-request-carries-model-and-priority ()
  "Optional model and priority ride the same entry when supplied."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                 (lambda (_dir) nil)))
        (agent-repl--workspace-create-request
         :name "tuned" :git-root "/tmp/source" :base-commit "master"
         :model "opus" :priority "p1")
        (let ((entry (agent-repl-test--sole-command-entry)))
          (should (equal (alist-get 'model entry) "opus"))
          (should (equal (alist-get 'priority entry) "p1")))))))

(ert-deftest agent-repl-test-create-request-carries-ungated-consent ()
  "Ungated consent is call-site state and still crosses the request."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                 (lambda (_dir) '(:allow-ungated t))))
        (agent-repl--workspace-create-request
         :name "ungated" :git-root "/tmp/source" :base-commit "master"
         :source-dir "/tmp/source")
        (should (eq (alist-get 'allow_ungated
                               (agent-repl-test--sole-command-entry))
                    t))))))

(ert-deftest agent-repl-test-create-request-writes-atomically ()
  "The payload is renamed into place; no partial file is ever claimable."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (let ((renamed nil))
        (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil))
                  ((symbol-function 'rename-file)
                   (lambda (from to &optional ok)
                     (setq renamed (list from to ok)))))
          (agent-repl--workspace-create-request
           :name "atomic" :git-root "/tmp/source" :base-commit "master")
          (should renamed)
          ;; The staged name is dot-prefixed, so the daemon's inbox scanner
          ;; (prefix `workspace_commands_') cannot see it mid-write.
          (should (string-prefix-p
                   ".workspace_commands_"
                   (file-name-nondirectory (nth 0 renamed))))
          (should (string-prefix-p
                   "workspace_commands_"
                   (file-name-nondirectory (nth 1 renamed)))))))))

(ert-deftest agent-repl-test-create-request-rejects-missing-name ()
  "A nameless request writes nothing at all."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (should-error
       (agent-repl--workspace-create-request
        :name "  " :git-root "/tmp/source" :base-commit "master"))
      (should-not (directory-files agent-repl--output-dir nil
                                   "\\`workspace_commands_")))))

(ert-deftest agent-repl-test-create-request-rejects-missing-base-without-fork ()
  "A non-fork request with no base commit writes nothing at all."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (should-error
       (agent-repl--workspace-create-request
        :name "based" :git-root "/tmp/source"))
      (should-not (directory-files agent-repl--output-dir nil
                                   "\\`workspace_commands_")))))

;;;; ---- Tests: request correlation ----

(defun agent-repl-test--materialize-available (available)
  "Run the announcement handler for AVAILABLE with persp/wire stubbed."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
            ((symbol-function 'agent-repl--ws-dir-owner) (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
            ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
            ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil))
            ((symbol-function 'persp-kill) (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--uds-send-command) (lambda (&rest _) "ack"))
            ((symbol-function 'agent-repl--uds-track-command) (lambda (&rest _) nil)))
    (agent-repl--workspace-create-handle-available available)))

(ert-deftest agent-repl-test-available-correlated-to-own-request-jumps ()
  "A creation Emacs asked for jumps to the new tab and confirms it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (let (jumped announced)
        (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil))
                  ((symbol-function 'agent-repl--switch-to-workspace)
                   (lambda (ws) (setq jumped ws)))
                  ((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args)
                     (setq announced (apply #'format fmt args)))))
          (let ((id (agent-repl--workspace-create-request
                     :name "mine" :git-root "/tmp/source"
                     :base-commit "master" :jump t)))
            (agent-repl-test--materialize-available
             (list :jobId (concat id ":0") :finalName "mine"
                   :worktreePath "/tmp/wt/mine" :sessionId "session-mine"))
            (should (equal jumped "mine"))
            (should (string-match-p "mine" announced))))))))

(ert-deftest agent-repl-test-available-unsolicited-materializes-silently ()
  "An unsolicited creation is materialized but never steals focus."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (let (jumped)
        (cl-letf (((symbol-function 'agent-repl--switch-to-workspace)
                   (lambda (&rest _) (setq jumped t))))
          (agent-repl-test--materialize-available
           '(:jobId "workspace_commands_other:0" :finalName "theirs"
             :worktreePath "/tmp/wt/theirs" :sessionId "session-theirs"))
          (should (agent-repl--ws-live-p "theirs"))
          (should-not jumped))))))

(ert-deftest agent-repl-test-available-correlation-is-claimed-once ()
  "A replayed announcement does not jump a second time."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (let ((jumps 0))
        (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil))
                  ((symbol-function 'agent-repl--switch-to-workspace)
                   (lambda (&rest _) (cl-incf jumps))))
          (let* ((id (agent-repl--workspace-create-request
                      :name "once" :git-root "/tmp/source"
                      :base-commit "master" :jump t))
                 (available (list :jobId (concat id ":0") :finalName "once"
                                  :worktreePath "/tmp/wt/once"
                                  :sessionId "session-once")))
            (agent-repl-test--materialize-available available)
            (agent-repl-test--materialize-available available)
            (should (= jumps 1))))))))

(ert-deftest agent-repl-test-available-arms-the-deferred-ui-drains ()
  "A materialized workspace is born with its magit / initial-buffer drains."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--eager-open-panels)
                 (lambda (&rest _) nil)))
        (agent-repl-test--materialize-available
         '(:jobId "workspace_commands_drains:0" :finalName "drained"
           :worktreePath "/tmp/wt/drained" :sessionId "session-drained"))
        (should (eq (agent-repl--ws-get "drained" :pending-magit) t))
        (should (eq (agent-repl--ws-get "drained" :pending-initial-buffers)
                    t))))))

(ert-deftest agent-repl-test-available-background-workspace-opens-panels ()
  "A workspace nobody is switched to still gets its panels built eagerly."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (let (opened)
        (cl-letf (((symbol-function 'agent-repl--eager-open-panels)
                   (lambda (ws) (setq opened ws))))
          (agent-repl-test--materialize-available
           '(:jobId "workspace_commands_bg:0" :finalName "background"
             :worktreePath "/tmp/wt/background" :sessionId "session-bg"))
          (should (equal opened "background")))))))

(ert-deftest agent-repl-test-available-jumped-workspace-skips-eager-open ()
  "A workspace the user is moved to builds its panels on the switch itself."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (let (opened)
        (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                   (lambda (_dir) nil))
                  ((symbol-function 'agent-repl--switch-to-workspace)
                   (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--eager-open-panels)
                   (lambda (ws) (setq opened ws))))
          (let ((id (agent-repl--workspace-create-request
                     :name "jumped" :git-root "/tmp/source"
                     :base-commit "master" :jump t)))
            (agent-repl-test--materialize-available
             (list :jobId (concat id ":0") :finalName "jumped"
                   :worktreePath "/tmp/wt/jumped" :sessionId "session-jump"))
            (should-not opened)))))))

(ert-deftest agent-repl-test-available-priority-lands-on-the-workspace ()
  "The announced priority is what the tab's badge reads.
Nothing derives a priority locally, so a workspace whose announcement
carries one gets exactly that string on its `:priority'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--eager-open-panels)
                 (lambda (&rest _) nil)))
        (agent-repl-test--materialize-available
         '(:jobId "workspace_commands_pri:0" :finalName "prioritized"
           :worktreePath "/tmp/wt/prioritized" :sessionId "session-pri"
           :priority "p1"))
        (should (equal (agent-repl--ws-get "prioritized" :priority) "p1"))))))

(ert-deftest agent-repl-test-available-without-priority-leaves-it-nil ()
  "An announcement carrying no priority leaves the workspace without one,
so the tab paints no badge rather than inventing a default."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--eager-open-panels)
                 (lambda (&rest _) nil)))
        (agent-repl-test--materialize-available
         '(:jobId "workspace_commands_nopri:0" :finalName "unprioritized"
           :worktreePath "/tmp/wt/unprioritized" :sessionId "session-nopri"))
        (should-not (agent-repl--ws-get "unprioritized" :priority))))))

(ert-deftest agent-repl-test-available-envelope-survives-a-local-plist-edit ()
  "Editing a metadata-supplied key must not rewrite the replay envelope.
The bookkeeping plist shares METADATA's cons cells as its tail, so an
uncopied envelope would be mutated in place by any later `--ws-put' and
turn the next reconnect replay into a false conflict."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--eager-open-panels)
                 (lambda (&rest _) nil)))
        (let ((available
               '(:jobId "workspace_commands_alias:0" :finalName "aliased"
                 :worktreePath "/tmp/wt/aliased" :sessionId "session-alias"
                 :priority "p3")))
          (agent-repl-test--materialize-available available)
          (agent-repl--ws-put "aliased" :priority "p1")
          (should (equal (plist-get
                          (agent-repl--ws-get "aliased"
                                              :daemon-workspace-metadata)
                          :priority)
                         "p3"))
          (should (eq (agent-repl-test--materialize-available available)
                      'existing)))))))

(ert-deftest agent-repl-test-create-failure-releases-its-correlation ()
  "A failed job clears its pending entry so nothing waits on it forever."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-command-inbox
      (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
                 (lambda (_dir) nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (let ((id (agent-repl--workspace-create-request
                   :name "doomed" :git-root "/tmp/source"
                   :base-commit "master" :jump t)))
          (should (= (hash-table-count agent-repl--workspace-create-requests) 1))
          (agent-repl--handle-workspace-create-failed-command
           `((job_id . ,(concat id ":0")) (requested_name . "doomed")
             (error . "branch exists")))
          (should
           (= (hash-table-count agent-repl--workspace-create-requests) 0)))))))

(ert-deftest agent-repl-test-create-failure-is-always-announced ()
  "A failure Emacs did not request is still surfaced loudly."
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      (agent-repl--handle-workspace-create-failed-command
       '((job_id . "workspace_commands_x:0") (requested_name . "orphan")
         (error . "git_root has no .git")))
      (should (string-match-p "orphan" echoed))
      (should (string-match-p "git_root has no .git" echoed)))))

(ert-deftest agent-repl-test-create-workspace-has-no-wire-command ()
  "Emacs cannot send `createWorkspace': the inbox is the only ingress."
  (should-not (fboundp 'agent-repl--workspace-create-send))
  (should-not (member "createWorkspace" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-workspace-available-validates-before-mutation ()
  "A missing authoritative field causes no materialization and no ACK."
  (let ((materialized nil)
        (sent nil))
    (cl-letf (((symbol-function 'agent-repl--ws-materialize-daemon-workspace)
               (lambda (&rest _) (setq materialized t)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (setq sent t))))
      (should-error
       (agent-repl--workspace-create-handle-available
        '(:jobId "job-1" :finalName "new" :worktreePath "/tmp/new")))
      (should-not materialized)
      (should-not sent))))

(ert-deftest agent-repl-test-workspace-available-replay-is-idempotent-and-acks ()
  "An exact reconnect replay creates one perspective and sends another ACK."
  (agent-repl-test--with-clean-state
    (let ((creates 0)
          (sets 0)
          (acks nil)
          (tracks nil)
          (available
           '(:jobId "job-1" :finalName "new"
             :worktreePath "/tmp/new" :sessionId "session-1"
             :branch "DWC/new" :gitRoot "/tmp/source"
             :baseCommit "HEAD" :sourceWorkspace "source"
             :sourceDir "/tmp/source" :initialPromptQueued t
             :configDir "/tmp/account" :permissionMode "auto"
             :allowUngated t)))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new)
                 (lambda (_ws) (cl-incf creates) 'persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (cl-incf sets)))
                ((symbol-function 'persp-kill)
                 (lambda (&rest _) (error "rollback must not run")))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &optional workspace _process)
                   (push (list field payload workspace) acks)
                   (format "ack-%d" (length acks))))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest args) (push args tracks))))
        (should (eq (agent-repl--workspace-create-handle-available available)
                    'created))
        (should (eq (agent-repl--workspace-create-handle-available available)
                    'existing))
        (should (= creates 1))
        (should (= sets 1))
        (should (= (length acks) 2))
        (dolist (ack acks)
          (should (equal (car ack) "workspaceMaterialized"))
          (should (equal (plist-get (cadr ack) :jobId) "job-1"))
          (should (equal (caddr ack) "new")))
        (should (= (length tracks) 2))
        (should (equal (agent-repl--ws-get "new" :frontend-session-id)
                       "session-1"))
        (should (equal (agent-repl--ws-get "new" :config-dir-override)
                       "/tmp/account"))
        (should (equal (agent-repl--ws-get "new" :permission-mode) "auto"))
        (should (eq (agent-repl--ws-get "new" :allow-ungated) t))
        (should (eq (agent-repl--ws-get "new" :initial-prompt-queued) t))))))

(ert-deftest agent-repl-test-workspace-available-never-creates-git-session-or-shim ()
  "Materialization touches only perspective/bookkeeping and its ACK."
  (agent-repl-test--with-clean-state
    (let ((available
           '(:jobId "job-2" :finalName "plain"
             :worktreePath "/tmp/plain" :sessionId "session-2"
             :initialPromptQueued nil)))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
                ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil))
                ((symbol-function 'persp-kill) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "ack"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--async-git)
                 (lambda (&rest _) (error "git forbidden")))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _) (error "session creation forbidden")))
                ((symbol-function 'agent-repl--spawn-agent-shim)
                 (lambda (&rest _) (error "shim startup forbidden")))
                ((symbol-function 'agent-repl--send)
                 (lambda (&rest _) (error "local prompt delivery forbidden"))))
        (should
         (eq (agent-repl--workspace-create-handle-available available)
             'created))
        (should (agent-repl--ws-live-p "plain"))
        (should-not (agent-repl--ws-get "plain" :initial-prompt-queued))))))

(ert-deftest agent-repl-test-workspace-available-project-dir-has-no-trailing-slash ()
  "The announced worktree is stored in `directory-file-name' spelling.
Emacs echoes `:project-dir' back as the UDS `workspace' key, so a trailing
slash produced a key the daemon has no session for."
  (agent-repl-test--with-clean-state
    (let ((available
           '(:jobId "job-slash" :finalName "sluggish"
             :worktreePath "/tmp/wt/sluggish/" :sessionId "session-slash")))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
                ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil))
                ((symbol-function 'persp-kill) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "ack"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil)))
        (agent-repl--workspace-create-handle-available available)
        (should (equal (agent-repl--ws-get "sluggish" :project-dir)
                       "/tmp/wt/sluggish"))))))

(ert-deftest agent-repl-test-workspace-available-ws-id-matches-canonical-derivation ()
  "The materialized `:ws-id' equals the shared canonical-path derivation."
  (agent-repl-test--with-clean-state
    (let ((available
           '(:jobId "job-id" :finalName "ided"
             :worktreePath "/tmp/wt/ided/" :sessionId "session-id")))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
                ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil))
                ((symbol-function 'persp-kill) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "ack"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil)))
        (agent-repl--workspace-create-handle-available available)
        (should (equal (agent-repl--ws-get "ided" :ws-id)
                       (substring (md5 (agent-repl--path-canonical
                                        "/tmp/wt/ided"))
                                  0 agent-repl-workspace-id-length)))))))

(ert-deftest agent-repl-test-workspace-available-rolls-back-on-perspective-setup-error ()
  "Failure after perspective creation leaves no hash entry or perspective."
  (agent-repl-test--with-clean-state
    (let ((killed nil)
          (acked nil)
          (available
           '(:jobId "job-3" :finalName "broken"
             :worktreePath "/tmp/broken" :sessionId "session-3")))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (error "persp property failed")))
                ((symbol-function 'persp-kill)
                 (lambda (ws) (setq killed ws)))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) (setq acked t))))
        (should-error
         (agent-repl--workspace-create-handle-available available))
        (should (equal killed "broken"))
        (should-not (agent-repl--ws-known-p "broken"))
        (should-not acked)))))

(ert-deftest agent-repl-test-workspace-command-files-are-not-emacs-owned ()
  "Emacs exposes no watcher, drain, or file claimant for daemon-owned JSON."
  (dolist (fn '(agent-repl--register-workspace-commands-watch
                agent-repl--drain-workspace-commands-files
                agent-repl--process-workspace-commands-file))
    (should-not (fboundp fn))))

(ert-deftest agent-repl-test-daemon-materialization-envelope-clears-on-tombstone ()
  "The replay envelope does not retain a dead workspace's session id."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "dead" :project-dir "/tmp/dead/")
    (agent-repl--ws-put
     "dead" :daemon-workspace-metadata
     '(:frontend-session-id "session-dead"))
    (agent-repl--ws-del "dead")
    (should-not
     (agent-repl--ws-get "dead" :daemon-workspace-metadata))))

(ert-deftest agent-repl-test-host-action-dispatches-ui-handler-and-completes ()
  "HostAction preserves UI-only behavior through its completion command."
  (let ((handled nil)
        (completion nil))
    (cl-letf (((symbol-function 'agent-repl--handle-switch-command)
               (lambda (cmd) (setq handled cmd)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional workspace _process)
                 (setq completion (list field payload workspace))
                 "host-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "action-1" :switchWorkspace (:dir "/tmp/repo"))))
      (should (equal handled '((dir . "/tmp/repo"))))
      (should (equal (car completion) "hostActionCompleted"))
      (should (equal (plist-get (cadr completion) :actionId) "action-1"))
      (should (eq (plist-get (cadr completion) :ok) t)))))

;;;; ---- Tests: deferred host-action completion ----
;;
;; A handler that only DISPATCHES its effect must not have that read as the
;; effect succeeding. The merge is the case that motivated it: the daemon
;; recorded `ok=true' while the merge's own rejection was still in flight, so
;; the failure that killed every merge left the workspace showing nothing.

(ert-deftest agent-repl-test-host-action-deferred-handler-sends-no-completion ()
  "A handler that defers leaves the action outstanding, not completed."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-deferrals (make-hash-table :test 'equal))
        (completions nil))
    (cl-letf (((symbol-function 'agent-repl--handle-switch-command)
               (lambda (_cmd) (agent-repl--host-action-defer "tok-1")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional _ws _process)
                 (push (list field payload) completions)
                 "host-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should (eq (agent-repl--workspace-create-handle-host-action
                   '(:actionId "action-1" :switchWorkspace (:dir "/tmp/repo")))
                  :deferred))
      (should-not completions))))

(ert-deftest agent-repl-test-host-action-deferred-settles-ok ()
  "Settling a deferred action OK sends its success completion."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-deferrals (make-hash-table :test 'equal))
        (agent-repl--host-action-success-order nil)
        (completions nil))
    (cl-letf (((symbol-function 'agent-repl--handle-switch-command)
               (lambda (_cmd) (agent-repl--host-action-defer "tok-1")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &optional _ws _process)
                 (push payload completions)
                 "host-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (agent-repl--workspace-create-handle-host-action
       '(:actionId "action-1" :switchWorkspace (:dir "/tmp/repo")))
      (agent-repl--host-action-settle "tok-1" t nil)
      (should (equal (length completions) 1))
      (should (equal (plist-get (car completions) :actionId) "action-1"))
      (should (eq (plist-get (car completions) :ok) t)))))

(ert-deftest agent-repl-test-host-action-deferred-settles-failed ()
  "Settling a deferred action with an error reports ok=false and the reason."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-deferrals (make-hash-table :test 'equal))
        (completions nil))
    (cl-letf (((symbol-function 'agent-repl--handle-switch-command)
               (lambda (_cmd) (agent-repl--host-action-defer "tok-1")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &optional _ws _process)
                 (push payload completions)
                 "host-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (agent-repl--workspace-create-handle-host-action
       '(:actionId "action-1" :switchWorkspace (:dir "/tmp/repo")))
      (agent-repl--host-action-settle "tok-1" nil "resolve dirs: not wired")
      (should (equal (length completions) 1))
      (should (eq (plist-get (car completions) :ok) json-false))
      (should (equal (plist-get (car completions) :error)
                     "resolve dirs: not wired")))))

(ert-deftest agent-repl-test-host-action-deferred-failure-is-retryable ()
  "A settled failure drops the cached outcome so redelivery re-runs it."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-deferrals (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'agent-repl--handle-switch-command)
               (lambda (_cmd) (agent-repl--host-action-defer "tok-1")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) "host-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (agent-repl--workspace-create-handle-host-action
       '(:actionId "action-1" :switchWorkspace (:dir "/tmp/repo")))
      (agent-repl--host-action-settle "tok-1" nil "merge rejected")
      (should-not (gethash "action-1" agent-repl--host-action-outcomes)))))

(ert-deftest agent-repl-test-host-action-settle-of-an-untracked-token-is-inert ()
  "A token no host action is waiting on completes nothing."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-deferrals (make-hash-table :test 'equal))
        (completions nil))
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &optional _ws _process)
                 (push payload completions)
                 "host-ack")))
      (agent-repl--host-action-settle "interactive-merge" t nil)
      (should-not completions))))

(ert-deftest agent-repl-test-host-action-defer-outside-a-handler-is-inert ()
  "The merge dispatch stays callable from an interactive command."
  (let ((agent-repl--host-action-deferral nil))
    (should (equal (agent-repl--host-action-defer "tok-1") "tok-1"))))

(ert-deftest agent-repl-test-host-action-workspace-create-failure-is-announced ()
  "A failed creation job is logged and echoed, then completed ok."
  (let ((logged nil)
        (echoed nil)
        (completion nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args))))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _)
                 (setq completion (list field payload))
                 "failure-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "job-1:failed"
          :workspaceCreateFailed
          (:jobId "job-1" :requestedName "DWC/feature"
           :error "plan worktree: exit=128"))))
      (should (string-match-p "DWC/feature" echoed))
      (should (string-match-p "plan worktree: exit=128" echoed))
      (should (cl-some (lambda (line)
                         (string-match-p "JOB FAILED job-id=job-1" line))
                       logged))
      (should (equal (car completion) "hostActionCompleted"))
      (should (eq (plist-get (cadr completion) :ok) t)))))

(ert-deftest agent-repl-test-host-action-legacy-command-translates-struct ()
  "legacyCommand converts its recursive Struct and ACKs handler completion."
  (let ((handled nil)
        (completion nil)
        (tracked nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-command)
               (lambda (cmd) (setq handled cmd)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional workspace _process)
                 (setq completion (list field payload workspace))
                 "host-legacy-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest args) (setq tracked args))))
      (should
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "legacy-1"
          :legacyCommand
          (:type "send"
           :payload (:workspace "ws1"
                     :data (:pgn "1. e4" :labels ("a" "b")))))))
      (should
       (equal handled
              '((workspace . "ws1")
                (data . ((pgn . "1. e4") (labels . ("a" "b")))))))
      (should (equal (car completion) "hostActionCompleted"))
      (should (equal (plist-get (cadr completion) :actionId) "legacy-1"))
      (should (eq (plist-get (cadr completion) :ok) t))
      (should (equal tracked
                     '("host-legacy-ack" "hostActionCompleted" "ws1"))))))

(ert-deftest agent-repl-test-host-action-legacy-types-are-exact ()
  "Only the eight daemon legacy-command types resolve to host handlers.
\"merge\" is pointedly absent: a merge is a daemon COMMAND
\(`mergeWorkspace'), not a UI effect the daemon asks Emacs to perform."
  (should
   (equal
    (mapcar #'car agent-repl--legacy-host-action-handlers)
    '("prompt" "finish" "close" "open"
      "clipboard" "send" "eval" "set-view")))
  (should-not (assoc "create" agent-repl--legacy-host-action-handlers))
  (should-not (assoc "merge" agent-repl--legacy-host-action-handlers)))

(ert-deftest agent-repl-test-host-action-unknown-legacy-type-nacks ()
  "Unknown legacy types fail loudly and send an unsuccessful completion."
  (let ((completion nil))
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _)
                 (setq completion (list field payload))
                 "host-failure-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should-error
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "legacy-bad"
          :legacyCommand (:type "create" :payload (:name "forbidden")))))
      (should (equal (car completion) "hostActionCompleted"))
      (should (equal (plist-get (cadr completion) :actionId) "legacy-bad"))
      (should (eq (plist-get (cadr completion) :ok) json-false))
      (should (string-match-p
               "unsupported HostAction legacyCommand type create"
               (plist-get (cadr completion) :error))))))

(ert-deftest agent-repl-test-host-action-set-view-dispatches-to-sidebar ()
  "A set-view legacy HostAction reaches the sidebar handler and ACKs ok."
  (let ((completion nil)
        (seen nil))
    (cl-letf (((symbol-function 'agent-repl--handle-set-view-command)
               (lambda (cmd) (setq seen cmd)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional workspace _process)
                 (setq completion (list field payload workspace))
                 "host-ok-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (agent-repl--workspace-create-handle-host-action
       '(:actionId "legacy-set-view"
         :legacyCommand (:type "set-view" :payload (:view "task"))))
      (should (equal (alist-get 'view seen) "task"))
      (should (equal (plist-get (cadr completion) :actionId) "legacy-set-view"))
      (should (eq (plist-get (cadr completion) :ok) t)))))

(ert-deftest agent-repl-test-host-action-handler-error-nacks-and-resignals ()
  "A legacy handler error is ACKed false before the error escapes."
  (let ((completion nil))
    (cl-letf (((symbol-function 'agent-repl--handle-prompt-command)
               (lambda (_cmd) (error "prompt handler exploded")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (setq completion payload)
                 "host-failure-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should-error
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "legacy-handler-failure"
          :legacyCommand
          (:type "prompt" :payload (:workspace "ws1" :prompt "hi")))))
      (should (eq (plist-get completion :ok) json-false))
      (should (equal (plist-get completion :error)
                     "prompt handler exploded")))))

(ert-deftest agent-repl-test-host-action-overlap-runs-handler-once-and-resends ()
  "In-flight and completed duplicates never repeat a non-idempotent handler."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-success-order nil)
        (action
         '(:actionId "overlap-1"
           :legacyCommand
           (:type "clipboard" :payload (:text "one copy"))))
        (handler-calls 0)
        completions)
    (cl-letf (((symbol-function 'agent-repl--handle-clipboard-command)
               (lambda (_cmd)
                 (cl-incf handler-calls)
                 ;; Simulate a live delivery re-entering while the snapshot
                 ;; copy is still executing.
                 (agent-repl--workspace-create-handle-host-action action)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (push payload completions)
                 (format "completion-%d" (length completions))))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should
       (agent-repl--workspace-create-handle-host-action action))
      ;; One original completion plus one replay for the suppressed overlap.
      (should (= (length completions) 2))
      (should (= handler-calls 1))
      ;; A later completed duplicate resends the same successful outcome.
      (should
       (eq (agent-repl--workspace-create-handle-host-action action)
           :duplicate))
      (should (= (length completions) 3))
      (should (= handler-calls 1))
      (dolist (payload completions)
        (should (equal (plist-get payload :actionId) "overlap-1"))
        (should (eq (plist-get payload :ok) t))))))

(ert-deftest agent-repl-test-host-action-failure-completion-makes-redelivery-retryable ()
  "After failure completion, redelivery executes the handler again."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-success-order nil)
        (action
         '(:actionId "retry-1"
           :legacyCommand
           (:type "prompt" :payload (:workspace "ws1" :prompt "retry"))))
        (handler-calls 0)
        completions)
    (cl-letf (((symbol-function 'agent-repl--handle-prompt-command)
               (lambda (_cmd)
                 (cl-incf handler-calls)
                 (when (= handler-calls 1)
                   (error "first attempt failed"))))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (push payload completions)
                 (format "completion-%d" (length completions))))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should-error
       (agent-repl--workspace-create-handle-host-action action))
      (should-not
       (gethash "retry-1" agent-repl--host-action-outcomes))
      (should
       (agent-repl--workspace-create-handle-host-action action))
      (should (= handler-calls 2))
      (should (= (length completions) 2))
      (should (eq (plist-get (cadr completions) :ok) json-false))
      (should (equal (plist-get (cadr completions) :error)
                     "first attempt failed"))
      (should (eq (plist-get (car completions) :ok) t)))))

(ert-deftest agent-repl-test-snapshot-materializes-before-render-state ()
  "Reconnect snapshots never render a created workspace before Available."
  (let ((events nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-create-handle-available)
               (lambda (_item) (push 'available events)))
              ((symbol-function 'agent-repl--frontend-apply-workspace-state)
               (lambda (_item) (push 'workspace-state events)))
              ((symbol-function 'agent-repl--frontend-apply-session-view)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--frontend-apply-session-init)
               (lambda (&rest _) nil)))
      (agent-repl--frontend-apply-snapshot
       '(:workspaceAvailable ((:jobId "job"))
         :workspaces ((:workspace "/tmp/new"))))
      (should (equal (nreverse events) '(available workspace-state))))))

(provide 'test-workspace-create-client)

;;; test-workspace-create-client.el ends here
