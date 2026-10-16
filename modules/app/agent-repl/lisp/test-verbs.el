;;; test-verbs.el --- ERT tests for agent-repl verbs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-verbs.el -f ert-run-tests-batch-and-exit
;;
;; Every rpc is stubbed with a function that RECORDS its request and invokes
;; the caller's callback SYNCHRONOUSLY, so each of the three answer shapes --
;; a success arm, a daemon-authored error arm, and a transport failure -- is
;; exercised deterministically with no process and no daemon anywhere.
;;
;; The tab teardown (`agent-repl--kill-one-workspace') is recorded rather
;; than run: it is workspace.el's persp-mode boundary, and what these tests
;; assert is WHETHER a verb tore the tab down, not what teardown does.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-verbs--sent nil
  "Requests the stubbed rpcs received, oldest first, as (OP . REQUEST).")

(defvar agent-repl-test-verbs--torn-down nil
  "Workspaces whose tab teardown was invoked.")

(defvar agent-repl-test-verbs--messages nil
  "Strings passed to `message' during a test.")

(defvar agent-repl-test-verbs--handover nil
  "Refusal arms handed to host.el's handover path, as (WS ARM).")

(defvar agent-repl-test-verbs--selected nil
  "Project roots handed to `agent-repl-switch-to-project', oldest first.
The selection is commands.el's persp/projectile boundary, so it is
RECORDED here rather than run -- exactly as the tab teardown is.")

(defvar agent-repl-test-verbs--tab-switched nil
  "Workspace names handed to `agent-repl--ws-switch', oldest first.
Switching to a workspace's OWN TAB is a different landing from switching
to its project directory, so it is recorded separately from
`agent-repl-test-verbs--selected' -- a test that says the origin was left
alone has to be able to tell the two apart.")

(defvar agent-repl-test-verbs--switch-requests nil
  "Workspaces handed to `agent-repl-host-request-switch', oldest first.
The request is host.el's daemon boundary, so it is RECORDED here.")

(defvar agent-repl-test-verbs--drawn nil
  "What the stubbed `agent-repl-roster-drawn-tab-order' answers.")

(defun agent-repl-test-verbs--ref (&optional id dir)
  "Return a decoded `WorkspaceRef' plist, echoed verbatim by production."
  (list :id (or id "ws-id-1") :dir (or dir "/tmp/agent-repl-test/ws-1")))

(defun agent-repl-test-verbs--created (&optional ref)
  "Return the answers alist for a create that SUCCEEDED, minting REF.
`CreateWorkspaceSuccess' is one of the few answers that is not empty --
\"Callers learn the identity from the response\" -- so a create test that
cares what the verb does with the minted identity has to script it."
  (list (cons :create
              (list :response
                    (list :arm :success
                          :value (list :workspace (or ref (agent-repl-test-verbs--ref))))))))

(defun agent-repl-test-verbs--tab-arrives (id name)
  "Deliver the roster arrival that gives the minted ID a tab called NAME.
A minted ref is the daemon's ANSWER and the workspace itself reaches
Emacs on the roster stream, so the landing waits for the tab
\(`agent-repl-verbs--pending-landing-fire').  A create test that expects
to STAND on what it made therefore has to let that tab arrive."
  (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
             (lambda (want) (and (equal want id) name))))
    (agent-repl-verbs--pending-landing-fire)))

(defun agent-repl-test-verbs--repo-ref (&optional id dir)
  "Return a decoded `RepositoryRef' plist."
  (list :id (or id "repo-id-1") :dir (or dir "/tmp/agent-repl-test/repo")))

(defun agent-repl-test-verbs--repo-section ()
  "Return the fixture roster's single repository section.
The DYNAMIC creation modes derive their repository from the section the
current workspace's row sits in, so a create test hands them this."
  (car (agent-repl-verbs--repo-sections (agent-repl-test-verbs--roster nil))))

(defun agent-repl-test-verbs--row (&rest overrides)
  "Return a decoded `RosterRow' with OVERRIDES applied at the top level."
  (let ((row (list :workspace (list :workspace (agent-repl-test-verbs--ref))
                   :attention nil
                   :priority nil
                   :name (list :text "row-one")
                   :status (list :arm :ready :value nil)
                   :current (list :current nil)
                   :children nil
                   :when '(:arm :no-turn-running :value nil)
                   :detail (list :branch nil :parent-branch nil :summary nil)
                   :closed (list :closed nil))))
    (while overrides
      (setq row (plist-put row (pop overrides) (pop overrides))))
    row))

(defun agent-repl-test-verbs--roster (rows &optional merged-rows sections hidden-rows)
  "Return a decoded `WorkspaceRoster' carrying ROWS in one repo section.
HIDDEN-ROWS, when given, are the hidden section's rows."
  (list :repository
        (list :sections
              (or sections
                  (list (list :key (list :repository (agent-repl-test-verbs--repo-ref))
                              :header (list :label (list :text "repo-one"))
                              :rows (list :rows rows)))))
        :hidden (list :header (list :label (list :text "Hidden"))
                      :rows (list :rows hidden-rows))
        :recently-merged (list :header (list :label (list :text "Recently Merged"))
                               :groups (and merged-rows
                                            (list (list :key (list :repository (agent-repl-test-verbs--repo-ref))
                                                        :header (list :label (list :text "repo-one"))
                                                        :rows (list :rows merged-rows)
                                                        :drawn (list :arm :shown :value nil)))))
        :current nil))

(defmacro agent-repl-test-verbs--with (answers &rest body)
  "Run BODY with every verb rpc stubbed to answer from ANSWERS.
ANSWERS is an alist of (OP . ANSWER) where OP is the rpc's method keyword
and ANSWER is either `(:response PLIST)' -- delivered to `:on-response' --
or `(:failure PLIST)', delivered to `:on-failure'.  An op with no entry
answers a bare success, which is what almost every verb's success is."
  (declare (indent 1))
  `(let ((agent-repl-test-verbs--sent nil)
         (agent-repl-verbs--pending-landing nil)
         (agent-repl-verbs--pending-landing-dispatched-at nil)
         (agent-repl-verbs--pending-landing-lander nil)
         (agent-repl--minibuffer-held nil)
         (agent-repl--minibuffer-release-armed nil)
         (agent-repl-host--editor-switches 0)
         (agent-repl-test-verbs--tab-switched nil)
         (agent-repl-test-verbs--torn-down nil)
         (agent-repl-test-verbs--messages nil)
         (agent-repl-test-verbs--handover nil)
         (agent-repl-test-verbs--selected nil)
         (agent-repl-test-verbs--restart-holds nil)
         (agent-repl-test-verbs--switch-requests nil)
         (agent-repl-test-verbs--drawn nil)
         (agent-repl-test-verbs--answers ,answers))
     (cl-letf* (((symbol-function 'agent-repl-host-request-switch)
                 (lambda (ws _trigger)
                   (setq agent-repl-test-verbs--switch-requests
                         (append agent-repl-test-verbs--switch-requests (list ws)))
                   t))
                ((symbol-function 'agent-repl-roster-drawn-tab-order)
                 (lambda () agent-repl-test-verbs--drawn))
                ((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) (agent-repl-test-verbs--ref)))
                ((symbol-function 'agent-repl--ws-require-known)
                 (lambda (_ws _context) nil))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'test-conn))
                ((symbol-function 'agent-repl-host-faults) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (ws arm) (push (list ws arm) agent-repl-test-verbs--handover)))
                ((symbol-function 'agent-repl-host-take-restart-hold)
                 (lambda (ws) (push ws agent-repl-test-verbs--restart-holds)))
                ((symbol-function 'agent-repl-host-release-restart-hold)
                 (lambda (ws _reason)
                   (setq agent-repl-test-verbs--restart-holds
                         (delete ws agent-repl-test-verbs--restart-holds))))
                ((symbol-function 'agent-repl-link-primary) (lambda () 'test-conn))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                ;; The default registry is one real workspace, "ws-one", which
                ;; is also the current perspective: that is what makes the
                ;; command wrappers act on it without a picker.
                ((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-one")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws key)
                   (and (eq key :project-dir) (equal ws "ws-one") "/tmp/agent-repl-test/ws-1")))
                ((symbol-function 'agent-repl--ws-log-routable-p)
                 (lambda (ws)
                   (and (stringp ws) (not (member ws '("main" "none"))))))
                ((symbol-function 'agent-repl--workspace-log-identity)
                 (lambda (ws)
                   (list :project-dir (format "/tmp/agent-repl-test/%s" ws)
                         :workspace-id (format "id-%s" ws))))
                ((symbol-function 'agent-repl--pseudo-workspace-name-p)
                 (lambda (ws) (member ws '("main" "none"))))
                ((symbol-function 'agent-repl--kill-one-workspace)
                 (lambda (ws &optional _p) (push ws agent-repl-test-verbs--torn-down)))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (&optional project)
                   (push project agent-repl-test-verbs--selected)))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _)
                   (push ws agent-repl-test-verbs--tab-switched)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (if args (apply #'format fmt args) fmt)
                         agent-repl-test-verbs--messages)
                   nil))
                ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-rpc-close-workspace)
                 (agent-repl-test-verbs--stub :close))
                ((symbol-function 'agent-repl-rpc-kill-workspace)
                 (agent-repl-test-verbs--stub :kill))
                ((symbol-function 'agent-repl-rpc-nuke-workspace)
                 (agent-repl-test-verbs--stub :nuke))
                ((symbol-function 'agent-repl-rpc-open-workspace)
                 (agent-repl-test-verbs--stub :open))
                ((symbol-function 'agent-repl-rpc-merge-workspace)
                 (agent-repl-test-verbs--stub :merge))
                ((symbol-function 'agent-repl-rpc-restart-workspace)
                 (agent-repl-test-verbs--stub :restart))
                ((symbol-function 'agent-repl-rpc-interrupt)
                 (agent-repl-test-verbs--stub :interrupt))
                ((symbol-function 'agent-repl-rpc-create-workspace)
                 (agent-repl-test-verbs--stub :create))
                ((symbol-function 'agent-repl-rpc-register-repository)
                 (agent-repl-test-verbs--stub :register-repository))
                ((symbol-function 'agent-repl-rpc-set-workspace-priority)
                 (agent-repl-test-verbs--stub :set-priority))
                ((symbol-function 'agent-repl-rpc-set-workspace-hidden)
                 (agent-repl-test-verbs--stub :set-hidden))
                ((symbol-function 'agent-repl-rpc-move-workspace)
                 (agent-repl-test-verbs--stub :move))
                ((symbol-function 'agent-repl-rpc-update-shutdown-schedule)
                 (agent-repl-test-verbs--stub :shutdown-schedule))
                ((symbol-function 'agent-repl-rpc-update-merge-queue)
                 (agent-repl-test-verbs--stub :merge-queue))
                ((symbol-function 'agent-repl-rpc-deploy)
                 (agent-repl-test-verbs--stub :deploy))
                ((symbol-function 'agent-repl-rpc-daemon-health)
                 (agent-repl-test-verbs--stub :daemon-health))
                ((symbol-function 'agent-repl-rpc-send-hidden-keepalives)
                 (agent-repl-test-verbs--stub :hidden-keepalives))
                ((symbol-function 'agent-repl-rpc-session-health)
                 (agent-repl-test-verbs--stub :session-health)))
       (agent-repl-test--recording-popups ,@body))))

(defvar agent-repl-test-verbs--restart-holds nil
  "Workspaces whose composer a forced restart closed, bound by `--with'.")

(defvar agent-repl-test-verbs--answers nil
  "The scripted answers for the rpc stubs, bound by the `--with' macro.")

(ert-deftest agent-repl-test-verbs-read-prompt-without-a-workspace-uses-no-composer ()
  "A creation prompt outside agent-repl does not read an unroutable composer."
  ;; Arrange.
  (let (read-workspace logged-workspace)
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
              ((symbol-function 'agent-repl--read-input-buffer)
               (lambda (ws) (setq read-workspace ws)))
              ((symbol-function 'agent-repl--log)
               (lambda (ws &rest _args) (setq logged-workspace ws)))
              ((symbol-function 'read-string)
               (lambda (_prompt initial &rest _) initial)))
      ;; Act.
      (agent-repl-verbs--read-prompt "Commission: "))
    ;; Assert.
    (should-not read-workspace)
    (should (agent-repl--central-log-scope-reason logged-workspace))))

(defun agent-repl-test-verbs--stub (op)
  "Return an rpc stub for OP that records its request and answers it."
  (lambda (_conn request &rest keys)
    (push (cons op request) agent-repl-test-verbs--sent)
    (let* ((scripted (cdr (assq op agent-repl-test-verbs--answers)))
           (failure (plist-get scripted :failure))
           (response (or (plist-get scripted :response)
                         (list :arm :success :value nil))))
      (if failure
          (funcall (plist-get keys :on-failure) failure)
        (funcall (plist-get keys :on-response) response)))))

(defun agent-repl-test-verbs--request (op)
  "Return the request recorded for OP, or nil."
  (cdr (assq op (reverse agent-repl-test-verbs--sent))))

(defun agent-repl-test-verbs--messaged-p (needle)
  "Return non-nil when any recorded `message' contained NEEDLE."
  (cl-find-if (lambda (m) (string-match-p (regexp-quote needle) m))
              agent-repl-test-verbs--messages))

(ert-deftest agent-repl-test-verbs-send-correlates-the-request-and-response ()
  "A verb's outbound boundary and synchronous answer share one request id."
  ;; Arrange
  (let (seen)
    (cl-letf (((symbol-function 'agent-repl--next-log-request-id)
               (lambda () "request-1"))
              ((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws _level _verbosity fmt _args &rest _)
                 (push (list fmt
                             agent-repl--log-context-workspace
                             agent-repl--log-context-request-id)
                       seen))))
      ;; Act
      (agent-repl-verbs--send
       (lambda (_conn _request &rest keys)
         (funcall (plist-get keys :on-response)
                  (list :arm :success :value nil)))
       'test-conn nil :ws "ws-one" :op "close")
      ;; Assert
      (should (equal (nreverse seen)
                     '(("elisp.verbs.send op=%s ws=%s" "ws-one" "request-1")
                       ("elisp.verbs.ack op=%s ws=%s outcome=success"
                        "ws-one" "request-1")))))))

;;;; ---- Request shapes: one test per verb ----

(ert-deftest agent-repl-verbs-close-echoes-the-ref ()
  "CloseWorkspace carries the daemon-minted ref verbatim."
  ;; Arrange / Act
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-close "ws-one")
    ;; Assert
    (should (equal (agent-repl-test-verbs--request :close)
                   (list :workspace (agent-repl-test-verbs--ref))))))

(ert-deftest agent-repl-verbs-kill-echoes-the-ref ()
  "KillWorkspace carries the ref, and a minted op id opting into the ack."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-kill "ws-one")
    (let ((request (agent-repl-test-verbs--request :kill)))
      (should (equal (plist-get request :workspace) (agent-repl-test-verbs--ref)))
      (should (stringp (plist-get request :op-id))))))

(ert-deftest agent-repl-verbs-nuke-echoes-the-ref ()
  "NukeWorkspace carries the ref, and a minted op id opting into the ack."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-nuke "ws-one")
    (let ((request (agent-repl-test-verbs--request :nuke)))
      (should (equal (plist-get request :workspace) (agent-repl-test-verbs--ref)))
      (should (stringp (plist-get request :op-id))))))

(ert-deftest agent-repl-verbs-nuke-declares-the-departure-it-orders ()
  "The worktree goes before the answer lands, so the ORDER is what is recorded."
  (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
    (agent-repl-test-verbs--with
        '((:nuke . (:failure (:detail "the daemon never answered"))))
      ;; Act
      (agent-repl-verb-nuke "ws-one")
      ;; Assert
      (should (agent-repl--log-workspace-departing-p "ws-one")))))

(ert-deftest agent-repl-verbs-a-refused-nuke-withdraws-the-departure ()
  "A nuke the daemon refused destroyed nothing, so nothing is excused."
  (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
    (agent-repl-test-verbs--with
        '((:nuke . (:response (:arm :error :value (:cause (:arm :blocked :value nil))))))
      ;; Act
      (agent-repl-verb-nuke "ws-one")
      ;; Assert
      (should-not (agent-repl--log-workspace-departing-p "ws-one")))))

(ert-deftest agent-repl-verbs-a-refused-nuke-still-reaches-the-generic-handling ()
  "Withdrawing the order claims no arm: the refusal is reported as ever."
  (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
    (agent-repl-test-verbs--with
        '((:nuke . (:response (:arm :error :value (:cause (:arm :blocked :value nil))))))
      ;; Act
      (agent-repl-verb-nuke "ws-one")
      ;; Assert
      (should (agent-repl-test-verbs--messaged-p "nuke refused: blocked")))))

(ert-deftest agent-repl-verbs-merge-sends-a-one-shot-of-its-own-branch ()
  "MergeWorkspace from Emacs is a ONE-SHOT of the workspace's own branch.
The requesting workspace is the ref; the mode is `one_shot' with the
`own_branch' landing, which closes the workspace once it lands."
  ;; Arrange
  (agent-repl-test-verbs--with nil
    ;; Act
    (agent-repl-verb-merge "ws-one")
    ;; Assert
    (should (equal (agent-repl-test-verbs--request :merge)
                   (list :workspace (agent-repl-test-verbs--ref)
                         :mode '(:arm :one-shot
                                 :value (:landing (:arm :own-branch :value nil))))))))

(ert-deftest agent-repl-verbs-open-echoes-the-given-ref ()
  "OpenWorkspace carries the closed row's own ref, not the current one's."
  (let ((closed-ref (agent-repl-test-verbs--ref "closed-id" "/tmp/closed")))
    (agent-repl-test-verbs--with nil
      (agent-repl-verb-open closed-ref)
      (should (equal (plist-get (agent-repl-test-verbs--request :open) :workspace)
                     closed-ref)))))

(ert-deftest agent-repl-verbs-open-sends-an-op-id ()
  "Every open carries a client-minted op_id -- the token that correlates it
to the stages the daemon pushes on the WatchDaemon channel while the rpc
is still in flight."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (plist-get (agent-repl-test-verbs--request :open) :op-id))))

(ert-deftest agent-repl-verbs-open-reports-the-request-leaving ()
  "The open says so the instant it is sent, naming the workspace."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: opening workspace /tmp/closed…"))))

(ert-deftest agent-repl-verbs-open-reports-its-completion ()
  "A successful open says the workspace is open, not merely that it asked."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace opened: /tmp/closed"))))

(ert-deftest agent-repl-verbs-open-reports-each-daemon-stage ()
  "A stage the daemon pushes for THIS open reaches the minibuffer."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (let ((op-id (plist-get (agent-repl-test-verbs--request :open) :op-id)))
      ;; The op is retired by the success above, so the stage is replayed
      ;; against a freshly registered one -- the correlation, not the verb,
      ;; is what this pins.
      (agent-repl-mutation-progress-register
       op-id :on-stage (lambda (stage)
                         (agent-repl-workspace-progress-report :open stage)))
      (agent-repl-mutation-progress-handle
       (list :op-id op-id
             :event (list :arm :open :value (list :stage :starting-session)))))
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: starting the workspace's session…"))))

(ert-deftest agent-repl-verbs-open-retires-its-op-on-success ()
  "The open's outcome rides its rpc, so nothing on the stream would ever
retire the registration: the success does."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (let ((op-id (plist-get (agent-repl-test-verbs--request :open) :op-id))
          (stages nil))
      (cl-letf (((symbol-function 'agent-repl-workspace-progress-report)
                 (lambda (_kind phase &rest _) (push phase stages))))
        (agent-repl-mutation-progress-handle
         (list :op-id op-id
               :event (list :arm :open :value (list :stage :starting-session)))))
      (should-not stages))))

(ert-deftest agent-repl-verbs-open-retires-its-op-when-nobody-answers ()
  "A daemon that never answered will never push a stage either."
  (agent-repl-test-verbs--with
      (list (cons :open (list :failure "connection refused")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (let ((op-id (plist-get (agent-repl-test-verbs--request :open) :op-id))
          (stages nil))
      (cl-letf (((symbol-function 'agent-repl-workspace-progress-report)
                 (lambda (_kind phase &rest _) (push phase stages))))
        (agent-repl-mutation-progress-handle
         (list :op-id op-id
               :event (list :arm :open :value (list :stage :starting-session)))))
      (should-not stages))))

(ert-deftest agent-repl-verbs-restart-request-carries-no-force ()
  "A restart request is the workspace alone: no `force' key."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-restart "ws-one")
    (should-not (plist-member (agent-repl-test-verbs--request :restart) :force))))

(ert-deftest agent-repl-verbs-restart-closes-the-composer-on-the-send ()
  "A restart's SEND is what closes the composer, not the later push."
  (agent-repl-test-verbs--with nil
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one")
    ;; Assert
    (should (equal agent-repl-test-verbs--restart-holds '("ws-one")))))

(ert-deftest agent-repl-verbs-restart-refused-gives-the-composer-back ()
  "A refused restart bounces nothing, so it holds nothing shut."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:cause (:arm :not-yet-adopted :value nil))))))
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one")
    ;; Assert
    (should (null agent-repl-test-verbs--restart-holds))))

(ert-deftest agent-repl-verbs-restart-success-says-nothing ()
  "A restart answers once it HAPPENED, so its success shows no notice.
Owner ruling 2026-10-08: \"i really expect no notification, i expect it to
just restart instantly\" -- this replaces the earlier \"under way\" notice."
  (agent-repl-test-verbs--with nil
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one")
    ;; Assert: the log's own records echo in batch; nothing else is said.
    (should-not (cl-remove-if (lambda (m) (string-match-p "\\[agent-repl\\] elisp\\." m))
                              agent-repl-test-verbs--messages))))

(ert-deftest agent-repl-verbs-restart-relaunch-failed-is-said-as-a-failure ()
  "A relaunch that failed is said in the echo area as the restart failing."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:cause (:arm :relaunch-failed
                                                :value (:detail "the prelaunched shim died")))))))
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one")
    ;; Assert
    (should (agent-repl-test-verbs--messaged-p "agent-repl: the restart failed: the prelaunched shim died"))))

(ert-deftest agent-repl-verbs-restart-relaunch-failed-gives-the-composer-back ()
  "A failed relaunch bounced nothing to completion, so it holds nothing shut."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:cause (:arm :relaunch-failed
                                                :value (:detail "the prelaunched shim died")))))))
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one")
    ;; Assert
    (should (null agent-repl-test-verbs--restart-holds))))

(ert-deftest agent-repl-verbs-restart-relaunch-failed-is-not-reported-as-a-refusal ()
  "The failure is claimed: no generic \"restart refused\" line beside it."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:cause (:arm :relaunch-failed
                                                :value (:detail "x")))))))
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one")
    ;; Assert
    (should-not (agent-repl-test-verbs--messaged-p "restart refused"))))

(ert-deftest agent-repl-verbs-restart-waits-with-no-timeout ()
  "A restart's answer waits as long as the daemon's restart does.
Owner ruling 2026-10-10: a restart over a merge mid-step happens at the
step's end, however long that takes, and the call waits for it."
  (agent-repl-test-verbs--with nil
    ;; Arrange
    (let ((timeout 'unset))
      (cl-letf (((symbol-function 'agent-repl-rpc-restart-workspace)
                 (lambda (_conn _request &rest keys)
                   (setq timeout (plist-get keys :timeout)))))
        ;; Act
        (agent-repl-verb-restart "ws-one"))
      ;; Assert
      (should (eq timeout :none)))))

;;;; ---- Interrupt -------------------------------------------------------

(ert-deftest agent-repl-verbs-interrupt-targets-the-turn ()
  "`agent-repl-verb-interrupt' aims the Interrupt at the running turn, always
asking it to roll an unanswered prompt back into the composer."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :interrupted-turn :value nil)))))
    (agent-repl-verb-interrupt "ws-one")
    (should (equal (plist-get (agent-repl-test-verbs--request :interrupt) :target)
                   '(:arm :turn :value (:roll-back-unanswered t))))))

(ert-deftest agent-repl-verbs-interrupt-rolled-back-unanswered-refills-the-composer ()
  "A stop that rolled its unanswered prompt back hands the answer to the
composer's rollback refill, and claims no plain stop."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value
                                  (:arm :rolled-back-unanswered :value (:prompt (:content nil)))))))
    (let ((refilled nil))
      (cl-letf (((symbol-function 'agent-repl--input-rollback-done)
                 (lambda (ws success) (push (list ws success) refilled))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should (equal refilled '(("ws-one" (:prompt (:content nil))))))
      (should-not (agent-repl-test-verbs--messaged-p "turn stopped")))))

(ert-deftest agent-repl-verbs-interrupt-not-rolled-back-says-the-prompt-was-kept ()
  "A stop whose rollback was refused says the turn stopped, the prompt was
kept, and why."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value
                                  (:arm :interrupted-turn
                                   :value (:not-rolled-back (:cause (:arm :first-prompt :value nil))))))))
    (let ((refilled nil))
      (cl-letf (((symbol-function 'agent-repl--input-rollback-done)
                 (lambda (&rest args) (push args refilled))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should-not refilled)
      (should (agent-repl-test-verbs--messaged-p "turn stopped, prompt kept"))
      (should (agent-repl-test-verbs--messaged-p
               (agent-repl--input-rollback-refusal-text '(:arm :first-prompt :value nil)))))))

(ert-deftest agent-repl-verbs-interrupt-first-ask-does-not-confirm-agents ()
  "The first ask never speculatively sets `confirm_agents'."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :interrupted-turn :value nil)))))
    (agent-repl-verb-interrupt "ws-one")
    (should (eq (plist-get (agent-repl-test-verbs--request :interrupt) :confirm-agents) nil))))

(ert-deftest agent-repl-verbs-interrupt-turn-stopped-is-a-message ()
  "An interrupted turn draws a calm message."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :interrupted-turn :value nil)))))
    (agent-repl-verb-interrupt "ws-one")
    (should (agent-repl-test-verbs--messaged-p "turn stopped"))))

(ert-deftest agent-repl-verbs-interrupt-nothing-running-is-a-message-not-an-error ()
  "No turn in flight is answered by `nothing_running' and drawn as a message,
never signalled as an error."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :nothing-running :value nil)))))
    (agent-repl-verb-interrupt "ws-one")
    (should (agent-repl-test-verbs--messaged-p "nothing to interrupt"))))

(ert-deftest agent-repl-verbs-interrupt-nothing-running-hands-the-offer-to-the-composer ()
  "A rollback offer on `nothing_running' is the stop key's second press:
the offer goes to input.el, and nothing claims there was nothing to do."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value
                                  (:arm :nothing-running
                                   :value (:rollback (:arm :choose :value nil)))))))
    (let ((offered nil))
      (cl-letf (((symbol-function 'agent-repl--input-rollback-offer)
                 (lambda (ws offer) (push (list ws offer) offered))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should (equal offered '(("ws-one" (:arm :choose :value nil)))))
      (should-not (agent-repl-test-verbs--messaged-p "nothing to interrupt")))))

(ert-deftest agent-repl-verbs-interrupt-nothing-running-without-an-offer-rolls-nothing-back ()
  "With no prompt to roll back, nothing is offered to the composer."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value
                                  (:arm :nothing-running :value (:rollback nil))))))
    (let ((offered nil))
      (cl-letf (((symbol-function 'agent-repl--input-rollback-offer)
                 (lambda (&rest args) (push args offered))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should-not offered)
      (should (agent-repl-test-verbs--messaged-p "nothing to interrupt")))))

(ert-deftest agent-repl-verbs-interrupt-merge-queue-offered-offers-no-rollback ()
  "A stop that raised the merge-queue question hands nothing to the composer's
rollback, and never says there was nothing to interrupt."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :merge-queue-offered :value nil)))))
    (let ((offered nil))
      (cl-letf (((symbol-function 'agent-repl--input-rollback-offer)
                 (lambda (&rest args) (push args offered)))
                ((symbol-function 'agent-repl--webview-window) (lambda (_ws) nil)))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should-not offered)
      (should-not (agent-repl-test-verbs--messaged-p "nothing to interrupt"))
      (should (agent-repl-test-verbs--messaged-p "Evict, Prioritize or Cancel")))))

(ert-deftest agent-repl-verbs-interrupt-merge-queue-offered-selects-the-webview ()
  "A stop that raised the merge-queue question selects the workspace's webview
window, so the next key reaches the focused question."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :merge-queue-offered :value nil)))))
    (let ((selected nil))
      (cl-letf (((symbol-function 'agent-repl--webview-window) (lambda (ws) (list :win ws)))
                ((symbol-function 'select-window) (lambda (win &rest _) (push win selected))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should (equal selected '((:win "ws-one")))))))

(ert-deftest agent-repl-verbs-interrupt-merge-queue-offered-without-a-webview-selects-nothing ()
  "With no window showing the webview, nothing is selected and the message
still says where the question stands."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :merge-queue-offered :value nil)))))
    (let ((selected nil))
      (cl-letf (((symbol-function 'agent-repl--webview-window) (lambda (_ws) nil))
                ((symbol-function 'select-window) (lambda (win &rest _) (push win selected))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should-not selected)
      (should (agent-repl-test-verbs--messaged-p "in its bubble")))))

(ert-deftest agent-repl-verbs-interrupt-merge-queue-offered-is-logged ()
  "Raising the merge-queue question is recorded through the canonical log."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :merge-queue-offered :value nil)))))
    (let ((records nil))
      (cl-letf (((symbol-function 'agent-repl--webview-window) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--info)
                 (lambda (ws fmt &rest args) (push (list ws (apply #'format fmt args)) records))))
        ;; Act
        (agent-repl-verb-interrupt "ws-one"))
      ;; Assert
      (should (cl-find-if (lambda (r) (string-match-p "elisp.verbs.interrupt-merge-queue-offered ws=ws-one" (cadr r)))
                          records)))))

(ert-deftest agent-repl-verbs-interrupt-detached-count-is-stated ()
  "A confirmed stop that also ended agents states the count."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :interrupted-detached :value (:count 3))))))
    (agent-repl-verb-interrupt "ws-one")
    (should (agent-repl-test-verbs--messaged-p "3 agents also ended"))))

(ert-deftest agent-repl-verbs-interrupt-confirm-yes-resends-with-confirm-agents ()
  "Confirming the challenge re-sends the identical stop with `confirm_agents'."
  (agent-repl-test-verbs--with nil
    (let ((calls 0)
          (requests nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl-rpc-interrupt)
                 (lambda (_conn request &rest keys)
                   (push request requests)
                   (setq calls (1+ calls))
                   (funcall (plist-get keys :on-response)
                            (if (= calls 1)
                                '(:arm :error :value
                                  (:cause (:arm :confirm-required :value (:live-agent-count 2))))
                              '(:arm :success :value (:arm :interrupted-turn :value nil)))))))
        (agent-repl-verb-interrupt "ws-one")
        ;; Two calls: the challenged first ask, then the confirmed re-send.
        (should (= calls 2))
        ;; `requests' is newest-first: the re-send carries confirm, the first not.
        (should (eq (plist-get (car requests) :confirm-agents) t))
        (should (eq (plist-get (cadr requests) :confirm-agents) nil))))))

(ert-deftest agent-repl-verbs-interrupt-confirm-no-leaves-the-turn-running ()
  "Declining the challenge sends nothing further and says the turn stands."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :error :value
                                  (:cause (:arm :confirm-required :value (:live-agent-count 2)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (agent-repl-verb-interrupt "ws-one")
      (should (agent-repl-test-verbs--messaged-p "turn left running"))
      ;; Only the first ask went out; declining resends nothing.
      (should (= 1 (length agent-repl-test-verbs--sent))))))

(ert-deftest agent-repl-verbs-interrupt-no-session-is-reported ()
  "A `no_session' refusal is surfaced through the generic refusal path."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :error :value (:cause (:arm :no-session :value nil))))))
    (agent-repl-verb-interrupt "ws-one")
    (should (agent-repl-test-verbs--messaged-p "interrupt refused: no-session"))))

(ert-deftest agent-repl-verbs-interrupt-transport-failure-is-loud ()
  "A transport failure of the interrupt is reported, never swallowed."
  (agent-repl-test-verbs--with
      '((:interrupt . (:failure (:kind :unreachable :message "boom"))))
    (agent-repl-verb-interrupt "ws-one")
    (should (agent-repl-test-verbs--messaged-p "interrupt failed"))))

(ert-deftest agent-repl-verbs-interrupt-command-targets-current-workspace ()
  "`agent-repl-interrupt-turn' acts on the current workspace's ref."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :interrupted-turn :value nil)))))
    (agent-repl-interrupt-turn)
    (should (equal (plist-get (agent-repl-test-verbs--request :interrupt) :workspace)
                   (agent-repl-test-verbs--ref)))))

(ert-deftest agent-repl-verbs-set-priority-carries-the-level ()
  "SetWorkspacePriority carries the level arm when one is given."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-priority "ws-one" :p1)
    (should (equal (plist-get (agent-repl-test-verbs--request :set-priority) :priority)
                   (list :arm :p1 :value nil)))))

(ert-deftest agent-repl-verbs-set-priority-clear-omits-the-field ()
  "CLEARING a priority is the ABSENCE of the field, never a sentinel level."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-priority "ws-one" nil)
    (should-not (plist-get (agent-repl-test-verbs--request :set-priority) :priority))))

(ert-deftest agent-repl-verbs-set-hidden-carries-the-hide-arm ()
  "Hiding a workspace names it and sets the `hide' visibility arm."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-hidden "ws-one" t)
    (let ((request (agent-repl-test-verbs--request :set-hidden)))
      (should (equal (plist-get request :workspace) (agent-repl-test-verbs--ref)))
      (should (equal (plist-get request :visibility) (list :arm :hide :value nil))))))

(ert-deftest agent-repl-verbs-set-hidden-carries-the-show-arm ()
  "Showing a workspace again sets the `show' visibility arm."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-hidden "ws-one" nil)
    (should (equal (plist-get (agent-repl-test-verbs--request :set-hidden) :visibility)
                   (list :arm :show :value nil)))))

(ert-deftest agent-repl-verbs-set-hidden-success-echoes-the-change ()
  "An accepted hide says so in the echo area; the push is the new state."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-hidden "ws-one" t)
    (should (agent-repl-test-verbs--messaged-p "ws-one hidden"))))

(ert-deftest agent-repl-verbs-hide-command-targets-the-current-workspace ()
  "`agent-repl-hide-workspace' (`SPC TAB h') hides the current workspace."
  (agent-repl-test-verbs--with nil
    (agent-repl-hide-workspace)
    (let ((request (agent-repl-test-verbs--request :set-hidden)))
      (should (equal (plist-get request :workspace) (agent-repl-test-verbs--ref)))
      (should (eq (plist-get (plist-get request :visibility) :arm) :hide)))))

;;;; ---- SendHiddenKeepalives (`SPC j R') ---------------------------------

(defun agent-repl-test-verbs--keepalive (name arm &optional value)
  "Return a decoded SendHiddenKeepalivesWorkspace for NAME with outcome ARM."
  (list :workspace (list :id (concat "id-" name) :dir (concat "/work/" name))
        :outcome (list :arm arm :value value)))

(defun agent-repl-test-verbs--keepalives-answer (workspaces)
  "Return the answers alist for a SendHiddenKeepalives success over WORKSPACES."
  (list (cons :hidden-keepalives
              (list :response (list :arm :success :value (list :workspaces workspaces))))))

(ert-deftest agent-repl-verbs-hidden-keepalives-sends-the-empty-request ()
  "The command asks the daemon once, naming no workspace."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--keepalives-answer nil)
    (agent-repl-send-hidden-keepalives)
    (should (assq :hidden-keepalives agent-repl-test-verbs--sent))
    (should-not (agent-repl-test-verbs--request :hidden-keepalives))))

(ert-deftest agent-repl-verbs-hidden-keepalives-counts-the-sent ()
  "Every workspace whose keep-alive went out is counted, not named."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--keepalives-answer
       (list (agent-repl-test-verbs--keepalive "alpha" :sent)
             (agent-repl-test-verbs--keepalive "beta" :sent)))
    (agent-repl-send-hidden-keepalives)
    (should (agent-repl-test-verbs--messaged-p "keep-alive sent to 2 of 2 hidden workspace(s)"))))

(ert-deftest agent-repl-verbs-hidden-keepalives-names-each-one-not-sent ()
  "A workspace whose keep-alive did not go out is named beside why."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--keepalives-answer
       (list (agent-repl-test-verbs--keepalive "alpha" :sent)
             (agent-repl-test-verbs--keepalive "beta" :not-running)))
    (agent-repl-send-hidden-keepalives)
    (should (agent-repl-test-verbs--messaged-p
             "keep-alive sent to 1 of 2 hidden workspace(s) · beta not running"))))

(ert-deftest agent-repl-verbs-hidden-keepalives-says-so-when-none-are-hidden ()
  "An empty hidden section is an answer, said in words."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--keepalives-answer nil)
    (agent-repl-send-hidden-keepalives)
    (should (agent-repl-test-verbs--messaged-p "no hidden workspaces to keep warm"))))

(ert-deftest agent-repl-verbs-hidden-keepalives-words-each-outcome ()
  "Each outcome the daemon names has its own words."
  (dolist (case `((:failed (:detail "stream closed") "failed (stream closed)")
                  (:skipped (:reason (:arm :busy :value (:detail "a turn is in flight")))
                            "skipped: a turn is in flight")
                  (:skipped (:reason (:arm :detached-work-live :value (:live-items 2)))
                            "skipped: 2 background item(s) running")
                  (:skipped (:reason (:arm :cache-lapsed :value (:lapsed-at-ms 1)))
                            "skipped: its cache already lapsed")))
    (should (equal (agent-repl-verbs--keepalive-outcome-text
                    (list :arm (nth 0 case) :value (nth 1 case)))
                   (nth 2 case)))))

(ert-deftest agent-repl-verbs-hidden-keepalives-unknown-outcome-fails-loudly ()
  "An outcome arm with no wording is a contract breach, never silence."
  (should-error (agent-repl-verbs--keepalive-outcome-text (list :arm :mystery :value nil))))

(ert-deftest agent-repl-verbs-hidden-keepalives-refusal-is-reported ()
  "An unreadable registry reaches the generic refusal report."
  (agent-repl-test-verbs--with
      '((:hidden-keepalives
         . (:response (:arm :error
                       :value (:cause (:arm :registry-unreadable
                                       :value (:detail "database is locked")))))))
    (agent-repl-send-hidden-keepalives)
    (should (agent-repl-test-verbs--messaged-p "send-hidden-keepalives refused"))))

(ert-deftest agent-repl-verbs-hidden-keepalives-transport-failure-is-loud ()
  "A daemon that does not answer is reported, never swallowed."
  (agent-repl-test-verbs--with
      '((:hidden-keepalives . (:failure (:kind :unreachable :message "boom"))))
    (agent-repl-send-hidden-keepalives)
    (should (agent-repl-test-verbs--messaged-p "send-hidden-keepalives failed"))))

(ert-deftest agent-repl-verbs-roster-or-view-prefers-the-given-roster ()
  "A given roster is answered verbatim, whatever the view holds."
  (let ((agent-repl-roster-view '(:view t)))
    (should (equal (agent-repl-verbs--roster-or-view '(:given t)) '(:given t)))))

(ert-deftest agent-repl-verbs-roster-or-view-defaults-to-the-view ()
  "A nil roster answers `agent-repl-roster-view'."
  (let ((agent-repl-roster-view '(:view t)))
    (should (equal (agent-repl-verbs--roster-or-view nil) '(:view t)))))

(ert-deftest agent-repl-verbs-roster-or-view-is-nil-before-any-push ()
  "A nil roster before any push answers nil."
  (let ((agent-repl-roster-view nil))
    (should-not (agent-repl-verbs--roster-or-view nil))))

(defconst agent-repl-test-verbs--source
  (expand-file-name "verbs.el" (file-name-directory (or load-file-name buffer-file-name)))
  "verbs.el beside this suite, for the tests that read its source.")

(ert-deftest agent-repl-verbs-roster-defaulting-has-one-home ()
  "Every roster reader in verbs.el defaults through `agent-repl-verbs--roster-or-view'.
A reader that hand-rolls the defaulting reads the view a second time and fails here."
  ;; Arrange.
  (with-temp-buffer
    (insert-file-contents agent-repl-test-verbs--source)
    ;; Act.
    (let ((reads (how-many (regexp-quote "(boundp 'agent-repl-roster-view)")
                           (point-min) (point-max))))
      ;; Assert.
      (should (= reads 1)))))

(ert-deftest agent-repl-verbs-hide-lands-on-the-first-drawn-tab ()
  "An accepted hide switches to the first tab the bar draws."
  (agent-repl-test-verbs--with nil
    ;; Arrange.
    (setq agent-repl-test-verbs--drawn '("ws-zero" "ws-one" "ws-two"))
    ;; Act.
    (agent-repl-verb-set-hidden "ws-one" t)
    ;; Assert.
    (should (equal agent-repl-test-verbs--switch-requests '("ws-zero")))))

(ert-deftest agent-repl-verbs-hide-of-the-first-tab-lands-on-the-next ()
  "Hiding the first drawn tab lands on the one after it, whether or not the push removed it yet."
  (agent-repl-test-verbs--with nil
    ;; Arrange: the push that takes ws-one off the bar has not arrived.
    (setq agent-repl-test-verbs--drawn '("ws-one" "ws-two"))
    ;; Act.
    (agent-repl-verb-set-hidden "ws-one" t)
    ;; Assert.
    (should (equal agent-repl-test-verbs--switch-requests '("ws-two")))))

(ert-deftest agent-repl-verbs-hide-of-the-only-tab-stays-and-logs ()
  "A hide that leaves no other drawn tab requests no switch and records why."
  (agent-repl-test-verbs--with nil
    ;; Arrange.
    (setq agent-repl-test-verbs--drawn '("ws-one"))
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (ws fmt &rest args) (push (cons ws (apply #'format fmt args)) logged))))
        ;; Act.
        (agent-repl-verb-set-hidden "ws-one" t))
      ;; Assert.
      (should-not agent-repl-test-verbs--switch-requests)
      (should (member '("ws-one" . "elisp.verbs.hide-landing-none ws=ws-one") logged)))))

(ert-deftest agent-repl-verbs-hide-refused-requests-no-switch ()
  "A refused hide leaves the frame where it is."
  (agent-repl-test-verbs--with
      '((:set-hidden . (:response (:arm :error
                                   :value (:cause (:arm :unknown-workspace :value nil))))))
    ;; Arrange.
    (setq agent-repl-test-verbs--drawn '("ws-zero" "ws-one"))
    ;; Act.
    (agent-repl-verb-set-hidden "ws-one" t)
    ;; Assert.
    (should-not agent-repl-test-verbs--switch-requests)))

(ert-deftest agent-repl-verbs-show-lands-on-the-shown-workspace ()
  "An accepted show switches to the workspace brought back."
  (agent-repl-test-verbs--with nil
    ;; Arrange.
    (setq agent-repl-test-verbs--drawn '("ws-zero"))
    ;; Act.
    (agent-repl-verb-set-hidden "ws-one" nil)
    ;; Assert.
    (should (equal agent-repl-test-verbs--switch-requests '("ws-one")))))

(ert-deftest agent-repl-verbs-hidden-rows-flattens-and-drops-closed ()
  "The hidden section's open rows, children included; closed rows are dropped."
  ;; Arrange.
  (let* ((child (agent-repl-test-verbs--row
                 :workspace (list :workspace (agent-repl-test-verbs--ref "kid" "/tmp/k"))
                 :name (list :text "child")))
         (parent (agent-repl-test-verbs--row :name (list :text "parent")
                                             :children (list child)))
         (closed (agent-repl-test-verbs--row
                  :workspace (list :workspace (agent-repl-test-verbs--ref "cid" "/tmp/c"))
                  :name (list :text "closed")
                  :closed (list :closed t)))
         (roster (agent-repl-test-verbs--roster nil nil nil (list parent closed))))
    ;; Act.
    (let ((rows (agent-repl-verbs--hidden-rows roster)))
      ;; Assert.
      (should (equal (mapcar #'agent-repl-roster-row-name rows) '("parent" "child"))))))

(ert-deftest agent-repl-verbs-hidden-rows-reads-the-roster-view ()
  "With no ROSTER the hidden rows come from `agent-repl-roster-view'."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil nil
                                        (list (agent-repl-test-verbs--row
                                               :name (list :text "hid"))))))
    ;; Act / Assert.
    (should (equal (mapcar #'agent-repl-roster-row-name (agent-repl-verbs--hidden-rows))
                   '("hid")))))

(ert-deftest agent-repl-verbs-show-hidden-offers-tab-names-shows-and-switches ()
  "`SPC TAB H' offers the hidden workspaces' tabs, shows the pick and switches to it."
  ;; Arrange.
  (let* ((hidden (agent-repl-test-verbs--row
                  :workspace (list :workspace (agent-repl-test-verbs--ref "hid" "/tmp/h"))
                  :name (list :text "hidden-one")))
         (agent-repl-roster-view (agent-repl-test-verbs--roster nil nil nil (list hidden)))
         (offered nil))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "hid") "hidden-one·repo")))
                ((symbol-function 'completing-read)
                 (lambda (_p candidates &rest _) (setq offered candidates) "hidden-one·repo")))
        ;; Act.
        (agent-repl-show-hidden-workspace)
        ;; Assert.
        (should (equal offered '("hidden-one·repo")))
        (should (eq (plist-get (plist-get (agent-repl-test-verbs--request :set-hidden)
                                          :visibility)
                               :arm)
                    :show))
        (should (equal agent-repl-test-verbs--switch-requests '("hidden-one·repo")))))))

(ert-deftest agent-repl-verbs-show-hidden-with-none-refuses-and-logs ()
  "`SPC TAB H' with nothing hidden is a user error, recorded, and sends nothing."
  ;; Arrange.
  (let ((agent-repl-roster-view (agent-repl-test-verbs--roster nil))
        (logged nil))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act / Assert.
        (should-error (agent-repl-show-hidden-workspace) :type 'user-error)
        (should (member "elisp.verbs.show-hidden-none" logged))
        (should-not agent-repl-test-verbs--sent)
        (should-not agent-repl-test-verbs--switch-requests)))))

;;;; ---- MoveWorkspace ----

(defmacro agent-repl-test-verbs--with-two-refs (&rest body)
  "Run BODY with `ws-one' and `ws-two' resolving to two distinct refs."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl-host-ref)
              (lambda (ws) (agent-repl-test-verbs--ref (concat "id-" ws) (concat "/w/" ws)))))
     ,@body))

(ert-deftest agent-repl-verbs-move-carries-both-refs ()
  "MoveWorkspace names the workspace to move and the target by ref."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-two-refs
      ;; Act.
      (agent-repl-verb-move "ws-one" "ws-two")
      ;; Assert.
      (let ((request (agent-repl-test-verbs--request :move)))
        (should (equal (plist-get request :workspace) (agent-repl-test-verbs--ref "id-ws-one" "/w/ws-one")))
        (should (equal (plist-get request :target) (agent-repl-test-verbs--ref "id-ws-two" "/w/ws-two")))))))

(ert-deftest agent-repl-verbs-move-success-says-so ()
  "A move the daemon made is reported to the user."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-two-refs
      ;; Act.
      (agent-repl-verb-move "ws-one" "ws-two")
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p "moved ws-one to ws-two's place")))))

(ert-deftest agent-repl-verbs-move-refusals-take-the-generic-path ()
  "A refused move is reported by the generic refusal path."
  (agent-repl-test-verbs--with
      '((:move . (:response (:arm :error :value (:cause (:arm :unknown-workspace
                                                              :value (:workspace (:id "id-ws-two" :dir "/w/ws-two"))))))))
    (agent-repl-test-verbs--with-two-refs
      ;; Act.
      (agent-repl-verb-move "ws-one" "ws-two")
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p "move refused: unknown-workspace")))))

;;;; ---- Ack handling: one test per outcome ----

(ert-deftest agent-repl-verbs-close-success-tears-the-tab-down ()
  "A quiet close proceeds with the view teardown."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-close "ws-one")
    (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))

(ert-deftest agent-repl-verbs-close-blocked-leaves-the-tab ()
  "A BLOCKED close draws no dialog and leaves the tab in place."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error :value (:cause (:arm :blocked :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-blocked-names-the-footer ()
  "The blocked message sends the user to the footer, which carries the reasons."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error :value (:cause (:arm :blocked :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p
             "close blocked -- see the workspace footer"))))

(ert-deftest agent-repl-verbs-close-blocked-echoes-the-summary ()
  "The daemon's own composed sentence is echoed verbatim when it sent one."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked
                                              :value (:turn-in-flight t :live-work 0
                                                      :held-prompts 0 :merge-queued nil
                                                      :summary "a turn is running")))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p "close blocked -- a turn is running"))))

(ert-deftest agent-repl-verbs-close-blocked-empty-summary-names-the-footer ()
  "With no composed sentence the footer stays the place the reasons are read."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked
                                              :value (:turn-in-flight nil :live-work 2
                                                      :held-prompts 0 :merge-queued nil
                                                      :summary "")))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p
             "close blocked -- see the workspace footer"))))

(ert-deftest agent-repl-verbs-close-blocked-with-evidence-leaves-the-tab ()
  "Evidence on the refusal does not make it any less a refusal."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked
                                              :value (:turn-in-flight t :live-work 1
                                                      :held-prompts 3 :merge-queued t
                                                      :summary "busy")))))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-transport-failure-leaves-the-tab ()
  "Nobody answering is not permission to tear the tab down."
  (agent-repl-test-verbs--with '((:close . (:failure (:kind :transport))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-kill-success-tears-the-tab-down ()
  "A kill's success proceeds with the view teardown."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-kill "ws-one")
    (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))

(ert-deftest agent-repl-verbs-nuke-success-tears-the-tab-down ()
  "A nuke's success proceeds with the view teardown."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-nuke "ws-one")
    (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))

;;;; ---- Kill and nuke: the tab closes on the ack ----

(ert-deftest agent-repl-verbs-an-accepted-teardown-closes-the-tab-at-once ()
  "The daemon's `accepted' ack tears the tab down; the teardown's end is later."
  (dolist (verb '((:kill . agent-repl-verb-kill) (:nuke . agent-repl-verb-nuke)))
    (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (agent-repl-test-verbs--with
          (list (cons (car verb) '(:response (:arm :accepted :value (:op-id "op")))))
        ;; Act
        (funcall (cdr verb) "ws-one")
        ;; Assert
        (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))))

(ert-deftest agent-repl-verbs-an-accepted-teardown-awaits-its-end-on-the-channel ()
  "The op stays registered after the ack: its end arrives on the channel."
  (agent-repl-test-verbs--with '((:kill . (:response (:arm :accepted :value (:op-id "op")))))
    (agent-repl-verb-kill "ws-one")
    (should (gethash (plist-get (agent-repl-test-verbs--request :kill) :op-id)
                     agent-repl-mutation-progress--pending))))

(ert-deftest agent-repl-verbs-a-failed-teardown-is-recorded-and-echoed ()
  "A teardown that failed after its tab closed is an ERROR and an echo."
  (agent-repl-test-verbs--with '((:kill . (:response (:arm :accepted :value (:op-id "op")))))
    (agent-repl-verb-kill "ws-one")
    (let ((op-id (plist-get (agent-repl-test-verbs--request :kill) :op-id))
          errors)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-mutation-progress-handle
         (list :op-id op-id
               :event '(:arm :kill :value (:arm :failed :value (:internal "could not stop"))))))
      ;; Assert
      (should (cl-some (lambda (e) (string-match-p "elisp.verbs.kill-teardown-failed ws=ws-one" e)) errors))
      (should (agent-repl-test-verbs--messaged-p "kill of ws-one FAILED after its tab closed: could not stop")))))

(ert-deftest agent-repl-verbs-a-failed-nuke-words-its-typed-refusal ()
  "A nuke's typed git refusal is echoed as the arm and its fields."
  (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
    (agent-repl-test-verbs--with '((:nuke . (:response (:arm :accepted :value (:op-id "op")))))
      (agent-repl-verb-nuke "ws-one")
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
        (agent-repl-mutation-progress-handle
         (list :op-id (plist-get (agent-repl-test-verbs--request :nuke) :op-id)
               :event '(:arm :nuke :value (:arm :failed :value (:arm :refusal :value (:cause (:arm :git-failed :value (:detail "locked")))))))))
      ;; Assert
      (should (agent-repl-test-verbs--messaged-p "nuke of ws-one FAILED after its tab closed: git-failed")))))

(ert-deftest agent-repl-verbs-a-refused-teardown-forgets-its-op-and-keeps-the-tab ()
  "A synchronous refusal ends the op: nothing will be pushed for it."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error :value (:cause (:arm :unknown-workspace :value nil))))))
    (agent-repl-verb-kill "ws-one")
    (should-not (gethash (plist-get (agent-repl-test-verbs--request :kill) :op-id)
                         agent-repl-mutation-progress--pending))
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-an-unanswered-teardown-forgets-its-op-and-keeps-the-tab ()
  "Nobody answering ends the op too, and is no permission to close the tab."
  (agent-repl-test-verbs--with '((:kill . (:failure (:kind :transport))))
    (agent-repl-verb-kill "ws-one")
    (should-not (gethash (plist-get (agent-repl-test-verbs--request :kill) :op-id)
                         agent-repl-mutation-progress--pending))
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-merge-success-says-enqueued-only ()
  "Merge success means ENQUEUED; Emacs holds no further merge state."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-merge "ws-one")
    (should (agent-repl-test-verbs--messaged-p "merge enqueued"))
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-error-arm-does-not-tear-the-tab-down ()
  "A daemon-authored refusal changes no editor state."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error
                             :value (:cause (:arm :unknown-workspace :value nil))))))
    (agent-repl-verb-kill "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

;;;; ---- Arm-generic refusal handling ----
;;
;; Every `<Rpc>Error' spells its refusal as a `cause' oneof whose ARM IS THE
;; REASON, so the handling is generic over the arm rather than a table this
;; file would have to keep in step with the contract.

(ert-deftest agent-repl-verbs-refusal-names-the-arm ()
  "A refusal draws the arm keyword, whatever arm the daemon chose."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :already-queued :value nil))))))
    (agent-repl-verb-merge "ws-one")
    (should (agent-repl-test-verbs--messaged-p "merge refused: already-queued"))))

(ert-deftest agent-repl-verbs-refusal-draws-an-unmodelled-arm-too ()
  "An arm this file has never heard of still reaches the user by name.
The point of arm-generic handling: a new refusal arm works the day the
daemon starts sending it, with no table to update here."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:cause (:arm :some-future-arm :value nil))))))
    (agent-repl-verb-restart "ws-one")
    (should (agent-repl-test-verbs--messaged-p "restart refused: some-future-arm"))))

(ert-deftest agent-repl-verbs-refusal-carries-the-arms-own-fields ()
  "Fields living inside the arm are drawn beside it."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error
                             :value (:cause (:arm :workspace-ref-mismatch
                                             :value (:registry-dir "/tmp/real")))))))
    (agent-repl-verb-kill "ws-one")
    (should (agent-repl-test-verbs--messaged-p "/tmp/real"))))

(ert-deftest agent-repl-verbs-open-vendor-start-failure-names-the-arm ()
  "An open refused because the VENDOR would not start names that arm."
  (agent-repl-test-verbs--with
      '((:open . (:response (:arm :error
                            :value (:cause (:arm :vendor-start-failed
                                            :value (:detail "the sdk threw")))))))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "open refused: vendor-start-failed"))))

(ert-deftest agent-repl-verbs-open-vendor-start-failure-echoes-the-detail ()
  "The shim's own account is the user's only lead, so it is drawn too."
  (agent-repl-test-verbs--with
      '((:open . (:response (:arm :error
                            :value (:cause (:arm :vendor-start-failed
                                            :value (:detail "the sdk threw")))))))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "the sdk threw"))))

(defconst agent-repl-test-verbs--unrestorable-refusal
  '((:open . (:response (:arm :error
                         :value (:cause (:arm :worktree-unrestorable
                                         :value (:dir "/tmp/gone" :branch "feature/gone"
                                                 :detail "the workspace's directory /tmp/gone is gone and its branch feature/gone no longer exists in /tmp/repo; there is nothing to restore it from")))))))
  "An OpenWorkspace refusal for a row with nothing left to restore from.")

(ert-deftest agent-repl-verbs-open-unrestorable-draws-the-daemons-sentence ()
  "An unrestorable row is drawn as the daemon's own sentence, not an arm keyword."
  (agent-repl-test-verbs--with agent-repl-test-verbs--unrestorable-refusal
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/gone"))
    (should (agent-repl-test-verbs--messaged-p
             "open refused: the workspace's directory /tmp/gone is gone and its branch feature/gone no longer exists"))))

(ert-deftest agent-repl-verbs-open-unrestorable-is-recorded-at-info ()
  "A user picking an unrestorable row is an ordinary answer: INFO, never WARN or ERROR."
  (agent-repl-test-verbs--with agent-repl-test-verbs--unrestorable-refusal
    (let (infos faults)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos)))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) faults)))
                ((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) faults))))
        (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/gone")))
      (should (cl-some (lambda (m) (string-prefix-p "elisp.verbs.open-unrestorable" m)) infos))
      (should-not (cl-some (lambda (m) (string-match-p "unrestorable\\|-refused" m)) faults)))))

(defun agent-repl-test-verbs--lock-holder-refusal (how)
  "An OpenWorkspace refusal whose lock holder /b/shim-lock failed as HOW."
  `((:open . (:response (:arm :error
                          :value (:cause (:arm :lock-holder-unavailable
                                          :value (:failure (:binary "/b/shim-lock" :how ,how)))))))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-says-the-helper-failed ()
  "An open refused because the shim's lock helper failed names the helper."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :spawn-failed :value (:os-error "spawn ENOENT")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "the shim's lock helper /b/shim-lock could not be spawned"))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-echoes-the-os-error ()
  "The OS error is the user's lead to the broken binary, so it is drawn too."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :spawn-failed :value (:os-error "spawn ENOENT")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p ": spawn ENOENT;"))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-states-the-exit-code ()
  "A holder that exited is drawn with its code and stderr."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :exited :value (:code 1 :stderr "EACCES")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "exited with code 1 before taking the lock (EACCES)"))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-states-the-signal ()
  "A holder a signal killed is drawn with the signal."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :signaled :value (:signal "SIGSEGV" :stderr "")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "was killed by SIGSEGV before taking the lock;"))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-states-the-line ()
  "A holder that answered the wrong line is drawn with that line."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :misanswered :value (:line "ok")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "answered \"ok\" instead of \"locked\" and was killed"))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-states-the-bound ()
  "A holder that never answered is drawn with the bound the shim waited."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :silent :value (:timeout-ms 5000)))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "gave no \"locked\" answer within 5000 ms and was killed"))))

(ert-deftest agent-repl-verbs-open-lock-holder-unavailable-denies-an-owner ()
  "The refusal must never read as another process owning the conversation."
  (agent-repl-test-verbs--with
      (agent-repl-test-verbs--lock-holder-refusal '(:arm :exited :value (:code 1 :stderr "")))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "no other process owns this conversation"))))

(ert-deftest agent-repl-verbs-empty-arm-draws-no-empty-fields ()
  "An empty arm is its own whole assertion and renders no trailing payload."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :already-merging :value nil))))))
    (agent-repl-verb-merge "ws-one")
    (should (agent-repl-test-verbs--messaged-p "merge refused: already-merging"))
    (should-not (agent-repl-test-verbs--messaged-p "already-merging ("))))

(ert-deftest agent-repl-verbs-transferring-away-goes-to-the-handover ()
  "`transferring_away' is the handover's ordering, not a user-facing failure."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :transferring-away
                                              :value (:address "127.0.0.1:9999")))))))
    (agent-repl-verb-merge "ws-one")
    (should (equal (length agent-repl-test-verbs--handover) 1))
    (should (equal (car (car agent-repl-test-verbs--handover)) "ws-one"))
    (should (equal (plist-get (nth 1 (car agent-repl-test-verbs--handover)) :value)
                   '(:address "127.0.0.1:9999")))))

(ert-deftest agent-repl-verbs-transferring-away-draws-nothing ()
  "The rollout it belongs to is supposed to be invisible, so it is silent."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :transferring-away
                                              :value (:address "127.0.0.1:9999")))))))
    (agent-repl-verb-merge "ws-one")
    (should-not (agent-repl-test-verbs--messaged-p "refused"))))

(ert-deftest agent-repl-verbs-not-yet-adopted-goes-to-the-handover ()
  "`not_yet_adopted' is the successor's own too-early refusal, also silent."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error
                             :value (:cause (:arm :not-yet-adopted :value nil))))))
    (agent-repl-verb-kill "ws-one")
    (should (equal (length agent-repl-test-verbs--handover) 1))
    (should-not (agent-repl-test-verbs--messaged-p "refused"))))

(ert-deftest agent-repl-verbs-handover-refusal-changes-no-editor-state ()
  "A handover refusal is not a close: the tab stays where it is."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :not-yet-adopted :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-non-blocked-arm-falls-through ()
  "Close claims only `blocked'; every other arm gets the generic report."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :unknown-workspace :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p "close refused: unknown-workspace"))
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-blocked-still-draws-the-footer-message ()
  "Close's one claimed arm keeps its prescribed treatment."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p
             "close blocked -- see the workspace footer"))
    (should-not (agent-repl-test-verbs--messaged-p "close refused"))))

(ert-deftest agent-repl-verbs-create-refusal-names-its-arm ()
  "A create refusal reports the daemon's own reason, e.g. an unknown repo."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :unknown-repository :value nil))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: the daemon refused it: unknown-repository"))))

(ert-deftest agent-repl-verbs-create-one-shot-policy-missing-names-the-directory ()
  "A one-shot refused for want of a repository policy names the directory the
user must write.  EMACS NEVER LOOKS AT THE FILESYSTEM FOR THIS: the daemon
detects the absence and Emacs draws the refusal it sent."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :one-shot-policy-missing
                                               :value (:repository-root "/src/p"
                                                       :policy-dir "/src/p/.agent-repl/prompts"
                                                       :missing-files ("oneshot-completion-directive.md"))))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot
                            :prompt "ship it")
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: /src/p states no one-shot policy -- write oneshot-completion-directive.md in /src/p/.agent-repl/prompts"))))

(ert-deftest agent-repl-verbs-create-one-shot-policy-missing-is-recorded-as-an-error ()
  "The refusal is a FAILED create and is recorded at the ERROR rung (owner
ruling, 2026-09-27), which is what a durable sweep for failed creates reads."
  (let (levels)
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws level &rest _) (push level levels))))
      (agent-repl-test-verbs--with
          '((:create . (:response (:arm :error
                                   :value (:cause (:arm :one-shot-policy-missing
                                                   :value (:repository-root "/src/p"
                                                           :policy-dir "/src/p/.agent-repl/prompts"
                                                           :missing-files nil)))))))
        (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot
                                :prompt "ship it")))
    (should (member "error" levels))))

(ert-deftest agent-repl-verbs-create-one-shot-policy-missing-with-no-files-names-the-directory-alone ()
  "With no file list the directory is still the answer, and no empty list is
drawn beside it."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :one-shot-policy-missing
                                               :value (:repository-root "/src/p"
                                                       :policy-dir "/src/p/.agent-repl/prompts"
                                                       :missing-files nil)))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot
                            :prompt "ship it")
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: /src/p states no one-shot policy -- write /src/p/.agent-repl/prompts"))))

(ert-deftest agent-repl-verbs-create-other-refusals-still-fall-through ()
  "The one-shot policy handler CLAIMS only its own arm; every other create
refusal still reaches the generic reporting."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :base-ref-unresolved
                                               :value (:ref "origin/main")))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: the daemon refused it: base-ref-unresolved"))))

(ert-deftest agent-repl-verbs-create-temporary-repository-names-the-folder ()
  "A create refused for a temporary repository names the folder and why."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :inside-temporary-directory
                                               :value (:dir "/private/tmp/scratch"
                                                       :temporary-root "/private/tmp")))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: /private/tmp/scratch is inside the temporary directory /private/tmp; agent-repl does not register temporary folders"))))

(ert-deftest agent-repl-verbs-register-refusal-sentence-falls-through-an-unknown-arm ()
  "A RegisterWorkspace arm with no sentence is named by keyword, never dropped."
  (should (equal (agent-repl-verbs-register-refusal-sentence
                  "/w/x" '(:cause (:arm :some-new-arm :value nil)))
                 "the daemon refused /w/x: some-new-arm")))

(ert-deftest agent-repl-verbs-create-naming-failed-names-the-cause ()
  "A create refused because the workspace could not be named states the cause
the daemon read off the failure."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :naming-failed
                                               :value (:model "haiku" :cause "timeout"
                                                       :attempts 2 :answer "")))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :initial-prompt "fix the flaky login test")
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: the workspace could not be named (timeout, 2 attempts)"))))

(ert-deftest agent-repl-verbs-create-naming-failed-quotes-the-model-s-answer ()
  "An INVALID answer is drawn, because it is what says whether to retry or to
supply a name by hand."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :naming-failed
                                               :value (:model "haiku" :cause "invalid_answer"
                                                       :attempts 2 :answer "Fix The Login")))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :initial-prompt "fix the flaky login test")
    (should (agent-repl-test-verbs--messaged-p "the model answered \"Fix The Login\""))))

(ert-deftest agent-repl-verbs-create-naming-failed-is-recorded-as-an-error ()
  "The refusal is a FAILED create and is recorded at the ERROR rung (owner
ruling, 2026-09-27), never only as a warning."
  (let (levels)
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws level &rest _) (push level levels))))
      (agent-repl-test-verbs--with
          '((:create . (:response (:arm :error
                                   :value (:cause (:arm :naming-failed
                                                   :value (:model "haiku" :cause "timeout"
                                                           :attempts 2 :answer "")))))))
        (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                                :initial-prompt "fix the flaky login test")))
    (should (member "error" levels))))

(ert-deftest agent-repl-verbs-create-streamed-naming-failed-reaches-the-minibuffer ()
  "The incident's shape: a naming refusal that arrives LATER on the progress
stream is echoed as the create's failure, not left as a *Messages* warning."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :accepted :value nil))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :parent (agent-repl-test-verbs--ref) :fork t)
    (agent-repl-mutation-progress-handle
     (list :op-id (plist-get (agent-repl-test-verbs--request :create) :op-id)
           :event (list :arm :create
                        :value (list :arm :failed
                                     :value (list :arm :refusal
                                                  :value '(:cause (:arm :naming-failed
                                                                   :value (:cause "invalid_answer" :attempts 2
                                                                           :answer "Describe the work"))))))))
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation FAILED: the workspace could not be named (invalid_answer, 2 attempts)"))))

(ert-deftest agent-repl-verbs-create-handover-refusal-is-not-a-failure ()
  "A handover arm is the rollout's ordering: it goes to the handover and is
never echoed as a failed create."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :not-yet-adopted :value nil))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (should (equal (length agent-repl-test-verbs--handover) 1))
    (should-not (agent-repl-test-verbs--messaged-p "FAILED"))))

(ert-deftest agent-repl-verbs-create-echoes-the-daemon-s-acceptance ()
  "The option-B ack is the first fact of the sequence, and it is shown."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :accepted :value nil))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :initial-prompt "fix the flaky login test")
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: the daemon accepted the workspace create…"))))

(ert-deftest agent-repl-verbs-create-acceptance-overtaken-by-a-stage-is-not-echoed ()
  "An ack arriving after the daemon's first stage push would put an older line
over a newer one, so it is recorded and not echoed."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-rpc-create-workspace)
               (lambda (_conn request &rest keys)
                 (push (cons :create request) agent-repl-test-verbs--sent)
                 (agent-repl-mutation-progress-handle
                  (list :op-id (plist-get request :op-id)
                        :event (list :arm :create
                                     :value (list :arm :entered-stage :value :deriving-name))))
                 (funcall (plist-get keys :on-response) (list :arm :accepted :value nil)))))
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                              :initial-prompt "fix the flaky login test"))
    (should-not (agent-repl-test-verbs--messaged-p "accepted the workspace create"))))

(ert-deftest agent-repl-verbs-create-synchronous-success-reports-completion ()
  "A daemon that answered success on the rpc still says the create is done."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :success
                               :value (:workspace (:id "new-id" :dir "/tmp/new-ws"))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :name "chosen")
    (should (agent-repl-test-verbs--messaged-p "agent-repl: workspace created: /tmp/new-ws"))))

(ert-deftest agent-repl-verbs-create-acks-immediately ()
  "A create echoes the ack the instant it runs, before any slow work: under
option B the minibuffer reflects the create at once and the real outcome
rides the progress channel."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :initial-prompt "fix the flaky login test")
    (should (agent-repl-test-verbs--messaged-p "agent-repl: creating workspace…"))))

(ert-deftest agent-repl-verbs-create-sends-an-op-id ()
  "Every create carries a client-minted op_id -- the token that correlates it
to the staged progress the daemon pushes on the WatchDaemon channel."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :name "chosen-name")
    (should (plist-get (agent-repl-test-verbs--request :create) :op-id))))

(ert-deftest agent-repl-verbs-create-stage-message-renders-deriving-name ()
  "The deriving-name stage renders the owner's exact minibuffer line."
  (agent-repl-test-verbs--with nil
    (agent-repl-verbs--create-stage-message :deriving-name)
    (should (agent-repl-test-verbs--messaged-p "agent-repl: deriving the workspace's name…"))))

(ert-deftest agent-repl-verbs-create-stage-message-renders-creating-worktree ()
  "The creating-worktree stage renders the owner's exact minibuffer line."
  (agent-repl-test-verbs--with nil
    (agent-repl-verbs--create-stage-message :creating-worktree)
    (should (agent-repl-test-verbs--messaged-p "agent-repl: creating the workspace's git worktree…"))))

(ert-deftest agent-repl-verbs-create-failure-internal-messages-the-error ()
  "An internal failure event surfaces the daemon's sentence loudly."
  (agent-repl-test-verbs--with nil
    (agent-repl-verbs--create-failure :internal "materialize worktree: boom")
    (should (agent-repl-test-verbs--messaged-p
             "agent-repl: workspace creation failed: materialize worktree: boom"))))

(ert-deftest agent-repl-verbs-missing-ref-refuses-before-sending ()
  "A workspace with no daemon identity cannot be addressed at all."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      (should-error (agent-repl-verb-close "ws-one") :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

;;;; ---- Interactive command behavior ----

(ert-deftest agent-repl-verbs-nuke-command-confirms-before-sending ()
  "The one data-destroying verb asks first."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t)))
      (agent-repl-nuke-workspace)
      (should (agent-repl-test-verbs--request :nuke)))))

(ert-deftest agent-repl-verbs-nuke-command-declined-sends-nothing ()
  "Declining the nuke confirmation sends no request."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) nil)))
      (agent-repl-nuke-workspace)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-kill-command-confirms-before-sending ()
  "`SPC j x' asks y/n first, and a yes sends the kill."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (agent-repl-kill-workspace)
      (should (agent-repl-test-verbs--request :kill)))))

(ert-deftest agent-repl-verbs-kill-command-declined-sends-nothing ()
  "Declining the kill confirmation sends no request."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
      (agent-repl-kill-workspace)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-kill-command-declined-is-logged ()
  "A declined kill is recorded, so the non-action is visible in the log."
  (agent-repl-test-verbs--with nil
    (let (logged)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        (agent-repl-kill-workspace)
        (should (cl-some (lambda (m) (string-prefix-p "elisp.verbs.kill-declined" m)) logged))))))

(ert-deftest agent-repl-verbs-restart-command-takes-no-prefix-argument ()
  "`agent-repl-restart-workspace' is not prefix-sensitive: its interactive spec is bare."
  (should (equal (cadr (interactive-form 'agent-repl-restart-workspace)) nil)))

(ert-deftest agent-repl-verbs-restart-command-sends-the-one-immediate-restart ()
  "The command sends a restart request with no force key."
  (agent-repl-test-verbs--with nil
    (agent-repl-restart-workspace)
    (should-not (plist-member (agent-repl-test-verbs--request :restart) :force))))

(ert-deftest agent-repl-verbs-open-command-offers-only-closed-rows ()
  "The open picker's candidates are exactly the roster's CLOSED rows."
  (let* ((open-row (agent-repl-test-verbs--row :name (list :text "open-row")))
         (closed-row (agent-repl-test-verbs--row
                      :workspace (list :workspace (agent-repl-test-verbs--ref "cid" "/tmp/c"))
                      :name (list :text "closed-row")
                      :closed (list :closed t)))
         (agent-repl-roster-view (agent-repl-test-verbs--roster (list open-row closed-row)))
         (offered nil))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p candidates &rest _) (setq offered candidates) "closed-row")))
        (agent-repl-open-workspace)
        (should (equal offered '("closed-row")))
        (should (equal (plist-get (agent-repl-test-verbs--request :open) :workspace)
                       (agent-repl-test-verbs--ref "cid" "/tmp/c")))))))

(ert-deftest agent-repl-verbs-open-command-refuses-with-no-closed-rows ()
  "With nothing closed there is nothing to open, and that is a refusal."
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with nil
      (should-error (agent-repl-open-workspace) :type 'user-error))))

(ert-deftest agent-repl-verbs-closed-rows-walks-children ()
  "A closed CHILD row is reachable: the walk is depth-first over children."
  (let* ((child (agent-repl-test-verbs--row
                 :workspace (list :workspace (agent-repl-test-verbs--ref "kid" "/tmp/kid"))
                 :name (list :text "kid") :closed (list :closed t)))
         (parent (agent-repl-test-verbs--row :children (list child)))
         (roster (agent-repl-test-verbs--roster (list parent))))
    (should (equal (mapcar #'agent-repl-roster-row-name
                           (agent-repl-verbs--closed-rows roster))
                   '("kid")))))

(ert-deftest agent-repl-verbs-closed-rows-includes-recently-merged ()
  "The recently-merged section is walked after the repo sections."
  (let* ((merged (agent-repl-test-verbs--row
                  :workspace (list :workspace (agent-repl-test-verbs--ref "m" "/tmp/m"))
                  :name (list :text "merged-one") :closed (list :closed t)))
         (roster (agent-repl-test-verbs--roster nil (list merged))))
    (should (equal (mapcar #'agent-repl-roster-row-name
                           (agent-repl-verbs--closed-rows roster))
                   '("merged-one")))))

;;;; ---- The row walk ----

(ert-deftest agent-repl-verbs-all-rows-is-the-roster-walk ()
  "The verbs walk the rows `agent-repl-roster-walk' walks, in its order.
ONE WALK serves the tab bar and the verbs, so a hand-rolled verb walk that
skipped a section the bar walks fails here rather than passing silently."
  (let* ((kid (agent-repl-test-verbs--row
               :workspace (list :workspace (agent-repl-test-verbs--ref "kid" "/tmp/kid"))
               :name (list :text "kid")))
         (parent (agent-repl-test-verbs--row :children (list kid)))
         (merged (agent-repl-test-verbs--row
                  :workspace (list :workspace (agent-repl-test-verbs--ref "m" "/tmp/m"))
                  :name (list :text "merged-one")))
         (roster (agent-repl-test-verbs--roster (list parent) (list merged))))
    (should (equal (agent-repl-verbs--all-rows roster)
                   (mapcar (lambda (entry) (plist-get entry :row))
                           (agent-repl-roster-walk roster))))
    (should (equal (mapcar #'agent-repl-roster-row-name
                           (agent-repl-verbs--all-rows roster))
                   '("row-one" "kid" "merged-one")))))

(ert-deftest agent-repl-verbs-all-rows-defaults-to-the-live-roster ()
  "With no ROSTER the walk reads `agent-repl-roster-view'."
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (should (equal (mapcar #'agent-repl-roster-row-name (agent-repl-verbs--all-rows))
                   '("row-one")))))

(ert-deftest agent-repl-verbs-all-rows-of-no-roster-is-empty ()
  "Before the first push there is no roster, and so no rows."
  (let ((agent-repl-roster-view nil))
    (should (null (agent-repl-verbs--all-rows)))))

;;;; ---- Create: the standard form ----

(ert-deftest agent-repl-verbs-create-standard-sends-the-repository-and-form ()
  "A standard create names its repository and sets the standard form arm."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :name "n")
    (let ((request (agent-repl-test-verbs--request :create)))
      (should (equal (plist-get request :repository) (agent-repl-test-verbs--repo-ref)))
      (should (eq (plist-get (plist-get request :form) :arm) :standard)))))

(ert-deftest agent-repl-verbs-create-omits-absent-facts ()
  "Absent creation facts are OMITTED: absence is what the proto reads."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (let ((request (agent-repl-test-verbs--request :create)))
      (should-not (plist-get request :parent))
      (should-not (plist-get request :model))
      (should-not (plist-get request :priority))
      (should-not (plist-get request :allow-ungated)))))

(ert-deftest agent-repl-verbs-create-command-sends-no-name-and-no-base-ref ()
  "A DYNAMIC create asks for neither a name nor a base ref, and sends neither.
The daemon mints the name, and an absent base ref IS the repo's main
branch -- so a `read-string' here would be a question the ruling removed."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "do a thing"))
              ((symbol-function 'read-string)
               (lambda (&rest _) (error "a dynamic create asks nothing but its prompt"))))
      (agent-repl-create-workspace)
      (let ((standard (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should-not (plist-get standard :name))
        (should-not (plist-get standard :base-ref))))))

(ert-deftest agent-repl-verbs-fork-into-primary-sends-a-fork-source-and-no-parent ()
  "`SPC TAB f\=' forks the conversation into a TOP-LEVEL workspace: the
current workspace rides as the fork source, and no parent is sent."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "fork it")))
      (agent-repl-fork-workspace-into-primary)
      (let ((request (agent-repl-test-verbs--request :create)))
        (should (equal (plist-get request :fork-source) (agent-repl-test-verbs--ref)))
        (should-not (plist-get request :parent))))))

(ert-deftest agent-repl-verbs-create-command-sends-no-parent ()
  "`SPC TAB n\=' is never a child: it sends no parent at all."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-create-workspace)
      (should-not (plist-get (agent-repl-test-verbs--request :create) :parent)))))

(ert-deftest agent-repl-verbs-fork-command-sets-fork-inside-the-parent ()
  "A fork lives INSIDE the parent: a fork without a parent is unrepresentable."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "fork it")))
      (agent-repl-fork-workspace)
      (should (eq (plist-get (plist-get (agent-repl-test-verbs--request :create) :parent)
                             :fork)
                  t)))))

;;;; ---- Create: the four modes and the questions each one asks ----

(ert-deftest agent-repl-verbs-create-command-asks-no-repository ()
  "A DYNAMIC create never asks for a repository (owner ruling, 2026-09-12)."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "do a thing"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a dynamic create must not ask for a repository"))))
        ;; Act.
        (agent-repl-create-workspace)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-create-command-uses-the-current-workspaces-section ()
  "The derived repository is the section the CURRENT workspace's row sits in."
  ;; Arrange: two sections, the current workspace's row in the second.
  (let* ((other (list :key (list :repository (agent-repl-test-verbs--repo-ref
                                              "other-id" "/tmp/other"))
                      :header (list :label (list :text "other-repo"))
                      :rows (list :rows nil)))
         (mine (list :key (list :repository (agent-repl-test-verbs--repo-ref))
                     :header (list :label (list :text "repo-one"))
                     :rows (list :rows (list (agent-repl-test-verbs--row)))))
         (agent-repl-roster-view
          (agent-repl-test-verbs--roster nil nil (list other mine))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "do a thing")))
        ;; Act.
        (agent-repl-create-workspace)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-create-command-without-a-current-workspace-refuses ()
  "With no current workspace a dynamic create has nothing to derive from."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
        ;; Act / Assert.
        (should-error (agent-repl-create-workspace) :type 'user-error)
        (should-not agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-fork-command-asks-no-repository ()
  "A fork is a DYNAMIC mode too: prompt only, repository derived."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "fork it"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a fork must not ask for a repository"))))
        ;; Act.
        (agent-repl-fork-workspace)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-oneshot-asks-no-repository ()
  "The one-shot derives its repository instead of asking."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "commission"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a one-shot must not ask for a repository"))))
        ;; Act.
        (agent-repl-create-oneshot nil)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-static-create-sends-its-name ()
  "The STATIC create asks a repository and a REQUIRED name, and sends both."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-workspace-static)
      ;; Assert.
      (let ((request (agent-repl-test-verbs--request :create)))
        (should (equal (plist-get request :repository) (agent-repl-test-verbs--repo-ref)))
        (should (equal (plist-get (plist-get (plist-get request :form) :value) :name)
                       "named-one"))))))

(ert-deftest agent-repl-verbs-static-create-sends-no-initial-prompt ()
  "The STATIC create sends NO prompt: the workspace comes up idle."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt)
               (lambda (_p &optional _history-file _unseeded) (error "a static create asks for no prompt")))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-workspace-static)
      ;; Assert.
      (should-not (plist-get (plist-get (plist-get (agent-repl-test-verbs--request :create)
                                                   :form)
                                        :value)
                             :initial-prompt)))))

(ert-deftest agent-repl-verbs-static-create-refuses-a-blank-name ()
  "The static create's name is REQUIRED: a blank one is refused before send."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'read-string) (lambda (&rest _) "   ")))
      ;; Act / Assert.
      (should-error (agent-repl-create-workspace-static) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-named-fork-into-primary-sends-the-name-and-no-prompt ()
  "`SPC TAB F\=' names the fork into the primary branch and asks no prompt."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt)
               (lambda (_p &optional _history-file _unseeded) (error "a named fork asks for no prompt")))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-fork-workspace-into-primary-static)
      ;; Assert.
      (let ((request (agent-repl-test-verbs--request :create)))
        (should (equal (plist-get request :fork-source) (agent-repl-test-verbs--ref)))
        (should-not (plist-get request :parent))
        (should (equal (plist-get (plist-get (plist-get request :form) :value) :name)
                       "named-one"))
        (should-not (plist-get (plist-get (plist-get request :form) :value) :initial-prompt))))))

(ert-deftest agent-repl-verbs-static-create-sends-no-parent ()
  "`SPC TAB N\=' is never a child: it sends no parent at all."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-workspace-static)
      ;; Assert.
      (should-not (plist-get (agent-repl-test-verbs--request :create) :parent)))))

;;;; ---- Create: the named fork ----

(ert-deftest agent-repl-verbs-named-fork-sends-the-name-inside-a-forking-parent ()
  "`SPC TAB F\=' sends its name, the current workspace as parent, and the fork."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'read-string) (lambda (&rest _) "named-fork")))
      ;; Act.
      (agent-repl-fork-workspace-static)
      ;; Assert.
      (let* ((request (agent-repl-test-verbs--request :create))
             (parent (plist-get request :parent)))
        (should (equal (plist-get (plist-get (plist-get request :form) :value) :name)
                       "named-fork"))
        (should (equal (plist-get parent :workspace) (agent-repl-test-verbs--ref)))
        (should (eq (plist-get parent :fork) t))))))

(ert-deftest agent-repl-verbs-named-fork-sends-no-initial-prompt ()
  "`SPC TAB F\=' asks no prompt and sends none: the fork comes up idle."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt)
               (lambda (_p &optional _history-file _unseeded) (error "a named fork asks for no prompt")))
              ((symbol-function 'read-string) (lambda (&rest _) "named-fork")))
      ;; Act.
      (agent-repl-fork-workspace-static)
      ;; Assert.
      (should-not (plist-get (plist-get (plist-get (agent-repl-test-verbs--request :create)
                                                   :form)
                                        :value)
                             :initial-prompt)))))

(ert-deftest agent-repl-verbs-named-fork-refuses-a-blank-name ()
  "The named fork\='s name is REQUIRED: a blank one is refused before send."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'read-string) (lambda (&rest _) "   ")))
      ;; Act / Assert.
      (should-error (agent-repl-fork-workspace-static) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-named-fork-asks-no-repository ()
  "The named fork targets the CURRENT workspace\='s repository, never a picked one."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "named-fork"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a named fork must not ask for a repository"))))
        ;; Act.
        (agent-repl-fork-workspace-static)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

;;;; ---- Create: invocation and abandoned-read logging ----

(defconst agent-repl-test-verbs--creation-commands
  '(agent-repl-create-workspace
    agent-repl-create-workspace-static
    agent-repl-fork-workspace
    agent-repl-fork-workspace-static
    agent-repl-fork-workspace-into-primary
    agent-repl-fork-workspace-into-primary-static
    agent-repl-create-oneshot
    agent-repl-create-oneshot-for-current-workspace)
  "Every interactive creation command.")

(defun agent-repl-test-verbs--quits-p (command)
  "Return non-nil when calling COMMAND signals `quit'.
`should-error' cannot observe a quit: it catches `error' conditions only,
and a quit escaping a test is reported as QUIT rather than as a failure."
  (condition-case nil
      (progn (funcall command) nil)
    (quit t)))

(ert-deftest agent-repl-verbs-creation-commands-log-their-invocation-before-reading ()
  "Each creation command records its invocation BEFORE its first question.
A fork that left no trace was indistinguishable from one never run."
  (dolist (command agent-repl-test-verbs--creation-commands)
    (ert-info ((symbol-name command))
      ;; Arrange: the first question records what was logged, then quits.
      (let (logs logged-at-first-read)
        (agent-repl-test-verbs--with nil
          (cl-letf (((symbol-function 'agent-repl--info)
                     (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                    ((symbol-function 'agent-repl-verbs--dynamic-repository)
                     (lambda (_c)
                       (setq logged-at-first-read (copy-sequence logs))
                       (signal 'quit nil)))
                    ((symbol-function 'agent-repl-verbs--read-repository)
                     (lambda ()
                       (setq logged-at-first-read (copy-sequence logs))
                       (signal 'quit nil))))
            ;; Act.
            (should (agent-repl-test-verbs--quits-p command))))
        ;; Assert.
        (should (member (format "elisp.verbs.create-invoked command=%s" command)
                        logged-at-first-read))))))

(ert-deftest agent-repl-verbs-creation-commands-log-a-quit-read-and-resignal-it ()
  "A quit out of a creation question is recorded, then propagates as a quit."
  ;; Arrange: each command, and the question it is quit out of.
  (dolist (case '((agent-repl-create-workspace prompt)
                  (agent-repl-create-workspace-static name)
                  (agent-repl-fork-workspace prompt)
                  (agent-repl-fork-workspace-static name)
                  (agent-repl-fork-workspace-into-primary prompt)
                  (agent-repl-fork-workspace-into-primary-static name)
                  (agent-repl-create-oneshot prompt)
                  (agent-repl-create-oneshot-for-current-workspace prompt)))
    (pcase-let ((`(,command ,read) case))
      (ert-info ((symbol-name command))
        (let (logs)
          (agent-repl-test-verbs--with nil
            (cl-letf (((symbol-function 'agent-repl--info)
                       (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                      ((symbol-function 'agent-repl-verbs--section-of-ws)
                       (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                      ((symbol-function 'agent-repl-verbs--read-repository)
                       (lambda () (agent-repl-test-verbs--repo-ref)))
                      ((symbol-function 'agent-repl-verbs--read-prompt)
                       (lambda (_p &optional _history-file _unseeded) (signal 'quit nil)))
                      ((symbol-function 'read-string)
                       (lambda (&rest _) (signal 'quit nil))))
              ;; Act / Assert: the quit still reaches the caller.
              (should (agent-repl-test-verbs--quits-p command))
              (should-not agent-repl-test-verbs--sent)))
          ;; Assert: and it was recorded, naming the question.
          (should (seq-some
                   (lambda (text)
                     (string-search
                      (format "elisp.verbs.create-read-abandoned command=%s read=%s signal=quit"
                              command read)
                      text))
                   logs)))))))

(ert-deftest agent-repl-verbs-named-fork-logs-a-refused-blank-name ()
  "A blank name's `user-error\=' is recorded against the name question."
  ;; Arrange.
  (let (logs)
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'read-string) (lambda (&rest _) "")))
        ;; Act.
        (should-error (agent-repl-fork-workspace-static) :type 'user-error)))
    ;; Assert.
    (should (seq-some
             (lambda (text)
               (string-search
                "elisp.verbs.create-read-abandoned command=agent-repl-fork-workspace-static read=name signal=user-error"
                text))
             logs))))

;;;; ---- Create: standing on what was just created ----

(ert-deftest agent-repl-verbs-a-second-mint-supersedes-a-waiting-landing ()
  "The user stands in ONE place, so a newer mint replaces one still waiting."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "first" "/tmp/a") 0)
      ;; Act.
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "second" "/tmp/b") 0)
      ;; Assert.
      (should (equal (plist-get agent-repl-verbs--pending-landing :id) "second")))))

(ert-deftest agent-repl-verbs-select-minted-without-a-dir-is-reported ()
  "A minted ref carrying no dir is REPORTED, never silently skipped.
The decoder already refuses an answer without the ref, so a missing dir
is a contract breach and the user is owed the reason nothing came up."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    ;; Act.
    (agent-repl-verbs-select-minted (list :id "no-dir" :dir "") 0)
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)
    (should (agent-repl-test-verbs--messaged-p "no directory to switch to"))))

;;;; ---- Create: the one-shot form ----

(ert-deftest agent-repl-verbs-oneshot-sends-no-finish ()
  "A one-shot carries its prompt and NOTHING else: there is no finish choice,
because what happens on completion is the repository's own directive."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "commission")))
      (agent-repl-create-oneshot nil)
      (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should (plist-get one-shot :prompt))
        (should-not (plist-member one-shot :finish))))))

(ert-deftest agent-repl-verbs-oneshot-carries-its-prompt ()
  "A one-shot IS its prompt, so the prompt travels as a UserSaid text block."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "commission")))
      (agent-repl-create-oneshot nil)
      (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should (equal (plist-get one-shot :prompt)
                       (list :content
                             (list :blocks
                                   (list (list :arm :text
                                               :value (list :text "commission")))))))))))

(ert-deftest agent-repl-verbs-oneshot-refuses-a-blank-commission ()
  "A one-shot with no prompt is refused before a request is built."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "   ")))
      (should-error (agent-repl-create-oneshot nil) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-oneshot-prefix-arg-picks-a-model ()
  "A prefix argument threads the chosen model onto the shared facts."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "commission"))
              ((symbol-function 'agent-repl-verbs--read-model) (lambda () "haiku")))
      (agent-repl-create-oneshot t)
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :model) "haiku")))))

;;;; ---- Create: the one-shot about the current workspace ----

(defmacro agent-repl-test-verbs--with-oneshot-reads (prompts &rest body)
  "Run BODY with the one-shot\='s repository and commission reads stubbed.
Every prompt label the commission read is offered is pushed onto PROMPTS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
              (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
             ((symbol-function 'agent-repl-verbs--read-prompt)
              (lambda (label &optional _history-file _unseeded) (push label ,prompts) "fix its feed")))
     ,@body))

(ert-deftest agent-repl-verbs-oneshot-for-current-workspace-names-it-as-the-context ()
  "`SPC TAB C-o\=' sends the current workspace\='s ref as the one-shot\='s context."
  (let (prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        ;; Act.
        (agent-repl-create-oneshot-for-current-workspace nil)
        ;; Assert.
        (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form) :value)))
          (should (equal (plist-get one-shot :context) (agent-repl-test-verbs--ref)))
          (should (plist-get one-shot :prompt)))))))

(ert-deftest agent-repl-verbs-oneshot-for-current-workspace-stays-top-level ()
  "The context one-shot is no child and no fork: neither a parent nor a fork source rides."
  (let (prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        ;; Act.
        (agent-repl-create-oneshot-for-current-workspace nil)
        ;; Assert.
        (let ((request (agent-repl-test-verbs--request :create)))
          (should-not (plist-get request :parent))
          (should-not (plist-get request :fork-source)))))))

(ert-deftest agent-repl-verbs-oneshot-for-current-workspace-prompt-names-the-workspace ()
  "The commission prompt names the workspace the commission is about."
  (let (prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        ;; Act.
        (agent-repl-create-oneshot-for-current-workspace nil)
        ;; Assert.
        (should (equal prompts '("One-shot fix for ws-one: ")))))))

(ert-deftest agent-repl-verbs-oneshot-sends-no-context ()
  "`SPC TAB o\=' names no context workspace: its commission stands on its own words."
  (let (prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        ;; Act.
        (agent-repl-create-oneshot nil)
        ;; Assert.
        (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form) :value)))
          (should-not (plist-get one-shot :context))
          (should (equal prompts '("One-shot commission: "))))))))

(ert-deftest agent-repl-verbs-oneshot-for-current-workspace-without-a-ref-sends-nothing ()
  "A current workspace with no daemon identity refuses the create before any send."
  (let (prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl-verbs--roster-ref) (lambda (_ws) nil)))
          ;; Act.
          (let ((err (should-error (agent-repl-create-oneshot-for-current-workspace nil)
                                   :type 'user-error)))
            ;; Assert.
            (should (string-match-p "has no daemon identity" (cadr err)))
            (should-not agent-repl-test-verbs--sent)))))))

(ert-deftest agent-repl-verbs-oneshot-for-current-workspace-prefix-arg-picks-a-model ()
  "A prefix argument threads the chosen model onto the context one-shot too."
  (let (prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        (cl-letf (((symbol-function 'agent-repl-verbs--read-model) (lambda () "haiku")))
          ;; Act.
          (agent-repl-create-oneshot-for-current-workspace t)
          ;; Assert.
          (should (equal (plist-get (agent-repl-test-verbs--request :create) :model) "haiku")))))))

(ert-deftest agent-repl-verbs-oneshot-commands-share-the-one-shot-composition ()
  "Both one-shot commands compose through `agent-repl-verbs--create-oneshot\='."
  (let (calls prompts)
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-oneshot-reads prompts
        (cl-letf (((symbol-function 'agent-repl-verbs--create-oneshot)
                   (lambda (command pick-model about-current)
                     (push (list command pick-model about-current) calls))))
          ;; Act.
          (agent-repl-create-oneshot nil)
          (agent-repl-create-oneshot-for-current-workspace t)
          ;; Assert.
          (should (equal (reverse calls)
                         '((agent-repl-create-oneshot nil nil)
                           (agent-repl-create-oneshot-for-current-workspace t t)))))))))

;;;; ---- Create: the shared one-shot composition ----

(ert-deftest agent-repl-verbs-create-oneshot-helper-logs-its-command-and-sends-a-one-shot ()
  "The shared one-shot composition logs the COMMAND it was handed and sends a one-shot."
  ;; Arrange.
  (let (logs)
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "commission")))
        ;; Act.
        (agent-repl-verbs--create-oneshot 'some-oneshot-command nil nil)
        ;; Assert.
        (should (member "elisp.verbs.create-invoked command=some-oneshot-command" logs))
        (should (eq (plist-get (plist-get (agent-repl-test-verbs--request :create) :form) :arm)
                    :one-shot))))))

;;;; ---- Create: the required-prompt reader ----

(ert-deftest agent-repl-verbs-read-required-prompt-returns-the-text-as-read ()
  "A non-blank answer comes back untouched, surrounding whitespace included."
  ;; Arrange.
  (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) " ask \n")))
    ;; Act / Assert.
    (should (equal (agent-repl-verbs--read-required-prompt "Q: " "refused") " ask \n"))))

(ert-deftest agent-repl-verbs-read-required-prompt-refuses-a-blank-answer ()
  "A whitespace-only answer is refused with the caller's refusal."
  ;; Arrange.
  (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) " \t\n")))
    ;; Act.
    (let ((err (should-error (agent-repl-verbs--read-required-prompt "Q: " "it IS its prompt")
                             :type 'user-error)))
      ;; Assert.
      (should (equal (cadr err) "agent-repl: it IS its prompt")))))

(ert-deftest agent-repl-verbs-prompt-only-creates-share-the-required-prompt-reader ()
  "The one-shot and help creates both read their prompt through the shared reader."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)) nil
                                        (agent-repl-test-verbs--help-sections)))
        (prompts nil))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl-verbs--read-required-prompt)
                   (lambda (prompt-text _refusal &optional _history-file _unseeded) (push prompt-text prompts) "q")))
          ;; Act.
          (agent-repl-create-oneshot nil)
          (agent-repl-create-help-workspace)
          ;; Assert.
          (should (equal (reverse prompts)
                         '("One-shot commission: " "agent-repl question: "))))))))

;;;; ---- Create: which prompts the composer seeds ----

(defun agent-repl-test-verbs--read-prompt-seeded-by (unseeded)
  "Read a prompt with UNSEEDED from a workspace whose composer holds a draft.
Return a plist of the `:initial' text the read was offered and whether the
composer was `:read'."
  (let (offered composer-read)
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--read-input-buffer)
               (lambda (_ws) (setq composer-read t) "  the draft  "))
              ((symbol-function 'read-string)
               (lambda (_prompt initial &rest _) (setq offered initial) "typed")))
      (agent-repl-verbs--read-prompt "Commission: " nil unseeded))
    (list :initial offered :read composer-read)))

(ert-deftest agent-repl-verbs-read-prompt-seeds-from-the-composer-by-default ()
  "A creation prompt offers the composer's trimmed draft as its initial text."
  ;; Act.
  (let ((read (agent-repl-test-verbs--read-prompt-seeded-by nil)))
    ;; Assert.
    (should (equal (plist-get read :initial) "the draft"))))

(ert-deftest agent-repl-verbs-read-prompt-unseeded-offers-no-initial-text ()
  "An UNSEEDED creation prompt starts empty though the composer holds a draft."
  ;; Act.
  (let ((read (agent-repl-test-verbs--read-prompt-seeded-by t)))
    ;; Assert.
    (should-not (plist-get read :initial))))

(ert-deftest agent-repl-verbs-read-prompt-unseeded-never-reads-the-composer ()
  "An UNSEEDED creation prompt leaves the composer unread."
  ;; Act.
  (let ((read (agent-repl-test-verbs--read-prompt-seeded-by t)))
    ;; Assert.
    (should-not (plist-get read :read))))

(ert-deftest agent-repl-verbs-read-prompt-unseeded-logs-no-missing-composer ()
  "An UNSEEDED read outside a workspace records no missing composer: it never looked."
  ;; Arrange.
  (let (logged)
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest _) (setq logged t)))
              ((symbol-function 'read-string)
               (lambda (_prompt initial &rest _) initial)))
      ;; Act.
      (agent-repl-verbs--read-prompt "Commission: " nil t))
    ;; Assert.
    (should-not logged)))

(ert-deftest agent-repl-verbs-read-required-prompt-passes-unseeded-through ()
  "The required-prompt reader hands UNSEEDED to the prompt reader."
  ;; Arrange.
  (let (passed)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt)
               (lambda (_p &optional _history-file unseeded) (setq passed unseeded) "ask")))
      ;; Act.
      (agent-repl-verbs--read-required-prompt "Q: " "refused" nil t))
    ;; Assert.
    (should passed)))

(defun agent-repl-test-verbs--unseeded-by (command)
  "Run creation COMMAND and return the UNSEEDED flag its prompt was read with."
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)) nil
                                        (agent-repl-test-verbs--help-sections)))
        (unseeded 'never-read))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
                   (lambda () (agent-repl-test-verbs--repo-ref)))
                  ((symbol-function 'agent-repl-verbs--read-prompt)
                   (lambda (_p &optional _history-file flag) (setq unseeded flag) "do a thing")))
          (funcall command))))
    unseeded))

(ert-deftest agent-repl-verbs-oneshot-commission-is-unseeded ()
  "The one-shot commission never starts from the composer's draft."
  ;; Act / Assert.
  (should (eq (agent-repl-test-verbs--unseeded-by #'agent-repl-create-oneshot) 'unseeded)))

(ert-deftest agent-repl-verbs-other-prompted-creates-stay-seeded ()
  "Every prompted create other than the one-shot still starts from the composer."
  (dolist (command '(agent-repl-create-workspace
                     agent-repl-fork-workspace
                     agent-repl-fork-workspace-into-primary
                     agent-repl-create-help-workspace))
    (ert-info ((symbol-name command))
      ;; Act / Assert.
      (should-not (agent-repl-test-verbs--unseeded-by command)))))

;;;; ---- Create: the persisted prompt history ----

(defmacro agent-repl-test-verbs--with-history-file (var &rest body)
  "Run BODY with VAR naming a history file in a fresh temporary directory."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "agent-repl-test-history-" t))
          (,var (expand-file-name "history.el" dir)))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun agent-repl-test-verbs--fake-minibuffer (typed &optional during ending)
  "Return a `read-string' stub standing in for the minibuffer.
The stub inserts its INITIAL input, runs `minibuffer-setup-hook' as the
real minibuffer does, then inserts each string of TYPED as a separate
edit.  DURING, when given, is called with the HIST value after the edits,
while the read is still open.  ENDING `quit' signals a quit instead of
returning the text."
  (lambda (_prompt initial hist)
    (with-temp-buffer
      (when initial (insert initial))
      (run-hooks 'minibuffer-setup-hook)
      (dolist (text typed)
        (goto-char (point-max))
        (insert text))
      (when during (funcall during (symbol-value hist)))
      (if (eq ending 'quit)
          (signal 'quit nil)
        (buffer-string)))))

(ert-deftest agent-repl-verbs-persisted-history-entries-blank-text-adds-nothing ()
  "A blank text leaves the history as it was."
  ;; Act / Assert.
  (should (equal (agent-repl-verbs--persisted-history-entries " \n" '("a" "b")) '("a" "b"))))

(ert-deftest agent-repl-verbs-persisted-history-entries-moves-an-equal-entry-to-the-head ()
  "A text already in the history appears once, at its head."
  ;; Act / Assert.
  (should (equal (agent-repl-verbs--persisted-history-entries "b" '("a" "b" "c")) '("b" "a" "c"))))

(ert-deftest agent-repl-verbs-persisted-history-entries-cut-to-history-length ()
  "The history is cut to `history-length'."
  ;; Arrange.
  (let ((history-length 2))
    ;; Act / Assert.
    (should (equal (agent-repl-verbs--persisted-history-entries "new" '("a" "b")) '("new" "a")))))

(ert-deftest agent-repl-verbs-persisted-history-load-absent-file-is-empty ()
  "A history file that does not exist yet is an empty history."
  (agent-repl-test-verbs--with-history-file file
    ;; Act / Assert.
    (should-not (agent-repl-verbs--persisted-history-load file))))

(ert-deftest agent-repl-verbs-persisted-history-load-invalid-file-is-logged-and-signalled ()
  "A history file holding anything but a list of strings is a broken invariant."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (agent-repl--write-sexp-file file '("ok" 42))
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act.
        (should-error (agent-repl-verbs--persisted-history-load file)))
      ;; Assert.
      (should (= (length logged) 1))
      (should (string-match-p "persisted-history-invalid" (car logged)))
      (should (string-match-p (regexp-quote file) (car logged))))))

(ert-deftest agent-repl-verbs-read-persisted-saves-each-edit-before-the-read-ends ()
  "Text typed into the minibuffer is on disk while the read is still open.
That is what survives a crash: nothing waits for the read to end."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (agent-repl--write-sexp-file file '("older"))
    (let (on-disk)
      (cl-letf (((symbol-function 'read-string)
                 (agent-repl-test-verbs--fake-minibuffer
                  '("fix " "the bug")
                  (lambda (_hist) (setq on-disk (agent-repl--read-sexp-file file))))))
        ;; Act.
        (agent-repl-verbs--read-string-persisted "Q: " nil file))
      ;; Assert.
      (should (equal on-disk '("fix the bug" "older"))))))

(ert-deftest agent-repl-verbs-read-persisted-offers-the-file-history ()
  "The read's history, walked by `M-p' and the up arrow, is the file's."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (agent-repl--write-sexp-file file '("draft before the crash" "older"))
    (let (offered)
      (cl-letf (((symbol-function 'read-string)
                 (agent-repl-test-verbs--fake-minibuffer
                  '("x") (lambda (hist) (setq offered hist)))))
        ;; Act.
        (agent-repl-verbs--read-string-persisted "Q: " nil file))
      ;; Assert.
      (should (equal offered '("draft before the crash" "older"))))))

(ert-deftest agent-repl-verbs-read-persisted-submit-leaves-the-text-at-the-head-once ()
  "A submitted text heads the saved history exactly once."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (agent-repl--write-sexp-file file '("older"))
    (cl-letf (((symbol-function 'read-string)
               (agent-repl-test-verbs--fake-minibuffer '("do" " it"))))
      ;; Act.
      (let ((text (agent-repl-verbs--read-string-persisted "Q: " nil file)))
        ;; Assert.
        (should (equal text "do it"))
        (should (equal (agent-repl--read-sexp-file file) '("do it" "older")))))))

(ert-deftest agent-repl-verbs-read-persisted-quit-keeps-the-draft ()
  "A quit read keeps its typed text in the history and still quits."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (cl-letf (((symbol-function 'read-string)
               (agent-repl-test-verbs--fake-minibuffer '("half a thought") nil 'quit)))
      ;; Act.
      (should (eq (condition-case nil
                      (progn (agent-repl-verbs--read-string-persisted "Q: " nil file) 'returned)
                    (quit 'quit))
                  'quit)))
    ;; Assert.
    (should (equal (agent-repl--read-sexp-file file) '("half a thought")))))

(ert-deftest agent-repl-verbs-read-persisted-browsed-entry-keeps-the-draft ()
  "Showing an existing entry, as `M-p' does, does not displace the saved draft."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (agent-repl--write-sexp-file file '("older"))
    (let (on-disk)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt _initial _hist)
                   (with-temp-buffer
                     (run-hooks 'minibuffer-setup-hook)
                     (insert "my draft")
                     (erase-buffer)
                     (insert "older")
                     (setq on-disk (agent-repl--read-sexp-file file))
                     (signal 'quit nil)))))
        ;; Act.
        (should (eq (condition-case nil
                        (progn (agent-repl-verbs--read-string-persisted "Q: " nil file) 'returned)
                      (quit 'quit))
                    'quit)))
      ;; Assert.
      (should (equal on-disk '("my draft" "older")))
      (should (equal (agent-repl--read-sexp-file file) '("my draft" "older"))))))

(ert-deftest agent-repl-verbs-read-persisted-cleared-minibuffer-keeps-the-draft ()
  "Emptying the minibuffer does not erase the saved draft."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt _initial _hist)
                 (with-temp-buffer
                   (run-hooks 'minibuffer-setup-hook)
                   (insert "my draft")
                   (erase-buffer)
                   (signal 'quit nil)))))
      ;; Act.
      (condition-case nil
          (agent-repl-verbs--read-string-persisted "Q: " nil file)
        (quit nil)))
    ;; Assert.
    (should (equal (agent-repl--read-sexp-file file) '("my draft")))))

(ert-deftest agent-repl-verbs-read-persisted-failed-save-aborts-the-read ()
  "A save that fails mid-read aborts the read and is logged with its file."
  (agent-repl-test-verbs--with-history-file file
    ;; Arrange.
    (let (logged returned)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (signal 'file-error '("disk full"))))
                ((symbol-function 'agent-repl--error)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) logged)))
                ((symbol-function 'read-string)
                 (agent-repl-test-verbs--fake-minibuffer '("words"))))
        ;; Act.
        (should-error (setq returned (agent-repl-verbs--read-string-persisted "Q: " nil file))
                      :type 'file-error))
      ;; Assert.
      (should-not returned)
      (should-not (file-exists-p file))
      (should logged)
      (should (cl-every (lambda (line) (string-match-p (regexp-quote file) line)) logged)))))

(ert-deftest agent-repl-verbs-read-prompt-with-a-history-file-reads-persisted ()
  "A creation prompt given a history file keeps its history in that file."
  ;; Arrange.
  (let (read-with)
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
              ((symbol-function 'agent-repl--log) #'ignore)
              ((symbol-function 'agent-repl-verbs--read-string-persisted)
               (lambda (_prompt _initial file) (setq read-with file) "text")))
      ;; Act.
      (agent-repl-verbs--read-prompt "Commission: " "/tmp/h.el"))
    ;; Assert.
    (should (equal read-with "/tmp/h.el"))))

(ert-deftest agent-repl-verbs-oneshot-reads-its-commission-with-the-oneshot-history ()
  "The one-shot commission prompt keeps its history in the one-shot file."
  (agent-repl-test-verbs--with nil
    ;; Arrange.
    (let (history-files)
      (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'agent-repl-verbs--read-prompt)
                 (lambda (_p &optional history-file _unseeded) (push history-file history-files) "commission")))
        ;; Act.
        (agent-repl-create-oneshot nil))
      ;; Assert.
      (should (equal history-files (list (agent-repl-verbs--oneshot-history-file)))))))

(ert-deftest agent-repl-verbs-oneshot-history-file-lives-in-the-global-state-dir ()
  "The one-shot history is process-wide, under the global state directory."
  ;; Act / Assert.
  (should (equal (file-name-directory (agent-repl-verbs--oneshot-history-file))
                 (agent-repl--global-state-dir))))

;;;; ---- Create: the help workspace (SPC o h) ----

(defmacro agent-repl-test-verbs--with-help-repo (common-dir &rest body)
  "Run BODY with agent-repl's module root resolving to COMMON-DIR.
COMMON-DIR is the git common-dir `agent-repl--repo-key-for-dir' answers
for the module root (nil: not a git repository).  Paths canonicalize by
trailing-slash removal alone, so the fixture directories need not exist."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--repo-key-for-dir) (lambda (_dir) ,common-dir))
             ((symbol-function 'agent-repl--path-canonical) #'directory-file-name))
     ,@body))

(defun agent-repl-test-verbs--help-sections ()
  "Return two sections: agent-repl's repository, and the current workspace's other one."
  (list (list :key (list :repository (agent-repl-test-verbs--repo-ref
                                       "agent-repl-repo" "/tmp/agent-repl-test/doom"))
              :header (list :label (list :text "doom"))
              :rows (list :rows nil))
        (list :key (list :repository (agent-repl-test-verbs--repo-ref))
              :header (list :label (list :text "repo-one"))
              :rows (list :rows (list (agent-repl-test-verbs--row))))))

;;;; ---- Create: no creation moves the user ----

(defmacro agent-repl-test-verbs--with-create-commands (&rest body)
  "Run BODY with the creation commands' questions answered.
The repository is the current workspace's section, the prompt and the
name are fixed, and agent-repl's own repository is the help target."
  (declare (indent 0))
  `(let ((agent-repl-roster-view
          (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)) nil
                                         (agent-repl-test-verbs--help-sections))))
     (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
       (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
                  (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                 ((symbol-function 'agent-repl-verbs--read-repository)
                  (lambda (&rest _) (agent-repl-test-verbs--repo-ref)))
                 ((symbol-function 'agent-repl-verbs--read-prompt)
                  (lambda (_p &optional _history-file _unseeded) "do a thing"))
                 ((symbol-function 'agent-repl-verbs--read-required-prompt)
                  (lambda (_p _refusal &optional _history-file _unseeded) "a question"))
                 ((symbol-function 'read-string) (lambda (&rest _) "named")))
         ,@body))))

(defconst agent-repl-test-verbs--standard-creation-commands
  '(agent-repl-create-workspace
    agent-repl-create-workspace-static
    agent-repl-fork-workspace
    agent-repl-fork-workspace-static
    agent-repl-fork-workspace-into-primary
    agent-repl-fork-workspace-into-primary-static
    agent-repl-create-help-workspace)
  "Every command that creates a standard workspace from a keybinding.")

(defun agent-repl-test-verbs--create-and-arrive (command)
  "Run creation COMMAND, then deliver the roster arrival of its tab.
Answers the moves the user saw: every workspace switched to and every
project landed on."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (agent-repl-test-verbs--with-create-commands
      (funcall command)
      (agent-repl-test-verbs--tab-arrives "new-id" "new-ws")
      (list :landed agent-repl-test-verbs--selected
            :switched agent-repl-test-verbs--tab-switched
            :requested agent-repl-test-verbs--switch-requests
            :pending agent-repl-verbs--pending-landing))))

(ert-deftest agent-repl-verbs-dynamic-create-stands-still ()
  "`SPC TAB n' leaves the user where they are once its tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive 'agent-repl-create-workspace)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-static-create-stands-still ()
  "`SPC TAB N' leaves the user where they are once its tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive 'agent-repl-create-workspace-static)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-child-fork-stands-still ()
  "`SPC TAB c' leaves the user on the parent once the fork's tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive 'agent-repl-fork-workspace)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-named-child-fork-stands-still ()
  "`SPC TAB C' leaves the user on the parent once the fork's tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive 'agent-repl-fork-workspace-static)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-primary-fork-stands-still ()
  "`SPC TAB f' leaves the user where they are once the fork's tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive 'agent-repl-fork-workspace-into-primary)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-named-primary-fork-stands-still ()
  "`SPC TAB F' leaves the user where they are once the fork's tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive
                'agent-repl-fork-workspace-into-primary-static)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-help-create-stands-still ()
  "`SPC o h' leaves the user where they are once the help tab arrives."
  ;; Arrange / Act.
  (let ((moves (agent-repl-test-verbs--create-and-arrive 'agent-repl-create-help-workspace)))
    ;; Assert.
    (should (equal moves '(:landed nil :switched nil :requested nil :pending nil)))))

(ert-deftest agent-repl-verbs-no-creation-command-asks-to-select ()
  "Every creation command creates through `agent-repl-verb-create' with no
selection argument at all, so a new creation path cannot select by habit."
  (dolist (command agent-repl-test-verbs--standard-creation-commands)
    ;; Arrange.
    (let (args)
      (cl-letf (((symbol-function 'agent-repl-verb-create)
                 (lambda (&rest a) (setq args a))))
        (agent-repl-test-verbs--with nil
          (agent-repl-test-verbs--with-create-commands
            ;; Act.
            (funcall command))))
      ;; Assert.
      (should args)
      (should-not (plist-member (cddr args) :select)))))

(ert-deftest agent-repl-verbs-create-refuses-a-select-argument ()
  "`agent-repl-verb-create' has no SELECT key: asking it to select is an error."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    ;; Act / Assert.
    (should-error (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :select t))))

(ert-deftest agent-repl-verbs-help-prompt-meta-wraps-the-preamble ()
  "The preamble is an injected span ahead of the question, which stays bare."
  ;; Arrange.
  (let ((agent-repl-help-preamble "PREAMBLE"))
    ;; Act / Assert.
    (should (equal (agent-repl-verbs--help-prompt "how do tabs work?")
                   (concat (agent-repl--meta-wrap "PREAMBLE") "\n\nhow do tabs work?")))))

(ert-deftest agent-repl-verbs-help-preamble-names-the-skill-and-forbids-changes ()
  "The shipped preamble names the skill and withholds implementation."
  ;; Assert.
  (should (string-match-p "/agent-repl-help" agent-repl-help-preamble))
  (should (string-match-p "explicit approval" agent-repl-help-preamble)))

(ert-deftest agent-repl-verbs-help-preamble-names-a-skill-the-module-ships ()
  "The skill the preamble names is one this module ships, under its own name."
  ;; Arrange.
  (let ((skill-md (expand-file-name "skills/agent-repl-help/SKILL.md"
                                    (file-name-directory agent-repl-metaprompt-file))))
    ;; Act / Assert.
    (should (file-exists-p skill-md))
    (with-temp-buffer
      (insert-file-contents skill-md)
      (should (re-search-forward "^name: agent-repl-help$" nil t)))))

(ert-deftest agent-repl-verbs-help-creates-in-agent-repls-repository ()
  "The help workspace goes to agent-repl's repository, not the current workspace's."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil (agent-repl-test-verbs--help-sections))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "q")))
          ;; Act.
          (agent-repl-create-help-workspace)
          ;; Assert.
          (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                         (agent-repl-test-verbs--repo-ref
                          "agent-repl-repo" "/tmp/agent-repl-test/doom"))))))))

(ert-deftest agent-repl-verbs-help-sends-a-standard-form-with-the-composed-prompt ()
  "The help workspace is a STANDARD create whose initial prompt is the composition."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil (agent-repl-test-verbs--help-sections)))
        (agent-repl-help-preamble "PREAMBLE"))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt)
                   (lambda (_p &optional _history-file _unseeded) "  how do tabs work?  ")))
          ;; Act.
          (agent-repl-create-help-workspace)
          ;; Assert.
          (let ((form (plist-get (agent-repl-test-verbs--request :create) :form)))
            (should (eq (plist-get form :arm) :standard))
            (should (equal (plist-get (plist-get form :value) :initial-prompt)
                           (agent-repl-verbs--said
                            (agent-repl-verbs--help-prompt "how do tabs work?"))))))))))

(ert-deftest agent-repl-verbs-help-needs-no-current-workspace ()
  "The repository is fixed, so asking from no workspace still creates."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil (agent-repl-test-verbs--help-sections))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "q")))
          ;; Act.
          (agent-repl-create-help-workspace)
          ;; Assert.
          (should (agent-repl-test-verbs--request :create)))))))

(ert-deftest agent-repl-verbs-help-refuses-a-blank-question ()
  "A help workspace IS its question, so a blank one sends nothing."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil (agent-repl-test-verbs--help-sections))))
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p &optional _history-file _unseeded) "   ")))
          ;; Act / Assert.
          (should-error (agent-repl-create-help-workspace) :type 'user-error)
          (should-not agent-repl-test-verbs--sent))))))

(ert-deftest agent-repl-verbs-help-refuses-when-the-module-is-in-no-repository ()
  "No git common-dir for the module root is refused and logged as an error."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil (agent-repl-test-verbs--help-sections)))
        (logged nil))
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-help-repo nil
        (cl-letf (((symbol-function 'agent-repl--error)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
                  ((symbol-function 'agent-repl-verbs--read-prompt)
                   (lambda (_p &optional _history-file _unseeded) (error "the question must not be asked"))))
          ;; Act / Assert.
          (should-error (agent-repl-create-help-workspace) :type 'user-error)
          (should-not agent-repl-test-verbs--sent)
          (should (cl-some (lambda (m)
                             (string-match-p
                              "\\`elisp.verbs.help-repository-unresolved module-root=.+ cause=not-a-git-repository\\'"
                              m))
                           logged)))))))

(ert-deftest agent-repl-verbs-help-refuses-when-the-repository-is-not-on-the-roster ()
  "agent-repl's repository missing from the roster is refused and logged as a warning."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row))))
        (logged nil))
    (agent-repl-test-verbs--with nil
      (agent-repl-test-verbs--with-help-repo "/tmp/agent-repl-test/doom/.git"
        (cl-letf (((symbol-function 'agent-repl--warn)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
                  ((symbol-function 'agent-repl-verbs--read-prompt)
                   (lambda (_p &optional _history-file _unseeded) (error "the question must not be asked"))))
          ;; Act / Assert.
          (should-error (agent-repl-create-help-workspace) :type 'user-error)
          (should-not agent-repl-test-verbs--sent)
          (should (cl-some (lambda (m)
                             (string-match-p
                              "\\`elisp.verbs.help-repository-unregistered module-root=.+ main=/tmp/agent-repl-test/doom\\'"
                              m))
                           logged)))))))

(ert-deftest agent-repl-verbs-help-resolves-the-repository-through-the-shared-main-worktree-helper ()
  "The help repository is found by `agent-repl--main-worktree-for-dir', not a hand-rolled derivation."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster nil nil (agent-repl-test-verbs--help-sections)))
        (asked nil))
    (cl-letf (((symbol-function 'agent-repl--main-worktree-for-dir)
               (lambda (dir) (push dir asked) "/tmp/agent-repl-test/doom"))
              ((symbol-function 'agent-repl--repo-key-for-dir)
               (lambda (_dir) (error "the derivation must go through the shared helper")))
              ((symbol-function 'agent-repl--path-canonical) #'directory-file-name))
      ;; Act.
      (let ((ref (agent-repl-verbs--help-repository)))
        ;; Assert.
        (should (equal asked (list (file-name-directory agent-repl-metaprompt-file))))
        (should (equal ref (agent-repl-test-verbs--repo-ref
                            "agent-repl-repo" "/tmp/agent-repl-test/doom")))))))

;;;; ---- Repository selection ----

(ert-deftest agent-repl-verbs-read-repository-defaults-to-the-current-section ()
  "The repository defaults to the section the current workspace's row sits in."
  (let* ((row (agent-repl-test-verbs--row))
         (agent-repl-roster-view (agent-repl-test-verbs--roster (list row)))
         (seen-default nil))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p _c &optional _pr _rm _ii _h default)
                   (setq seen-default default) "repo-one")))
        (should (equal (agent-repl-verbs--read-repository)
                       (agent-repl-test-verbs--repo-ref)))
        (should (equal seen-default "repo-one"))))))

(ert-deftest agent-repl-verbs-read-repository-refuses-with-no-sections ()
  "With no repositories on the roster there is nothing to create in.
The roster is built inline rather than through the fixture: the fixture
supplies a default section precisely so the other tests have one, and
this test needs the genuinely sectionless roster."
  (let ((agent-repl-roster-view
         (list :repository (list :sections nil)
               :recently-merged (list :header (list :label (list :text "Recently Merged"))
                                      :groups nil)
               :current nil)))
    (agent-repl-test-verbs--with nil
      (should-error (agent-repl-verbs--read-repository) :type 'user-error))))

;;;; ---- Shutdown schedule ----

(ert-deftest agent-repl-verbs-shutdown-schedule-carries-at-ms-and-reason ()
  "A scheduled drain carries its deadline instant and its typed reason."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-shutdown-schedule
     (list :arm :schedule :at-ms 1700000000000 :reason (list :arm :deploy)))
    (let ((action (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action)))
      (should (eq (plist-get action :arm) :schedule))
      (should (equal (plist-get (plist-get action :value) :at-ms) 1700000000000))
      (should (eq (plist-get (plist-get (plist-get action :value) :reason) :arm) :deploy)))))

(ert-deftest agent-repl-verbs-shutdown-cancel-carries-an-empty-arm ()
  "Cancel is the whole assertion: the arm carries no payload."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-shutdown-schedule (list :arm :cancel))
    (should (equal (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action)
                   (list :arm :cancel :value nil)))))

(ert-deftest agent-repl-verbs-shutdown-schedule-command-requires-a-reason ()
  "The schedule command always reads a reason: the proto requires one."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "maintenance")))
      (agent-repl-daemon-shutdown-schedule 5)
      (let* ((action (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action))
             (reason (plist-get (plist-get action :value) :reason)))
        (should (eq (plist-get reason :arm) :maintenance))))))

(ert-deftest agent-repl-verbs-operator-reason-refuses-a-blank-note ()
  "An operator reason's note is REQUIRED non-blank, refused before send."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "operator"))
              ((symbol-function 'read-string) (lambda (&rest _) "   ")))
      (should-error (agent-repl-daemon-shutdown-schedule 5) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-operator-reason-carries-its-note ()
  "A non-blank operator note travels inside the arm that owns it."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "operator"))
              ((symbol-function 'read-string) (lambda (&rest _) "rolling the daemon")))
      (agent-repl-daemon-shutdown-now)
      (let* ((action (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action))
             (reason (plist-get (plist-get action :value) :reason)))
        (should (equal (plist-get (plist-get reason :value) :note) "rolling the daemon"))))))

;;;; ---- Deploy ----

(defconst agent-repl-test-verbs--deploy-success
  '(:arm :success
    :value (:components
            ((:component :daemon :build "d1"
              :outcome (:arm :handing-over :value (:workspaces 3 :busy 1 :forced nil)))
             (:component :shim :build "s1"
              :outcome (:arm :shims
                        :value (:bounces ((:workspace "ws-a" :when (:arm :bounced-now :value (:forced nil)))
                                          (:workspace "ws-b" :when (:arm :registered
                                                                    :value (:turn-in-flight t :detached-work 0)))))))
             (:component :webapp :build "w1" :outcome (:arm :reload-pushed :value (:recipients 2)))
             (:component :store :build "st1" :outcome (:arm :restarted :value nil))
             (:component :sidecar :build "sc1" :outcome (:arm :up-to-date :value nil))
             (:component :elisp :build "e1" :outcome (:arm :deferred-to-successor :value nil)))))
  "A DeploySuccess naming one outcome of every arm.")

(defconst agent-repl-test-verbs--deploy-errors
  '(((:arm :already-deploying :value nil)
     "agent-repl: deploy refused: a deploy is already running; ask again when it ends")
    ((:arm :already-rolling-out :value (:waiting-on ("ws-a" "ws-b")))
     "agent-repl: deploy refused: a handover is already in flight, waiting on ws-a, ws-b")
    ((:arm :joining :value nil)
     "agent-repl: deploy refused: this daemon is a successor still joining a handover"))
  "Every DeployError cause arm the verb echoes, and the line it is echoed by.")

(defconst agent-repl-test-verbs--deploy-fault-errors
  '((:arm :build-failed :value (:step "webapp" :detail "tsc: 2 errors" :log "/tmp/b.log"))
    (:arm :service-restart-failed :value (:component :store :detail "exit 78"))
    (:arm :install-failed :value (:component :daemon :detail "EACCES")))
  "Every DeployError cause arm the daemon also stands as a `deploy_failed' fault.
Its minibuffer line is the pushed fault's, never the verb's.")

(ert-deftest agent-repl-verbs-deploy-unforced-sends-no-force ()
  "Without a prefix the deploy is unforced and asks nothing."
  (agent-repl-test-verbs--with nil
    ;; Arrange
    (let ((asked nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq asked t) t)))
        ;; Act
        (agent-repl-deploy nil))
      ;; Assert
      (should (equal (list (agent-repl-test-verbs--request :deploy) asked)
                     '((:force nil) nil))))))

(ert-deftest agent-repl-verbs-deploy-prefix-sends-force-once-confirmed ()
  "With a prefix and a yes, the deploy is sent FORCED."
  (agent-repl-test-verbs--with nil
    ;; Arrange
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      ;; Act
      (agent-repl-deploy '(4)))
    ;; Assert
    (should (equal (agent-repl-test-verbs--request :deploy) '(:force t)))))

(ert-deftest agent-repl-verbs-deploy-forced-confirmation-names-running-turns ()
  "The forced deploy's question says it ends running turns."
  (agent-repl-test-verbs--with nil
    ;; Arrange
    (let ((prompt nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (p) (setq prompt p) t)))
        ;; Act
        (agent-repl-deploy '(4)))
      ;; Assert
      (should (string-search "ends every running turn" prompt)))))

(ert-deftest agent-repl-verbs-deploy-forced-declined-sends-nothing ()
  "A declined forced deploy sends nothing and says so."
  (agent-repl-test-verbs--with nil
    ;; Arrange
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should-error (agent-repl-deploy '(4)) :type 'user-error))
    (should-not (agent-repl-test-verbs--request :deploy))))

(ert-deftest agent-repl-verbs-deploy-waits-the-deploy-timeout ()
  "The deploy waits its own long deadline, not an ordinary verb's."
  (agent-repl-test-verbs--with nil
    ;; Arrange
    (let ((timeout nil)
          (agent-repl-deploy-timeout-seconds 1234))
      (cl-letf (((symbol-function 'agent-repl-rpc-deploy)
                 (lambda (_conn _request &rest keys) (setq timeout (plist-get keys :timeout)))))
        ;; Act
        (agent-repl-deploy nil))
      ;; Assert
      (should (equal timeout 1234)))))

(ert-deftest agent-repl-verbs-deploy-success-is-one-line-naming-every-decision ()
  "A deploy's answer is one echo-area line naming each component's decision."
  (agent-repl-test-verbs--with
      `((:deploy . (:response ,agent-repl-test-verbs--deploy-success)))
    ;; Act
    (agent-repl-deploy nil)
    ;; Assert
    (should (equal (car agent-repl-test-verbs--messages)
                   "agent-repl: deploy: daemon handing over 3 workspaces (1 busy); shim 1 bounced now, 1 registered; webapp reload pushed to 2; store restarted; sidecar up to date; elisp deferred to the successor"))))

(ert-deftest agent-repl-verbs-deploy-restarting-names-both-layouts ()
  "A layout restart's decision names the running and the fresh state layout."
  (agent-repl-test-verbs--with
      '((:deploy . (:response (:arm :success
                               :value (:components
                                       ((:component :daemon :build "d"
                                         :outcome (:arm :restarting
                                                   :value (:running-state-layout 7
                                                           :fresh-state-layout 8
                                                           :workspaces 3 :busy 1
                                                           :forced t)))))))))
    ;; Act
    (agent-repl-deploy nil)
    ;; Assert
    (should (equal (car agent-repl-test-verbs--messages)
                   "agent-repl: deploy: daemon restarting for state layout 7→8, 3 workspaces (1 busy, forced)"))))

(ert-deftest agent-repl-verbs-deploy-forced-success-says-forced ()
  "A forced deploy's answer says it was forced."
  (agent-repl-test-verbs--with
      '((:deploy . (:response (:arm :success
                               :value (:components ((:component :daemon :build "d"
                                                     :outcome (:arm :up-to-date :value nil))))))))
    ;; Arrange
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      ;; Act
      (agent-repl-deploy '(4)))
    ;; Assert
    (should (equal (car agent-repl-test-verbs--messages)
                   "agent-repl: deploy (forced): daemon up to date"))))

(ert-deftest agent-repl-verbs-deploy-success-logs-every-component ()
  "Each component's outcome is its own log record, with its build."
  (agent-repl-test-verbs--with
      `((:deploy . (:response ,agent-repl-test-verbs--deploy-success)))
    ;; Arrange
    (let ((records nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args)
                   (when (string-prefix-p "elisp.verbs.deploy-component" fmt)
                     (push (apply #'format fmt args) records)))))
        ;; Act
        (agent-repl-deploy nil))
      ;; Assert
      (should (equal (mapcar (lambda (r) (and (string-match "component=\\([a-z]+\\) build=\\([a-z0-9]+\\)" r)
                                              (list (match-string 1 r) (match-string 2 r))))
                             (reverse records))
                     '(("daemon" "d1") ("shim" "s1") ("webapp" "w1") ("store" "st1")
                       ("sidecar" "sc1") ("elisp" "e1")))))))

(ert-deftest agent-repl-verbs-deploy-every-refusal-is-echoed-with-its-detail ()
  "Every DeployError arm reaches the echo area with its detail."
  (dolist (case agent-repl-test-verbs--deploy-errors)
    (agent-repl-test-verbs--with
        `((:deploy . (:response (:arm :error :value (:cause ,(car case))))))
      ;; Act
      (agent-repl-deploy nil)
      ;; Assert
      (should (equal (car agent-repl-test-verbs--messages) (cadr case))))))

(defun agent-repl-test-verbs--deploy-error-records (cause)
  "Refuse a deploy with CAUSE and answer the ERROR records it made."
  (let ((errors nil))
    (agent-repl-test-verbs--with
        `((:deploy . (:response (:arm :error :value (:cause ,cause)))))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        (agent-repl-deploy nil)))
    errors))

(defun agent-repl-test-verbs--deploy-refused-record (cause)
  "The ERROR record a deploy refused with CAUSE is expected to make."
  (format "elisp.verbs.deploy-refused arm=%S fields=%S"
          (plist-get cause :arm) (plist-get cause :value)))

(ert-deftest agent-repl-verbs-deploy-every-refusal-is-an-error-record ()
  "Every DeployError arm the verb echoes is recorded at ERROR with its fields."
  (dolist (case agent-repl-test-verbs--deploy-errors)
    ;; Arrange / Act
    (let ((errors (agent-repl-test-verbs--deploy-error-records (car case))))
      ;; Assert
      (should (equal errors (list (agent-repl-test-verbs--deploy-refused-record (car case))))))))

(ert-deftest agent-repl-verbs-deploy-every-fault-refusal-is-an-error-record ()
  "Every DeployError arm its fault echoes is still recorded at ERROR here."
  (dolist (cause agent-repl-test-verbs--deploy-fault-errors)
    ;; Arrange / Act
    (let ((errors (agent-repl-test-verbs--deploy-error-records cause)))
      ;; Assert
      (should (equal errors (list (agent-repl-test-verbs--deploy-refused-record cause)))))))

(ert-deftest agent-repl-verbs-deploy-every-fault-refusal-leaves-the-echo-to-its-fault ()
  "A failure the daemon stands as a fault is not echoed by the verb.
Its pushed fault's line is the one minibuffer line (daemon-link.el)."
  (dolist (cause agent-repl-test-verbs--deploy-fault-errors)
    (agent-repl-test-verbs--with
        `((:deploy . (:response (:arm :error :value (:cause ,cause)))))
      ;; Act
      (agent-repl-deploy nil)
      ;; Assert
      (should-not (agent-repl-test-verbs--messaged-p "deploy refused")))))

(ert-deftest agent-repl-verbs-deploy-fault-refusal-records-its-sentence ()
  "A failure left to its fault still records the verb's own sentence for it."
  (let ((records nil))
    (agent-repl-test-verbs--with
        '((:deploy . (:response (:arm :error
                                 :value (:cause (:arm :install-failed
                                                 :value (:component :daemon :detail "EACCES")))))))
      ;; Arrange
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (when (string-prefix-p "elisp.verbs.deploy-refusal-echoed-by-its-fault" fmt)
                     (push (apply #'format fmt args) records)))))
        ;; Act
        (agent-repl-deploy nil)))
    ;; Assert
    (should (equal records
                   '("elisp.verbs.deploy-refusal-echoed-by-its-fault arm=:install-failed sentence=\"the daemon artifact could not be installed, so nothing was restarted: EACCES\"")))))

(ert-deftest agent-repl-verbs-deploy-fault-arms-are-every-step-failure ()
  "The verb leaves the echo to the fault for exactly the three step failures."
  (should (equal agent-repl-verbs--deploy-fault-arms
                 (mapcar (lambda (cause) (plist-get cause :arm))
                         '((:arm :build-failed) (:arm :install-failed)
                           (:arm :service-restart-failed))))))

(ert-deftest agent-repl-verbs-deploy-unanswered-is-a-transport-failure ()
  "A daemon that does not answer the deploy is reported as a failure."
  (agent-repl-test-verbs--with
      '((:deploy . (:failure (:kind :transport :message "refused"))))
    ;; Act
    (agent-repl-deploy nil)
    ;; Assert
    (should (agent-repl-test-verbs--messaged-p "deploy failed -- the daemon did not answer"))))

;;;; ---- Merge queue ----

(defmacro agent-repl-test-verbs--with-repository (repository &rest body)
  "Run BODY with the roster answering REPOSITORY for any workspace id."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl-roster-repository-of)
              (lambda (&rest _) ,repository)))
     ,@body))

(ert-deftest agent-repl-verbs-merge-queue-pause-names-the-current-repository ()
  "THE QUEUE IS PER REPOSITORY, so a plain pause names the one the current
workspace's roster section is keyed by."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-pause)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :pause
                           :value (list :repository (agent-repl-test-verbs--repo-ref))))))))

(ert-deftest agent-repl-verbs-merge-queue-pause-daemon-wide-omits-the-repository ()
  "A prefix argument means every repository that has a queue: UNSET."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository
        (error "a daemon-wide pause must not consult the roster")
      (agent-repl-merge-queue-pause t)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :pause :value (list :repository nil)))))))

(ert-deftest agent-repl-verbs-merge-queue-pause-without-a-repository-refuses ()
  "No section for the workspace means no scope to send; pausing every
repository instead would exceed what the caller asked for."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository nil
      (should-error (agent-repl-merge-queue-pause) :type 'user-error))))

(ert-deftest agent-repl-verbs-merge-queue-resume-names-the-current-repository ()
  "Resume carries the same per-repository scope as pause."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-resume)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :resume
                           :value (list :repository (agent-repl-test-verbs--repo-ref))))))))

(ert-deftest agent-repl-verbs-merge-queue-resume-daemon-wide-omits-the-repository ()
  "A prefix argument resumes every repository that has a queue."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository
        (error "a daemon-wide resume must not consult the roster")
      (agent-repl-merge-queue-resume t)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :resume :value (list :repository nil)))))))

(ert-deftest agent-repl-verbs-merge-queue-unknown-repository-names-the-repository ()
  "The arm is EMPTY, so the repository the user is told about is the one this
pause sent."
  (agent-repl-test-verbs--with
      '((:merge-queue . (:response (:arm :error
                                    :value (:cause (:arm :unknown-repository :value nil))))))
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-pause)
      (should (agent-repl-test-verbs--messaged-p
               (format "merge-queue refused: the daemon's registry does not hold repository %S"
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-merge-queue-unknown-repository-logs-the-repository ()
  "The dynamic values go in the log context, never baked into the slug."
  (let ((records nil))
    (agent-repl-test-verbs--with
        '((:merge-queue . (:response (:arm :error
                                      :value (:cause (:arm :unknown-repository :value nil))))))
      (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
        (cl-letf (((symbol-function 'agent-repl--warn)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) records))))
          (agent-repl-merge-queue-pause))))
    (should (cl-find-if
             (lambda (record)
               (string-prefix-p "elisp.verbs.merge-queue-unknown-repository action=:pause"
                                record))
             records))))

(ert-deftest agent-repl-verbs-merge-queue-other-arms-still-report-generically ()
  "Claiming one arm must not swallow the rest: another arm reports as always."
  (agent-repl-test-verbs--with
      '((:merge-queue . (:response (:arm :error
                                    :value (:cause (:arm :already-paused :value nil))))))
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-pause)
      (should (agent-repl-test-verbs--messaged-p "merge-queue refused: already-paused")))))

(ert-deftest agent-repl-verbs-merge-queue-evict-names-the-workspace ()
  "Evict takes ONE workspace's merge off the queue, named by its ref."
  (agent-repl-test-verbs--with nil
    (agent-repl-merge-queue-evict)
    (let ((action (plist-get (agent-repl-test-verbs--request :merge-queue) :action)))
      (should (eq (plist-get action :arm) :evict))
      (should (equal (plist-get (plist-get action :value) :workspace)
                     (agent-repl-test-verbs--ref))))))

;;;; ---- Health ----

(defun agent-repl-test-verbs--health-text ()
  "Return the health buffer's contents."
  (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer)
    (buffer-string)))

(ert-deftest agent-repl-verbs-health-insert-shows-the-health-buffer-in-the-popup ()
  "The health report is shown through the ONE shared popup."
  (agent-repl-test--recording-popups
    ;; Act
    (agent-repl-verbs--health-insert '("daemon: HEALTHY"))
    ;; Assert
    (should (equal agent-repl-test--popups-shown
                   (list (get-buffer agent-repl-verbs-health-buffer))))))

(ert-deftest agent-repl-verbs-daemon-health-healthy-says-so ()
  "A healthy daemon renders the verdict and no faults."
  (agent-repl-test-verbs--with
      '((:daemon-health . (:response (:arm :success
                                      :value (:arm :healthy :value nil)))))
    (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
    (agent-repl-daemon-health)
    (should (string-match-p "daemon: HEALTHY" (agent-repl-test-verbs--health-text)))))

(ert-deftest agent-repl-verbs-daemon-health-unhealthy-prints-each-fault ()
  "UNHEALTHY IS AN ANSWER: its faults' details are printed verbatim."
  (agent-repl-test-verbs--with
      '((:daemon-health
         . (:response (:arm :success
                       :value (:arm :unhealthy
                               :value (:faults ((:detail "shim adoption stalled"
                                                 :kind (:arm :adoption-window-expired
                                                        :value nil))
                                                (:detail "store socket absent"
                                                 :kind (:arm :log-sink-poisoned
                                                        :value nil)))))))))
    (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
    (agent-repl-daemon-health)
    (let ((text (agent-repl-test-verbs--health-text)))
      (should (string-match-p "daemon: UNHEALTHY (2 fault(s))" text))
      (should (string-match-p "shim adoption stalled" text))
      (should (string-match-p "store socket absent" text)))))

(ert-deftest agent-repl-verbs-session-health-includes-standing-host-faults ()
  "The host stream's STANDING faults are printed beside the pulled ones."
  (agent-repl-test-verbs--with
      '((:session-health . (:response (:arm :success
                                       :value (:arm :healthy :value nil)))))
    (cl-letf (((symbol-function 'agent-repl-host-faults)
               (lambda (_ws) (list (list :detail "generation fault window open"
                                         :kind (list :arm :link-severed :value nil))))))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (let ((text (agent-repl-test-verbs--health-text)))
        (should (string-match-p "session ws-one: HEALTHY" text))
        (should (string-match-p "standing host faults (1)" text))
        (should (string-match-p "generation fault window open" text))))))

(ert-deftest agent-repl-verbs-daemon-fault-line-names-its-kind ()
  "A DaemonFault renders its KIND beside the detail, never the detail alone."
  (agent-repl-test-verbs--with
      '((:daemon-health
         . (:response (:arm :success
                       :value (:arm :unhealthy
                               :value (:faults ((:detail "prompts dir gone"
                                                 :kind (:arm :prompts-dir-missing
                                                        :value nil)))))))))
    (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
    (agent-repl-daemon-health)
    (should (string-match-p "prompts-dir-missing: prompts dir gone"
                            (agent-repl-test-verbs--health-text)))))

(ert-deftest agent-repl-verbs-session-fault-line-names-its-kind ()
  "A SessionFault renders its KIND beside the detail, through the same formatter."
  (agent-repl-test-verbs--with
      '((:session-health
         . (:response (:arm :success
                       :value (:arm :unhealthy
                               :value (:faults ((:detail "shim exited 2"
                                                 :kind (:arm :shim-start-failed
                                                        :value nil)))))))))
    (cl-letf (((symbol-function 'agent-repl-host-faults) (lambda (_ws) nil)))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (should (string-match-p "shim-start-failed: shim exited 2"
                              (agent-repl-test-verbs--health-text))))))

(ert-deftest agent-repl-verbs-standing-host-fault-line-names-its-kind ()
  "A standing HostFault renders its KIND beside the detail (audit-3 #29)."
  (agent-repl-test-verbs--with
      '((:session-health . (:response (:arm :success
                                       :value (:arm :healthy :value nil)))))
    (cl-letf (((symbol-function 'agent-repl-host-faults)
               (lambda (_ws) (list (list :detail "store socket unreachable"
                                         :kind (list :arm :bounce-unknown :value nil))))))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (should (string-match-p "bounce-unknown: store socket unreachable"
                              (agent-repl-test-verbs--health-text))))))

(ert-deftest agent-repl-verbs-session-health-echoes-the-ref ()
  "SessionHealth is a per-workspace pull and carries the ref."
  (agent-repl-test-verbs--with
      '((:session-health . (:response (:arm :success
                                       :value (:arm :healthy :value nil)))))
    (agent-repl-session-health "ws-one")
    (should (equal (agent-repl-test-verbs--request :session-health)
                   (list :workspace (agent-repl-test-verbs--ref))))))

;;;; ---- Priority reading ----

(ert-deftest agent-repl-verbs-read-priority-clear-label-answers-nil ()
  "The clear entry answers nil, which is how absence is spelled."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) agent-repl-verbs-priority-clear-label)))
    (should-not (agent-repl-verbs--read-priority))))

(ert-deftest agent-repl-verbs-read-priority-label-answers-its-arm ()
  "A level label answers that level's arm."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "P0.5")))
    (should (equal (agent-repl-verbs--read-priority) :p05))))

;;;; ---- Flat arms to codec oneofs ----

(ert-deftest agent-repl-verbs-arm-wraps-a-flat-arms-fields ()
  "A flat arm's own fields become the oneof's VALUE plist."
  (should (equal (agent-repl-verbs--arm (list :arm :evict :workspace "ref"))
                 (list :arm :evict :value (list :workspace "ref")))))

(ert-deftest agent-repl-verbs-arm-gives-an-empty-arm-a-nil-value ()
  "An arm carrying nothing gets a nil VALUE: being set is its whole assertion."
  (should (equal (agent-repl-verbs--arm (list :arm :pause))
                 (list :arm :pause :value nil))))

(ert-deftest agent-repl-verbs-arm-of-nothing-is-nothing ()
  "No arm at all translates to nil rather than an unset-arm oneof."
  (should-not (agent-repl-verbs--arm nil)))

(ert-deftest agent-repl-verbs-level-arm-builds-the-priority-oneof ()
  "A bare level keyword becomes the level arm; the levels carry nothing."
  (should (equal (agent-repl-verbs--level-arm :p2) (list :arm :p2 :value nil))))

(ert-deftest agent-repl-verbs-level-arm-of-nil-clears ()
  "No level is the ABSENCE of the field, never a sentinel arm."
  (should-not (agent-repl-verbs--level-arm nil)))

(ert-deftest agent-repl-verbs-shutdown-action-translates-the-nested-reason ()
  "The `DrainReason' nested in a flat action is translated on the way past."
  (should (equal (agent-repl-verbs--shutdown-action
                  (list :arm :now :reason (list :arm :operator :note "n")))
                 (list :arm :now
                       :value (list :reason (list :arm :operator
                                                  :value (list :note "n")))))))

;;;; ---- Creation forms built at the verb boundary ----

(ert-deftest agent-repl-verbs-create-standard-wraps-the-prompt-as-user-said ()
  "A standard form's initial prompt rides as `UserSaid', not as bare text."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :initial-prompt "fix the flake")
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (blocks (plist-get (plist-get (plist-get (plist-get form :value)
                                                    :initial-prompt)
                                         :content)
                              :blocks)))
      (should (equal (plist-get (plist-get (car blocks) :value) :text) "fix the flake")))))

(ert-deftest agent-repl-verbs-create-standard-wraps-a-merge-action-as-user-said ()
  "A standard form's pre-merge action rides as `UserSaid', not as bare text."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :merge-actions (list :before-ws-merge "run the linter"))
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (blocks (plist-get (plist-get (plist-get (plist-get (plist-get form :value)
                                                              :merge-actions)
                                                    :before-ws-merge)
                                         :content)
                              :blocks)))
      (should (equal (plist-get (plist-get (car blocks) :value) :text) "run the linter")))))

(ert-deftest agent-repl-verbs-create-standard-wraps-a-postprocessing-prompt-as-user-said ()
  "A standard form's postprocessing prompt rides as `UserSaid' too."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :merge-actions (list :postprocessing-prompt "tidy up"))
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (blocks (plist-get (plist-get (plist-get (plist-get (plist-get form :value)
                                                              :merge-actions)
                                                    :postprocessing-prompt)
                                         :content)
                              :blocks)))
      (should (equal (plist-get (plist-get (car blocks) :value) :text) "tidy up")))))

(ert-deftest agent-repl-verbs-create-standard-without-merge-actions-omits-them ()
  "No merge actions is the ABSENCE of the message, never an empty one."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (let ((form (plist-get (agent-repl-test-verbs--request :create) :form)))
      (should-not (plist-get (plist-get form :value) :merge-actions)))))

(ert-deftest agent-repl-verbs-create-fork-without-a-parent-is-refused ()
  "A fork without a parent is unrepresentable, so it is refused, not encoded."
  (agent-repl-test-verbs--with nil
    ;; Act / Assert
    (should-error (agent-repl-verb-create (agent-repl-test-verbs--repo-ref)
                                          :standard :fork t)
                  :type 'user-error)))

(ert-deftest agent-repl-verbs-create-fork-without-a-parent-sends-nothing ()
  "The refusal is BEFORE the send: silently dropping the fork would create
a plain workspace the caller never asked for."
  (agent-repl-test-verbs--with nil
    ;; Act
    (ignore-errors
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :fork t))
    ;; Assert
    (should-not agent-repl-test-verbs--sent)))

(ert-deftest agent-repl-verbs-create-fork-rides-inside-the-parent ()
  "FORK lives INSIDE the parent: a fork without a parent is unrepresentable."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :parent (agent-repl-test-verbs--ref) :fork t)
    (should (equal (plist-get (agent-repl-test-verbs--request :create) :parent)
                   (list :workspace (agent-repl-test-verbs--ref) :fork t)))))

(ert-deftest agent-repl-verbs-create-unknown-form-refuses ()
  "A form arm the verb does not know is refused before anything is sent."
  (agent-repl-test-verbs--with nil
    (should-error (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :bogus)
                  :type 'user-error)))

;;;; ---- Resolution and logging ----

(ert-deftest agent-repl-verbs-conn-stands-the-link-when-none-is-up ()
  "A daemon-admin verb with no standing link STANDS one rather than refusing."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () nil))
              ((symbol-function 'agent-repl-link-connect) (lambda () 'stood-conn)))
      (should (eq (agent-repl-verbs--conn) 'stood-conn)))))

(ert-deftest agent-repl-verbs-conn-refuses-when-the-link-cannot-be-stood ()
  "No daemon anywhere is a refusal, not a request sent into nothing."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () nil))
              ((symbol-function 'agent-repl-link-connect) (lambda () nil)))
      (should-error (agent-repl-verbs--conn) :type 'user-error))))

(ert-deftest agent-repl-verbs-refusal-slug-names-the-verb ()
  "A refusal's operation slug is `elisp.verbs.<op>-refused'."
  (let ((formats nil))
    (agent-repl-test-verbs--with
        '((:merge . (:response (:arm :error
                                :value (:cause (:arm :already-queued :value nil))))))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest _args) (push fmt formats))))
        (agent-repl-verb-merge "ws-one")))
    (should (cl-find-if (lambda (fmt) (string-prefix-p "elisp.verbs.merge-refused" fmt))
                        formats))))

;;;; ---- Current-perspective target resolution ----

(defmacro agent-repl-test-verbs--with-registry (names dirs current &rest body)
  "Run BODY with NAMES live, DIRS as their `:project-dir's and CURRENT active.
NAMES that are absent from DIRS own no directory, which is exactly how a
persp-mode perspective such as \"main\" appears in the registry."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () ,names))
             ((symbol-function 'agent-repl--ws-get)
              (lambda (ws key)
                (and (eq key :project-dir) (cdr (assoc ws ,dirs)))))
             ((symbol-function 'agent-repl--pseudo-workspace-name-p)
              (lambda (ws) (member ws '("main" "none"))))
             ((symbol-function 'agent-repl--ws-current-name) (lambda () ,current)))
     ,@body))

(ert-deftest agent-repl-verbs-close-with-an-empty-registry-refuses ()
  "No registered workspace at all: the picker's refusal, and no wire call."
  ;; Arrange
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      ;; Act / Assert
      (should (equal (should-error (agent-repl-close-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-close-with-only-pseudo-workspaces-refuses ()
  "A registry holding only persp-mode's own perspectives is an EMPTY registry."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("main" "none") nil "main"
      (should (equal (should-error (agent-repl-close-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered"))))))

(ert-deftest agent-repl-verbs-close-from-a-pseudo-perspective-picks ()
  "Current is \"main\" but real workspaces exist: the user PICKS one."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("main" "ws-one") '(("ws-one" . "/tmp/ws-one")) "main"
      (cl-letf (((symbol-function 'agent-repl--read-known-workspace)
                 (lambda (_prompt) "ws-one")))
        (agent-repl-close-workspace))
      (should (equal (agent-repl-test-verbs--request :close)
                     (list :workspace (agent-repl-test-verbs--ref)))))))

(ert-deftest agent-repl-verbs-close-from-a-registered-workspace-acts-on-it ()
  "A registered current workspace is acted on directly, with no picker."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      (cl-letf (((symbol-function 'agent-repl--read-known-workspace)
                 (lambda (_prompt) (error "the picker must not run"))))
        (agent-repl-close-workspace))
      (should (agent-repl-test-verbs--request :close)))))

(ert-deftest agent-repl-verbs-close-without-a-ref-keeps-the-identity-refusal ()
  "A REAL registered workspace with no daemon ref still refuses on identity."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
        (should (equal (should-error (agent-repl-close-workspace) :type 'user-error)
                       '(user-error "agent-repl: workspace ws-one has no daemon identity yet")))))))

(ert-deftest agent-repl-verbs-a-closed-workspace-is-addressed-by-its-roster-ref ()
  "A closed workspace has no tab and no host entry; its roster row's ref serves."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      (let ((agent-repl-roster-view
             (list :repository
                   (list :sections
                         (list (list :rows
                                     (list :rows
                                           (list (list :workspace (list :workspace '(:id "id-1" :dir "/tmp/ws-one"))
                                                       :name '(:text "ws-one")
                                                       :closed '(:closed t))))))))))
        (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
          (should (equal (agent-repl-verbs--ref "ws-one") '(:id "id-1" :dir "/tmp/ws-one"))))))))

(ert-deftest agent-repl-verbs-a-roster-row-of-another-directory-is-no-ref ()
  "A row is matched by directory, never by a colliding name."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      (let ((agent-repl-roster-view
             (list :repository
                   (list :sections
                         (list (list :rows
                                     (list :rows
                                           (list (list :workspace (list :workspace '(:id "id-2" :dir "/other/ws-one"))
                                                       :name '(:text "ws-one")
                                                       :closed '(:closed t))))))))))
        (should-not (agent-repl-verbs--roster-ref "ws-one"))))))

(ert-deftest agent-repl-verbs-kill-with-an-empty-registry-refuses ()
  "The kill verb defaults to the current perspective too, and refuses alike."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      (should (equal (should-error (agent-repl-kill-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-restart-with-an-empty-registry-refuses ()
  "So does restart: same default, same refusal, nothing sent."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      (should (equal (should-error (agent-repl-restart-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-merge-command-without-prefix-closes-on-landing ()
  "`SPC TAB M' with no prefix merges the current workspace and closes it."
  ;; Arrange
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      ;; Act
      (let ((current-prefix-arg nil))
        (call-interactively #'agent-repl-merge-workspace))
      ;; Assert
      (should (equal (plist-get (agent-repl-test-verbs--request :merge) :mode)
                     '(:arm :one-shot :value (:landing (:arm :own-branch :value nil))))))))

(ert-deftest agent-repl-verbs-merge-command-ignores-a-prefix ()
  "`C-u SPC TAB M' is the same one-shot: a merge that leaves its workspace
working is an async merge, which Emacs does not offer."
  ;; Arrange
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      ;; Act
      (let ((current-prefix-arg '(4)))
        (call-interactively #'agent-repl-merge-workspace))
      ;; Assert
      (should (equal (plist-get (agent-repl-test-verbs--request :merge) :mode)
                     '(:arm :one-shot :value (:landing (:arm :own-branch :value nil))))))))

(ert-deftest agent-repl-verbs-merge-with-an-empty-registry-refuses ()
  "And merge, which shares the same current-perspective default."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      (should (equal (should-error (agent-repl-merge-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))


;;;; ---- Model selection ----

(ert-deftest agent-repl-verbs-read-model-seeds-the-configured-model ()
  "The picker opens on `agent-repl-interactive-model' as its initial input."
  (let ((agent-repl-interactive-model "sonnet")
        (seen-initial :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm initial &rest _)
                 (setq seen-initial initial) initial)))
      (should (equal (agent-repl-verbs--read-model) "sonnet"))
      (should (equal seen-initial "sonnet")))))

(ert-deftest agent-repl-verbs-read-model-seeds-nothing-when-unset ()
  "A nil setting seeds no initial input, so the prompt opens blank."
  (let ((agent-repl-interactive-model nil)
        (seen-initial :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm initial &rest _)
                 (setq seen-initial initial) "")))
      (should-not (agent-repl-verbs--read-model))
      (should-not seen-initial))))

(ert-deftest agent-repl-verbs-read-model-seeds-todays-default ()
  "Unconfigured, the picker opens on the defcustom's shipped default."
  (let ((seen-initial :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm initial &rest _)
                 (setq seen-initial initial) initial)))
      (agent-repl-verbs--read-model)
      (should (equal seen-initial
                     (eval (car (get 'agent-repl-interactive-model
                                     'standard-value))
                           t))))))

(ert-deftest agent-repl-verbs-read-model-honors-an-erased-seed ()
  "Erasing the seed back to blank still asks the daemon to choose."
  (let ((agent-repl-interactive-model "opus"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "   ")))
      (should-not (agent-repl-verbs--read-model)))))

(ert-deftest agent-repl-verbs-register-repository-switched-away-from-moves-nothing ()
  "Registering a repository withdraws its landing on a switch in Emacs too."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
        (agent-repl-register-repository)))
    (agent-repl-test-verbs--user-switches)
    ;; Act.
    (agent-repl-test-verbs--tab-arrives "ws-id-1" "repo-ws")
    ;; Assert.
    (should-not agent-repl-test-verbs--tab-switched)))

(provide 'test-verbs)

;;; test-verbs.el ends here

(ert-deftest agent-repl-verbs-a-landing-still-waiting-for-its-tab-is-recorded ()
  "A landing that did not fire says what it is still waiting for.
Nothing here polls or times out, so for as long as the tab did not come
the log went silent between `pending-landing-registered' and nothing at
all -- which is what three minutes of waiting on a register whose row
came back CLOSED left behind."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "waiting" "/tmp/w") 0)
        (cl-letf (((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
          ;; Act.
          (agent-repl-verbs--pending-landing-fire)))
      ;; Assert.
      (should (seq-some (lambda (text)
                          (string-search "elisp.verbs.pending-landing-waiting id=waiting" text))
                        logs)))))

(ert-deftest agent-repl-verbs-a-landing-that-fires-records-no-wait ()
  "The wait record names a landing that is STILL pending, never one that landed."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "arrives" "/tmp/w") 0))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl-switch-to-project) #'ignore))
        ;; Act.
        (agent-repl-test-verbs--tab-arrives "arrives" "arrived-ws"))
      ;; Assert.
      (should-not (seq-some (lambda (text)
                              (string-search "elisp.verbs.pending-landing-waiting" text))
                            logs)))))

(ert-deftest agent-repl-verbs-a-waiting-landing-is-not-cleared-by-the-record ()
  "Recording the wait must not consume the landing: the tab is still coming."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "kept" "/tmp/w") 0)
      ;; Act.
      (agent-repl-verbs--pending-landing-fire))
    ;; Assert.
    (should (equal (plist-get agent-repl-verbs--pending-landing :id) "kept"))))

;;;; ---- A landing the user switched away from is withdrawn ----

(defun agent-repl-test-verbs--user-switches ()
  "Record that the user switched workspaces in Emacs, as host.el counts it."
  (setq agent-repl-host--editor-switches (1+ agent-repl-host--editor-switches)))

(ert-deftest agent-repl-verbs-a-landing-switched-away-from-before-the-answer-moves-nothing ()
  "A user who switched in Emacs before the answer is not dragged back."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--user-switches)
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "new-ws")))
      ;; Act.
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)))

(ert-deftest agent-repl-verbs-a-landing-switched-away-from-before-the-answer-waits-for-nothing ()
  "A withdrawn landing leaves no landing pending for its tab."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--user-switches)
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      ;; Act.
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
    ;; Assert.
    (should-not agent-repl-verbs--pending-landing)))

(ert-deftest agent-repl-verbs-a-withdrawn-landing-is-recorded-at-info ()
  "An automatic move that did not happen says why, with both counts."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let (infos)
      (agent-repl-test-verbs--user-switches)
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act.
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
      ;; Assert.
      (should (member "elisp.verbs.landing-withdrawn id=new dir=/tmp/n edge=answered reason=user-switched dispatched-at=0 now=1"
                      infos)))))

(ert-deftest agent-repl-verbs-a-landing-switched-away-from-while-waiting-moves-nothing ()
  "A switch in Emacs while the tab is on its way withdraws the landing."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
    (agent-repl-test-verbs--user-switches)
    ;; Act.
    (agent-repl-test-verbs--tab-arrives "new" "new-ws")
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)))

(ert-deftest agent-repl-verbs-a-withdrawn-waiting-landing-is-cleared ()
  "The tab arrived and the landing was judged, so nothing stays pending."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
    (agent-repl-test-verbs--user-switches)
    ;; Act.
    (agent-repl-test-verbs--tab-arrives "new" "new-ws")
    ;; Assert.
    (should-not agent-repl-verbs--pending-landing)))

(ert-deftest agent-repl-verbs-a-waiting-landing-with-no-switch-still-lands ()
  "Without a switch in Emacs the tab's arrival lands the user as before."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
    ;; Act.
    (agent-repl-test-verbs--tab-arrives "new" "new-ws")
    ;; Assert.
    (should (equal agent-repl-test-verbs--selected '("/tmp/n")))))

(ert-deftest agent-repl-verbs-a-landing-is-an-automatic-switch ()
  "The landing's own activation must not count as the user's switch."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let (automatic)
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "new-ws"))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (_dir) (setq automatic agent-repl-host-automatic-switch))))
        ;; Act.
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
      ;; Assert.
      (should automatic))))

(ert-deftest agent-repl-verbs-a-landing-is-held-while-a-minibuffer-is-active ()
  "A landing whose tab arrives while the user types moves nothing yet."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0))
    (cl-letf (((symbol-function 'agent-repl--minibuffer-active-p) (lambda () t)))
      ;; Act.
      (agent-repl-test-verbs--tab-arrives "new" "new-ws"))
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)
    (should (equal (mapcar #'car agent-repl--minibuffer-held) '(landing)))))

(ert-deftest agent-repl-verbs-a-held-landing-lands-once-the-minibuffer-exits ()
  "The held landing is made when the minibuffer has gone, as Emacs's own move."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let ((active t) automatic)
      (cl-letf (((symbol-function 'agent-repl--minibuffer-active-p) (lambda () active))
                ((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "new-ws"))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (dir)
                   (setq automatic agent-repl-host-automatic-switch)
                   (push dir agent-repl-test-verbs--selected))))
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0)
        (setq active nil)
        ;; Act.
        (agent-repl--minibuffer-release))
      ;; Assert.
      (should (equal agent-repl-test-verbs--selected '("/tmp/n")))
      (should automatic))))

(ert-deftest agent-repl-verbs-a-held-landing-whose-workspace-left-is-dropped ()
  "A workspace gone from the roster by the release is not landed on: the drop is said."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let ((active t) (tab "new-ws") infos)
      (cl-letf (((symbol-function 'agent-repl--minibuffer-active-p) (lambda () active))
                ((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) tab))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "new" "/tmp/n") 0)
        (setq active nil tab nil)
        ;; Act.
        (agent-repl--minibuffer-release))
      ;; Assert.
      (should-not agent-repl-test-verbs--selected)
      (should (member "elisp.verbs.landing-dropped ws=new-ws id=new reason=workspace-gone"
                      infos)))))

;;;; ---- The arrival reason a verb leaves for the roster (owner ruling #6) ----

(ert-deftest agent-repl-verbs-create-claims-its-arrival-as-created ()
  "A create leaves `created' for the tab it is about to be given."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                  (agent-repl-test-verbs--ref "made" "/tmp/made"))
      ;; Act.
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
      ;; Assert.
      (should (equal (agent-repl--panels-take-arrival-reason "made") "created")))))

(ert-deftest agent-repl-verbs-fork-claims-its-arrival-as-forked ()
  "A fork leaves `forked', so its panels open naming the verb that asked."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                  (agent-repl-test-verbs--ref "child" "/tmp/child"))
      ;; Act.
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                              :parent (agent-repl-test-verbs--ref) :fork t)
      ;; Assert.
      (should (equal (agent-repl--panels-take-arrival-reason "child") "forked")))))

(ert-deftest agent-repl-verbs-one-shot-claims-its-arrival-though-it-never-selects ()
  "A one-shot never stands on its workspace, and still opens its panels."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                  (agent-repl-test-verbs--ref "shot" "/tmp/shot"))
      ;; Act.
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot :prompt "go")
      ;; Assert.
      (should (equal (agent-repl--panels-take-arrival-reason "shot") "created")))))

(ert-deftest agent-repl-verbs-open-claims-its-arrival-as-reopened ()
  "Re-opening a closed workspace leaves `reopened' for its returning tab."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl-test-verbs--with nil
      ;; Act.
      (agent-repl-verb-open (agent-repl-test-verbs--ref "back" "/tmp/back"))
      ;; Assert.
      (should (equal (agent-repl--panels-take-arrival-reason "back") "reopened")))))

(ert-deftest agent-repl-verbs-a-refused-create-claims-no-arrival ()
  "A create the daemon refused mints nothing, so it claims no arrival."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl-test-verbs--with
        (list (cons :create (list :failure (list :arm :error :value nil))))
      ;; Act.
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
      ;; Assert.
      (should (equal (agent-repl--panels-take-arrival-reason "ws-id-1") "arrived")))))

;;;; ---- RegisterRepository ----
;;
;; `SPC j .': a repository joins the roster on its own, picked as a FILE
;; inside it.  A repository used to enter only as a side effect of
;; registering a workspace, so a checkout nobody had worked in yet could not
;; be named at all.

(defun agent-repl-test-verbs--register-repository-answer
    (&optional already-known workspace-already-known)
  "Return the answers alist for a RegisterRepository that SUCCEEDED.
ALREADY-KNOWN non-nil scripts the idempotent answer for the REPOSITORY,
and WORKSPACE-ALREADY-KNOWN the one for the main-worktree WORKSPACE the
same call registers.  The two are independent, which is why they are two
arguments."
  (list (cons :register-repository
              (list :response
                    (list :arm :success
                          :value (list :repository (agent-repl-test-verbs--repo-ref)
                                       :already-known already-known
                                       :workspace (list :id "ws-id-1"
                                                        :dir "/tmp/agent-repl-test/repo")
                                       :workspace-already-known
                                       workspace-already-known))))))

(defmacro agent-repl-test-verbs--picking-file (path &rest body)
  "Run BODY with `read-file-name' answering PATH."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) ,path)))
     ,@body))

(ert-deftest agent-repl-verbs-register-repository-sends-the-picked-path ()
  "The request carries the FILE the user picked, verbatim."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/lisp/verbs.el"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (equal (agent-repl-test-verbs--request :register-repository)
                     (list :path "/tmp/agent-repl-test/repo/lisp/verbs.el"))))))

(ert-deftest agent-repl-verbs-register-repository-reports-a-fresh-registration ()
  "A repository the daemon minted is echoed as registered, by its resolved dir."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "registered repository /tmp/agent-repl-test/repo")))))

(ert-deftest agent-repl-verbs-register-repository-reports-the-workspace-it-opened ()
  "The ack names BOTH facts: the repository, and the workspace opened with it.
The main worktree is what `SPC p p\=' can then switch to, so a report that
named only the repository would omit the half the owner asked for."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "registered repository /tmp/agent-repl-test/repo; workspace repo opened")))))

(ert-deftest agent-repl-verbs-register-repository-reports-a-workspace-already-known ()
  "A main worktree already registered is reported as already known, not opened."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer nil t)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "registered repository /tmp/agent-repl-test/repo; workspace repo already known")))))

(ert-deftest agent-repl-verbs-register-repository-reports-the-two-already-knowns-apart ()
  "The repository may be already known while its workspace is freshly opened.
That is the state a repository registered before the workspace half landed
is in, so the two bools are reported independently rather than as one."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer t nil)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "already known repository /tmp/agent-repl-test/repo; workspace repo opened")))))

(ert-deftest agent-repl-verbs-register-repository-reports-one-already-known ()
  "`already_known' is an ANSWER: the command says so rather than claiming a mint."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer t t)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "already known repository /tmp/agent-repl-test/repo; workspace repo already known")))))

(ert-deftest agent-repl-verbs-register-repository-claims-its-arrival-as-registered ()
  "`SPC j .\' leaves `registered\' so the ROSTER opens the workspace it asked for.
The new workspace is born in this editor when its roster row arrives, and
the roster opens panels only for an arrival a verb claimed by name -- so
claiming it is what makes the tab a real one to switch to."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
      (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
        ;; Act.
        (agent-repl-register-repository)
        ;; Assert.
        (should (equal (agent-repl--panels-take-arrival-reason "ws-id-1")
                       "registered"))))))

(ert-deftest agent-repl-verbs-register-repository-switches-to-the-new-workspace-tab ()
  "Registering a repository STANDS THE USER ON its main worktree's own tab."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      (agent-repl-test-verbs--tab-arrives "ws-id-1" "repo-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--tab-switched '("repo-ws"))))))

(ert-deftest agent-repl-verbs-register-repository-leaves-the-origin-workspace-alone ()
  "The workspace the command was RUN FROM is not touched by the switch.
Landing by project DIRECTORY let Doom pick the perspective by the repo
directory's basename, miss the workspace's own tab, and mint one from the
origin's window configuration -- which is how `SPC j .\' run from iterm-2
turned iterm-2 into explanation-engine.  The landing is by identity now,
so no project switch is made at all."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      (agent-repl-test-verbs--tab-arrives "ws-id-1" "repo-ws")
      ;; Assert.
      (should-not agent-repl-test-verbs--selected))))

(ert-deftest agent-repl-verbs-register-repository-switches-to-an-already-known-workspace ()
  "A main worktree already registered still gets switched to, not skipped.
`already_known\' is an answer about MINTING, never a reason to leave the
user where they were."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--register-repository-answer t t)
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      (agent-repl-test-verbs--tab-arrives "ws-id-1" "repo-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--tab-switched '("repo-ws"))))))

(ert-deftest agent-repl-verbs-a-refused-register-repository-switches-nowhere ()
  "A refusal moves the user NOWHERE: there is no workspace to come up on."
  ;; Arrange.
  (agent-repl-test-verbs--with
      (list (cons :register-repository
                  (list :response
                        (list :arm :error
                              :value (list :cause (list :arm :not-in-a-repository
                                                        :value nil))))))
    (agent-repl-test-verbs--picking-file "/tmp/elsewhere/loose.txt"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should-not agent-repl-test-verbs--tab-switched))))

(ert-deftest agent-repl-verbs-register-repository-reports-a-path-in-no-repository ()
  "The `not_in_a_repository' arm reaches the user as its own sentence."
  ;; Arrange.
  (agent-repl-test-verbs--with
      (list (cons :register-repository
                  (list :response
                        (list :arm :error
                              :value (list :cause (list :arm :not-in-a-repository
                                                        :value nil))))))
    (agent-repl-test-verbs--picking-file "/tmp/elsewhere/loose.txt"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "that path is not inside a git repository: /tmp/elsewhere/loose.txt")))))

(ert-deftest agent-repl-verbs-register-repository-reports-an-unreadable-path ()
  "The `unreadable_path' arm reaches the user as its own sentence."
  ;; Arrange.
  (agent-repl-test-verbs--with
      (list (cons :register-repository
                  (list :response
                        (list :arm :error
                              :value (list :cause (list :arm :unreadable-path
                                                        :value nil))))))
    (agent-repl-test-verbs--picking-file "/tmp/absent.txt"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "that path cannot be read: /tmp/absent.txt")))))

(ert-deftest agent-repl-verbs-register-repository-reports-a-temporary-folder ()
  "The `inside_temporary_directory' arm names the refused folder and why."
  ;; Arrange.
  (agent-repl-test-verbs--with
      (list (cons :register-repository
                  (list :response
                        (list :arm :error
                              :value (list :cause (list :arm :inside-temporary-directory
                                                        :value (list :dir "/private/tmp/scratch"
                                                                     :temporary-root "/private/tmp")))))))
    (agent-repl-test-verbs--picking-file "/tmp/scratch/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "/private/tmp/scratch is inside the temporary directory /private/tmp; agent-repl does not register temporary folders")))))

(ert-deftest agent-repl-verbs-register-repository-refusal-is-recorded-as-an-answer ()
  "The handler records a refusal at INFO: an answer, not a fault (no WARN)."
  (let (records)
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws level _verbosity fmt &rest _)
                 (push (cons level fmt) records))))
      ;; Arrange.
      (agent-repl-test-verbs--with
          (list (cons :register-repository
                  (list :response
                        (list :arm :error
                              :value (list :cause (list :arm :inside-temporary-directory
                                                        :value (list :dir "/private/tmp/scratch"
                                                                     :temporary-root "/private/tmp")))))))
        (agent-repl-test-verbs--picking-file "/tmp/scratch/README.md"
          ;; Act.
          (agent-repl-register-repository))))
    ;; Assert.
    (let ((refused (seq-filter (lambda (r) (string-search "register-repository-refused" (cdr r)))
                               records)))
      (should refused)
      (should (seq-every-p (lambda (r) (equal (car r) "info")) refused)))))

(ert-deftest agent-repl-verbs-register-repository-falls-through-an-arm-it-has-no-sentence-for ()
  "An arm this command does not know still reports, through the generic path.
A refusal the daemon adds later must reach the user the day it ships."
  ;; Arrange.
  (agent-repl-test-verbs--with
      (list (cons :register-repository
                  (list :response
                        (list :arm :error
                              :value (list :cause (list :arm :some-future-arm
                                                        :value nil))))))
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p "register-repository refused: some-future-arm")))))

(ert-deftest agent-repl-verbs-register-repository-reports-a-transport-failure ()
  "Nobody answering is a different fact from the daemon refusing."
  ;; Arrange.
  (agent-repl-test-verbs--with
      (list (cons :register-repository (list :failure (list :detail "no daemon"))))
    (agent-repl-test-verbs--picking-file "/tmp/agent-repl-test/repo/README.md"
      ;; Act.
      (agent-repl-register-repository)
      ;; Assert.
      (should (agent-repl-test-verbs--messaged-p
               "register-repository failed -- the daemon did not answer")))))

(ert-deftest agent-repl-verbs-register-repository-defaults-to-the-buffers-own-file ()
  "The buffer you are looking at is almost always in the repository you mean."
  ;; Arrange.
  (let (prompted-default)
    (cl-letf (((symbol-function 'buffer-file-name) (lambda (&rest _) "/tmp/repo/a/b.el"))
              ((symbol-function 'read-file-name)
               (lambda (_prompt _dir default &rest _) (setq prompted-default default) default)))
      ;; Act.
      (agent-repl-verbs--register-repository-read-path))
    ;; Assert.
    (should (equal prompted-default "/tmp/repo/a/b.el"))))

(ert-deftest agent-repl-verbs-register-repository-falls-back-to-the-default-directory ()
  "A buffer visiting no file still has a directory, which is a complete answer."
  ;; Arrange.
  (let ((default-directory "/tmp/repo/a/")
        prompted-default)
    (cl-letf (((symbol-function 'buffer-file-name) (lambda (&rest _) nil))
              ((symbol-function 'read-file-name)
               (lambda (_prompt _dir default &rest _) (setq prompted-default default) default)))
      ;; Act.
      (agent-repl-verbs--register-repository-read-path))
    ;; Assert.
    (should (equal prompted-default "/tmp/repo/a/"))))

;;;; ---- agent-repl-verbs--send-op: the op dies with the rpc's own end ----

(defmacro agent-repl-test-verbs--send-op-with (answer &rest body)
  "Run BODY with `agent-repl-verbs--send' answering ANSWER to its caller.
ANSWER is (KEY . VALUE): the callback KEY the fake dispatcher invokes."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl-verbs--send)
              (lambda (_rpc _conn _request &rest keys)
                (let ((fn (plist-get keys (car ,answer))))
                  (when fn (funcall fn (cdr ,answer)))))))
     ,@body))

(ert-deftest agent-repl-verbs-send-op-forgets-the-op-on-every-end-of-the-rpc ()
  "A success, a refusal and no answer each retire the op before the callback."
  (dolist (end '(:on-success :on-error :on-transport-failure))
    (clrhash agent-repl-mutation-progress--pending)
    (agent-repl-mutation-progress-register "op-e" :on-stage #'ignore)
    (let (seen-registered)
      (agent-repl-test-verbs--send-op-with (cons end 'value)
        (apply #'agent-repl-verbs--send-op "op-e" #'ignore nil nil
               (list end (lambda (_) (setq seen-registered
                                           (gethash "op-e" agent-repl-mutation-progress--pending))))))
      (should-not (gethash "op-e" agent-repl-mutation-progress--pending))
      (should-not seen-registered))))

(ert-deftest agent-repl-verbs-send-op-keeps-an-accepted-op ()
  "An accepted ack keeps the op: its end is still on the progress channel."
  (clrhash agent-repl-mutation-progress--pending)
  (agent-repl-mutation-progress-register "op-a" :on-stage #'ignore)
  (agent-repl-test-verbs--send-op-with (cons :on-accepted 'value)
    (agent-repl-verbs--send-op "op-a" #'ignore nil nil :on-accepted #'ignore))
  (should (gethash "op-a" agent-repl-mutation-progress--pending)))

(ert-deftest agent-repl-verbs-send-op-lets-on-error-claim-the-arm ()
  "ON-ERROR's answer is what the dispatcher reads as the claim."
  (let (claimed)
    (cl-letf (((symbol-function 'agent-repl-verbs--send)
               (lambda (_rpc _conn _request &rest keys)
                 (setq claimed (funcall (plist-get keys :on-error) 'value)))))
      (agent-repl-verbs--send-op "op-c" #'ignore nil nil :on-error (lambda (_) 'mine)))
    (should (eq claimed 'mine))))

(ert-deftest agent-repl-verbs-no-verb-forgets-its-op-by-hand ()
  "Every verb that registers an op sends through `agent-repl-verbs--send-op'.
A verb that forgets its op by hand is the one that misses a path."
  (dolist (expected '(("verbs.el" . 1) ("conversations.el" . 0)))
    (let ((source (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name (car expected)
                                       (file-name-directory
                                        (symbol-file 'agent-repl-verbs--send-op 'defun))))
                    (buffer-string)))
          (count 0) (start 0))
      (while (setq start (string-search "(agent-repl-mutation-progress-forget op-id)" source start))
        (setq count (1+ count) start (1+ start)))
      ;; The one occurrence in verbs.el is `agent-repl-verbs--send-op' itself.
      (should (equal (cons (car expected) count) expected)))))

;;;; ---- One set of row readers ----

(ert-deftest agent-repl-test-verbs-reads-rows-through-the-roster-readers ()
  "verbs.el spells no row reader of its own: every read of a row's ref,
name or closed flag is roster.el's, so the two files cannot disagree."
  ;; Arrange
  (let ((source (with-temp-buffer
                  (insert-file-contents agent-repl-test-verbs--source)
                  (buffer-string))))
    ;; Act / Assert
    (should-not (string-match-p "(defun agent-repl-verbs--row-\\(ref\\|name\\|closed-p\\) " source))))
