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
                   :when nil
                   :detail (list :branch nil :parent-branch nil :summary nil)
                   :closed (list :closed nil))))
    (while overrides
      (setq row (plist-put row (pop overrides) (pop overrides))))
    row))

(defun agent-repl-test-verbs--roster (rows &optional merged-rows sections)
  "Return a decoded `WorkspaceRoster' carrying ROWS in one repo section."
  (list :repository
        (list :sections
              (or sections
                  (list (list :key (list :repository (agent-repl-test-verbs--repo-ref))
                              :header (list :label (list :text "repo-one"))
                              :rows (list :rows rows)))))
        :task (list :sections nil)
        :recently-merged (list :header (list :label (list :text "Recently Merged"))
                               :rows (list :rows merged-rows))
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
         (agent-repl-verbs--pending-landing-lander nil)
         (agent-repl-test-verbs--tab-switched nil)
         (agent-repl-test-verbs--torn-down nil)
         (agent-repl-test-verbs--messages nil)
         (agent-repl-test-verbs--handover nil)
         (agent-repl-test-verbs--selected nil)
         (agent-repl-test-verbs--restart-holds nil)
         (agent-repl-test-verbs--answers ,answers))
     (cl-letf* (((symbol-function 'agent-repl-host-ref)
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
                ((symbol-function 'agent-repl-rpc-update-shutdown-schedule)
                 (agent-repl-test-verbs--stub :shutdown-schedule))
                ((symbol-function 'agent-repl-rpc-update-merge-queue)
                 (agent-repl-test-verbs--stub :merge-queue))
                ((symbol-function 'agent-repl-rpc-deploy)
                 (agent-repl-test-verbs--stub :deploy))
                ((symbol-function 'agent-repl-rpc-daemon-health)
                 (agent-repl-test-verbs--stub :daemon-health))
                ((symbol-function 'agent-repl-rpc-session-health)
                 (agent-repl-test-verbs--stub :session-health)))
       ,@body)))

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
  "KillWorkspace carries the ref and nothing else."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-kill "ws-one")
    (should (equal (agent-repl-test-verbs--request :kill)
                   (list :workspace (agent-repl-test-verbs--ref))))))

(ert-deftest agent-repl-verbs-nuke-echoes-the-ref ()
  "NukeWorkspace carries the ref and nothing else."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-nuke "ws-one")
    (should (equal (agent-repl-test-verbs--request :nuke)
                   (list :workspace (agent-repl-test-verbs--ref))))))

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

(ert-deftest agent-repl-verbs-merge-echoes-the-ref ()
  "MergeWorkspace carries the ref and nothing else."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-merge "ws-one")
    (should (equal (agent-repl-test-verbs--request :merge)
                   (list :workspace (agent-repl-test-verbs--ref))))))

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

(ert-deftest agent-repl-verbs-restart-states-force-false ()
  "A graceful restart states `force' explicitly rather than omitting it."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-restart "ws-one" nil)
    (should (equal (plist-get (agent-repl-test-verbs--request :restart) :force) nil))))

(ert-deftest agent-repl-verbs-restart-force-closes-the-composer-on-the-send ()
  "A forced restart's SEND is what closes the composer, not the later push."
  (agent-repl-test-verbs--with nil
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one" t)
    ;; Assert
    (should (equal agent-repl-test-verbs--restart-holds '("ws-one")))))

(ert-deftest agent-repl-verbs-restart-graceful-leaves-the-composer-open ()
  "A graceful restart is SCHEDULED, so it takes no hold on the composer."
  (agent-repl-test-verbs--with nil
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one" nil)
    ;; Assert
    (should (null agent-repl-test-verbs--restart-holds))))

(ert-deftest agent-repl-verbs-restart-refused-gives-the-composer-back ()
  "A refused forced restart bounces nothing, so it holds nothing shut."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:arm :no-session :value nil)))))
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one" t)
    ;; Assert
    (should (null agent-repl-test-verbs--restart-holds))))

(ert-deftest agent-repl-verbs-restart-force-sets-force-true ()
  "A forced restart sets `force'."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-restart "ws-one" t)
    (should (eq (plist-get (agent-repl-test-verbs--request :restart) :force) t))))

;;;; ---- Interrupt -------------------------------------------------------

(ert-deftest agent-repl-verbs-interrupt-targets-the-turn ()
  "`agent-repl-verb-interrupt' aims the Interrupt at the running turn."
  (agent-repl-test-verbs--with
      '((:interrupt . (:response (:arm :success :value (:arm :interrupted-turn :value nil)))))
    (agent-repl-verb-interrupt "ws-one")
    (should (equal (plist-get (agent-repl-test-verbs--request :interrupt) :target)
                   '(:arm :turn :value nil)))))

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
    (agent-repl-verb-restart "ws-one" nil)
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
    (should (agent-repl-test-verbs--messaged-p "create refused: unknown-repository"))))

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
             "create refused: /src/p states no one-shot policy -- write oneshot-completion-directive.md in /src/p/.agent-repl/prompts"))))

(ert-deftest agent-repl-verbs-create-one-shot-policy-missing-is-recorded-as-a-warning ()
  "The refusal is recorded at the WARNING rung, which is what a durable sweep
for refused creates reads."
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
    (should (member "warn" levels))))

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
             "create refused: /src/p states no one-shot policy -- write /src/p/.agent-repl/prompts"))))

(ert-deftest agent-repl-verbs-create-other-refusals-still-fall-through ()
  "The one-shot policy handler CLAIMS only its own arm; every other create
refusal still reaches the generic reporting."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :base-ref-unresolved
                                               :value (:ref "origin/main")))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (should (agent-repl-test-verbs--messaged-p "create refused: base-ref-unresolved"))))

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
             "create refused: the workspace could not be named (timeout, 2 attempts)"))))

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

(ert-deftest agent-repl-verbs-create-naming-failed-is-recorded-as-a-warning ()
  "The refusal is recorded at the WARNING rung, beside the one-shot policy
refusal it sits next to."
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
    (should (member "warn" levels))))

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

(ert-deftest agent-repl-verbs-restart-command-prefix-arg-forces ()
  "A prefix argument makes `agent-repl-restart-workspace' forced."
  (agent-repl-test-verbs--with nil
    (agent-repl-restart-workspace t)
    (should (eq (plist-get (agent-repl-test-verbs--request :restart) :force) t))))

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
    (should (equal (mapcar #'agent-repl-verbs--row-name
                           (agent-repl-verbs--closed-rows roster))
                   '("kid")))))

(ert-deftest agent-repl-verbs-closed-rows-includes-recently-merged ()
  "The recently-merged section is walked after the repo sections."
  (let* ((merged (agent-repl-test-verbs--row
                  :workspace (list :workspace (agent-repl-test-verbs--ref "m" "/tmp/m"))
                  :name (list :text "merged-one") :closed (list :closed t)))
         (roster (agent-repl-test-verbs--roster nil (list merged))))
    (should (equal (mapcar #'agent-repl-verbs--row-name
                           (agent-repl-verbs--closed-rows roster))
                   '("merged-one")))))

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
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string)
               (lambda (&rest _) (error "a dynamic create asks nothing but its prompt"))))
      (agent-repl-create-workspace)
      (let ((standard (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should-not (plist-get standard :name))
        (should-not (plist-get standard :base-ref))))))

(ert-deftest agent-repl-verbs-child-create-command-sends-the-parent ()
  "`agent-repl-create-child-workspace\=' makes the new workspace a CHILD."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-create-child-workspace)
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :parent)
                     (list :workspace (agent-repl-test-verbs--ref)))))))

(ert-deftest agent-repl-verbs-create-command-sends-no-parent ()
  "`SPC TAB n\=' is never a child: it sends no parent at all."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-create-workspace)
      (should-not (plist-get (agent-repl-test-verbs--request :create) :parent)))))

(ert-deftest agent-repl-verbs-fork-command-sets-fork-inside-the-parent ()
  "A fork lives INSIDE the parent: a fork without a parent is unrepresentable."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it")))
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
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
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
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing")))
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
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it"))
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
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission"))
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
               (lambda (_p) (error "a static create asks for no prompt")))
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

(ert-deftest agent-repl-verbs-child-static-create-sends-the-parent-and-the-name ()
  "`agent-repl-create-child-workspace-static\=' names a child, and asks no prompt."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt)
               (lambda (_p) (error "a static create asks for no prompt")))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-child-workspace-static)
      ;; Assert.
      (let ((request (agent-repl-test-verbs--request :create)))
        (should (equal (plist-get request :parent)
                       (list :workspace (agent-repl-test-verbs--ref))))
        (should (equal (plist-get (plist-get (plist-get request :form) :value) :name)
                       "named-one"))))))

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
               (lambda (_p) (error "a named fork asks for no prompt")))
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

(ert-deftest agent-repl-verbs-named-fork-selects-the-created-workspace ()
  "`SPC TAB F\=' stands on the fork, exactly as `SPC TAB f\=' does."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "fork-id" "/tmp/agent-repl-test/fork"))
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'read-string) (lambda (&rest _) "named-fork")))
      ;; Act.
      (agent-repl-fork-workspace-static)
      (agent-repl-test-verbs--tab-arrives "fork-id" "fork-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--selected
                     '("/tmp/agent-repl-test/fork"))))))

;;;; ---- Create: invocation and abandoned-read logging ----

(defconst agent-repl-test-verbs--creation-commands
  '(agent-repl-create-workspace
    agent-repl-create-child-workspace
    agent-repl-create-workspace-static
    agent-repl-create-child-workspace-static
    agent-repl-fork-workspace
    agent-repl-fork-workspace-static
    agent-repl-create-oneshot)
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
                  (agent-repl-create-child-workspace prompt)
                  (agent-repl-create-workspace-static name)
                  (agent-repl-create-child-workspace-static name)
                  (agent-repl-fork-workspace prompt)
                  (agent-repl-fork-workspace-static name)
                  (agent-repl-create-oneshot prompt)))
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
                       (lambda (_p) (signal 'quit nil)))
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

(ert-deftest agent-repl-verbs-create-command-selects-the-created-workspace ()
  "`SPC TAB n' stands on the workspace it just made.
Creating one is a statement about where the user intends to work next, so
the create selects it the same way registering a directory does."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      ;; Act.
      (agent-repl-create-workspace)
      (agent-repl-test-verbs--tab-arrives "new-id" "new-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--selected
                     '("/tmp/agent-repl-test/new"))))))

(ert-deftest agent-repl-verbs-fork-command-selects-the-created-workspace ()
  "`SPC TAB f' stands on the fork: you forked in order to work in the fork."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "fork-id" "/tmp/agent-repl-test/fork"))
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it")))
      ;; Act.
      (agent-repl-fork-workspace)
      (agent-repl-test-verbs--tab-arrives "fork-id" "fork-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--selected
                     '("/tmp/agent-repl-test/fork"))))))

(ert-deftest agent-repl-verbs-create-lands-on-the-panel-not-magit ()
  "The workspace `SPC TAB n\' just made comes up on ITS OWN PANEL.
The recorded selection runs the real landing policy, so a magit status
opened over the new workspace\'s panel shows up here as a magit call."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (dir) (and (equal dir "/tmp/agent-repl-test/new") "new-ws")))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs)))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (dir)
                   (push dir agent-repl-test-verbs--selected)
                   (agent-repl--ws-switch-project-display dir))))
        ;; Act.
        (agent-repl-create-workspace)
        (agent-repl-test-verbs--tab-arrives "new-id" "new-ws")
        ;; Assert.
        (should agent-repl-test-verbs--selected)
        (should-not magit-dirs)))))

(ert-deftest agent-repl-verbs-fork-lands-on-the-panel-not-magit ()
  "A fork comes up on its own panel too: the fork is where you meant to work."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "fork-id" "/tmp/agent-repl-test/fork"))
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it"))
                ((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (dir) (and (equal dir "/tmp/agent-repl-test/fork") "fork-ws")))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs)))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (dir)
                   (push dir agent-repl-test-verbs--selected)
                   (agent-repl--ws-switch-project-display dir))))
        ;; Act.
        (agent-repl-fork-workspace)
        (agent-repl-test-verbs--tab-arrives "fork-id" "fork-ws")
        ;; Assert.
        (should agent-repl-test-verbs--selected)
        (should-not magit-dirs)))))

(ert-deftest agent-repl-verbs-create-does-not-land-before-the-tab-arrives ()
  "A minted ref whose tab has not reached the roster moves the user NOWHERE.
Standing on a directory that is not a workspace yet arms no panels and
lands the user on Doom's empty-project fallback, so the landing waits."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      ;; Act.
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :select t)
      ;; Assert.
      (should-not agent-repl-test-verbs--selected))))

(ert-deftest agent-repl-verbs-a-second-mint-supersedes-a-waiting-landing ()
  "The user stands in ONE place, so a newer mint replaces one still waiting."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "first" "/tmp/a"))
      ;; Act.
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "second" "/tmp/b"))
      ;; Assert.
      (should (equal (plist-get agent-repl-verbs--pending-landing :id) "second")))))

(ert-deftest agent-repl-verbs-create-without-select-stands-still ()
  "A create that did not ask to be selected moves the user NOWHERE.
`select' is off by default because a one-shot is fire-and-forget and must
not steal the user's place."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    ;; Act.
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)))

(ert-deftest agent-repl-verbs-create-select-without-a-dir-is-reported ()
  "A success whose ref carries no dir is REPORTED, never silently skipped.
The decoder already refuses a success without the ref, so a missing dir is
a contract breach and the user is owed the reason nothing came up."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created (list :id "no-dir" :dir ""))
    ;; Act.
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :select t)
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
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
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
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
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
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "   ")))
      (should-error (agent-repl-create-oneshot nil) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-oneshot-prefix-arg-picks-a-model ()
  "A prefix argument threads the chosen model onto the shared facts."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission"))
              ((symbol-function 'agent-repl-verbs--read-model) (lambda () "haiku")))
      (agent-repl-create-oneshot t)
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :model) "haiku")))))

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
               :task (list :sections nil)
               :recently-merged (list :header (list :label (list :text "Recently Merged"))
                                      :rows (list :rows nil))
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
  '(((:arm :build-failed :value (:step "webapp" :detail "tsc: 2 errors" :log "/tmp/b.log"))
     "agent-repl: deploy refused: the webapp build failed, so nothing was deployed: tsc: 2 errors (log: /tmp/b.log)")
    ((:arm :already-deploying :value nil)
     "agent-repl: deploy refused: a deploy is already running; ask again when it ends")
    ((:arm :already-rolling-out :value (:waiting-on ("ws-a" "ws-b")))
     "agent-repl: deploy refused: a handover is already in flight, waiting on ws-a, ws-b")
    ((:arm :joining :value nil)
     "agent-repl: deploy refused: this daemon is a successor still joining a handover")
    ((:arm :service-restart-failed :value (:component :store :detail "exit 78"))
     "agent-repl: deploy refused: store did not come back onto the fresh build: exit 78")
    ((:arm :install-failed :value (:component :daemon :detail "EACCES"))
     "agent-repl: deploy refused: the daemon artifact could not be installed, so nothing was restarted: EACCES"))
  "Every DeployError cause arm and the echo-area line it is reported by.")

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

(ert-deftest agent-repl-verbs-deploy-every-refusal-is-an-error-record ()
  "Every DeployError arm is recorded at ERROR with its arm and fields."
  (dolist (case agent-repl-test-verbs--deploy-errors)
    (agent-repl-test-verbs--with
        `((:deploy . (:response (:arm :error :value (:cause ,(car case))))))
      ;; Arrange
      (let ((errors nil))
        (cl-letf (((symbol-function 'agent-repl--error)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
          ;; Act
          (agent-repl-deploy nil))
        ;; Assert
        (should (equal errors
                       (list (format "elisp.verbs.deploy-refused arm=%S fields=%S"
                                     (plist-get (car case) :arm)
                                     (plist-get (car case) :value)))))))))

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
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "waiting" "/tmp/w"))
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
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "arrives" "/tmp/w")))
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
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "kept" "/tmp/w"))
      ;; Act.
      (agent-repl-verbs--pending-landing-fire))
    ;; Assert.
    (should (equal (plist-get agent-repl-verbs--pending-landing :id) "kept"))))

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
