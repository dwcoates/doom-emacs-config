;;; test-roster.el --- ERT tests for agent-repl roster.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-roster.el -f ert-run-tests-batch-and-exit
;;
;; Fixtures are DECODED roster plists built through the same constructors
;; wire-roster.el produces, so a change to the decoded shape breaks these
;; tests rather than letting them pass against a shape nothing emits.
;;
;; W2-A owns host.el and daemon-link.el; every name from them is stubbed
;; here with `cl-letf', which is also how these tests observe what roster.el
;; asks of them.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixture builders ----

(defun agent-repl-test-roster--row (id name status &rest overrides)
  "Return a decoded `RosterRow' plist for ID, NAME and STATUS.
OVERRIDES is a plist merged over the defaults: `:closed', `:children',
`:attention', `:priority', `:viewed', `:detached-live', `:current', `:dir',
`:availability'
\(an arm keyword, default `:available')."
  (let ((dir (or (plist-get overrides :dir) (concat "/w/" id))))
    (list :workspace (list :workspace (list :id id :dir dir))
          :attention (plist-get overrides :attention)
          :priority (plist-get overrides :priority)
          :viewed (plist-get overrides :viewed)
          :detached-live (plist-get overrides :detached-live)
          :availability (list :arm (or (plist-get overrides :availability) :available)
                              :value nil)
          :name (list :text name)
          :status (list :arm status :value nil)
          :current (list :current (or (plist-get overrides :current) :false))
          :children (plist-get overrides :children)
          :when (list :arm nil :value nil)
          :detail (list :branch nil :parent-branch nil :summary nil)
          :closed (list :closed (if (plist-member overrides :closed)
                                    (plist-get overrides :closed)
                                  :false)))))

(defun agent-repl-test-roster--section (label rows &optional collapsed)
  "Return a decoded `RosterRepoSection' plist labelled LABEL carrying ROWS.
COLLAPSED non-nil makes the daemon hold the section collapsed."
  (list :key (list :repository (list :id (concat "repo-" label) :dir (concat "/r/" label)))
        :header (list :label (list :text label))
        :rows (list :rows rows)
        :fold (list :arm (if collapsed :collapsed :expanded) :value nil)))

(cl-defun agent-repl-test-roster--roster (&key sections merged current)
  "Return a decoded `WorkspaceRoster' plist from SECTIONS, MERGED and CURRENT."
  (list :repository (list :sections sections)
        ;; The task view is deliberately POPULATED in these fixtures: it is
        ;; the same workspaces regrouped, and the walk must ignore it.
        :task (list :sections
                    (mapcar (lambda (s)
                              (list :key (list :task-id "t-1")
                                    :header (list :label (list :text "task")
                                                  :done (list :done :false))
                                    :rows (plist-get s :rows)))
                            sections))
        :recently-merged (list :header (list :label (list :text "Recently Merged"))
                               :rows (list :rows merged))
        :current (and current (list :workspace (list :id current :dir (concat "/w/" current))))))

;;;; ---- Harness ----

(defvar agent-repl-test-roster--subscribed nil
  "Workspaces host.el was asked to subscribe, newest last.")

(defvar agent-repl-test-roster--unsubscribed nil
  "Workspaces host.el was asked to unsubscribe, newest last.")

(defvar agent-repl-test-roster--switched nil
  "Workspaces the roster asked the editor to switch to, newest last.")

(defvar agent-repl-test-roster--created nil
  "Perspectives the roster asked workspace.el to create, newest last.")

(defvar agent-repl-test-roster--killed nil
  "Perspectives the roster asked workspace.el to kill, newest last.")

(defvar agent-repl-test-roster--current-name nil
  "What `agent-repl--ws-current-name' answers during a test.")

(defmacro agent-repl-test-roster--with-editor (&rest body)
  "Run BODY with the editor and host boundaries stubbed and recorded.
persp-mode is absent in batch, so workspace creation is reduced to the
registry write the roster actually depends on; every W2-A name is a stub
whose calls are the observation."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-roster--subscribed nil)
           (agent-repl-test-roster--unsubscribed nil)
           (agent-repl-test-roster--switched nil)
           (agent-repl-test-roster--created nil)
           (agent-repl-test-roster--killed nil)
           (agent-repl-test-roster--current-name nil)
           (agent-repl-roster-view nil)
           (agent-repl-roster--tab-order nil)
           (agent-repl-roster--hidden-tabs nil)
           (agent-repl-roster--rows-by-id (make-hash-table :test 'equal))
           (agent-repl-roster--status-by-id (make-hash-table :test 'equal))
           (agent-repl-roster--viewed-by-id (make-hash-table :test 'equal))
           (agent-repl-roster-viewed-cleared-functions nil)
           (agent-repl-roster-finish-functions nil)
           (agent-repl-roster-status-change-functions nil)
           (agent-repl-roster-update-functions nil)
           (agent-repl-roster-bringup-functions nil)
           (agent-repl-roster--bringup-carry nil)
           (agent-repl-roster--held-id nil)
           (agent-repl-host-last-selected-id nil)
           (agent-repl-host-reselect-pending nil))
       (cl-letf (((symbol-function 'agent-repl--ws-create)
                  (lambda (ws &optional dir)
                    (push ws agent-repl-test-roster--created)
                    (agent-repl--ws-put ws :project-dir (or dir "/w/x"))
                    ws))
                 ((symbol-function 'agent-repl--ws-persp-kill)
                  (lambda (ws) (push ws agent-repl-test-roster--killed) t))
                 ((symbol-function 'agent-repl--ws-persp-exists-p)
                  (lambda (_ws) t))
                 ((symbol-function 'agent-repl--ws-rename-persp)
                  (lambda (_old _new) t))
                 ((symbol-function 'agent-repl--ws-switch)
                  (lambda (ws &rest _) (push ws agent-repl-test-roster--switched) ws))
                 ((symbol-function 'agent-repl--ws-current-name)
                  (lambda () agent-repl-test-roster--current-name))
                 ((symbol-function 'agent-repl--ws-log-routable-p)
                  (lambda (ws)
                    (and (stringp ws) (not (member ws '("main" "none"))))))
                 ((symbol-function 'agent-repl--workspace-log-identity)
                  (lambda (ws)
                    (list :project-dir (format "/tmp/agent-repl-test/%s" ws)
                          :workspace-id (format "id-%s" ws))))
                 ((symbol-function 'agent-repl-host-subscribe)
                  (lambda (_conn ws _ref) (push ws agent-repl-test-roster--subscribed) ws))
                 ((symbol-function 'agent-repl-host-unsubscribe)
                  (lambda (ws) (push ws agent-repl-test-roster--unsubscribed) ws))
                 ((symbol-function 'agent-repl-link-primary)
                  (lambda () 'fake-conn)))
         ,@body))))

(defun agent-repl-test-roster--tabs ()
  "Return the live roster-owned workspace names, in tab order."
  (agent-repl-roster-tab-order))

(ert-deftest agent-repl-test-roster-push-binds-a-request-correlation-id ()
  "One inbound roster push carries one process-global request identity."
  ;; Arrange
  (let (seen)
    (cl-letf (((symbol-function 'agent-repl--next-log-request-id)
               (lambda () "roster-1"))
              ((symbol-function 'agent-repl-roster-apply)
               (lambda (roster)
                 (setq seen (list roster
                                  agent-repl--log-context-workspace
                                  agent-repl--log-context-request-id)))))
      ;; Act
      (agent-repl-roster-on-push '(:arm :roster :value roster-value))
      ;; Assert
      (should (equal seen
                     (list 'roster-value agent-repl--global-log-scope
                           "roster-1"))))))

;;;; ---- The walk ----

(ert-deftest agent-repl-test-roster-walk-visits-sections-in-order ()
  "Sections are walked in the resolver's order; clients do not re-sort."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "beta" (list (agent-repl-test-roster--row "b" "b-row" :ready)))
                                 (agent-repl-test-roster--section
                                  "alpha" (list (agent-repl-test-roster--row "a" "a-row" :ready)))))))
    ;; Act
    (let ((ids (mapcar (lambda (e) (agent-repl-roster-row-id (plist-get e :row)))
                       (agent-repl-roster-walk roster))))
      ;; Assert
      (should (equal ids '("b" "a"))))))

(ert-deftest agent-repl-test-roster-repository-of-answers-the-section-key ()
  "A workspace's repository is ITS SECTION'S OWN key, the imported join token."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "alpha" (list (agent-repl-test-roster--row
                                                 "a" "a-row" :ready)))
                                 (agent-repl-test-roster--section
                                  "beta" (list (agent-repl-test-roster--row
                                                "b" "b-row" :ready)))))))
    ;; Act / Assert
    (should (equal (agent-repl-roster-repository-of "b" roster)
                   '(:id "repo-beta" :dir "/r/beta")))))

(ert-deftest agent-repl-test-roster-repository-of-finds-a-child-row ()
  "A nested row belongs to its section too - the walk is depth-first."
  ;; Arrange
  (let* ((child (agent-repl-test-roster--row "kid" "kid" :ready))
         (parent (agent-repl-test-roster--row "par" "par" :ready :children (list child)))
         (roster (agent-repl-test-roster--roster
                  :sections (list (agent-repl-test-roster--section
                                   "alpha" (list parent))))))
    ;; Act / Assert
    (should (equal (agent-repl-roster-repository-of "kid" roster)
                   '(:id "repo-alpha" :dir "/r/alpha")))))

(ert-deftest agent-repl-test-roster-repository-of-is-nil-for-an-unknown-id ()
  "No section holds the id: nil, so the caller refuses rather than guessing."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "alpha" (list (agent-repl-test-roster--row
                                                 "a" "a-row" :ready)))))))
    ;; Act / Assert
    (should-not (agent-repl-roster-repository-of "nope" roster))))

(ert-deftest agent-repl-test-roster-repository-of-ignores-the-merged-section ()
  "The recently-merged section carries no repository key, so it answers nothing."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :merged (list (agent-repl-test-roster--row "m" "m-row" :ready)))))
    ;; Act / Assert
    (should-not (agent-repl-roster-repository-of "m" roster))))

(ert-deftest agent-repl-test-roster-walk-visits-children-depth-first ()
  "A row precedes its children, which is the contract's render order."
  ;; Arrange
  (let* ((child (agent-repl-test-roster--row "kid" "kid" :ready))
         (parent (agent-repl-test-roster--row "par" "par" :ready :children (list child)))
         (sibling (agent-repl-test-roster--row "sib" "sib" :ready))
         (roster (agent-repl-test-roster--roster
                  :sections (list (agent-repl-test-roster--section
                                   "repo" (list parent sibling))))))
    ;; Act
    (let ((ids (mapcar (lambda (e) (agent-repl-roster-row-id (plist-get e :row)))
                       (agent-repl-roster-walk roster))))
      ;; Assert
      (should (equal ids '("par" "kid" "sib"))))))

(ert-deftest agent-repl-test-roster-walk-puts-recently-merged-last ()
  "The recently-merged rows come after every repository section."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "repo" (list (agent-repl-test-roster--row "a" "a" :ready))))
                 :merged (list (agent-repl-test-roster--row "m" "m" :merged)))))
    ;; Act
    (let ((ids (mapcar (lambda (e) (agent-repl-roster-row-id (plist-get e :row)))
                       (agent-repl-roster-walk roster))))
      ;; Assert
      (should (equal ids '("a" "m"))))))

(ert-deftest agent-repl-test-roster-walk-ignores-the-task-view ()
  "The task view is the same rows regrouped; walking it would double them."
  ;; Arrange — the fixture's task view carries the very same rows.
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "repo" (list (agent-repl-test-roster--row "a" "a" :ready)))))))
    ;; Act / Assert
    (should (equal (length (agent-repl-roster-walk roster)) 1))))

;;;; ---- Reconciliation ----

(ert-deftest agent-repl-test-roster-an-open-row-opens-a-tab ()
  "A row with closed=false gets a tab."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "fix-login" :ready)))))))
      ;; Act
      (agent-repl-roster-apply roster)
      ;; Assert
      (should (equal (agent-repl-test-roster--tabs) '("fix-login"))))))

(ert-deftest agent-repl-test-roster-a-new-tab-carries-its-project-dir ()
  "A new tab carries `:project-dir', taken from the row's ref, on its own.
`agent-repl--ws-create' seeds `:project-dir' only when persp-mode hands
back a real perspective object, so the inner stub here creates the entry
WITHOUT one -- the persp-absent reality.  `:project-dir' is the identity
key the durable log sink, history, the composer's attachment root, panels
and magit all read, and a tab born without one is a `(no repo)' stub for
the rest of its life."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (cl-letf (((symbol-function 'agent-repl--ws-create)
               (lambda (ws &optional _dir)
                 (push ws agent-repl-test-roster--created)
                 (agent-repl--ws-put ws :ws-dir-hash ws)
                 ws)))
      (let ((roster (agent-repl-test-roster--roster
                     :sections (list (agent-repl-test-roster--section
                                      "repo" (list (agent-repl-test-roster--row
                                                    "a" "fix-login" :ready)))))))
        ;; Act
        (agent-repl-roster-apply roster)
        ;; Assert
        (should (equal (agent-repl--ws-get "fix-login" :project-dir) "/w/a"))))))

(ert-deftest agent-repl-test-roster-a-new-tab-subscribes-its-host-stream ()
  "Opening a tab opens that workspace's WatchHostWorkspace subscription."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "fix-login" :ready)))))))
      ;; Act
      (agent-repl-roster-apply roster)
      ;; Assert
      (should (equal agent-repl-test-roster--subscribed '("fix-login"))))))

(ert-deftest agent-repl-test-roster-a-closed-row-gets-no-tab ()
  "A row with closed=true has no tab: merged, closed and killed rows carry it."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row
                                                  "a" "gone" :dead :closed t)))))))
      ;; Act
      (agent-repl-roster-apply roster)
      ;; Assert
      (should (equal (agent-repl-test-roster--tabs) nil)))))

(ert-deftest agent-repl-test-roster-a-row-turning-closed-tears-its-tab-down ()
  "A tab whose row goes closed is torn down."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "fix-login" :ready))))))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row
                                     "a" "fix-login" :merged :closed t))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) nil))))

(ert-deftest agent-repl-test-roster-a-torn-down-tab-unsubscribes-its-host-stream ()
  "Tearing a tab down cancels that workspace's host subscription."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "fix-login" :ready))))))
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil))
    ;; Assert
    (should (equal agent-repl-test-roster--unsubscribed '("fix-login")))))

(ert-deftest agent-repl-test-roster-a-failed-open-leaves-the-later-tabs-open ()
  "A row that cannot open its tab must not cost the rows behind it.
One workspace whose worktree had been deleted aborted a whole cold
start's reconcile on its first row and drew no tabs at all."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (cl-letf (((symbol-function 'agent-repl--ws-create)
               (lambda (ws &optional dir)
                 (when (equal ws "first") (error "no such directory"))
                 (agent-repl--ws-put ws :project-dir (or dir "/w/x"))
                 ws))
              ((symbol-function 'agent-repl--error) #'ignore))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                      (agent-repl-test-roster--row "b" "second" :ready)
                                      (agent-repl-test-roster--row "c" "third" :ready)))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("second" "third")))))

(ert-deftest agent-repl-test-roster-a-failed-open-is-recorded-at-error ()
  "The row that failed is loud: its name, its id, and the error ride a record."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil))
      (cl-letf (((symbol-function 'agent-repl--ws-create)
                 (lambda (_ws &optional _dir) (error "no such directory")))
                ((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "first" :ready)))))))
      ;; Assert
      (should (seq-some
               (lambda (text)
                 (string-search "elisp.roster.row-reconcile-failed: ws=first id=a" text))
               logs)))))

(ert-deftest agent-repl-test-roster-a-failed-open-of-an-unroutable-row-logs-centrally ()
  "A failed row whose workspace owns no sink records against the central scope."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((scopes nil))
      (cl-letf (((symbol-function 'agent-repl--ws-create)
                 (lambda (_ws &optional _dir) (error "no such directory")))
                ((symbol-function 'agent-repl--ws-log-name) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--error)
                 (lambda (ws _fmt &rest _args) (push ws scopes))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "first" :ready)))))))
      ;; Assert
      (should (equal (car (car scopes)) :agent-repl-central)))))

(ert-deftest agent-repl-test-roster-a-failed-open-still-orders-the-survivors ()
  "The tab ORDER after a failed row is the roster's walk order over the rest."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (cl-letf (((symbol-function 'agent-repl--ws-create)
               (lambda (ws &optional dir)
                 (when (equal ws "second") (error "no such directory"))
                 (agent-repl--ws-put ws :project-dir (or dir "/w/x"))
                 ws))
              ((symbol-function 'agent-repl--error) #'ignore))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                      (agent-repl-test-roster--row "b" "second" :ready)
                                      (agent-repl-test-roster--row "c" "third" :ready)))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("first" "third")))))

(ert-deftest agent-repl-test-roster-a-failed-teardown-does-not-abort-the-walk ()
  "A teardown that signals outside its own guards leaves the rest to run."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready))))))
    (let (tombstoned)
      (cl-letf (((symbol-function 'agent-repl--ws-del)
                 (lambda (ws)
                   (when (equal ws "first") (error "registry write failed"))
                   (push ws tombstoned)
                   (agent-repl--ws-put ws :killed-at (current-time))))
                ((symbol-function 'agent-repl--error) #'ignore))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil)))
      ;; Assert: the walk still reached the second row's tombstone.
      (should (member "second" tombstoned)))))

(ert-deftest agent-repl-test-roster-a-signalling-persp-kill-does-not-abort-the-walk ()
  "A persp kill that signals leaves the REST of the teardown walk to run.
The kill runs against a live frame, so it can signal on something the
roster knows nothing about; escaping here would abort the reconcile
mid-list and leave later tabs describing a roster nobody finished
reading."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready))))))
    ;; Act
    (cl-letf (((symbol-function 'agent-repl--ws-persp-kill)
               (lambda (ws)
                 (push ws agent-repl-test-roster--killed)
                 (when (equal ws "first")
                   (error "Window is dedicated to `*agent-panel-input-first*'")))))
      (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil)))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) nil))))

(ert-deftest agent-repl-test-roster-a-signalling-persp-kill-is-recorded ()
  "The failed kill is loud: the workspace and the error ride the record."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "first" :ready))))))
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (_ws) (error "Window is dedicated to `*agent-panel-input-first*'")))
                ((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil)))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "elisp.roster.tab-teardown-persp-kill-failed" text))
                        logs)))))

(ert-deftest agent-repl-test-roster-teardown-of-the-current-tab-lands-on-a-survivor ()
  "A roster teardown of the workspace the user STANDS ON lands them on a
survivor -- through the one teardown order every teardown uses, not a
second one of the roster's own.
Without it the frame kept whatever persp-mode dropped it in and the main
area came up on the fallback buffer."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready))))))
    (setq agent-repl-test-roster--current-name "first")
    (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
              ((symbol-function 'agent-repl--ws-all-names) (lambda () '("second")))
              ((symbol-function 'agent-repl--ws-list-names) (lambda () '("second"))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "b" "second" :ready)))))))
    ;; Assert
    (should (equal (car agent-repl-test-roster--switched) "second"))))

(ert-deftest agent-repl-test-roster-merged-current-tab-lands-on-the-previously-selected ()
  "An IMPLICIT close -- the daemon closing a workspace after its merge lands --
of the tab the user stands on lands them on the workspace selected before
it, not the first tab: one rule for every close (owner ruling, 2026-09-30)."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready)
                                    (agent-repl-test-roster--row "c" "third" :ready))))))
    (setq agent-repl-test-roster--current-name "third")
    (let ((agent-repl--workspace-history '("third" "second" "first")))
      (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
                ((symbol-function 'agent-repl--ws-all-names)
                 (lambda () '("first" "second" "third")))
                ((symbol-function 'agent-repl--ws-list-names)
                 (lambda () '("first" "second" "third"))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                        (agent-repl-test-roster--row "b" "second" :ready))))
          :merged (list (agent-repl-test-roster--row "c" "third" :merged :closed t))))))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("second")))))

;;; --- The one selection-recency order

(defmacro agent-repl-test-roster--with-instants (instants history &rest body)
  "Run BODY with INSTANTS (alist name -> ms) as the roster's durable
last-selected instants and HISTORY as this session's switches."
  (declare (indent 2))
  `(let ((agent-repl--workspace-history ,history)
         (instants ,instants))
     (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws)
                (lambda (ws)
                  (let ((ms (cdr (assoc ws instants))))
                    (and ms (list :last-selected (list :at-ms ms)))))))
       ,@body)))

(ert-deftest agent-repl-test-roster-recency-order-puts-session-history-first ()
  "This session's switches outrank a newer durable instant from the roster."
  (agent-repl-test-roster--with-instants '(("a" . 100) ("b" . 900)) '("a")
    (should (equal (agent-repl-roster-selection-recency-order '("b" "a"))
                   '("a" "b")))))

(ert-deftest agent-repl-test-roster-recency-order-orders-the-rest-by-the-durable-instant ()
  "Workspaces the history does not hold order newest durable instant first."
  (agent-repl-test-roster--with-instants '(("a" . 100) ("b" . 900) ("c" . 500)) nil
    (should (equal (agent-repl-roster-selection-recency-order '("a" "b" "c"))
                   '("b" "c" "a")))))

(ert-deftest agent-repl-test-roster-recency-order-keeps-the-never-selected-in-given-order ()
  "Never-selected workspaces come last, in the order the caller gave them."
  (agent-repl-test-roster--with-instants '(("s" . 100)) nil
    (should (equal (agent-repl-roster-selection-recency-order '("y" "s" "x"))
                   '("s" "y" "x")))))

(ert-deftest agent-repl-test-roster-recency-order-ignores-history-outside-names ()
  "A history entry that is not a candidate (closed, departing) adds nothing."
  (agent-repl-test-roster--with-instants nil '("gone" "b")
    (should (equal (agent-repl-roster-selection-recency-order '("a" "b"))
                   '("b" "a")))))

(ert-deftest agent-repl-test-roster-last-selected-ms-reads-the-row ()
  "The durable instant is the row's `last_selected'."
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (plist-put (agent-repl-test-roster--row "a" "first" :ready)
                                               :last-selected '(:at-ms 42)))))))
    (should (equal (agent-repl-roster-last-selected-ms "first") 42))))

(ert-deftest agent-repl-test-roster-last-selected-ms-is-nil-for-a-never-selected-row ()
  "A row with no `last_selected' was never selected."
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready))))))
    (should-not (agent-repl-roster-last-selected-ms "first"))))

(ert-deftest agent-repl-test-roster-merged-current-tab-lands-by-the-durable-instant-after-restart ()
  "After an Emacs restart the session history holds only the closing tab, and
the landing is the workspace the roster says was selected most recently."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (plist-put (agent-repl-test-roster--row "a" "first" :ready)
                                               :last-selected '(:at-ms 100))
                                    (plist-put (agent-repl-test-roster--row "b" "second" :ready)
                                               :last-selected '(:at-ms 900))
                                    (plist-put (agent-repl-test-roster--row "c" "third" :ready)
                                               :last-selected '(:at-ms 1000)))))))
    (setq agent-repl-test-roster--current-name "third")
    (let ((agent-repl--workspace-history '("third")))
      (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
                ((symbol-function 'agent-repl--ws-all-names)
                 (lambda () '("first" "second" "third")))
                ((symbol-function 'agent-repl--ws-list-names)
                 (lambda () '("first" "second" "third"))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (plist-put (agent-repl-test-roster--row "a" "first" :ready)
                                                   :last-selected '(:at-ms 100))
                                        (plist-put (agent-repl-test-roster--row "b" "second" :ready)
                                                   :last-selected '(:at-ms 900)))))
          :merged (list (plist-put (agent-repl-test-roster--row "c" "third" :merged :closed t)
                                   :last-selected '(:at-ms 1000)))))))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("second")))))

(ert-deftest agent-repl-test-roster-teardown-goes-through-land-then-kill ()
  "The roster's tab teardown uses the ONE teardown order
\(`agent-repl--ws-land-then-kill'), the same one
`agent-repl--kill-one-workspace' uses -- never an order of its own."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready))))))
    (let (torn-down)
      (cl-letf (((symbol-function 'agent-repl--ws-land-then-kill)
                 (lambda (ws) (push ws torn-down) t)))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "b" "second" :ready)))))))
      ;; Assert
      (should (equal torn-down '("first"))))))

(ert-deftest agent-repl-test-roster-teardown-of-the-current-tab-lands-before-the-kill ()
  "A roster teardown of the tab the user STANDS ON switches to the survivor
BEFORE the persp is killed, so the kill never runs on the current persp."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready))))))
    (setq agent-repl-test-roster--current-name "first")
    (let ((order nil))
      (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
                ((symbol-function 'agent-repl--ws-all-names) (lambda () '("first" "second")))
                ((symbol-function 'agent-repl--ws-list-names) (lambda () '("first" "second")))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (push (list :switch ws) order)))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push (list :kill ws) order) t)))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "b" "second" :ready)))))))
      ;; Assert
      (should (equal (nreverse order) '((:switch "second") (:kill "first")))))))

(ert-deftest agent-repl-test-roster-teardown-of-another-tab-does-not-move-the-user ()
  "Tearing down a tab the user is NOT standing on leaves them where they are:
the landing is for the workspace that vanished under them, nothing else."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "first" :ready)
                                    (agent-repl-test-roster--row "b" "second" :ready))))))
    (setq agent-repl-test-roster--current-name "second")
    (setq agent-repl-test-roster--switched nil)
    (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
              ((symbol-function 'agent-repl--ws-all-names) (lambda () '("second")))
              ((symbol-function 'agent-repl--ws-list-names) (lambda () '("second"))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "b" "second" :ready)))))))
    ;; Assert
    (should-not agent-repl-test-roster--switched)))

(ert-deftest agent-repl-test-roster-teardown-with-no-survivor-is-reported ()
  "The last tab torn down has nowhere to land: no switch is made, and the
frame left on the fallback buffer is REPORTED rather than silently taken
for a landing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((warnings nil))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "first" :ready))))))
      (setq agent-repl-test-roster--current-name "first")
      (setq agent-repl-test-roster--switched nil)
      (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
                ((symbol-function 'agent-repl--ws-all-names) (lambda () nil))
                ((symbol-function 'agent-repl--ws-list-names) (lambda () nil))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil)))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "NO surviving workspace to land in" text))
                        warnings))
      (should-not agent-repl-test-roster--switched))))

(ert-deftest agent-repl-test-roster-teardown-is-idempotent ()
  "A second push with the row already gone tears nothing down twice.
CloseWorkspace's own success tears the tab down too, so the roster push
that follows must not care which of the two got there first."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "fix-login" :ready))))))
    (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil))
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--roster :sections nil))
    ;; Assert
    (should (equal (length agent-repl-test-roster--unsubscribed) 1))))

(ert-deftest agent-repl-test-roster-tab-order-follows-the-walk ()
  "Tab order is the walk order strictly; there is no local ordering."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                    (agent-repl-test-roster--row "b" "two" :ready))))))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "two" :ready)
                                    (agent-repl-test-roster--row "a" "one" :ready))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("two" "one")))))

(ert-deftest agent-repl-test-roster-a-renamed-row-renames-its-tab ()
  "The ref id is the identity, so a changed name renames rather than adds."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "old-name" :ready))))))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "new-name" :ready))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("new-name")))))

(ert-deftest agent-repl-test-roster-a-rename-re-keys-host-state ()
  "Host state is keyed on the name too, so the NEW name answers with the ref."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-host--by-name (make-hash-table :test 'equal)))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "old-name" :ready))))))
      (agent-repl-host--put "old-name" :ref (list :id "a" :dir "/w/a"))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "new-name" :ready))))))
      ;; Assert
      (should (equal (agent-repl-host-ref "new-name") (list :id "a" :dir "/w/a"))))))

(ert-deftest agent-repl-test-roster-a-rename-drops-the-old-host-key ()
  "The old name stops answering, so no push updates a dead key's state."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-host--by-name (make-hash-table :test 'equal)))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "old-name" :ready))))))
      (agent-repl-host--put "old-name" :ref (list :id "a" :dir "/w/a"))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "new-name" :ready))))))
      ;; Assert
      (should (null (agent-repl-host-ref "old-name"))))))

(ert-deftest agent-repl-test-roster-a-rename-onto-a-tombstone-keeps-the-old-tab ()
  "A collision with a tombstoned name is refused whole: the tab keeps its name."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "gone" :ready)
                                    (agent-repl-test-roster--row "b" "stays" :ready))))))
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "stays" :ready))))))
    ;; Act -- "gone" is now tombstoned; rename b onto it
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "gone" :ready))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("stays")))))

(ert-deftest agent-repl-test-roster-a-reopened-row-gets-a-live-tab-again ()
  "A row that closed and came back is LIVE again, not left tombstoned.
`--ws-del' tombstones, so the reopen must clear the stamp through
`agent-repl--ws-revive' or `closed = false' never ensures a tab."
  ;; Arrange -- open the row, then close it so its name is tombstoned.
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "back" :ready))))))
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section "repo" nil))))
    (should (agent-repl--ws-tombstoned-p "back"))
    ;; Act -- the SAME id reopens.
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "back" :ready))))))
    ;; Assert.
    (should (equal (agent-repl--ws-by-ref-id "a") "back"))))

(ert-deftest agent-repl-test-roster-a-reopened-row-keeps-its-previous-close-on-record ()
  "The reopen revives the name without erasing `:last-killed-at'."
  ;; Arrange.
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "back" :ready))))))
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section "repo" nil))))
    ;; Act.
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "back" :ready))))))
    ;; Assert.
    (should (agent-repl--ws-get "back" :last-killed-at))))

(ert-deftest agent-repl-test-roster-a-refused-rename-does-not-abort-the-walk ()
  "The `user-error' must not escape the push handler mid-reconcile."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "gone" :ready)
                                    (agent-repl-test-roster--row "b" "stays" :ready))))))
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "stays" :ready))))))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "gone" :ready)
                                    (agent-repl-test-roster--row "c" "later" :ready))))))
    ;; Assert -- the row AFTER the refused rename was still opened
    (should (member "later" (agent-repl-test-roster--tabs)))))

(ert-deftest agent-repl-test-roster-a-refused-rename-leaves-host-state-alone ()
  "Refused whole: host state is not half-moved either."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-host--by-name (make-hash-table :test 'equal)))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "gone" :ready)
                                      (agent-repl-test-roster--row "b" "stays" :ready))))))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "b" "stays" :ready))))))
      (agent-repl-host--put "stays" :ref (list :id "b" :dir "/w/b"))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "b" "gone" :ready))))))
      ;; Assert
      (should (equal (agent-repl-host-ref "stays") (list :id "b" :dir "/w/b"))))))

(ert-deftest agent-repl-test-roster-a-refused-rename-is-recorded ()
  "Refusals are loud: the reason rides the record."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "gone" :ready)
                                      (agent-repl-test-roster--row "b" "stays" :ready))))))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "b" "stays" :ready))))))
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "b" "gone" :ready)))))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "elisp.roster.tab-rename-refused" text))
                        logs)))))

(ert-deftest agent-repl-test-roster-a-rename-opens-no-second-tab ()
  "A rename leaves exactly one tab: the same workspace, whatever its name."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "old-name" :ready))))))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "new-name" :ready))))))
    ;; Assert
    (should (equal (length agent-repl-test-roster--created) 1))))

(ert-deftest agent-repl-test-roster-colliding-names-take-the-repo-suffix ()
  "Names collide across repos and ids do not, so a collision is suffixed."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "alpha" (list (agent-repl-test-roster--row "a" "fix" :ready)))
                      (agent-repl-test-roster--section
                       "beta" (list (agent-repl-test-roster--row "b" "fix" :ready))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("fix·alpha" "fix·beta")))))

(ert-deftest agent-repl-test-roster-an-uncontested-name-takes-no-suffix ()
  "Only a name more than one open row carries is disambiguated."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "alpha" (list (agent-repl-test-roster--row "a" "fix" :ready)))
                      (agent-repl-test-roster--section
                       "beta" (list (agent-repl-test-roster--row "b" "other" :ready))))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("fix" "other")))))

(ert-deftest agent-repl-test-roster-a-recently-merged-row-still-open-keeps-its-tab ()
  "closed=false is the whole membership rule, recently-merged included."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :merged (list (agent-repl-test-roster--row "m" "landed" :merged :closed :false))))
    ;; Assert
    (should (equal (agent-repl-test-roster--tabs) '("landed")))))

;;;; ---- The current workspace (R8) ----

(ert-deftest agent-repl-test-roster-a-daemon-originated-current-switches-tabs ()
  "A `current' Emacs did not originate is a tab-switch request (R8)."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                    (agent-repl-test-roster--row "b" "two" :ready))))
      :current "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("two")))))

(ert-deftest agent-repl-test-roster-emacs-own-selection-switches-nothing ()
  "A `current' matching Emacs's own last selection is not a request.
Re-selection is idempotent, which is what keeps this from looping."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one"
          agent-repl-host-last-selected-id "b")
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                    (agent-repl-test-roster--row "b" "two" :ready))))
      :current "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched nil))))

(ert-deftest agent-repl-test-roster-a-current-during-a-relink-switches-nothing ()
  "A roster push landing mid re-registration must not move the frame.
The daemon that just relaunched stamped `current' on whichever workspace
re-registered first, which is a walk order and not the user's choice;
host.el is re-asserting the real selection at that moment."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one"
          agent-repl-host-reselect-pending "/w/one")
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                    (agent-repl-test-roster--row "b" "two" :ready))))
      :current "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched nil))))

(ert-deftest agent-repl-test-roster-a-current-after-a-relink-switches-again ()
  "Once the re-select is acknowledged the frame follows `current' again.
A suppression that outlived its re-select would deafen Emacs to the
user's next sidebar click."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one"
          agent-repl-host-reselect-pending nil)
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                    (agent-repl-test-roster--row "b" "two" :ready))))
      :current "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("two")))))

(ert-deftest agent-repl-test-roster-a-current-already-selected-switches-nothing ()
  "The workspace already on screen is not switched to again."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "two")
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "two" :ready))))
      :current "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched nil))))

(ert-deftest agent-repl-test-roster-no-current-switches-nothing ()
  "An unset `current' means there is no selection, not a switch to nowhere."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "b" "two" :ready))))))
    ;; Assert
    (should (equal agent-repl-test-roster--switched nil))))

;;;; ---- The finish edge ----

(defmacro agent-repl-test-roster--recording-finishes (var &rest body)
  "Run BODY with the finish hook recording workspaces onto VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (add-hook 'agent-repl-roster-finish-functions
               (lambda (ws) (push ws ,var)))
     ,@body))

(ert-deftest agent-repl-test-roster-running-to-settled-fires-the-finish-edge ()
  "thinking -> done is the finish edge."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-finishes fired
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :done))))))
      ;; Assert
      (should (equal fired '("one"))))))

(ert-deftest agent-repl-test-roster-the-finish-edge-fires-once-per-edge ()
  "A settled row pushed again is not a second edge."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-finishes fired
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking))))))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :done))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :done))))))
      ;; Assert
      (should (equal (length fired) 1)))))

(ert-deftest agent-repl-test-roster-permission-to-thinking-is-no-finish-edge ()
  "A permission ask returning to thinking is a move WITHIN the running set."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-finishes fired
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :permission))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking))))))
      ;; Assert
      (should (equal fired nil)))))

(ert-deftest agent-repl-test-roster-a-first-sighting-is-no-finish-edge ()
  "A row seen for the first time already settled has crossed nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-finishes fired
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal fired nil)))))



;;;; ---- The viewed marker ----

(ert-deftest agent-repl-test-roster-row-viewed-p-when-marker-present ()
  "A row carrying `RosterRowViewed' reads as viewed."
  (should (agent-repl-roster-row-viewed-p
           (agent-repl-test-roster--row "a" "one" :ready :viewed '(:viewed t)))))

(ert-deftest agent-repl-test-roster-row-viewed-p-when-marker-absent ()
  "A row without the marker reads as not viewed."
  (should-not (agent-repl-roster-row-viewed-p
               (agent-repl-test-roster--row "a" "one" :ready))))

(ert-deftest agent-repl-test-roster-viewed-for-ws-after-a-push ()
  "After a push, a workspace's marker is its current row's `:viewed'."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready :viewed '(:viewed t)))))))
    ;; Assert
    (should (equal (agent-repl-roster-viewed-for-ws "one") '(:viewed t)))))

(ert-deftest agent-repl-test-roster-viewed-for-ws-before-any-push ()
  "Before any push carried its row, a workspace has no marker."
  (agent-repl-test-roster--with-editor
    (should-not (agent-repl-roster-viewed-for-ws "one"))))

(ert-deftest agent-repl-test-roster-row-detached-live-p-when-marker-present ()
  "A row carrying `RosterRowDetachedLive' reads as having live detached work."
  (should (agent-repl-roster-row-detached-live-p
           (agent-repl-test-roster--row "a" "one" :done :detached-live t))))

(ert-deftest agent-repl-test-roster-row-detached-live-p-when-marker-absent ()
  "A row without the marker reads as having no live detached work."
  (should-not (agent-repl-roster-row-detached-live-p
               (agent-repl-test-roster--row "a" "one" :done))))

(ert-deftest agent-repl-test-roster-detached-live-for-ws-after-a-push ()
  "After a push, a workspace's detached-work fact is its current row's."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :done :detached-live t))))))
    ;; Assert
    (should (agent-repl-roster-detached-live-for-ws "one"))))

(ert-deftest agent-repl-test-roster-detached-live-for-ws-before-any-push ()
  "Before any push carried its row, a workspace has no detached-work fact."
  (agent-repl-test-roster--with-editor
    (should-not (agent-repl-roster-detached-live-for-ws "one"))))

(defmacro agent-repl-test-roster--recording-viewed-clears (var &rest body)
  "Run BODY with the viewed-cleared hook recording each WS onto VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (add-hook 'agent-repl-roster-viewed-cleared-functions
               (lambda (ws) (push ws ,var)))
     ,@body))

(ert-deftest agent-repl-test-roster-a-dropped-viewed-marker-announces-a-clear ()
  "A row whose viewed marker went present->absent announces a clear."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-viewed-clears cleared
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready :viewed '(:viewed t)))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal cleared '("one"))))))

(ert-deftest agent-repl-test-roster-a-restated-viewed-marker-is-no-clear ()
  "A re-push restating the marker cleared nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-viewed-clears cleared
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready :viewed '(:viewed t)))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready :viewed '(:viewed t)))))))
      ;; Assert
      (should (equal cleared nil)))))

(ert-deftest agent-repl-test-roster-an-unviewed-first-sighting-is-no-clear ()
  "A row first seen without the marker had nothing to clear."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-viewed-clears cleared
      
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal cleared nil)))))

;;;; ---- The status change ----

(defmacro agent-repl-test-roster--recording-status-changes (var &rest body)
  "Run BODY with the status-change hook recording (WS PREVIOUS CURRENT) onto VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (add-hook 'agent-repl-roster-status-change-functions
               (lambda (ws previous current) (push (list ws previous current) ,var)))
     ,@body))

(ert-deftest agent-repl-test-roster-a-changed-arm-announces-a-status-change ()
  "A row whose arm moved announces the change, whatever moved it."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-status-changes changed
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking))))))
      ;; Assert
      (should (equal changed '(("one" :ready :thinking)))))))

(ert-deftest agent-repl-test-roster-a-restated-arm-is-no-status-change ()
  "A re-push restating the same arm changed nothing and announces nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-status-changes changed
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal changed nil)))))

(ert-deftest agent-repl-test-roster-a-first-sighting-is-no-status-change ()
  "A row seen for the first time has moved away from nothing.
The daemon applies the same rule to its own copy of this edge, so the two
cannot disagree about what counts as new activity."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-status-changes changed
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal changed nil)))))

(ert-deftest agent-repl-test-roster-a-settling-arm-announces-a-status-change-too ()
  "The announcement is not the finish edge: EVERY changed arm announces.
A finish edge is one KIND of status change, and a reaction that only ran
on finishes would miss a workspace going back to work."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-status-changes changed
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :done))))))
      ;; Assert
      (should (equal changed '(("one" :thinking :done)))))))

(ert-deftest agent-repl-test-roster-status-changes-are-announced-per-workspace ()
  "Two rows changing in one push announce once each, not once for the push."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-status-changes changed
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                      (agent-repl-test-roster--row "b" "two" :ready))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking)
                                      (agent-repl-test-roster--row "b" "two" :done))))))
      ;; Assert
      (should (equal (sort (mapcar #'car changed) #'string<) '("one" "two"))))))

(ert-deftest agent-repl-test-roster-idle-async-settles-the-turn ()
  "idle_async is SETTLED: no foreground turn runs, only detached work."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-finishes fired
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :thinking))))))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :idle-async))))))
      ;; Assert
      (should (equal fired '("one"))))))

;;;; ---- Invariants ----

(ert-deftest agent-repl-test-roster-a-duplicate-ref-id-drops-the-push ()
  "Two rows sharing one id make the tab identity ambiguous: drop it whole."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                                 (agent-repl-test-roster--row "a" "two" :ready)))))))
      ;; Act
      (should (null (agent-repl-roster-apply roster)))
      ;; Assert
      (should (equal (agent-repl-test-roster--tabs) nil)))))

(ert-deftest agent-repl-test-roster-a-dropped-push-does-not-become-the-view ()
  "A refused push leaves the last good view standing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((good (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      (agent-repl-roster-apply good)
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "b" "x" :ready)
                                      (agent-repl-test-roster--row "b" "y" :ready))))))
      ;; Assert
      (should (eq agent-repl-roster-view good)))))

;;;; ---- The view and its hooks ----

(ert-deftest agent-repl-test-roster-an-accepted-push-becomes-the-view ()
  "The view is whole-replaced by every accepted push."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Act
      (agent-repl-roster-apply roster)
      ;; Assert
      (should (eq agent-repl-roster-view roster)))))

(ert-deftest agent-repl-test-roster-an-accepted-push-runs-the-update-hook ()
  "Handlers see the roster the push carried."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((seen nil)
          (roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      (add-hook 'agent-repl-roster-update-functions (lambda (r) (setq seen r)))
      ;; Act
      (agent-repl-roster-apply roster)
      ;; Assert
      (should (eq seen roster)))))

(ert-deftest agent-repl-test-roster-status-for-ws-answers-the-rows-arm ()
  "A workspace's status is its row's arm, looked up by ref id."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :compacting))))))
    ;; Act / Assert
    (should (eq (agent-repl-roster-status-for-ws "one") :compacting))))

(ert-deftest agent-repl-test-roster-status-for-an-unknown-workspace-is-nil ()
  "A workspace the roster has not spoken about has no status to report."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act / Assert
    (should (null (agent-repl-roster-status-for-ws "never-heard-of")))))

;;;; ---- The subscription ----

(ert-deftest agent-repl-test-roster-link-up-subscribes ()
  "The link coming up is what subscribes the roster stream."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((conns nil)
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (conn _on-push _on-close &optional _on-open) (push conn conns) 'fake-stream)))
        ;; Act
        (agent-repl-roster-on-link-up 'conn-1)
        ;; Assert
        (should (equal conns '(conn-1)))))))

(ert-deftest agent-repl-test-roster-resubscribing-cancels-the-prior-stream ()
  "A second subscribe replaces the standing stream rather than stacking one."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((cancelled nil)
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (_conn _on-push _on-close &optional _on-open) 'fake-stream))
                ((symbol-function 'agent-repl-connect-stream-cancel)
                 (lambda (s) (push s cancelled))))
        (agent-repl-roster-subscribe 'conn-1)
        ;; Act
        (agent-repl-roster-subscribe 'conn-2)
        ;; Assert
        (should (equal cancelled '(fake-stream)))))))

(ert-deftest agent-repl-test-roster-promotion-resubscribes-on-the-successor ()
  "A promotion fires no up hooks, so THIS is what keeps the tabs reconciling."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((conns nil)
          (successor (agent-repl-connect-open "127.0.0.1:9100"))
          (old (agent-repl-connect-open "127.0.0.1:9001"))
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (conn _on-push _on-close &optional _on-open)
                   (push conn conns) 'fake-stream)))
        ;; Act
        (agent-repl-roster-on-link-promote old successor)
        ;; Assert
        (should (equal conns (list successor)))))))

(ert-deftest agent-repl-test-roster-promotion-resubscription-is-logged ()
  "The re-subscription is news: it is the repair of a stream a rollout killed."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil)
          (successor (agent-repl-connect-open "127.0.0.1:9100"))
          (old (agent-repl-connect-open "127.0.0.1:9001"))
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (_conn _on-push _on-close &optional _on-open) 'fake-stream))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-on-link-promote old successor)
        ;; Assert
        (should (seq-some (lambda (text)
                            (string-search "elisp.roster.resubscribed-on-promotion" text))
                          logs))))))

(ert-deftest agent-repl-test-roster-promotion-cancels-the-stream-on-the-old-conn ()
  "The prior stream is replaced, never stacked: one roster stream, always."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((cancelled nil)
          (successor (agent-repl-connect-open "127.0.0.1:9100"))
          (old (agent-repl-connect-open "127.0.0.1:9001"))
          (agent-repl-roster--stream 'old-stream))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (_conn _on-push _on-close &optional _on-open) 'fake-stream))
                ((symbol-function 'agent-repl-connect-stream-cancel)
                 (lambda (stream) (push stream cancelled))))
        ;; Act
        (agent-repl-roster-on-link-promote old successor)
        ;; Assert
        (should (equal cancelled '(old-stream)))))))

(ert-deftest agent-repl-test-roster-registers-on-the-promote-hook ()
  "The registration IS the fix: without it a rollout leaves no roster stream."
  ;; Act / Assert
  (should (memq #'agent-repl-roster-on-link-promote
                (default-value 'agent-repl-link-promote-functions))))

(ert-deftest agent-repl-test-roster-link-down-keeps-the-last-view ()
  "The last view is the newest thing anyone knows; blanking it would lie."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-roster--stream 'fake-stream))
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Act
      (agent-repl-roster-on-link-down 'conn-1)
      ;; Assert
      (should agent-repl-roster-view))))

(ert-deftest agent-repl-test-roster-link-down-forgets-the-stream ()
  "The stream is gone with the link; the next link-up subscribes a fresh one."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-roster--stream 'fake-stream))
      ;; Act
      (agent-repl-roster-on-link-down 'conn-1)
      ;; Assert
      (should (null agent-repl-roster--stream)))))


;;;; ---- Subscription acceptance ----

(ert-deftest agent-repl-test-roster-subscribe-alone-is-not-subscribed ()
  "A spawned roster stream that was never accepted delivers no tabs."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil)
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (_conn _on-push _on-close &optional _on-open) 'fake-stream))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-subscribe 'conn-1))
      ;; Assert
      (should-not (seq-some (lambda (text)
                              (string-search "elisp.roster.subscribed" text))
                            logs)))))

(ert-deftest agent-repl-test-roster-acceptance-logs-subscribed ()
  "The daemon accepting the roster watch is what records it as subscribed."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil)
          (accepted nil)
          (conn (agent-repl-connect-open "127.0.0.1:9001"))
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (_conn _on-push _on-close &optional on-open)
                   (setq accepted on-open) 'fake-stream))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        (agent-repl-roster-subscribe conn)
        ;; Act
        (funcall accepted))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "elisp.roster.subscribed" text))
                        logs)))))

(ert-deftest agent-repl-test-roster-acceptance-names-the-method ()
  "The accepted record says WHICH subscription stands."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((logs nil)
          (accepted nil)
          (conn (agent-repl-connect-open "127.0.0.1:9001"))
          (agent-repl-roster--stream nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (_conn _on-push _on-close &optional on-open)
                   (setq accepted on-open) 'fake-stream))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        (agent-repl-roster-subscribe conn)
        ;; Act
        (funcall accepted))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "method=\"WatchWorkspaceRoster\"" text))
                        logs)))))

;;;; ---- The roster follows the live daemon ----
;;
;; Regression, 2026-09-27: a daemon exited under a standing roster stream
;; (`WatchWorkspaceRoster ... no-end-frame') and nothing re-subscribed it
;; unless the link itself went down or was promoted.

(defmacro agent-repl-test-roster--with-stream (live &rest body)
  "Run BODY with the stubbed link answering LIVE and a roster stream recorder.
Each subscribe answers a fresh stream record `(:conn C :on-push F
:on-close F :on-open F)', newest first in `streams'."
  (declare (indent 1))
  `(let ((streams nil)
         (agent-repl-roster--stream nil)
         (agent-repl-roster--accepted nil)
         (agent-repl-roster--ending-stream nil))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () ,live))
               ((symbol-function 'agent-repl-connect-stream-cancel) #'ignore)
               ((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                (lambda (conn on-push on-close &optional on-open)
                  (let ((record (list :conn conn :on-push on-push
                                      :on-close on-close :on-open on-open)))
                    (push record streams)
                    record))))
       ,@body)))

(ert-deftest agent-repl-test-roster-an-accepted-stream-lost-resubscribes-on-the-live-daemon ()
  "A stream the daemon accepted and then dropped is re-subscribed on the live one."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175")))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe dead)
        (funcall (plist-get (car streams) :on-open))
        ;; Act
        (funcall (plist-get (car streams) :on-close)
                 '(:error (:kind :no-end-frame :message "producer closed without an end frame")))
        ;; Assert
        (should (eq (plist-get agent-repl-roster--stream :conn) live))))))

(ert-deftest agent-repl-test-roster-a-resubscription-on-loss-is-recorded-at-info ()
  "The loss is ERROR already; the re-subscription that answers it is INFO."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175"))
          (logs nil))
      (agent-repl-test-roster--with-stream live
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
          (agent-repl-roster-subscribe dead)
          (funcall (plist-get (car streams) :on-open))
          ;; Act
          (funcall (plist-get (car streams) :on-close) '(:ended)))
        ;; Assert
        (should (member "elisp.roster.resubscribed-on-loss address=\"127.0.0.1:58175\"" logs))))))

(ert-deftest agent-repl-test-roster-a-stream-lost-with-no-link-waits-for-the-link-up ()
  "With no daemon reachable nothing is dialed; the link-up edge subscribes."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((dead (agent-repl-connect-open "127.0.0.1:61043")))
      (agent-repl-test-roster--with-stream nil
        (agent-repl-roster-subscribe dead)
        (funcall (plist-get (car streams) :on-open))
        ;; Act
        (funcall (plist-get (car streams) :on-close) '(:ended))
        ;; Assert
        (should (and (= (length streams) 1)
                     (null agent-repl-roster--stream)))))))

(ert-deftest agent-repl-test-roster-a-stream-never-accepted-is-not-redialed-from-its-close ()
  "A stream that died before acceptance met a daemon that is not answering."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((live (agent-repl-connect-open "127.0.0.1:58175")))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe live)
        ;; Act
        (funcall (plist-get (car streams) :on-close) '(:error (:kind :transport)))
        ;; Assert
        (should (= (length streams) 1))))))

(ert-deftest agent-repl-test-roster-a-stale-stream-close-keeps-the-standing-stream ()
  "A close of a stream already replaced does not forget the one standing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((old-conn (agent-repl-connect-open "127.0.0.1:61043"))
          (new-conn (agent-repl-connect-open "127.0.0.1:58175")))
      (agent-repl-test-roster--with-stream new-conn
        (agent-repl-roster-subscribe old-conn)
        (let ((old (car streams)))
          (agent-repl-roster-subscribe new-conn)
          ;; Act
          (funcall (plist-get old :on-close) '(:ended))
          ;; Assert
          (should (eq agent-repl-roster--stream (car streams))))))))

;;;; ---- A planned ending: the daemon's last frame says the end is expected ----

(defmacro agent-repl-test-roster--capturing (level &rest body)
  "Run BODY collecting every LEVEL record's formatted text into `logs'.
LEVEL is the logging rung's symbol, e.g. `agent-repl--info'."
  (declare (indent 1))
  `(cl-letf (((symbol-function ,level)
              (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
     ,@body))

(defun agent-repl-test-roster--end-planned (record)
  "Deliver the planned ending on the stubbed stream RECORD, then its clean end."
  (funcall (plist-get record :on-push) '(:arm :ending :value nil))
  (funcall (plist-get record :on-close) '(:ended)))

(ert-deftest agent-repl-test-roster-ending-push-is-recorded-at-info ()
  "The planned-ending frame itself is on the record, at INFO."
  ;; Arrange
  (let ((logs nil)
        (agent-repl-roster--stream 'standing)
        (agent-repl-roster--ending-stream nil))
    (agent-repl-test-roster--capturing 'agent-repl--info
      ;; Act
      (agent-repl-roster-on-push '(:arm :ending :value nil) 'standing))
    ;; Assert
    (should (member "elisp.roster.stream-ending" logs))))

(ert-deftest agent-repl-test-roster-an-unknown-push-arm-is-an-error ()
  "A push arm this consumer does not hold is recorded at ERROR, never applied."
  ;; Arrange
  (let ((logs nil) (applied nil))
    (cl-letf (((symbol-function 'agent-repl-roster-apply) (lambda (_) (setq applied t))))
      (agent-repl-test-roster--capturing 'agent-repl--error
        ;; Act
        (agent-repl-roster-on-push '(:arm :teleport :value nil))))
    ;; Assert
    (should (and (not applied)
                 (seq-some (lambda (l) (string-prefix-p "elisp.roster.unknown-push" l)) logs)))))

(ert-deftest agent-repl-test-roster-planned-end-is-recorded-at-info ()
  "A clean end after the planned ending is INFO, not the dropped-stream ERROR."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175"))
          (logs nil))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe old)
        (funcall (plist-get (car streams) :on-open))
        (agent-repl-test-roster--capturing 'agent-repl--info
          ;; Act
          (agent-repl-test-roster--end-planned (car streams)))
        ;; Assert
        (should (member "elisp.roster.stream-close: reason=planned-ending" logs))))))

(ert-deftest agent-repl-test-roster-planned-end-writes-no-error ()
  "A planned end never writes an ERROR."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175"))
          (logs nil))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe old)
        (funcall (plist-get (car streams) :on-open))
        (agent-repl-test-roster--capturing 'agent-repl--error
          ;; Act
          (agent-repl-test-roster--end-planned (car streams)))
        ;; Assert
        (should (null logs))))))

(ert-deftest agent-repl-test-roster-planned-end-resubscribes-on-the-live-daemon ()
  "A planned end follows the live daemon, exactly as a loss does."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175")))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe old)
        (funcall (plist-get (car streams) :on-open))
        ;; Act
        (agent-repl-test-roster--end-planned (car streams))
        ;; Assert
        (should (eq (plist-get agent-repl-roster--stream :conn) live))))))

(ert-deftest agent-repl-test-roster-clean-end-without-ending-is-still-an-error ()
  "A clean end the daemon did NOT announce stays the dropped-stream ERROR."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175"))
          (logs nil))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe old)
        (funcall (plist-get (car streams) :on-open))
        (agent-repl-test-roster--capturing 'agent-repl--error
          ;; Act
          (funcall (plist-get (car streams) :on-close) '(:ended)))
        ;; Assert
        (should (seq-some (lambda (l) (string-search "reason=ended-without-cancel" l)) logs))))))

(ert-deftest agent-repl-test-roster-error-after-ending-is-still-an-error ()
  "A transport error after the ending is a loss: only a CLEAN end is planned."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
          (live (agent-repl-connect-open "127.0.0.1:58175"))
          (logs nil))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe old)
        (funcall (plist-get (car streams) :on-open))
        (funcall (plist-get (car streams) :on-push) '(:arm :ending :value nil))
        (agent-repl-test-roster--capturing 'agent-repl--error
          ;; Act
          (funcall (plist-get (car streams) :on-close) '(:error (:kind :transport))))
        ;; Assert
        (should logs)))))

(ert-deftest agent-repl-test-roster-running-to-turn-failed-is-a-finish-edge ()
  "A turn that FAILED has ended: thinking -> turn-failed is the finish edge."
  ;; Act / Assert
  (should (agent-repl-roster--finish-edge-p :thinking :turn-failed)))

;;; test-roster.el ends here

;;;; ---- Tests: move-tab-to-back ----

(ert-deftest agent-repl-test-roster-move-tab-to-back-puts-ws-last ()
  "The deprio shuffle moves WS to the last slot of the roster tab order."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-roster--tab-order '("a" "b" "c")))
      ;; Act
      (agent-repl-roster-move-tab-to-back "a")
      ;; Assert
      (should (equal agent-repl-roster--tab-order '("b" "c" "a"))))))

(ert-deftest agent-repl-test-roster-move-tab-to-back-returns-the-new-order ()
  "The shuffle returns the order it installed, so the caller can mirror it."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-roster--tab-order '("a" "b" "c")))
      ;; Act
      (let ((got (agent-repl-roster-move-tab-to-back "b")))
        ;; Assert
        (should (equal got '("a" "c" "b")))))))

(ert-deftest agent-repl-test-roster-move-tab-to-back-unknown-ws-is-nil ()
  "A workspace with no tab returns nil and leaves the order alone."
  ;; Arrange
  (let ((agent-repl-roster--tab-order '("a" "b")))
    ;; Act
    (let ((got (agent-repl-roster-move-tab-to-back "nope")))
      ;; Assert
      (should (and (null got) (equal agent-repl-roster--tab-order '("a" "b")))))))


;;;; ---- The bring-up publication (a startup paints nothing) ----

(ert-deftest agent-repl-test-roster-reconcile-records-the-pass-at-info ()
  "The reconcile record goes out on the INFO rung, which the durable sink keeps.
It is the end of the startup's first-roster phase, and the DEBUG rung it
used to use does not clear the default `info' log-file level, so the
record reached no file at all."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "elisp.roster.reconcile: tabs=1" text))
                        logs)))))

(ert-deftest agent-repl-test-roster-reconcile-is-not-recorded-at-debug ()
  "The reconcile record must not fall back to the rung the durable sink drops."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should-not (seq-some (lambda (text)
                              (string-search "elisp.roster.reconcile:" text))
                            logs)))))

(defmacro agent-repl-test-roster--recording-bringup (var &rest body)
  "Run BODY with every bring-up publication appended to VAR, oldest first."
  (declare (indent 1))
  `(let ((agent-repl-roster-bringup-functions
          (list (lambda (opened total finished)
                  (setq ,var (append ,var (list (list opened total finished))))))))
     ,@body))

(ert-deftest agent-repl-test-roster-reconcile-publishes-a-step-per-tab-opened ()
  "One publication per tab born, counting up to the number it set out to open.
Panels park until focus, so a cold start paints nothing and the painted
count never moves; the tabs opening is the only bring-up there is."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (seen)
      (agent-repl-test-roster--recording-bringup seen
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                        (agent-repl-test-roster--row "b" "two" :ready)))))))
      ;; Assert
      (should (equal (seq-remove (lambda (step) (nth 2 step)) seen)
                     '((1 2 nil) (2 2 nil)))))))

(ert-deftest agent-repl-test-roster-reconcile-closes-the-pass-when-it-opened-tabs ()
  "The pass ends with one FINISHED publication naming what actually opened."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (seen)
      (agent-repl-test-roster--recording-bringup seen
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should (equal (seq-filter (lambda (step) (nth 2 step)) seen)
                     '((1 1 t)))))))

(ert-deftest agent-repl-test-roster-a-steady-state-pass-publishes-nothing ()
  "Every row already tabbed opens nothing, so it announces no bring-up."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row
                                                  "a" "one" :ready))))))
          seen)
      (agent-repl-roster-apply roster)
      (agent-repl-test-roster--recording-bringup seen
        ;; Act
        (agent-repl-roster-apply roster))
      ;; Assert
      (should (null seen)))))

(ert-deftest agent-repl-test-roster-a-failed-row-is-not-counted-as-opened ()
  "A row that could not open its tab is not a workspace that came up."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (seen)
      (cl-letf (((symbol-function 'agent-repl--ws-create)
                 (lambda (ws &optional dir)
                   (when (equal ws "one") (error "no such directory"))
                   (agent-repl--ws-put ws :project-dir (or dir "/w/x"))
                   ws))
                ((symbol-function 'agent-repl--error) #'ignore))
        (agent-repl-test-roster--recording-bringup seen
          ;; Act
          (agent-repl-roster-apply
           (agent-repl-test-roster--roster
            :sections (list (agent-repl-test-roster--section
                             "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                          (agent-repl-test-roster--row "b" "two" :ready))))))))
      ;; Assert
      (should (equal seen '((1 2 nil) (1 2 t)))))))

(ert-deftest agent-repl-test-roster-a-signalling-bringup-handler-is-contained ()
  "A progress display must never take down the reconciliation it reports on."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-roster-bringup-functions
           (list (lambda (&rest _) (error "display blew up")))))
      (cl-letf (((symbol-function 'agent-repl--warn) #'ignore))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should (equal (agent-repl-test-roster--tabs) '("one"))))))

(ert-deftest agent-repl-test-roster-a-signalling-bringup-handler-is-recorded ()
  "The containment is a WARNING, never a swallow: the handler is named."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl-roster-bringup-functions
           (list (lambda (&rest _) (error "display blew up"))))
          (logs nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "elisp.roster.bringup-handler-failed" text))
                        logs)))))

(ert-deftest agent-repl-test-roster-records-the-closed-row-it-gives-no-tab ()
  "The roster says which row it declined a tab for, and why.
`agent-repl-roster-desired-tabs' is the whole answer to \"why is there no
tab for that workspace\", and it used to give it silently: a register
whose row came back CLOSED drew no tab and left nothing in any log
saying the roster had been asked for one and declined."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-desired-tabs
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row
                                         "shut" "scratch-repo" :ready :closed t)))))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "elisp.roster.no-tab: id=shut name=scratch-repo reason=closed"
                                         text))
                        logs)))))

(ert-deftest agent-repl-test-roster-records-nothing-for-a-row-it-does-draw ()
  "The record names the rows REFUSED a tab; an open row is not one of them."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
        ;; Act
        (agent-repl-roster-desired-tabs
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "open" "one" :ready)))))))
      ;; Assert
      (should-not (seq-some (lambda (text) (string-search "elisp.roster.no-tab:" text))
                            logs)))))

;;;; ---- Opening a workspace opens its panels (owner ruling 2026-09-13, #6) ----

(defmacro agent-repl-test-roster--recording-arrivals (var &rest body)
  "Run BODY with every panel arrival appended to VAR as (WS ID), oldest first."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--panels-open-on-arrival)
              (lambda (ws id) (setq ,var (append ,var (list (list ws id)))))))
     ,@body))

(ert-deftest agent-repl-test-roster-a-tab-born-after-startup-opens-its-panels ()
  "A row that becomes open with the editor watching opens that workspace's panels."
  ;; Arrange — the startup roster has already been delivered.
  (agent-repl-test-roster--with-editor
    (let ((agent-repl--panels-arrivals-armed t)
          seen)
      (agent-repl-test-roster--recording-arrivals seen
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should (equal seen '(("one" "a")))))))

(ert-deftest agent-repl-test-roster-a-steady-state-push-opens-no-panels ()
  "A push whose rows are already tabbed — a plain switch — touches no panels."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((agent-repl--panels-arrivals-armed t)
          (roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
          seen)
      (agent-repl-roster-apply roster)
      (agent-repl-test-roster--recording-arrivals seen
        ;; Act
        (agent-repl-roster-apply roster))
      ;; Assert
      (should-not seen))))

(ert-deftest agent-repl-test-roster-the-first-reconcile-arms-the-arrival-gate ()
  "The startup roster is what arms later arrivals; it opens no panels itself."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
    ;; Assert
    (should agent-repl--panels-arrivals-armed)))

(ert-deftest agent-repl-test-roster-panels-open-before-the-minted-landing ()
  "A created workspace's panels are open BEFORE the verb stands on it."
  ;; Arrange — a create whose answer beat the roster push, so its landing is
  ;; still pending when the tab arrives.
  (agent-repl-test-roster--with-editor
    (let ((agent-repl--panels-arrivals-armed t)
          (agent-repl-verbs--pending-landing '(:id "a" :dir "/w/one"))
          (agent-repl-roster-update-functions
           (list #'agent-repl-verbs--pending-landing-fire))
          order)
      (cl-letf (((symbol-function 'agent-repl--panels-open-on-arrival)
                 (lambda (_ws _id) (setq order (append order '(panels)))))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (_dir) (setq order (append order '(landing))))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)))))))
      ;; Assert
      (should (equal order '(panels landing))))))

;;;; ---- Availability: a workspace opens only once the daemon has it ----

(defun agent-repl-test-roster--one-section (&rest rows)
  "Return a roster whose one repository section carries ROWS in order."
  (agent-repl-test-roster--roster
   :sections (list (agent-repl-test-roster--section "repo" rows))))

(ert-deftest agent-repl-test-roster-a-pending-row-gets-no-tab ()
  "The daemon has no session to offer yet, so there is nothing to open."
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :init :availability :pending)))
    ;; Assert
    (should-not (agent-repl--ws-by-ref-id "a"))))

(ert-deftest agent-repl-test-roster-a-pending-row-holds-every-later-row ()
  "Registry order: a ready row after a pending one waits for it."
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :init :availability :pending)
      (agent-repl-test-roster--row "b" "two" :ready)))
    ;; Assert
    (should-not (agent-repl--ws-by-ref-id "b"))))

(ert-deftest agent-repl-test-roster-rows-before-a-pending-row-open ()
  "Only the rows AFTER the first pending row are held."
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :ready)
      (agent-repl-test-roster--row "b" "two" :init :availability :pending)))
    ;; Assert
    (should (equal agent-repl-roster--tab-order '("one")))))

(ert-deftest agent-repl-test-roster-an-unavailable-row-opens ()
  "A failed bring-up still opens; its status arm draws the failure."
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :start-failed :availability :unavailable)))
    ;; Assert
    (should (agent-repl--ws-by-ref-id "a"))))

(ert-deftest agent-repl-test-roster-a-tabbed-row-reported-pending-is-kept ()
  "A relaunched daemon re-reporting an open workspace pending tears nothing down."
  (agent-repl-test-roster--with-editor
    ;; Arrange
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :ready)))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :init :availability :pending)))
    ;; Assert
    (should (equal agent-repl-roster--tab-order '("one")))))

(ert-deftest agent-repl-test-roster-a-tabbed-pending-row-holds-nothing-after-it ()
  "An open workspace does not hold the rows after it, whatever it reports."
  (agent-repl-test-roster--with-editor
    ;; Arrange
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :ready)))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :init :availability :pending)
      (agent-repl-test-roster--row "b" "two" :ready)))
    ;; Assert
    (should (equal agent-repl-roster--tab-order '("one" "two")))))

(ert-deftest agent-repl-test-roster-a-resolved-row-opens-with-the-rows-it-held ()
  "The push that resolves the held row opens it and walks on."
  (agent-repl-test-roster--with-editor
    ;; Arrange
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :init :availability :pending)
      (agent-repl-test-roster--row "b" "two" :ready)))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :ready)
      (agent-repl-test-roster--row "b" "two" :ready)))
    ;; Assert
    (should (equal agent-repl-roster--tab-order '("one" "two")))))

(ert-deftest agent-repl-test-roster-a-held-pass-does-not-finish-the-bringup ()
  "A held row is a bring-up still under way, so no FINISHED publication."
  (agent-repl-test-roster--with-editor
    (let (seen)
      (agent-repl-test-roster--recording-bringup seen
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--one-section
          (agent-repl-test-roster--row "a" "one" :ready)
          (agent-repl-test-roster--row "b" "two" :init :availability :pending))))
      ;; Assert
      (should-not (seq-filter (lambda (step) (nth 2 step)) seen)))))

(ert-deftest agent-repl-test-roster-a-held-bringup-keeps-its-count-across-passes ()
  "The pass that opens the held rows goes on counting the same bring-up."
  (agent-repl-test-roster--with-editor
    (let (seen)
      ;; Arrange
      (agent-repl-roster-apply
       (agent-repl-test-roster--one-section
        (agent-repl-test-roster--row "a" "one" :ready)
        (agent-repl-test-roster--row "b" "two" :init :availability :pending)))
      (agent-repl-test-roster--recording-bringup seen
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--one-section
          (agent-repl-test-roster--row "a" "one" :ready)
          (agent-repl-test-roster--row "b" "two" :ready))))
      ;; Assert
      (should (equal seen '((2 2 nil) (2 2 t)))))))

(ert-deftest agent-repl-test-roster-a-held-startup-leaves-the-arrival-gate-unarmed ()
  "Rows still arriving pass by pass keep the startup's panel exemption."
  (agent-repl-test-roster--with-editor
    (let ((agent-repl--panels-arrivals-armed nil))
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--one-section
        (agent-repl-test-roster--row "a" "one" :init :availability :pending)))
      ;; Assert
      (should-not agent-repl--panels-arrivals-armed))))

(ert-deftest agent-repl-test-roster-a-hold-is-recorded-once-per-row ()
  "A bring-up pushes the roster many times; the hold is stated once."
  (agent-repl-test-roster--with-editor
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        ;; Act
        (dotimes (_ 2)
          (agent-repl-roster-apply
           (agent-repl-test-roster--one-section
            (agent-repl-test-roster--row "a" "one" :init :availability :pending)))))
      ;; Assert
      (should (= 1 (seq-count (lambda (text) (string-search "elisp.roster.held:" text))
                              logs))))))

;;;; ---- A collapsed repository's tabs are off the bar ----

(ert-deftest agent-repl-test-roster-walk-marks-a-collapsed-sections-rows ()
  "Each walked row says whether its repository is collapsed."
  (let ((walked (agent-repl-roster-walk
                 (agent-repl-test-roster--roster
                  :sections (list (agent-repl-test-roster--section
                                   "open" (list (agent-repl-test-roster--row "w1" "one" :ready)))
                                  (agent-repl-test-roster--section
                                   "shut" (list (agent-repl-test-roster--row "w2" "two" :ready)) t))))))
    (should (equal (mapcar (lambda (entry) (plist-get entry :collapsed)) walked) '(nil t)))))

(ert-deftest agent-repl-test-roster-reconcile-keeps-a-collapsed-repositorys-tabs-off-the-drawing ()
  "A collapsed repository's workspaces keep their tabs and are not drawn."
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-reconcile
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "open" (list (agent-repl-test-roster--row "w1" "one" :ready)))
                      (agent-repl-test-roster--section
                       "shut" (list (agent-repl-test-roster--row "w2" "two" :ready)) t)
                      (agent-repl-test-roster--section
                       "late" (list (agent-repl-test-roster--row "w3" "three" :ready))))))
    (should (equal (agent-repl-roster-tab-order) '("one" "two" "three")))
    (should (equal (agent-repl-roster-drawn-tab-order) '("one" "three")))))

(ert-deftest agent-repl-test-roster-reconcile-draws-a-repository-again-once-expanded ()
  "The next push that expands the repository draws its tabs again."
  (agent-repl-test-roster--with-editor
    (let ((push (lambda (collapsed)
                  (agent-repl-roster-reconcile
                   (agent-repl-test-roster--roster
                    :sections (list (agent-repl-test-roster--section
                                     "shut" (list (agent-repl-test-roster--row "w2" "two" :ready))
                                     collapsed)))))))
      (funcall push t)
      (funcall push nil)
      (should (equal (agent-repl-roster-drawn-tab-order) '("two"))))))
