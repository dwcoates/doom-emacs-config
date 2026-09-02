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
`:attention', `:priority', `:current', `:dir'."
  (let ((dir (or (plist-get overrides :dir) (concat "/w/" id))))
    (list :workspace (list :workspace (list :id id :dir dir))
          :attention (plist-get overrides :attention)
          :priority (plist-get overrides :priority)
          :name (list :text name)
          :status (list :arm status :value nil)
          :current (list :current (or (plist-get overrides :current) :false))
          :children (plist-get overrides :children)
          :when (list :arm nil :value nil)
          :detail (list :branch nil :parent-branch nil :summary nil)
          :closed (list :closed (if (plist-member overrides :closed)
                                    (plist-get overrides :closed)
                                  :false)))))

(defun agent-repl-test-roster--section (label rows)
  "Return a decoded `RosterRepoSection' plist labelled LABEL carrying ROWS."
  (list :key (list :repository (list :id (concat "repo-" label) :dir (concat "/r/" label)))
        :header (list :label (list :text label))
        :rows (list :rows rows)))

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
           (agent-repl-roster--rows-by-id (make-hash-table :test 'equal))
           (agent-repl-roster--status-by-id (make-hash-table :test 'equal))
           (agent-repl-roster-finish-functions nil)
           (agent-repl-roster-update-functions nil)
           (agent-repl-host-last-selected-id nil))
       (cl-letf (((symbol-function 'agent-repl--ws-create)
                  (lambda (ws &optional dir)
                    (push ws agent-repl-test-roster--created)
                    (agent-repl--ws-put ws :project-dir (or dir "/w/x"))
                    ws))
                 ((symbol-function 'agent-repl--ws-persp-kill)
                  (lambda (ws) (push ws agent-repl-test-roster--killed) t))
                 ((symbol-function 'agent-repl--ws-rename-persp)
                  (lambda (_old _new) t))
                 ((symbol-function 'agent-repl--ws-switch)
                  (lambda (ws &rest _) (push ws agent-repl-test-roster--switched) ws))
                 ((symbol-function 'agent-repl--ws-current-name)
                  (lambda () agent-repl-test-roster--current-name))
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
                 (agent-repl--ws-put ws :ws-id ws)
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

;;; test-roster.el ends here
