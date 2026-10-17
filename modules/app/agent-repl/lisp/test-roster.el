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

(defconst agent-repl-test-roster--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory this suite and `roster.el' live in.")

(defun agent-repl-test-roster--row (id name status &rest overrides)
  "Return a decoded `RosterRow' plist for ID, NAME and STATUS.
OVERRIDES is a plist merged over the defaults: `:closed', `:children',
`:attention', `:priority', `:viewed', `:detached-live', `:one-shot'
\(non-nil for a one-shot), `:async-merging' (non-nil while the branch
merges async), `:current', `:dir', `:availability' (an arm keyword,
default `:available')."
  (let ((dir (or (plist-get overrides :dir) (concat "/w/" id))))
    (list :workspace (list :workspace (list :id id :dir dir))
          :attention (plist-get overrides :attention)
          :priority (plist-get overrides :priority)
          :viewed (plist-get overrides :viewed)
          :detached-live (plist-get overrides :detached-live)
          :one-shot (and (plist-get overrides :one-shot) t)
          :async-merging (and (plist-get overrides :async-merging) t)
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

(defun agent-repl-test-roster--merged-group (label rows &optional collapsed)
  "Return a decoded `RosterMergedRepoGroup' for repository LABEL holding ROWS.
COLLAPSED non-nil puts it on the `repository_collapsed' arm."
  (list :key (list :repository (list :id (concat "r-" label) :dir (concat "/r/" label)))
        :header (list :label (list :text label))
        :rows (list :rows rows)
        :drawn (list :arm (if collapsed :repository-collapsed :shown) :value nil)))

(cl-defun agent-repl-test-roster--roster (&key sections hidden merged merged-groups current tab-bar)
  "Return a decoded `WorkspaceRoster' plist from SECTIONS, HIDDEN, MERGED and CURRENT.
HIDDEN is the hidden section's rows; the section itself is always there,
expanded, as the daemon always sends it.  MERGED is the recently-merged
rows, carried in one shown group; MERGED-GROUPS, when given, is the band's
groups instead.  TAB-BAR is the daemon's tab bar, defaulting to the
fixture's rows in the order it lists them (`agent-repl-test--tab-bar-of'),
a collapsed merged group's rows withheld."
  (list :repository (list :sections sections)
        :tab-bar (or tab-bar (agent-repl-test--tab-bar-of
                              sections hidden merged merged-groups))
        :hidden (list :header (list :label (list :text "Hidden"))
                      :rows (list :rows hidden)
                      :fold (list :arm :expanded :value nil))
        :recently-merged (list :header (list :label (list :text "Recently Merged"))
                               :groups (or merged-groups
                                           (and merged
                                                (list (agent-repl-test-roster--merged-group
                                                       "alpha" merged)))))
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

(defvar agent-repl-test-roster--landed nil
  "Workspaces whose input window the roster asked to land in, newest first.")

(defvar agent-repl-test-roster--following-at-switch :unset
  "`agent-repl-roster-following' as the stubbed switch saw it.")

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
           (agent-repl-host-reselect-pending nil)
           (agent-repl-host--select-in-flight nil)
           (agent-repl-host--select-queued nil)
           (agent-repl-host--confirming nil)
           (agent-repl-host-last-request nil)
           (agent-repl-roster--judged-current nil)
           (agent-repl-roster--pushed-at nil)
           (agent-repl-test-roster--landed nil)
           (agent-repl-test-roster--following-at-switch :unset)
           ;; A STANDING ROSTER STREAM, the ordinary state: a view is only
           ;; followed while the stream that served it stands.
           (agent-repl-roster--stream 'agent-repl-test-roster--standing-stream))
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
                  (lambda (ws &rest _)
                    (setq agent-repl-test-roster--following-at-switch
                          agent-repl-roster-following)
                    (push ws agent-repl-test-roster--switched) ws))
                 ((symbol-function 'agent-repl--maybe-autoselect-input)
                  (lambda (ws) (push ws agent-repl-test-roster--landed)))
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

(defun agent-repl-test-roster--with-subsections (section &rest subsection-rows)
  "Return SECTION carrying one subsection per list of SUBSECTION-ROWS."
  (append section
          (list :subsections
                (seq-map-indexed (lambda (rows i)
                                   (list :key (list :subsection-id (format "sub-%d" i))
                                         :header (list :label (list :text (format "sub %d" i)))
                                         :rows (list :rows rows)))
                                 subsection-rows))))

(ert-deftest agent-repl-test-roster-section-rows-follow-the-sections-own-with-its-subsections ()
  "A section's rows are its unsectioned rows, then each subsection's in turn."
  ;; Arrange
  (let ((section (agent-repl-test-roster--with-subsections
                  (agent-repl-test-roster--section
                   "alpha" (list (agent-repl-test-roster--row "a" "a-row" :ready)))
                  (list (agent-repl-test-roster--row "s" "s-row" :ready))
                  nil
                  (list (agent-repl-test-roster--row "t" "t-row" :ready)))))
    ;; Act
    (let ((ids (mapcar #'agent-repl-roster-row-id (agent-repl-roster-section-rows section))))
      ;; Assert
      (should (equal ids '("a" "s" "t"))))))

(ert-deftest agent-repl-test-roster-walk-visits-a-sections-subsections ()
  "The walk finds a workspace in a subsection, after the section's own rows."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--with-subsections
                                  (agent-repl-test-roster--section
                                   "alpha" (list (agent-repl-test-roster--row "a" "a-row" :ready)))
                                  (list (agent-repl-test-roster--row "s" "s-row" :ready)))
                                 (agent-repl-test-roster--section
                                  "beta" (list (agent-repl-test-roster--row "b" "b-row" :ready)))))))
    ;; Act
    (let ((ids (mapcar (lambda (e) (agent-repl-roster-row-id (plist-get e :row)))
                       (agent-repl-roster-walk roster))))
      ;; Assert
      (should (equal ids '("a" "s" "b"))))))

(ert-deftest agent-repl-test-roster-repository-of-finds-a-workspace-in-a-subsection ()
  "A workspace in a subsection belongs to its section's repository."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--with-subsections
                                  (agent-repl-test-roster--section "alpha" nil)
                                  (list (agent-repl-test-roster--row "s" "s-row" :ready)))))))
    ;; Act / Assert
    (should (equal (plist-get (agent-repl-roster-repository-of "s" roster) :id)
                   "repo-alpha"))))

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

(ert-deftest agent-repl-test-roster-walk-visits-merged-groups-in-order ()
  "The recently-merged band's groups are walked in the band's order."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :merged-groups
                 (list (agent-repl-test-roster--merged-group
                        "alpha" (list (agent-repl-test-roster--row "m1" "m1" :merged)))
                       (agent-repl-test-roster--merged-group
                        "beta" (list (agent-repl-test-roster--row "m2" "m2" :merged)))))))
    ;; Act
    (let ((ids (mapcar (lambda (e) (agent-repl-roster-row-id (plist-get e :row)))
                       (agent-repl-roster-walk roster))))
      ;; Assert
      (should (equal ids '("m1" "m2"))))))

(ert-deftest agent-repl-test-roster-walk-collapses-a-collapsed-repositorys-merged-group ()
  "A merged group on `repository_collapsed' is walked off the bar's drawing."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :merged-groups
                 (list (agent-repl-test-roster--merged-group
                        "alpha" (list (agent-repl-test-roster--row "m1" "m1" :merged)) t)))))
    ;; Act
    (let ((entry (car (agent-repl-roster-walk roster))))
      ;; Assert
      (should (plist-get entry :collapsed)))))

(ert-deftest agent-repl-test-roster-walk-keeps-a-shown-merged-group-on-the-bar ()
  "A merged group on `shown' is walked as drawn, under the band's label."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :merged (list (agent-repl-test-roster--row "m1" "m1" :merged)))))
    ;; Act
    (let ((entry (car (agent-repl-roster-walk roster))))
      ;; Assert
      (should-not (plist-get entry :collapsed))
      (should (equal (plist-get entry :label) "Recently Merged")))))

(ert-deftest agent-repl-test-roster-walk-names-each-rows-section ()
  "Every walked entry names the section its row sits in, children included."
  ;; Arrange
  (let ((roster (agent-repl-test-roster--roster
                 :sections (list (agent-repl-test-roster--section
                                  "alpha"
                                  (list (agent-repl-test-roster--row
                                         "p" "p" :ready
                                         :children (list (agent-repl-test-roster--row "c" "c" :ready))))))
                 :hidden (list (agent-repl-test-roster--row "h" "h" :ready))
                 :merged (list (agent-repl-test-roster--row "m" "m" :merged)))))
    ;; Act
    (let ((sections (mapcar (lambda (e) (cons (agent-repl-roster-row-id (plist-get e :row))
                                              (plist-get e :section)))
                            (agent-repl-roster-walk roster))))
      ;; Assert
      (should (equal sections '(("p" . :repository) ("c" . :repository)
                                ("h" . :hidden) ("m" . :merged)))))))

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

(ert-deftest agent-repl-test-roster-instant-ms-reads-the-named-field ()
  "The instant reader answers the `:at-ms' of the row field it is named."
  (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws)
             (lambda (_ws) '(:last-selected (:at-ms 7) :other (:at-ms 9)))))
    (should (equal (agent-repl-roster--instant-ms "a" :other) 9))))

(ert-deftest agent-repl-test-roster-instant-ms-is-nil-for-an-absent-field ()
  "A row without the field has no instant to read."
  (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws)
             (lambda (_ws) '(:last-selected nil))))
    (should-not (agent-repl-roster--instant-ms "a" :last-selected))))

(ert-deftest agent-repl-test-roster-instant-ms-is-nil-without-a-row ()
  "A workspace the roster has no row for has no instant."
  (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws) (lambda (_ws) nil)))
    (should-not (agent-repl-roster--instant-ms "a" :last-selected))))

(ert-deftest agent-repl-test-roster-instant-accessors-share-the-reader ()
  "Every durable-instant accessor reads through `agent-repl-roster--instant-ms',
naming its own row field, so no accessor hand-rolls a divergent read."
  (let (asked)
    (cl-letf (((symbol-function 'agent-repl-roster--instant-ms)
               (lambda (ws field) (push (cons ws field) asked) 1)))
      (agent-repl-roster-last-selected-ms "a")
      (agent-repl-roster-last-turn-ended-ms "b"))
    (should (equal (reverse asked) '(("a" . :last-selected) ("b" . :last-turn-ended))))))

(ert-deftest agent-repl-test-roster-last-turn-ended-ms-reads-the-row ()
  "The durable turn-end instant is the row's `last_turn_ended'."
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (plist-put (agent-repl-test-roster--row "a" "first" :done)
                                               :last-turn-ended '(:at-ms 42)))))))
    (should (equal (agent-repl-roster-last-turn-ended-ms "first") 42))))

;;; --- The last-finished ranking

(defmacro agent-repl-test-roster--with-finishes (rows &rest body)
  "Run BODY with ROWS (list of (NAME STATUS ENDED-MS)) as the roster's rows."
  (declare (indent 1))
  `(let ((rows ,rows))
     (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws)
                (lambda (ws)
                  (let ((row (assoc ws rows)))
                    (and row
                         (list :status (list :arm (nth 1 row) :value nil)
                               :last-turn-ended (and (nth 2 row)
                                                     (list :at-ms (nth 2 row)))))))))
       ,@body)))

(ert-deftest agent-repl-test-roster-last-finished-picks-the-newest-done ()
  "Among done workspaces the one whose turn ended last wins."
  (agent-repl-test-roster--with-finishes '(("a" :done 100) ("b" :done 900) ("c" :done 500))
    (should (equal (agent-repl-roster-last-finished-name '("a" "b" "c")) "b"))))

(ert-deftest agent-repl-test-roster-last-finished-skips-a-workspace-working-again ()
  "A workspace whose next turn already runs has not finished, however recent its last end."
  (agent-repl-test-roster--with-finishes '(("a" :done 100) ("b" :thinking 900))
    (should (equal (agent-repl-roster-last-finished-name '("a" "b")) "a"))))

(ert-deftest agent-repl-test-roster-last-finished-skips-an-interrupted-turn ()
  "An interrupted turn is not a finish: the user stopped it."
  (agent-repl-test-roster--with-finishes '(("a" :done 100) ("b" :interrupted 900))
    (should (equal (agent-repl-roster-last-finished-name '("a" "b")) "a"))))

(ert-deftest agent-repl-test-roster-last-finished-skips-a-failed-turn ()
  "A failed turn is turquoise, not green: it is not a finish."
  (agent-repl-test-roster--with-finishes '(("a" :done 100) ("b" :turn-failed 900))
    (should (equal (agent-repl-roster-last-finished-name '("a" "b")) "a"))))

(ert-deftest agent-repl-test-roster-last-finished-skips-a-done-row-without-an-instant ()
  "A done row with no recorded close has nothing to rank by."
  (agent-repl-test-roster--with-finishes '(("a" :done 100) ("b" :done nil))
    (should (equal (agent-repl-roster-last-finished-name '("b" "a")) "a"))))

(ert-deftest agent-repl-test-roster-last-finished-is-nil-with-no-done-row ()
  "No done workspace means no finish to jump to."
  (agent-repl-test-roster--with-finishes '(("a" :thinking 100) ("b" :ready 900))
    (should-not (agent-repl-roster-last-finished-name '("a" "b")))))

(ert-deftest agent-repl-test-roster-last-finished-keeps-the-given-order-on-a-tie ()
  "Two finishes at the same instant resolve to the first NAMES gives."
  (agent-repl-test-roster--with-finishes '(("a" :done 500) ("b" :done 500))
    (should (equal (agent-repl-roster-last-finished-name '("b" "a")) "b"))))

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

(ert-deftest agent-repl-test-roster-a-current-whose-stream-ended-is-not-followed ()
  "A dead daemon's last `current' must not move the frame off the user's
own choice (2026-10-07): the view decides nothing once its stream ended."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    (setq agent-repl-roster--stream nil)
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                    (agent-repl-test-roster--row "b" "two" :ready))))
      :current "b"))
    ;; Assert
    (should (null agent-repl-test-roster--switched))))

(ert-deftest agent-repl-test-roster-a-current-whose-stream-ended-is-recorded-at-info ()
  "The ignored `current' is said, so the frame staying put is visible."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    (setq agent-repl-roster--stream nil)
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                        (agent-repl-test-roster--row "b" "two" :ready))))
          :current "b")))
      ;; Assert
      (should (cl-find-if (lambda (line) (string-prefix-p "elisp.roster.current: stream-ended id=b" line))
                          logged)))))

(defun agent-repl-test-roster--two-tabs (current)
  "Return a roster with tabs one (a) and two (b) and CURRENT selected."
  (agent-repl-test-roster--roster
   :sections (list (agent-repl-test-roster--section
                    "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                 (agent-repl-test-roster--row "b" "two" :ready))))
   :current current))

(ert-deftest agent-repl-test-roster-a-current-emacs-selected-before-still-moves-the-frame ()
  "A `current' naming a workspace Emacs once selected is still obeyed.
Regression, 2026-10-02: every id Emacs had ever selected was taken for
\"Emacs's own\" and skipped, so a sidebar click to a visited workspace
went nowhere.  Emacs never leads now, so there is nothing to skip."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one"
          agent-repl-host-last-selected-id "b")
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("two")))))

(ert-deftest agent-repl-test-roster-a-current-is-deferred-behind-another-pending-request ()
  "While Emacs's request for another workspace is unanswered, a `current'
naming something else moves nothing: rapid presses do not walk the frame
through every stop."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "a"))
    (setq agent-repl-host--select-in-flight "one")
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--switched nil))))

(ert-deftest agent-repl-test-roster-a-current-naming-the-pending-request-is-followed ()
  "The push stamping the very workspace Emacs requested moves the frame at
once, without waiting for the request's answer."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "a"))
    (setq agent-repl-host--select-in-flight "two")
    (cl-letf (((symbol-function 'agent-repl-host-ref)
               (lambda (ws) (and (equal ws "two") (list :id "b" :dir "/w/b")))))
      ;; Act
      (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("two")))))

(ert-deftest agent-repl-test-roster-a-deferred-current-is-applied-once-nothing-is-pending ()
  "Re-applying reads the LAST accepted roster, so a deferred `current' is
obeyed once the pending request is answered: the last push wins."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one"
          agent-repl-host--select-in-flight "one")
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
    (setq agent-repl-host--select-in-flight nil)
    ;; Act
    (agent-repl-roster-apply-current)
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("two")))))

(ert-deftest agent-repl-test-roster-a-followed-switch-is-marked-as-following ()
  "The switch runs with `agent-repl-roster-following' bound, so the
activation hook sends no SelectWorkspace echo."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
    ;; Assert
    (should (eq agent-repl-test-roster--following-at-switch t))))

(ert-deftest agent-repl-test-roster-a-followed-switch-lands-in-the-input-window ()
  "Following `current' lands the destination's input window at once."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--landed '("two")))))

(ert-deftest agent-repl-test-roster-a-current-already-shown-lands-nothing ()
  "No switch means no landing: the user's window is left where it is."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "two")
    ;; Act
    (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
    ;; Assert
    (should (equal agent-repl-test-roster--landed nil))))

(ert-deftest agent-repl-test-roster-a-current-without-a-tab-is-recorded-at-info ()
  "A `current' whose workspace has no tab yet is a durable decision."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((infos nil))
      (setq agent-repl-test-roster--current-name "one")
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))
          :current "zz")))
      ;; Assert
      (should (member "elisp.roster.current: no tab yet id=zz" infos)))))

(ert-deftest agent-repl-test-roster-a-deferred-current-is-recorded-at-info ()
  "A `current' put off behind a pending request is a durable decision."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((infos nil))
      (setq agent-repl-test-roster--current-name "one"
            agent-repl-host--select-in-flight "one")
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
      ;; Assert
      (should (member "elisp.roster.current: deferred id=b ws=two requested=one" infos)))))

(ert-deftest agent-repl-test-roster-a-current-is-held-while-a-minibuffer-is-active ()
  "A `current' arriving while the user types in a minibuffer moves nothing:
following it would select the destination's input window."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    (let ((agent-repl--minibuffer-held nil))
      (cl-letf (((symbol-function 'agent-repl--minibuffer-active-p) (lambda () t)))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
      ;; Assert
      (should (null agent-repl-test-roster--switched))
      (should (equal (mapcar #'car agent-repl--minibuffer-held) '(roster-follow))))))

(ert-deftest agent-repl-test-roster-a-held-current-follows-the-newest-roster-on-release ()
  "The release re-applies the roster as it stands then, so a selection that
changed while the user typed is followed once, to where it ended."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "one")
    (let ((agent-repl--minibuffer-held nil)
          (agent-repl--minibuffer-release-armed nil)
          (active t))
      (cl-letf (((symbol-function 'agent-repl--minibuffer-active-p) (lambda () active)))
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "a"))
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
        (setq active nil)
        ;; Act
        (agent-repl--minibuffer-release))
      ;; Assert
      (should (equal agent-repl-test-roster--switched '("two"))))))

(ert-deftest agent-repl-test-roster-a-followed-current-is-recorded-at-info ()
  "The follow is recorded at INFO with its push arrival."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((infos nil))
      (setq agent-repl-test-roster--current-name "one")
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
      ;; Assert
      (should (cl-find-if (lambda (m)
                            (string-match-p
                             "\\`elisp\\.roster\\.current: followed ws=two id=b from=one pushed-at=[0-9:.]+ since-push-ms=[0-9]+ since-request-ms=none\\'"
                             m))
                          infos)))))

(ert-deftest agent-repl-test-roster-a-followed-request-records-its-latency ()
  "A follow of Emacs's own request states the milliseconds since it."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((infos nil))
      (setq agent-repl-test-roster--current-name "one"
            agent-repl-host-last-request (cons "two" (float-time)))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
      ;; Assert
      (should (cl-find-if (lambda (m) (string-match-p "since-request-ms=[0-9]+\\'" m))
                          infos)))))

(ert-deftest agent-repl-test-roster-already-shown-is-info-the-first-time ()
  "The first push naming the shown workspace records the judgment at INFO."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((infos nil))
      (setq agent-repl-test-roster--current-name "two")
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
      ;; Assert
      (should (member "elisp.roster.current: already shown ws=two" infos)))))

(ert-deftest agent-repl-test-roster-already-shown-repeats-drop-to-debug ()
  "Every push carries `current'; a repeat judgment is not re-recorded at INFO."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((infos nil))
      (setq agent-repl-test-roster--current-name "two")
      (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b"))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "b")))
      ;; Assert
      (should-not (member "elisp.roster.current: already shown ws=two" infos)))))

(ert-deftest agent-repl-test-roster-the-switch-precedes-the-finish-hooks ()
  "The selection is applied before the edge hooks run, so a switch the
user is waiting on never queues behind repaint and notification work."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((seen :unset))
      (setq agent-repl-test-roster--current-name "one")
      (agent-repl-roster-apply (agent-repl-test-roster--two-tabs "a"))
      (setq agent-repl-roster-status-change-functions
            (list (lambda (&rest _) (setq seen agent-repl-test-roster--switched))))
      ;; Act: row b changes status in the same push that selects it.
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready)
                                      (agent-repl-test-roster--row "b" "two" :done))))
        :current "b"))
      ;; Assert
      (should (equal seen '("two"))))))

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

(ert-deftest agent-repl-test-roster-an-unchanged-status-is-not-recorded ()
  "Every push walks every row; a row whose arm did not move records nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((push (lambda ()
                  (agent-repl-roster-apply
                   (agent-repl-test-roster--roster
                    :sections (list (agent-repl-test-roster--section
                                     "repo" (list (agent-repl-test-roster--row "a" "one" :thinking)))))))))
      (funcall push)
      (let (logged)
        (cl-letf (((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (funcall push))
        ;; Assert
        (should-not (seq-some (lambda (text) (string-prefix-p "elisp.roster.status:" text)) logged))))))

(ert-deftest agent-repl-test-roster-a-changed-status-is-recorded ()
  "A row whose arm moved without a finish edge is recorded."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-roster-apply
         (agent-repl-test-roster--roster
          :sections (list (agent-repl-test-roster--section
                           "repo" (list (agent-repl-test-roster--row "a" "one" :thinking)))))))
      ;; Assert
      (should (seq-some (lambda (text) (string-prefix-p "elisp.roster.status: id=a from=:ready to=:thinking" text))
                        logged)))))

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

(ert-deftest agent-repl-test-roster-question-to-thinking-is-no-finish-edge ()
  "A question gate returning to thinking is a move WITHIN the running set."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-finishes fired
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :question))))))
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

(ert-deftest agent-repl-test-roster-row-one-shot-reads-the-mark ()
  "A row carrying `RosterRowOneShot' reads as a one-shot."
  (should (agent-repl-roster-row-one-shot
           (agent-repl-test-roster--row "a" "one" :done :one-shot t))))

(ert-deftest agent-repl-test-roster-row-one-shot-when-no-one-shot ()
  "A row without the mark reads as no one-shot."
  (should-not (agent-repl-roster-row-one-shot
               (agent-repl-test-roster--row "a" "one" :done))))

(ert-deftest agent-repl-test-roster-one-shot-for-ws-after-a-push ()
  "After a push, a workspace's one-shot mark is its current row's."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :done :one-shot t))))))
    ;; Assert
    (should (agent-repl-roster-one-shot-for-ws "one"))))

(ert-deftest agent-repl-test-roster-one-shot-for-ws-before-any-push ()
  "Before any push carried its row, a workspace has no one-shot outcome."
  (agent-repl-test-roster--with-editor
    (should-not (agent-repl-roster-one-shot-for-ws "one"))))

(ert-deftest agent-repl-test-roster-row-async-merging-reads-the-marker ()
  "A row carrying `RosterRowAsyncMerging' reads as async-merging."
  (should (agent-repl-roster-row-async-merging
           (agent-repl-test-roster--row "a" "one" :thinking :async-merging t))))

(ert-deftest agent-repl-test-roster-row-async-merging-when-absent ()
  "A row without the marker reads as not async-merging."
  (should-not (agent-repl-roster-row-async-merging
               (agent-repl-test-roster--row "a" "one" :done))))

(ert-deftest agent-repl-test-roster-async-merging-for-ws-after-a-push ()
  "After a push, a workspace's async-merging marker is its current row's."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :done :async-merging t))))))
    ;; Assert
    (should (agent-repl-roster-async-merging-for-ws "one"))))

(ert-deftest agent-repl-test-roster-async-merging-for-ws-before-any-push ()
  "Before any push carried its row, a workspace carries no async-merging marker."
  (agent-repl-test-roster--with-editor
    (should-not (agent-repl-roster-async-merging-for-ws "one"))))

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

(ert-deftest agent-repl-test-roster-handover-subscribes-on-the-successor ()
  "An accepted successor carries the roster: the outgoing daemon may never leave."
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
        (agent-repl-roster-on-link-handover old successor)
        ;; Assert
        (should (equal conns (list successor)))))))

(ert-deftest agent-repl-test-roster-handover-cancels-the-stream-on-the-old-conn ()
  "The handover replaces the outgoing daemon\='s roster stream, never stacks one."
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
        (agent-repl-roster-on-link-handover old successor)
        ;; Assert
        (should (equal cancelled '(old-stream)))))))

(ert-deftest agent-repl-test-roster-handover-subscription-is-logged ()
  "The move is news: it names the daemon the roster now rides."
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
        (agent-repl-roster-on-link-handover old successor)
        ;; Assert
        (should (seq-some (lambda (text)
                            (string-search "elisp.roster.resubscribed-on-handover" text))
                          logs))))))

(ert-deftest agent-repl-test-roster-registers-on-the-handover-hook ()
  "The registration IS the fix: without it a stuck handover hides every tab."
  ;; Act / Assert
  (should (memq #'agent-repl-roster-on-link-handover
                (default-value 'agent-repl-link-handover-functions))))

(ert-deftest agent-repl-test-roster-promotion-keeps-a-stream-already-on-the-successor ()
  "A roster moved at the handover is not torn down and re-opened at the promotion."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((conns nil)
          (successor (agent-repl-connect-open "127.0.0.1:9100"))
          (old (agent-repl-connect-open "127.0.0.1:9001"))
          (agent-repl-roster--stream 'successor-stream)
          (agent-repl-roster--stream-conn nil))
      (setq agent-repl-roster--stream-conn successor)
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-workspace-roster)
                 (lambda (conn _on-push _on-close &optional _on-open)
                   (push conn conns) 'fake-stream)))
        ;; Act
        (agent-repl-roster-on-link-promote old successor)
        ;; Assert
        (should (null conns))
        (should (eq agent-repl-roster--stream 'successor-stream))))))

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
         (agent-repl-roster--stream-conn nil)
         (agent-repl-roster--accepted nil)
         (agent-repl-roster--ending-stream nil)
         (ending-conn nil))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () ,live))
               ((symbol-function 'agent-repl-link-ending-p)
                (lambda (conn) (and conn (eq conn ending-conn))))
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

;; Regression, 2026-10-03: a bounce's announced stand-down ended the roster
;; stream before the link's own, and the roster re-subscribed on the daemon
;; that had just said it was leaving.

(ert-deftest agent-repl-test-roster-planned-end-on-the-departing-live-daemon-subscribes-nothing ()
  "A planned end while the link still names the departing daemon dials nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((leaving (agent-repl-connect-open "127.0.0.1:61043")))
      (agent-repl-test-roster--with-stream leaving
        (agent-repl-roster-subscribe leaving)
        (funcall (plist-get (car streams) :on-open))
        ;; Act
        (agent-repl-test-roster--end-planned (car streams))
        ;; Assert
        (should (and (= (length streams) 1)
                     (null agent-repl-roster--stream)))))))

(ert-deftest agent-repl-test-roster-planned-end-on-the-departing-live-daemon-records-the-wait ()
  "The wait for the link edge is on the record, at INFO."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((leaving (agent-repl-connect-open "127.0.0.1:61043"))
          (logs nil))
      (agent-repl-test-roster--with-stream leaving
        (agent-repl-roster-subscribe leaving)
        (funcall (plist-get (car streams) :on-open))
        (agent-repl-test-roster--capturing 'agent-repl--info
          ;; Act
          (agent-repl-test-roster--end-planned (car streams)))
        ;; Assert
        (should (member "elisp.roster.resubscribe-awaiting-link-edge reason=daemon-departing address=\"127.0.0.1:61043\"" logs))))))

(ert-deftest agent-repl-test-roster-a-loss-on-a-live-daemon-that-announced-its-ending-waits ()
  "A loss on a daemon whose link carried the ending waits for the link edge too."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((leaving (agent-repl-connect-open "127.0.0.1:61043")))
      (agent-repl-test-roster--with-stream leaving
        (agent-repl-roster-subscribe leaving)
        (funcall (plist-get (car streams) :on-open))
        (setq ending-conn leaving)
        ;; Act
        (funcall (plist-get (car streams) :on-close) '(:error (:kind :transport)))
        ;; Assert
        (should (= (length streams) 1))))))

(ert-deftest agent-repl-test-roster-a-loss-on-a-live-daemon-that-said-nothing-resubscribes ()
  "A loss on the live daemon that announced no ending re-subscribes on it at once."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((live (agent-repl-connect-open "127.0.0.1:61043")))
      (agent-repl-test-roster--with-stream live
        (agent-repl-roster-subscribe live)
        (funcall (plist-get (car streams) :on-open))
        ;; Act
        (funcall (plist-get (car streams) :on-close) '(:error (:kind :transport)))
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

(ert-deftest agent-repl-test-roster-running-to-turn-died-is-a-finish-edge ()
  "A turn that died with agent-repl's machinery has ended: thinking -> turn-died
is the finish edge (owner ruling, 2026-10-06)."
  (should (agent-repl-roster--finish-edge-p :thinking :turn-died)))

(ert-deftest agent-repl-test-roster-running-to-vendor-blocked-is-a-finish-edge ()
  "A turn the vendor ended stands as vendor-blocked: thinking -> vendor-blocked
is the finish edge (owner ruling, 2026-10-06)."
  (should (agent-repl-roster--finish-edge-p :thinking :vendor-blocked)))

(ert-deftest agent-repl-test-roster-running-to-turn-failed-is-a-finish-edge ()
  "A turn that FAILED has ended: thinking -> turn-failed is the finish edge."
  ;; Act / Assert
  (should (agent-repl-roster--finish-edge-p :thinking :turn-failed)))

(ert-deftest agent-repl-test-roster-async-merging-is-no-settled-status ()
  "An async merge is a row marker, not a status: no status list names it."
  ;; Act / Assert
  (should-not (memq :async-merging agent-repl-roster-settled-statuses))
  (should-not (memq :async-merging agent-repl-roster-running-statuses)))

(ert-deftest agent-repl-test-roster-for-ws-applies-the-reader-to-the-row ()
  "`agent-repl-roster--for-ws' answers READER of WS's current row."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "a" "one" :done))))))
    ;; Act / Assert
    (should (equal (agent-repl-roster--for-ws "one" (lambda (row) (plist-get row :name)))
                   '(:text "one")))))

(ert-deftest agent-repl-test-roster-for-ws-before-any-push-never-calls-the-reader ()
  "Before any push carried its row, the reader is never called and nil answers."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((called nil))
      ;; Act
      (should-not (agent-repl-roster--for-ws "one" (lambda (_) (setq called t))))
      ;; Assert
      (should-not called))))

(ert-deftest agent-repl-test-roster-for-ws-readers-share-the-one-shape ()
  "Every `agent-repl-roster-*-for-ws' reader goes through `agent-repl-roster--for-ws'."
  ;; Arrange
  (let ((forms nil))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "roster.el" agent-repl-test-roster--dir))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    ;; Act
    (let ((readers (seq-filter (lambda (form)
                                 (and (eq (car-safe form) 'defun)
                                      (string-match-p "\\`agent-repl-roster-.*-for-ws\\'"
                                                      (symbol-name (nth 1 form)))
                                      (not (memq (nth 1 form) '(agent-repl-roster-row-for-ws
                                                                agent-repl-roster--for-ws)))))
                               forms)))
      ;; Assert
      (should (>= (length readers) 4))
      (dolist (form readers)
        (should (eq (car-safe (car (last form))) 'agent-repl-roster--for-ws))))))

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

;;;; ---- The tab bar's order is the daemon's ----

(defun agent-repl-test-roster--tab (id &optional arm)
  "Return a decoded `RosterTab' for workspace ID with visibility ARM."
  (list :workspace (list :id id :dir (concat "/w/" id))
        :visibility (list :arm (or arm :drawn) :value nil)))

(ert-deftest agent-repl-test-roster-desired-tabs-follow-the-tab-bar-not-the-sections ()
  "The tabs come in the daemon's tab-bar order, whatever order the sections walk."
  ;; Arrange: the sections walk a, b, c; the registry puts c first.
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "a" :ready)
                                                 (agent-repl-test-roster--row "b" "b" :ready)
                                                 (agent-repl-test-roster--row "c" "c" :ready))))
                   :tab-bar (list :tabs (list (agent-repl-test-roster--tab "c")
                                              (agent-repl-test-roster--tab "a")
                                              (agent-repl-test-roster--tab "b"))))))
      ;; Act
      (let ((ids (mapcar (lambda (tab) (plist-get tab :id))
                         (agent-repl-roster-desired-tabs roster))))
        ;; Assert
        (should (equal ids '("c" "a" "b")))))))

(ert-deftest agent-repl-test-roster-a-withheld-tab-is-collapsed ()
  "A tab the daemon withholds keeps its tab but comes back `:collapsed'."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let ((roster (agent-repl-test-roster--roster
                   :sections (list (agent-repl-test-roster--section
                                    "repo" (list (agent-repl-test-roster--row "a" "a" :ready)
                                                 (agent-repl-test-roster--row "h" "h" :ready))))
                   :tab-bar (list :tabs (list (agent-repl-test-roster--tab "a")
                                              (agent-repl-test-roster--tab "h" :withheld-hidden))))))
      ;; Act
      (let ((collapsed (mapcar (lambda (tab) (cons (plist-get tab :id) (plist-get tab :collapsed)))
                               (agent-repl-roster-desired-tabs roster))))
        ;; Assert
        (should (equal collapsed '(("a") ("h" . t))))))))

(ert-deftest agent-repl-test-roster-a-tab-with-no-row-is-recorded-at-error ()
  "A tab naming a workspace no grouping draws is a broken roster, said loudly."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (let ((tabs (agent-repl-roster-desired-tabs
                     (agent-repl-test-roster--roster
                      :sections (list (agent-repl-test-roster--section
                                       "repo" (list (agent-repl-test-roster--row "a" "a" :ready))))
                      :tab-bar (list :tabs (list (agent-repl-test-roster--tab "ghost")
                                                 (agent-repl-test-roster--tab "a")))))))
          ;; Assert
          (should (equal (mapcar (lambda (tab) (plist-get tab :id)) tabs) '("a")))))
      (should (member "elisp.roster.tab-without-row: id=ghost" errors)))))

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
          (agent-repl-verbs--pending-landing nil)
          (agent-repl-verbs--pending-landing-dispatched-at nil)
          (agent-repl-verbs--pending-landing-lander nil)
          (agent-repl-host--editor-switches 0)
          (agent-repl-roster-update-functions
           (list #'agent-repl-verbs--pending-landing-fire))
          order)
      (agent-repl-verbs--pending-landing-register '(:id "a" :dir "/w/one") 0)
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

;;;; ---- A hidden workspace's tab is off the bar ----

(ert-deftest agent-repl-test-roster-walk-marks-every-hidden-row-collapsed ()
  "The hidden section's rows, children included, walk as collapsed.
Its own fold is EXPANDED here: that fold is the sidebar's, never the bar's."
  (let ((walked (agent-repl-roster-walk
                 (agent-repl-test-roster--roster
                  :sections (list (agent-repl-test-roster--section
                                   "open" (list (agent-repl-test-roster--row "w1" "one" :ready))))
                  :hidden (list (agent-repl-test-roster--row
                                 "w2" "two" :ready
                                 :children (list (agent-repl-test-roster--row "w3" "three" :ready))))))))
    (should (equal (mapcar (lambda (entry) (plist-get entry :collapsed)) walked) '(nil t t)))))

(ert-deftest agent-repl-test-roster-walk-puts-the-hidden-section-before-recently-merged ()
  "The walk is repositories, then hidden, then recently merged."
  (let ((walked (agent-repl-roster-walk
                 (agent-repl-test-roster--roster
                  :sections (list (agent-repl-test-roster--section
                                   "open" (list (agent-repl-test-roster--row "w1" "one" :ready))))
                  :hidden (list (agent-repl-test-roster--row "w2" "two" :ready))
                  :merged (list (agent-repl-test-roster--row "w3" "three" :merged))))))
    (should (equal (mapcar (lambda (entry) (agent-repl-roster-row-id (plist-get entry :row))) walked)
                   '("w1" "w2" "w3")))))

(ert-deftest agent-repl-test-roster-reconcile-keeps-a-hidden-workspaces-tab-off-the-drawing ()
  "A hidden workspace keeps its tab, and the drawn numbering closes up over it."
  (agent-repl-test-roster--with-editor
    (agent-repl-roster-reconcile
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "repo" (list (agent-repl-test-roster--row "w1" "one" :ready)
                                    (agent-repl-test-roster--row "w3" "three" :ready))))
      :hidden (list (agent-repl-test-roster--row "w2" "two" :ready))))
    (should (member "two" (agent-repl-roster-tab-order)))
    (should (equal (agent-repl-roster-drawn-tab-order) '("one" "three")))))

(ert-deftest agent-repl-test-roster-reconcile-draws-a-workspace-again-once-shown ()
  "The next push that moves the workspace back into its repository draws its tab."
  (agent-repl-test-roster--with-editor
    (let ((one (agent-repl-test-roster--row "w1" "one" :ready))
          (two (agent-repl-test-roster--row "w2" "two" :ready)))
      (agent-repl-roster-reconcile
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section "repo" (list one)))
        :hidden (list two)))
      (agent-repl-roster-reconcile
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section "repo" (list one two)))))
      (should (equal (agent-repl-roster-drawn-tab-order) '("one" "two"))))))

;;;; ---- One row, indexed ahead of the next push ----

(ert-deftest agent-repl-test-roster-note-row-indexes-a-newer-row ()
  "A row from a revision past the roster held is indexed under its id."
  (agent-repl-test-roster--with-editor
    ;; Arrange
    (let ((row (agent-repl-test-roster--row "a" "one" :thinking))
          (agent-repl-roster-view '(:revision 4)))
      ;; Act
      (agent-repl-roster-note-row row 5)
      ;; Assert
      (should (eq (gethash "a" agent-repl-roster--rows-by-id) row)))))

(ert-deftest agent-repl-test-roster-note-row-keeps-a-row-no-newer-than-the-roster ()
  "A row from a revision the held roster already reached is kept out."
  (agent-repl-test-roster--with-editor
    ;; Arrange
    (let ((held (agent-repl-test-roster--row "a" "one" :done))
          (stale (agent-repl-test-roster--row "a" "one" :thinking))
          (agent-repl-roster-view '(:revision 5)))
      (puthash "a" held agent-repl-roster--rows-by-id)
      ;; Act
      (agent-repl-roster-note-row stale 5)
      ;; Assert
      (should (eq (gethash "a" agent-repl-roster--rows-by-id) held)))))

;;;; ---- A fresh tab is held until the daemon answers it open ----

(defmacro agent-repl-test-roster--opens-unanswered (&rest body)
  "Run BODY inside `agent-repl-test-roster--with-editor' with opens left unanswered.
Every request is recorded in `asked' as its list of ref ids."
  (declare (indent 0))
  `(agent-repl-test-roster--with-editor
     (let* ((asked nil)
            (agent-repl-open-tabs-sender
             (lambda (workspaces _on-frame _on-close)
               (push (mapcar (lambda (w) (plist-get (plist-get w :workspace) :id)) workspaces)
                     asked)
               t)))
       (ignore asked)
       ,@body)))

(ert-deftest agent-repl-test-roster-a-fresh-row-is-made-but-its-tab-held ()
  "A fresh row's workspace is created at once, but its tab is not drawn."
  (agent-repl-test-roster--opens-unanswered
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section (agent-repl-test-roster--row "a" "one" :ready)))
    ;; Assert
    (should (equal (list (agent-repl--ws-by-ref-id "a") agent-repl-roster--tab-order)
                   '("one" nil)))))

(ert-deftest agent-repl-test-roster-a-walk-asks-for-every-fresh-row-at-once ()
  "Every fresh row is asked open in one request, a pending one included."
  (agent-repl-test-roster--opens-unanswered
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :init :availability :pending)
      (agent-repl-test-roster--row "b" "two" :ready)))
    ;; Assert
    (should (equal asked '(("a" "b"))))))

(ert-deftest agent-repl-test-roster-a-tab-answered-open-is-drawn ()
  "A tab answered open is drawn once the order is re-derived."
  (agent-repl-test-roster--opens-unanswered
    ;; Arrange
    (agent-repl-roster-apply
     (agent-repl-test-roster--one-section
      (agent-repl-test-roster--row "a" "one" :ready)
      (agent-repl-test-roster--row "b" "two" :ready)))
    ;; Act
    (puthash "a" 'open agent-repl-open-tabs--state)
    (agent-repl-roster-refresh-order)
    ;; Assert
    (should (equal agent-repl-roster--tab-order '("one")))))

(ert-deftest agent-repl-test-roster-a-current-naming-a-held-tab-switches-nowhere ()
  "The selection cannot land on a tab not yet answered open."
  (agent-repl-test-roster--opens-unanswered
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "r" (list (agent-repl-test-roster--row "a" "one" :ready))))
      :current "a"))
    ;; Assert
    (should (null agent-repl-test-roster--switched))))

(ert-deftest agent-repl-test-roster-a-current-naming-an-opened-tab-is-followed ()
  "Once the tab is answered open, the same current is followed."
  (agent-repl-test-roster--opens-unanswered
    ;; Arrange
    (agent-repl-roster-apply
     (agent-repl-test-roster--roster
      :sections (list (agent-repl-test-roster--section
                       "r" (list (agent-repl-test-roster--row "a" "one" :ready))))
      :current "a"))
    (puthash "a" 'open agent-repl-open-tabs--state)
    ;; Act
    (agent-repl-roster-apply-current)
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("one")))))

;;; The tab's drawn status, on the record

(defmacro agent-repl-test-roster--recording-drawn-statuses (var &rest body)
  "Run BODY with VAR bound to the elisp.roster.drawn-status INFO records, oldest first.
Each element is (WS LINE)."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'agent-repl--info)
                (lambda (ws fmt &rest args)
                  (let ((line (apply #'format fmt args)))
                    (when (string-prefix-p "elisp.roster.drawn-status:" line)
                      (setq ,var (append ,var (list (list ws line)))))))))
       ,@body)))

(ert-deftest agent-repl-test-roster-a-first-sighting-records-the-drawn-status ()
  "The first arm a row draws is on the record, as moving from nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-drawn-statuses drawn
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal (mapcar #'cadr drawn)
                     '("elisp.roster.drawn-status: id=a from=nil to=:ready"))))))

(ert-deftest agent-repl-test-roster-a-changed-arm-records-the-drawn-status ()
  "A row whose arm moved records the new arm beside the one it replaced."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-drawn-statuses drawn
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
      (should (equal (cadr (car (last drawn)))
                     "elisp.roster.drawn-status: id=a from=:ready to=:thinking")))))

(ert-deftest agent-repl-test-roster-a-restated-arm-records-no-drawn-status ()
  "A re-push restating the same arm draws nothing new and records nothing."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-drawn-statuses drawn
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
      (should (= (length drawn) 1)))))

(ert-deftest agent-repl-test-roster-the-drawn-status-is-recorded-on-the-tabs-workspace ()
  "The record goes to the tab's own workspace sink, not the central one."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (agent-repl-test-roster--recording-drawn-statuses drawn
      ;; Act
      (agent-repl-roster-apply
       (agent-repl-test-roster--roster
        :sections (list (agent-repl-test-roster--section
                         "repo" (list (agent-repl-test-roster--row "a" "one" :ready))))))
      ;; Assert
      (should (equal (car (car drawn)) "one")))))

;;;; ---- Reading the switcher's facts off a row ----

(ert-deftest agent-repl-test-roster-row-closed-by-user-at-reads-a-users-close ()
  "A user's close answers its instant."
  (should (equal (agent-repl-roster-row-closed-by-user-at
                  '(:closure (:at-ms 42 :by (:arm :user :value nil))))
                 42)))

(ert-deftest agent-repl-test-roster-row-closed-by-user-at-ignores-every-other-cause ()
  "A close the user did not make answers nil."
  (dolist (arm '(:agent :merge :daemon))
    (should-not (agent-repl-roster-row-closed-by-user-at
                 (list :closure (list :at-ms 42 :by (list :arm arm :value nil)))))))

(ert-deftest agent-repl-test-roster-row-closed-by-user-at-of-an-unrecorded-close-is-nil ()
  "A row with no close record answers nil."
  (should-not (agent-repl-roster-row-closed-by-user-at '(:closure nil))))

(ert-deftest agent-repl-test-roster-row-merged-at-reads-the-merged-arm ()
  "A merged row's when-column carries when its merge landed."
  (should (equal (agent-repl-roster-row-merged-at
                  '(:when (:arm :merged :value (:at-ms 7))))
                 7)))

(ert-deftest agent-repl-test-roster-row-merged-at-of-an-unmerged-row-is-nil ()
  "Any other when-arm answers nil."
  (should-not (agent-repl-roster-row-merged-at
               '(:when (:arm :hidden :value (:at-ms 7 :cache nil))))))

(ert-deftest agent-repl-test-roster-row-commit-count-reads-the-detail ()
  "The detail panel's count is the row's commit count, zero included."
  (should (equal (agent-repl-roster-row-commit-count '(:detail (:commits (:count 0)))) 0))
  (should (equal (agent-repl-roster-row-commit-count '(:detail (:commits (:count 3)))) 3))
  (should-not (agent-repl-roster-row-commit-count '(:detail (:commits nil)))))

(ert-deftest agent-repl-test-roster-row-instant-ms-reads-a-durable-instant ()
  "The one instant reader answers a row's `:at-ms', nil without the field."
  (should (equal (agent-repl-roster-row-instant-ms '(:last-turn-ended (:at-ms 9)) :last-turn-ended) 9))
  (should-not (agent-repl-roster-row-instant-ms '(:last-turn-ended nil) :last-turn-ended))
  (should-not (agent-repl-roster-row-instant-ms nil :last-selected)))

(ert-deftest agent-repl-test-roster-workspace-instants-go-through-the-row-reader ()
  "Every per-workspace instant accessor reads through the one row reader."
  ;; Arrange
  (let ((row '(:last-selected (:at-ms 11) :last-turn-ended (:at-ms 22)))
        (read nil))
    (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws) (lambda (_ws) row))
              ((symbol-function 'agent-repl-roster-row-instant-ms)
               (lambda (r field) (push field read) (plist-get (plist-get r field) :at-ms))))
      ;; Act
      (agent-repl-roster-last-selected-ms "w")
      (agent-repl-roster-last-turn-ended-ms "w")
      ;; Assert
      (should (equal read '(:last-turn-ended :last-selected))))))

(ert-deftest agent-repl-test-roster-move-frame-switches-as-a-follow-and-lands-in-the-input ()
  "The frame moves bound as a follow, so the activation is never counted as
a second switch, and lands in the destination's input window."
  ;; Arrange
  (let (switched-following landed)
    (cl-letf (((symbol-function 'agent-repl--ws-switch)
               (lambda (_ws) (setq switched-following agent-repl-roster-following)))
              ((symbol-function 'agent-repl--maybe-autoselect-input)
               (lambda (ws) (setq landed ws))))
      ;; Act
      (agent-repl-roster-move-frame "alpha")
      ;; Assert
      (should (equal (list switched-following landed) '(t "alpha"))))))

(ert-deftest agent-repl-test-roster-every-move-to-a-selected-workspace-goes-through-move-frame ()
  "The roster's follow and Emacs's own switch request move the frame
through `agent-repl-roster-move-frame', and neither switches on its own."
  (dolist (spec '(("roster.el" . "agent-repl-roster--follow")
                  ("host.el" . "agent-repl-host--move-on-request")))
    (with-temp-buffer
      ;; Arrange
      (insert-file-contents (expand-file-name (car spec) agent-repl-test--module-dir))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (re-search-forward (concat "^(defun " (regexp-quote (cdr spec)) " "))
      (beginning-of-defun)
      ;; Act
      (let ((body (buffer-substring (point) (progn (end-of-defun) (point)))))
        ;; Assert
        (should (string-match-p "(agent-repl-roster-move-frame " body))
        (should-not (string-match-p "(agent-repl--ws-switch " body))))))

;;;; ---- An answered selection is never moved back by a roster from before it ----

(defun agent-repl-test-roster--at-revision (roster revision)
  "ROSTER with its revision set to REVISION."
  (plist-put (copy-sequence roster) :revision revision))

(ert-deftest agent-repl-test-roster-a-roster-from-before-the-answered-selection-is-not-followed ()
  "The selection answered at revision 7 holds: a roster at 6 naming the
workspace the user left is deferred, so the frame stays where Emacs moved
it (2026-10-16: it went back, then forward, on every switch)."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "two"
          agent-repl-host--confirming (cons "two" 7))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--at-revision (agent-repl-test-roster--two-tabs "a") 6))
    ;; Assert
    (should (null agent-repl-test-roster--switched))
    (should (equal agent-repl-host--confirming '("two" . 7)))))

(ert-deftest agent-repl-test-roster-the-roster-carrying-the-answered-selection-releases-it ()
  "A roster at the answered revision releases the selection and is judged
as any roster is."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "two"
          agent-repl-host--confirming (cons "two" 7))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--at-revision (agent-repl-test-roster--two-tabs "b") 7))
    ;; Assert
    (should (null agent-repl-host--confirming))
    (should (null agent-repl-test-roster--switched))))

(ert-deftest agent-repl-test-roster-a-later-roster-naming-another-workspace-is-followed ()
  "Once released, a roster past the answered revision that names another
workspace is the daemon's newer word, and is followed."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-test-roster--current-name "two"
          agent-repl-host--confirming (cons "two" 7))
    ;; Act
    (agent-repl-roster-apply
     (agent-repl-test-roster--at-revision (agent-repl-test-roster--two-tabs "a") 8))
    ;; Assert
    (should (equal agent-repl-test-roster--switched '("one")))))

(ert-deftest agent-repl-test-roster-a-closed-stream-ends-the-wait-for-an-answered-selection ()
  "The next daemon's revisions start afresh, so the wait ends with the stream."
  ;; Arrange
  (agent-repl-test-roster--with-editor
    (setq agent-repl-host--confirming (cons "two" 7))
    ;; Act
    (agent-repl-roster-on-close '(:cancelled))
    ;; Assert
    (should (null agent-repl-host--confirming))))
