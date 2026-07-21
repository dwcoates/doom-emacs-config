;;; test-sidebar.el --- ERT tests for sidebar.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the workspaces-sidebar roster feed and its action handlers.
;; Pure elisp: the webview push boundary
;; (`agent-repl--frontend-webview-execute-script'), the persp membership
;; probe (`agent-repl--ws-open-p'), and the picker/tab-bar entry points
;; are `cl-letf'-mocked; workspace fixtures go through the
;; `agent-repl--ws-put' wrapper API.  The roster universe
;; (`agent-repl--live-ws-names') is pure in-memory hash state, so it
;; runs REAL against the fixtures — tombstoning a fixture is how a test
;; excludes it.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-sidebar.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

;; Sidebar globals are rebound by `agent-repl-test--with-clean-state'
;; (test-helpers.el) along with the rest of the module state, since the
;; 1Hz state tick reaches them from any test.

(defun agent-repl-test--sidebar-ws (name dir &rest plist)
  "Register workspace NAME at DIR with a pre-cached repo key.
The `:group-key' default keeps roster builds off the git wrapper;
PLIST key/value pairs are applied after it and may override it."
  (agent-repl--ws-put name :project-dir dir)
  (agent-repl--ws-put name :group-key "/repos/doom/.git")
  (while plist
    (agent-repl--ws-put name (pop plist) (pop plist))))

(defun agent-repl-test--sidebar-repo (roster key)
  "Return ROSTER's repo plist with KEY, or nil."
  (cl-find-if (lambda (r) (equal (plist-get r :key) key))
              (append (plist-get roster :repos) nil)))

(defun agent-repl-test--sidebar-row (rows name)
  "Return the row plist named NAME in the vector ROWS, or nil."
  (cl-find-if (lambda (r) (equal (plist-get r :name) name))
              (append rows nil)))

;;;; ---- Wire status mapping ----------------------------------------------

(ert-deftest agent-repl-test-sidebar-wire-status-table ()
  "Each mapped render-state keyword serializes to its contract string.
Perspective is held open so the render-state mapping is exercised rather
than the perspective-less short-circuit."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (dolist (case '((:thinking . "thinking") (:permission . "permission")
                      (:init . "init") (:idle . "idle") (:idle-async . "idle")
                      (:stop-failed . "dead") (:dead . "dead")
                      (:merging . "merging") (:merge-queued . "merge-queued")
                      (:merge-conflict . "merge-conflict")
                      (:merge-failed . "merge-failed") (:merged . "merged")))
        (cl-letf (((symbol-function 'agent-repl--ws-render-status)
                   (lambda (_ws) (car case))))
          (should (equal (agent-repl--sidebar-wire-status "ws") (cdr case))))))))

(ert-deftest agent-repl-test-sidebar-wire-status-done-unacked ()
  "An unacked :done serializes as plain \"done\"."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :agent-state :done)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "done")))))

(ert-deftest agent-repl-test-sidebar-wire-status-done-acked-is-viewed ()
  "An acked :done serializes as \"done-viewed\" (tab-bar viewed semantics)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws"
                                 :agent-state :done :done-acked t)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "done-viewed")))))

(ert-deftest agent-repl-test-sidebar-wire-status-nil-is-none ()
  "A nil render status (unborn workspace) serializes as \"none\"."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "none")))))

(ert-deftest agent-repl-test-sidebar-wire-status-perspective-less-is-inactive ()
  "A perspective-less workspace serializes as \"inactive\", overriding its state.
The `agent-repl--ws-open-p' nil branch dominates even a live `:merged'
render state, since sidebar-but-not-tab-bar is the fact being conveyed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-render-status) (lambda (_ws) :merged)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "inactive")))))

(ert-deftest agent-repl-test-sidebar-wire-status-unmapped-errors ()
  "An unmapped render-state keyword signals instead of defaulting."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-render-status)
               (lambda (_ws) :not-a-state)))
      (should-error (agent-repl--sidebar-wire-status "ws")))))

;;;; ---- The closed (greyed) predicate ---------------------------------------

(ert-deftest agent-repl-test-sidebar-closed-p-table ()
  "Perspective-less or torn-down-REPL rows are closed; hosted ones are not."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (dolist (case '((t :active nil) (t :inactive t) (t :hidden t)
                    (t nil nil) (nil :active t)))
      (cl-destructuring-bind (open repl expected) case
        (agent-repl--ws-put "ws" :repl-state repl)
        (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                   (lambda (_ws) open)))
          (should (eq (agent-repl--sidebar-closed-p "ws") expected)))))))

;;;; ---- The roster universe -------------------------------------------------

(ert-deftest agent-repl-test-sidebar-entries-live-workspaces-only ()
  "Tombstoned registrations do not enter the roster."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "live" "/tmp/live")
    (agent-repl-test--sidebar-ws "tomb" "/tmp/tomb" :nuked-at 1.0)
    (should (equal (agent-repl--sidebar-entries)
                   '(("live" . "/tmp/live"))))))

(ert-deftest agent-repl-test-sidebar-entries-skips-dirless-workspace ()
  "A live workspace with no `:project-dir' is skipped (with a log)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl--ws-put "dirless" :prefix-counter 1)
    (should (equal (agent-repl--sidebar-entries)
                   '(("ws" . "/tmp/ws"))))))

;;;; ---- Roster building ---------------------------------------------------

(ert-deftest agent-repl-test-sidebar-build-groups-by-repo ()
  "Two same-repo workspaces land as two roots of one repo section."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "a" "/tmp/a")
    (agent-repl-test--sidebar-ws "b" "/tmp/b")
    (let* ((roster (car (agent-repl--sidebar-build)))
           (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git")))
      (should (= 1 (length (plist-get roster :repos))))
      (should (equal (plist-get repo :label) "doom"))
      (should (= 2 (length (plist-get repo :rows)))))))

(ert-deftest agent-repl-test-sidebar-build-nests-child-under-parent ()
  "A `:source-ws-dir' matching another live entry nests the row as its child."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "parent" "/tmp/parent")
    (agent-repl-test--sidebar-ws "child" "/tmp/child"
                                 :source-ws-dir "/tmp/parent")
    (let* ((roster (car (agent-repl--sidebar-build)))
           (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git"))
           (rows (plist-get repo :rows))
           (parent (agent-repl-test--sidebar-row rows "parent")))
      (should (= 1 (length rows)))
      (should (agent-repl-test--sidebar-row (plist-get parent :children)
                                            "child")))))

(ert-deftest agent-repl-test-sidebar-build-tombstoned-parent-roots-child ()
  "A child whose parent workspace is tombstoned roots in its own repo."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "parent" "/tmp/parent" :nuked-at 1.0)
    (agent-repl-test--sidebar-ws "child" "/tmp/child"
                                 :source-ws-dir "/tmp/parent")
    (let* ((roster (car (agent-repl--sidebar-build)))
           (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git")))
      (should (agent-repl-test--sidebar-row (plist-get repo :rows)
                                            "child")))))

(ert-deftest agent-repl-test-sidebar-build-self-parent-errors ()
  "A row whose `:source-ws-dir' is its own dir signals a corrupt plist."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :source-ws-dir "/tmp/ws")
    (should-error (agent-repl--sidebar-build))))

(ert-deftest agent-repl-test-sidebar-build-cycle-errors ()
  "A `:source-ws-dir' cycle orphans its family and must signal."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "a" "/tmp/a" :source-ws-dir "/tmp/b")
    (agent-repl-test--sidebar-ws "b" "/tmp/b" :source-ws-dir "/tmp/a")
    (should-error (agent-repl--sidebar-build))))

(ert-deftest agent-repl-test-sidebar-build-unknown-repo-sections-last ()
  "The `(no repo)' sentinel section sorts after every labeled repo."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "known" "/tmp/known")
    (agent-repl-test--sidebar-ws "lost" "/tmp/lost"
                                 :group-key agent-repl--repo-key-unknown)
    (let* ((roster (car (agent-repl--sidebar-build)))
           (keys (mapcar (lambda (r) (plist-get r :key))
                         (append (plist-get roster :repos) nil))))
      (should (equal keys (list "/repos/doom/.git"
                                agent-repl--repo-key-unknown))))))

(ert-deftest agent-repl-test-sidebar-build-folded-repo-serialized-not-flat ()
  "A folded repo keeps its serialized rows but yields no flat dirs."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl--toggle-repo-fold "/repos/doom/.git")
    (let* ((built (agent-repl--sidebar-build))
           (repo (agent-repl-test--sidebar-repo (car built) "/repos/doom/.git")))
      (should (eq (plist-get repo :folded) t))
      (should (= 1 (length (plist-get repo :rows))))
      (should (null (cdr built))))))

(ert-deftest agent-repl-test-sidebar-build-siblings-sort-by-created-at ()
  "Siblings order by `:created-at' ascending."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "younger" "/tmp/younger" :created-at 200.0)
    (agent-repl-test--sidebar-ws "older" "/tmp/older" :created-at 100.0)
    (should (equal (cdr (agent-repl--sidebar-build))
                   (list (agent-repl--path-canonical "/tmp/older")
                         (agent-repl--path-canonical "/tmp/younger"))))))

(ert-deftest agent-repl-test-sidebar-build-flat-order-depth-first ()
  "Flat dirs list parents immediately before their children."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "parent" "/tmp/parent" :created-at 100.0)
    (agent-repl-test--sidebar-ws "child" "/tmp/child"
                                 :source-ws-dir "/tmp/parent")
    (agent-repl-test--sidebar-ws "uncle" "/tmp/uncle" :created-at 200.0)
    (should (equal (cdr (agent-repl--sidebar-build))
                   (mapcar #'agent-repl--path-canonical
                           '("/tmp/parent" "/tmp/child" "/tmp/uncle"))))))

;;;; ---- Row serialization --------------------------------------------------

(ert-deftest agent-repl-test-sidebar-row-closed-when-repl-inactive ()
  "An open workspace whose REPL is `:inactive' serializes closed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :inactive)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" nil (vector))))
        (should (eq (plist-get row :closed) t))))))

(ert-deftest agent-repl-test-sidebar-row-closed-when-perspective-less ()
  "A live workspace with no open perspective serializes closed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :active)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil)))
      (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" nil (vector))))
        (should (eq (plist-get row :closed) t))))))

(ert-deftest agent-repl-test-sidebar-row-hosted-not-closed ()
  "An open workspace with an `:active' REPL serializes closed=false."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :active)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" nil (vector))))
        (should (eq (plist-get row :closed) :false))))))

(ert-deftest agent-repl-test-sidebar-row-current-flag ()
  "The row matching CURRENT-NAME serializes current=true."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" "ws" (vector))))
      (should (eq (plist-get row :current) t)))))

(ert-deftest agent-repl-test-sidebar-row-optionals-null ()
  "Missing branch/parent/summary/recency serialize as JSON null."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" nil (vector))))
      (should (eq (plist-get row :branch) :null))
      (should (eq (plist-get row :parentBranch) :null))
      (should (eq (plist-get row :summary) :null))
      (should (eq (plist-get row :lastViewedAt) :null)))))

(ert-deftest agent-repl-test-sidebar-roster-json-roundtrip ()
  "The serialized roster parses back with contract keys and JSON types."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws"
                                 :agent-state :thinking
                                 :branch-name "DWC/ws"
                                 :last-viewed-at 1000.5)
    ;; Hold the perspective open so the row carries its live "thinking"
    ;; status rather than the perspective-less "inactive" short-circuit.
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (let* ((json (json-serialize (car (agent-repl--sidebar-build))))
             (parsed (json-parse-string json :object-type 'alist
                                        :null-object :null
                                        :false-object :false))
             (repo (aref (alist-get 'repos parsed) 0))
             (row (aref (alist-get 'rows repo) 0)))
        (should (eq (alist-get 'navDir parsed) :null))
        (should (eq (alist-get 'folded repo) :false))
        (should (equal (alist-get 'label repo) "doom"))
        (should (equal (alist-get 'status row) "thinking"))
        (should (equal (alist-get 'branch row) "DWC/ws"))
        (should (= (alist-get 'lastViewedAt row) 1000.5))
        (should (equal (append (alist-get 'children row) nil) nil))))))

;;;; ---- Pushing ------------------------------------------------------------

(ert-deftest agent-repl-test-sidebar-push-targets-live-webviews-only ()
  "The push script lands in live frontend buffers and skips dead ones."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "live" "/tmp/live")
    (agent-repl-test--sidebar-ws "dead" "/tmp/dead")
    (let ((buf (generate-new-buffer " sidebar-live-view")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "live" :frontend-buffer buf)
            (agent-repl--ws-put "dead" :frontend-buffer
                                (let ((b (generate-new-buffer " dead-view")))
                                  (kill-buffer b) b))
            (let (pushed)
              (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                         (lambda (b script) (push (cons b script) pushed))))
                (agent-repl--sidebar-push))
              (should (= 1 (length pushed)))
              (should (eq (caar pushed) buf))
              (should (string-match-p "agentReplWorkspaceRoster" (cdar pushed)))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-sidebar-push-refreshes-flat-dirs ()
  "A push rewrites `agent-repl--sidebar-flat-dirs' from the fresh build."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (setq agent-repl--sidebar-flat-dirs '("/stale"))
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
               (lambda (_b _s))))
      (agent-repl--sidebar-push))
    (should (equal agent-repl--sidebar-flat-dirs
                   (list (agent-repl--path-canonical "/tmp/ws"))))))

(ert-deftest agent-repl-test-sidebar-push-script-guards-missing-hook ()
  "The push script no-ops (page side) when the hook is not yet planted."
  (should (equal (agent-repl--sidebar-push-script "{}")
                 "window.agentReplWorkspaceRoster && window.agentReplWorkspaceRoster({});")))

(ert-deftest agent-repl-test-sidebar-expand-script-guards-missing-hook ()
  "The expand script no-ops (page side) when the hook is not yet planted."
  (should (equal (agent-repl--sidebar-expand-script "/tmp/ws")
                 "window.agentReplWorkspaceExpand && window.agentReplWorkspaceExpand(\"/tmp/ws\");")))

(ert-deftest agent-repl-test-sidebar-expand-script-json-encodes-dir ()
  "A dir with JS-hostile characters rides in as an escaped JSON string."
  (should (equal (agent-repl--sidebar-expand-script "/tmp/a\"b")
                 "window.agentReplWorkspaceExpand && window.agentReplWorkspaceExpand(\"/tmp/a\\\"b\");")))

(ert-deftest agent-repl-test-sidebar-expand-push-fires-in-live-webviews ()
  "The expand push lands the expand script in a live frontend buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "live" "/tmp/live")
    (let ((buf (generate-new-buffer " sidebar-expand-view")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "live" :frontend-buffer buf)
            (let (pushed)
              (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                         (lambda (b script) (push (cons b script) pushed))))
                (agent-repl--sidebar-expand-push "/tmp/live"))
              (should (= 1 (length pushed)))
              (should (eq (caar pushed) buf))
              (should (string-match-p "agentReplWorkspaceExpand" (cdar pushed)))
              (should (string-match-p "/tmp/live" (cdar pushed)))))
        (kill-buffer buf)))))

;;;; ---- The 1Hz gate --------------------------------------------------------

(ert-deftest agent-repl-test-sidebar-tick-skips-when-signature-unchanged ()
  "Two ticks over unchanged state push exactly once."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((pushes 0))
      (cl-letf (((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--sidebar-tick)
        (agent-repl--sidebar-tick))
      (should (= pushes 1)))))

(ert-deftest agent-repl-test-sidebar-tick-pushes-on-fold-change ()
  "A fold flip between ticks re-pushes."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((pushes 0))
      (cl-letf (((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--sidebar-tick)
        (agent-repl--toggle-repo-fold "/repos/doom/.git")
        (agent-repl--sidebar-tick))
      (should (= pushes 2)))))

;;;; ---- Keyboard navigation -------------------------------------------------

(ert-deftest agent-repl-test-sidebar-nav-empty-user-errors ()
  "Navigation over an empty roster raises `user-error'."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--sidebar-nav-move 1) :type 'user-error)))

(ert-deftest agent-repl-test-sidebar-nav-next-starts-at-first ()
  "With no cursor, next lands on the first visible row."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-flat-dirs '("/a" "/b" "/c"))
    (cl-letf (((symbol-function 'agent-repl--sidebar-open-dir) (lambda (_dir))))
      (agent-repl--sidebar-nav-move 1))
    (should (equal agent-repl--sidebar-nav-dir "/a"))))

(ert-deftest agent-repl-test-sidebar-nav-prev-starts-at-last ()
  "With no cursor, prev lands on the last visible row."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-flat-dirs '("/a" "/b" "/c"))
    (cl-letf (((symbol-function 'agent-repl--sidebar-open-dir) (lambda (_dir))))
      (agent-repl--sidebar-nav-move -1))
    (should (equal agent-repl--sidebar-nav-dir "/c"))))

(ert-deftest agent-repl-test-sidebar-nav-wraps ()
  "Advancing past the last row wraps to the first."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-flat-dirs '("/a" "/b")
          agent-repl--sidebar-nav-dir "/b")
    (cl-letf (((symbol-function 'agent-repl--sidebar-open-dir) (lambda (_dir))))
      (agent-repl--sidebar-nav-move 1))
    (should (equal agent-repl--sidebar-nav-dir "/a"))))

(ert-deftest agent-repl-test-sidebar-nav-move-opens-landed-dir ()
  "Moving auto-selects: the landed row opens through the shared open path."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-flat-dirs '("/a" "/b")
          agent-repl--sidebar-nav-dir "/a")
    (let (opened)
      (cl-letf (((symbol-function 'agent-repl--sidebar-open-dir)
                 (lambda (dir) (setq opened dir))))
        (agent-repl--sidebar-nav-move 1))
      (should (equal opened "/b")))))

(ert-deftest agent-repl-test-sidebar-nav-show-info-without-cursor-errors ()
  "Showing info with no cursor raises `user-error'."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl-sidebar-nav-show-info) :type 'user-error)))

(ert-deftest agent-repl-test-sidebar-nav-show-info-expands-cursor-dir ()
  "Showing info toggles the cursor row's detail panel via the expand push."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-nav-dir "/tmp/ws")
    (let (expanded)
      (cl-letf (((symbol-function 'agent-repl--sidebar-expand-push)
                 (lambda (dir) (setq expanded dir))))
        (agent-repl-sidebar-nav-show-info))
      (should (equal expanded "/tmp/ws")))))

;;;; ---- Opening -------------------------------------------------------------

(ert-deftest agent-repl-test-sidebar-open-dir-unknown-errors ()
  "Opening a dir no live entry matches signals loudly."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--sidebar-open-dir "/tmp/nowhere"))))

(ert-deftest agent-repl-test-sidebar-open-dir-dispatches-picker-payload ()
  "Opening routes the entry through `agent-repl--picker-open-selection'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let (payload)
      (cl-letf (((symbol-function 'agent-repl--picker-open-selection)
                 (lambda (p) (setq payload p)))
                ((symbol-function 'agent-repl--sidebar-push) (lambda ()))
                ((symbol-function 'agent-repl--frontend-boot-session)
                 (lambda (&rest _))))
        (agent-repl--sidebar-open-dir "/tmp/ws"))
      (should (equal (plist-get payload :name) "ws"))
      (should (equal (plist-get payload :project-dir) "/tmp/ws"))
      (should (eq (plist-get payload :live-p) t)))))

(ert-deftest agent-repl-test-sidebar-open-dir-boots-agent-session ()
  "Opening starts agent-repl in the workspace, not just the workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let (booted)
      (cl-letf (((symbol-function 'agent-repl--picker-open-selection)
                 (lambda (_payload)))
                ((symbol-function 'agent-repl--sidebar-push) (lambda ()))
                ((symbol-function 'agent-repl--frontend-boot-session)
                 (lambda (ws &optional dir &rest _) (setq booted (cons ws dir)))))
        (agent-repl--sidebar-open-dir "/tmp/ws"))
      (should (equal booted '("ws" . "/tmp/ws"))))))

(ert-deftest agent-repl-test-sidebar-open-dir-arms-show-when-perspless ()
  "A perspective-less (closed) target arms `:pending-show-panels' so it opens."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--picker-open-selection) (lambda (_p)))
              ((symbol-function 'agent-repl--frontend-boot-session) (lambda (&rest _)))
              ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
      (agent-repl--sidebar-open-dir "/tmp/ws"))
    (should (eq (agent-repl--ws-get "ws" :pending-show-panels) t))))

(ert-deftest agent-repl-test-sidebar-open-dir-arms-show-when-repl-torn-down ()
  "An open perspective with an `:inactive' REPL still arms `:pending-show-panels'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :inactive)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--picker-open-selection) (lambda (_p)))
              ((symbol-function 'agent-repl--frontend-boot-session) (lambda (&rest _)))
              ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
      (agent-repl--sidebar-open-dir "/tmp/ws"))
    (should (eq (agent-repl--ws-get "ws" :pending-show-panels) t))))

(ert-deftest agent-repl-test-sidebar-open-dir-leaves-open-target-unarmed ()
  "An already-open (hosted) target is not armed — its panels restore on switch."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :active)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--picker-open-selection) (lambda (_p)))
              ((symbol-function 'agent-repl--frontend-boot-session) (lambda (&rest _)))
              ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
      (agent-repl--sidebar-open-dir "/tmp/ws"))
    (should-not (agent-repl--ws-get "ws" :pending-show-panels))))

(ert-deftest agent-repl-test-sidebar-open-dir-arms-show-before-switch ()
  "The show flag is armed BEFORE `--picker-open-selection' runs the switch."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :inactive)
    (let (armed-at-switch)
      (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--picker-open-selection)
                 (lambda (_p)
                   (setq armed-at-switch
                         (agent-repl--ws-get "ws" :pending-show-panels))))
                ((symbol-function 'agent-repl--frontend-boot-session) (lambda (&rest _)))
                ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
        (agent-repl--sidebar-open-dir "/tmp/ws"))
      (should (eq armed-at-switch t)))))

(ert-deftest agent-repl-test-sidebar-entry-for-dir-canonicalizes ()
  "Dir matching survives trailing-slash variance via canonicalization."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws/")
    (should (equal (car (agent-repl--sidebar-entry-for-dir "/tmp/ws"))
                   "ws"))))

;;;; ---- Recently merged ------------------------------------------------------

(defun agent-repl-test--roster-repo-names (roster)
  "Return every workspace name rendered in ROSTER's by-repo sections."
  (apply #'append
         (mapcar (lambda (g)
                   (mapcar (lambda (r) (plist-get r :name))
                           (append (plist-get g :rows) nil)))
                 (append (plist-get roster :repos) nil))))

(ert-deftest agent-repl-test-sidebar-merged-excluded-from-repo-groups ()
  "A merged workspace leaves the by-repo list entirely."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "live" "/tmp/live")
    (agent-repl-test--sidebar-ws "gone" "/tmp/gone"
                                 :repl-state :merged
                                 :merge-completed-at (float-time))
    (let ((names (agent-repl-test--roster-repo-names
                  (car (agent-repl--sidebar-build)))))
      (should (member "live" names))
      (should-not (member "gone" names)))))

(ert-deftest agent-repl-test-sidebar-merged-listed-newest-first ()
  "Recently merged rows sort by merge time, newest first."
  (agent-repl-test--with-clean-state
    (let ((now (float-time)))
      (agent-repl-test--sidebar-ws "older" "/tmp/older"
                                   :repl-state :merged :merge-completed-at (- now 100))
      (agent-repl-test--sidebar-ws "newer" "/tmp/newer"
                                   :repl-state :merged :merge-completed-at (- now 10))
      (let* ((roster (car (agent-repl--sidebar-build)))
             (rows (append (plist-get (plist-get roster :recentlyMerged) :rows) nil)))
        (should (equal (mapcar (lambda (r) (plist-get r :name)) rows)
                       '("newer" "older")))))))

(ert-deftest agent-repl-test-sidebar-merged-before-epoch-hidden ()
  "A merge older than the epoch renders nowhere at all."
  (agent-repl-test--with-clean-state
    (let* ((now (float-time))
           (agent-repl--sidebar-merged-epoch now))
      (agent-repl-test--sidebar-ws "stale" "/tmp/stale"
                                   :repl-state :merged :merge-completed-at (- now 1000))
      (let ((roster (car (agent-repl--sidebar-build))))
        (should (eq (plist-get roster :recentlyMerged) :null))
        (should-not (member "stale" (agent-repl-test--roster-repo-names roster)))))))

(ert-deftest agent-repl-test-sidebar-merged-window-wipes-past-gap ()
  "An activity gap beyond the window bumps the epoch, wiping the section."
  (agent-repl-test--with-clean-state
    (let* ((now (float-time))
           (agent-repl--sidebar-last-activity
            (- now agent-repl-sidebar-merged-window-seconds 1))
           (agent-repl--sidebar-merged-epoch nil)
           (agent-repl--sidebar-merged-persisted-at now))
      (cl-letf (((symbol-function 'agent-repl--sidebar-save-merged-window)
                 (lambda ())))
        (agent-repl--sidebar-refresh-merged-window))
      (should agent-repl--sidebar-merged-epoch))))

(ert-deftest agent-repl-test-sidebar-merged-window-holds-within-gap ()
  "An activity gap inside the window leaves the epoch untouched."
  (agent-repl-test--with-clean-state
    (let* ((now (float-time))
           (agent-repl--sidebar-last-activity (- now 5))
           (agent-repl--sidebar-merged-epoch nil)
           (agent-repl--sidebar-merged-persisted-at now))
      (cl-letf (((symbol-function 'agent-repl--sidebar-save-merged-window)
                 (lambda ())))
        (agent-repl--sidebar-refresh-merged-window))
      (should-not agent-repl--sidebar-merged-epoch))))

;;;; ---- Command handlers -----------------------------------------------------

(ert-deftest agent-repl-test-sidebar-switch-command-missing-dir-errors ()
  "A switch command without `dir' signals instead of no-oping."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-switch-command '((type . "switch"))))))

(ert-deftest agent-repl-test-sidebar-switch-command-opens-dir ()
  "A switch command opens its dir through the shared open path."
  (agent-repl-test--with-clean-state
    (let (opened)
      (cl-letf (((symbol-function 'agent-repl--sidebar-open-dir)
                 (lambda (dir) (setq opened dir))))
        (agent-repl--handle-switch-command
         '((type . "switch") (dir . "/tmp/ws"))))
      (should (equal opened "/tmp/ws")))))

(ert-deftest agent-repl-test-sidebar-fold-command-missing-key-errors ()
  "A fold command without `repo_key' signals."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-fold-command
                   '((type . "fold") (folded . t))))))

(ert-deftest agent-repl-test-sidebar-fold-command-missing-folded-errors ()
  "A fold command without `folded' signals — absent is not false."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (should-error (agent-repl--handle-fold-command
                   '((type . "fold") (repo_key . "/repos/doom/.git"))))))

(ert-deftest agent-repl-test-sidebar-fold-command-unknown-key-errors ()
  "A fold command naming no live workspace's repo signals."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (should-error (agent-repl--handle-fold-command
                   '((type . "fold") (repo_key . "/repos/foreign/.git")
                     (folded . t))))))

(ert-deftest agent-repl-test-sidebar-fold-command-folds ()
  "A fold command with folded=true folds the repo, redraws, and pushes."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((redrawn 0) (pushes 0))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redrawn)))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--handle-fold-command
         '((type . "fold") (repo_key . "/repos/doom/.git") (folded . t))))
      (should (agent-repl--repo-folded-p "/repos/doom/.git"))
      (should (= redrawn 1))
      (should (= pushes 1)))))

(ert-deftest agent-repl-test-sidebar-fold-command-unfolds-on-json-false ()
  "A fold command with folded=:json-false (json-read false) unfolds."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl--toggle-repo-fold "/repos/doom/.git")
    (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda ()))
              ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
      (agent-repl--handle-fold-command
       '((type . "fold") (repo_key . "/repos/doom/.git")
         (folded . :json-false))))
    (should-not (agent-repl--repo-folded-p "/repos/doom/.git"))))

(ert-deftest agent-repl-test-sidebar-fold-command-idempotent ()
  "A fold command asking for the current state toggles nothing."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl--toggle-repo-fold "/repos/doom/.git")
    (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda ()))
              ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
      (agent-repl--handle-fold-command
       '((type . "fold") (repo_key . "/repos/doom/.git") (folded . t))))
    (should (agent-repl--repo-folded-p "/repos/doom/.git"))))

(provide 'test-sidebar)
;;; test-sidebar.el ends here
