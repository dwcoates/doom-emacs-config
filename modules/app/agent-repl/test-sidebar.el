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

(defun agent-repl-test--sidebar-task-group (roster key)
  "Return ROSTER's task group plist with KEY, or nil."
  (cl-find-if (lambda (g) (equal (plist-get g :key) key))
              (append (plist-get roster :tasks) nil)))

;;;; ---- Capturing what gets published ------------------------------------

(defvar agent-repl-test--sidebar-published nil
  "Every `(FIELD . PAYLOAD)' sent under `agent-repl-test--sidebar-with-link'.")

(defvar agent-repl-test--sidebar-tracked nil
  "Every tracked publish under the link mock: `(:request-id :field :on-failure)'.")

(defmacro agent-repl-test--sidebar-with-link (&rest body)
  "Run BODY with a live frontend link mocked and every publish captured.
Stands in for the UDS boundary only: `agent-repl--uds-connected-p' reports
open, sends are recorded instead of written, and tracked commands keep
their `:on-failure' callback reachable so a NACK can be delivered."
  (declare (indent 0) (debug t))
  `(let ((agent-repl-test--sidebar-published nil)
         (agent-repl-test--sidebar-tracked nil)
         (agent-repl-test--sidebar-request-n 0))
     (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
               ((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional _ws _proc)
                  (push (cons field payload) agent-repl-test--sidebar-published)
                  (format "req-%d" (cl-incf agent-repl-test--sidebar-request-n))))
               ((symbol-function 'agent-repl--uds-track-command)
                (lambda (request-id field _ws &optional on-failure &rest _)
                  (push (list :request-id request-id :field field
                              :on-failure on-failure)
                        agent-repl-test--sidebar-tracked)
                  request-id)))
       ,@body)))

(defvar agent-repl-test--sidebar-request-n 0
  "Request-id counter for the link mock, rebound per capture.")

(defun agent-repl-test--sidebar-rosters ()
  "Return every published `WorkspaceRoster' payload plist, oldest first."
  (mapcar (lambda (sent) (plist-get (cdr sent) :roster))
          (reverse agent-repl-test--sidebar-published)))

(defun agent-repl-test--sidebar-wire ()
  "Return the LAST published roster as parsed protojson.
Encoded and re-parsed on purpose: the assertion subject is the wire text
the daemon receives, not the plist that happens to produce it."
  (json-parse-string (json-encode (car (last (agent-repl-test--sidebar-rosters))))
                     :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun agent-repl-test--sidebar-wire-rows (wire)
  "Return the row vector of WIRE's first repository section."
  (alist-get 'rows (aref (alist-get 'sections (alist-get 'repository wire)) 0)))

(defmacro agent-repl-test--with-sidebar-frame (fire &rest body)
  "Run BODY capturing every scheduled roster flush into FIRE, newest first.
Models one event-loop frame: `agent-repl--sidebar-push' requests a flush
and nothing runs until a captured callback is funcalled, which is exactly
the window requests are supposed to coalesce inside."
  (declare (indent 1))
  `(let ((,fire nil))
     (setq agent-repl--sidebar-flush-pending nil)
     (cl-letf (((symbol-function 'agent-repl--sidebar-run-timer)
                (lambda (_seconds callback) (push callback ,fire) 'timer)))
       ,@body)))

(defun agent-repl-test--sidebar-publish-now ()
  "Force one gated roster publish and return whatever the flush returned.
Invalidates the last signature first, so a test about the PUBLISH (the
wire shape, the revision counter, the ack tracking) is never silently
turned into a test about the gate."
  (agent-repl--sidebar-invalidate)
  (agent-repl--sidebar-flush))

(defun agent-repl-test--sidebar-publish-once (&rest plist)
  "Register one workspace from PLIST, publish, and return the parsed wire.
PLIST is passed to `agent-repl-test--sidebar-ws' after NAME and DIR,
which default to \"ws\" and \"/tmp/ws\".  The perspective is held open so
rows carry a live lifecycle rather than the perspective-less shortcut."
  (apply #'agent-repl-test--sidebar-ws "ws" "/tmp/ws" plist)
  (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (agent-repl-test--sidebar-wire))))

(defun agent-repl-test--sidebar-task (id title &rest plist)
  "Register a task ID titled TITLE directly in the task hash, return ID.
PLIST key/value pairs (e.g. `:done t') are merged onto the task; the
suite's `agent-repl--tasks-loaded' is already t (test-helpers.el), so
this never touches disk."
  (puthash id (append plist
                      (list :id id :title title :done nil :created-at 0.0))
           agent-repl--tasks)
  id)

;;;; ---- Wire status mapping ----------------------------------------------

(ert-deftest agent-repl-test-sidebar-wire-status-table ()
  "Each mapped render-state keyword serializes to its contract string.
Perspective is held open so the render-state mapping is exercised rather
than the perspective-less short-circuit."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (dolist (case '((:thinking . "thinking") (:permission . "permission")
                      (:init . "init") (:done . "done")
                      (:ready . "ready") (:idle . "ready")
                      (:idle-async . "idle-async")
                      (:vendor-blocked . "vendor-blocked")
                      (:start-failed . "start-failed")
                      (:degraded . "degraded") (:dead . "dead")
                      (:merging . "merging") (:merge-queued . "merge-queued")
                      (:merge-conflict . "merge-conflict")
                      (:merge-failed . "merge-failed") (:merged . "merged")))
        (cl-letf (((symbol-function 'agent-repl--ws-render-status)
                   (lambda (_ws) (car case))))
          (should (equal (agent-repl--sidebar-wire-status "ws") (cdr case))))))))

(ert-deftest agent-repl-test-sidebar-wire-status-done-is-plain-done ()
  "A :done workspace serializes as \"done\", viewed or not.
There is no viewed axis any more: `:done', `:ready' and `:idle' are all
green, so tracking whether the user had looked at a `:done' changed the
dot without changing anything true."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-render-status) (lambda (_ws) :done)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "done")))))

(ert-deftest agent-repl-test-sidebar-wire-status-done-viewed-is-gone ()
  "\"done-viewed\" is no longer a wire status the sidebar can emit."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :done-acked t)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-render-status) (lambda (_ws) :done)))
      (should-not (equal (agent-repl--sidebar-wire-status "ws") "done-viewed")))))

(ert-deftest agent-repl-test-sidebar-wire-status-nil-is-none ()
  "A nil render status (tombstoned workspace) serializes as \"none\"."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-render-status) (lambda (_ws) nil)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "none")))))

(ert-deftest agent-repl-test-sidebar-wire-status-perspective-less-merged-is-merged ()
  "A perspective-less `:merged' workspace still serializes as \"merged\".
`:merged' is perspective-independent in
`agent-repl--sidebar-status-wire': the row is reporting concluded work,
not tab-bar membership, so `agent-repl--ws-open-p' nil no longer
overrides it — that override was exactly the defect that hid the
Recently Merged dot behind a question mark."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-render-status) (lambda (_ws) :merged)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "merged")))))

(ert-deftest agent-repl-test-sidebar-wire-status-perspective-less-live-is-inactive ()
  "A perspective-less LIVE status still serializes as \"inactive\".
`:ready' is perspective-bound in `agent-repl--sidebar-status-wire', so
losing the tab-bar membership still dominates: there is no live session
whose lifecycle a status dot could report."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-render-status) (lambda (_ws) :ready)))
      (should (equal (agent-repl--sidebar-wire-status "ws") "inactive")))))

(ert-deftest agent-repl-test-sidebar-wire-status-unmapped-errors ()
  "An unmapped render-state keyword signals instead of defaulting."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-render-status)
               (lambda (_ws) :not-a-state)))
      (should-error (agent-repl--sidebar-wire-status "ws")))))

(ert-deftest agent-repl-test-sidebar-status-wire-every-entry-classified ()
  "Every `agent-repl--sidebar-status-wire' entry names a boolean flag.
A newly added render state whose entry omits the
PERSPECTIVE-INDEPENDENT-P cell would leave `agent-repl--sidebar-wire-status'
unable to decide whether it needs a live perspective — this pins that
every entry lands on exactly one side of the rule rather than silently
falling through as neither."
  (dolist (entry agent-repl--sidebar-status-wire)
    (let ((row (cdr entry)))
      (should (stringp (car row)))
      (should (memq (cdr row) '(nil t))))))

(ert-deftest agent-repl-test-sidebar-wire-row-merged-perspective-less-publishes-merged-arm ()
  "A merged, perspective-less workspace publishes the `merged' arm, not `inactive'.
End-to-end reproduction of the fixed defect: `preserve-entry' leaves the
merge landed but the perspective gone, and the row must still draw the
merged dot instead of the inactive question mark."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "merged" "/tmp/merged"
                                 :pushed-render-state :merged
                                 :merge-completed-at 100.0)
    (agent-repl-test--sidebar-with-persps '("other")
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let* ((wire (agent-repl-test--sidebar-wire))
               (row (aref (alist-get 'rows (alist-get 'recentlyMerged wire)) 0)))
          (should (assq 'merged row))
          (should (null (assq 'inactive row))))))))

;;;; ---- The closed (greyed) predicate ---------------------------------------

(ert-deftest agent-repl-test-sidebar-closed-p-table ()
  "Only a torn-down REPL greys a row; perspective presence is irrelevant.
Amended per the user's ruling that greying is RESERVED for the
panels-dismissed condition: the `(nil :active)' row used to expect
`t' (perspective-less was closed) and now expects nil, because a
perspective-less workspace loses its row entirely instead of greying."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (dolist (case '((t :active nil) (t :inactive t)
                    (t nil nil) (nil :active nil)
                    (nil :inactive t) (nil nil nil)))
      (cl-destructuring-bind (open repl expected) case
        (agent-repl--ws-put "ws" :repl-state repl)
        (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                   (lambda (_ws) open)))
          (should (eq (agent-repl--sidebar-closed-p "ws") expected)))))))

(ert-deftest agent-repl-test-sidebar-closed-p-merged-stays-greyed ()
  "A merged workspace greys even though its REPL is not `:inactive'.
Merged rows outlive their tab inside Recently Merged, and the ruling
keeps that section's rendering unchanged.  Merged-ness is the durable
merge instant, so that is what the arrangement carries."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :merge-completed-at 100.0)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil)))
      (should (eq (agent-repl--sidebar-closed-p "ws") t)))))

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
    (agent-repl--ws-put "dirless" :agent-state :idle)
    (should (equal (agent-repl--sidebar-entries)
                   '(("ws" . "/tmp/ws"))))))

;;;; ---- Roster membership per workspace state -------------------------------
;;
;; The membership axis the user's ruling settled: a workspace that left
;; the tab bar loses its row, a workspace that still has its tab keeps
;; one (greyed when its panes are dismissed), and merged rows are
;; exempt.  Each test below pins exactly one of those states.  The
;; fixtures bind `persp-names-cache' so the roster consults perspective
;; membership rather than falling back to plain registration.

(defmacro agent-repl-test--sidebar-with-persps (names &rest body)
  "Run BODY with `persp-names-cache' bound to NAMES (a list of strings)."
  (declare (indent 1))
  `(let ((persp-names-cache ,names))
     ,@body))

(ert-deftest agent-repl-test-sidebar-entries-drops-perspective-less-workspace ()
  "A live registration whose perspective was killed leaves the roster."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "kept" "/tmp/kept")
    (agent-repl-test--sidebar-ws "gone" "/tmp/gone")
    (agent-repl-test--sidebar-with-persps '("kept")
      (should (equal (agent-repl--sidebar-entries)
                     '(("kept" . "/tmp/kept")))))))

(ert-deftest agent-repl-test-sidebar-entries-drops-tombstoned-workspace ()
  "A tombstoned (nuked) registration leaves the roster even with a persp."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "kept" "/tmp/kept")
    (agent-repl-test--sidebar-ws "tomb" "/tmp/tomb" :nuked-at 1.0)
    (agent-repl-test--sidebar-with-persps '("kept" "tomb")
      (should (equal (agent-repl--sidebar-entries)
                     '(("kept" . "/tmp/kept")))))))

(ert-deftest agent-repl-test-sidebar-entries-keeps-panes-closed-workspace ()
  "A workspace that still has its tab but no panes stays in the roster."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :inactive)
    (agent-repl-test--sidebar-with-persps '("ws")
      (should (equal (agent-repl--sidebar-entries)
                     '(("ws" . "/tmp/ws")))))))

(ert-deftest agent-repl-test-sidebar-panes-closed-workspace-renders-greyed ()
  "The panes-closed row that survives is the one that renders greyed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :inactive)
    (agent-repl-test--sidebar-with-persps '("ws")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git"))
             (row (agent-repl-test--sidebar-row (plist-get repo :rows) "ws")))
        (should (eq (plist-get row :closed) t))))))

(ert-deftest agent-repl-test-sidebar-entries-keeps-closed-session-live-workspace ()
  "A tabbed workspace whose session never started keeps an ungreyed row."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state nil)
    (agent-repl-test--sidebar-with-persps '("ws")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git"))
             (row (agent-repl-test--sidebar-row (plist-get repo :rows) "ws")))
        (should row)
        (should (eq (plist-get row :closed) :false))))))

(ert-deftest agent-repl-test-sidebar-entries-keeps-perspective-less-merged ()
  "A merged workspace survives losing its perspective — the ruling's exemption."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "merged" "/tmp/merged" :merge-completed-at 100.0)
    (agent-repl-test--sidebar-with-persps '("other")
      (should (equal (agent-repl--sidebar-entries)
                     '(("merged" . "/tmp/merged")))))))

(ert-deftest agent-repl-test-sidebar-merged-section-survives-perspective-kill ()
  "The Recently Merged section still renders a merged, perspective-less row."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "merged" "/tmp/merged" :merge-completed-at 100.0)
    (agent-repl-test--sidebar-with-persps '("other")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (recent (plist-get roster :recentlyMerged))
             (row (agent-repl-test--sidebar-row (plist-get recent :rows) "merged")))
        (should row)
        (should (eq (plist-get row :closed) t))))))

(ert-deftest agent-repl-test-sidebar-folded-repo-keeps-its-rows ()
  "Folding a repo collapses the section without dropping its rows."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (puthash "/repos/doom/.git" t agent-repl--folded-repos)
    (agent-repl-test--sidebar-with-persps '("ws")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git")))
        (should (eq (plist-get repo :folded) t))
        (should (agent-repl-test--sidebar-row (plist-get repo :rows) "ws"))))))

(ert-deftest agent-repl-test-sidebar-kill-repaints-without-the-killed-row ()
  "Killing a workspace publishes a roster that no longer carries its row.
The publish is REQUESTED by the kill itself
\(`agent-repl--ws-repaint-sidebar') and lands on the frame\='s flush, so
the row goes with the tab rather than lingering until the next tick."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "kept" "/tmp/kept")
    (agent-repl-test--sidebar-ws "doomed" "/tmp/doomed")
    (let ((persp-names-cache '("kept" "doomed")))
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--with-sidebar-frame fire
          (cl-letf ((;; The kill: the persp leaves the cache, exactly as
                     ;; `+workspace/kill' would make it.
                     (symbol-function '+workspace/kill)
                     (lambda (ws) (setq persp-names-cache (delete ws persp-names-cache)))))
            (agent-repl--ws-kill "doomed"))
          (should (= (length fire) 1))
          (funcall (car fire)))
        (let ((published (json-encode (car (last (agent-repl-test--sidebar-rosters))))))
          (should (= (length agent-repl-test--sidebar-published) 1))
          (should (string-match-p "/tmp/kept" published))
          (should-not (string-match-p "/tmp/doomed" published)))))))

(ert-deftest agent-repl-test-sidebar-entries-keeps-all-when-persp-cache-unusable ()
  "With no usable persp cache the roster falls back to plain registration.
Absence of evidence is not evidence of absence: a nil cache would
otherwise answer `not open' for every workspace and empty the sidebar."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "a" "/tmp/a")
    (agent-repl-test--sidebar-ws "b" "/tmp/b")
    (agent-repl-test--sidebar-with-persps nil
      (should (equal (sort (mapcar #'car (agent-repl--sidebar-entries)) #'string<)
                     '("a" "b"))))))

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

;;;; ---- View selector + task view ------------------------------------------

(ert-deftest agent-repl-test-sidebar-build-default-view-is-repository ()
  "The default build reports the repository view with an empty task array."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "a" "/tmp/a")
    (let ((roster (car (agent-repl--sidebar-build))))
      (should (equal (plist-get roster :view) "repository"))
      (should (equal (append (plist-get roster :tasks) nil) nil))
      (should (= 1 (length (plist-get roster :repos)))))))

(ert-deftest agent-repl-test-sidebar-build-task-view-reports-view ()
  "The task view reports view=task with an empty repo array."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (agent-repl-test--sidebar-ws "a" "/tmp/a")
    (let ((roster (car (agent-repl--sidebar-build))))
      (should (equal (plist-get roster :view) "task"))
      (should (equal (append (plist-get roster :repos) nil) nil)))))

(ert-deftest agent-repl-test-sidebar-build-task-view-groups-under-task ()
  "A workspace assigned to a task roots under that task's section."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (let ((id (agent-repl-test--sidebar-task "t1" "Ship it")))
      (agent-repl-test--sidebar-ws "a" "/tmp/a" :task-id id)
      (let* ((roster (car (agent-repl--sidebar-build)))
             (group (agent-repl-test--sidebar-task-group roster id)))
        (should (equal (plist-get group :label) "Ship it"))
        (should (agent-repl-test--sidebar-row (plist-get group :rows) "a"))))))

(ert-deftest agent-repl-test-sidebar-build-task-view-empty-task-renders ()
  "A task with no workspaces still renders as a zero-row section."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (let ((id (agent-repl-test--sidebar-task "t1" "Empty")))
      (let* ((roster (car (agent-repl--sidebar-build)))
             (group (agent-repl-test--sidebar-task-group roster id)))
        (should group)
        (should (= 0 (length (plist-get group :rows))))))))

(ert-deftest agent-repl-test-sidebar-build-task-view-unassigned-under-no-task ()
  "A workspace in no task collects under the No task catch-all section."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (agent-repl-test--sidebar-ws "loner" "/tmp/loner")
    (let* ((roster (car (agent-repl--sidebar-build)))
           (group (agent-repl-test--sidebar-task-group
                   roster agent-repl--sidebar-no-task-key)))
      (should (equal (plist-get group :label) "No task"))
      (should (agent-repl-test--sidebar-row (plist-get group :rows) "loner")))))

(ert-deftest agent-repl-test-sidebar-build-task-view-no-task-sections-last ()
  "The No task catch-all section sorts after every real task."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (let ((id (agent-repl-test--sidebar-task "t1" "Real" :created-at 1.0)))
      (agent-repl-test--sidebar-ws "a" "/tmp/a" :task-id id)
      (agent-repl-test--sidebar-ws "loner" "/tmp/loner")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (keys (mapcar (lambda (g) (plist-get g :key))
                           (append (plist-get roster :tasks) nil))))
        (should (equal keys (list id agent-repl--sidebar-no-task-key)))))))

(ert-deftest agent-repl-test-sidebar-build-task-view-done-flag ()
  "A done task's section serializes done=true; an open one done=false."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (let ((done-id (agent-repl-test--sidebar-task "d" "Done" :done t))
          (open-id (agent-repl-test--sidebar-task "o" "Open")))
      (agent-repl-test--sidebar-ws "a" "/tmp/a" :task-id done-id)
      (agent-repl-test--sidebar-ws "b" "/tmp/b" :task-id open-id)
      (let* ((roster (car (agent-repl--sidebar-build))))
        (should (eq (plist-get (agent-repl-test--sidebar-task-group roster done-id)
                               :done)
                    t))
        (should (eq (plist-get (agent-repl-test--sidebar-task-group roster open-id)
                               :done)
                    :false))))))

(ert-deftest agent-repl-test-sidebar-build-task-view-child-inherits-parent-task ()
  "A child workspace nests under its parent's task section, inheriting it."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (let ((id (agent-repl-test--sidebar-task "t1" "Family")))
      (agent-repl-test--sidebar-ws "parent" "/tmp/parent" :task-id id)
      (agent-repl-test--sidebar-ws "child" "/tmp/child"
                                   :source-ws-dir "/tmp/parent")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (group (agent-repl-test--sidebar-task-group roster id))
             (parent (agent-repl-test--sidebar-row (plist-get group :rows) "parent")))
        (should (agent-repl-test--sidebar-row (plist-get parent :children)
                                              "child"))))))

(ert-deftest agent-repl-test-sidebar-build-repo-groups-carry-done-false ()
  "Every repo section carries done=false for the uniform group contract."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "a" "/tmp/a")
    (let* ((roster (car (agent-repl--sidebar-build)))
           (repo (agent-repl-test--sidebar-repo roster "/repos/doom/.git")))
      (should (eq (plist-get repo :done) :false)))))

;;;; ---- Row serialization --------------------------------------------------

(ert-deftest agent-repl-test-sidebar-row-closed-when-repl-inactive ()
  "An open workspace whose REPL is `:inactive' serializes closed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :inactive)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" nil (vector))))
        (should (eq (plist-get row :closed) t))))))

(ert-deftest agent-repl-test-sidebar-row-not-closed-when-perspective-less ()
  "A perspective-less workspace no longer serializes closed.
Amended per the user's ruling: this test previously asserted
closed=true for a perspective-less row.  Such a row is now dropped from
the roster entirely (see the roster-membership tests), so the greyed
flag it used to carry would be a contradiction — the row does not exist
to be greyed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :repl-state :active)
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) nil)))
      (let ((row (agent-repl--sidebar-row-plist "ws" "/tmp/ws" nil (vector))))
        (should (eq (plist-get row :closed) :false))))))

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
                                 :pushed-render-state :thinking
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

(ert-deftest agent-repl-test-sidebar-push-refreshes-flat-dirs ()
  "A push rewrites `agent-repl--sidebar-flat-dirs' from the fresh build."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (setq agent-repl--sidebar-flat-dirs '("/stale"))
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now))
    (should (equal agent-repl--sidebar-flat-dirs
                   (list (agent-repl--path-canonical "/tmp/ws"))))))

;;;; ---- Publishing the roster to the daemon ---------------------------------

(ert-deftest agent-repl-test-sidebar-push-sends-the-publish-command ()
  "A push emits exactly one `publishWorkspaceRoster' frontend command."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (should (equal (mapcar #'car agent-repl-test--sidebar-published)
                     '("publishWorkspaceRoster"))))))

(ert-deftest agent-repl-test-sidebar-push-no-longer-executes-a-script ()
  "The per-webview injection path is GONE: a push evaluates no script.
The roster now reaches views through the daemon's retained copy, so an
execute-script here would be a second, divergent delivery channel."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl--ws-put "ws" :frontend-buffer (current-buffer))
    (let (scripts)
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                 (lambda (_buf script) (push script scripts))))
        (agent-repl-test--sidebar-with-link
          (agent-repl-test--sidebar-publish-now)))
      (should (null scripts)))))

(ert-deftest agent-repl-test-sidebar-roster-hook-constant-is-gone ()
  "The webview roster hook constant no longer exists to be pushed through."
  (should-not (boundp 'agent-repl--sidebar-roster-hook)))

(ert-deftest agent-repl-test-sidebar-push-script-builder-is-gone ()
  "The roster push-script builder no longer exists."
  (should-not (fboundp 'agent-repl--sidebar-push-script)))

(ert-deftest agent-repl-test-sidebar-publish-skipped-when-link-down ()
  "With no live link the publish is skipped rather than raised.
A daemon that is not there cannot receive a roster, and the reconnect
publish delivers a fresher one than the frame that could not be sent."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil)))
      (should (null (agent-repl-test--sidebar-publish-now))))))

(ert-deftest agent-repl-test-sidebar-publish-holds-revision-when-link-down ()
  "A skipped publish consumes no revision, so the counter stays truthful."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil)))
      (agent-repl-test--sidebar-publish-now))
    (should (= agent-repl--sidebar-roster-revision 0))))

;;;; ---- The published protojson shape ---------------------------------------

(ert-deftest agent-repl-test-sidebar-wire-repository-view-arm ()
  "The repository grouping is carried as the `repository' oneof arm."
  (agent-repl-test--with-clean-state
    (let ((wire (agent-repl-test--sidebar-publish-once)))
      (should (alist-get 'repository wire))
      (should (null (alist-get 'task wire))))))

(ert-deftest agent-repl-test-sidebar-wire-task-view-arm ()
  "The task grouping is carried as the `task' oneof arm, excluding the other."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-view :task)
    (let ((wire (agent-repl-test--sidebar-publish-once)))
      (should (alist-get 'task wire))
      (should (null (alist-get 'repository wire))))))

(ert-deftest agent-repl-test-sidebar-wire-carries-no-view-mode-field ()
  "The set arm IS the grouping: no `view' field rides beside it."
  (agent-repl-test--with-clean-state
    (should (null (alist-get 'view (agent-repl-test--sidebar-publish-once))))))

(ert-deftest agent-repl-test-sidebar-wire-repo-section-key ()
  "A repository section carries its fold key as `repoKey'."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (section (aref (alist-get 'sections (alist-get 'repository wire)) 0)))
      (should (equal (alist-get 'repoKey section) "/repos/doom/.git")))))

(ert-deftest agent-repl-test-sidebar-wire-folded-section-keeps-its-rows ()
  "A folded section publishes folded=true AND still carries its rows.
Folding is display state, so unfolding must need no republish."
  (agent-repl-test--with-clean-state
    (agent-repl--toggle-repo-fold "/repos/doom/.git")
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (section (aref (alist-get 'sections (alist-get 'repository wire)) 0)))
      (should (eq (alist-get 'folded section) t))
      (should (= 1 (length (alist-get 'rows section)))))))

(ert-deftest agent-repl-test-sidebar-wire-unfolded-section-omits-folded ()
  "An unfolded section omits `folded' entirely: proto3 drops a false bool."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (section (aref (alist-get 'sections (alist-get 'repository wire)) 0)))
      (should (null (assq 'folded section))))))

(ert-deftest agent-repl-test-sidebar-wire-task-section-identity-and-title ()
  "A task section carries the task id as identity and the title for display."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-task "t1" "Ship the roster")
    (setq agent-repl--sidebar-view :task)
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :task-id "t1")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let* ((wire (agent-repl-test--sidebar-wire))
               (section (cl-find-if (lambda (s) (equal (alist-get 'taskId s) "t1"))
                                    (append (alist-get 'sections
                                                       (alist-get 'task wire))
                                            nil))))
          (should section)
          (should (equal (alist-get 'title section) "Ship the roster")))))))

(ert-deftest agent-repl-test-sidebar-wire-task-section-done ()
  "A completed task section publishes done=true."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-task "t1" "Done task" :done t)
    (setq agent-repl--sidebar-view :task)
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws" :task-id "t1")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let* ((wire (agent-repl-test--sidebar-wire))
               (section (cl-find-if (lambda (s) (equal (alist-get 'taskId s) "t1"))
                                    (append (alist-get 'sections
                                                       (alist-get 'task wire))
                                            nil))))
          (should (eq (alist-get 'done section) t)))))))

(ert-deftest agent-repl-test-sidebar-wire-row-identity-fields ()
  "A row carries its dir (identity) and name (display)."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (equal (alist-get 'dir row) (agent-repl--path-canonical "/tmp/ws")))
      (should (equal (alist-get 'name row) "ws")))))

(ert-deftest agent-repl-test-sidebar-wire-row-has-no-status-field ()
  "A row carries NO `status' field: the set arm is the whole assertion."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :pushed-render-state :thinking))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (null (assq 'status row))))))

(ert-deftest agent-repl-test-sidebar-wire-row-status-arm-is-empty-object ()
  "The status arm's value is the empty object every arm message is."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :pushed-render-state :thinking))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (equal (alist-get 'thinking row) nil)))))

(ert-deftest agent-repl-test-sidebar-wire-row-sets-exactly-one-status-arm ()
  "Exactly one arm is set: two lifecycles is no more a lifecycle than none."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :pushed-render-state :thinking))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0))
           (arms (cl-count-if (lambda (cell)
                                (rassoc (car cell)
                                        (mapcar (lambda (m)
                                                  (cons (car m)
                                                        (intern (substring
                                                                 (symbol-name (cdr m)) 1))))
                                                agent-repl--sidebar-status-arm)))
                              row)))
      (should (= arms 1)))))

(ert-deftest agent-repl-test-sidebar-wire-row-camel-case-arm-idle-async ()
  "A kebab-case wire status publishes under its lowerCamel arm key."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :pushed-render-state :idle-async))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (assq 'idleAsync row))
      (should (null (assq 'idle-async row))))))

(ert-deftest agent-repl-test-sidebar-wire-row-camel-case-arm-merge-conflict ()
  "A merge-conflict row publishes under the `mergeConflict' arm key."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :pushed-render-state :merge-conflict))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (assq 'mergeConflict row)))))

(ert-deftest agent-repl-test-sidebar-wire-row-inactive-arm ()
  "A perspective-less row publishes the `inactive' arm."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (let* ((wire (agent-repl-test--sidebar-wire))
             (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
        (should (assq 'inactive row))))))

(ert-deftest agent-repl-test-sidebar-status-arm-table-covers-every-wire-status ()
  "Every wire status the sidebar can emit maps to an arm key."
  (dolist (pair agent-repl--sidebar-status-wire)
    (should (agent-repl--sidebar-status-arm (cadr pair)))))

(ert-deftest agent-repl-test-sidebar-status-arm-unmapped-errors ()
  "An unmapped wire status signals rather than publishing an armless row."
  (should-error (agent-repl--sidebar-status-arm "not-a-status")))

(ert-deftest agent-repl-test-sidebar-wire-row-children-nest ()
  "A spawned child rides nested in its parent's `children'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "parent" "/tmp/parent")
    (agent-repl-test--sidebar-ws "child" "/tmp/child"
                                 :source-ws-dir "/tmp/parent")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t)))
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let* ((wire (agent-repl-test--sidebar-wire))
               (rows (agent-repl-test--sidebar-wire-rows wire))
               (parent (aref rows 0)))
          (should (= 1 (length rows)))
          (should (equal (alist-get 'name parent) "parent"))
          (should (equal (alist-get 'name (aref (alist-get 'children parent) 0))
                         "child")))))))

(ert-deftest agent-repl-test-sidebar-wire-current-dir ()
  "The current workspace's dir is published as `currentDir'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws")))
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (should (equal (alist-get 'currentDir (agent-repl-test--sidebar-wire))
                       (agent-repl--path-canonical "/tmp/ws")))))))

(ert-deftest agent-repl-test-sidebar-wire-current-dir-omitted-when-none ()
  "With no current workspace the empty `currentDir' is omitted."
  (agent-repl-test--with-clean-state
    (should (null (assq 'currentDir (agent-repl-test--sidebar-publish-once))))))

(ert-deftest agent-repl-test-sidebar-wire-nav-dir ()
  "The keyboard cursor's dir is published as `navDir'."
  (agent-repl-test--with-clean-state
    (setq agent-repl--sidebar-nav-dir "/tmp/ws")
    (should (equal (alist-get 'navDir (agent-repl-test--sidebar-publish-once))
                   "/tmp/ws"))))

(ert-deftest agent-repl-test-sidebar-wire-nav-dir-omitted-when-unset ()
  "With no cursor the empty `navDir' is omitted rather than sent as null."
  (agent-repl-test--with-clean-state
    (should (null (assq 'navDir (agent-repl-test--sidebar-publish-once))))))

;;;; ---- Row display fields --------------------------------------------------

(ert-deftest agent-repl-test-sidebar-epoch-ms-converts-seconds-to-milliseconds ()
  "A known `float-time' seconds value converts to its exact millisecond."
  (should (= (agent-repl--sidebar-epoch-ms 1750000000.123) 1750000000123)))

(ert-deftest agent-repl-test-sidebar-epoch-ms-absent-stamp-is-zero ()
  "The build's `:null' for an absent stamp converts to the contract's 0."
  (should (= (agent-repl--sidebar-epoch-ms :null) 0)))

(ert-deftest agent-repl-test-sidebar-wire-row-last-viewed-at-ms ()
  "A viewed row publishes its view stamp in MILLISECONDS, not seconds."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once :last-viewed-at 1000.5))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (= (alist-get 'lastViewedAtMs row) 1000500)))))

(ert-deftest agent-repl-test-sidebar-wire-row-never-viewed-omits-last-viewed-at-ms ()
  "A never-viewed row omits the stamp: proto3 drops the 0 that means never."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (null (assq 'lastViewedAtMs row))))))

(ert-deftest agent-repl-test-sidebar-wire-row-merged-at-ms ()
  "A merged row publishes its merge instant in MILLISECONDS."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "merged" "/tmp/merged" :merge-completed-at 100.0)
    (agent-repl-test--sidebar-with-persps '("other")
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let* ((wire (agent-repl-test--sidebar-wire))
               (row (aref (alist-get 'rows (alist-get 'recentlyMerged wire)) 0)))
          (should (= (alist-get 'mergedAtMs row) 100000)))))))

(ert-deftest agent-repl-test-sidebar-wire-row-not-merged-omits-merged-at-ms ()
  "An unmerged row omits the stamp: proto3 drops the 0 that means not merged."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (null (assq 'mergedAtMs row))))))

(ert-deftest agent-repl-test-sidebar-wire-row-branch ()
  "A row publishes its own git branch for the detail panel."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once :branch-name "feat/roster"))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (equal (alist-get 'branch row) "feat/roster")))))

(ert-deftest agent-repl-test-sidebar-wire-row-unknown-branch-omitted ()
  "An unknown branch is omitted, which reads back as the empty string."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (null (assq 'branch row))))))

(ert-deftest agent-repl-test-sidebar-wire-row-parent-branch ()
  "A row publishes the branch it was cut from as `parentBranch'."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :parent-branch-name "master"))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (equal (alist-get 'parentBranch row) "master")))))

(ert-deftest agent-repl-test-sidebar-wire-row-summary ()
  "A row publishes its last prompt summary as the detail panel's prose line."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once
                  :last-prompt-summary "wiring the roster"))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (equal (alist-get 'summary row) "wiring the roster")))))

(ert-deftest agent-repl-test-sidebar-wire-row-absent-summary-omitted ()
  "No summary is omitted rather than published as JSON null."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (null (assq 'summary row))))))

(ert-deftest agent-repl-test-sidebar-wire-row-closed ()
  "A panels-dismissed row publishes closed=true beside its status arm."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once :repl-state :inactive))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (eq (alist-get 'closed row) t)))))

(ert-deftest agent-repl-test-sidebar-wire-row-open-omits-closed ()
  "An open row omits `closed' entirely: proto3 drops a false bool."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (row (aref (agent-repl-test--sidebar-wire-rows wire) 0)))
      (should (null (assq 'closed row))))))

;;;; ---- Section display fields ----------------------------------------------

(ert-deftest agent-repl-test-sidebar-wire-repo-section-label ()
  "A repository section carries its display label beside its fold key."
  (agent-repl-test--with-clean-state
    (let* ((wire (agent-repl-test--sidebar-publish-once))
           (section (aref (alist-get 'sections (alist-get 'repository wire)) 0)))
      (should (equal (alist-get 'label section) "doom")))))

(ert-deftest agent-repl-test-sidebar-wire-merged-section-label ()
  "The Recently Merged section carries its heading, authored in one place."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "merged" "/tmp/merged" :merge-completed-at 100.0)
    (agent-repl-test--sidebar-with-persps '("other")
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let ((section (alist-get 'recentlyMerged
                                  (agent-repl-test--sidebar-wire))))
          (should (equal (alist-get 'label section) "Recently Merged")))))))

(ert-deftest agent-repl-test-sidebar-wire-merged-section-folded ()
  "Folding Recently Merged publishes folded=true on the section."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "merged" "/tmp/merged" :merge-completed-at 100.0)
    (agent-repl--toggle-repo-fold agent-repl--sidebar-merged-key)
    (agent-repl-test--sidebar-with-persps '("other")
      (agent-repl-test--sidebar-with-link
        (agent-repl-test--sidebar-publish-now)
        (let ((section (alist-get 'recentlyMerged
                                  (agent-repl-test--sidebar-wire))))
          (should (eq (alist-get 'folded section) t)))))))

;;;; ---- The publisher epoch -------------------------------------------------

(ert-deftest agent-repl-test-sidebar-wire-carries-boot-id ()
  "Every roster names its publisher epoch: an empty `bootId' is nacked."
  (agent-repl-test--with-clean-state
    (let ((boot-id (alist-get 'bootId (agent-repl-test--sidebar-publish-once))))
      (should (stringp boot-id))
      (should-not (string-empty-p boot-id)))))

(ert-deftest agent-repl-test-sidebar-wire-boot-id-stable-across-publishes ()
  "Two publishes of one boot carry the identical epoch, so both are same-epoch."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (agent-repl-test--sidebar-publish-now)
      (let ((ids (mapcar (lambda (r) (plist-get r :bootId))
                         (agent-repl-test--sidebar-rosters))))
        (should (= (length ids) 2))
        (should (equal (nth 0 ids) (nth 1 ids)))))))

(ert-deftest agent-repl-test-sidebar-boot-id-minted-once-per-boot ()
  "The minted epoch is memoized: a second call returns the same string."
  (let ((agent-repl--sidebar-boot-id nil))
    (should (equal (agent-repl--sidebar-boot-id)
                   (agent-repl--sidebar-boot-id)))))

(ert-deftest agent-repl-test-sidebar-boot-id-fresh-boot-mints-a-new-epoch ()
  "A fresh boot mints a different epoch, so its revision 1 is never stale."
  (let* ((first (let ((agent-repl--sidebar-boot-id nil))
                  (agent-repl--sidebar-boot-id)))
         (second (let ((agent-repl--sidebar-boot-id nil))
                   (agent-repl--sidebar-boot-id))))
    (should-not (equal first second))))

;;;; ---- The revision counter ------------------------------------------------

(ert-deftest agent-repl-test-sidebar-first-publish-is-revision-one ()
  "The first roster of an Emacs boot publishes revision 1, not 0."
  (agent-repl-test--with-clean-state
    (should (= (alist-get 'revision (agent-repl-test--sidebar-publish-once)) 1))))

(ert-deftest agent-repl-test-sidebar-revision-increments-per-publish ()
  "Each publish carries the next revision, so ordering is decidable."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (agent-repl-test--sidebar-publish-now)
      (agent-repl-test--sidebar-publish-now)
      (should (equal (mapcar (lambda (r) (plist-get r :revision))
                             (agent-repl-test--sidebar-rosters))
                     '(1 2 3))))))

;;;; ---- Reconnect -----------------------------------------------------------

(ert-deftest agent-repl-test-sidebar-publishes-on-connect ()
  "A freshly opened link is handed the roster, so a restarted daemon regains it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--with-sidebar-frame fire
        (agent-repl--sidebar-publish-on-connect)
        (funcall (car fire)))
      (should (equal (mapcar #'car agent-repl-test--sidebar-published)
                     '("publishWorkspaceRoster"))))))

(ert-deftest agent-repl-test-sidebar-connect-publish-survives-an-unmoved-signature ()
  "A reconnect republishes even though no roster field moved.
The daemon that just replaced the old one retains nothing, so the gate
must not observe `unchanged' and drop the one publish that restores it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      ;; Publish once so the signature is current and genuinely unmoved.
      (agent-repl-test--sidebar-publish-now)
      (agent-repl-test--with-sidebar-frame fire
        (agent-repl--sidebar-publish-on-connect)
        (funcall (car fire)))
      (should (= (length agent-repl-test--sidebar-published) 2)))))

(ert-deftest agent-repl-test-sidebar-connect-publisher-is-registered ()
  "The reconnect publish is wired to the UDS connected hook, not to a timer."
  (should (memq #'agent-repl--sidebar-publish-on-connect
                agent-repl-uds-connected-functions)))

;;;; ---- A rejected publish is never silent ----------------------------------

(ert-deftest agent-repl-test-sidebar-publish-is-tracked-for-its-ack ()
  "The publish is tracked, so its ack can be matched rather than ignored."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (should (equal (mapcar (lambda (e) (plist-get e :field))
                             agent-repl-test--sidebar-tracked)
                     '("publishWorkspaceRoster"))))))

(ert-deftest agent-repl-test-sidebar-publish-nack-is-logged-loudly ()
  "A NACKed publish names the refused revision in the durable log."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        (agent-repl-test--sidebar-with-link
          (agent-repl-test--sidebar-publish-now)
          (funcall (plist-get (car agent-repl-test--sidebar-tracked) :on-failure)
                   "daemon said no")))
      (should (cl-find-if (lambda (line)
                            (and (string-match-p "sidebar-publish: NACKED" line)
                                 (string-match-p "revision=1" line)
                                 (string-match-p "daemon said no" line)))
                          logged)))))

(ert-deftest agent-repl-test-sidebar-publish-nack-does-not-rewind-revision ()
  "A NACK leaves the counter climbing: no retained revision is guessed at.
`CommandAck' carries no retained-revision field, so rewinding could only
invent one — and a counter that silently reverses could publish a stale
roster over a newer one."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--sidebar-publish-now)
      (funcall (plist-get (car agent-repl-test--sidebar-tracked) :on-failure)
               "revision 7 is not newer than 9")
      (agent-repl-test--sidebar-publish-now)
      (should (equal (mapcar (lambda (r) (plist-get r :revision))
                             (agent-repl-test--sidebar-rosters))
                     '(1 2))))))

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

(ert-deftest agent-repl-test-sidebar-tick-publishes-once-over-unchanged-state ()
  "Two ticks over unchanged state publish exactly once."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl--sidebar-tick)
      (agent-repl--sidebar-tick)
      (should (= (length agent-repl-test--sidebar-published) 1)))))

(ert-deftest agent-repl-test-sidebar-tick-publishes-on-fold-change ()
  "A fold flip between ticks republishes."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl--sidebar-tick)
      (agent-repl--toggle-repo-fold "/repos/doom/.git")
      (agent-repl--sidebar-tick)
      (should (= (length agent-repl-test--sidebar-published) 2)))))

;;;; ---- The publish gate is unbypassable ------------------------------------

(ert-deftest agent-repl-test-sidebar-many-requests-in-one-frame-publish-once ()
  "N `--sidebar-push' requests inside one frame produce exactly one publish.
This is the whole of the roster-rate fix: nine callers reached past the
1Hz gate and published the full roster on every fold, switch, nav and
state change (277 sends in twenty minutes, peaking at ten in a second)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--with-sidebar-frame fire
        (dotimes (_ 8) (agent-repl--sidebar-push))
        ;; Eight requests, one scheduled flush.
        (should (= (length fire) 1))
        (funcall (car fire)))
      (should (= (length agent-repl-test--sidebar-published) 1)))))

(ert-deftest agent-repl-test-sidebar-unchanged-signature-publishes-nothing ()
  "A request over an unmoved signature drops its flush entirely."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      ;; Publish once so the signature is current.
      (agent-repl-test--sidebar-publish-now)
      (agent-repl-test--with-sidebar-frame fire
        (dotimes (_ 5) (agent-repl--sidebar-push))
        (funcall (car fire)))
      (should (= (length agent-repl-test--sidebar-published) 1)))))

(ert-deftest agent-repl-test-sidebar-push-publishes-nothing-by-itself ()
  "The request is a request: on its own it neither builds nor publishes.
There is no remaining spelling of \"publish the roster right now,
ungated\" for a new caller to reach for."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (setq agent-repl--sidebar-flat-dirs '("/stale"))
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--with-sidebar-frame _fire
        (agent-repl--sidebar-push))
      (should (null agent-repl-test--sidebar-published))
      (should (equal agent-repl--sidebar-flat-dirs '("/stale"))))))

(ert-deftest agent-repl-test-sidebar-a-second-frame-schedules-again ()
  "Coalescing is per frame: a request after the flush schedules a new one."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--with-sidebar-frame fire
        (agent-repl--sidebar-push)
        (funcall (car fire))
        (agent-repl-test--sidebar-ws "ws2" "/tmp/ws2")
        (agent-repl--sidebar-push)
        (should (= (length fire) 2))
        (funcall (car fire)))
      (should (= (length agent-repl-test--sidebar-published) 2)))))

;;;; ---- The rail moves with the tab bar, not a second later -----------------
;;
;; The dot and the tab are two renderings of ONE value.  What used to differ
;; was when each read it: the tab bar recolored as the pushed state was
;; stored, the rail waited for the 1Hz tick to notice.  These pin the repaint
;; onto the application point itself, so no interval exists in which the two
;; surfaces disagree.

(ert-deftest agent-repl-test-sidebar-pushes-on-a-pushed-state-transition ()
  "Applying a pushed render state repaints the rail in the same breath."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((pushes 0))
      (cl-letf (((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--sidebar-react-to-pushed-state "ws" :ready :init))
      (should (= pushes 1)))))

(ert-deftest agent-repl-test-sidebar-skips-a-push-that-moved-no-keyword ()
  "A re-push of the same keyword changes no dot, so it repaints nothing."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (let ((pushes 0))
      (cl-letf (((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--sidebar-react-to-pushed-state "ws" :ready :ready))
      (should (= pushes 0)))))

(ert-deftest agent-repl-test-sidebar-transition-publish-quiets-the-next-tick ()
  "The transition\='s flush refreshes the signature, so the tick adds nothing."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "ws" "/tmp/ws")
    (agent-repl-test--sidebar-with-link
      (agent-repl-test--with-sidebar-frame fire
        (agent-repl--sidebar-react-to-pushed-state "ws" :ready :init)
        (funcall (car fire)))
      (agent-repl--sidebar-tick)
      (should (= (length agent-repl-test--sidebar-published) 1)))))

(ert-deftest agent-repl-test-sidebar-reactor-is-registered-on-the-state-hook ()
  "The repaint is wired to the state-transition hook, not to a timer alone."
  (should (memq #'agent-repl--sidebar-react-to-pushed-state
                agent-repl-ws-state-transition-functions)))

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
                                 :pushed-render-state :merged
                                 :merge-completed-at (float-time))
    (let ((names (agent-repl-test--roster-repo-names
                  (car (agent-repl--sidebar-build)))))
      (should (member "live" names))
      (should-not (member "gone" names)))))

(ert-deftest agent-repl-test-sidebar-merged-survives-post-merge-hibernation ()
  "A merged workspace the daemon then HIBERNATES keeps its merged row.
The observed disappearance: the render state went `:merged' and, seconds
later, `:hibernated' when the daemon reclaimed the session's memory, and a
section keyed on `:merged' lost the row exactly then."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "napping" "/tmp/napping"
                                 :pushed-render-state :hibernated
                                 :merge-completed-at (float-time))
    (agent-repl-test--sidebar-with-persps '("other")
      (let* ((roster (car (agent-repl--sidebar-build)))
             (recent (plist-get roster :recentlyMerged)))
        (should (agent-repl-test--sidebar-row (plist-get recent :rows) "napping"))))))

(ert-deftest agent-repl-test-sidebar-merged-revived-rejoins-the-live-groups ()
  "A merged row revived back into a perspective leaves Recently Merged.
Reopening a merged workspace restores its perspective and its session, and
a live workspace does not belong in a list of work that has receded."
  (agent-repl-test--with-clean-state
    (agent-repl-test--sidebar-ws "revived" "/tmp/revived"
                                 :pushed-render-state :ready
                                 :merge-completed-at (float-time))
    (agent-repl-test--sidebar-with-persps '("revived")
      (let ((roster (car (agent-repl--sidebar-build))))
        (should (eq (plist-get roster :recentlyMerged) :null))
        (should (member "revived" (agent-repl-test--roster-repo-names roster)))))))

(ert-deftest agent-repl-test-sidebar-merged-capped-at-the-max-rows ()
  "Recently Merged renders at most `agent-repl-sidebar-merged-max-rows'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-sidebar-merged-max-rows 3)
          (now (float-time)))
      (dotimes (i 5)
        (agent-repl-test--sidebar-ws (format "m%d" i) (format "/tmp/m%d" i)
                                     :pushed-render-state :merged
                                     :merge-completed-at (+ now i)))
      (let* ((roster (car (agent-repl--sidebar-build)))
             (rows (append (plist-get (plist-get roster :recentlyMerged) :rows) nil)))
        (should (equal (mapcar (lambda (r) (plist-get r :name)) rows)
                       '("m4" "m3" "m2")))))))

(ert-deftest agent-repl-test-sidebar-merged-cap-names-what-it-dropped ()
  "The cap logs the merges it dropped rather than truncating silently."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-sidebar-merged-max-rows 1)
          (now (float-time))
          (logged nil))
      (agent-repl-test--sidebar-ws "kept" "/tmp/kept"
                                   :pushed-render-state :merged
                                   :merge-completed-at (+ now 1))
      (agent-repl-test--sidebar-ws "dropped" "/tmp/dropped"
                                   :pushed-render-state :merged
                                   :merge-completed-at now)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        (agent-repl--sidebar-merged-sorted (agent-repl--sidebar-entries)))
      (should (cl-find-if (lambda (line)
                            (and (string-match-p "sidebar-merged-sorted: cap=1" line)
                                 (string-match-p "dropped" line)))
                          logged)))))

(ert-deftest agent-repl-test-sidebar-merged-cap-leaves-a-short-section-whole ()
  "A merge count at the cap renders every row (the cap drops nothing)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-sidebar-merged-max-rows 2)
          (now (float-time)))
      (agent-repl-test--sidebar-ws "a" "/tmp/a"
                                   :pushed-render-state :merged :merge-completed-at now)
      (agent-repl-test--sidebar-ws "b" "/tmp/b"
                                   :pushed-render-state :merged :merge-completed-at (- now 1))
      (let* ((roster (car (agent-repl--sidebar-build)))
             (rows (append (plist-get (plist-get roster :recentlyMerged) :rows) nil)))
        (should (= (length rows) 2))))))

(ert-deftest agent-repl-test-sidebar-merged-listed-newest-first ()
  "Recently merged rows sort by merge time, newest first."
  (agent-repl-test--with-clean-state
    (let ((now (float-time)))
      (agent-repl-test--sidebar-ws "older" "/tmp/older"
                                   :pushed-render-state :merged :merge-completed-at (- now 100))
      (agent-repl-test--sidebar-ws "newer" "/tmp/newer"
                                   :pushed-render-state :merged :merge-completed-at (- now 10))
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
                                   :pushed-render-state :merged :merge-completed-at (- now 1000))
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

;;;; ---- View + task command handlers ---------------------------------------

(ert-deftest agent-repl-test-sidebar-set-view-command-bad-view-errors ()
  "A set-view command with an unrecognized view signals."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-set-view-command
                   '((type . "set-view") (view . "bogus"))))))

(ert-deftest agent-repl-test-sidebar-set-view-command-switches-and-pushes ()
  "A set-view command flips the active view and pushes a fresh roster."
  (agent-repl-test--with-clean-state
    (let ((pushes 0))
      (cl-letf (((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--handle-set-view-command
         '((type . "set-view") (view . "task"))))
      (should (eq agent-repl--sidebar-view :task))
      (should (= pushes 1)))))

;;;; ---- Deferred minibuffer prompts --------------------------------------
;;
;; The handlers run in the unix-socket transport's process filter, where a
;; synchronous minibuffer read gets no command loop and swallows every
;; keystroke.  They therefore hand the read to `run-at-time', which these
;; tests either capture (to prove the handler itself never prompts) or run
;; immediately (to exercise the prompt body).

(defmacro agent-repl-test--with-captured-timer (var &rest body)
  "Execute BODY with `run-at-time' capturing its thunk into VAR instead of
scheduling it.  VAR is bound to nil first, so a handler that never
schedules leaves it nil."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_time _repeat fn &rest _) (setq ,var fn) 'fake-timer)))
       ,@body)))

(defmacro agent-repl-test--with-sync-timer (&rest body)
  "Execute BODY with `run-at-time' invoking its thunk immediately."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'run-at-time)
              (lambda (_time _repeat fn &rest _) (funcall fn) 'fake-timer)))
     ,@body))

(ert-deftest agent-repl-test-sidebar-task-create-command-defers-the-prompt ()
  "A task-create command schedules the title read instead of prompting."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl--sidebar-prompt-pending nil)
          (prompted nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompted t) "")))
        ;; Act
        (agent-repl-test--with-captured-timer thunk
          (agent-repl--handle-task-create-command '((type . "task-create")))
          ;; Assert
          (should-not prompted)
          (should (functionp thunk)))))))

(ert-deftest agent-repl-test-sidebar-task-create-deferred-creates-and-pushes ()
  "The deferred task-create read creates the prompted task and pushes."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl--sidebar-prompt-pending nil)
          (pushes 0))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "New task"))
                ;; Keep the created task in-memory only — no disk write.
                ((symbol-function 'agent-repl--tasks-save) (lambda ()))
                ((symbol-function 'agent-repl--task-org-ensure)
                 (lambda (&rest _) "/tmp/notes.org"))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        ;; Act
        (agent-repl-test--with-sync-timer
          (agent-repl--handle-task-create-command '((type . "task-create")))))
      ;; Assert
      (should (= 1 (hash-table-count agent-repl--tasks)))
      (should (= pushes 1)))))

(ert-deftest agent-repl-test-sidebar-task-create-deferred-empty-title-noops ()
  "A deferred task-create read that returns empty creates nothing."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl--sidebar-prompt-pending nil)
          (pushes 0))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  "))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        ;; Act
        (agent-repl-test--with-sync-timer
          (agent-repl--handle-task-create-command '((type . "task-create")))))
      ;; Assert
      (should (= 0 (hash-table-count agent-repl--tasks)))
      (should (= pushes 0)))))

(ert-deftest agent-repl-test-sidebar-task-create-second-click-is-dropped ()
  "A second + click while a prompt is pending schedules no second read."
  (agent-repl-test--with-clean-state
    ;; Arrange: the first click's thunk is captured, never run, so its
    ;; prompt stays pending exactly as it would while the user types.
    (let ((agent-repl--sidebar-prompt-pending nil)
          (scheduled 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) 'fake-timer)))
        ;; Act
        (agent-repl--handle-task-create-command '((type . "task-create")))
        (agent-repl--handle-task-create-command '((type . "task-create"))))
      ;; Assert
      (should (= scheduled 1)))))

(ert-deftest agent-repl-test-sidebar-task-create-second-click-returns-nil ()
  "The dropped second + click reports the drop to its caller."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl--sidebar-prompt-pending nil))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl--handle-task-create-command '((type . "task-create")))
        ;; Assert
        (should-not (agent-repl--handle-task-create-command
                     '((type . "task-create"))))))))

(ert-deftest agent-repl-test-sidebar-task-create-second-click-logs-the-drop ()
  "The dropped second + click leaves a log line naming the pending prompt."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl--sidebar-prompt-pending nil)
          (lines nil))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) lines))))
        ;; Act
        (agent-repl--handle-task-create-command '((type . "task-create")))
        (agent-repl--handle-task-create-command '((type . "task-create"))))
      ;; Assert
      (should (cl-find-if
               (lambda (l) (string-match-p "already pending=task-create" l))
               lines)))))

(ert-deftest agent-repl-test-sidebar-prompt-pending-clears-after-an-error ()
  "A prompt that signals still clears the pending flag, unwedging the button."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl--sidebar-prompt-pending nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (error "quit out of the minibuffer"))))
        ;; Act
        (should-error
         (agent-repl-test--with-sync-timer
           (agent-repl--handle-task-create-command '((type . "task-create"))))))
      ;; Assert
      (should-not agent-repl--sidebar-prompt-pending))))

(ert-deftest agent-repl-test-sidebar-task-toggle-done-command-missing-id-errors ()
  "A task-toggle-done command with no id signals."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-task-toggle-done-command
                   '((type . "task-toggle-done"))))))

(ert-deftest agent-repl-test-sidebar-task-toggle-done-command-toggles ()
  "A task-toggle-done command flips the task and pushes."
  (agent-repl-test--with-clean-state
    (let ((id (agent-repl-test--sidebar-task "t1" "toggle"))
          (pushes 0))
      (cl-letf (((symbol-function 'agent-repl--tasks-save) (lambda ()))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        (agent-repl--handle-task-toggle-done-command
         `((type . "task-toggle-done") (id . ,id))))
      (should (plist-get (gethash id agent-repl--tasks) :done))
      (should (= pushes 1)))))

(ert-deftest agent-repl-test-sidebar-task-open-command-missing-id-errors ()
  "A task-open command with no id signals."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-task-open-command
                   '((type . "task-open"))))))

(ert-deftest agent-repl-test-sidebar-task-open-command-opens ()
  "A task-open command routes the task id to `agent-repl--task-open'."
  (agent-repl-test--with-clean-state
    (let ((id (agent-repl-test--sidebar-task "t1" "open")) opened)
      (cl-letf (((symbol-function 'agent-repl--task-open)
                 (lambda (i) (setq opened i))))
        (agent-repl--handle-task-open-command
         `((type . "task-open") (id . ,id))))
      (should (equal opened id)))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-missing-id-errors ()
  "A task-add-workspace command with no id signals."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-task-add-workspace-command
                   '((type . "task-add-workspace"))))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-unknown-id-errors ()
  "A task-add-workspace command naming no known task signals in-context.
The check must stay synchronous: an error raised from the deferred timer
could no longer fail the host action."
  (agent-repl-test--with-clean-state
    ;; Arrange / Act / Assert
    (let ((agent-repl--sidebar-prompt-pending nil))
      (should-error (agent-repl--handle-task-add-workspace-command
                     '((type . "task-add-workspace") (id . "nope")))))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-defers-the-chooser ()
  "A task-add-workspace command schedules the chooser instead of prompting."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((id (agent-repl-test--sidebar-task "t1" "target"))
           (agent-repl--sidebar-prompt-pending nil)
           (prompted nil))
      (agent-repl-test--sidebar-ws "free" "/tmp/free")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq prompted t) "free")))
        ;; Act
        (agent-repl-test--with-captured-timer thunk
          (agent-repl--handle-task-add-workspace-command
           `((type . "task-add-workspace") (id . ,id)))
          ;; Assert
          (should-not prompted)
          (should (functionp thunk)))))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-deferred-assigns-choice ()
  "The deferred chooser assigns the chosen workspace's `:task-id' and pushes."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((id (agent-repl-test--sidebar-task "t1" "target"))
          (agent-repl--sidebar-prompt-pending nil)
          (pushes 0))
      (agent-repl-test--sidebar-ws "free" "/tmp/free")
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "free"))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        ;; Act
        (agent-repl-test--with-sync-timer
          (agent-repl--handle-task-add-workspace-command
           `((type . "task-add-workspace") (id . ,id)))))
      ;; Assert
      (should (equal (agent-repl--ws-get "free" :task-id) id))
      (should (= pushes 1)))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-deferred-cancel-noops ()
  "A cancelled deferred chooser assigns nothing."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((id (agent-repl-test--sidebar-task "t1" "target"))
          (agent-repl--sidebar-prompt-pending nil)
          (pushes 0))
      (agent-repl-test--sidebar-ws "free" "/tmp/free")
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (cl-incf pushes))))
        ;; Act
        (agent-repl-test--with-sync-timer
          (agent-repl--handle-task-add-workspace-command
           `((type . "task-add-workspace") (id . ,id)))))
      ;; Assert
      (should-not (agent-repl--ws-get "free" :task-id))
      (should (= pushes 0)))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-second-click-is-dropped ()
  "A second task + click while a prompt is pending schedules no second read."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((id (agent-repl-test--sidebar-task "t1" "target"))
          (agent-repl--sidebar-prompt-pending nil)
          (scheduled 0))
      (agent-repl-test--sidebar-ws "free" "/tmp/free")
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) 'fake-timer)))
        ;; Act
        (agent-repl--handle-task-add-workspace-command
         `((type . "task-add-workspace") (id . ,id)))
        (agent-repl--handle-task-add-workspace-command
         `((type . "task-add-workspace") (id . ,id))))
      ;; Assert
      (should (= scheduled 1)))))

(ert-deftest agent-repl-test-sidebar-task-prompts-share-one-pending-slot ()
  "A task-add-workspace click is dropped while a task-create prompt is open.
Both read the one minibuffer, so the guard is shared rather than per-action."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((id (agent-repl-test--sidebar-task "t1" "target"))
          (agent-repl--sidebar-prompt-pending nil))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl--handle-task-create-command '((type . "task-create")))
        ;; Assert
        (should-not (agent-repl--handle-task-add-workspace-command
                     `((type . "task-add-workspace") (id . ,id))))))))

(ert-deftest agent-repl-test-sidebar-task-add-workspace-none-available ()
  "The interactive add is a no-op when every workspace is already in the task."
  (agent-repl-test--with-clean-state
    (let ((id (agent-repl-test--sidebar-task "t1" "full"))
          (chosen nil))
      (agent-repl-test--sidebar-ws "member" "/tmp/member" :task-id id)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq chosen t) "member"))
                ((symbol-function 'agent-repl--sidebar-push) (lambda ())))
        (agent-repl--task-add-workspace-interactive id))
      (should-not chosen))))

;;;; ---- The two context cuts --------------------------------------------

(ert-deftest agent-repl-test-sidebar-wire-status-clearing ()
  ":clearing carries its own wire string rather than borrowing thinking's."
  ;; Act / Assert
  (should (equal (car (alist-get :clearing agent-repl--sidebar-status-wire)) "clearing")))

(ert-deftest agent-repl-test-sidebar-wire-status-compacting ()
  ":compacting carries its own wire string rather than borrowing thinking's."
  ;; Act / Assert
  (should (equal (car (alist-get :compacting agent-repl--sidebar-status-wire)) "compacting")))

;;;; ---- The RosterRow.status proto vocabulary ---------------------------

;; `frontend.v1.RosterRow.status' is the third face of the sidebar's status
;; contract, beside this file's wire table and the webapp's WorkspaceStatus
;; union.  These tests pin the oneof's ARM NAMES against the elisp list so a
;; status added on one side and forgotten on the other fails here rather than
;; drawing a wrong dot.  The proto text is READ, not duplicated: a copy of the
;; vocabulary in the test would be a fourth list to drift.

(defconst agent-repl-test--frontend-proto-file
  (expand-file-name "proto/agentshim/frontend/v1/frontend.proto"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to frontend.proto, resolved at LOAD time.
`load-file-name' is nil once loading finishes, so a helper that reads it
when CALLED would resolve nothing under `emacs -batch -l'.")

(defun agent-repl-test--roster-status-keywords ()
  "Return the wire strings the RosterRow.status oneof declares as arms.
Each arm name maps to the sidebar's wire spelling by replacing
underscores with hyphens, so `idle_async' reads as \"idle-async\".  Only
arms INSIDE the oneof block count; the empty per-status messages declared
beside it are declarations, not vocabulary."
  (let ((proto agent-repl-test--frontend-proto-file)
        (names nil))
    (with-temp-buffer
      (insert-file-contents proto)
      (goto-char (point-min))
      (unless (re-search-forward "^ *oneof status *{" nil t)
        (error "frontend.proto declares no RosterRow.status oneof"))
      (let ((end (save-excursion
                   (goto-char (match-beginning 0))
                   (forward-list)
                   (point))))
        (while (re-search-forward
                "^ *RosterRowStatus[A-Za-z]+ +\\([a-z_]+\\) *=" end t)
          (push (replace-regexp-in-string "_" "-" (match-string 1)) names))))
    (nreverse names)))

(defun agent-repl-test--sidebar-wire-vocabulary ()
  "Return every wire status string the sidebar can emit.
The mapped render states, plus the two `agent-repl--sidebar-wire-status'
derives structurally rather than from the table."
  (delete-dups
   (append (mapcar #'cadr agent-repl--sidebar-status-wire)
           '("inactive" "none"))))

(ert-deftest agent-repl-test-sidebar-roster-oneof-covers-every-wire-status ()
  "Every status the sidebar emits has a RosterRow.status arm."
  ;; Arrange
  (let ((arms (agent-repl-test--roster-status-keywords)))
    ;; Act
    (let ((missing (cl-remove-if (lambda (s) (member s arms))
                                 (agent-repl-test--sidebar-wire-vocabulary))))
      ;; Assert
      (should (null missing)))))

(ert-deftest agent-repl-test-sidebar-roster-oneof-adds-no-unknown-status ()
  "RosterRow.status declares no arm the sidebar cannot emit."
  ;; Arrange
  (let ((vocab (agent-repl-test--sidebar-wire-vocabulary)))
    ;; Act
    (let ((extra (cl-remove-if (lambda (s) (member s vocab))
                               (agent-repl-test--roster-status-keywords))))
      ;; Assert
      (should (null extra)))))

(ert-deftest agent-repl-test-sidebar-roster-oneof-declares-no-unspecified-arm ()
  "Absence of a status is an unset oneof, never an arm that names absence."
  ;; Act / Assert
  (should-not (member "unspecified" (agent-repl-test--roster-status-keywords))))

(ert-deftest agent-repl-test-sidebar-roster-status-is-not-an-enum ()
  "The status vocabulary is a oneof of messages, never an enum.
State-specifying enums are not used: a proto3 enum's zero value is both
\"unset\" and a legal member, which is exactly the ambiguity the oneof
removes."
  ;; Arrange
  (with-temp-buffer
    (insert-file-contents agent-repl-test--frontend-proto-file)
    ;; Act / Assert
    (goto-char (point-min))
    (should-not (re-search-forward "^ *enum RosterRowStatus" nil t))))

(provide 'test-sidebar)
;;; test-sidebar.el ends here
