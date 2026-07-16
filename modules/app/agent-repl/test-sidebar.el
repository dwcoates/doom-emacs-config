;;; test-sidebar.el --- ERT tests for sidebar.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the GUI sidebar bridge.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-sidebar.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defconst agent-repl-sidebar-test--project-dir "/tmp/agent-repl-test-repo/ws")
(defconst agent-repl-sidebar-test--group-key "/tmp/agent-repl-test-repo/.git")

(defun agent-repl-sidebar-test--register (ws &rest props)
  "Register WS in `agent-repl--workspaces' with PROPS.
Seeds `:project-dir' and `:group-key' when omitted, mirroring the
drawer test helper: the sidebar renders only real workspaces, and the
group key keeps render paths from shelling out to git."
  (let ((plist (copy-sequence props)))
    (unless (plist-member plist :project-dir)
      (setq plist (plist-put plist :project-dir
                             agent-repl-sidebar-test--project-dir)))
    (unless (plist-member plist :group-key)
      (setq plist (plist-put plist :group-key
                             agent-repl-sidebar-test--group-key)))
    (puthash ws plist agent-repl--workspaces)))

(defmacro agent-repl-sidebar-test--with-state (&rest body)
  "Run BODY with clean workspace + merge + sidebar state.
Kills the singleton drawer buffer first so its buffer-local
marked/expanded sets (which the sidebar shares) cannot leak across
tests, and let-binds every sidebar global that pushes mutate."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (agent-repl-test--with-merge-state
       (when-let ((buf (get-buffer agent-repl-drawer-buffer-name)))
         (kill-buffer buf))
       (let ((agent-repl-sidebar-enabled nil)
             (agent-repl-sidebar--last-push-signature 'unset)
             (agent-repl-sidebar--last-push-time 0)
             (agent-repl-sidebar--last-failure-time 0)
             (agent-repl-sidebar--last-action-result nil)
             (agent-repl-drawer--global-visible-p nil))
         (unwind-protect
             (progn ,@body)
           (when-let ((buf (get-buffer agent-repl-drawer-buffer-name)))
             (kill-buffer buf)))))))

(defun agent-repl-sidebar-test--in-flight (ws dir commits index &optional conflict-sha)
  "Register WS as an in-flight merge into DIR over COMMITS, applying INDEX."
  (push (list :source-ws ws :target-dir dir :started-at 0.0)
        agent-repl--in-flight-merges)
  (agent-repl--merge-progress-begin ws commits)
  (agent-repl--merge-progress-put ws :commit-index index)
  (when conflict-sha
    (agent-repl--merge-progress-put ws :conflict-sha conflict-sha)))

(defun agent-repl-sidebar-test--section (snapshot id)
  "Return the section alist with ID from SNAPSHOT, or nil."
  (seq-find (lambda (sec) (equal (alist-get 'id sec) id))
            (append (alist-get 'sections snapshot) nil)))

(defun agent-repl-sidebar-test--entries (snapshot section-id)
  "Return every entry alist under SECTION-ID in SNAPSHOT, group order."
  (let ((section (agent-repl-sidebar-test--section snapshot section-id)))
    (apply #'append
           (mapcar (lambda (group) (append (alist-get 'entries group) nil))
                   (append (alist-get 'groups section) nil)))))

(defun agent-repl-sidebar-test--entry (snapshot section-id ws)
  "Return WS's entry alist under SECTION-ID in SNAPSHOT, or nil."
  (seq-find (lambda (entry) (equal (alist-get 'ws entry) ws))
            (agent-repl-sidebar-test--entries snapshot section-id)))

(defun agent-repl-sidebar-test--mark (ws)
  "Mark WS in the shared drawer marked-set."
  (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
    (agent-repl-drawer--ensure-marked-set)
    (puthash ws t agent-repl-drawer--marked-set)))

(defun agent-repl-sidebar-test--expand (ws)
  "Expand WS in the shared drawer expanded-set (without git)."
  (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
    (agent-repl-drawer--ensure-expanded-set)
    (puthash ws t agent-repl-drawer--expanded-set)))

(defun agent-repl-sidebar-test--action-file (dir payload)
  "Write PAYLOAD (an alist) as a sidebar action file in DIR; return its path."
  (let ((file (expand-file-name
               (format "sidebar_action_%s.json" (abs (random))) dir)))
    (with-temp-file file
      (insert (json-encode payload)))
    file))

;;;; ---- Snapshot: top level ----

(ert-deftest agent-repl-sidebar-test-snapshot-type-and-version ()
  "The snapshot carries the type tag and schema version."
  (agent-repl-sidebar-test--with-state
    (let ((snap (agent-repl-sidebar--snapshot)))
      (should (equal "workspace-snapshot" (alist-get 'type snap)))
      (should (equal agent-repl-sidebar-version
                     (alist-get 'sidebar_version snap))))))

(ert-deftest agent-repl-sidebar-test-snapshot-sections-order-and-counts ()
  "Sections appear in drawer order with per-section counts."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws-main")
    (agent-repl-sidebar-test--register "ws-hidden" :repl-state :hidden)
    (agent-repl-sidebar-test--register "ws-merged" :repl-state :merged)
    (let* ((snap (agent-repl-sidebar--snapshot))
           (sections (append (alist-get 'sections snap) nil)))
      (should (equal '("main" "hidden" "merging" "merged")
                     (mapcar (lambda (s) (alist-get 'id s)) sections)))
      (should (equal '(("MAIN" . 1) ("HIDDEN" . 1) ("MERGING" . 0) ("MERGED" . 1))
                     (mapcar (lambda (s) (cons (alist-get 'label s)
                                               (alist-get 'count s)))
                             sections))))))

(ert-deftest agent-repl-sidebar-test-snapshot-omits-empty-hidden-section ()
  "HIDDEN is omitted entirely when no workspace is hidden."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws-main")
    (should-not (agent-repl-sidebar-test--section
                 (agent-repl-sidebar--snapshot) "hidden"))))

(ert-deftest agent-repl-sidebar-test-snapshot-empty-main-has-empty-groups ()
  "An empty MAIN section is still present, with an empty groups array."
  (agent-repl-sidebar-test--with-state
    (let ((main (agent-repl-sidebar-test--section
                 (agent-repl-sidebar--snapshot) "main")))
      (should main)
      (should (equal 0 (alist-get 'count main)))
      (should (equal [] (alist-get 'groups main))))))

(ert-deftest agent-repl-sidebar-test-snapshot-marks-array-empty-is-vector ()
  "With nothing marked, `marks' is an empty JSON array, never null."
  (agent-repl-sidebar-test--with-state
    (should (equal [] (alist-get 'marks (agent-repl-sidebar--snapshot))))))

(ert-deftest agent-repl-sidebar-test-snapshot-marks-lists-marked-ws ()
  "A workspace marked in the shared drawer set appears in `marks'."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl-sidebar-test--mark "ws1")
    (should (equal ["ws1"] (alist-get 'marks (agent-repl-sidebar--snapshot))))))

(ert-deftest agent-repl-sidebar-test-snapshot-visible-flag-encodes-false ()
  "A hidden sidebar encodes `sidebar_visible' as JSON false, not null."
  (agent-repl-sidebar-test--with-state
    (should (string-match-p "\"sidebar_visible\":false"
                            (json-encode (agent-repl-sidebar--snapshot))))))

(ert-deftest agent-repl-sidebar-test-snapshot-current-ws-flag ()
  "The active workspace's entry carries current=t and the top-level name."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl-sidebar-test--register "ws2")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws2")))
      (let ((snap (agent-repl-sidebar--snapshot)))
        (should (equal "ws2" (alist-get 'current_ws snap)))
        (should (eq t (alist-get 'current
                                 (agent-repl-sidebar-test--entry snap "main" "ws2"))))
        (should (eq :json-false
                    (alist-get 'current
                               (agent-repl-sidebar-test--entry snap "main" "ws1"))))))))

;;;; ---- Snapshot: entries ----

(ert-deftest agent-repl-sidebar-test-entry-glyph-and-status ()
  "An entry's glyph and status string come from render-status."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :agent-state :thinking)
    (let ((entry (agent-repl-sidebar-test--entry
                  (agent-repl-sidebar--snapshot) "main" "ws1")))
      (should (equal "thinking" (alist-get 'status entry)))
      (should (equal (alist-get :thinking agent-repl-drawer-state-icons)
                     (alist-get 'glyph entry))))))

(ert-deftest agent-repl-sidebar-test-entry-name-color-for-thinking ()
  "A thinking workspace ships the drawer's thinking-red name color."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :agent-state :thinking)
    (should (equal agent-repl--color-thinking-red
                   (alist-get 'name_color
                              (agent-repl-sidebar-test--entry
                               (agent-repl-sidebar--snapshot) "main" "ws1"))))))

(ert-deftest agent-repl-sidebar-test-entry-name-color-nil-for-merged ()
  "A merged workspace's name carries no color (glyph is the signal)."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :repl-state :merged)
    (should-not (alist-get 'name_color
                           (agent-repl-sidebar-test--entry
                            (agent-repl-sidebar--snapshot) "merged" "ws1")))))

(ert-deftest agent-repl-sidebar-test-entry-summary-pending-renders-ellipsis ()
  "A pending summary renders the drawer's `…' placeholder with the flag set."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :last-prompt-summary-pending t)
    (let ((entry (agent-repl-sidebar-test--entry
                  (agent-repl-sidebar--snapshot) "main" "ws1")))
      (should (equal "…" (alist-get 'summary entry)))
      (should (eq t (alist-get 'summary_pending entry))))))

(ert-deftest agent-repl-sidebar-test-entry-dirty-flag ()
  "`:git-clean' = `dirty' sets the entry's dirty flag."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :git-clean 'dirty)
    (should (eq t (alist-get 'dirty
                             (agent-repl-sidebar-test--entry
                              (agent-repl-sidebar--snapshot) "main" "ws1"))))))

(ert-deftest agent-repl-sidebar-test-entry-marked-flag-from-shared-set ()
  "The shared drawer marked-set drives the entry's marked flag."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl-sidebar-test--mark "ws1")
    (should (eq t (alist-get 'marked
                             (agent-repl-sidebar-test--entry
                              (agent-repl-sidebar--snapshot) "main" "ws1"))))))

(ert-deftest agent-repl-sidebar-test-entry-collapsed-has-null-detail ()
  "A collapsed entry ships no detail object."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (should-not (alist-get 'detail
                           (agent-repl-sidebar-test--entry
                            (agent-repl-sidebar--snapshot) "main" "ws1")))))

(ert-deftest agent-repl-sidebar-test-entry-expanded-detail-cached-fields ()
  "An expanded entry ships the cached detail fields."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1"
                                       :detail-branch "DWC/x"
                                       :detail-master-ahead 3
                                       :detail-last-commit "feat: y"
                                       :detail-last-commit-time "2 hours ago")
    (agent-repl-sidebar-test--expand "ws1")
    (let ((detail (alist-get 'detail
                             (agent-repl-sidebar-test--entry
                              (agent-repl-sidebar--snapshot) "main" "ws1"))))
      (should (equal "DWC/x" (alist-get 'branch detail)))
      (should (equal 3 (alist-get 'ahead_master detail)))
      (should (equal "feat: y" (alist-get 'last_commit detail)))
      (should (equal "2 hours ago" (alist-get 'last_commit_time detail))))))

(ert-deftest agent-repl-sidebar-test-detail-suppresses-source-when-trunk ()
  "ahead_source (and its branch) is suppressed when the source IS the trunk."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1"
                                       :detail-source-branch
                                       agent-repl-master-branch-name
                                       :detail-source-ahead 5)
    (agent-repl-sidebar-test--expand "ws1")
    (let ((detail (alist-get 'detail
                             (agent-repl-sidebar-test--entry
                              (agent-repl-sidebar--snapshot) "main" "ws1"))))
      (should-not (alist-get 'source_branch detail))
      (should-not (alist-get 'ahead_source detail)))))

(ert-deftest agent-repl-sidebar-test-detail-zero-dirty-count-suppressed ()
  "A zero dirty count ships as nil so the browser renders no line."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :detail-dirty-count 0)
    (agent-repl-sidebar-test--expand "ws1")
    (should-not (alist-get 'dirty_count
                           (alist-get 'detail
                                      (agent-repl-sidebar-test--entry
                                       (agent-repl-sidebar--snapshot)
                                       "main" "ws1"))))))

(ert-deftest agent-repl-sidebar-test-detail-merged-into-only-in-merged ()
  "merged_into ships only for MERGED-section entries."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :merge-target-name "master")
    (agent-repl-sidebar-test--expand "ws1")
    (should-not (alist-get 'merged_into
                           (alist-get 'detail
                                      (agent-repl-sidebar-test--entry
                                       (agent-repl-sidebar--snapshot)
                                       "main" "ws1"))))))

(ert-deftest agent-repl-sidebar-test-entry-child-depth ()
  "A child workspace (source-ws chain) lands one depth level deeper."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "parent" :project-dir "/tmp/sb-parent")
    (agent-repl-sidebar-test--register "child"
                                       :project-dir "/tmp/sb-child"
                                       :source-ws-dir "/tmp/sb-parent")
    (let ((snap (agent-repl-sidebar--snapshot)))
      (should (equal 0 (alist-get 'depth
                                  (agent-repl-sidebar-test--entry
                                   snap "main" "parent"))))
      (should (equal 1 (alist-get 'depth
                                  (agent-repl-sidebar-test--entry
                                   snap "main" "child")))))))

;;;; ---- Snapshot: groups ----

(ert-deftest agent-repl-sidebar-test-folded-group-ships-no-entries ()
  "A folded repo group ships folded=t and an empty entries array."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl--toggle-repo-fold agent-repl-sidebar-test--group-key)
    (let* ((main (agent-repl-sidebar-test--section
                  (agent-repl-sidebar--snapshot) "main"))
           (group (elt (alist-get 'groups main) 0)))
      (should (eq t (alist-get 'folded group)))
      (should (equal [] (alist-get 'entries group))))))

;;;; ---- Snapshot: merge queue ----

(ert-deftest agent-repl-sidebar-test-merge-queue-absent-when-idle ()
  "An idle merge queue ships no merge_queue key at all."
  (agent-repl-sidebar-test--with-state
    (should-not (assq 'merge_queue (agent-repl-sidebar--snapshot)))))

(ert-deftest agent-repl-sidebar-test-merge-queue-rows-and-separator ()
  "The merge queue ships a leading separator then commit rows in order."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--in-flight
     "ws" "/r/doom" '(("abc1234" . "feat: one") ("def5678" . "feat: two")) 0)
    (let* ((mq (alist-get 'merge_queue (agent-repl-sidebar--snapshot)))
           (rows (append (alist-get 'rows mq) nil)))
      (should (equal 2 (alist-get 'count mq)))
      (should (equal "separator" (alist-get 'kind (nth 0 rows))))
      (should (equal "doom" (alist-get 'project (nth 0 rows))))
      (should (equal "current" (alist-get 'state (nth 1 rows))))
      (should (equal "abc1234" (alist-get 'sha (nth 1 rows))))
      (should (equal "pending" (alist-get 'state (nth 2 rows)))))))

(ert-deftest agent-repl-sidebar-test-merge-queue-conflict-detail ()
  "A conflicted commit ships its file count and resolver phase."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--in-flight
     "ws" "/r/doom" '(("abc1234" . "feat: one")) 0 "abc1234")
    (agent-repl--merge-progress-put "ws" :conflict-files '("a.el" "b.el"))
    (agent-repl--merge-progress-put "ws" :resolver-phase 'analyzing)
    (let* ((mq (alist-get 'merge_queue (agent-repl-sidebar--snapshot)))
           (commit (seq-find (lambda (row)
                               (equal "commit" (alist-get 'kind row)))
                             (append (alist-get 'rows mq) nil))))
      (should (equal "conflict" (alist-get 'state commit)))
      (should (equal 2 (alist-get 'conflict_files commit)))
      (should (equal "analyzing" (alist-get 'resolver_phase commit))))))

;;;; ---- Push gating ----

(defmacro agent-repl-sidebar-test--count-posts (&rest body)
  "Run BODY with `--post-snapshot' stubbed; return the POST count."
  (declare (indent 0))
  `(let ((posts 0))
     (cl-letf (((symbol-function 'agent-repl-sidebar--post-snapshot)
                (lambda (_json) (setq posts (1+ posts)))))
       ,@body)
     posts))

(ert-deftest agent-repl-sidebar-test-push-noop-when-disabled ()
  "With the bridge disabled, push never POSTs."
  (agent-repl-sidebar-test--with-state
    (should (zerop (agent-repl-sidebar-test--count-posts
                     (agent-repl-sidebar--push))))))

(ert-deftest agent-repl-sidebar-test-push-once-per-signature ()
  "An unchanged view-model suppresses the second push."
  (agent-repl-sidebar-test--with-state
    (let ((agent-repl-sidebar-enabled t)
          (agent-repl-sidebar--last-push-time (float-time)))
      (agent-repl-sidebar-test--register "ws1")
      (should (equal 1 (agent-repl-sidebar-test--count-posts
                         (agent-repl-sidebar--push)
                         (setq agent-repl-sidebar--last-push-time (float-time))
                         (agent-repl-sidebar--push)))))))

(ert-deftest agent-repl-sidebar-test-push-again-when-state-changes ()
  "A state change between pushes re-POSTs."
  (agent-repl-sidebar-test--with-state
    (let ((agent-repl-sidebar-enabled t)
          (agent-repl-sidebar--last-push-time (float-time)))
      (agent-repl-sidebar-test--register "ws1")
      (should (equal 2 (agent-repl-sidebar-test--count-posts
                         (agent-repl-sidebar--push)
                         (agent-repl-sidebar-test--register "ws2")
                         (setq agent-repl-sidebar--last-push-time (float-time))
                         (agent-repl-sidebar--push)))))))

(ert-deftest agent-repl-sidebar-test-push-heartbeat-forces-repost ()
  "An elapsed heartbeat re-POSTs even with an unchanged signature."
  (agent-repl-sidebar-test--with-state
    (let ((agent-repl-sidebar-enabled t))
      (should (equal 2 (agent-repl-sidebar-test--count-posts
                         (agent-repl-sidebar--push)
                         (setq agent-repl-sidebar--last-push-time
                               (- (float-time)
                                  (1+ agent-repl-sidebar-heartbeat-seconds)))
                         (agent-repl-sidebar--push)))))))

(ert-deftest agent-repl-sidebar-test-push-backoff-after-failure ()
  "Inside the failure backoff window, an unforced push is suppressed."
  (agent-repl-sidebar-test--with-state
    (let ((agent-repl-sidebar-enabled t)
          (agent-repl-sidebar--last-failure-time (float-time)))
      (should (zerop (agent-repl-sidebar-test--count-posts
                       (agent-repl-sidebar--push)))))))

(ert-deftest agent-repl-sidebar-test-push-force-overrides-backoff ()
  "A forced push POSTs even inside the failure backoff window."
  (agent-repl-sidebar-test--with-state
    (let ((agent-repl-sidebar-enabled t)
          (agent-repl-sidebar--last-failure-time (float-time)))
      (should (equal 1 (agent-repl-sidebar-test--count-posts
                         (agent-repl-sidebar--push t)))))))

;;;; ---- Action execution ----

(defmacro agent-repl-sidebar-test--run-action (payload &rest letf-specs)
  "Process PAYLOAD (an action alist) from a temp file with LETF-SPECS stubbed.
Returns `agent-repl-sidebar--last-action-result'."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "sidebar-actions" t))
          (agent-repl-sidebar-actions-dir (file-name-as-directory dir)))
     (unwind-protect
         (cl-letf (,@letf-specs)
           (agent-repl-sidebar--process-action-file
            (agent-repl-sidebar-test--action-file dir ,payload))
           agent-repl-sidebar--last-action-result)
       (delete-directory dir t))))

(ert-deftest agent-repl-sidebar-test-action-file-deleted-after-processing ()
  "The action file is deleted even when the action fails."
  (agent-repl-sidebar-test--with-state
    (let* ((dir (make-temp-file "sidebar-actions" t))
           (agent-repl-sidebar-actions-dir (file-name-as-directory dir))
           (file (agent-repl-sidebar-test--action-file
                  dir '((id . "a1") (action . "bogus") (targets . [])))))
      (unwind-protect
          (progn
            (agent-repl-sidebar--process-action-file file)
            (should-not (file-exists-p file)))
        (delete-directory dir t)))))

(ert-deftest agent-repl-sidebar-test-action-interrupt-dispatches ()
  "An interrupt action calls `agent-repl-interrupt' per target."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (let* ((interrupted nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a1") (action . "interrupt") (targets . ["ws1"]))
                     ((symbol-function 'agent-repl-interrupt)
                      (lambda (ws) (push ws interrupted))))))
      (should (equal '("ws1") interrupted))
      (should (eq t (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-unknown-reports-error ()
  "An unknown action lands in last_action_result as a failure."
  (agent-repl-sidebar-test--with-state
    (let ((result (agent-repl-sidebar-test--run-action
                      '((id . "a2") (action . "frobnicate") (targets . [])))))
      (should (eq :json-false (alist-get 'ok result)))
      (should (string-match-p "frobnicate" (alist-get 'error result)))
      (should (equal "a2" (alist-get 'id result))))))

(ert-deftest agent-repl-sidebar-test-action-unknown-workspace-reports-error ()
  "A target that is not a known workspace fails loudly."
  (agent-repl-sidebar-test--with-state
    (let ((result (agent-repl-sidebar-test--run-action
                      '((id . "a3") (action . "interrupt") (targets . ["nope"])))))
      (should (eq :json-false (alist-get 'ok result)))
      (should (string-match-p "nope" (alist-get 'error result))))))

(ert-deftest agent-repl-sidebar-test-action-nuke-merged-requires-confirmation ()
  "Nuking a MERGED workspace without confirmed=true is refused."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :repl-state :merged)
    (let* ((finished nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a4") (action . "nuke") (targets . ["ws1"]))
                     ((symbol-function 'agent-repl--finish-workspace)
                      (lambda (ws) (push ws finished))))))
      (should-not finished)
      (should (eq :json-false (alist-get 'ok result)))
      (should (string-match-p "confirmation" (alist-get 'error result))))))

(ert-deftest agent-repl-sidebar-test-action-nuke-merged-confirmed-finishes ()
  "Nuking a MERGED workspace with confirmed=true finishes it."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :repl-state :merged)
    (let* ((finished nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a5") (action . "nuke") (targets . ["ws1"])
                         (confirmed . t))
                     ((symbol-function 'agent-repl--finish-workspace)
                      (lambda (ws) (push ws finished))))))
      (should (equal '("ws1") finished))
      (should (eq t (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-nuke-non-merged-dispatches ()
  "Nuking a non-MERGED workspace takes the standard nuke path."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (let* ((nuked nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a6") (action . "nuke") (targets . ["ws1"]))
                     ((symbol-function 'agent-repl-nuke-workspace)
                      (lambda (ws) (push ws nuked))))))
      (should (equal '("ws1") nuked))
      (should (eq t (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-kill-merged-refused ()
  "Killing a MERGED workspace is refused, mirroring the drawer's `d'."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :repl-state :merged)
    (let ((result (agent-repl-sidebar-test--run-action
                      '((id . "a7") (action . "kill") (targets . ["ws1"])))))
      (should (eq :json-false (alist-get 'ok result)))
      (should (string-match-p "MERGED" (alist-get 'error result))))))

(ert-deftest agent-repl-sidebar-test-action-send-prompt-dispatches-per-target ()
  "send-prompt sends the args prompt to every target."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl-sidebar-test--register "ws2")
    (let* ((sent nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a8") (action . "send-prompt")
                         (targets . ["ws1" "ws2"])
                         (args . ((prompt . "do the thing"))))
                     ((symbol-function 'agent-repl--send)
                      (lambda (prompt ws) (push (cons ws prompt) sent))))))
      (should (equal 2 (length sent)))
      (should (equal "do the thing" (cdr (assoc "ws1" sent))))
      (should (eq t (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-send-prompt-empty-refused ()
  "An empty prompt is refused rather than sent."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (let* ((sent nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a9") (action . "send-prompt")
                         (targets . ["ws1"]) (args . ((prompt . ""))))
                     ((symbol-function 'agent-repl--send)
                      (lambda (prompt ws) (push (cons ws prompt) sent))))))
      (should-not sent)
      (should (eq :json-false (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-visit-switches ()
  "visit switches to the target workspace."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (let* ((switched nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a10") (action . "visit") (targets . ["ws1"]))
                     ((symbol-function 'agent-repl--ws-switch)
                      (lambda (ws) (push ws switched))))))
      (should (equal '("ws1") switched))
      (should (eq t (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-visit-merged-reactivates ()
  "visit on a MERGED workspace reactivates instead of switching."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1" :repl-state :merged)
    (let* ((reactivated nil)
           (switched nil)
           (result (agent-repl-sidebar-test--run-action
                       '((id . "a11") (action . "visit") (targets . ["ws1"]))
                     ((symbol-function 'agent-repl-drawer--reactivate-merged)
                      (lambda (ws) (push ws reactivated)))
                     ((symbol-function 'agent-repl--ws-switch)
                      (lambda (ws) (push ws switched))))))
      (should (equal '("ws1") reactivated))
      (should-not switched)
      (should (eq t (alist-get 'ok result))))))

(ert-deftest agent-repl-sidebar-test-action-toggle-mark-shared-set ()
  "toggle-mark mutates the shared drawer marked-set."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl-sidebar-test--run-action
        '((id . "a12") (action . "toggle-mark") (targets . ["ws1"])))
    (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
      (should (agent-repl-drawer--marked-p "ws1")))))

(ert-deftest agent-repl-sidebar-test-action-clear-marks-empties-shared-set ()
  "clear-marks empties the shared drawer marked-set."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (agent-repl-sidebar-test--mark "ws1")
    (agent-repl-sidebar-test--run-action
        '((id . "a13") (action . "clear-marks") (targets . [])))
    (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
      (should-not (agent-repl-drawer--marked-p "ws1")))))

(ert-deftest agent-repl-sidebar-test-action-toggle-expand-refreshes-cache ()
  "Expanding via toggle-expand refreshes the target's detail cache."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (let ((refreshed nil))
      (agent-repl-sidebar-test--run-action
          '((id . "a14") (action . "toggle-expand") (targets . ["ws1"]))
        ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
         (lambda (ws) (push ws refreshed))))
      (should (equal '("ws1") refreshed))
      (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
        (should (agent-repl-drawer--expanded-p "ws1"))))))

(ert-deftest agent-repl-sidebar-test-action-toggle-fold-repaints-tab-bar ()
  "toggle-fold folds the repo key and forces a tab-bar repaint."
  (agent-repl-sidebar-test--with-state
    (let ((redraws 0))
      (agent-repl-sidebar-test--run-action
          `((id . "a15") (action . "toggle-fold")
            (targets . [,agent-repl-sidebar-test--group-key]))
        ((symbol-function 'agent-repl--force-tab-bar-redraw)
         (lambda () (setq redraws (1+ redraws)))))
      (should (equal 1 redraws))
      (should (agent-repl--repo-folded-p agent-repl-sidebar-test--group-key)))))

(ert-deftest agent-repl-sidebar-test-action-hide-sidebar-hides-drawer ()
  "hide-sidebar flips the shared visibility flag via `agent-repl-drawer-hide'."
  (agent-repl-sidebar-test--with-state
    (let ((hidden nil))
      (agent-repl-sidebar-test--run-action
          '((id . "a16") (action . "hide-sidebar") (targets . []))
        ((symbol-function 'agent-repl-drawer-hide)
         (lambda () (setq hidden t))))
      (should hidden))))

(ert-deftest agent-repl-sidebar-test-action-forces-push ()
  "Processing an action force-pushes a snapshot even when disabled-gated
state is otherwise unchanged."
  (agent-repl-sidebar-test--with-state
    (agent-repl-sidebar-test--register "ws1")
    (let ((agent-repl-sidebar-enabled t)
          (pushes 0))
      (cl-letf (((symbol-function 'agent-repl-sidebar--post-snapshot)
                 (lambda (_json) (setq pushes (1+ pushes))))
                ((symbol-function 'agent-repl-interrupt) #'ignore))
        (let* ((dir (make-temp-file "sidebar-actions" t))
               (agent-repl-sidebar-actions-dir (file-name-as-directory dir)))
          (unwind-protect
              (agent-repl-sidebar--process-action-file
               (agent-repl-sidebar-test--action-file
                dir '((id . "a17") (action . "interrupt") (targets . ["ws1"]))))
            (delete-directory dir t))))
      (should (>= pushes 1)))))

;;;; ---- Drain ----

(ert-deftest agent-repl-sidebar-test-drain-processes-matching-files-only ()
  "Drain processes sidebar_action_*.json and ignores other files."
  (agent-repl-sidebar-test--with-state
    (let* ((dir (make-temp-file "sidebar-actions" t))
           (agent-repl-sidebar-actions-dir (file-name-as-directory dir)))
      (unwind-protect
          (progn
            (agent-repl-sidebar-test--action-file
             dir '((id . "a18") (action . "clear-marks") (targets . [])))
            (with-temp-file (expand-file-name "unrelated.json" dir)
              (insert "{}"))
            (should (equal 1 (agent-repl-sidebar--drain-action-files)))
            (should (file-exists-p (expand-file-name "unrelated.json" dir))))
        (delete-directory dir t)))))

(provide 'test-sidebar)
;;; test-sidebar.el ends here
