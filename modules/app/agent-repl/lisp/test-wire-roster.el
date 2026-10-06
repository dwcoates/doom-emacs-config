;;; test-wire-roster.el --- ERT tests for agent-repl wire-roster.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-wire-roster.el -f ert-run-tests-batch-and-exit
;;
;; Fixtures are hand-written from `frontend/v1/sidebar.proto' in Go's
;; protojson shape.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defmacro agent-repl-test-wire-roster--quiet (&rest body)
  "Run BODY with the logging ladder stubbed out."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--error) (lambda (&rest _) nil))
             ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
     ,@body))

(defun agent-repl-test-wire-roster--parse (json)
  "Parse JSON exactly as the codec's callers do."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object :null :false-object :false))

(defun agent-repl-test-wire-roster--decode (decoder json)
  "Decode JSON with DECODER, quietly."
  (agent-repl-test-wire-roster--quiet
    (funcall decoder (agent-repl-test-wire-roster--parse json))))

(defun agent-repl-test-wire-roster--breach (decoder json)
  "Return the `agent-repl-wire-error' data decoding JSON with DECODER raises."
  (agent-repl-test-wire-roster--quiet
    (condition-case err
        (progn (funcall decoder (agent-repl-test-wire-roster--parse json)) nil)
      (agent-repl-wire-error (cdr err)))))

(defun agent-repl-test-wire-roster--row (&rest fragments)
  "Return a minimal valid RosterRow JSON carrying FRAGMENTS.
A row's non-optional message fields are all present; FRAGMENTS supply the
status arm and whatever else the case is about."
  (concat "{\"workspace\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/1\"}},"
          "\"name\":{\"text\":\"fix-flaky\"},"
          "\"current\":{},\"when\":{},\"detail\":{},\"closed\":{},"
          "\"availability\":{\"available\":{}}"
          (mapconcat (lambda (f) (concat "," f)) fragments "")
          "}"))

;;;; ---- RosterRow, whole ----

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-message-tree ()
  "A row decodes with the contract's nesting preserved, nothing flattened."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"ready\":{}"))
                 '(:workspace (:workspace (:id "ws-1" :dir "/w/1"))
                   :attention nil
                   :priority nil
                   :viewed nil
                   :reviving nil
                   :detached-live nil
                   :last-selected nil
                   :availability (:arm :available :value nil)
                   :name (:text "fix-flaky")
                   :status (:arm :ready :value nil)
                   :current (:current nil)
                   :children nil
                   :when nil
                   :detail (:branch nil :parent-branch nil :summary nil)
                   :closed (:closed nil)))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-viewed-marker ()
  "A row carrying `viewed' decodes it as PRESENT — the PARTIAL display mode."
  (should (eq (plist-get (agent-repl-test-wire-roster--decode
                          #'agent-repl-wire-decode-roster-row
                          (agent-repl-test-wire-roster--row
                           "\"ready\":{}" "\"viewed\":{}"))
                         :viewed)
              t)))

(ert-deftest agent-repl-test-wire-roster-row-viewed-carries-no-payload ()
  "`RosterRowViewed' is EMPTY: a field inside it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"ready\":{}" "\"viewed\":{\"mode\":\"partial\"}"))
                 '("RosterRowViewed" mode "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-reviving-marker ()
  "A row carrying `reviving' decodes it as PRESENT."
  (should (eq (plist-get (agent-repl-test-wire-roster--decode
                          #'agent-repl-wire-decode-roster-row
                          (agent-repl-test-wire-roster--row
                           "\"ready\":{}" "\"reviving\":{}"))
                         :reviving)
              t)))

(ert-deftest agent-repl-test-wire-roster-row-reviving-carries-no-payload ()
  "`RosterRowReviving' is EMPTY: a field inside it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"ready\":{}" "\"reviving\":{\"since\":1}"))
                 '("RosterRowReviving" since "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-detached-live-marker ()
  "A row carrying `detachedLive' decodes it as PRESENT."
  (should (eq (plist-get (agent-repl-test-wire-roster--decode
                          #'agent-repl-wire-decode-roster-row
                          (agent-repl-test-wire-roster--row
                           "\"done\":{}" "\"detachedLive\":{}"))
                         :detached-live)
              t)))

(ert-deftest agent-repl-test-wire-roster-row-detached-live-carries-no-payload ()
  "`RosterRowDetachedLive' is EMPTY: a field inside it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"done\":{}" "\"detachedLive\":{\"count\":1}"))
                 '("RosterRowDetachedLive" count "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-last-selected-instant ()
  "A row carrying `lastSelected' decodes its durable selection instant."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row
                              "\"ready\":{}" "\"lastSelected\":{\"atMs\":\"1756400000000\"}"))
                            :last-selected)
                 '(:at-ms 1756400000000))))

(ert-deftest agent-repl-test-wire-roster-row-last-selected-refuses-an-unknown-field ()
  "`RosterRowLastSelected' carries only its instant: anything else is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"ready\":{}" "\"lastSelected\":{\"atMs\":\"1\",\"by\":\"emacs\"}"))
                 '("RosterRowLastSelected" by "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-every-status-arm ()
  "Every one of the declared status arms decodes to its own keyword."
  (dolist (arm agent-repl-wire-roster-row-status-arms)
    (should (equal (plist-get (agent-repl-test-wire-roster--decode
                               #'agent-repl-wire-decode-roster-row
                               (agent-repl-test-wire-roster--row
                                (format "\"%s\":{}" (nth 0 arm))))
                              :status)
                   (list :arm (nth 1 arm) :value nil)))))

(ert-deftest agent-repl-test-wire-roster-row-status-arms-match-the-bindings ()
  "The arm table is exactly the frozen schema's, read from the Go bindings.
An arm landed in the contract without a decoder fails here rather than
arriving as an unknown field on some later push."
  (let ((declared (sort (agent-repl-test--generated-oneof-arms
                         "frontend/v1/sidebar.pb.go" "RosterRow")
                        #'string<))
        (spelled (sort (mapcar (lambda (arm) (symbol-name (nth 0 arm)))
                               agent-repl-wire-roster-row-status-arms)
                       #'string<)))
    (should (equal spelled declared))))

(ert-deftest agent-repl-test-wire-roster-row-status-count-is-twenty-five ()
  "The status vocabulary is the 25 arms the contract declares."
  (should (equal (length agent-repl-wire-roster-row-status-keywords) 25)))

(ert-deftest agent-repl-test-wire-roster-row-unset-status-is-a-breach ()
  "A row with no lifecycle is a contract breach, not a default dot."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row))
                 '("RosterRow" status "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-row-two-status-arms-is-a-breach ()
  "Setting more than one status arm is the same breach from the other side."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"ready\":{}" "\"thinking\":{}"))
                 '("RosterRow" status "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-roster-row-retired-merge-enqueuing-is-refused ()
  "The retired `merge_enqueuing' arm is refused as an unknown field.
No merge fact is shown before the requesting turn ends (merge-landing.md,
Landed change 1), so tag 19 is reserved."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"mergeEnqueuing\":{}"))
                 '("RosterRow" mergeEnqueuing "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-retired-merge-conflict-is-refused ()
  "The retired `merge_conflict' arm is refused as an unknown field.
A merge never parks (merge-landing.md, Landed change 1), so tag 22 is
reserved."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"mergeConflict\":{}"))
                 '("RosterRow" mergeConflict "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-merge-failed-decodes ()
  "The `merge_failed' arm decodes to `:merge-failed'."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row "\"mergeFailed\":{}"))
                            :status)
                 '(:arm :merge-failed :value nil))))

(ert-deftest agent-repl-test-wire-roster-row-unknown-status-arm-is-refused ()
  "An arm this build does not know is refused loudly, never drawn."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"hibernated\":{}"))
                 '("RosterRow" hibernated "unknown field"))))

;;;; ---- The row's optional elements ----

(ert-deftest agent-repl-test-wire-roster-row-absent-attention-is-nil ()
  "No unseen notification means the optional marker is simply absent."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row "\"ready\":{}"))
                            :attention)
                 nil)))

(ert-deftest agent-repl-test-wire-roster-row-present-attention-is-t ()
  "The attention marker is EMPTY and presence is the fact, so it decodes to t."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row
                              "\"ready\":{}" "\"attention\":{}"))
                            :attention)
                 t)))

(ert-deftest agent-repl-test-wire-roster-row-absent-priority-is-unprioritized ()
  "UNSET priority is unprioritized — no badge, never a \"P?\" placeholder."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row "\"ready\":{}"))
                            :priority)
                 nil)))

(ert-deftest agent-repl-test-wire-roster-row-present-priority-carries-its-label ()
  "The priority badge is a resolver-composed label the client draws verbatim."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row
                              "\"ready\":{}" "\"priority\":{\"label\":\"P0.5\"}"))
                            :priority)
                 '(:label "P0.5"))))

(ert-deftest agent-repl-test-wire-roster-row-without-a-workspace-is-a-breach ()
  "The row's join key is not optional."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"current\":{},"
                          "\"when\":{},\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" workspace "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-without-a-name-is-a-breach ()
  "Every row has a display name element."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},\"ready\":{},\"availability\":{\"available\":{}},"
                          "\"current\":{},\"when\":{},\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" name "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-without-closed-is-a-breach ()
  "The receded-styling element is stated by the resolver, always."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"current\":{},"
                          "\"when\":{},\"detail\":{}}"))
                 '("RosterRow" closed "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-closed-true-decodes-to-t ()
  "A closed row is switchable but its panes are dismissed."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                                     "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},"
                                     "\"current\":{},\"when\":{},\"detail\":{},"
                                     "\"closed\":{\"closed\":true}}"))
                            :closed)
                 '(:closed t))))

;;;; ---- The when column ----

(ert-deftest agent-repl-test-wire-roster-when-unset-is-legal ()
  "UNSET oneof = nothing to show; the column is empty, not \"0ms ago\"."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when "{}")
                 nil)))

(ert-deftest agent-repl-test-wire-roster-when-refuses-the-retired-last-selected-arm ()
  "`last_selected' is retired and reserved in `RosterRowWhen': it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"lastSelected\":{\"atMs\":\"1756400000000\"}}")
                 '("RosterRowWhen" lastSelected "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-when-active-carries-its-instant ()
  "Regression: the daemon added a `active' (last-activity) arm to the when
column (2026-09-15); the strict decoder must accept it or it rejects the whole
WatchWorkspaceRoster push and every tab falls back to a stale blue status."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"active\":{\"atMs\":\"1756400000000\"}}")
                 '(:arm :active :value (:at-ms 1756400000000)))))

(ert-deftest agent-repl-test-wire-roster-when-created-carries-its-instant ()
  "Regression: the `created' fallback arm must decode too (see `active')."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"created\":{\"atMs\":\"1756400000000\"}}")
                 '(:arm :created :value (:at-ms 1756400000000)))))

(ert-deftest agent-repl-test-wire-roster-when-merged-accepts-a-numeric-instant ()
  "protojson accepts a number for int64, and the merged instant is one."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"merged\":{\"atMs\":1756400000000}}")
                 '(:arm :merged :value (:at-ms 1756400000000)))))

(ert-deftest agent-repl-test-wire-roster-when-two-arms-is-a-breach ()
  "The daemon chooses ONE value for the column; two is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"active\":{\"atMs\":\"1\"},\"merged\":{\"atMs\":\"2\"}}")
                 '("RosterRowWhen" shown "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-roster-row-without-when-is-a-breach ()
  "The when element itself is a required message, even when its oneof is unset."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"current\":{},"
                          "\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" when "required message field is absent"))))

;;;; ---- The detail panel ----

(ert-deftest agent-repl-test-wire-roster-detail-absent-lines-are-nil ()
  "Each detail line is PRESENT OR ABSENT BY MESSAGE PRESENCE."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-detail "{}")
                 '(:branch nil :parent-branch nil :summary nil))))

(ert-deftest agent-repl-test-wire-roster-detail-present-lines-decode ()
  "A present line decodes to its own plist, an absent sibling to nil."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-detail
                  "{\"branch\":{\"name\":\"proto/wire\"},\"summary\":{\"text\":\"codec\"}}")
                 '(:branch (:name "proto/wire")
                   :parent-branch nil
                   :summary (:text "codec")))))

(ert-deftest agent-repl-test-wire-roster-detail-present-empty-line-is-not-absent ()
  "A line present but empty decodes to its plist, not to nil."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row-detail
                             "{\"branch\":{}}")
                            :branch)
                 '(:name ""))))

(ert-deftest agent-repl-test-wire-roster-detail-refuses-an-unknown-line ()
  "An unmodeled detail line is refused as an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-detail "{\"ahead\":{\"n\":1}}")
                 '("RosterRowDetail" ahead "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-without-detail-is-a-breach ()
  "The detail panel element is required even when all three lines are absent."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"current\":{},"
                          "\"when\":{},\"closed\":{}}"))
                 '("RosterRow" detail "required message field is absent"))))

;;;; ---- Nested families ----

(ert-deftest agent-repl-test-wire-roster-row-absent-children-is-the-empty-list ()
  "A row with no spawned family decodes to no children."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row "\"ready\":{}"))
                            :children)
                 nil)))

(ert-deftest agent-repl-test-wire-roster-row-children-decode-recursively ()
  "A nested family decodes as rows, to any depth, in render order."
  (let* ((grandchild (agent-repl-test-wire-roster--row "\"done\":{}"))
         (child (agent-repl-test-wire-roster--row
                 "\"thinking\":{}" (format "\"children\":[%s]" grandchild)))
         (row (agent-repl-test-wire-roster--row
               "\"ready\":{}" (format "\"children\":[%s]" child)))
         (decoded (agent-repl-test-wire-roster--decode
                   #'agent-repl-wire-decode-roster-row row)))
    (should (equal (plist-get
                    (car (plist-get (car (plist-get decoded :children)) :children))
                    :status)
                   '(:arm :done :value nil)))))

;;;; ---- Sections ----

(ert-deftest agent-repl-test-wire-roster-rows-empty-is-the-empty-list ()
  "A section with no rows decodes to an empty rows list."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-rows "{\"rows\":[]}")
                 '(:rows nil))))

(ert-deftest agent-repl-test-wire-roster-rows-populated-decodes-in-order ()
  "Rows decode in the resolver's order; clients do not re-sort."
  (let ((json (format "{\"rows\":[%s,%s]}"
                      (agent-repl-test-wire-roster--row "\"ready\":{}")
                      (agent-repl-test-wire-roster--row "\"merged\":{}"))))
    (should (equal (mapcar (lambda (row) (plist-get (plist-get row :status) :arm))
                           (plist-get (agent-repl-test-wire-roster--decode
                                       #'agent-repl-wire-decode-roster-rows json)
                                      :rows))
                   '(:ready :merged)))))

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-key-header-rows ()
  "A repo section carries its stable key, its header, its rows and its fold."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repo-section
                  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
                          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                          "\"rows\":{\"rows\":[]},\"expanded\":{}}"))
                 '(:key (:repository (:id "r1" :dir "/src"))
                   :header (:label (:text "doom") :count (:workspaces 2))
                   :rows (:rows nil)
                   :fold (:arm :expanded :value nil)))))

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-a-collapsed-fold ()
  "A collapsed section decodes its fold arm as `:collapsed'."
  (should (eq (plist-get
               (plist-get (agent-repl-test-wire-roster--decode
                           #'agent-repl-wire-decode-roster-repo-section
                           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
                                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                                   "\"rows\":{\"rows\":[]},\"collapsed\":{}}"))
                          :fold)
               :arm)
              :collapsed)))

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-fold-is-a-breach ()
  "The fold is never unset on a wire section."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section
           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                   "\"rows\":{\"rows\":[]}}"))))

(ert-deftest agent-repl-test-wire-roster-repo-section-with-both-folds-is-a-breach ()
  "Expanded and collapsed at once is a malformed frame."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section
           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                   "\"rows\":{\"rows\":[]},\"expanded\":{},\"collapsed\":{}}"))))

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-key-is-a-breach ()
  "The section's fold and join key is required."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-section
                  "{\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},\"rows\":{\"rows\":[]}}")
                 '("RosterRepoSection" key "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-section-header-without-a-label-is-a-breach ()
  "A header always has one author for its heading text."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-section-header "{}")
                 '("RosterSectionHeader" label "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-task-section-carries-the-done-check ()
  "A task header carries the done axis, which repos do not have."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-task-section
                  (concat "{\"key\":{\"taskId\":\"t-9\"},"
                          "\"header\":{\"label\":{\"text\":\"ship codec\"},"
                          "\"done\":{\"done\":true}},"
                          "\"rows\":{\"rows\":[]},\"expanded\":{}}"))
                 '(:key (:task-id "t-9")
                   :header (:label (:text "ship codec") :done (:done t))
                   :rows (:rows nil)
                   :fold (:arm :expanded :value nil)))))

(ert-deftest agent-repl-test-wire-roster-task-header-without-done-is-a-breach ()
  "The done check is an element of the task header, not an optional extra."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-task-section-header
                  "{\"label\":{\"text\":\"t\"}}")
                 '("RosterTaskSectionHeader" done "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-merged-section-has-no-key ()
  "The recently-merged section's fold identity is fixed; it carries no key."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-merged-section
                  (concat "{\"header\":{\"label\":{\"text\":\"Recently Merged\"},\"count\":{\"workspaces\":2}},"
                          "\"rows\":{\"rows\":[]},\"collapsed\":{}}"))
                 '(:header (:label (:text "Recently Merged") :count (:workspaces 2)) :rows (:rows nil)
                   :fold (:arm :collapsed :value nil)))))

(ert-deftest agent-repl-test-wire-roster-merged-section-refuses-a-key ()
  "A key on the merged section is an unknown field, refused."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-merged-section
                  (concat "{\"key\":{},\"header\":{\"label\":{\"text\":\"m\"},\"count\":{\"workspaces\":2}},"
                          "\"rows\":{\"rows\":[]}}"))
                 '("RosterMergedSection" key "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-label-refuses-an-unknown-field ()
  "An unknown key on a label is refused at the label."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-label "{\"text\":\"a\",\"icon\":\"x\"}")
                 '("RosterLabel" icon "unknown field"))))

;;;; ---- The roster ----

(defconst agent-repl-test-wire-roster--roster-json
  (concat "{\"repository\":{\"sections\":[{"
          "\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
          "\"rows\":{\"rows\":[]},\"expanded\":{}}]},"
          "\"task\":{\"sections\":[]},"
          "\"recentlyMerged\":{\"header\":{\"label\":{\"text\":\"Recently Merged\"},\"count\":{\"workspaces\":2}},"
          "\"rows\":{\"rows\":[]},\"collapsed\":{}},"
          "\"current\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/1\"}},"
          "\"shownRepository\":{}}")
  "A whole roster: both groupings resolved, the merged section, and current.")

(ert-deftest agent-repl-test-wire-roster-decodes-the-whole-roster ()
  "The roster is always whole, never a delta, and decodes as one picture."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-workspace-roster
                  agent-repl-test-wire-roster--roster-json)
                 '(:repository (:sections ((:key (:repository (:id "r1" :dir "/src"))
                                            :header (:label (:text "doom") :count (:workspaces 2))
                                            :rows (:rows nil)
                                            :fold (:arm :expanded :value nil))))
                   :task (:sections nil)
                   :recently-merged (:header (:label (:text "Recently Merged") :count (:workspaces 2))
                                     :rows (:rows nil)
                                     :fold (:arm :collapsed :value nil))
                   :current (:workspace (:id "ws-1" :dir "/w/1"))
                   :shown (:arm :shown-repository :value nil)))))

(ert-deftest agent-repl-test-wire-roster-absent-current-is-nil ()
  "UNSET current means there is no selected workspace."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (concat "{\"repository\":{\"sections\":[]},"
                                     "\"task\":{\"sections\":[]},"
                                     "\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},"
                                     "\"rows\":{},\"collapsed\":{}},\"shownRepository\":{}}"))
                            :current)
                 nil)))

(ert-deftest agent-repl-test-wire-roster-without-the-repository-view-is-a-breach ()
  "BOTH groupings arrive fully resolved; an absent one is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"task\":{\"sections\":[]},"
                          "\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{}}}"))
                 '("WorkspaceRoster" repository "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-the-task-view-is-a-breach ()
  "The task grouping is resolved alongside the repository grouping."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[]},"
                          "\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{}}}"))
                 '("WorkspaceRoster" task "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-recently-merged-is-a-breach ()
  "The merged section is hoisted onto every roster, always present."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  "{\"repository\":{\"sections\":[]},\"task\":{\"sections\":[]}}")
                 '("WorkspaceRoster" recentlyMerged "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-refuses-an-unknown-field ()
  "An unknown key on the roster is refused at the roster."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[]},\"task\":{\"sections\":[]},"
                          "\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{}},"
                          "\"hibernating\":{}}"))
                 '("WorkspaceRoster" hibernating "unknown field"))))

;;;; ---- The view state: folds and the grouping shown ----

(defconst agent-repl-test-wire-roster--merged-json
  "\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"collapsed\":{}}"
  "A merged band, collapsed, as a JSON member.")

(defun agent-repl-test-wire-roster--with-shown (shown)
  "A roster JSON whose `shown' members are SHOWN (a JSON fragment, maybe empty)."
  (concat "{\"repository\":{\"sections\":[]},\"task\":{\"sections\":[]},"
          agent-repl-test-wire-roster--merged-json
          (if (string-empty-p shown) "" (concat "," shown))
          "}"))

(ert-deftest agent-repl-test-wire-roster-decodes-the-task-grouping-shown ()
  "WHICH GROUPING IS SHOWN is the daemon's view state, decoded as its arm."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (agent-repl-test-wire-roster--with-shown "\"shownTask\":{}"))
                            :shown)
                 '(:arm :shown-task :value nil))))

(ert-deftest agent-repl-test-wire-roster-without-a-grouping-shown-is-a-breach ()
  "The grouping shown is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (agent-repl-test-wire-roster--with-shown ""))
                 '("WorkspaceRoster" shown "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-with-both-groupings-shown-is-a-breach ()
  "Exactly one grouping is shown."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (agent-repl-test-wire-roster--with-shown "\"shownTask\":{},\"shownRepository\":{}"))
                 '("WorkspaceRoster" shown "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-roster-decodes-a-task-section-fold ()
  "A task section's fold is the daemon's view state, decoded as its arm."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-task-section
                             (concat "{\"key\":{\"taskId\":\"t1\"},"
                                     "\"header\":{\"label\":{},\"done\":{}},"
                                     "\"rows\":{},\"collapsed\":{}}"))
                            :fold)
                 '(:arm :collapsed :value nil))))

(ert-deftest agent-repl-test-wire-roster-a-task-section-without-a-fold-is-a-breach ()
  "A task section's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-task-section
                  (concat "{\"key\":{\"taskId\":\"t1\"},"
                          "\"header\":{\"label\":{},\"done\":{}},\"rows\":{}}"))
                 '("RosterTaskSection" fold "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-decodes-the-merged-band-fold ()
  "The merged band's fold is the daemon's view state, decoded as its arm."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-merged-section
                             "{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}}")
                            :fold)
                 '(:arm :expanded :value nil))))

(ert-deftest agent-repl-test-wire-roster-a-merged-band-without-a-fold-is-a-breach ()
  "The merged band's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-merged-section
                  "{\"header\":{\"label\":{},\"count\":{}},\"rows\":{}}")
                 '("RosterMergedSection" fold "oneof is unset"))))

;;;; ---- The rpc ----

(ert-deftest agent-repl-test-wire-roster-request-is-empty ()
  "The roster is global, so the request has nothing to address."
  (should (equal (agent-repl-test-wire-roster--quiet
                   (json-serialize
                    (agent-repl-wire-encode-watch-workspace-roster-request nil)))
                 "{}")))

(ert-deftest agent-repl-test-wire-roster-response-wraps-the-roster ()
  "The roster arm wraps `frontend.v1.WorkspaceRoster' itself, whole."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-watch-workspace-roster-response
                  (format "{\"roster\":%s}" agent-repl-test-wire-roster--roster-json))
                 (list :arm :roster
                       :value (agent-repl-test-wire-roster--decode
                               #'agent-repl-wire-decode-workspace-roster
                               agent-repl-test-wire-roster--roster-json)))))

(ert-deftest agent-repl-test-wire-roster-response-ending-decodes-to-its-arm ()
  "The planned ending decodes to the `:ending' arm, carrying nothing."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-watch-workspace-roster-response
                  "{\"ending\":{}}")
                 '(:arm :ending :value nil))))

(ert-deftest agent-repl-test-wire-roster-response-push-arms-pinned ()
  "The response's push arms are exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_watch_workspace_roster.pb.go"
                        "WatchWorkspaceRosterResponse")
                       #'string<)
                 (sort (list "roster" "ending") #'string<))))

(ert-deftest agent-repl-test-wire-roster-response-without-an-arm-is-a-breach ()
  "A roster push carrying neither arm is a breach, not an empty sidebar."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-watch-workspace-roster-response "{}")
                 '("WatchWorkspaceRosterResponse" push "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-response-refuses-an-unknown-field ()
  "An unknown key on the response is refused."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-watch-workspace-roster-response
                  "{\"epoch\":1}")
                 '("WatchWorkspaceRosterResponse" epoch "unknown field"))))

(provide 'test-wire-roster)

;;; test-wire-roster.el ends here

;;;; ---- RosterRow.availability ----

(ert-deftest agent-repl-test-wire-roster-availability-decodes-pending ()
  "The daemon is still bringing the session up."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-availability "{\"pending\":{}}")
                 '(:arm :pending :value nil))))

(ert-deftest agent-repl-test-wire-roster-availability-decodes-available ()
  "The shim link has connected."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-availability "{\"available\":{}}")
                 '(:arm :available :value nil))))

(ert-deftest agent-repl-test-wire-roster-availability-decodes-unavailable ()
  "The bring-up ended without a session."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-availability "{\"unavailable\":{}}")
                 '(:arm :unavailable :value nil))))

(ert-deftest agent-repl-test-wire-roster-availability-unset-is-a-breach ()
  "An unset availability oneof is a contract breach, never a default."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-availability "{}")
                 '("RosterRowAvailability" availability "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-row-without-availability-is-a-breach ()
  "Every row states whether its workspace may be opened yet."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"current\":{},"
                          "\"when\":{},\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" availability "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-section-header-decodes-its-count ()
  "A section header carries how many workspaces the section holds."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-section-header
                  "{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":14}}")
                 '(:label (:text "doom") :count (:workspaces 14)))))

(ert-deftest agent-repl-test-wire-roster-section-header-without-a-count-is-a-breach ()
  "The count is always set, so its absence is a contract breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-section-header
                  "{\"label\":{\"text\":\"doom\"}}")
                 '("RosterSectionHeader" count "required message field is absent"))))
