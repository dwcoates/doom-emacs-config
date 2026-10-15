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
          "\"current\":{},\"when\":{\"noTurnRunning\":{}},\"detail\":{},\"closed\":{},"
          "\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"}"
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
                   :one-shot nil
                   :opening nil
                   :async-merging nil
                   :last-selected nil
                   :last-turn-ended nil
                   :closure nil
                   :last-updated (:at-ms 1)
                   :availability (:arm :available :value nil)
                   :name (:text "fix-flaky")
                   :status (:arm :ready :value nil)
                   :current (:current nil)
                   :children nil
                   :when (:arm :no-turn-running :value nil)
                   :detail (:branch nil :parent-branch nil :summary nil :commits nil)
                   :closed (:closed nil)))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-one-shot-mark ()
  "A row carrying `oneShot' decodes it as PRESENT."
  (should (eq (plist-get (agent-repl-test-wire-roster--decode
                          #'agent-repl-wire-decode-roster-row
                          (agent-repl-test-wire-roster--row "\"ready\":{}" "\"oneShot\":{}"))
                         :one-shot)
              t)))

(ert-deftest agent-repl-test-wire-roster-row-one-shot-mark-carries-no-payload ()
  "`RosterRowOneShot' is EMPTY: a retired outcome inside it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"ready\":{}" "\"oneShot\":{\"failed\":{}}"))
                 '("RosterRowOneShot" failed "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-async-merging-marker ()
  "A row carrying `asyncMerging\=' decodes it as PRESENT, beside its status arm."
  ;; Act
  (let ((row (agent-repl-test-wire-roster--decode
              #'agent-repl-wire-decode-roster-row
              (agent-repl-test-wire-roster--row "\"thinking\":{}" "\"asyncMerging\":{}"))))
    ;; Assert
    (should (eq (plist-get row :async-merging) t))
    (should (equal (plist-get row :status) '(:arm :thinking :value nil)))))

(ert-deftest agent-repl-test-wire-roster-row-async-merging-marker-carries-no-payload ()
  "`RosterRowAsyncMerging\=' is EMPTY: a field inside it is an unknown field."
  ;; Act / Assert
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"ready\":{}" "\"asyncMerging\":{\"repo\":{}}"))
                 '("RosterRowAsyncMerging" repo "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-opening-marker ()
  "A row carrying `opening' decodes it as PRESENT, beside its status arm."
  (let ((row (agent-repl-test-wire-roster--decode
              #'agent-repl-wire-decode-roster-row
              (agent-repl-test-wire-roster--row "\"inactive\":{}" "\"opening\":{}"))))
    (should (equal (list (plist-get row :opening) (plist-get row :status))
                   '(t (:arm :inactive :value nil))))))

(ert-deftest agent-repl-test-wire-roster-row-opening-marker-carries-no-payload ()
  "`RosterRowOpening' is EMPTY: a field inside it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"inactive\":{}" "\"opening\":{\"why\":1}"))
                 '("RosterRowOpening" why "unknown field"))))

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

(ert-deftest agent-repl-test-wire-roster-row-refuses-the-retired-merge-workspace-marker ()
  "`RosterRow.merge_workspace' is retired (tag 58 reserved): a row carrying it is unknown."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"merging\":{}" "\"mergeWorkspace\":{\"label\":{\"text\":\"merge → master\"}}"))
                 '("RosterRow" mergeWorkspace "unknown field"))))

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

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-last-turn-ended-instant ()
  "A row carrying `lastTurnEnded' decodes its durable turn-end instant."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row
                              "\"done\":{}" "\"lastTurnEnded\":{\"atMs\":\"1756400000000\"}"))
                            :last-turn-ended)
                 '(:at-ms 1756400000000))))

(ert-deftest agent-repl-test-wire-roster-row-last-turn-ended-refuses-an-unknown-field ()
  "`RosterRowLastTurnEnded' carries only its instant: anything else is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"done\":{}" "\"lastTurnEnded\":{\"atMs\":\"1\",\"kind\":\"done\"}"))
                 '("RosterRowLastTurnEnded" kind "unknown field"))))

(defun agent-repl-test-wire-roster--row-updated-at (last-updated)
  "Return a minimal valid RosterRow JSON whose `lastUpdated' is LAST-UPDATED.
LAST-UPDATED is the field's raw JSON, or nil to leave the field absent."
  (concat "{\"workspace\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/1\"}},"
          "\"name\":{\"text\":\"fix-flaky\"},\"ready\":{},"
          "\"current\":{},\"when\":{\"noTurnRunning\":{}},\"detail\":{},\"closed\":{},"
          "\"availability\":{\"available\":{}}"
          (if last-updated (concat ",\"lastUpdated\":" last-updated) "")
          "}"))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-last-updated-instant ()
  "A row's `lastUpdated' decodes to its update recency in epoch ms."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row-updated-at
                              "{\"atMs\":\"1756400000000\"}"))
                            :last-updated)
                 '(:at-ms 1756400000000))))

(ert-deftest agent-repl-test-wire-roster-row-without-last-updated-is-a-breach ()
  "Every row states its update recency: an absent one is a contract breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row-updated-at nil))
                 '("RosterRow" lastUpdated "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-last-updated-refuses-an-unknown-field ()
  "`RosterRowLastUpdated' carries only its instant: anything else is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row-updated-at
                   "{\"atMs\":\"1\",\"by\":\"emacs\"}"))
                 '("RosterRowLastUpdated" by "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-close-record ()
  "A row carrying `closure' decodes its durable close instant and its cause."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (agent-repl-test-wire-roster--row
                              "\"inactive\":{}"
                              "\"closure\":{\"atMs\":\"1756400000000\",\"user\":{}}"))
                            :closure)
                 '(:at-ms 1756400000000 :by (:arm :user :value nil)))))

(ert-deftest agent-repl-test-wire-roster-row-closure-decodes-every-cause ()
  "Every declared close cause decodes to its own keyword."
  (dolist (arm agent-repl-wire-roster-row-closure-by-arms)
    (should (equal (plist-get (agent-repl-test-wire-roster--decode
                               #'agent-repl-wire-decode-roster-row-closure
                               (format "{\"atMs\":\"1\",\"%s\":{}}" (nth 0 arm)))
                              :by)
                   (list :arm (nth 1 arm) :value nil)))))

(ert-deftest agent-repl-test-wire-roster-row-closure-refuses-an-unset-cause ()
  "A close record with no cause is a breach, never a default."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-closure
                  "{\"atMs\":\"1\"}")
                 '("RosterRowClosure" by "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-row-closure-refuses-an-unknown-field ()
  "`RosterRowClosure' carries only its instant and its cause."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-closure
                  "{\"atMs\":\"1\",\"user\":{},\"why\":\"x\"}")
                 '("RosterRowClosure" why "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-row-closure-arms-match-the-contract ()
  "The cause table names exactly the generated oneof's arms."
  (should (equal (sort (mapcar (lambda (arm) (symbol-name (nth 0 arm)))
                               agent-repl-wire-roster-row-closure-by-arms)
                       #'string<)
                 (sort (agent-repl-test--generated-oneof-arms
                        "frontend/v1/sidebar.pb.go" "RosterRowClosure")
                       #'string<))))

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

(ert-deftest agent-repl-test-wire-roster-row-status-count-is-thirty-one ()
  "The status vocabulary is the 31 arms the contract declares."
  (should (equal (length agent-repl-wire-roster-row-status-keywords) 31)))

(ert-deftest agent-repl-test-wire-roster-decodes-the-cold-gate-arm ()
  "A session parked on its cold gate decodes to its empty arm."
  (should (equal (agent-repl-wire--decode-oneof
                  "RosterRow" 'status '((coldGate))
                  agent-repl-wire-roster-row-status-arms)
                 '(:arm :cold-gate :value nil))))

(ert-deftest agent-repl-test-wire-roster-decodes-the-closing-arm ()
  "The closing arm decodes to its keyword: a refused close."
  (should (memq :closing agent-repl-wire-roster-row-status-keywords)))

(ert-deftest agent-repl-test-wire-roster-decodes-the-daemon-impaired-arm ()
  "The daemon-impaired arm decodes to its keyword."
  (should (memq :daemon-impaired agent-repl-wire-roster-row-status-keywords)))

(ert-deftest agent-repl-test-wire-roster-decodes-the-waiting-arm ()
  "The waiting arm decodes to its keyword: a cold gate or an interrupt landing."
  (should (memq :waiting agent-repl-wire-roster-row-status-keywords)))

(ert-deftest agent-repl-test-wire-roster-decodes-the-question-arm ()
  "The question arm decodes to its keyword: a question gate (owner ruling,
2026-10-08)."
  (should (assq 'question agent-repl-wire-roster-row-status-arms))
  (should (memq :question agent-repl-wire-roster-row-status-keywords)))

(ert-deftest agent-repl-test-wire-roster-decodes-the-turn-died-arm ()
  "The turn-died arm decodes to its keyword (owner ruling, 2026-10-06)."
  (should (assq 'turnDied agent-repl-wire-roster-row-status-arms))
  (should (memq :turn-died agent-repl-wire-roster-row-status-keywords)))

(ert-deftest agent-repl-test-wire-roster-async-merging-is-no-status-arm ()
  "An async merge is a row marker, never a status arm (owner request, 2026-10-13)."
  ;; Act / Assert
  (should-not (assq 'asyncMerging agent-repl-wire-roster-row-status-arms))
  (should-not (memq :async-merging agent-repl-wire-roster-row-status-keywords)))

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
                  (concat "{\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"},\"current\":{},"
                          "\"when\":{\"noTurnRunning\":{}},\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" workspace "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-without-a-name-is-a-breach ()
  "Every row has a display name element."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},\"ready\":{},\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"},"
                          "\"current\":{},\"when\":{\"noTurnRunning\":{}},\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" name "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-without-closed-is-a-breach ()
  "The receded-styling element is stated by the resolver, always."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"},\"current\":{},"
                          "\"when\":{\"noTurnRunning\":{}},\"detail\":{}}"))
                 '("RosterRow" closed "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-row-closed-true-decodes-to-t ()
  "A closed row is switchable but its panes are dismissed."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row
                             (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                                     "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"},"
                                     "\"current\":{},\"when\":{\"noTurnRunning\":{}},\"detail\":{},"
                                     "\"closed\":{\"closed\":true}}"))
                            :closed)
                 '(:closed t))))

;;;; ---- The when column ----

(ert-deftest agent-repl-test-wire-roster-when-refuses-the-retired-last-selected-arm ()
  "`last_selected' is retired and reserved in `RosterRowWhen': it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"lastSelected\":{\"atMs\":\"1756400000000\"}}")
                 '("RosterRowWhen" lastSelected "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-when-arms-match-the-bindings ()
  "The when-arm table is exactly the frozen schema's, read from the Go bindings."
  (should (equal (sort (mapcar (lambda (arm) (symbol-name (nth 0 arm)))
                               agent-repl-wire-roster-row-when-arms)
                       #'string<)
                 (sort (agent-repl-test--generated-oneof-arms
                        "frontend/v1/sidebar.pb.go" "RosterRowWhen")
                       #'string<))))

(defconst agent-repl-test-wire-roster--when-arm-samples
  '((syncMerging "{\"startedAtMs\":\"1\"}" (:started-at-ms 1))
    (turnRunning "{\"startedAtMs\":\"1\"}" (:started-at-ms 1))
    (noTurnRunning "{}" nil)
    (merged "{\"atMs\":\"1\"}" (:at-ms 1))
    (hidden "{\"atMs\":\"1\"}" (:at-ms 1 :cache nil)))
  "One payload per when arm and the value it decodes to.")

(ert-deftest agent-repl-test-wire-roster-when-decodes-every-arm-of-its-table ()
  "Every arm of the one when-arm table is accepted by the key allow-list and
decodes to its keyword: the decode and the allow-list read the same table."
  (should (equal (mapcar #'car agent-repl-wire-roster-row-when-arms)
                 (mapcar #'car agent-repl-test-wire-roster--when-arm-samples)))
  (dolist (arm agent-repl-wire-roster-row-when-arms)
    (let ((sample (alist-get (nth 0 arm) agent-repl-test-wire-roster--when-arm-samples)))
      (should (equal (agent-repl-test-wire-roster--decode
                      #'agent-repl-wire-decode-roster-row-when
                      (format "{\"%s\":%s}" (nth 0 arm) (nth 0 sample)))
                     (list :arm (nth 1 arm) :value (nth 1 sample)))))))

;;;; ---- The shared instant decoder ----

(ert-deftest agent-repl-test-wire-roster-decode-instant-reads-at-ms ()
  "The instant decoder answers the plist `(:at-ms)' from a decimal string."
  (should (equal (agent-repl-test-wire-roster--decode
                  (lambda (v) (agent-repl-wire-roster--decode-instant "Some" v))
                  "{\"atMs\":\"1756400000000\"}")
                 '(:at-ms 1756400000000))))

(ert-deftest agent-repl-test-wire-roster-decode-instant-refuses-an-extra-key ()
  "An unknown field is a breach naming the MESSAGE the caller passed."
  (should (equal (agent-repl-test-wire-roster--breach
                  (lambda (v) (agent-repl-wire-roster--decode-instant "Some" v))
                  "{\"atMs\":\"1\",\"extra\":1}")
                 '("Some" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-instant-decoders-share-the-helper ()
  "Every roster instant decoder is the shared helper under its own message
name: each one's breach names that message exactly as the helper's does."
  (dolist (pair '((agent-repl-wire-decode-roster-row-last-selected . "RosterRowLastSelected")
                  (agent-repl-wire-decode-roster-row-last-turn-ended . "RosterRowLastTurnEnded")
                  (agent-repl-wire-decode-roster-row-when-merged . "RosterRowWhenMerged")))
    (let ((json "{\"atMs\":\"1\",\"extra\":1}"))
      (should (equal (agent-repl-test-wire-roster--breach (car pair) json)
                     (agent-repl-test-wire-roster--breach
                      (lambda (v) (agent-repl-wire-roster--decode-instant (cdr pair) v))
                      json))))))

;;;; ---- The shared start decoder ----

(ert-deftest agent-repl-test-wire-roster-decode-started-at-reads-started-at-ms ()
  "The start decoder answers the plist `(:started-at-ms)' from a decimal string."
  (should (equal (agent-repl-test-wire-roster--decode
                  (lambda (v) (agent-repl-wire-roster--decode-started-at "Some" v))
                  "{\"startedAtMs\":\"1756400000000\"}")
                 '(:started-at-ms 1756400000000))))

(ert-deftest agent-repl-test-wire-roster-decode-started-at-refuses-an-extra-key ()
  "An unknown field is a breach naming the MESSAGE the caller passed."
  (should (equal (agent-repl-test-wire-roster--breach
                  (lambda (v) (agent-repl-wire-roster--decode-started-at "Some" v))
                  "{\"startedAtMs\":\"1\",\"extra\":1}")
                 '("Some" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-decode-started-at-refuses-a-non-integer ()
  "A start that is no int64 is a breach naming the MESSAGE and the field."
  (should (equal (car (agent-repl-test-wire-roster--breach
                       (lambda (v) (agent-repl-wire-roster--decode-started-at "Some" v))
                       "{\"startedAtMs\":\"soon\"}"))
                 "Some")))

(ert-deftest agent-repl-test-wire-roster-start-decoders-share-the-helper ()
  "Every counting when-arm decoder is the shared helper under its own message
name: each one's breach names that message exactly as the helper's does."
  (dolist (pair '((agent-repl-wire-decode-roster-row-when-sync-merging . "RosterRowWhenSyncMerging")
                  (agent-repl-wire-decode-roster-row-when-turn-running . "RosterRowWhenTurnRunning")))
    (let ((json "{\"startedAtMs\":\"1\",\"extra\":1}"))
      (should (equal (agent-repl-test-wire-roster--breach (car pair) json)
                     (agent-repl-test-wire-roster--breach
                      (lambda (v) (agent-repl-wire-roster--decode-started-at (cdr pair) v))
                      json))))))

(ert-deftest agent-repl-test-wire-roster-when-turn-running-carries-its-start ()
  "Regression: the strict decoder must accept the `turnRunning' arm or it
rejects the whole WatchWorkspaceRoster push and every tab falls back to a
stale blue status."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"turnRunning\":{\"startedAtMs\":\"1756400000000\"}}")
                 '(:arm :turn-running :value (:started-at-ms 1756400000000)))))

(ert-deftest agent-repl-test-wire-roster-when-turn-running-refuses-an-unknown-field ()
  "The running arm carries its start alone."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when-turn-running
                  "{\"startedAtMs\":\"1\",\"atMs\":\"1\"}")
                 '("RosterRowWhenTurnRunning" atMs "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-when-sync-merging-carries-its-start ()
  "Regression: the strict decoder must accept the `syncMerging' arm (see
`turnRunning')."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"syncMerging\":{\"startedAtMs\":\"1756400000000\"}}")
                 '(:arm :sync-merging :value (:started-at-ms 1756400000000)))))

(ert-deftest agent-repl-test-wire-roster-when-sync-merging-refuses-an-unknown-field ()
  "The sync merging arm carries its start alone."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when-sync-merging
                  "{\"startedAtMs\":\"1\",\"atMs\":\"1\"}")
                 '("RosterRowWhenSyncMerging" atMs "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-when-no-turn-running-decodes ()
  "The `noTurnRunning' arm must decode too (see `turnRunning')."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"noTurnRunning\":{}}")
                 '(:arm :no-turn-running :value nil))))

(ert-deftest agent-repl-test-wire-roster-when-unset-is-a-breach ()
  "The when-column's oneof is never unset."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when "{}")
                 '("RosterRowWhen" shown "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-when-retired-turn-ended-and-created-are-refused ()
  "The retired `turnEnded' and `created' arms (fields 5 and 4) are unknown fields now."
  (dolist (arm '(turnEnded created))
    (should (equal (agent-repl-test-wire-roster--breach
                    #'agent-repl-wire-decode-roster-row-when
                    (format "{\"%s\":{\"atMs\":\"1\"}}" arm))
                   (list "RosterRowWhen" arm "unknown field")))))

(ert-deftest agent-repl-test-wire-roster-when-hidden-carries-its-instant ()
  "A hidden row whose cache lapse the daemon does not know decodes with no
cache."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"hidden\":{\"atMs\":\"1756400000000\"}}")
                 '(:arm :hidden :value (:at-ms 1756400000000 :cache nil)))))

(ert-deftest agent-repl-test-wire-roster-when-hidden-carries-its-cache-lapse ()
  "Regression watch: a hidden row's cache lapse (2026-10-13) must decode, or
the strict decoder rejects the whole WatchWorkspaceRoster push and the tab
bar goes dark."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"hidden\":{\"atMs\":\"1\",\"cache\":{\"lapsesAtMs\":\"2\"}}}")
                 '(:arm :hidden :value (:at-ms 1 :cache (:lapses-at-ms 2))))))

(ert-deftest agent-repl-test-wire-roster-when-hidden-refuses-an-extra-key ()
  "An unknown field on a hidden row is a breach naming its message."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when-hidden
                  "{\"atMs\":\"1\",\"extra\":1}")
                 '("RosterRowWhenHidden" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-when-hidden-cache-refuses-an-extra-key ()
  "An unknown field on a hidden row's cache lapse is a breach naming its
message."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when-hidden-cache
                  "{\"lapsesAtMs\":\"1\",\"extra\":1}")
                 '("RosterRowWhenHiddenCache" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-when-retired-active-is-refused ()
  "The retired `active' arm (field 3, reserved) is an unknown field now."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-when
                  "{\"active\":{\"atMs\":\"1756400000000\"}}")
                 '("RosterRowWhen" active "unknown field"))))

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
                  "{\"noTurnRunning\":{},\"merged\":{\"atMs\":\"2\"}}")
                 '("RosterRowWhen" shown "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-roster-row-without-when-is-a-breach ()
  "The when element itself is a required message."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (concat "{\"workspace\":{\"workspace\":{\"id\":\"w\"}},"
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"},\"current\":{},"
                          "\"detail\":{},\"closed\":{}}"))
                 '("RosterRow" when "required message field is absent"))))

;;;; ---- The detail panel ----

(ert-deftest agent-repl-test-wire-roster-detail-absent-lines-are-nil ()
  "Each detail line is PRESENT OR ABSENT BY MESSAGE PRESENCE."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-detail "{}")
                 '(:branch nil :parent-branch nil :summary nil :commits nil))))

(ert-deftest agent-repl-test-wire-roster-detail-present-lines-decode ()
  "A present line decodes to its own plist, an absent sibling to nil."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-row-detail
                  "{\"branch\":{\"name\":\"proto/wire\"},\"summary\":{\"text\":\"codec\"}}")
                 '(:branch (:name "proto/wire")
                   :parent-branch nil
                   :summary (:text "codec")
                   :commits nil))))

(ert-deftest agent-repl-test-wire-roster-detail-commits-decode ()
  "A present commit count decodes to its plist."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row-detail
                             "{\"commits\":{\"count\":7}}")
                            :commits)
                 '(:count 7))))

(ert-deftest agent-repl-test-wire-roster-detail-commits-zero-is-present ()
  "A zero count, which protojson omits, decodes as a present count of 0."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-row-detail
                             "{\"commits\":{}}")
                            :commits)
                 '(:count 0))))

(ert-deftest agent-repl-test-wire-roster-detail-commits-refuses-a-negative-count ()
  "A negative count breaches the unsigned field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row-detail-commits "{\"count\":-1}")
                 '("RosterRowDetailCommits" count "expected a non-negative integer"))))

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
                          "\"name\":{\"text\":\"n\"},\"ready\":{},\"availability\":{\"available\":{}},\"lastUpdated\":{\"atMs\":\"1\"},\"current\":{},"
                          "\"when\":{\"noTurnRunning\":{}},\"closed\":{}}"))
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
                  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                          "\"rows\":{\"rows\":[]},\"expanded\":{}}"))
                 '(:key (:repository (:id "r1" :dir "/src"))
                   :header (:label (:text "doom") :count (:workspaces 2))
                   :rows (:rows nil)
                   :fold (:arm :expanded :value nil)
                   :divergence (:state (:arm :pending :value nil))))))

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-a-collapsed-fold ()
  "A collapsed section decodes its fold arm as `:collapsed'."
  (should (eq (plist-get
               (plist-get (agent-repl-test-wire-roster--decode
                           #'agent-repl-wire-decode-roster-repo-section
                           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                                   "\"rows\":{\"rows\":[]},\"collapsed\":{}}"))
                          :fold)
               :arm)
              :collapsed)))

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-fold-is-a-breach ()
  "The fold is never unset on a wire section."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section
           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                   "\"rows\":{\"rows\":[]}}"))))

(ert-deftest agent-repl-test-wire-roster-repo-section-with-both-folds-is-a-breach ()
  "Expanded and collapsed at once is a malformed frame."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section
           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                   "\"rows\":{\"rows\":[]},\"expanded\":{},\"collapsed\":{}}"))))

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-key-is-a-breach ()
  "The section's fold and join key is required."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-section
                  "{\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},\"rows\":{\"rows\":[]}}")
                 '("RosterRepoSection" key "required message field is absent"))))

;;;; ---- Repository divergence ----

(defconst agent-repl-test-wire-roster--section-without-divergence
  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
          "\"rows\":{\"rows\":[]},\"expanded\":{}}")
  "A repository section with every field but its divergence.")

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-divergence-is-a-breach ()
  "The divergence is always set on a wire section, pending until measured."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-section
                  agent-repl-test-wire-roster--section-without-divergence)
                 '("RosterRepoSection" divergence "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-repo-divergence-decodes-a-count ()
  "A counted divergence carries the branch, its upstream and both counts."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repo-divergence
                  "{\"counted\":{\"branch\":\"master\",\"upstream\":\"origin/master\",\"ahead\":2,\"behind\":7}}")
                 '(:state (:arm :counted
                           :value (:branch "master" :upstream "origin/master" :ahead 2 :behind 7))))))

(ert-deftest agent-repl-test-wire-roster-repo-divergence-decodes-no-upstream ()
  "A branch tracking nothing decodes its branch under `:no-upstream'."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repo-divergence
                  "{\"noUpstream\":{\"branch\":\"main\"}}")
                 '(:state (:arm :no-upstream :value (:branch "main"))))))

(ert-deftest agent-repl-test-wire-roster-repo-divergence-decodes-a-failure ()
  "A failed measurement decodes its reason."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repo-divergence
                  "{\"failed\":{\"reason\":\"fetching origin failed\"}}")
                 '(:state (:arm :failed :value (:reason "fetching origin failed"))))))

(ert-deftest agent-repl-test-wire-roster-repo-divergence-unset-is-a-breach ()
  "The measurement's arm is never unset."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-divergence "{}")
                 '("RosterRepoDivergence" state "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-repo-divergence-refuses-an-unknown-field ()
  "An unknown key on a divergence is refused at the divergence."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-divergence
                  "{\"pending\":{},\"stale\":true}")
                 '("RosterRepoDivergence" stale "unknown field"))))

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
                          "\"groups\":[],\"collapsed\":{}}"))
                 '(:header (:label (:text "Recently Merged") :count (:workspaces 2)) :groups nil
                   :fold (:arm :collapsed :value nil)))))

(ert-deftest agent-repl-test-wire-roster-merged-section-refuses-a-key ()
  "A key on the merged section is an unknown field, refused."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-merged-section
                  (concat "{\"key\":{},\"header\":{\"label\":{\"text\":\"m\"},\"count\":{\"workspaces\":2}},"
                          "\"groups\":[]}"))
                 '("RosterMergedSection" key "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-label-refuses-an-unknown-field ()
  "An unknown key on a label is refused at the label."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-label "{\"text\":\"a\",\"icon\":\"x\"}")
                 '("RosterLabel" icon "unknown field"))))

;;;; ---- The roster ----

(defconst agent-repl-test-wire-roster--roster-json
  (concat "{\"repository\":{\"sections\":[{"
          "\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
          "\"rows\":{\"rows\":[]},\"expanded\":{}}],\"openCount\":{\"workspaces\":2}},"
          "\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{\"text\":\"Open\"},\"collapsed\":{}},"
          "\"hidden\":{\"header\":{\"label\":{\"text\":\"Hidden\"},\"count\":{\"workspaces\":0}},"
          "\"rows\":{\"rows\":[]},\"expanded\":{}},"
          "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{\"text\":\"Recently Merged\"},\"count\":{\"workspaces\":2}},"
          "\"groups\":[],\"collapsed\":{}},"
          "\"current\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/1\"}},"
          "\"shownRepository\":{}}")
  "A whole roster: every grouping resolved, the open, hidden and merged sections, and current.")

(ert-deftest agent-repl-test-wire-roster-decodes-the-whole-roster ()
  "The roster is always whole, never a delta, and decodes as one picture."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-workspace-roster
                  agent-repl-test-wire-roster--roster-json)
                 '(:repository (:sections ((:key (:repository (:id "r1" :dir "/src"))
                                            :header (:label (:text "doom") :count (:workspaces 2))
                                            :rows (:rows nil)
                                            :fold (:arm :expanded :value nil)
                                            :divergence (:state (:arm :pending :value nil))))
                               :open-count (:workspaces 2))
                   :task (:sections nil :open-count (:workspaces 0))
                   :maturity (:sections nil :open-count (:workspaces 0))
                   :open (:label (:text "Open") :fold (:arm :collapsed :value nil))
                   :hidden (:header (:label (:text "Hidden") :count (:workspaces 0))
                            :rows (:rows nil)
                            :fold (:arm :expanded :value nil))
                   :recently-merged (:header (:label (:text "Recently Merged") :count (:workspaces 2))
                                     :groups nil
                                     :fold (:arm :collapsed :value nil))
                   :tab-bar (:tabs nil)
                   :current (:workspace (:id "ws-1" :dir "/w/1"))
                   :shown (:arm :shown-repository :value nil)
                   :revision 0))))

(ert-deftest agent-repl-test-wire-roster-decodes-the-revision ()
  "The roster's revision decodes from protojson's decimal string."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (concat (substring agent-repl-test-wire-roster--roster-json 0 -1)
                                     ",\"revision\":\"9\"}"))
                            :revision)
                 9)))

(ert-deftest agent-repl-test-wire-roster-decodes-the-tab-bar ()
  "The tab bar is the registry's order, each tab its ref and visibility arm."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-tab-bar
                  (concat "{\"tabs\":["
                          "{\"workspace\":{\"id\":\"b\",\"dir\":\"/w/b\"},\"drawn\":{}},"
                          "{\"workspace\":{\"id\":\"c\",\"dir\":\"/w/c\"},\"withheldCollapsed\":{}},"
                          "{\"workspace\":{\"id\":\"a\",\"dir\":\"/w/a\"},\"withheldHidden\":{}}]}"))
                 '(:tabs ((:workspace (:id "b" :dir "/w/b") :visibility (:arm :drawn :value nil))
                          (:workspace (:id "c" :dir "/w/c") :visibility (:arm :withheld-collapsed :value nil))
                          (:workspace (:id "a" :dir "/w/a") :visibility (:arm :withheld-hidden :value nil)))))))

(ert-deftest agent-repl-test-wire-roster-a-tab-without-visibility-is-a-breach ()
  "A tab must say whether it is drawn; an unset visibility is a breach."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-tab
           "{\"workspace\":{\"id\":\"a\",\"dir\":\"/w/a\"}}")))

(ert-deftest agent-repl-test-wire-roster-without-the-tab-bar-is-a-breach ()
  "The tab bar is always set; a roster without one is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (replace-regexp-in-string "\"tabBar\":{}," "" agent-repl-test-wire-roster--roster-json t t))
                 '("WorkspaceRoster" tabBar "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-absent-current-is-nil ()
  "UNSET current means there is no selected workspace."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},"
                                     "\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                                     "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}},"
                                     "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},"
                                     "\"groups\":[],\"collapsed\":{}},\"shownRepository\":{}}"))
                            :current)
                 nil)))

(ert-deftest agent-repl-test-wire-roster-without-the-repository-view-is-a-breach ()
  "BOTH groupings arrive fully resolved; an absent one is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}},"
                          "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]}}"))
                 '("WorkspaceRoster" repository "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-the-task-view-is-a-breach ()
  "The task grouping is resolved alongside the repository grouping."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},"
                          "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]}}"))
                 '("WorkspaceRoster" task "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-the-maturity-view-is-a-breach ()
  "The maturity grouping is resolved alongside the other two."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"task\":{\"sections\":[],\"openCount\":{}},"
                          "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]}}"))
                 '("WorkspaceRoster" maturity "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-recently-merged-is-a-breach ()
  "The merged section is hoisted onto every roster, always present."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}}}"))
                 '("WorkspaceRoster" recentlyMerged "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-the-hidden-section-is-a-breach ()
  "The hidden section is on every roster, empty when nothing is hidden."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[],\"collapsed\":{}}}"))
                 '("WorkspaceRoster" hidden "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-decodes-a-collapsed-hidden-section ()
  "The hidden section decodes its header, rows and fold."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-hidden-section
                  (concat "{\"header\":{\"label\":{\"text\":\"Hidden Workspaces\"},\"count\":{\"workspaces\":1}},"
                          "\"rows\":{\"rows\":[]},\"collapsed\":{}}"))
                 '(:header (:label (:text "Hidden Workspaces") :count (:workspaces 1))
                   :rows (:rows nil)
                   :fold (:arm :collapsed :value nil)))))

(ert-deftest agent-repl-test-wire-roster-a-hidden-section-without-a-fold-is-a-breach ()
  "The hidden section's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-hidden-section
                  "{\"header\":{\"label\":{},\"count\":{}},\"rows\":{}}")
                 '("RosterHiddenSection" fold "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-a-hidden-section-refuses-a-key ()
  "The hidden section has no key of its own; one is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-hidden-section
                  "{\"key\":{},\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}}")
                 '("RosterHiddenSection" key "unknown field"))))

;;;; ---- The keyless sections: one decoder shape ----

(ert-deftest agent-repl-test-wire-roster-keyless-section-decodes-through-its-hooks ()
  "The keyless-section decoder hands each field to its hook and names the message."
  (should (equal (agent-repl-test-wire-roster--decode
                  (lambda (value)
                    (agent-repl-wire--decode-keyless-section
                     "TestBand" value
                     (lambda (_) 'header) (lambda (_) 'rows) (lambda (_) 'fold)))
                  "{\"header\":{},\"rows\":{},\"expanded\":{}}")
                 '(:header header :rows rows :fold fold))))

(ert-deftest agent-repl-test-wire-roster-keyless-section-refuses-a-key ()
  "A key on a keyless section is an unknown field of that message."
  (should (equal (agent-repl-test-wire-roster--breach
                  (lambda (value)
                    (agent-repl-wire--decode-keyless-section
                     "TestBand" value #'identity #'identity #'identity))
                  "{\"key\":{},\"header\":{},\"rows\":{},\"expanded\":{}}")
                 '("TestBand" key "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-keyless-section-requires-its-header ()
  "An absent header is a breach of the named message."
  (should (equal (agent-repl-test-wire-roster--breach
                  (lambda (value)
                    (agent-repl-wire--decode-keyless-section
                     "TestBand" value #'identity #'identity #'identity))
                  "{\"rows\":{},\"expanded\":{}}")
                 '("TestBand" header "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-every-keyless-section-shares-one-decoder ()
  "The hidden section decodes through the one keyless shape.
The merged band carries groups rather than rows, so it has its own decoder.
A hand-rolled hidden-section decoder would skip the shared helper and fail here."
  (dolist (decoder '(agent-repl-wire-decode-roster-hidden-section))
    (let ((calls 0))
      (cl-letf* ((shared (symbol-function 'agent-repl-wire--decode-keyless-section))
                 ((symbol-function 'agent-repl-wire--decode-keyless-section)
                  (lambda (&rest args) (cl-incf calls) (apply shared args))))
        (agent-repl-test-wire-roster--decode
         decoder "{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}}"))
      (should (= calls 1)))))

(ert-deftest agent-repl-test-wire-roster-refuses-an-unknown-field ()
  "An unknown key on the roster is refused at the roster."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}},"
                          "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]},"
                          "\"hibernating\":{}}"))
                 '("WorkspaceRoster" hibernating "unknown field"))))

;;;; ---- The view state: folds and the grouping shown ----

(defconst agent-repl-test-wire-roster--hidden-json
  "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}}"
  "An empty hidden section, expanded, as a JSON member.")

(defconst agent-repl-test-wire-roster--merged-json
  "\"tabBar\":{},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[],\"collapsed\":{}}"
  "A merged band, collapsed, as a JSON member.")

(defun agent-repl-test-wire-roster--with-shown (shown)
  "A roster JSON whose `shown' members are SHOWN (a JSON fragment, maybe empty)."
  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"task\":{\"sections\":[],\"openCount\":{}},\"maturity\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
          agent-repl-test-wire-roster--hidden-json ","
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

(ert-deftest agent-repl-test-wire-roster-decodes-the-maturity-grouping-shown ()
  "The maturity grouping is a grouping the daemon can show."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (agent-repl-test-wire-roster--with-shown "\"shownMaturity\":{}"))
                            :shown)
                 '(:arm :shown-maturity :value nil))))

(defconst agent-repl-test-wire-roster--maturity-section-json
  (concat "{\"key\":{\"stage\":{\"inProgress\":{}}},"
          "\"header\":{\"label\":{\"text\":\"In Progress\"},\"count\":{\"workspaces\":1}},"
          "\"rows\":{},\"collapsed\":{}}")
  "A collapsed in-progress maturity section with no rows.")

(ert-deftest agent-repl-test-wire-roster-decodes-a-maturity-section ()
  "A maturity section decodes its stage key, header, rows and fold."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-maturity-section
                  agent-repl-test-wire-roster--maturity-section-json)
                 '(:key (:stage (:stage (:arm :in-progress :value nil)))
                   :header (:label (:text "In Progress") :count (:workspaces 1))
                   :rows (:rows nil)
                   :fold (:arm :collapsed :value nil)))))

(ert-deftest agent-repl-test-wire-roster-a-maturity-section-without-a-fold-is-a-breach ()
  "A maturity section's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-maturity-section
                  (concat "{\"key\":{\"stage\":{\"backlog\":{}}},"
                          "\"header\":{\"label\":{},\"count\":{}},\"rows\":{}}"))
                 '("RosterMaturitySection" fold "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-decodes-the-maturity-view-in-order ()
  "The maturity view keeps the resolver's stage order."
  (should (equal (mapcar (lambda (section)
                           (plist-get (plist-get (plist-get section :key) :stage) :stage))
                         (plist-get (agent-repl-test-wire-roster--decode
                                     #'agent-repl-wire-decode-roster-maturity-view
                                     (concat "{\"sections\":["
                                             "{\"key\":{\"stage\":{\"backlog\":{}}},\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}},"
                                             "{\"key\":{\"stage\":{\"merging\":{}}},\"header\":{\"label\":{},\"count\":{}},\"rows\":{},\"expanded\":{}}"
                                             "],\"openCount\":{}}"))
                                    :sections))
                 '((:arm :backlog :value nil) (:arm :merging :value nil)))))

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
                             "{\"header\":{\"label\":{},\"count\":{}},\"groups\":[],\"expanded\":{}}")
                            :fold)
                 '(:arm :expanded :value nil))))

(ert-deftest agent-repl-test-wire-roster-a-merged-band-without-a-fold-is-a-breach ()
  "The merged band's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-merged-section
                  "{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]}")
                 '("RosterMergedSection" fold "oneof is unset"))))

;;;; ---- The recently-merged band's repository groups ----

(defconst agent-repl-test-wire-roster--merged-group-json
  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/r1\"}},"
          "\"header\":{\"label\":{\"text\":\"alpha\"}},"
          "\"rows\":{\"rows\":[]},%s}")
  "A `RosterMergedRepoGroup' with its `drawn' arm left as a %s hole.")

(ert-deftest agent-repl-test-wire-roster-decodes-a-shown-merged-group ()
  "A group carries its repository key, its label header, its rows and its arm."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-merged-repo-group
                  (format agent-repl-test-wire-roster--merged-group-json "\"shown\":{}"))
                 '(:key (:repository (:id "r1" :dir "/r1"))
                   :header (:label (:text "alpha"))
                   :rows (:rows nil)
                   :drawn (:arm :shown :value nil)))))

(ert-deftest agent-repl-test-wire-roster-decodes-a-collapsed-repositorys-merged-group ()
  "A collapsed repository's group decodes onto its own arm."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-merged-repo-group
                             (format agent-repl-test-wire-roster--merged-group-json
                                     "\"repositoryCollapsed\":{}"))
                            :drawn)
                 '(:arm :repository-collapsed :value nil))))

(ert-deftest agent-repl-test-wire-roster-a-merged-group-without-its-arm-is-a-breach ()
  "A group's `drawn' arm is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-merged-repo-group
                  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/r1\"}},"
                          "\"header\":{\"label\":{\"text\":\"alpha\"}},\"rows\":{\"rows\":[]}}"))
                 '("RosterMergedRepoGroup" drawn "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-a-merged-group-header-refuses-a-fold ()
  "A group's header has no fold of its own: a fold on it is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-merged-repo-group-header
                  "{\"label\":{\"text\":\"alpha\"},\"collapsed\":{}}")
                 '("RosterMergedRepoGroupHeader" collapsed "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-merged-section-decodes-its-groups-in-order ()
  "The band's groups decode in wire order, each through the group decoder."
  (let ((decoded (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-merged-section
                  (concat "{\"header\":{\"label\":{},\"count\":{}},\"groups\":["
                          (format agent-repl-test-wire-roster--merged-group-json "\"shown\":{}") ","
                          (replace-regexp-in-string
                           "alpha" "beta"
                           (format agent-repl-test-wire-roster--merged-group-json "\"repositoryCollapsed\":{}"))
                          "],\"expanded\":{}}"))))
    (should (equal (mapcar (lambda (g) (list (plist-get (plist-get (plist-get g :header) :label) :text)
                                             (plist-get (plist-get g :drawn) :arm)))
                           (plist-get decoded :groups))
                   '(("alpha" :shown) ("beta" :repository-collapsed))))))

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
                          "\"when\":{\"noTurnRunning\":{}},\"detail\":{},\"closed\":{},\"lastUpdated\":{\"atMs\":\"1\"}}"))
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

;;;; ---- The grouping views' shared shape ----

(ert-deftest agent-repl-test-wire-roster-grouping-view-decodes-its-sections-in-order ()
  "The shared grouping-view decoder keeps the resolver's section order."
  (should (equal (agent-repl-test-wire-roster--quiet
                   (agent-repl-wire--decode-grouping-view
                    "RosterTaskView"
                    (agent-repl-test-wire-roster--parse "{\"sections\":[1,2],\"openCount\":{\"workspaces\":2}}")
                    (lambda (n) (* 10 n))))
                 '(:sections (10 20) :open-count (:workspaces 2)))))

(ert-deftest agent-repl-test-wire-roster-grouping-view-refuses-an-unknown-field-under-its-name ()
  "The shared grouping-view decoder names the message it decodes in a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  (lambda (value)
                    (agent-repl-wire--decode-grouping-view "RosterTaskView" value #'identity))
                  "{\"sections\":[],\"extra\":{}}")
                 '("RosterTaskView" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-every-grouping-view-shares-the-grouping-shape ()
  "Every grouping view decodes through `agent-repl-wire--decode-grouping-view'."
  (dolist (case '((agent-repl-wire-decode-roster-repository-view . "RosterRepositoryView")
                  (agent-repl-wire-decode-roster-task-view . "RosterTaskView")
                  (agent-repl-wire-decode-roster-maturity-view . "RosterMaturityView")))
    (let (seen)
      (cl-letf* ((real (symbol-function 'agent-repl-wire--decode-grouping-view))
                 ((symbol-function 'agent-repl-wire--decode-grouping-view)
                  (lambda (name value sections)
                    (push name seen)
                    (funcall real name value sections))))
        (agent-repl-test-wire-roster--decode (car case) "{\"sections\":[],\"openCount\":{}}"))
      (should (equal seen (list (cdr case)))))))

;;;; ---- The open-workspaces section ----

(ert-deftest agent-repl-test-wire-roster-without-the-open-section-is-a-breach ()
  "The open section is on every roster."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (replace-regexp-in-string
                   "\"open\":{\"label\":{\"text\":\"Open\"},\"collapsed\":{}}," ""
                   agent-repl-test-wire-roster--roster-json t t))
                 '("WorkspaceRoster" open "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-decodes-an-expanded-open-section ()
  "The open section decodes its label and its fold."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-open-section
                  "{\"label\":{\"text\":\"Open\"},\"expanded\":{}}")
                 '(:label (:text "Open") :fold (:arm :expanded :value nil)))))

(ert-deftest agent-repl-test-wire-roster-an-open-section-without-a-fold-is-a-breach ()
  "The open section's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-open-section
                  "{\"label\":{\"text\":\"Open\"}}")
                 '("RosterOpenSection" fold "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-an-open-section-refuses-an-unknown-field ()
  "An unknown key on the open section is refused at the section."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-open-section
                  "{\"label\":{},\"expanded\":{},\"header\":{}}")
                 '("RosterOpenSection" header "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-a-grouping-view-without-its-open-count-is-a-breach ()
  "Every grouping view carries the open section's count."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-task-view "{\"sections\":[]}")
                 '("RosterTaskView" openCount "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-an-open-count-refuses-an-unknown-field ()
  "An unknown key on the open count is refused at the count."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-open-count "{\"workspaces\":1,\"rows\":2}")
                 '("RosterOpenCount" rows "unknown field"))))
