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
                   :idle-async-unread nil
                   :reviving nil
                   :detached-live nil
                   :one-shot nil
                   :opening nil
                   :async-merging nil
                   :merge-paused nil
                   :last-selected nil
                   :last-turn-ended nil
                   :closure nil
                   :last-updated (:at-ms 1)
                   :availability (:arm :available :value nil)
                   :name (:text "fix-flaky")
                   :status (:arm :ready :value nil)
                   :current (:current nil)
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

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-idle-async-unread-marker ()
  "A row carrying `idleAsyncUnread\=' decodes it as the turn end\='s arm keyword."
  (dolist (case '(("done" . :done) ("interrupted" . :interrupted) ("turnFailed" . :turn-failed)))
    ;; Act
    (let ((row (agent-repl-test-wire-roster--decode
                #'agent-repl-wire-decode-roster-row
                (agent-repl-test-wire-roster--row
                 "\"idleAsync\":{}"
                 (format "\"idleAsyncUnread\":{\"%s\":{}}" (car case))))))
      ;; Assert
      (should (eq (plist-get row :idle-async-unread) (cdr case))))))

(ert-deftest agent-repl-test-wire-roster-row-idle-async-unread-marker-names-a-turn-end ()
  "`RosterRowIdleAsyncUnread\=' with no turn end set is a contract breach."
  ;; Act / Assert
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"idleAsync\":{}" "\"idleAsyncUnread\":{}"))
                 '("RosterRowIdleAsyncUnread" turn_end "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-row-idle-async-unread-marker-only-on-idle-async ()
  "The unread-turn-end marker on any arm but `idle_async\=' is a contract breach."
  ;; Act
  (let ((breach (agent-repl-test-wire-roster--breach
                 #'agent-repl-wire-decode-roster-row
                 (agent-repl-test-wire-roster--row "\"done\":{}" "\"idleAsyncUnread\":{\"done\":{}}"))))
    ;; Assert
    (should (equal (seq-take breach 2) '("RosterRow" idle_async_unread)))))

(ert-deftest agent-repl-test-wire-roster-row-decodes-the-merge-paused-marker ()
  "A row carrying `mergePaused\=' decodes it as PRESENT, beside its status arm."
  ;; Act
  (let ((row (agent-repl-test-wire-roster--decode
              #'agent-repl-wire-decode-roster-row
              (agent-repl-test-wire-roster--row "\"merging\":{}" "\"mergePaused\":{}"))))
    ;; Assert
    (should (eq (plist-get row :merge-paused) t))
    (should (equal (plist-get row :status) '(:arm :merging :value nil)))))

(ert-deftest agent-repl-test-wire-roster-row-merge-paused-marker-carries-no-payload ()
  "`RosterRowMergePaused\=' is EMPTY: a field inside it is an unknown field."
  ;; Act / Assert
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row
                   "\"merging\":{}" "\"mergePaused\":{\"repo\":{}}"))
                 '("RosterRowMergePaused" repo "unknown field"))))

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

;;;; ---- The lists' rows ----

(defun agent-repl-test-wire-roster--open-row (tab &rest children)
  "Return a `RosterOpenRow' JSON with TAB (a JSON member) and CHILDREN."
  (format "{\"row\":%s,\"tab\":{%s},\"children\":[%s]}"
          (agent-repl-test-wire-roster--row "\"ready\":{}") tab
          (mapconcat #'identity children ",")))

(ert-deftest agent-repl-test-wire-roster-a-row-carries-no-family ()
  "The shared row holds no family: each list nests its own row type."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-row
                  (agent-repl-test-wire-roster--row "\"ready\":{}" "\"children\":[]"))
                 '("RosterRow" children "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-open-row-decodes-its-numbered-tab ()
  "An open row carries its row, its tab and its family."
  (let ((decoded (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-open-row
                  (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":3}"))))
    (should (equal (list (plist-get (plist-get (plist-get decoded :row) :status) :arm)
                         (plist-get decoded :tab)
                         (plist-get decoded :children))
                   '(:ready (:tab (:arm :numbered :value (:number 3))) nil)))))

(ert-deftest agent-repl-test-wire-roster-open-row-decodes-a-folded-tab ()
  "A folded tab decodes onto its own arm."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-open-row
                             (agent-repl-test-wire-roster--open-row "\"folded\":{}"))
                            :tab)
                 '(:tab (:arm :folded :value nil)))))

(ert-deftest agent-repl-test-wire-roster-an-open-row-without-a-tab-is-a-breach ()
  "Every open row has a tab; an unset tab is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-open-row
                  (agent-repl-test-wire-roster--open-row ""))
                 '("RosterOpenRowTab" tab "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-open-row-children-decode-recursively ()
  "An open family decodes as open rows, to any depth, in render order."
  (let* ((grandchild (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":3}"))
         (child (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":2}" grandchild))
         (decoded (agent-repl-test-wire-roster--decode
                   #'agent-repl-wire-decode-roster-open-row
                   (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":1}" child))))
    (should (equal (plist-get (car (plist-get (car (plist-get decoded :children)) :children)) :tab)
                   '(:tab (:arm :numbered :value (:number 3)))))))

(ert-deftest agent-repl-test-wire-roster-hidden-row-decodes-its-family ()
  "A hidden row nests its hidden family."
  (let ((decoded (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-hidden-row
                  (format "{\"row\":%s,\"children\":[{\"row\":%s}]}"
                          (agent-repl-test-wire-roster--row "\"ready\":{}")
                          (agent-repl-test-wire-roster--row "\"done\":{}")))))
    (should (equal (plist-get (plist-get (plist-get (car (plist-get decoded :children)) :row) :status) :arm)
                   :done))))

(ert-deftest agent-repl-test-wire-roster-merged-and-closed-rows-nest-nothing ()
  "A merged or a closed row stands alone: a family on it is an unknown field."
  (dolist (spec '((agent-repl-wire-decode-roster-merged-row "RosterMergedRow")
                  (agent-repl-wire-decode-roster-closed-row "RosterClosedRow")))
    (should (equal (agent-repl-test-wire-roster--breach
                    (car spec)
                    (format "{\"row\":%s,\"children\":[]}" (agent-repl-test-wire-roster--row "\"merged\":{}")))
                   (list (cadr spec) 'children "unknown field")))))

(ert-deftest agent-repl-test-wire-roster-every-list-decodes-its-rows-in-order ()
  "Each list's rows decode in the resolver's order through the one list decoder."
  (dolist (spec (list (list #'agent-repl-wire-decode-roster-open-rows
                            (agent-repl-test-wire-roster--open-row "\"folded\":{}"))
                      (list #'agent-repl-wire-decode-roster-hidden-rows
                            (format "{\"row\":%s}" (agent-repl-test-wire-roster--row "\"ready\":{}")))
                      (list #'agent-repl-wire-decode-roster-merged-rows
                            (format "{\"row\":%s}" (agent-repl-test-wire-roster--row "\"ready\":{}")))
                      (list #'agent-repl-wire-decode-roster-closed-rows
                            (format "{\"row\":%s}" (agent-repl-test-wire-roster--row "\"ready\":{}")))))
    (let ((rows (plist-get (agent-repl-test-wire-roster--decode
                            (car spec) (format "{\"rows\":[%s,%s]}" (cadr spec) (cadr spec)))
                           :rows)))
      (should (= (length rows) 2)))
    (should (equal (agent-repl-test-wire-roster--decode (car spec) "{\"rows\":[]}") '(:rows nil)))))

(ert-deftest agent-repl-test-wire-roster-every-list-shares-one-decoder ()
  "Every list's rows decode through `agent-repl-wire--decode-row-list'.
A hand-rolled list decoder would skip the shared helper and fail here."
  (dolist (decoder '(agent-repl-wire-decode-roster-open-rows agent-repl-wire-decode-roster-hidden-rows
                     agent-repl-wire-decode-roster-merged-rows agent-repl-wire-decode-roster-closed-rows))
    (let ((calls 0))
      (cl-letf* ((shared (symbol-function 'agent-repl-wire--decode-row-list))
                 ((symbol-function 'agent-repl-wire--decode-row-list)
                  (lambda (&rest args) (cl-incf calls) (apply shared args))))
        (agent-repl-test-wire-roster--decode decoder "{\"rows\":[]}"))
      (should (= calls 1)))))

;;;; ---- Sections ----

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-key-header-rows ()
  "A repo section carries its stable key, its header, its rows and its fold."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repo-section
                  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                          "\"expanded\":{\"collapsible\":{}}}"))
                 '(:key (:repository (:id "r1" :dir "/src"))
                   :header (:label (:text "doom") :count (:workspaces 2))
                   :fold (:arm :expanded :value (:collapse (:arm :collapsible :value nil)))
                   :divergence (:state (:arm :pending :value nil))
                   :entries nil))))

(defconst agent-repl-test-wire-roster--subsection
  (concat "{\"key\":{\"subsectionId\":\"s1\"},"
          "\"header\":{\"label\":{\"text\":\"later\"},\"count\":{\"workspaces\":0}},"
          "\"openRows\":{\"rows\":[]},\"expanded\":{\"collapsible\":{}}}")
  "One empty subsection named later, expanded.")

(defun agent-repl-test-wire-roster--section-with-entries (&rest entries)
  "Return a repository section JSON whose body is ENTRIES, each a JSON object."
  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":0}},"
          "\"expanded\":{\"collapsible\":{}},"
          (format "\"entries\":[%s]}" (mapconcat #'identity entries ","))))

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-its-entries-in-order ()
  "A section's entries decode in drawn order, each on its own arm."
  (let ((entries (plist-get
                  (agent-repl-test-wire-roster--decode
                   #'agent-repl-wire-decode-roster-repo-section
                   (agent-repl-test-wire-roster--section-with-entries
                    (format "{\"row\":%s}" (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":1}"))
                    (format "{\"subsection\":%s}" agent-repl-test-wire-roster--subsection)
                    (format "{\"row\":%s}" (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":2}"))))
                  :entries)))
    (should (equal (mapcar (lambda (entry) (plist-get (plist-get entry :entry) :arm)) entries)
                   '(:row :subsection :row)))))

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-a-subsection-entry ()
  "A subsection entry carries the subsection's key, header, rows and fold."
  (should (equal (plist-get
                  (car (plist-get
                        (agent-repl-test-wire-roster--decode
                         #'agent-repl-wire-decode-roster-repo-section
                         (agent-repl-test-wire-roster--section-with-entries
                          (format "{\"subsection\":%s}" agent-repl-test-wire-roster--subsection)))
                        :entries))
                  :entry)
                 '(:arm :subsection
                   :value (:key (:subsection-id "s1")
                           :header (:label (:text "later") :count (:workspaces 0))
                           :open-rows (:rows nil)
                           :fold (:arm :expanded :value (:collapse (:arm :collapsible :value nil))))))))

(ert-deftest agent-repl-test-wire-roster-a-repo-entry-with-no-arm-is-a-breach ()
  "Every entry stands for something; an unset arm is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-entry "{}")
                 '("RosterRepoEntry" entry "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-a-repo-entry-with-two-arms-is-a-breach ()
  "An entry is a row or a subsection, never both."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-entry
                  (format "{\"row\":%s,\"subsection\":%s}"
                          (agent-repl-test-wire-roster--open-row "\"numbered\":{\"number\":1}")
                          agent-repl-test-wire-roster--subsection))
                 '("RosterRepoEntry" entry "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-roster-a-repo-section-with-retired-open-rows-is-a-breach ()
  "The retired `openRows' body is refused as an unknown field, never read."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-section
                  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":0}},"
                          "\"openRows\":{\"rows\":[]},\"expanded\":{\"collapsible\":{}}}"))
                 '("RosterRepoSection" openRows "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-subsection-decodes-a-collapsed-fold ()
  "A collapsed subsection decodes its fold arm as `:collapsed'."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-roster-subsection
                             (replace-regexp-in-string "\"expanded\":{\"collapsible\":{}}" "\"collapsed\":{}" agent-repl-test-wire-roster--subsection t t))
                            :fold)
                 '(:arm :collapsed :value nil))))

(ert-deftest agent-repl-test-wire-roster-subsection-without-a-fold-is-a-breach ()
  "A subsection's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-subsection
                  (replace-regexp-in-string ",\"expanded\":{\"collapsible\":{}}" "" agent-repl-test-wire-roster--subsection t t))
                 '("RosterSubsection" fold "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-subsection-without-a-key-is-a-breach ()
  "A subsection's identity is required."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-subsection
                  "{\"header\":{\"label\":{\"text\":\"later\"},\"count\":{\"workspaces\":0}},\"openRows\":{\"rows\":[]}}")
                 '("RosterSubsection" key "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-subsection-with-an-unknown-field-is-a-breach ()
  "The codec refuses a field the contract does not spell."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-subsection
           "{\"key\":{\"subsectionId\":\"s1\"},\"header\":{\"label\":{\"text\":\"x\"},\"count\":{\"workspaces\":0}},\"openRows\":{\"rows\":[]},\"fold\":{}}")))

(ert-deftest agent-repl-test-wire-roster-repo-section-decodes-a-collapsed-fold ()
  "A collapsed section decodes its fold arm as `:collapsed'."
  (should (eq (plist-get
               (plist-get (agent-repl-test-wire-roster--decode
                           #'agent-repl-wire-decode-roster-repo-section
                           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                                   "\"entries\":[],\"collapsed\":{}}"))
                          :fold)
               :arm)
              :collapsed)))

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-fold-is-a-breach ()
  "The fold is never unset on a wire section."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section
           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                   "\"entries\":[]}"))))

(ert-deftest agent-repl-test-wire-roster-repo-section-with-both-folds-is-a-breach ()
  "Expanded and collapsed at once is a malformed frame."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section
           (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},\"divergence\":{\"pending\":{}},"
                   "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
                   "\"entries\":[],\"expanded\":{\"collapsible\":{}},\"collapsed\":{}}"))))

(ert-deftest agent-repl-test-wire-roster-repo-section-without-a-key-is-a-breach ()
  "The section's fold and join key is required."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-section
                  "{\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},\"entries\":[]}")
                 '("RosterRepoSection" key "required message field is absent"))))

;;;; ---- Whether an expanded section may collapse ----

(ert-deftest agent-repl-test-wire-roster-repo-expanded-decodes-holds-current ()
  "An expanded section holding the current workspace decodes `:holds-current'."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repo-section-expanded
                  "{\"holdsCurrent\":{}}")
                 '(:collapse (:arm :holds-current :value nil)))))

(ert-deftest agent-repl-test-wire-roster-repo-expanded-without-a-collapse-is-a-breach ()
  "An expanded section always says whether it may collapse."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repo-section-expanded "{}")
                 '("RosterRepoSectionExpanded" collapse "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-repo-expanded-with-both-arms-is-a-breach ()
  "Collapsible and holding the current workspace at once is a malformed frame."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-repo-section-expanded
           "{\"collapsible\":{},\"holdsCurrent\":{}}")))

(ert-deftest agent-repl-test-wire-roster-subsection-expanded-decodes-holds-current ()
  "An expanded subsection holding the current workspace decodes `:holds-current'."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-subsection-expanded
                  "{\"holdsCurrent\":{}}")
                 '(:collapse (:arm :holds-current :value nil)))))

(ert-deftest agent-repl-test-wire-roster-subsection-expanded-without-a-collapse-is-a-breach ()
  "An expanded subsection always says whether it may collapse."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-subsection-expanded "{}")
                 '("RosterSubsectionExpanded" collapse "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-subsection-expanded-with-an-unknown-field-is-a-breach ()
  "The codec refuses a field the contract does not spell."
  (should (agent-repl-test-wire-roster--breach
           #'agent-repl-wire-decode-roster-subsection-expanded
           "{\"collapsible\":{},\"pinned\":{}}")))

;;;; ---- The shared expanded-fold decoder ----

(ert-deftest agent-repl-test-wire-roster-expanded-collapse-hands-each-arm-its-decoder ()
  "Each set arm is decoded by the decoder handed for it, under its keyword."
  (should (equal (list (agent-repl-test-wire-roster--decode
                        (lambda (v) (agent-repl-wire--decode-expanded-collapse
                                     "Probe" v (lambda (_) 'c) (lambda (_) 'h)))
                        "{\"collapsible\":{}}")
                       (agent-repl-test-wire-roster--decode
                        (lambda (v) (agent-repl-wire--decode-expanded-collapse
                                     "Probe" v (lambda (_) 'c) (lambda (_) 'h)))
                        "{\"holdsCurrent\":{}}"))
                 '((:collapse (:arm :collapsible :value c))
                   (:collapse (:arm :holds-current :value h))))))

(ert-deftest agent-repl-test-wire-roster-expanded-collapse-names-its-message-in-a-breach ()
  "A breach names the message the helper was handed."
  (should (equal (agent-repl-test-wire-roster--breach
                  (lambda (v) (agent-repl-wire--decode-expanded-collapse "Probe" v #'ignore #'ignore))
                  "{}")
                 '("Probe" collapse "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-every-expanded-fold-decodes-through-the-helper ()
  "Both expanded folds share the one decoder, so neither can drift."
  (let (names)
    (cl-letf* ((shared (symbol-function 'agent-repl-wire--decode-expanded-collapse))
               ((symbol-function 'agent-repl-wire--decode-expanded-collapse)
                (lambda (name &rest args) (push name names) (apply shared name args))))
      (agent-repl-test-wire-roster--decode #'agent-repl-wire-decode-roster-repo-section-expanded "{\"collapsible\":{}}")
      (agent-repl-test-wire-roster--decode #'agent-repl-wire-decode-roster-subsection-expanded "{\"collapsible\":{}}"))
    (should (equal (nreverse names) '("RosterRepoSectionExpanded" "RosterSubsectionExpanded")))))

;;;; ---- Repository divergence ----

(defconst agent-repl-test-wire-roster--section-without-divergence
  (concat "{\"key\":{\"repository\":{\"id\":\"r1\",\"dir\":\"/src\"}},"
          "\"header\":{\"label\":{\"text\":\"doom\"},\"count\":{\"workspaces\":2}},"
          "\"expanded\":{\"collapsible\":{}}}")
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
          "\"expanded\":{\"collapsible\":{}}}],\"openCount\":{\"workspaces\":2}},"
          "\"open\":{\"label\":{\"text\":\"Open\"},\"collapsed\":{}},"
          "\"hidden\":{\"header\":{\"label\":{\"text\":\"Hidden\"},\"count\":{\"workspaces\":0}},"
          "\"hiddenRows\":{\"rows\":[]},\"expanded\":{}},"
          "\"closed\":{\"rows\":[]},\"recentlyMerged\":{\"header\":{\"label\":{\"text\":\"Recently Merged\"},\"count\":{\"workspaces\":2}},"
          "\"groups\":[],\"collapsed\":{}},"
          "\"current\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/1\"}}}")
  "A whole roster: the repository sections, the open, hidden and merged sections, and current.")

(ert-deftest agent-repl-test-wire-roster-decodes-the-whole-roster ()
  "The roster is always whole, never a delta, and decodes as one picture."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-workspace-roster
                  agent-repl-test-wire-roster--roster-json)
                 '(:repository (:sections ((:key (:repository (:id "r1" :dir "/src"))
                                            :header (:label (:text "doom") :count (:workspaces 2))
                                            :fold (:arm :expanded :value (:collapse (:arm :collapsible :value nil)))
                                            :divergence (:state (:arm :pending :value nil))
                                            :entries nil))
                               :open-count (:workspaces 2))
                   :open (:label (:text "Open") :fold (:arm :collapsed :value nil))
                   :hidden (:header (:label (:text "Hidden") :count (:workspaces 0))
                            :hidden-rows (:rows nil)
                            :fold (:arm :expanded :value nil))
                   :recently-merged (:header (:label (:text "Recently Merged") :count (:workspaces 2))
                                     :groups nil
                                     :fold (:arm :collapsed :value nil))
                   :closed (:rows nil)
                   :current (:workspace (:id "ws-1" :dir "/w/1"))
                   :revision 0))))

(ert-deftest agent-repl-test-wire-roster-decodes-the-revision ()
  "The roster's revision decodes from protojson's decimal string."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (concat (substring agent-repl-test-wire-roster--roster-json 0 -1)
                                     ",\"revision\":\"9\"}"))
                            :revision)
                 9)))

(ert-deftest agent-repl-test-wire-roster-without-the-closed-list-is-a-breach ()
  "The closed list is always set; a roster without one is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (replace-regexp-in-string "\"closed\":{\"rows\":\\[\\]}," "" agent-repl-test-wire-roster--roster-json t))
                 '("WorkspaceRoster" closed "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-a-roster-refuses-a-tab-bar ()
  "There is no tab bar message: the Open section's rows are the tab bar."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat (substring agent-repl-test-wire-roster--roster-json 0 -1) ",\"tabBar\":{}}"))
                 '("WorkspaceRoster" tabBar "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-absent-current-is-nil ()
  "UNSET current means there is no selected workspace."
  (should (equal (plist-get (agent-repl-test-wire-roster--decode
                             #'agent-repl-wire-decode-workspace-roster
                             (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},"
                                     "\"open\":{\"label\":{},\"expanded\":{}},"
                                     "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{},\"expanded\":{}},"
                                     "\"closed\":{\"rows\":[]},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},"
                                     "\"groups\":[],\"collapsed\":{}}}"))
                            :current)
                 nil)))

(ert-deftest agent-repl-test-wire-roster-without-the-repository-view-is-a-breach ()
  "BOTH groupings arrive fully resolved; an absent one is a breach."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{},\"expanded\":{}},"
                          "\"closed\":{\"rows\":[]},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]}}"))
                 '("WorkspaceRoster" repository "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-recently-merged-is-a-breach ()
  "The merged section is hoisted onto every roster, always present."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{},\"expanded\":{}}}"))
                 '("WorkspaceRoster" recentlyMerged "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-without-the-hidden-section-is-a-breach ()
  "The hidden section is on every roster, empty when nothing is hidden."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"closed\":{\"rows\":[]},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[],\"collapsed\":{}}}"))
                 '("WorkspaceRoster" hidden "required message field is absent"))))

(ert-deftest agent-repl-test-wire-roster-decodes-a-collapsed-hidden-section ()
  "The hidden section decodes its header, rows and fold."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-hidden-section
                  (concat "{\"header\":{\"label\":{\"text\":\"Hidden Workspaces\"},\"count\":{\"workspaces\":1}},"
                          "\"hiddenRows\":{\"rows\":[]},\"collapsed\":{}}"))
                 '(:header (:label (:text "Hidden Workspaces") :count (:workspaces 1))
                   :hidden-rows (:rows nil)
                   :fold (:arm :collapsed :value nil)))))

(ert-deftest agent-repl-test-wire-roster-a-hidden-section-without-a-fold-is-a-breach ()
  "The hidden section's fold is never unset on the wire."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-hidden-section
                  "{\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{}}")
                 '("RosterHiddenSection" fold "oneof is unset"))))

(ert-deftest agent-repl-test-wire-roster-a-hidden-section-refuses-a-key ()
  "The hidden section has no key of its own; one is an unknown field."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-hidden-section
                  "{\"key\":{},\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{},\"expanded\":{}}")
                 '("RosterHiddenSection" key "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-refuses-an-unknown-field ()
  "An unknown key on the roster is refused at the roster."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-workspace-roster
                  (concat "{\"repository\":{\"sections\":[],\"openCount\":{}},\"open\":{\"label\":{},\"expanded\":{}},"
                          "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{},\"expanded\":{}},"
                          "\"closed\":{\"rows\":[]},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[]},"
                          "\"hibernating\":{}}"))
                 '("WorkspaceRoster" hibernating "unknown field"))))

;;;; ---- The view state: folds and the grouping shown ----

(defconst agent-repl-test-wire-roster--hidden-json
  "\"hidden\":{\"header\":{\"label\":{},\"count\":{}},\"hiddenRows\":{},\"expanded\":{}}"
  "An empty hidden section, expanded, as a JSON member.")

(defconst agent-repl-test-wire-roster--merged-json
  "\"closed\":{\"rows\":[]},\"recentlyMerged\":{\"header\":{\"label\":{},\"count\":{}},\"groups\":[],\"collapsed\":{}}"
  "A merged band, collapsed, as a JSON member.")

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
          "\"mergedRows\":{\"rows\":[]},%s}")
  "A `RosterMergedRepoGroup' with its `drawn' arm left as a %s hole.")

(ert-deftest agent-repl-test-wire-roster-decodes-a-shown-merged-group ()
  "A group carries its repository key, its label header, its rows and its arm."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-merged-repo-group
                  (format agent-repl-test-wire-roster--merged-group-json "\"shown\":{}"))
                 '(:key (:repository (:id "r1" :dir "/r1"))
                   :header (:label (:text "alpha"))
                   :merged-rows (:rows nil)
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
                          "\"header\":{\"label\":{\"text\":\"alpha\"}},\"mergedRows\":{\"rows\":[]}}"))
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

;;;; ---- The repository view ----

(ert-deftest agent-repl-test-wire-roster-repository-view-decodes-its-open-count ()
  "The repository view carries the open section's count beside its sections."
  (should (equal (agent-repl-test-wire-roster--decode
                  #'agent-repl-wire-decode-roster-repository-view
                  "{\"sections\":[],\"openCount\":{\"workspaces\":2}}")
                 '(:sections nil :open-count (:workspaces 2)))))

(ert-deftest agent-repl-test-wire-roster-repository-view-refuses-an-unknown-field ()
  "An unknown key on the repository view is refused under its name."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repository-view
                  "{\"sections\":[],\"openCount\":{},\"extra\":{}}")
                 '("RosterRepositoryView" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-roster-repository-view-without-its-open-count-is-a-breach ()
  "The repository view carries the open section's count."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-repository-view "{\"sections\":[]}")
                 '("RosterRepositoryView" openCount "required message field is absent"))))

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

(ert-deftest agent-repl-test-wire-roster-an-open-count-refuses-an-unknown-field ()
  "An unknown key on the open count is refused at the count."
  (should (equal (agent-repl-test-wire-roster--breach
                  #'agent-repl-wire-decode-roster-open-count "{\"workspaces\":1,\"rows\":2}")
                 '("RosterOpenCount" rows "unknown field"))))
