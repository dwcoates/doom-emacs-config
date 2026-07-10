;;; test-workspace-status-export.el --- ERT tests for workspace-status-export.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the JSON status export consumed by the `/workspace-status'
;; skill.  Covers keyword stringification, per-workspace entry shape,
;; snapshot collection across the registered workspaces, and the
;; on-disk write path.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-workspace-status-export.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'json)

;;;; ---- Tests: keyword-to-string helper ----

(ert-deftest agent-repl-test-ws-keyword-to-string-nil ()
  "nil maps to nil so empty fields serialize as JSON null."
  (should (null (agent-repl--ws-keyword-to-string nil))))

(ert-deftest agent-repl-test-ws-keyword-to-string-keyword ()
  "Keywords lose their leading colon."
  (should (equal "thinking" (agent-repl--ws-keyword-to-string :thinking)))
  (should (equal "stop-failed" (agent-repl--ws-keyword-to-string :stop-failed))))

(ert-deftest agent-repl-test-ws-keyword-to-string-symbol ()
  "Plain symbols (e.g. `clean'/`dirty' from :git-clean) serialize as their name."
  (should (equal "clean" (agent-repl--ws-keyword-to-string 'clean)))
  (should (equal "dirty" (agent-repl--ws-keyword-to-string 'dirty))))

(ert-deftest agent-repl-test-ws-keyword-to-string-string-passthrough ()
  "Strings (e.g. priority \"p1\") pass through unchanged."
  (should (equal "p1" (agent-repl--ws-keyword-to-string "p1"))))

;;;; ---- Tests: per-workspace entry shape ----

(ert-deftest agent-repl-test-workspace-status-entry-populated ()
  "A fully populated workspace surfaces every documented field as strings."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--ws-set-repl-state "ws1" :active)
    (agent-repl--ws-put "ws1" :project-dir "/tmp/proj")
    (agent-repl--ws-put "ws1" :source-ws-dir "/tmp/src")
    (agent-repl--ws-put "ws1" :priority "p1")
    (agent-repl--ws-put "ws1" :last-prompt-summary "Fix the bug")
    (agent-repl--ws-put "ws1" :git-clean 'dirty)
    (agent-repl--ws-put "ws1" :done-acked t)
    (let ((entry (agent-repl--workspace-status-entry "ws1")))
      (should (equal (cdr (assoc "agent_state" entry)) "thinking"))
      (should (equal (cdr (assoc "repl_state" entry)) "active"))
      (should (equal (cdr (assoc "project_dir" entry)) "/tmp/proj"))
      (should (equal (cdr (assoc "source_ws_dir" entry)) "/tmp/src"))
      (should (equal (cdr (assoc "priority" entry)) "p1"))
      (should (equal (cdr (assoc "last_prompt_summary" entry)) "Fix the bug"))
      (should (equal (cdr (assoc "git_clean" entry)) "dirty"))
      (should (eq    (cdr (assoc "done_acked" entry)) t)))))

(ert-deftest agent-repl-test-workspace-status-entry-legacy-claude-state-key ()
  "The entry also carries the legacy claude_state key, equal to agent_state.
External (out-of-repo) consumers of workspace-status.json still read
claude_state, so the writer emits both until they migrate."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (let ((entry (agent-repl--workspace-status-entry "ws1")))
      (should (equal (cdr (assoc "claude_state" entry))
                     (cdr (assoc "agent_state" entry)))))))

(ert-deftest agent-repl-test-workspace-status-entry-legacy-key-null-when-absent ()
  "The legacy claude_state key serializes as json-null for an unseen ws."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir nil)
    (let ((entry (agent-repl--workspace-status-entry "ws1")))
      (should (eq (cdr (assoc "claude_state" entry)) json-null)))))

(ert-deftest agent-repl-test-workspace-status-entry-empty-ws ()
  "An unseen workspace surfaces `json-null' for every absent field and
`json-false' for done-acked."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir nil)
    (let ((entry (agent-repl--workspace-status-entry "ws1")))
      (should (eq (cdr (assoc "agent_state" entry)) json-null))
      (should (eq (cdr (assoc "repl_state"   entry)) json-null))
      (should (eq (cdr (assoc "priority"     entry)) json-null))
      (should (eq (cdr (assoc "done_acked"   entry)) json-false)))))

(ert-deftest agent-repl-test-json-null-if-nil ()
  "`agent-repl--json-null-if-nil' substitutes the sentinel for nil and
leaves non-nil values alone."
  (should (eq (agent-repl--json-null-if-nil nil)   json-null))
  (should (equal (agent-repl--json-null-if-nil "x") "x"))
  (should (eq (agent-repl--json-null-if-nil t)     t)))

;;;; ---- Tests: snapshot collects all workspaces ----

(ert-deftest agent-repl-test-workspace-status-snapshot-includes-all ()
  "snapshot lists every key in agent-repl--workspaces under `workspaces'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws-a" :idle)
    (agent-repl--ws-set-agent-state "ws-b" :thinking)
    (let* ((snap (agent-repl--workspace-status-snapshot))
           (workspaces (cdr (assoc "workspaces" snap))))
      (should (hash-table-p workspaces))
      (should (gethash "ws-a" workspaces))
      (should (gethash "ws-b" workspaces))
      (should (equal "idle"
                     (cdr (assoc "agent_state" (gethash "ws-a" workspaces)))))
      (should (equal "thinking"
                     (cdr (assoc "agent_state" (gethash "ws-b" workspaces))))))))

(ert-deftest agent-repl-test-workspace-status-snapshot-has-updated-at ()
  "snapshot stamps an `updated_at' ISO-ish string at the top level."
  (agent-repl-test--with-clean-state
    (let* ((snap (agent-repl--workspace-status-snapshot))
           (stamp (cdr (assoc "updated_at" snap))))
      (should (stringp stamp))
      ;; YYYY-MM-DDTHH:MM:SS<tz>
      (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
                              stamp)))))

(ert-deftest agent-repl-test-workspace-status-snapshot-empty-roster ()
  "An empty roster still produces a parseable snapshot — workspaces is an empty object."
  (agent-repl-test--with-clean-state
    (let* ((snap (agent-repl--workspace-status-snapshot))
           (workspaces (cdr (assoc "workspaces" snap))))
      (should (hash-table-p workspaces))
      (should (zerop (hash-table-count workspaces))))))

(ert-deftest agent-repl-test-workspace-status-snapshot-skips-merged ()
  "Snapshot omits workspaces whose `repl-state' is `:merged'.
Merged workspaces have no live session and all their interesting
fields are null forever, so including them only bloats the JSON
encode."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws-live" :idle)
    (agent-repl--ws-set-repl-state   "ws-live" :active)
    (agent-repl--ws-set-repl-state   "ws-merged" :merged)
    (let* ((snap (agent-repl--workspace-status-snapshot))
           (workspaces (cdr (assoc "workspaces" snap))))
      (should (gethash "ws-live" workspaces))
      (should-not (gethash "ws-merged" workspaces)))))

(ert-deftest agent-repl-test-workspace-status-snapshot-skips-only-merged ()
  "All other `repl-state' values stay in the snapshot — only `:merged' is filtered."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "ws-active"   :active)
    (agent-repl--ws-set-repl-state "ws-inactive" :inactive)
    (agent-repl--ws-set-repl-state "ws-hidden"   :hidden)
    (agent-repl--ws-set-repl-state "ws-dead"     :dead)
    (agent-repl--ws-set-repl-state "ws-merged"   :merged)
    (let* ((snap (agent-repl--workspace-status-snapshot))
           (workspaces (cdr (assoc "workspaces" snap))))
      (should (gethash "ws-active"   workspaces))
      (should (gethash "ws-inactive" workspaces))
      (should (gethash "ws-hidden"   workspaces))
      (should (gethash "ws-dead"     workspaces))
      (should-not (gethash "ws-merged" workspaces)))))

(ert-deftest agent-repl-test-workspace-status-merged-p ()
  "`agent-repl--workspace-status-merged-p' returns t exactly for `:merged' workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "ws-m" :merged)
    (agent-repl--ws-set-repl-state "ws-a" :active)
    (agent-repl--ws-put            "ws-n" :foo 1) ; no repl-state set → nil
    (should      (agent-repl--workspace-status-merged-p "ws-m"))
    (should-not  (agent-repl--workspace-status-merged-p "ws-a"))
    (should-not  (agent-repl--workspace-status-merged-p "ws-n"))))

;;;; ---- Tests: JSON write to disk ----

(ert-deftest agent-repl-test-write-workspace-status-creates-file ()
  "write-workspace-status writes a file at the configured path."
  (agent-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "agent-repl-status-" nil ".json"))
           (agent-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (agent-repl--ws-set-agent-state "ws-disk" :done)
            (agent-repl--write-workspace-status)
            (should (file-exists-p tmp)))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest agent-repl-test-write-workspace-status-forces-utf-8 ()
  "Writer pins `coding-system-for-write' to utf-8-unix.

Regression guard: on Emacs 30 the default `with-temp-file' path can
land in `select-safe-coding-system' when the serialized JSON includes
bytes whose default encoding is ambiguous (e.g. a U+FFFD that snuck
into a `:last-prompt-summary' upstream).  The interactive prompt then
re-fires every workspace-status-write tick (1 Hz via the staggered
scheduler), which is unusable.  Detect a regression by intercepting
`write-region' and asserting the active coding system at write time."
  (agent-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "agent-repl-status-" nil ".json"))
           (agent-repl-workspace-status-file tmp)
           (observed-coding nil)
           (advice (lambda (orig start end filename &optional append visit lockname mustbenew)
                     (setq observed-coding coding-system-for-write)
                     (funcall orig start end filename append visit lockname mustbenew))))
      (unwind-protect
          (progn
            (advice-add 'write-region :around advice)
            (agent-repl--ws-set-agent-state "ws-codec" :idle)
            (agent-repl--write-workspace-status)
            (should (eq observed-coding 'utf-8-unix)))
        (advice-remove 'write-region advice)
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest agent-repl-test-write-workspace-status-parses-back ()
  "The written file is valid JSON and contains the workspace state."
  (agent-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "agent-repl-status-" nil ".json"))
           (agent-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (agent-repl--ws-set-agent-state "ws-rt" :permission)
            (agent-repl--ws-put "ws-rt" :priority "p2")
            (agent-repl--write-workspace-status)
            (let* ((json-object-type 'alist)
                   (parsed (json-read-file tmp))
                   (ws (cdr (assoc 'ws-rt (cdr (assoc 'workspaces parsed))))))
              (should ws)
              (should (equal "permission" (cdr (assoc 'agent_state ws))))
              (should (equal "p2" (cdr (assoc 'priority ws))))))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest agent-repl-test-write-workspace-status-nil-fields-serialize-as-null ()
  "Absent optional fields render as JSON null, not `{}'.  Regression
guard for the bare-nil-as-empty-object behavior of both `json-encode'
and `json-serialize'.  Output is compact (no spaces after colons)
since the writer uses `json-serialize' without pretty-printing."
  (agent-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "agent-repl-status-" nil ".json"))
           (agent-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (agent-repl--ws-set-agent-state "ws-null" :idle)
            (agent-repl--write-workspace-status)
            (with-temp-buffer
              (insert-file-contents tmp)
              (let ((raw (buffer-string)))
                (should (string-match-p "\"priority\":null" raw))
                (should (string-match-p "\"last_prompt_summary\":null" raw))
                (should-not (string-match-p "\"priority\":{}" raw)))))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest agent-repl-test-write-workspace-status-is-compact-not-pretty ()
  "The on-disk file is compact JSON.  Regression guard for the
pretty-printer that allocated ~900 MB of transient garbage per encode
on a 111-workspace registry."
  (agent-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "agent-repl-status-" nil ".json"))
           (agent-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (agent-repl--ws-set-agent-state "ws-compact" :idle)
            (agent-repl--write-workspace-status)
            (with-temp-buffer
              (insert-file-contents tmp)
              (let ((raw (buffer-string)))
                ;; Pretty-printed output would have newlines+indent
                ;; between every key/value pair; compact output has
                ;; at most a single trailing newline.
                (should (< (cl-count ?\n raw) 3)))))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest agent-repl-test-write-workspace-status-creates-parent-dir ()
  "Parent directory is created on demand so the user need not pre-provision it."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-repl-status-dir-" t))
           (tmp  (expand-file-name "nested/sub/workspace-status.json" root))
           (agent-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (agent-repl--write-workspace-status)
            (should (file-exists-p tmp)))
        (when (file-directory-p root) (delete-directory root t))))))

;;;; ---- Tests: staggered write scheduler ----

(defmacro agent-repl-test--with-stubbed-run-at-time (captured-sym &rest body)
  "Bind CAPTURED-SYM to a list that accumulates `run-at-time' calls.
Each entry is (DELAY REPEAT FN . ARGS).  Inside BODY, `run-at-time'
returns a benign timer object that satisfies `timerp' so callers that
treat the return value as a real timer do not crash."
  (declare (indent 1))
  `(let ((,captured-sym nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (delay repeat fn &rest args)
                  (push (cons delay (cons repeat (cons fn args))) ,captured-sym)
                  ;; Return something timerp recognises.
                  (timer-create))))
       ,@body)))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-empty-roster ()
  "No workspaces → no sub-timers scheduled, list stays empty."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-stubbed-run-at-time calls
      (setq agent-repl--workspace-status-write-sub-timers nil)
      (agent-repl--reschedule-workspace-status-writes)
      (should (null calls))
      (should (null agent-repl--workspace-status-write-sub-timers)))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-schedules-n ()
  "With N registered workspaces, scheduler queues N sub-timers."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-a" :foo 1)
    (agent-repl--ws-put "ws-b" :foo 2)
    (agent-repl--ws-put "ws-c" :foo 3)
    (agent-repl-test--with-stubbed-run-at-time calls
      (setq agent-repl--workspace-status-write-sub-timers nil)
      (agent-repl--reschedule-workspace-status-writes)
      (should (= 3 (length calls)))
      (should (= 3 (length agent-repl--workspace-status-write-sub-timers))))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-spreads-evenly ()
  "Sub-timer delays are evenly spaced from 0 to (window - window/N).
For N=4, delays are 0, 15, 30, 45 (with window=60)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-workspace-status-write-window-seconds 60))
      (dolist (ws '("ws-a" "ws-b" "ws-c" "ws-d"))
        (agent-repl--ws-put ws :foo 1))
      (agent-repl-test--with-stubbed-run-at-time calls
        (setq agent-repl--workspace-status-write-sub-timers nil)
        (agent-repl--reschedule-workspace-status-writes)
        ;; Capture call delays — order is reversed because we `push'.
        (let ((delays (sort (mapcar #'car calls) #'<)))
          (should (equal '(0.0 15.0 30.0 45.0) delays)))))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-schedules-target-fn ()
  "Each scheduled sub-timer targets `agent-repl--write-workspace-status'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-a" :foo 1)
    (agent-repl-test--with-stubbed-run-at-time calls
      (setq agent-repl--workspace-status-write-sub-timers nil)
      (agent-repl--reschedule-workspace-status-writes)
      (should (= 1 (length calls)))
      (let* ((call (car calls))
             ;; call shape: (DELAY REPEAT FN . ARGS) — destructure.
             (fn (car (cddr call))))
        (should (eq fn #'agent-repl--write-workspace-status))))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-cancels-prior ()
  "Reschedule cancels prior sub-timers before queueing new ones."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-a" :foo 1)
    (let* ((cancel-count 0)
           (stale-timer (timer-create))
           (agent-repl--workspace-status-write-sub-timers (list stale-timer)))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancel-count)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (timer-create))))
        (agent-repl--reschedule-workspace-status-writes)
        (should (= 1 cancel-count))))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-uses-custom-window ()
  "Custom window seconds change the spacing.  Window=120, N=2 → delays 0, 60."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-workspace-status-write-window-seconds 120))
      (agent-repl--ws-put "ws-a" :foo 1)
      (agent-repl--ws-put "ws-b" :foo 2)
      (agent-repl-test--with-stubbed-run-at-time calls
        (setq agent-repl--workspace-status-write-sub-timers nil)
        (agent-repl--reschedule-workspace-status-writes)
        (let ((delays (sort (mapcar #'car calls) #'<)))
          (should (equal '(0.0 60.0) delays)))))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-excludes-merged ()
  "Scheduler counts only non-merged workspaces when computing N.
With 1 live + 2 merged, N=1 → exactly one sub-timer is scheduled
\(at delay 0), not three.  This is the second half of the
merged-filter — without it, 100+ merged workspaces would still
schedule 100+ writes per window even though each write encoded the
filtered (small) snapshot."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "ws-live"    :active)
    (agent-repl--ws-set-repl-state "ws-merged1" :merged)
    (agent-repl--ws-set-repl-state "ws-merged2" :merged)
    (agent-repl-test--with-stubbed-run-at-time calls
      (setq agent-repl--workspace-status-write-sub-timers nil)
      (agent-repl--reschedule-workspace-status-writes)
      (should (= 1 (length calls))))))

(ert-deftest agent-repl-test-reschedule-workspace-status-writes-all-merged ()
  "All-merged roster schedules zero sub-timers — same as empty roster."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "ws-m1" :merged)
    (agent-repl--ws-set-repl-state "ws-m2" :merged)
    (agent-repl-test--with-stubbed-run-at-time calls
      (setq agent-repl--workspace-status-write-sub-timers nil)
      (agent-repl--reschedule-workspace-status-writes)
      (should (null calls))
      (should (null agent-repl--workspace-status-write-sub-timers)))))

(ert-deftest agent-repl-test-workspace-status-live-count ()
  "`agent-repl--workspace-status-live-count' returns the non-merged total."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "ws-a" :active)
    (agent-repl--ws-set-repl-state "ws-b" :inactive)
    (agent-repl--ws-set-repl-state "ws-c" :merged)
    (agent-repl--ws-set-repl-state "ws-d" :merged)
    (should (= 2 (agent-repl--workspace-status-live-count)))))

(provide 'test-workspace-status-export)
;;; test-workspace-status-export.el ends here
