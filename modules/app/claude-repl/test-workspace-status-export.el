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

(ert-deftest claude-repl-test-ws-keyword-to-string-nil ()
  "nil maps to nil so empty fields serialize as JSON null."
  (should (null (claude-repl--ws-keyword-to-string nil))))

(ert-deftest claude-repl-test-ws-keyword-to-string-keyword ()
  "Keywords lose their leading colon."
  (should (equal "thinking" (claude-repl--ws-keyword-to-string :thinking)))
  (should (equal "stop-failed" (claude-repl--ws-keyword-to-string :stop-failed))))

(ert-deftest claude-repl-test-ws-keyword-to-string-symbol ()
  "Plain symbols (e.g. `clean'/`dirty' from :git-clean) serialize as their name."
  (should (equal "clean" (claude-repl--ws-keyword-to-string 'clean)))
  (should (equal "dirty" (claude-repl--ws-keyword-to-string 'dirty))))

(ert-deftest claude-repl-test-ws-keyword-to-string-string-passthrough ()
  "Strings (e.g. priority \"p1\") pass through unchanged."
  (should (equal "p1" (claude-repl--ws-keyword-to-string "p1"))))

;;;; ---- Tests: per-workspace entry shape ----

(ert-deftest claude-repl-test-workspace-status-entry-populated ()
  "A fully populated workspace surfaces every documented field as strings."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl--ws-set-repl-state "ws1" :active)
    (claude-repl--ws-put "ws1" :project-dir "/tmp/proj")
    (claude-repl--ws-put "ws1" :source-ws-dir "/tmp/src")
    (claude-repl--ws-put "ws1" :priority "p1")
    (claude-repl--ws-put "ws1" :last-prompt-summary "Fix the bug")
    (claude-repl--ws-put "ws1" :git-clean 'dirty)
    (claude-repl--ws-put "ws1" :done-acked t)
    (let ((entry (claude-repl--workspace-status-entry "ws1")))
      (should (equal (cdr (assoc "claude_state" entry)) "thinking"))
      (should (equal (cdr (assoc "repl_state" entry)) "active"))
      (should (equal (cdr (assoc "project_dir" entry)) "/tmp/proj"))
      (should (equal (cdr (assoc "source_ws_dir" entry)) "/tmp/src"))
      (should (equal (cdr (assoc "priority" entry)) "p1"))
      (should (equal (cdr (assoc "last_prompt_summary" entry)) "Fix the bug"))
      (should (equal (cdr (assoc "git_clean" entry)) "dirty"))
      (should (eq    (cdr (assoc "done_acked" entry)) t)))))

(ert-deftest claude-repl-test-workspace-status-entry-empty-ws ()
  "An unseen workspace surfaces `json-null' for every absent field and
`json-false' for done-acked."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir nil)
    (let ((entry (claude-repl--workspace-status-entry "ws1")))
      (should (eq (cdr (assoc "claude_state" entry)) json-null))
      (should (eq (cdr (assoc "repl_state"   entry)) json-null))
      (should (eq (cdr (assoc "priority"     entry)) json-null))
      (should (eq (cdr (assoc "done_acked"   entry)) json-false)))))

(ert-deftest claude-repl-test-json-null-if-nil ()
  "`claude-repl--json-null-if-nil' substitutes the sentinel for nil and
leaves non-nil values alone."
  (should (eq (claude-repl--json-null-if-nil nil)   json-null))
  (should (equal (claude-repl--json-null-if-nil "x") "x"))
  (should (eq (claude-repl--json-null-if-nil t)     t)))

;;;; ---- Tests: snapshot collects all workspaces ----

(ert-deftest claude-repl-test-workspace-status-snapshot-includes-all ()
  "snapshot lists every key in claude-repl--workspaces under `workspaces'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws-a" :idle)
    (claude-repl--ws-set-claude-state "ws-b" :thinking)
    (let* ((snap (claude-repl--workspace-status-snapshot))
           (workspaces (cdr (assoc "workspaces" snap))))
      (should (hash-table-p workspaces))
      (should (gethash "ws-a" workspaces))
      (should (gethash "ws-b" workspaces))
      (should (equal "idle"
                     (cdr (assoc "claude_state" (gethash "ws-a" workspaces)))))
      (should (equal "thinking"
                     (cdr (assoc "claude_state" (gethash "ws-b" workspaces))))))))

(ert-deftest claude-repl-test-workspace-status-snapshot-has-updated-at ()
  "snapshot stamps an `updated_at' ISO-ish string at the top level."
  (claude-repl-test--with-clean-state
    (let* ((snap (claude-repl--workspace-status-snapshot))
           (stamp (cdr (assoc "updated_at" snap))))
      (should (stringp stamp))
      ;; YYYY-MM-DDTHH:MM:SS<tz>
      (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
                              stamp)))))

(ert-deftest claude-repl-test-workspace-status-snapshot-empty-roster ()
  "An empty roster still produces a parseable snapshot — workspaces is an empty object."
  (claude-repl-test--with-clean-state
    (let* ((snap (claude-repl--workspace-status-snapshot))
           (workspaces (cdr (assoc "workspaces" snap))))
      (should (hash-table-p workspaces))
      (should (zerop (hash-table-count workspaces))))))

;;;; ---- Tests: JSON write to disk ----

(ert-deftest claude-repl-test-write-workspace-status-creates-file ()
  "write-workspace-status writes a file at the configured path."
  (claude-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "claude-repl-status-" nil ".json"))
           (claude-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (claude-repl--ws-set-claude-state "ws-disk" :done)
            (claude-repl--write-workspace-status)
            (should (file-exists-p tmp)))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest claude-repl-test-write-workspace-status-parses-back ()
  "The written file is valid JSON and contains the workspace state."
  (claude-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "claude-repl-status-" nil ".json"))
           (claude-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (claude-repl--ws-set-claude-state "ws-rt" :permission)
            (claude-repl--ws-put "ws-rt" :priority "p2")
            (claude-repl--write-workspace-status)
            (let* ((json-object-type 'alist)
                   (parsed (json-read-file tmp))
                   (ws (cdr (assoc 'ws-rt (cdr (assoc 'workspaces parsed))))))
              (should ws)
              (should (equal "permission" (cdr (assoc 'claude_state ws))))
              (should (equal "p2" (cdr (assoc 'priority ws))))))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest claude-repl-test-write-workspace-status-nil-fields-serialize-as-null ()
  "Absent optional fields render as JSON null, not `{}'.  Regression
guard for `json-encode' treating bare nil as an empty alist."
  (claude-repl-test--with-clean-state
    (let* ((tmp (make-temp-file "claude-repl-status-" nil ".json"))
           (claude-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (claude-repl--ws-set-claude-state "ws-null" :idle)
            (claude-repl--write-workspace-status)
            (with-temp-buffer
              (insert-file-contents tmp)
              (let ((raw (buffer-string)))
                (should (string-match-p "\"priority\": null" raw))
                (should (string-match-p "\"last_prompt_summary\": null" raw))
                (should-not (string-match-p "\"priority\": {}" raw)))))
        (when (file-exists-p tmp) (delete-file tmp))))))

(ert-deftest claude-repl-test-write-workspace-status-creates-parent-dir ()
  "Parent directory is created on demand so the user need not pre-provision it."
  (claude-repl-test--with-clean-state
    (let* ((root (make-temp-file "claude-repl-status-dir-" t))
           (tmp  (expand-file-name "nested/sub/workspace-status.json" root))
           (claude-repl-workspace-status-file tmp))
      (unwind-protect
          (progn
            (claude-repl--write-workspace-status)
            (should (file-exists-p tmp)))
        (when (file-directory-p root) (delete-directory root t))))))

;;;; ---- Tests: staggered write scheduler ----

(defmacro claude-repl-test--with-stubbed-run-at-time (captured-sym &rest body)
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

(ert-deftest claude-repl-test-reschedule-workspace-status-writes-empty-roster ()
  "No workspaces → no sub-timers scheduled, list stays empty."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-stubbed-run-at-time calls
      (setq claude-repl--workspace-status-write-sub-timers nil)
      (claude-repl--reschedule-workspace-status-writes)
      (should (null calls))
      (should (null claude-repl--workspace-status-write-sub-timers)))))

(ert-deftest claude-repl-test-reschedule-workspace-status-writes-schedules-n ()
  "With N registered workspaces, scheduler queues N sub-timers."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-a" :foo 1)
    (claude-repl--ws-put "ws-b" :foo 2)
    (claude-repl--ws-put "ws-c" :foo 3)
    (claude-repl-test--with-stubbed-run-at-time calls
      (setq claude-repl--workspace-status-write-sub-timers nil)
      (claude-repl--reschedule-workspace-status-writes)
      (should (= 3 (length calls)))
      (should (= 3 (length claude-repl--workspace-status-write-sub-timers))))))

(ert-deftest claude-repl-test-reschedule-workspace-status-writes-spreads-evenly ()
  "Sub-timer delays are evenly spaced from 0 to (window - window/N).
For N=4, delays are 0, 15, 30, 45 (with window=60)."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-status-write-window-seconds 60))
      (dolist (ws '("ws-a" "ws-b" "ws-c" "ws-d"))
        (claude-repl--ws-put ws :foo 1))
      (claude-repl-test--with-stubbed-run-at-time calls
        (setq claude-repl--workspace-status-write-sub-timers nil)
        (claude-repl--reschedule-workspace-status-writes)
        ;; Capture call delays — order is reversed because we `push'.
        (let ((delays (sort (mapcar #'car calls) #'<)))
          (should (equal '(0.0 15.0 30.0 45.0) delays)))))))

(ert-deftest claude-repl-test-reschedule-workspace-status-writes-schedules-target-fn ()
  "Each scheduled sub-timer targets `claude-repl--write-workspace-status'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-a" :foo 1)
    (claude-repl-test--with-stubbed-run-at-time calls
      (setq claude-repl--workspace-status-write-sub-timers nil)
      (claude-repl--reschedule-workspace-status-writes)
      (should (= 1 (length calls)))
      (let* ((call (car calls))
             ;; call shape: (DELAY REPEAT FN . ARGS) — destructure.
             (fn (car (cddr call))))
        (should (eq fn #'claude-repl--write-workspace-status))))))

(ert-deftest claude-repl-test-reschedule-workspace-status-writes-cancels-prior ()
  "Reschedule cancels prior sub-timers before queueing new ones."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-a" :foo 1)
    (let* ((cancel-count 0)
           (stale-timer (timer-create))
           (claude-repl--workspace-status-write-sub-timers (list stale-timer)))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancel-count)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (timer-create))))
        (claude-repl--reschedule-workspace-status-writes)
        (should (= 1 cancel-count))))))

(ert-deftest claude-repl-test-reschedule-workspace-status-writes-uses-custom-window ()
  "Custom window seconds change the spacing.  Window=120, N=2 → delays 0, 60."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-status-write-window-seconds 120))
      (claude-repl--ws-put "ws-a" :foo 1)
      (claude-repl--ws-put "ws-b" :foo 2)
      (claude-repl-test--with-stubbed-run-at-time calls
        (setq claude-repl--workspace-status-write-sub-timers nil)
        (claude-repl--reschedule-workspace-status-writes)
        (let ((delays (sort (mapcar #'car calls) #'<)))
          (should (equal '(0.0 60.0) delays)))))))

(provide 'test-workspace-status-export)
;;; test-workspace-status-export.el ends here
