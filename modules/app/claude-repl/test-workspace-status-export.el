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
  "An unseen workspace surfaces nil for every field and json-false for done-acked."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir nil)
    (let ((entry (claude-repl--workspace-status-entry "ws1")))
      (should (null (cdr (assoc "claude_state" entry))))
      (should (null (cdr (assoc "repl_state" entry))))
      (should (null (cdr (assoc "priority" entry))))
      (should (eq   (cdr (assoc "done_acked" entry)) json-false)))))

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

(provide 'test-workspace-status-export)
;;; test-workspace-status-export.el ends here
