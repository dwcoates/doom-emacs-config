;;; test-memory-state.el --- ERT tests for memory-state.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the per-workspace memory-state dump file
;; (`<root>/.claude/emacs/memory-state.el').  Covers path resolution,
;; value formatting (buffers/processes/timers/structs/scalars), render
;; assembly, file write/round-trip, and the integration hook from
;; `agent-repl--ws-set-agent-state' / `agent-repl--ws-set-repl-state'.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-memory-state.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Test fixtures ----

(defmacro agent-repl-test--with-temp-root (root-sym &rest body)
  "Bind ROOT-SYM to a fresh temp directory, run BODY, then clean up.
The directory is created before BODY runs and recursively deleted on exit."
  (declare (indent 1))
  `(let ((,root-sym (make-temp-file "agent-repl-memstate-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,root-sym)
         (delete-directory ,root-sym t)))))

(cl-defstruct agent-repl-test--memstate-fake foo bar)

;;;; ---- Tests: memory-state-file path resolver ----

(ert-deftest agent-repl-test-memory-state-file-nil-root ()
  "Returns nil when ROOT is nil so callers can short-circuit."
  (should (null (agent-repl--memory-state-file nil))))

(ert-deftest agent-repl-test-memory-state-file-builds-path ()
  "Composes `<root>/.claude/emacs/memory-state.el'."
  (let ((path (agent-repl--memory-state-file "/tmp/proj")))
    (should (stringp path))
    (should (string-match-p "/\\.claude/emacs/memory-state\\.el\\'" path))
    (should (string-prefix-p "/tmp/proj/" path))))

;;;; ---- Tests: memory-state-format-value ----

(ert-deftest agent-repl-test-memory-state-format-value-string ()
  "Strings pass through unchanged."
  (should (equal (agent-repl--memory-state-format-value "hello") "hello")))

(ert-deftest agent-repl-test-memory-state-format-value-number ()
  "Numbers pass through unchanged."
  (should (equal (agent-repl--memory-state-format-value 42) 42)))

(ert-deftest agent-repl-test-memory-state-format-value-keyword ()
  "Keyword values pass through unchanged."
  (should (eq (agent-repl--memory-state-format-value :idle) :idle)))

(ert-deftest agent-repl-test-memory-state-format-value-nil ()
  "Nil passes through as nil (no special string)."
  (should (null (agent-repl--memory-state-format-value nil))))

(ert-deftest agent-repl-test-memory-state-format-value-list ()
  "Plain lists pass through unchanged."
  (should (equal (agent-repl--memory-state-format-value '("a" "b" "c"))
                 '("a" "b" "c"))))

(ert-deftest agent-repl-test-memory-state-format-value-live-buffer ()
  "Live buffers render as `#<buffer NAME live>'."
  (let ((buf (get-buffer-create " *memstate-fmt-live*")))
    (unwind-protect
        (should (equal (agent-repl--memory-state-format-value buf)
                       (format "#<buffer %s live>" (buffer-name buf))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-memory-state-format-value-dead-buffer ()
  "Killed buffers render with the `dead' liveness marker.
Emacs nulls out `buffer-name' for killed buffers, so the contract is
`carries dead', not `preserves name'."
  (let ((buf (get-buffer-create " *memstate-fmt-dead*")))
    (kill-buffer buf)
    (let ((rendered (agent-repl--memory-state-format-value buf)))
      (should (string-match-p "\\`#<buffer .* dead>\\'" rendered)))))

(ert-deftest agent-repl-test-memory-state-format-value-process ()
  "Processes render with their liveness."
  (let ((proc (make-pipe-process :name "memstate-fmt-proc" :noquery t)))
    (unwind-protect
        (let ((rendered (agent-repl--memory-state-format-value proc)))
          (should (string-match-p "#<process memstate-fmt-proc" rendered))
          (should (string-match-p "running\\|exited" rendered)))
      (delete-process proc))))

(ert-deftest agent-repl-test-memory-state-format-value-timer ()
  "Timers render as `#<timer pending|triggered>'."
  (let ((tmr (timer-create)))
    (should (string-match-p "\\`#<timer \\(pending\\|triggered\\)>\\'"
                            (agent-repl--memory-state-format-value tmr)))))

(ert-deftest agent-repl-test-memory-state-format-value-cl-struct ()
  "cl-structs render via pp-to-string (trimmed)."
  (let* ((s (make-agent-repl-test--memstate-fake :foo 1 :bar "x"))
         (rendered (agent-repl--memory-state-format-value s)))
    (should (stringp rendered))
    (should (string-match-p "memstate-fake" rendered))))

;;;; ---- Tests: memory-state-render ----

(ert-deftest agent-repl-test-memory-state-render-prepends-header ()
  "Render output begins with :ws and :written-at."
  (let ((out (agent-repl--memory-state-render "ws1" '(:foo 1))))
    (should (equal (nth 0 out) :ws))
    (should (equal (nth 1 out) "ws1"))
    (should (equal (nth 2 out) :written-at))
    (should (stringp (nth 3 out)))))

(ert-deftest agent-repl-test-memory-state-render-preserves-keys ()
  "Every key in the input plist appears in the rendered plist."
  (let ((out (agent-repl--memory-state-render
              "ws1" '(:agent-state :thinking :priority "p1" :deferred-prompts (a b)))))
    (should (plist-member out :agent-state))
    (should (plist-member out :priority))
    (should (plist-member out :deferred-prompts))
    (should (eq (plist-get out :agent-state) :thinking))
    (should (equal (plist-get out :priority) "p1"))
    (should (equal (plist-get out :deferred-prompts) '(a b)))))

(ert-deftest agent-repl-test-memory-state-render-formats-buffer-value ()
  "Buffer values in the input plist are replaced with readable strings."
  (let ((buf (get-buffer-create " *memstate-render-buf*")))
    (unwind-protect
        (let ((out (agent-repl--memory-state-render
                    "ws1" `(:frontend-buffer ,buf))))
          (should (equal (plist-get out :frontend-buffer)
                         (format "#<buffer %s live>" (buffer-name buf)))))
      (kill-buffer buf))))

;;;; ---- Tests: memory-state-write-file (round-trip) ----

(ert-deftest agent-repl-test-memory-state-write-file-creates-parent-dir ()
  "Write creates `.claude/emacs/' parent dir when missing."
  (agent-repl-test--with-temp-root root
    (let ((file (agent-repl--memory-state-file root)))
      (should-not (file-directory-p (file-name-directory file)))
      (agent-repl--memory-state-write-file file '(:foo 1))
      (should (file-directory-p (file-name-directory file)))
      (should (file-exists-p file)))))

(ert-deftest agent-repl-test-memory-state-write-file-round-trips ()
  "Written sexp is `read'-able and equals the input plist."
  (agent-repl-test--with-temp-root root
    (let* ((file (agent-repl--memory-state-file root))
           (data '(:ws "w" :written-at "t" :agent-state :thinking :priority "p1")))
      (agent-repl--memory-state-write-file file data)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((read-back (read (current-buffer))))
          (should (equal read-back data)))))))

(ert-deftest agent-repl-test-memory-state-write-file-header-present ()
  "File begins with the `memory-state dump' header comment."
  (agent-repl-test--with-temp-root root
    (let ((file (agent-repl--memory-state-file root)))
      (agent-repl--memory-state-write-file file '(:foo 1))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "memory-state dump" (buffer-string)))
        (should (string-match-p "dump-workspace" (buffer-string)))))))

;;;; ---- Tests: memory-state-save (full pipeline) ----

(ert-deftest agent-repl-test-memory-state-save-nil-ws-no-op ()
  "Save with nil ws is a no-op and returns without error."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--memory-state-save nil))))

(ert-deftest agent-repl-test-memory-state-save-missing-ws-no-op ()
  "Save for a ws not in `agent-repl--workspaces' is a no-op."
  (agent-repl-test--with-clean-state
    ;; ws not registered; no file should be created anywhere
    (should-not (agent-repl--memory-state-save "ghost-ws"))))

(ert-deftest agent-repl-test-memory-state-save-no-project-dir-no-op ()
  "Save skips when :project-dir is unset."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "stub-ws" :agent-state :idle)
    ;; no :project-dir → save should bail silently
    (agent-repl--memory-state-save "stub-ws")
    ;; Nothing to assert besides absence of error; no file path exists.
    (should t)))

(ert-deftest agent-repl-test-memory-state-save-writes-file ()
  "Save writes the file under <project-dir>/.claude/emacs/memory-state.el."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-root root
      (agent-repl--ws-put "ws1" :project-dir root)
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (agent-repl--memory-state-save "ws1")
      (let ((file (agent-repl--memory-state-file root)))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (equal (plist-get data :ws) "ws1"))
            (should (eq (plist-get data :agent-state) :thinking))
            (should (equal (plist-get data :project-dir) root))))))))

(ert-deftest agent-repl-test-memory-state-save-overwrites ()
  "Repeated save overwrites the file with the latest plist."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-root root
      (agent-repl--ws-put "ws1" :project-dir root)
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (agent-repl--memory-state-save "ws1")
      (agent-repl--ws-put "ws1" :agent-state :done)
      (agent-repl--memory-state-save "ws1")
      (let ((file (agent-repl--memory-state-file root)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (eq (plist-get data :agent-state) :done))))))))

(ert-deftest agent-repl-test-memory-state-save-renders-buffer-value ()
  "Saved file contains the readable buffer string, not a `#<…>' literal that breaks `read'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-root root
      (let ((buf (get-buffer-create " *memstate-save-buf*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "ws1" :project-dir root)
              (agent-repl--ws-put "ws1" :vterm-buffer buf)
              (agent-repl--ws-put "ws1" :agent-state :idle)
              (agent-repl--memory-state-save "ws1")
              (let ((file (agent-repl--memory-state-file root)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (let ((data (read (current-buffer))))
                    (should (stringp (plist-get data :vterm-buffer)))
                    (should (string-match-p "live"
                                            (plist-get data :vterm-buffer)))))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: integration with state setters ----

(ert-deftest agent-repl-test-memory-state-agent-state-setter-writes-file ()
  "`--ws-set-agent-state' triggers a memory-state save."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-root root
      (agent-repl--ws-put "ws1" :project-dir root)
      (agent-repl--ws-set-agent-state "ws1" :thinking)
      (let ((file (agent-repl--memory-state-file root)))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (eq (plist-get data :agent-state) :thinking))))))))

(ert-deftest agent-repl-test-memory-state-repl-state-setter-writes-file ()
  "`--ws-set-repl-state' triggers a memory-state save."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-root root
      (agent-repl--ws-put "ws1" :project-dir root)
      ;; :dead avoids the `:active`/`:inactive` --state-save side
      ;; effect that would touch the snapshot file paths; we want to test
      ;; memory-state specifically.
      (agent-repl--ws-set-repl-state "ws1" :dead)
      (let ((file (agent-repl--memory-state-file root)))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (eq (plist-get data :repl-state) :dead))))))))

(provide 'test-memory-state)
;;; test-memory-state.el ends here
