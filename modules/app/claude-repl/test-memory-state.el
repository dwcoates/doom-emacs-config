;;; test-memory-state.el --- ERT tests for memory-state.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the per-workspace memory-state dump file
;; (`<root>/.claude/emacs/memory-state.el').  Covers path resolution,
;; value formatting (buffers/processes/timers/structs/scalars), render
;; assembly, file write/round-trip, and the integration hook from
;; `claude-repl--ws-set-claude-state' / `claude-repl--ws-set-repl-state'.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-memory-state.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Test fixtures ----

(defmacro claude-repl-test--with-temp-root (root-sym &rest body)
  "Bind ROOT-SYM to a fresh temp directory, run BODY, then clean up.
The directory is created before BODY runs and recursively deleted on exit."
  (declare (indent 1))
  `(let ((,root-sym (make-temp-file "claude-repl-memstate-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,root-sym)
         (delete-directory ,root-sym t)))))

(cl-defstruct claude-repl-test--memstate-fake foo bar)

;;;; ---- Tests: memory-state-file path resolver ----

(ert-deftest claude-repl-test-memory-state-file-nil-root ()
  "Returns nil when ROOT is nil so callers can short-circuit."
  (should (null (claude-repl--memory-state-file nil))))

(ert-deftest claude-repl-test-memory-state-file-builds-path ()
  "Composes `<root>/.claude/emacs/memory-state.el'."
  (let ((path (claude-repl--memory-state-file "/tmp/proj")))
    (should (stringp path))
    (should (string-match-p "/\\.claude/emacs/memory-state\\.el\\'" path))
    (should (string-prefix-p "/tmp/proj/" path))))

;;;; ---- Tests: memory-state-format-value ----

(ert-deftest claude-repl-test-memory-state-format-value-string ()
  "Strings pass through unchanged."
  (should (equal (claude-repl--memory-state-format-value "hello") "hello")))

(ert-deftest claude-repl-test-memory-state-format-value-number ()
  "Numbers pass through unchanged."
  (should (equal (claude-repl--memory-state-format-value 42) 42)))

(ert-deftest claude-repl-test-memory-state-format-value-keyword ()
  "Keyword values pass through unchanged."
  (should (eq (claude-repl--memory-state-format-value :idle) :idle)))

(ert-deftest claude-repl-test-memory-state-format-value-nil ()
  "Nil passes through as nil (no special string)."
  (should (null (claude-repl--memory-state-format-value nil))))

(ert-deftest claude-repl-test-memory-state-format-value-list ()
  "Plain lists pass through unchanged."
  (should (equal (claude-repl--memory-state-format-value '("a" "b" "c"))
                 '("a" "b" "c"))))

(ert-deftest claude-repl-test-memory-state-format-value-live-buffer ()
  "Live buffers render as `#<buffer NAME live>'."
  (let ((buf (get-buffer-create " *memstate-fmt-live*")))
    (unwind-protect
        (should (equal (claude-repl--memory-state-format-value buf)
                       (format "#<buffer %s live>" (buffer-name buf))))
      (kill-buffer buf))))

(ert-deftest claude-repl-test-memory-state-format-value-dead-buffer ()
  "Killed buffers render with the `dead' liveness marker.
Emacs nulls out `buffer-name' for killed buffers, so the contract is
`carries dead', not `preserves name'."
  (let ((buf (get-buffer-create " *memstate-fmt-dead*")))
    (kill-buffer buf)
    (let ((rendered (claude-repl--memory-state-format-value buf)))
      (should (string-match-p "\\`#<buffer .* dead>\\'" rendered)))))

(ert-deftest claude-repl-test-memory-state-format-value-process ()
  "Processes render with their liveness."
  (let ((proc (make-pipe-process :name "memstate-fmt-proc" :noquery t)))
    (unwind-protect
        (let ((rendered (claude-repl--memory-state-format-value proc)))
          (should (string-match-p "#<process memstate-fmt-proc" rendered))
          (should (string-match-p "running\\|exited" rendered)))
      (delete-process proc))))

(ert-deftest claude-repl-test-memory-state-format-value-timer ()
  "Timers render as `#<timer pending|triggered>'."
  (let ((tmr (timer-create)))
    (should (string-match-p "\\`#<timer \\(pending\\|triggered\\)>\\'"
                            (claude-repl--memory-state-format-value tmr)))))

(ert-deftest claude-repl-test-memory-state-format-value-cl-struct ()
  "cl-structs render via pp-to-string (trimmed)."
  (let* ((s (make-claude-repl-test--memstate-fake :foo 1 :bar "x"))
         (rendered (claude-repl--memory-state-format-value s)))
    (should (stringp rendered))
    (should (string-match-p "memstate-fake" rendered))))

;;;; ---- Tests: memory-state-render ----

(ert-deftest claude-repl-test-memory-state-render-prepends-header ()
  "Render output begins with :ws and :written-at."
  (let ((out (claude-repl--memory-state-render "ws1" '(:foo 1))))
    (should (equal (nth 0 out) :ws))
    (should (equal (nth 1 out) "ws1"))
    (should (equal (nth 2 out) :written-at))
    (should (stringp (nth 3 out)))))

(ert-deftest claude-repl-test-memory-state-render-preserves-keys ()
  "Every key in the input plist appears in the rendered plist."
  (let ((out (claude-repl--memory-state-render
              "ws1" '(:claude-state :thinking :priority "p1" :pending-prompts (a b)))))
    (should (plist-member out :claude-state))
    (should (plist-member out :priority))
    (should (plist-member out :pending-prompts))
    (should (eq (plist-get out :claude-state) :thinking))
    (should (equal (plist-get out :priority) "p1"))
    (should (equal (plist-get out :pending-prompts) '(a b)))))

(ert-deftest claude-repl-test-memory-state-render-formats-buffer-value ()
  "Buffer values in the input plist are replaced with readable strings."
  (let ((buf (get-buffer-create " *memstate-render-buf*")))
    (unwind-protect
        (let ((out (claude-repl--memory-state-render
                    "ws1" `(:vterm-buffer ,buf))))
          (should (equal (plist-get out :vterm-buffer)
                         (format "#<buffer %s live>" (buffer-name buf)))))
      (kill-buffer buf))))

;;;; ---- Tests: memory-state-write-file (round-trip) ----

(ert-deftest claude-repl-test-memory-state-write-file-creates-parent-dir ()
  "Write creates `.claude/emacs/' parent dir when missing."
  (claude-repl-test--with-temp-root root
    (let ((file (claude-repl--memory-state-file root)))
      (should-not (file-directory-p (file-name-directory file)))
      (claude-repl--memory-state-write-file file '(:foo 1))
      (should (file-directory-p (file-name-directory file)))
      (should (file-exists-p file)))))

(ert-deftest claude-repl-test-memory-state-write-file-round-trips ()
  "Written sexp is `read'-able and equals the input plist."
  (claude-repl-test--with-temp-root root
    (let* ((file (claude-repl--memory-state-file root))
           (data '(:ws "w" :written-at "t" :claude-state :thinking :priority "p1")))
      (claude-repl--memory-state-write-file file data)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((read-back (read (current-buffer))))
          (should (equal read-back data)))))))

(ert-deftest claude-repl-test-memory-state-write-file-header-present ()
  "File begins with the `memory-state dump' header comment."
  (claude-repl-test--with-temp-root root
    (let ((file (claude-repl--memory-state-file root)))
      (claude-repl--memory-state-write-file file '(:foo 1))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "memory-state dump" (buffer-string)))
        (should (string-match-p "dump-workspace" (buffer-string)))))))

;;;; ---- Tests: memory-state-save (full pipeline) ----

(ert-deftest claude-repl-test-memory-state-save-nil-ws-no-op ()
  "Save with nil ws is a no-op and returns without error."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--memory-state-save nil))))

(ert-deftest claude-repl-test-memory-state-save-missing-ws-no-op ()
  "Save for a ws not in `claude-repl--workspaces' is a no-op."
  (claude-repl-test--with-clean-state
    ;; ws not registered; no file should be created anywhere
    (should-not (claude-repl--memory-state-save "ghost-ws"))))

(ert-deftest claude-repl-test-memory-state-save-no-project-dir-no-op ()
  "Save skips when :project-dir is unset."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "stub-ws" :claude-state :idle)
    ;; no :project-dir → save should bail silently
    (claude-repl--memory-state-save "stub-ws")
    ;; Nothing to assert besides absence of error; no file path exists.
    (should t)))

(ert-deftest claude-repl-test-memory-state-save-writes-file ()
  "Save writes the file under <project-dir>/.claude/emacs/memory-state.el."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-root root
      (claude-repl--ws-put "ws1" :project-dir root)
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (claude-repl--memory-state-save "ws1")
      (let ((file (claude-repl--memory-state-file root)))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (equal (plist-get data :ws) "ws1"))
            (should (eq (plist-get data :claude-state) :thinking))
            (should (equal (plist-get data :project-dir) root))))))))

(ert-deftest claude-repl-test-memory-state-save-overwrites ()
  "Repeated save overwrites the file with the latest plist."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-root root
      (claude-repl--ws-put "ws1" :project-dir root)
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (claude-repl--memory-state-save "ws1")
      (claude-repl--ws-put "ws1" :claude-state :done)
      (claude-repl--memory-state-save "ws1")
      (let ((file (claude-repl--memory-state-file root)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (eq (plist-get data :claude-state) :done))))))))

(ert-deftest claude-repl-test-memory-state-save-renders-buffer-value ()
  "Saved file contains the readable buffer string, not a `#<…>' literal that breaks `read'."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-root root
      (let ((buf (get-buffer-create " *memstate-save-buf*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "ws1" :project-dir root)
              (claude-repl--ws-put "ws1" :vterm-buffer buf)
              (claude-repl--ws-put "ws1" :claude-state :idle)
              (claude-repl--memory-state-save "ws1")
              (let ((file (claude-repl--memory-state-file root)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (let ((data (read (current-buffer))))
                    (should (stringp (plist-get data :vterm-buffer)))
                    (should (string-match-p "live"
                                            (plist-get data :vterm-buffer)))))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: integration with state setters ----

(ert-deftest claude-repl-test-memory-state-claude-state-setter-writes-file ()
  "`--ws-set-claude-state' triggers a memory-state save."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-root root
      (claude-repl--ws-put "ws1" :project-dir root)
      (claude-repl--ws-set-claude-state "ws1" :thinking)
      (let ((file (claude-repl--memory-state-file root)))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (eq (plist-get data :claude-state) :thinking))))))))

(ert-deftest claude-repl-test-memory-state-repl-state-setter-writes-file ()
  "`--ws-set-repl-state' triggers a memory-state save."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-root root
      (claude-repl--ws-put "ws1" :project-dir root)
      ;; :dead avoids the `:active`/`:inactive`/`:hidden` --state-save side
      ;; effect that would touch the snapshot file paths; we want to test
      ;; memory-state specifically.
      (claude-repl--ws-set-repl-state "ws1" :dead)
      (let ((file (claude-repl--memory-state-file root)))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (should (eq (plist-get data :repl-state) :dead))))))))

(provide 'test-memory-state)
;;; test-memory-state.el ends here
