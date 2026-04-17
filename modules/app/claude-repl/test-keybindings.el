;;; test-keybindings.el --- ERT tests for keybindings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for keybinding helpers, debug commands, and utility functions
;; defined in keybindings.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-keybindings.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'json)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; Additional stubs for persp APIs used by --gather-ws-diagnostics
(unless (fboundp 'persp-get-by-name)
  (defun persp-get-by-name (_name) "Stub." nil))
(unless (fboundp 'persp-buffers)
  (defun persp-buffers (_persp) "Stub." nil))

;;;; ---- Tests: claude-repl--cons-name-state ----

(ert-deftest claude-repl-test-cons-name-state-with-state ()
  "cons-name-state should return (NAME . state) when workspace has state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (let ((result (claude-repl--cons-name-state "ws1")))
      (should (equal (car result) "ws1"))
      (should (eq (cdr result) :thinking)))))

(ert-deftest claude-repl-test-cons-name-state-no-state ()
  "cons-name-state should return (NAME . nil) when workspace has no state."
  (claude-repl-test--with-clean-state
    (let ((result (claude-repl--cons-name-state "nonexistent")))
      (should (equal (car result) "nonexistent"))
      (should-not (cdr result)))))

;;;; ---- Tests: claude-repl--format-workspace-state ----

(ert-deftest claude-repl-test-format-workspace-state-with-value ()
  "format-workspace-state should format a pair with a non-nil state."
  (let ((result (claude-repl--format-workspace-state '("my-ws" . :thinking))))
    (should (string= result "  my-ws: :thinking"))))

(ert-deftest claude-repl-test-format-workspace-state-nil-state ()
  "format-workspace-state should show 'nil' when state is nil."
  (let ((result (claude-repl--format-workspace-state '("my-ws" . nil))))
    (should (string= result "  my-ws: nil"))))

;;;; ---- Tests: claude-repl--format-buffer-info ----

(ert-deftest claude-repl-test-format-buffer-info-with-values ()
  "format-buffer-info should show buffer name, owning workspace, and persp workspace."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*"
    (setq-local claude-repl--owning-workspace "my-ws")
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
               (lambda (_buf) "persp-ws")))
      (let ((result (claude-repl--format-buffer-info (current-buffer))))
        (should (string-match-p "\\*claude-panel-abcd1234\\*" result))
        (should (string-match-p "owning=my-ws" result))
        (should (string-match-p "persp=persp-ws" result))))))

(ert-deftest claude-repl-test-format-buffer-info-nil-values ()
  "format-buffer-info should show 'nil' for missing owning workspace and persp."
  (claude-repl-test--with-temp-buffer "*test-format-nil*"
    (setq-local claude-repl--owning-workspace nil)
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
               (lambda (_buf) nil)))
      (let ((result (claude-repl--format-buffer-info (current-buffer))))
        (should (string-match-p "owning=nil" result))
        (should (string-match-p "persp=nil" result))))))

;;;; ---- Tests: claude-repl--kill-before-workspace-delete ----

(ert-deftest claude-repl-test-kill-before-workspace-delete-when-running ()
  "kill-before-workspace-delete should call claude-repl-kill when vterm is running."
  (let ((killed nil))
    (cl-letf (((symbol-function 'claude-repl--vterm-running-p) (lambda () t))
              ((symbol-function 'claude-repl-kill) (lambda () (setq killed t))))
      (claude-repl--kill-before-workspace-delete)
      (should killed))))

(ert-deftest claude-repl-test-kill-before-workspace-delete-when-not-running ()
  "kill-before-workspace-delete should not call claude-repl-kill when vterm is not running."
  (let ((killed nil))
    (cl-letf (((symbol-function 'claude-repl--vterm-running-p) (lambda () nil))
              ((symbol-function 'claude-repl-kill) (lambda () (setq killed t))))
      (claude-repl--kill-before-workspace-delete)
      (should-not killed))))

(ert-deftest claude-repl-test-kill-before-workspace-delete-ignores-args ()
  "kill-before-workspace-delete should accept and ignore any arguments."
  (let ((killed nil))
    (cl-letf (((symbol-function 'claude-repl--vterm-running-p) (lambda () t))
              ((symbol-function 'claude-repl-kill) (lambda () (setq killed t))))
      (claude-repl--kill-before-workspace-delete "arg1" "arg2" 42)
      (should killed))))

;;;; ---- Tests: claude-repl--read-workspace ----

(ert-deftest claude-repl-test-read-workspace-returns-match ()
  "read-workspace should return the value from completing-read."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt coll &rest _) (car coll))))
    (should (equal (claude-repl--read-workspace "Pick: ") "test-ws"))))

;;;; ---- Tests: claude-repl--read-workspace-with-default ----

(ert-deftest claude-repl-test-read-workspace-with-default ()
  "read-workspace-with-default should pass current workspace as default."
  (let ((captured-default nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _require _hist _hist-var _default)
                 (setq captured-default _default)
                 _default))
              ((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (claude-repl--read-workspace-with-default "Pick: ")))
        (should (equal captured-default "current-ws"))
        (should (equal result "current-ws"))))))

;;;; ---- Tests: claude-repl--read-known-workspace ----

(ert-deftest claude-repl-test-read-known-workspace-no-workspaces ()
  "read-known-workspace signals user-error when no workspaces are registered."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--read-known-workspace "Pick: ") :type 'user-error)))

(ert-deftest claude-repl-test-read-known-workspace-defaults-to-current ()
  "read-known-workspace defaults to the current workspace when registered."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
    (let ((captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws2"))
                ((symbol-function 'completing-read)
                 (lambda (_p _c _pr _r _h _hv default)
                   (setq captured-default default)
                   default)))
        (claude-repl--read-known-workspace "Pick: ")
        (should (equal captured-default "ws2"))))))

(ert-deftest claude-repl-test-read-known-workspace-no-default-when-current-not-registered ()
  "read-known-workspace passes nil default when current workspace is not registered."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((captured-default 'sentinel))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "stranger"))
                ((symbol-function 'completing-read)
                 (lambda (_p _c _pr _r _h _hv default)
                   (setq captured-default default)
                   "ws1")))
        (claude-repl--read-known-workspace "Pick: ")
        (should-not captured-default)))))

;;;; ---- Tests: claude-repl--write-output-json ----

(ert-deftest claude-repl-test-write-output-json-creates-file ()
  "write-output-json should write JSON content to the output directory."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir))
    (unwind-protect
        (let ((file (claude-repl--write-output-json "test.json" '((key . "value")))))
          (should (file-exists-p file))
          (should (string= (expand-file-name "test.json" tmpdir) file))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (json-read-from-string (buffer-string))))
              (should (equal (alist-get 'key data) "value")))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-write-output-json-creates-directory ()
  "write-output-json should create the output directory if it does not exist."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (subdir (expand-file-name "nested/dir" tmpdir))
         (claude-repl--output-dir subdir))
    (unwind-protect
        (progn
          (should-not (file-directory-p subdir))
          (claude-repl--write-output-json "test.json" '((a . 1)))
          (should (file-directory-p subdir))
          (should (file-exists-p (expand-file-name "test.json" subdir))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-write-output-json-returns-full-path ()
  "write-output-json should return the full path of the written file."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir))
    (unwind-protect
        (let ((result (claude-repl--write-output-json "out.json" [1 2 3])))
          (should (stringp result))
          (should (string= result (expand-file-name "out.json" tmpdir))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: claude-repl--list-claude-vterm-buffers ----

(ert-deftest claude-repl-test-list-claude-vterm-buffers-filters ()
  "list-claude-vterm-buffers should return only buffers matching the vterm pattern."
  (let ((buf1 (get-buffer-create "*claude-panel-abcd1234*"))
        (buf2 (get-buffer-create "*not-claude*"))
        (buf3 (get-buffer-create "*claude-panel-11223344*")))
    (unwind-protect
        (let ((result (claude-repl--list-claude-vterm-buffers)))
          (should (memq buf1 result))
          (should (memq buf3 result))
          (should-not (memq buf2 result)))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest claude-repl-test-list-claude-vterm-buffers-empty ()
  "list-claude-vterm-buffers should return nil when no matching buffers exist."
  ;; Ensure no stray claude buffers
  (dolist (buf (buffer-list))
    (when (string-match-p claude-repl--vterm-buffer-re (buffer-name buf))
      (kill-buffer buf)))
  (should-not (claude-repl--list-claude-vterm-buffers)))

;;;; ---- Tests: claude-repl--send-digit-char ----

(ert-deftest claude-repl-test-send-digit-char-extracts-last-key ()
  "send-digit-char should extract the last key from the key sequence and send it."
  (let ((sent-char nil))
    (cl-letf (((symbol-function 'claude-repl-send-char)
               (lambda (ch) (setq sent-char ch)))
              ((symbol-function 'this-command-keys-vector)
               (lambda () [32 111 51])))  ;; SPC o 3
      (claude-repl--send-digit-char)
      (should (equal sent-char "3")))))

(ert-deftest claude-repl-test-send-digit-char-zero ()
  "send-digit-char should handle digit 0 correctly."
  (let ((sent-char nil))
    (cl-letf (((symbol-function 'claude-repl-send-char)
               (lambda (ch) (setq sent-char ch)))
              ((symbol-function 'this-command-keys-vector)
               (lambda () [32 111 48])))  ;; SPC o 0
      (claude-repl--send-digit-char)
      (should (equal sent-char "0")))))

(ert-deftest claude-repl-test-send-digit-char-nine ()
  "send-digit-char should handle digit 9 correctly."
  (let ((sent-char nil))
    (cl-letf (((symbol-function 'claude-repl-send-char)
               (lambda (ch) (setq sent-char ch)))
              ((symbol-function 'this-command-keys-vector)
               (lambda () [32 111 57])))  ;; SPC o 9
      (claude-repl--send-digit-char)
      (should (equal sent-char "9")))))

;;;; ---- Tests: claude-repl-paste-to-vterm ----

(ert-deftest claude-repl-test-paste-to-vterm-when-live ()
  "paste-to-vterm should forward Ctrl-V to the vterm buffer when live."
  (let ((sent-args nil))
    (claude-repl-test--with-clean-state
      (claude-repl-test--with-temp-buffer "*claude-panel-aabbccdd*"
        (cl-letf (((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-get)
                   (lambda (ws key)
                     (when (and (equal ws "test-ws") (eq key :vterm-buffer))
                       (current-buffer))))
                  ((symbol-function 'vterm-send-key)
                   (lambda (&rest args) (setq sent-args args))))
          (claude-repl-paste-to-vterm)
          (should (equal sent-args '("v" nil nil t))))))))

(ert-deftest claude-repl-test-paste-to-vterm-when-not-live ()
  "paste-to-vterm should do nothing when vterm is not live."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--vterm-live-p) (lambda () nil))
              ((symbol-function 'vterm-send-key)
               (lambda (&rest _) (setq sent t))))
      (claude-repl-paste-to-vterm)
      (should-not sent))))

;;;; ---- Tests: claude-repl-set-priority ----

(ert-deftest claude-repl-test-set-priority-stores-value ()
  "set-priority should store the priority in workspace state."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (claude-repl-set-priority "p1")
      (should (equal (claude-repl--ws-get "ws1" :priority) "p1")))))

(ert-deftest claude-repl-test-set-priority-clears-on-empty ()
  "set-priority with empty string should clear the priority."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (claude-repl-set-priority "p2")
      (should (equal (claude-repl--ws-get "ws1" :priority) "p2"))
      (claude-repl-set-priority "")
      (should-not (claude-repl--ws-get "ws1" :priority)))))

(ert-deftest claude-repl-test-set-priority-messages ()
  "set-priority should display a message with the new priority."
  (claude-repl-test--with-clean-state
    (let ((msg nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message) (lambda (fmt &rest args)
                                              (setq msg (apply #'format fmt args)))))
        (claude-repl-set-priority "p3")
        (should (string-match-p "p3" msg))
        (claude-repl-set-priority "")
        (should (string-match-p "cleared" msg))))))

;;;; ---- Tests: claude-repl-revert-and-eval-buffer ----

(ert-deftest claude-repl-test-revert-and-eval-buffer ()
  "revert-and-eval-buffer should call revert-buffer then eval-buffer."
  (let ((call-order nil))
    (cl-letf (((symbol-function 'revert-buffer)
               (lambda (&rest _) (push 'revert call-order)))
              ((symbol-function 'eval-buffer)
               (lambda (&rest _) (push 'eval call-order))))
      (claude-repl-revert-and-eval-buffer)
      ;; Order is reversed because we use push
      (should (equal call-order '(eval revert))))))

;;;; ---- Tests: claude-repl--kill-owned-panel-buffers ----

(ert-deftest claude-repl-test-kill-owned-panel-buffers-kills-matching ()
  "kill-owned-panel-buffers should kill panel buffers owned by the specified workspace."
  (let ((buf1 (get-buffer-create "*claude-panel-aabb0011*"))
        (buf2 (get-buffer-create "*claude-panel-input-aabb0011*"))
        (buf3 (get-buffer-create "*claude-panel-ccdd2233*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (setq-local claude-repl--owning-workspace "target-ws"))
          (with-current-buffer buf2
            (setq-local claude-repl--owning-workspace "target-ws"))
          (with-current-buffer buf3
            (setq-local claude-repl--owning-workspace "other-ws"))
          (claude-repl--kill-owned-panel-buffers "target-ws")
          ;; Buffers owned by target-ws should be killed
          (should-not (buffer-live-p buf1))
          (should-not (buffer-live-p buf2))
          ;; Buffer owned by other-ws should survive
          (should (buffer-live-p buf3)))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (when (buffer-live-p buf3) (kill-buffer buf3)))))

(ert-deftest claude-repl-test-kill-owned-panel-buffers-ignores-non-panel ()
  "kill-owned-panel-buffers should not kill non-panel buffers even if they have owning-workspace set."
  (let ((buf (get-buffer-create "*not-a-panel*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local claude-repl--owning-workspace "target-ws"))
          (claude-repl--kill-owned-panel-buffers "target-ws")
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-kill-owned-panel-buffers-no-match ()
  "kill-owned-panel-buffers should not kill anything when no buffers match."
  (let ((initial-count (length (buffer-list))))
    (claude-repl--kill-owned-panel-buffers "nonexistent-ws")
    ;; Buffer count should not decrease (no kills happened)
    (should (>= (length (buffer-list)) initial-count))))

(ert-deftest claude-repl-test-kill-owned-panel-buffers-silences-process ()
  "kill-owned-panel-buffers should silence process query before killing."
  (let ((buf (get-buffer-create "*claude-panel-99887766*")))
    (unwind-protect
        (let ((proc (start-process "test-proc" buf "cat")))
          (set-process-query-on-exit-flag proc t)
          (with-current-buffer buf
            (setq-local claude-repl--owning-workspace "target-ws"))
          ;; Verify the flag is set before calling the function
          (should (process-query-on-exit-flag proc))
          (claude-repl--kill-owned-panel-buffers "target-ws")
          ;; Buffer should have been killed (and process silenced first)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: claude-repl-debug/obliterate ----

(ert-deftest claude-repl-test-obliterate-removes-state ()
  "obliterate should remove all workspace state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status :thinking)
    (claude-repl--ws-put "ws1" :priority "p1")
    (should (claude-repl--ws-get "ws1" :status))
    (cl-letf (((symbol-function 'claude-repl--kill-owned-panel-buffers)
               (lambda (_ws) nil)))
      (claude-repl-debug/obliterate "ws1"))
    (should-not (claude-repl--ws-get "ws1" :status))
    (should-not (claude-repl--ws-get "ws1" :priority))))

;;;; ---- Tests: claude-repl-debug/clear-state ----

(ert-deftest claude-repl-test-debug-clear-state ()
  "clear-state should clear all state types for a workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl-debug/clear-state "ws1")
    (should-not (claude-repl--ws-state "ws1"))))

;;;; ---- Tests: claude-repl-debug/toggle-logging ----

(ert-deftest claude-repl-test-toggle-logging-on-off ()
  "toggle-logging without prefix should toggle between nil and t."
  (let ((claude-repl-debug nil))
    (claude-repl-debug/toggle-logging nil)
    (should (eq claude-repl-debug t))
    (claude-repl-debug/toggle-logging nil)
    (should (eq claude-repl-debug nil))))

(ert-deftest claude-repl-test-toggle-logging-verbose ()
  "toggle-logging with prefix should toggle verbose mode."
  (let ((claude-repl-debug nil))
    (claude-repl-debug/toggle-logging t)
    (should (eq claude-repl-debug 'verbose))
    (claude-repl-debug/toggle-logging t)
    (should (eq claude-repl-debug nil))))

(ert-deftest claude-repl-test-toggle-logging-verbose-from-t ()
  "toggle-logging with prefix from t should set verbose."
  (let ((claude-repl-debug t))
    ;; Non-nil but not 'verbose, so verbose branch: (if (eq ... 'verbose) nil 'verbose)
    (claude-repl-debug/toggle-logging t)
    (should (eq claude-repl-debug 'verbose))))

;;;; ---- Tests: claude-repl-debug/toggle-metaprompt ----

(ert-deftest claude-repl-test-toggle-metaprompt ()
  "toggle-metaprompt should flip claude-repl-skip-permissions."
  (let ((claude-repl-skip-permissions nil))
    (claude-repl-debug/toggle-metaprompt)
    (should claude-repl-skip-permissions)
    (claude-repl-debug/toggle-metaprompt)
    (should-not claude-repl-skip-permissions)))

;;;; ---- Tests: claude-repl-debug/prefix-counter ----

(ert-deftest claude-repl-test-prefix-counter-message ()
  "prefix-counter should report counter, period, and sends until next metaprompt."
  (claude-repl-test--with-clean-state
    (let ((msg nil)
          (claude-repl-prefix-period 5))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message) (lambda (fmt &rest args)
                                              (setq msg (apply #'format fmt args)))))
        ;; No counter set yet -- should default to 0
        (claude-repl-debug/prefix-counter)
        (should (string-match-p "ws1" msg))
        (should (string-match-p "counter: 0" msg))
        (should (string-match-p "period: 5" msg))
        (should (string-match-p "next metaprompt in: 5" msg))))))

(ert-deftest claude-repl-test-prefix-counter-with-existing-count ()
  "prefix-counter should compute remaining sends correctly."
  (claude-repl-test--with-clean-state
    (let ((msg nil)
          (claude-repl-prefix-period 7))
      (claude-repl--ws-put "ws1" :prefix-counter 10)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message) (lambda (fmt &rest args)
                                              (setq msg (apply #'format fmt args)))))
        (claude-repl-debug/prefix-counter)
        ;; 10 mod 7 = 3, so 7 - 3 = 4 sends until next
        (should (string-match-p "counter: 10" msg))
        (should (string-match-p "next metaprompt in: 4" msg))))))

;;;; ---- Tests: claude-repl-debug/--format-diagnostics ----

(ert-deftest claude-repl-test-format-diagnostics-full ()
  "format-diagnostics should include all diagnostic fields."
  (claude-repl-test--with-temp-buffer "*claude-panel-aabb0011*"
    (let* ((diag (list :vterm-buf (current-buffer)
                       :proc-alive t
                       :owning-ws "my-ws"
                       :has-window t
                       :claude-open t
                       :dirty nil))
           (result (claude-repl-debug/--format-diagnostics "ws1" diag :thinking :done)))
      (should (string-match-p "ws1" result))
      (should (string-match-p "\\*claude-panel-aabb0011\\*" result))
      (should (string-match-p "process=alive" result))
      (should (string-match-p "owning-ws=my-ws" result))
      (should (string-match-p "has-window=yes" result))
      (should (string-match-p "claude-open=yes" result))
      (should (string-match-p "dirty=no" result))
      (should (string-match-p ":thinking -> :done" result)))))

(ert-deftest claude-repl-test-format-diagnostics-nil-values ()
  "format-diagnostics should handle nil values gracefully."
  (let* ((diag (list :vterm-buf nil
                     :proc-alive nil
                     :owning-ws nil
                     :has-window nil
                     :claude-open nil
                     :dirty nil))
         (result (claude-repl-debug/--format-diagnostics "ws1" diag nil nil)))
    (should (string-match-p "process=dead/nil" result))
    (should (string-match-p "owning-ws=nil" result))
    (should (string-match-p "has-window=no" result))
    (should (string-match-p "claude-open=no" result))
    (should (string-match-p "dirty=no" result))
    (should (string-match-p "nil -> nil" result))))

(ert-deftest claude-repl-test-format-diagnostics-dirty ()
  "format-diagnostics should show dirty=yes when dirty is non-nil."
  (let* ((diag (list :vterm-buf nil :proc-alive nil :owning-ws nil
                     :has-window nil :claude-open nil :dirty t))
         (result (claude-repl-debug/--format-diagnostics "ws1" diag nil nil)))
    (should (string-match-p "dirty=yes" result))))

;;;; ---- Tests: claude-repl-debug/--apply-state-refresh ----

(ert-deftest claude-repl-test-apply-state-refresh-claude-open ()
  "apply-state-refresh with claude-open should call update-ws-state."
  (let ((updated nil))
    (cl-letf (((symbol-function 'claude-repl--update-ws-state)
               (lambda (ws) (setq updated ws))))
      (claude-repl-debug/--apply-state-refresh "ws1" t)
      (should (equal updated "ws1")))))

(ert-deftest claude-repl-test-apply-state-refresh-not-open-clears-non-thinking ()
  "apply-state-refresh with claude not open should clear non-thinking states."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl-debug/--apply-state-refresh "ws1" nil)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-apply-state-refresh-not-open-clears-thinking ()
  "apply-state-refresh with claude not open clears :thinking.
The underlying vterm is gone, so no hook will ever fire to clear it
naturally.  mark-dead-vterm clears :claude-state regardless of prior
value and writes :repl-state :dead."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl-debug/--apply-state-refresh "ws1" nil)
    (should-not (claude-repl--ws-claude-state "ws1"))
    (should (eq (claude-repl--ws-repl-state "ws1") :dead))))

(ert-deftest claude-repl-test-apply-state-refresh-not-open-no-state ()
  "apply-state-refresh with claude not open and no state should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl-debug/--apply-state-refresh "ws1" nil)
    (should-not (claude-repl--ws-state "ws1"))))

;;;; ---- Tests: claude-repl-debug/mock-workspace-generation ----

(ert-deftest claude-repl-test-mock-workspace-generation-default ()
  "mock-workspace-generation should write a default mock file."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir))
    (unwind-protect
        (progn
          (claude-repl-debug/mock-workspace-generation)
          (let ((file (expand-file-name "workspace_generation.json" tmpdir)))
            (should (file-exists-p file))
            (with-temp-buffer
              (insert-file-contents file)
              (let ((data (json-read-from-string (buffer-string))))
                (should (vectorp data))
                (should (equal (aref data 0) "DWC/mock-test"))))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-mock-workspace-generation-custom-names ()
  "mock-workspace-generation should accept custom branch names."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir))
    (unwind-protect
        (progn
          (claude-repl-debug/mock-workspace-generation '("branch-a" "branch-b"))
          (let ((file (expand-file-name "workspace_generation.json" tmpdir)))
            (with-temp-buffer
              (insert-file-contents file)
              (let ((data (json-read-from-string (buffer-string))))
                (should (equal (length data) 2))
                (should (equal (aref data 0) "branch-a"))
                (should (equal (aref data 1) "branch-b"))))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: claude-repl-debug/process-pending-commands ----

(ert-deftest claude-repl-test-process-pending-commands-no-dir ()
  "process-pending-commands should message when output dir does not exist."
  (let ((msg nil)
        (claude-repl--output-dir "/nonexistent/path/"))
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args)
                                            (setq msg (apply #'format fmt args)))))
      (claude-repl-debug/process-pending-commands)
      (should (string-match-p "No workspace_commands" msg)))))

(ert-deftest claude-repl-test-process-pending-commands-processes-files ()
  "process-pending-commands should process all matching JSON files."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir)
         (processed nil))
    (unwind-protect
        (progn
          ;; Create matching files
          (with-temp-file (expand-file-name "workspace_commands_001.json" tmpdir)
            (insert "[]"))
          (with-temp-file (expand-file-name "workspace_commands_002.json" tmpdir)
            (insert "[]"))
          ;; Create a non-matching file
          (with-temp-file (expand-file-name "other_file.json" tmpdir)
            (insert "[]"))
          (cl-letf (((symbol-function 'claude-repl--process-workspace-commands-file)
                     (lambda (file) (push file processed))))
            (claude-repl-debug/process-pending-commands)
            (should (= (length processed) 2))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: claude-repl-debug/workspace-states ----

(ert-deftest claude-repl-test-debug-workspace-states ()
  "workspace-states should message all workspace states."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :thinking)
    (let ((msg nil))
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args)
                                              (setq msg (apply #'format fmt args)))))
        (claude-repl-debug/workspace-states)
        (should (string-match-p "test-ws" msg))
        (should (string-match-p "thinking" msg))))))

;;;; ---- Tests: claude-repl-debug/buffer-info ----

(ert-deftest claude-repl-test-debug-buffer-info-with-buffers ()
  "buffer-info should display info for all claude vterm buffers."
  (let ((buf (get-buffer-create "*claude-panel-aabb0011*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local claude-repl--owning-workspace "ws1"))
          (let ((msg nil))
            (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
                       (lambda (_) "ws1"))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq msg (apply #'format fmt args)))))
              (claude-repl-debug/buffer-info)
              (should (string-match-p "\\*claude-panel-aabb0011\\*" msg))
              (should (string-match-p "owning=ws1" msg)))))
      (kill-buffer buf))))

(ert-deftest claude-repl-test-debug-buffer-info-no-buffers ()
  "buffer-info should show (none) when no claude buffers exist."
  ;; Clean up any stray claude buffers
  (dolist (buf (buffer-list))
    (when (string-match-p claude-repl--vterm-buffer-re (buffer-name buf))
      (kill-buffer buf)))
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (claude-repl-debug/buffer-info)
      (should (string-match-p "(none)" msg)))))

;;;; ---- Tests: claude-repl-debug/workspace-clean-p ----

(ert-deftest claude-repl-test-debug-workspace-clean-p ()
  "workspace-clean-p should report clean or dirty."
  (let ((msg nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (setq msg (apply #'format fmt args)))))
      (claude-repl-debug/workspace-clean-p "ws1")
      (should (string-match-p "clean" msg)))))

(ert-deftest claude-repl-test-debug-workspace-dirty ()
  "workspace-clean-p should show dirty when workspace has changes."
  (let ((msg nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) nil))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (setq msg (apply #'format fmt args)))))
      (claude-repl-debug/workspace-clean-p "ws1")
      (should (string-match-p "dirty" msg)))))

;;;; ---- Tests: claude-repl--output-dir constant ----

(ert-deftest claude-repl-test-output-dir-is-absolute ()
  "output-dir should be an absolute path under ~/.claude/output/."
  (should (file-name-absolute-p claude-repl--output-dir))
  (should (string-match-p "output/$" claude-repl--output-dir)))

;;;; ---- Tests: format-buffer-info with owning set but persp nil ----

(ert-deftest claude-repl-test-format-buffer-info-owning-set-persp-nil ()
  "format-buffer-info should show owning workspace value and persp=nil when persp is nil."
  (claude-repl-test--with-temp-buffer "*claude-panel-ff001122*"
    (setq-local claude-repl--owning-workspace "my-ws")
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
               (lambda (_buf) nil)))
      (let ((result (claude-repl--format-buffer-info (current-buffer))))
        (should (string-match-p "owning=my-ws" result))
        (should (string-match-p "persp=nil" result))))))

;;;; ---- Tests: list-claude-vterm-buffers excludes input buffers ----

(ert-deftest claude-repl-test-list-claude-vterm-buffers-excludes-input ()
  "list-claude-vterm-buffers should not include input buffers (only vterm pattern)."
  (let ((vterm-buf (get-buffer-create "*claude-panel-aabb1122*"))
        (input-buf (get-buffer-create "*claude-panel-input-aabb1122*")))
    (unwind-protect
        (let ((result (claude-repl--list-claude-vterm-buffers)))
          (should (memq vterm-buf result))
          (should-not (memq input-buf result)))
      (kill-buffer vterm-buf)
      (kill-buffer input-buf))))

;;;; ---- Tests: list-claude-vterm-buffers excludes killed buffers ----

(ert-deftest claude-repl-test-list-claude-vterm-buffers-excludes-killed ()
  "list-claude-vterm-buffers should not include killed/dead buffers."
  (let ((buf (get-buffer-create "*claude-panel-dead0001*")))
    (kill-buffer buf)
    ;; After killing, buffer-list should not contain buf, so result should not either
    (let ((result (claude-repl--list-claude-vterm-buffers)))
      (should-not (memq buf result)))))

;;;; ---- Tests: send-digit-char with non-digit last key ----

(ert-deftest claude-repl-test-send-digit-char-non-digit ()
  "send-digit-char should send whatever character the last key maps to, even if non-digit."
  (let ((sent-char nil))
    (cl-letf (((symbol-function 'claude-repl-send-char)
               (lambda (ch) (setq sent-char ch)))
              ((symbol-function 'this-command-keys-vector)
               (lambda () [32 111 97])))  ;; SPC o a (97 = ?a)
      (claude-repl--send-digit-char)
      (should (equal sent-char "a")))))

;;;; ---- Tests: mock-workspace-commands-with-priority (interactive) ----

(ert-deftest claude-repl-test-mock-workspace-commands-with-priority ()
  "mock-workspace-commands-with-priority should write a JSON file with priority and name."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir)
         (msg nil))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _coll &rest _) "p2"))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &rest _) "DWC/test-branch"))
                  ((symbol-function 'format-time-string)
                   (lambda (_fmt) "1234567890"))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (claude-repl-debug/mock-workspace-commands-with-priority)
          (let ((file (expand-file-name "workspace_commands_1234567890.json" tmpdir)))
            (should (file-exists-p file))
            (with-temp-buffer
              (insert-file-contents file)
              (let* ((data (json-read-from-string (buffer-string)))
                     (entry (aref data 0)))
                (should (equal (alist-get 'type entry) "create"))
                (should (equal (alist-get 'name entry) "DWC/test-branch"))
                (should (equal (alist-get 'priority entry) "p2"))))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: process-pending-commands with empty directory ----

(ert-deftest claude-repl-test-process-pending-commands-empty-dir ()
  "process-pending-commands should message 'No workspace_commands' when dir exists but has no matching files."
  (let* ((tmpdir (make-temp-file "claude-repl-test-" t))
         (claude-repl--output-dir tmpdir)
         (msg nil))
    (unwind-protect
        (progn
          ;; Create a non-matching file so the directory is not empty
          (with-temp-file (expand-file-name "unrelated.json" tmpdir)
            (insert "[]"))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
            (claude-repl-debug/process-pending-commands)
            (should (string-match-p "No workspace_commands" msg))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: workspace-states with empty workspace list ----

(ert-deftest claude-repl-test-debug-workspace-states-empty ()
  "workspace-states should handle an empty workspace list gracefully."
  (claude-repl-test--with-clean-state
    (let ((msg nil))
      (cl-letf (((symbol-function '+workspace-list-names) (lambda () nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (claude-repl-debug/workspace-states)
        (should (string-match-p "Workspace states:" msg))))))

;;;; ---- Tests: clear-state on workspace with no state (no-op) ----

(ert-deftest claude-repl-test-debug-clear-state-no-state ()
  "clear-state on a workspace with no state should be a no-op without errors."
  (claude-repl-test--with-clean-state
    ;; "ws-empty" has never had any state set
    (claude-repl-debug/clear-state "ws-empty")
    (should-not (claude-repl--ws-state "ws-empty"))))

;;;; ---- Tests: kill-owned-panel-buffers closes window before killing ----

(ert-deftest claude-repl-test-kill-owned-panel-buffers-closes-window ()
  "kill-owned-panel-buffers should close the buffer's window before killing the buffer."
  (let ((buf (get-buffer-create "*claude-panel-a1b2c3d4*"))
        (window-deleted nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local claude-repl--owning-workspace "target-ws"))
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (_buf &optional _all-frames) 'fake-window))
                    ((symbol-function 'delete-window)
                     (lambda (win)
                       (when (eq win 'fake-window)
                         (setq window-deleted t)))))
            (claude-repl--kill-owned-panel-buffers "target-ws")
            (should window-deleted)
            (should-not (buffer-live-p buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: set-owning-workspace (interactive) ----

(ert-deftest claude-repl-test-debug-set-owning-workspace ()
  "set-owning-workspace should set the owning workspace on the selected buffer."
  (let ((buf (get-buffer-create "*claude-panel-owntest01*")))
    (unwind-protect
        (let ((call-count 0))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt coll &rest _)
                       (setq call-count (1+ call-count))
                       (if (= call-count 1)
                           ;; First call: select buffer
                           (buffer-name buf)
                         ;; Second call: select workspace
                         "new-owner")))
                    ((symbol-function 'claude-repl--list-claude-vterm-buffers)
                     (lambda () (list buf)))
                    ((symbol-function '+workspace-list-names)
                     (lambda () '("new-owner" "other-ws"))))
            (claude-repl-debug/set-owning-workspace)
            (should (equal (buffer-local-value 'claude-repl--owning-workspace buf)
                           "new-owner"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: --gather-ws-diagnostics: all fields populated ----

(ert-deftest claude-repl-test-gather-ws-diagnostics-all-populated ()
  "gather-ws-diagnostics should return all fields populated when persp has a vterm buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-diagfull*"
      (setq-local claude-repl--owning-workspace "ws1")
      (let ((test-buf (current-buffer)))
        (cl-letf (((symbol-function 'claude-repl--ws-claude-open-p) (lambda (_) t))
                  ((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) nil))
                  ((symbol-function 'persp-get-by-name) (lambda (_) [fake-persp]))
                  ((symbol-function 'persp-buffers) (lambda (_) (list test-buf)))
                  ((symbol-function 'claude-repl--claude-buffer-p)
                   (lambda (&optional buf) (eq (or buf (current-buffer)) test-buf)))
                  ((symbol-function 'get-buffer-process) (lambda (_) 'fake-proc))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'get-buffer-window) (lambda (_buf &optional _) 'fake-win)))
          (let ((diag (claude-repl-debug/--gather-ws-diagnostics "ws1")))
            (should (eq (plist-get diag :vterm-buf) test-buf))
            (should (eq (plist-get diag :proc-alive) t))
            (should (equal (plist-get diag :owning-ws) "ws1"))
            (should (eq (plist-get diag :has-window) 'fake-win))
            (should (eq (plist-get diag :claude-open) t))
            (should (eq (plist-get diag :dirty) t))))))))

;;;; ---- Tests: --gather-ws-diagnostics: no persp found ----

(ert-deftest claude-repl-test-gather-ws-diagnostics-no-persp ()
  "gather-ws-diagnostics should return nil for buffer-related fields when no persp is found."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-claude-open-p) (lambda (_) nil))
              ((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t))
              ((symbol-function 'persp-get-by-name) (lambda (_) nil)))
      (let ((diag (claude-repl-debug/--gather-ws-diagnostics "nonexistent")))
        (should-not (plist-get diag :vterm-buf))
        (should-not (plist-get diag :proc-alive))
        (should-not (plist-get diag :owning-ws))
        (should-not (plist-get diag :has-window))
        (should-not (plist-get diag :claude-open))
        (should-not (plist-get diag :dirty))))))

;;;; ---- Tests: --gather-ws-diagnostics: persp is a symbol ----

(ert-deftest claude-repl-test-gather-ws-diagnostics-persp-is-symbol ()
  "gather-ws-diagnostics should return nil for buffer fields when persp is a symbol."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-claude-open-p) (lambda (_) nil))
              ((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t))
              ((symbol-function 'persp-get-by-name) (lambda (_) 'none)))
      (let ((diag (claude-repl-debug/--gather-ws-diagnostics "ws1")))
        ;; When persp is a symbol, (not (symbolp persp)) is nil, so persp-bufs is nil
        (should-not (plist-get diag :vterm-buf))
        (should-not (plist-get diag :proc-alive))
        (should-not (plist-get diag :owning-ws))
        (should-not (plist-get diag :has-window))))))

;;;; ---- Tests: --apply-state-refresh: not open, :permission clears ----

(ert-deftest claude-repl-test-apply-state-refresh-not-open-clears-permission ()
  "apply-state-refresh with claude not open should clear :permission state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (claude-repl-debug/--apply-state-refresh "ws1" nil)
    (should-not (claude-repl--ws-state "ws1"))))

;;;; ---- Tests: --apply-state-refresh: not open, :inactive clears ----

(ert-deftest claude-repl-test-apply-state-refresh-not-open-clears-inactive ()
  "apply-state-refresh with claude not open should clear :inactive state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (claude-repl-debug/--apply-state-refresh "ws1" nil)
    (should-not (claude-repl--ws-state "ws1"))))

;;;; ---- Tests: refresh-state full integration ----

(ert-deftest claude-repl-test-refresh-state-integration ()
  "refresh-state should gather diagnostics, apply state refresh, and format a message."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (let ((msg nil))
      (cl-letf (((symbol-function 'claude-repl-debug/--gather-ws-diagnostics)
                 (lambda (_ws)
                   (list :vterm-buf nil :proc-alive nil :owning-ws nil
                         :has-window nil :claude-open nil :dirty nil)))
                ((symbol-function 'claude-repl-debug/--apply-state-refresh)
                 (lambda (ws _open)
                   ;; Simulate clearing state (mirroring non-open + :done behavior)
                   (claude-repl--ws-claude-state-clear-if ws :done)))
                ((symbol-function 'claude-repl-debug/--format-diagnostics)
                 (lambda (ws _diag before after)
                   (format "diag: %s %s->%s" ws before after)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (claude-repl-debug/refresh-state "ws1")
        (should (string-match-p "diag: ws1" msg))
        (should (string-match-p ":done->nil" msg))))))

;;;; ---- Tests: claude-repl-debug/dump-workspace ----

(ert-deftest claude-repl-test-dump-workspace-no-workspaces ()
  "dump-workspace signals user-error when hashmap is empty."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl-debug/dump-workspace) :type 'user-error)))

(ert-deftest claude-repl-test-dump-workspace-shows-status ()
  "dump-workspace displays the workspace status in the output buffer."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "my-ws" :project-dir "/tmp/my-ws")
    (claude-repl--ws-put "my-ws" :status :thinking)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "my-ws")))
      (claude-repl-debug/dump-workspace)
      (with-current-buffer "*claude-repl-dump*"
        (let ((content (buffer-string)))
          (should (string-match-p "my-ws" content))
          (should (string-match-p ":status" content))
          (should (string-match-p ":thinking" content))
          (should (string-match-p ":project-dir" content))
          (should (string-match-p "/tmp/my-ws" content))))
      (kill-buffer "*claude-repl-dump*"))))

(ert-deftest claude-repl-test-dump-workspace-shows-buffer-summary ()
  "dump-workspace shows live/dead status for buffer values."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-dump-buf*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) "ws1")))
              (claude-repl-debug/dump-workspace)
              (with-current-buffer "*claude-repl-dump*"
                (let ((content (buffer-string)))
                  (should (string-match-p ":vterm-buffer" content))
                  (should (string-match-p "live" content))))
              (kill-buffer "*claude-repl-dump*")))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-test-dump-workspace-shows-nil-values ()
  "dump-workspace handles nil and missing values in the plist."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "ws1")))
      (claude-repl-debug/dump-workspace)
      (with-current-buffer "*claude-repl-dump*"
        (let ((content (buffer-string)))
          (should (string-match-p "ws1" content))
          (should (string-match-p ":project-dir" content))))
      (kill-buffer "*claude-repl-dump*"))))

;;;; ---- Tests: debug/cancel-timers (moved from core.el) ----

(ert-deftest claude-repl-test-debug-cancel-timers-calls-cancel ()
  "debug/cancel-timers should call `claude-repl--cancel-all-timers'."
  (let ((cancel-called nil)
        (claude-repl--timers nil))
    (cl-letf (((symbol-function 'claude-repl--cancel-all-timers)
               (lambda () (setq cancel-called t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (claude-repl-debug/cancel-timers)
      (should cancel-called))))

(ert-deftest claude-repl-test-debug-cancel-timers-emits-message ()
  "debug/cancel-timers should emit a message."
  (let ((msg-text nil)
        (claude-repl--timers nil))
    (cl-letf (((symbol-function 'claude-repl--cancel-all-timers) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest _args) (setq msg-text fmt))))
      (claude-repl-debug/cancel-timers)
      (should (stringp msg-text))
      (should (string-match-p "cancel" (downcase msg-text))))))

(ert-deftest claude-repl-test-debug-cancel-timers-no-timers ()
  "debug/cancel-timers should work when no timers are active."
  (let ((claude-repl--timers nil))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (claude-repl-debug/cancel-timers)
      (should (null claude-repl--timers)))))

;;; test-keybindings.el ends here
