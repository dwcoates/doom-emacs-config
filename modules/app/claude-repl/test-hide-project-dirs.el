;;; test-hide-project-dirs.el --- ERT tests for hide-project-dirs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the project-dir-based workspace hiding toggle.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-hide-project-dirs.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defun claude-repl-hide-project-dirs-test--register (ws dir)
  "Register WS in `claude-repl--workspaces' with `:project-dir' DIR."
  (puthash ws (list :project-dir dir) claude-repl--workspaces))

(defmacro claude-repl-hide-project-dirs-test--with-prefixes (prefixes &rest body)
  "Run BODY with `claude-repl-hide-project-dirs' temporarily set to PREFIXES.
Also disables the toggle by default so individual tests can enable it
locally."
  (declare (indent 1))
  `(let ((claude-repl-hide-project-dirs ,prefixes)
         (claude-repl-hide-project-dirs-enabled nil))
     ,@body))

;;;; ---- Predicate: --ws-matches-p ----

(ert-deftest claude-repl-test-hide-project-dirs-matches-direct ()
  "ws-matches-p is non-nil when project-dir IS the prefix itself."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (claude-repl-hide-project-dirs-test--register
       "ws-cc" (expand-file-name "~/workspace/ChessCom"))
      (should (claude-repl--hide-project-dirs--ws-matches-p "ws-cc")))))

(ert-deftest claude-repl-test-hide-project-dirs-matches-nested ()
  "ws-matches-p is non-nil for a nested directory under the prefix."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (claude-repl-hide-project-dirs-test--register
       "ws-deep" (expand-file-name "~/workspace/ChessCom/services/api"))
      (should (claude-repl--hide-project-dirs--ws-matches-p "ws-deep")))))

(ert-deftest claude-repl-test-hide-project-dirs-does-not-match-sibling ()
  "ws-matches-p is nil for a sibling dir whose name shares the prefix's leading chars."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (claude-repl-hide-project-dirs-test--register
       "ws-sibling" (expand-file-name "~/workspace/ChessCom-archive"))
      (should-not (claude-repl--hide-project-dirs--ws-matches-p "ws-sibling")))))

(ert-deftest claude-repl-test-hide-project-dirs-does-not-match-unrelated ()
  "ws-matches-p is nil for a workspace whose project-dir lives elsewhere."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (claude-repl-hide-project-dirs-test--register
       "ws-other" (expand-file-name "~/code/some-other-project"))
      (should-not (claude-repl--hide-project-dirs--ws-matches-p "ws-other")))))

(ert-deftest claude-repl-test-hide-project-dirs-ignores-missing-project-dir ()
  "ws-matches-p returns nil when the workspace has no `:project-dir'."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (puthash "ws-no-dir" (list :priority "p1") claude-repl--workspaces)
      (should-not (claude-repl--hide-project-dirs--ws-matches-p "ws-no-dir")))))

(ert-deftest claude-repl-test-hide-project-dirs-matches-second-prefix ()
  "ws-matches-p hits any prefix in the configured list, not just the first."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/Other")
              (expand-file-name "~/workspace/ChessCom"))
      (claude-repl-hide-project-dirs-test--register
       "ws-cc" (expand-file-name "~/workspace/ChessCom/foo"))
      (should (claude-repl--hide-project-dirs--ws-matches-p "ws-cc")))))

;;;; ---- Filter: --filter-hide-project-dir-names ----

(ert-deftest claude-repl-test-filter-hide-project-dir-names-passthrough-when-off ()
  "filter returns NAMES unchanged when the toggle is off, even with matches."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (claude-repl-hide-project-dirs-test--register
       "ws-cc" (expand-file-name "~/workspace/ChessCom/foo"))
      (claude-repl-hide-project-dirs-test--register
       "ws-other" (expand-file-name "~/code/foo"))
      (should (equal (claude-repl--filter-hide-project-dir-names
                      '("ws-cc" "ws-other") "ws-other")
                     '("ws-cc" "ws-other"))))))

(ert-deftest claude-repl-test-filter-hide-project-dir-names-drops-matches-when-on ()
  "filter drops every non-current workspace whose project-dir lives under a prefix."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (let ((claude-repl-hide-project-dirs-enabled t))
        (claude-repl-hide-project-dirs-test--register
         "ws-cc1" (expand-file-name "~/workspace/ChessCom/a"))
        (claude-repl-hide-project-dirs-test--register
         "ws-cc2" (expand-file-name "~/workspace/ChessCom/b"))
        (claude-repl-hide-project-dirs-test--register
         "ws-other" (expand-file-name "~/code/x"))
        (should (equal (claude-repl--filter-hide-project-dir-names
                        '("ws-cc1" "ws-cc2" "ws-other") "ws-other")
                       '("ws-other")))))))

(ert-deftest claude-repl-test-filter-hide-project-dir-names-keeps-current-even-if-match ()
  "filter always retains the current workspace even when it matches a prefix."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (let ((claude-repl-hide-project-dirs-enabled t))
        (claude-repl-hide-project-dirs-test--register
         "ws-cc" (expand-file-name "~/workspace/ChessCom/a"))
        (claude-repl-hide-project-dirs-test--register
         "ws-other" (expand-file-name "~/code/x"))
        (should (equal (claude-repl--filter-hide-project-dir-names
                        '("ws-cc" "ws-other") "ws-cc")
                       '("ws-cc" "ws-other")))))))

(ert-deftest claude-repl-test-filter-hide-project-dir-names-keeps-no-project-dir ()
  "filter keeps workspaces without `:project-dir' — they can't be classified."
  (claude-repl-test--with-clean-state
    (claude-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (let ((claude-repl-hide-project-dirs-enabled t))
        (puthash "ws-no-dir" (list :priority "p1") claude-repl--workspaces)
        (claude-repl-hide-project-dirs-test--register
         "ws-cc" (expand-file-name "~/workspace/ChessCom/a"))
        (should (equal (claude-repl--filter-hide-project-dir-names
                        '("ws-no-dir" "ws-cc") "ws-no-dir")
                       '("ws-no-dir")))))))

;;;; ---- Toggle command ----

(ert-deftest claude-repl-test-toggle-hide-project-dirs-flips-flag ()
  "toggle flips the flag and triggers tab-bar + drawer refresh."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-project-dirs-enabled nil)
          (redraw-called 0)
          (refresh-called 0))
      (cl-letf (((symbol-function 'claude-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-called)))
                ((symbol-function 'claude-repl-drawer--refresh-if-visible)
                 (lambda () (cl-incf refresh-called))))
        (claude-repl-toggle-hide-project-dirs)
        (should claude-repl-hide-project-dirs-enabled)
        (should (= redraw-called 1))
        (should (= refresh-called 1))
        (claude-repl-toggle-hide-project-dirs)
        (should-not claude-repl-hide-project-dirs-enabled)
        (should (= redraw-called 2))
        (should (= refresh-called 2))))))

(provide 'test-hide-project-dirs)
;;; test-hide-project-dirs.el ends here
