;;; test-merge-handlers.el --- Tests for merge-handlers.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the repo-routed `/workspace-merge' dispatch layer.
;; Covers:
;;   - `claude-repl--read-merge-handler-config-file' (file parsing)
;;   - `claude-repl--lookup-merge-handler-override' (defcustom lookup)
;;   - `claude-repl--resolve-merge-handler' (precedence + fallbacks)
;;   - `claude-repl--dispatch-merge-handler' (registry invocation)
;;   - `claude-repl--merge-handler-cherry-pick' (default handler)
;;   - `claude-repl--register-merge-handler' (registry mutation)

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixture helpers ----

(defmacro claude-repl-test--with-temp-repo (var &rest body)
  "Bind VAR to a fresh temp directory acting as a repo root, run BODY, then clean.
The directory is created with `make-temp-file' (deleted on exit)."
  (declare (indent 1))
  `(let ((,var (make-temp-file "claude-repl-test-repo-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun claude-repl-test--seed-merge-config (repo-root content)
  "Write CONTENT (string) to REPO-ROOT's `.claude-repl/workspace-merge.eld'."
  (let* ((dir (expand-file-name ".claude-repl" repo-root))
         (path (expand-file-name "workspace-merge.eld" dir)))
    (make-directory dir t)
    (with-temp-file path (insert content))
    path))

(defmacro claude-repl-test--with-clean-registry (&rest body)
  "Run BODY with a registry containing only `cherry-pick'.
Restores the prior registry on exit so tests don't bleed."
  (declare (indent 0))
  `(let ((claude-repl--merge-handler-registry
          (list (cons 'cherry-pick
                      #'claude-repl--merge-handler-cherry-pick)))
         (claude-repl-workspace-merge-handler-overrides nil))
     ,@body))

;;;; ---- Tests: read-merge-handler-config-file ----

(ert-deftest claude-repl-test-merge-config-reads-valid-alist ()
  "A well-formed alist file is parsed into its content."
  (claude-repl-test--with-temp-repo root
    (claude-repl-test--seed-merge-config
     root "((handler . create-pr) (args . (:add-to-merge-queue t)))")
    (let ((config (claude-repl--read-merge-handler-config-file root)))
      (should (equal (alist-get 'handler config) 'create-pr))
      (should (equal (alist-get 'args config)
                     '(:add-to-merge-queue t))))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-missing ()
  "A repo with no .claude-repl/workspace-merge.eld file returns nil."
  (claude-repl-test--with-temp-repo root
    (should-not (claude-repl--read-merge-handler-config-file root))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-malformed ()
  "A file containing unreadable junk returns nil (does not raise)."
  (claude-repl-test--with-temp-repo root
    (claude-repl-test--seed-merge-config root "this is (((not valid sexp")
    (should-not (claude-repl--read-merge-handler-config-file root))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-not-alist ()
  "A file whose top-level sexp is not a cons returns nil."
  (claude-repl-test--with-temp-repo root
    (claude-repl-test--seed-merge-config root "42")
    (should-not (claude-repl--read-merge-handler-config-file root))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-repo-root-nil ()
  "A nil repo-root short-circuits to nil without touching the filesystem."
  (should-not (claude-repl--read-merge-handler-config-file nil)))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-repo-missing ()
  "A repo-root pointing at a nonexistent directory returns nil."
  (should-not (claude-repl--read-merge-handler-config-file
               "/nonexistent/claude-repl-test/repo")))

;;;; ---- Tests: lookup-merge-handler-override ----

(ert-deftest claude-repl-test-merge-override-finds-exact-path ()
  "An override entry matching the canonical path is returned."
  (claude-repl-test--with-temp-repo root
    (let ((claude-repl-workspace-merge-handler-overrides
           (list (cons root '((handler . noop))))))
      (let ((config (claude-repl--lookup-merge-handler-override root)))
        (should (equal (alist-get 'handler config) 'noop))))))

(ert-deftest claude-repl-test-merge-override-matches-trailing-slash ()
  "A path with trailing slash matches a canonical entry without one."
  (claude-repl-test--with-temp-repo root
    (let* ((with-slash (file-name-as-directory root))
           (claude-repl-workspace-merge-handler-overrides
            (list (cons root '((handler . noop))))))
      (should (claude-repl--lookup-merge-handler-override with-slash)))))

(ert-deftest claude-repl-test-merge-override-returns-nil-no-match ()
  "No matching override returns nil."
  (claude-repl-test--with-temp-repo root
    (let ((claude-repl-workspace-merge-handler-overrides
           '(("/some/other/repo" . ((handler . noop))))))
      (should-not (claude-repl--lookup-merge-handler-override root)))))

(ert-deftest claude-repl-test-merge-override-returns-nil-when-repo-root-nil ()
  "A nil repo-root short-circuits."
  (let ((claude-repl-workspace-merge-handler-overrides
         '(("/some/repo" . ((handler . noop))))))
    (should-not (claude-repl--lookup-merge-handler-override nil))))

;;;; ---- Tests: resolve-merge-handler ----

(ert-deftest claude-repl-test-resolve-falls-back-to-cherry-pick ()
  "With no config anywhere, resolver returns `(cherry-pick . nil)'."
  (claude-repl-test--with-clean-registry
    (claude-repl-test--with-temp-repo root
      (should (equal (claude-repl--resolve-merge-handler root)
                     '(cherry-pick))))))

(ert-deftest claude-repl-test-resolve-uses-eld-when-present ()
  "Resolver returns the .eld-declared handler symbol + args."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (claude-repl-test--with-temp-repo root
      (claude-repl-test--seed-merge-config
       root "((handler . noop) (args . (:silent t)))")
      (let ((res (claude-repl--resolve-merge-handler root)))
        (should (eq (car res) 'noop))
        (should (equal (cdr res) '(:silent t)))))))

(ert-deftest claude-repl-test-resolve-uses-override-when-no-eld ()
  "With no .eld, the defcustom override supplies the handler."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (claude-repl-test--with-temp-repo root
      (let ((claude-repl-workspace-merge-handler-overrides
             (list (cons root '((handler . noop) (args . (:k v)))))))
        (let ((res (claude-repl--resolve-merge-handler root)))
          (should (eq (car res) 'noop))
          (should (equal (cdr res) '(:k v))))))))

(ert-deftest claude-repl-test-resolve-eld-wins-over-override ()
  "When both .eld and defcustom override are set, .eld wins."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (claude-repl-test--with-temp-repo root
      (claude-repl-test--seed-merge-config root "((handler . noop))")
      (let ((claude-repl-workspace-merge-handler-overrides
             (list (cons root '((handler . cherry-pick))))))
        (should (eq (car (claude-repl--resolve-merge-handler root))
                    'noop))))))

(ert-deftest claude-repl-test-resolve-falls-back-on-unknown-symbol ()
  "An unknown handler symbol falls back to cherry-pick (does not raise)."
  (claude-repl-test--with-clean-registry
    (claude-repl-test--with-temp-repo root
      (claude-repl-test--seed-merge-config
       root "((handler . totally-fake-handler))")
      (should (eq (car (claude-repl--resolve-merge-handler root))
                  'cherry-pick)))))

;;;; ---- Tests: dispatch-merge-handler ----

(ert-deftest claude-repl-test-dispatch-invokes-resolved-handler ()
  "Dispatcher invokes the resolved handler with TARGET-WS + ARGS."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (claude-repl-test--with-temp-repo root
        (claude-repl-test--seed-merge-config
         root "((handler . noop) (args . (:hello world)))")
        (claude-repl--dispatch-merge-handler "DWC/foo" root)
        (should (equal (plist-get captured :ws) "DWC/foo"))
        (should (equal (plist-get captured :args) '(:hello world)))))))

(ert-deftest claude-repl-test-dispatch-falls-back-to-cherry-pick ()
  "With no config, dispatcher invokes the registered cherry-pick handler."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (ws silent auto-resolve)
                   (setq captured (list :ws ws :silent silent
                                        :auto-resolve auto-resolve)))))
        (claude-repl-test--with-temp-repo root
          (claude-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal (plist-get captured :ws) "DWC/foo"))
          (should (eq (plist-get captured :silent) t))
          (should (eq (plist-get captured :auto-resolve) t)))))))

(ert-deftest claude-repl-test-dispatch-errors-on-missing-registry-entry ()
  "Dispatcher signals user-error if resolved symbol has no fn (registry empty)."
  (let ((claude-repl--merge-handler-registry nil)
        (claude-repl-workspace-merge-handler-overrides nil))
    (claude-repl-test--with-temp-repo root
      (should-error
       (claude-repl--dispatch-merge-handler "DWC/foo" root)
       :type 'user-error))))

;;;; ---- Tests: register-merge-handler ----

(ert-deftest claude-repl-test-register-replaces-existing-entry ()
  "Re-registering a symbol replaces the old function rather than duplicating."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) 'first))
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) 'second))
    (let ((matches (cl-remove-if-not
                    (lambda (entry) (eq (car entry) 'noop))
                    claude-repl--merge-handler-registry)))
      (should (= (length matches) 1))
      (should (eq (funcall (cdr (car matches))) 'second)))))

;;;; ---- Tests: cherry-pick handler ----

(ert-deftest claude-repl-test-cherry-pick-handler-calls-merge-into-source ()
  "The cherry-pick handler invokes --workspace-merge-into-source with silent + auto-resolve."
  (let ((captured nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
               (lambda (ws silent auto-resolve)
                 (setq captured (list ws silent auto-resolve)))))
      (claude-repl--merge-handler-cherry-pick "DWC/foo")
      (should (equal captured '("DWC/foo" t t))))))

(provide 'test-merge-handlers)

;;; test-merge-handlers.el ends here
