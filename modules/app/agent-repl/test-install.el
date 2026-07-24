;;; test-install.el --- ERT tests for install.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Emacs wrapper around .claude/install.sh.  Covers the
;; sandbox-detection predicate, the installed-state predicate against
;; synthetic settings.json fixtures, and the dispatch of interactive
;; commands through to the bash script (mocked).
;;
;; Bash-script integration coverage (fresh install, foreign keys, etc.)
;; lives in a later commit; here we stick to unit-level behavior of the
;; elisp surface.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-install.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- sandbox-p ----

(ert-deftest agent-repl-test-in-sandbox-dockerenv ()
  "in-sandbox-p returns t when /.dockerenv exists."
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (path) (equal path "/.dockerenv")))
            ((symbol-function 'getenv) (lambda (_) nil)))
    (should (agent-repl--in-sandbox-p))))

(ert-deftest agent-repl-test-in-sandbox-env-var ()
  "in-sandbox-p returns t when DOOM_SANDBOX=1."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv)
             (lambda (k) (and (equal k "DOOM_SANDBOX") "1"))))
    (should (agent-repl--in-sandbox-p))))

(ert-deftest agent-repl-test-in-sandbox-neither ()
  "in-sandbox-p returns nil when neither signal is present."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv) (lambda (_) nil)))
    (should-not (agent-repl--in-sandbox-p))))

(ert-deftest agent-repl-test-in-sandbox-env-other-value ()
  "DOOM_SANDBOX set to a non-1 value does NOT trigger sandbox mode."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv)
             (lambda (k) (and (equal k "DOOM_SANDBOX") "0"))))
    (should-not (agent-repl--in-sandbox-p))))

;; Every settings.json hook-writer test is gone with the writer itself (the
;; D-phase census): Emacs provisions no agent-harness hooks for ANY backend,
;; so there is no managed-hooks alist, no hooks-installed-p predicate, and no
;; hook-array writer left to cover.  What install.el still does — skill
;; symlink provisioning, the git pre-commit hook, the doctor — is covered
;; below.

;;;; ---- run-install-action dispatch ----

(ert-deftest agent-repl-test-install-action-sandbox-noop ()
  "install/uninstall/reinstall no-op when sandbox is detected."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (&rest _) (setq called t) '(0 ""))))
      (agent-repl--run-install-action "install")
      (should-not called))))

(ert-deftest agent-repl-test-install-action-passes-through ()
  "When not in sandbox, the action arg is forwarded to the script."
  (let ((received-arg nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (action) (setq received-arg action) '(0 "ok\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil)))
      (agent-repl--run-install-action "reinstall")
      (should (equal received-arg "reinstall")))))

(ert-deftest agent-repl-test-install-action-nonzero-exit-errors ()
  "Non-zero exit surfaces the output buffer and signals an error."
  (let ((buffer-shown nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'display-buffer)
               (lambda (_) (setq buffer-shown t))))
      (should-error (agent-repl--run-install-action "install"))
      (should buffer-shown))))

(ert-deftest agent-repl-test-install-action-quiet-nonzero-no-window ()
  "Quiet non-zero exit does NOT pop the output window."
  (let ((buffer-shown nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) (setq buffer-shown t))))
      (should-error (agent-repl--run-install-action "install" t))
      (should-not buffer-shown))))

(ert-deftest agent-repl-test-install-action-quiet-nonzero-logs ()
  "Quiet non-zero exit routes the script output to `agent-repl--log'."
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest args) (setq logged args))))
      (should-error (agent-repl--run-install-action "install" t))
      (should logged))))

(ert-deftest agent-repl-test-install-action-quiet-nonzero-still-errors ()
  "Quiet non-zero exit still signals an error so surfacing is preserved."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--run-install-script)
             (lambda (_) '(2 "boom\n")))
            ((symbol-function 'agent-repl--surface-install-output)
             (lambda (_) nil))
            ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
    (should-error (agent-repl--run-install-action "install" t))))

(ert-deftest agent-repl-test-install-action-surface-output ()
  "Zero-exit run pipes script output to the output buffer."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--run-install-script)
             (lambda (_) '(0 "hello from script\n"))))
    (agent-repl--run-install-action "install")
    (with-current-buffer agent-repl--install-output-buffer
      (should (string-match-p "hello from script"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

;;;; ---- Interactive commands ----

(ert-deftest agent-repl-test-install-hooks-calls-install ()
  "`agent-repl-install-hooks' dispatches the `install' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'agent-repl--run-install-action)
               (lambda (a) (setq action a))))
      (agent-repl-install-hooks)
      (should (equal action "install")))))

(ert-deftest agent-repl-test-uninstall-hooks-calls-uninstall ()
  "`agent-repl-uninstall-hooks' dispatches the `uninstall' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'agent-repl--run-install-action)
               (lambda (a) (setq action a))))
      (agent-repl-uninstall-hooks)
      (should (equal action "uninstall")))))

(ert-deftest agent-repl-test-reinstall-hooks-calls-reinstall ()
  "`agent-repl-reinstall-hooks' dispatches the `reinstall' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'agent-repl--run-install-action)
               (lambda (a) (setq action a))))
      (agent-repl-reinstall-hooks)
      (should (equal action "reinstall")))))

;;;; ---- Script location sanity ----

(ert-deftest agent-repl-test-install-script-path-exists ()
  "The resolved install-script path points at the checked-in .claude/install.sh."
  (should (file-exists-p agent-repl--install-script))
  (should (string-match-p "/\\.claude/install\\.sh\\'"
                          agent-repl--install-script)))

;;;; ---- doctor-issues ----

(defun test-install--doctor-level-count (issues level)
  "Return the number of entries in ISSUES whose level is LEVEL."
  (cl-count-if (lambda (i) (eq (car i) level)) issues))

(defun test-install--doctor-find (issues substring)
  "Return the first issue whose message contains SUBSTRING, else nil."
  (cl-find-if (lambda (i) (string-match-p substring (cdr i))) issues))

(ert-deftest agent-repl-test-doctor-sandbox-returns-nil ()
  "In sandbox, doctor-issues is a no-op."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t)))
    (should-not (agent-repl--doctor-issues))))

(ert-deftest agent-repl-test-doctor-all-skills-present-no-issues ()
  "With every managed skill link healthy and no stale links, doctor is clean."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--check-skill-links)
             (lambda (_issues) nil))
            ((symbol-function 'agent-repl--check-unmanaged-broken-links)
             (lambda (_issues) nil)))
    (should-not (agent-repl--doctor-issues))))

;; The settings.json / hook-registration / hook-script doctor tests
;; (missing-settings, missing-notification-warns, script-missing/not-
;; executable/drift, script-drift-p, settings-skip-short-circuit) were
;; deleted in the S8/S9 sentinel endgame: doctor-issues no longer inspects
;; Claude Code hook registrations or scripts — only managed skill symlinks.

;;;; ---- Bash-integration tests: intentionally not present ----
;;
;; Earlier revisions of this file invoked `bash .claude/install.sh' via
;; `call-process' under an isolated HOME tmpdir to assert end-to-end
;; installer behavior (fresh install, idempotency, foreign-entry
;; preservation, uninstall, sandbox short-circuit, etc.).
;;
;; Those tests have been removed because per AGENTS.md "No External
;; Processes or External State in Tests" the ERT suite covers ELISP
;; code only.  When the production artifact under test is a non-elisp
;; binary (a shell script, here), there is nothing meaningful for an
;; elisp test to assert without invoking that binary — at which point
;; the test is an integration test of an external program, not a unit
;; test of lisp code.  AGENTS.md explicitly disallows integration
;; tests inside the ERT suite.
;;
;; The bash installer is still covered, but outside ERT: install.sh's
;; behavior is exercised manually and by the install/uninstall flow
;; in any project that consumes the hook.  If automated coverage is
;; needed later, it lives in a dedicated CI job invoked separately
;; from the ERT batch run.

;;;; ---- maybe-install-hooks ----

(ert-deftest agent-repl-test-maybe-install-runs-when-issues ()
  "maybe-install-hooks invokes install when doctor reports issues."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should called))))

(ert-deftest agent-repl-test-maybe-install-passes-quiet ()
  "maybe-install-hooks dispatches the install action with QUIET non-nil.
The quiet flag is what routes a failed auto-install to the log instead of
popping the `*agent-repl-install*' window on every reload."
  (let ((received-args nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest args) (setq received-args args)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should (equal (nth 0 received-args) "install"))
      (should (nth 1 received-args)))))

(ert-deftest agent-repl-test-maybe-install-skips-when-clean ()
  "maybe-install-hooks no-ops when doctor reports no issues."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should-not called))))

(ert-deftest agent-repl-test-maybe-install-skips-in-sandbox ()
  "maybe-install-hooks no-ops in sandbox even when issues would be reported."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should-not called))))

(ert-deftest agent-repl-test-maybe-install-skips-when-disabled ()
  "maybe-install-hooks no-ops when the custom flag is nil."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks nil))
      (agent-repl--maybe-install-hooks)
      (should-not called))))

(ert-deftest agent-repl-test-maybe-install-swallows-error ()
  "Errors from the install action are caught, not propagated to startup."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--doctor-issues)
             (lambda () '((error . "missing"))))
            ((symbol-function 'agent-repl--run-install-action)
             (lambda (&rest _) (error "boom")))
            ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
            (noninteractive nil)
            (agent-repl-auto-install-hooks t))
    ;; Must not propagate the error — return value is irrelevant.
    (agent-repl--maybe-install-hooks)
    (should t)))

(ert-deftest agent-repl-test-maybe-install-logs-caught-error ()
  "A caught install-action error is surfaced via `agent-repl--log'."
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest _) (setq logged t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should logged))))

(ert-deftest agent-repl-test-maybe-install-not-on-startup-hook ()
  "Auto-install must NOT be deferred to `emacs-startup-hook' — later
agent-repl sub-modules depend on hooks being registered at load time."
  (should-not (memq 'agent-repl--maybe-install-hooks emacs-startup-hook)))

;;;; ---- skill-link helpers ----

(defun test-install--make-skills-tmp ()
  "Build a temp-dir trio (SRC LOCAL-SRC . DEST) for skill-link tests.
Creates SRC/<managed-skill-names> and LOCAL-SRC/<managed-local-skill-names>
as real directories so the symlink targets resolve, and DEST as an empty
directory ready for links."
  (let* ((root (make-temp-file "agent-repl-skill-test-" t))
         (src (expand-file-name "src/" root))
         (local-src (expand-file-name "local-src/" root))
         (dest (expand-file-name "dest/" root)))
    (make-directory src t)
    (make-directory local-src t)
    (make-directory dest t)
    (dolist (name agent-repl--managed-skills)
      (make-directory (expand-file-name name src) t))
    (dolist (name agent-repl--managed-local-skills)
      (make-directory (expand-file-name name local-src) t))
    (list src local-src dest)))

(defmacro test-install--with-skill-dirs (bindings &rest body)
  "Run BODY with SRC/LOCAL-SRC/DEST dirs materialized and defcustoms
pointed at them.
BINDINGS is ignored — provided so future test helpers can extend."
  (declare (indent 1))
  (ignore bindings)
  `(let* ((trio (test-install--make-skills-tmp))
          (src (nth 0 trio))
          (local-src (nth 1 trio))
          (dest (nth 2 trio)))
     (let ((agent-repl-skills-src-dir src)
           (agent-repl-local-skills-src-dir local-src)
           (agent-repl--skills-dest-dir dest))
       (unwind-protect (progn ,@body)
         (delete-directory (file-name-directory (directory-file-name src)) t)))))

(defun test-install--link-all-skills ()
  "Create symlinks at the test DEST for every managed (external + local) skill.
Use after `test-install--with-skill-dirs' has set up the temp dirs and
the defcustoms.  Returns the list of dests created."
  (let (created)
    (dolist (name agent-repl--managed-skills)
      (let ((d (agent-repl--skill-dest-path name)))
        (make-symbolic-link (agent-repl--skill-src-path name) d)
        (push d created)))
    (dolist (name agent-repl--managed-local-skills)
      (let ((d (agent-repl--skill-dest-path name)))
        (make-symbolic-link
         (agent-repl--skill-src-path name agent-repl-local-skills-src-dir)
         d)
        (push d created)))
    (nreverse created)))

(ert-deftest agent-repl-test-skill-link-ok-correct ()
  "skill-link-ok-p returns t when dest is a symlink to the expected src."
  (test-install--with-skill-dirs ()
    (let ((name (car agent-repl--managed-skills)))
      (make-symbolic-link (agent-repl--skill-src-path name)
                          (agent-repl--skill-dest-path name))
      (should (agent-repl--skill-link-ok-p name)))))

(ert-deftest agent-repl-test-skill-link-ok-missing ()
  "skill-link-ok-p returns nil when dest does not exist at all."
  (test-install--with-skill-dirs ()
    (should-not
     (agent-repl--skill-link-ok-p (car agent-repl--managed-skills)))))

(ert-deftest agent-repl-test-skill-link-ok-foreign-target ()
  "skill-link-ok-p returns nil when dest points at something other than our src."
  (test-install--with-skill-dirs ()
    (let* ((name (car agent-repl--managed-skills))
           (dest (agent-repl--skill-dest-path name)))
      (make-symbolic-link "/tmp/elsewhere" dest)
      (should-not (agent-repl--skill-link-ok-p name)))))

(ert-deftest agent-repl-test-check-skill-links-missing ()
  "Missing symlinks produce one warn per managed skill (external + local)."
  (test-install--with-skill-dirs ()
    (let ((issues (list nil))
          (expected (+ (length agent-repl--managed-skills)
                       (length agent-repl--managed-local-skills))))
      (agent-repl--check-skill-links issues)
      (should (= (length (car issues)) expected))
      (should (cl-every (lambda (i) (eq (car i) 'warn)) (car issues))))))

(ert-deftest agent-repl-test-check-skill-links-all-ok ()
  "All skills linked correctly (external + local) produces no issues."
  (test-install--with-skill-dirs ()
    (test-install--link-all-skills)
    (let ((issues (list nil)))
      (agent-repl--check-skill-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-check-skill-links-foreign ()
  "A foreign file at one dest path is flagged as `points elsewhere'."
  (test-install--with-skill-dirs ()
    (let ((name (car agent-repl--managed-skills)))
      (write-region "" nil (agent-repl--skill-dest-path name))
      (let ((issues (list nil)))
        (agent-repl--check-skill-links issues)
        (should (= 1 (length
                      (cl-remove-if-not
                       (lambda (i) (string-match-p "points elsewhere"
                                                    (cdr i)))
                       (car issues)))))))))

;;;; ---- local-skill specific tests ----

(ert-deftest agent-repl-test-managed-local-skills-nonempty ()
  "Repo-local skills list must include `runtime-eval-code' (regression guard).
debug-logs is deliberately NOT in the list — it is project-scoped via
the checked-in `<repo>/.claude/skills/debug-logs' symlink."
  (should (member "runtime-eval-code" agent-repl--managed-local-skills))
  (should-not (member "debug-logs" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-local-skills-src-dir-default ()
  "Default `agent-repl-local-skills-src-dir' points at this module's
checked-in `skills/' directory when install.el is loaded from a file."
  ;; The defcustom default is computed from `load-file-name'; when
  ;; install.el was loaded normally, the path should end in the
  ;; module-local `skills/' segment.
  (when agent-repl-local-skills-src-dir
    (should (string-match-p
             "modules/app/agent-repl/skills/?$"
             (directory-file-name
              (expand-file-name agent-repl-local-skills-src-dir))))))

(ert-deftest agent-repl-test-skill-link-ok-uses-local-src ()
  "skill-link-ok-p honors the SRC-DIR argument for local skills."
  (test-install--with-skill-dirs ()
    (let ((name (car agent-repl--managed-local-skills)))
      (make-symbolic-link
       (agent-repl--skill-src-path name agent-repl-local-skills-src-dir)
       (agent-repl--skill-dest-path name))
      ;; Correct when called with the local src-dir.
      (should (agent-repl--skill-link-ok-p
               name agent-repl-local-skills-src-dir))
      ;; Wrong when called with the external src-dir (different target).
      (should-not (agent-repl--skill-link-ok-p name)))))

(ert-deftest agent-repl-test-check-skill-links-local-missing ()
  "A missing local-skill symlink contributes its own warn entry."
  (test-install--with-skill-dirs ()
    ;; Link only the external skills; leave local-skill dests missing.
    (dolist (name agent-repl--managed-skills)
      (make-symbolic-link (agent-repl--skill-src-path name)
                          (agent-repl--skill-dest-path name)))
    (let ((issues (list nil)))
      (agent-repl--check-skill-links issues)
      (should (= (length (car issues))
                 (length agent-repl--managed-local-skills)))
      (should (cl-every (lambda (i)
                          (string-match-p "Skill symlink missing" (cdr i)))
                        (car issues))))))

(ert-deftest agent-repl-test-debug-logs-skill-file-exists ()
  "The checked-in debug-logs SKILL.md must exist with required frontmatter.
Regression guard so the file is not silently moved or deleted —
`/debug-logs' depends on it being discoverable at install time."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (skill-md (expand-file-name "debug-logs/SKILL.md" src-dir)))
    (should (file-exists-p skill-md))
    (with-temp-buffer
      (insert-file-contents skill-md)
      (goto-char (point-min))
      (should (looking-at "^---\n"))
      (should (re-search-forward "^name: debug-logs$" nil t))
      (should (re-search-forward "^description: " nil t)))))

(ert-deftest agent-repl-test-managed-local-skills-includes-runtime-eval-code ()
  "Repo-local skills list must include `runtime-eval-code' (regression guard).
`/runtime-eval-code' replaces the prior `/workspace-eval' skill and owns
the JSON contract that the editor's `\"eval\"' handler dispatches against."
  (should (member "runtime-eval-code" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-local-skills-includes-emit-workspace-commands ()
  "Repo-local skills list must include `emit-workspace-commands.sh' (regression guard).
explanation-engine folded the original into `workspace/run.sh --emit-commands'
(python3-dependent, absent from the doom-sandbox image), so the doom repo
now owns the trivial uuidgen-only emitter as a repo-local skill."
  (should (member "emit-workspace-commands.sh" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-skills-excludes-emit-workspace-commands ()
  "External managed-skills list must NOT include `emit-workspace-commands.sh'.
It moved to the repo-local list, so leaving it here would re-introduce the
dangling explanation-engine canonical-impl path that broke install."
  (should-not (member "emit-workspace-commands.sh" agent-repl--managed-skills)))

(ert-deftest agent-repl-test-emit-workspace-commands-skill-file-exists ()
  "The checked-in repo-local emit-workspace-commands.sh must exist and be executable.
Regression guard so the dispatch emitter the skill run.sh wrappers exec
is not silently moved or deleted."
  (let ((emit (expand-file-name
               "emit-workspace-commands.sh"
               (or agent-repl-local-skills-src-dir
                   (error "agent-repl-local-skills-src-dir is unset")))))
    (should (file-exists-p emit))
    (should (file-executable-p emit))))

(ert-deftest agent-repl-test-runtime-eval-code-skill-file-exists ()
  "The checked-in runtime-eval-code SKILL.md must exist with required frontmatter.
Regression guard so the file is not silently moved or deleted —
`/runtime-eval-code' depends on it being discoverable at install time."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (skill-md (expand-file-name "runtime-eval-code/SKILL.md" src-dir)))
    (should (file-exists-p skill-md))
    (with-temp-buffer
      (insert-file-contents skill-md)
      (goto-char (point-min))
      (should (looking-at "^---\n"))
      (should (re-search-forward "^name: runtime-eval-code$" nil t))
      (should (re-search-forward "^description: " nil t)))))

(ert-deftest agent-repl-test-runtime-eval-code-run-sh-executable ()
  "The checked-in runtime-eval-code run.sh must exist and be executable.
Regression guard: SKILL.md Step 0 dispatches `run.sh resolve-ws' to obtain
the workspace routing key, and the dispatch step pipes JSON into the same
script. A non-executable or missing file breaks every dispatch."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (run-sh (expand-file-name "runtime-eval-code/run.sh" src-dir)))
    (should (file-exists-p run-sh))
    (should (file-executable-p run-sh))))

(ert-deftest agent-repl-test-runtime-eval-code-dispatch-resolves-sibling-through-symlinked-dir ()
  "`run.sh dispatch' must find `emit-workspace-commands.sh' even when its
install dir is a symlink into a worktree.
Regression guard for the exit-127 bug: the dispatch path used a literal
`..', which the kernel resolved THROUGH the symlinked skill dir into the
worktree (no sibling there).  This builds that exact layout and asserts
dispatch exits 0 (resolves the install-dir sibling), which fails under
the old `..' form and passes under the string-only `dirname' form."
  (skip-unless (executable-find "bash"))
  (skip-unless (executable-find "uuidgen"))
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (checked-in-run-sh (expand-file-name "runtime-eval-code/run.sh" src-dir))
         (root (make-temp-file "agent-repl-dispatch-" t))
         ;; The "worktree" the skill dir really lives in; crucially its
         ;; PARENT has no emit-workspace-commands.sh, mirroring production.
         (worktree-rec (expand-file-name "worktree/runtime-eval-code" root))
         ;; The install layout where the sibling DOES exist.
         (install (expand-file-name "install" root))
         (install-link (expand-file-name "runtime-eval-code" install))
         (install-emit (expand-file-name "emit-workspace-commands.sh" install)))
    (unwind-protect
        (progn
          (make-directory worktree-rec t)
          (make-directory install t)
          (copy-file checked-in-run-sh
                     (expand-file-name "run.sh" worktree-rec) t nil nil t)
          (with-temp-file install-emit
            (insert "#!/usr/bin/env bash\nexit 0\n"))
          (set-file-modes install-emit #o755)
          (make-symbolic-link worktree-rec install-link t)
          (should (= 0 (call-process
                        "bash" nil nil nil
                        (expand-file-name "run.sh" install-link)
                        "dispatch"))))
      (delete-directory root t))))

(ert-deftest agent-repl-test-managed-local-skills-no-workspace-eval ()
  "The legacy `workspace-eval' skill name must NOT be present.
Regression guard: it was renamed/absorbed into `runtime-eval-code'.
Leaving the old name in the managed list would create a broken symlink
on install (no source directory matches it any more)."
  (should-not (member "workspace-eval" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-local-skills-no-workspace-open ()
  "The legacy `workspace-open' skill name must NOT be present.
Regression guard: it was superseded by the `create-or-update-workspace'
skill's `open' verb (which claims `/workspace-open' as its legacy alias).
Leaving the old name in the managed list would demand a host symlink
whose repo-local source directory no longer exists."
  (should-not (member "workspace-open" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-skills-includes-build-skill ()
  "External managed-skills list must include `build-skill' (regression guard).
The skill lives at `agent-repl-skills-src-dir'/build-skill on the host;
the doctor uses this list to verify the host symlink points at it."
  (should (member "build-skill" agent-repl--managed-skills)))

(ert-deftest agent-repl-test-managed-skills-includes-workspace ()
  "External managed-skills list must include the collapsed `workspace' skill.
The per-command `workspace-*' skills were folded into `/workspace', so
the doctor verifies the single `workspace' host symlink now."
  (should (member "workspace" agent-repl--managed-skills)))

(ert-deftest agent-repl-test-managed-skills-excludes-folded-workspace-skills ()
  "The folded per-command workspace skills must NOT be managed any more.
They were absorbed into `/workspace'; leaving any in the managed list
would make the doctor demand a host symlink whose impl no longer exists."
  (dolist (name '("workspace-merge" "workspace-status"
                  "workspace-update" "generate-workspace"))
    (should-not (member name agent-repl--managed-skills))))

;;;; ---- unmanaged broken symlink detection ----

(ert-deftest agent-repl-test-check-unmanaged-broken-link-detected ()
  "A broken symlink not in the managed set produces a warn-level issue."
  (test-install--with-skill-dirs ()
    ;; Create a broken symlink for a name we don't manage.
    (make-symbolic-link "/nonexistent/workspace-eval"
                        (expand-file-name "workspace-eval" dest))
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (= 1 (length (car issues))))
      (should (eq 'warn (caar (car issues))))
      (should (string-match-p "Unmanaged broken symlink"
                              (cdar (car issues)))))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-ignores-managed ()
  "A broken symlink for a managed skill name is NOT flagged by this check.
That case is already covered by `agent-repl--check-skill-links'."
  (test-install--with-skill-dirs ()
    ;; Create a broken symlink for a managed name.
    (make-symbolic-link "/nonexistent/target"
                        (expand-file-name (car agent-repl--managed-local-skills) dest))
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-ignores-valid ()
  "A non-broken unmanaged symlink produces no issue."
  (test-install--with-skill-dirs ()
    ;; Create a valid symlink to a real directory.
    (let ((target (make-temp-file "real-target-" t)))
      (unwind-protect
          (progn
            (make-symbolic-link target
                                (expand-file-name "some-foreign-skill" dest))
            (let ((issues (list nil)))
              (agent-repl--check-unmanaged-broken-links issues)
              (should (null (car issues)))))
        (delete-directory target t)))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-empty-dir ()
  "No issues when the skills dest dir is empty."
  (test-install--with-skill-dirs ()
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-nonexistent-dir ()
  "No issues when the skills dest dir does not exist."
  (let ((agent-repl--skills-dest-dir "/tmp/this-dir-should-not-exist-12345/"))
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-doctor-includes-unmanaged-broken-links ()
  "doctor-issues surfaces unmanaged broken symlinks alongside managed checks."
  (test-install--with-skill-dirs ()
    ;; Link all managed skills correctly so those checks pass.
    (test-install--link-all-skills)
    ;; Add an unmanaged broken symlink.
    (make-symbolic-link "/nonexistent/stale-skill"
                        (expand-file-name "stale-skill" dest))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil)))
      (let ((issues (agent-repl--doctor-issues)))
        (should (test-install--doctor-find issues "Unmanaged broken symlink"))))))

;;;; ---- Tests: settings-writer (codex-shared) ----
;;
;; The alt-account config-dir provisioning tests (config-dirs-to-provision,
;; provision-registers-into-each-config-dir) were deleted in the S8/S9
;; sentinel endgame: Emacs no longer provisions Claude Code hooks into any
;; CLAUDE_CONFIG_DIR.  `agent-repl--register-hooks-in-settings' survives only
;; as the codex-shared writer and is exercised below with explicit alists.

(ert-deftest agent-repl-test-register-hooks-malformed-signals ()
  "register-hooks-in-settings signals on malformed existing JSON (never silently resets)."
  (let* ((dir (make-temp-file "agent-register-" t))
         (settings (expand-file-name "settings.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file settings (insert "{not json"))
          (should-error
           (agent-repl--register-hooks-in-settings settings '((Stop . "/x/stop.sh")))))
      (delete-directory dir t))))

;;; test-install.el ends here
