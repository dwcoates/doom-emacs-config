;;; test-config.el --- Tests for agent-repl config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the pre-`core.el' surface defined in `config.el': the
;; loaded-version SHA and the early-boundary wrapper
;; `agent-repl--early-git-string' it reads through.  That code runs at the
;; top of config.el (before any module file is `require'd) and must
;; therefore not depend on any other agent-repl module having loaded.
;;
;; The orphan-cherry-pick recovery that used to live here is GONE with the
;; rest of Emacs's merge ownership: the daemon runs merges, so there is no
;; Emacs-side cherry-pick left for a hard kill to orphan.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load shared stubs first so `config.el' can be loaded in -Q.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

;; `config.el' calls `agent-repl--load-module' for every sub-file; stub it
;; to a no-op so we get the early defuns without re-loading the full module.
;;
;; This was previously spelled `(unless (fboundp 'load!) (defmacro load! ...))',
;; which NEVER fired: test-helpers.el (loaded above) already defines `load!'
;; as a real loader.  So this load re-loaded every production sub-module and
;; re-`defun'-ed their external-boundary wrappers, DISARMING the guards
;; test-helpers.el had installed — for the rest of the batch session, and
;; therefore for every test file the aggregate loads after this one.  That is
;; how `agent-repl-test-daemon-stop-deletes-and-clears' came to HTTP-GET the
;; developer's real `claude-repld'.  The guards are re-armed below regardless,
;; since `config.el' itself defines the `--early-git-string' wrapper.
(cl-letf (((symbol-function 'message) #'ignore)
          ((symbol-function 'agent-repl--load-module) (lambda (&rest _args) nil)))
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; config.el stays at the module root; this suite lives in `lisp/'.
    (load (expand-file-name "../config.el" dir) nil t)))

;; Re-arm the boundary guards over the wrappers this load re-`defun'-ed.
(when noninteractive
  (agent-repl-test--reinstall-external-guards))

;;;; ---- Tests: loaded-version SHA ----

(ert-deftest agent-repl-config-test-version/defvar-defaults-nil ()
  "`agent-repl--version' is declared (the batch load leaves it nil since
the refresh `setq' is gated behind `noninteractive')."
  (should (boundp 'agent-repl--version)))

(ert-deftest agent-repl-config-test-compute-version/returns-trimmed-sha ()
  "`--compute-version' returns the SHA produced by the early-git wrapper."
  (let ((agent-repl--config-file "/tmp/doom/modules/app/agent-repl/config.el"))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest _args) "deadbeefcafef00d")))
      (should (equal (agent-repl--compute-version) "deadbeefcafef00d")))))

(ert-deftest agent-repl-config-test-compute-version/passes-config-dir-to-git ()
  "`--compute-version' runs `rev-parse HEAD' in the config file's directory
so a linked worktree reports its own SHA."
  (let ((agent-repl--config-file "/tmp/doom/modules/app/agent-repl/config.el")
        (captured nil))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest args) (setq captured args) "abc123")))
      (agent-repl--compute-version)
      (should (equal captured
                     '("-C" "/tmp/doom/modules/app/agent-repl/"
                       "rev-parse" "HEAD"))))))

(ert-deftest agent-repl-config-test-compute-version/empty-sha-is-nil ()
  "An empty string from git (not a repo, etc.) maps to nil, not \"\"."
  (let ((agent-repl--config-file "/tmp/doom/modules/app/agent-repl/config.el"))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest _args) "")))
      (should (null (agent-repl--compute-version))))))

(ert-deftest agent-repl-config-test-compute-version/nil-config-file-is-nil ()
  "When the config-file path is unknown, `--compute-version' returns nil
without shelling out to git."
  (let ((agent-repl--config-file nil)
        (git-called nil))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest _args) (setq git-called t) "abc")))
      (should (null (agent-repl--compute-version)))
      (should-not git-called))))

(ert-deftest agent-repl-config-test-version-command/messages-and-returns-sha ()
  "`agent-repl-version' messages and returns the cached SHA."
  (let ((agent-repl--version "feedface1234")
        (messaged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq messaged (apply #'format fmt args)))))
      (should (equal (agent-repl-version) "feedface1234"))
      (should (equal messaged "agent-repl version: feedface1234")))))

(ert-deftest agent-repl-config-test-version-command/unknown-when-nil ()
  "`agent-repl-version' reports the \"unknown\" sentinel when the cached
SHA is nil."
  (let ((agent-repl--version nil)
        (messaged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq messaged (apply #'format fmt args)))))
      (should (equal (agent-repl-version) "unknown"))
      (should (equal messaged "agent-repl version: unknown")))))

;;;; ---- Tests: bootstrap-phase emission ----
;;
;; config.el runs before core.el defines the log-severity ladder, and is also
;; the code that reports core.el failing to load.  `--boot-info' / `--boot-warn'
;; must therefore hold the quiet/loud bifurcation on BOTH sides of that
;; boundary: delegating to the ladder once it exists, and degrading to a
;; correctly-pitched bare `message' when it does not.
;;
;; Note this file loads config.el with `load!' stubbed out, so core.el is
;; genuinely absent here — the fallback branch is the default state, and the
;; delegating branch is the one that must be simulated.

(defun agent-repl-test-config--capture-emission (thunk)
  "Run THUNK with `message' stubbed; return a plist (:text T :echoed BOOL).
:echoed is non-nil only when `inhibit-message' was nil at `message' time,
i.e. only when the line actually reached the echo area / modeline."
  (let ((text nil) (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq text (apply #'format fmt args)
                       echoed (not inhibit-message)))))
      (funcall thunk))
    (list :text text :echoed echoed)))

(ert-deftest agent-repl-config-test-boot-info/fallback-never-echoes ()
  "Pre-core, `--boot-info' still emits but must NOT reach the echo area."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-info "starting up")))))
    (should (string-match-p "\\[agent-repl\\] starting up" (plist-get res :text)))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-config-test-boot-info/fallback-expands-format-args ()
  "Pre-core, `--boot-info' expands its &rest ARGS into FMT."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-info "loaded %d of %d" 3 7)))))
    (should (string-match-p "loaded 3 of 7" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-info/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-info' routes through it."
  (let ((delegated nil))
    (cl-letf (((symbol-function 'agent-repl--info)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (apply #'format fmt args))))))
      (agent-repl--boot-info "hello %s" "world")
      (should (equal delegated '(nil "hello world"))))))

(ert-deftest agent-repl-config-test-boot-warn/fallback-reaches-echo-area ()
  "Pre-core (ladder undefined), `--boot-warn' MUST still reach the echo area —
core.el failing to load breaks the whole logging system, which is exactly the
genuine fatal condition the user has to see.  The harness loads core.el, so
`agent-repl--warn' is unbound here to force the true fallback branch."
  (let ((orig (symbol-function 'agent-repl--warn)))
    (unwind-protect
        (progn
          (fmakunbound 'agent-repl--warn)
          (let ((res (agent-repl-test-config--capture-emission
                      (lambda () (agent-repl--boot-warn "core.el exploded")))))
            (should (plist-get res :echoed))
            (should (string-match-p "WARNING: core.el exploded" (plist-get res :text)))))
      (fset 'agent-repl--warn orig))))

(ert-deftest agent-repl-config-test-boot-warn/delegated-is-quiet ()
  "Post-core, `--boot-warn' delegates to the now-quiet `agent-repl--warn', so a
delegated boot-warning is recorded but must NOT reach the echo area / modeline."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-warn "recoverable %s" "hiccup")))))
    (should-not (plist-get res :echoed))
    (should (string-match-p "WARNING: recoverable hiccup" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-warn/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-warn' routes through it."
  (let ((delegated nil))
    (cl-letf (((symbol-function 'agent-repl--warn)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (apply #'format fmt args))))))
      (agent-repl--boot-warn "bad %s" "thing")
      (should (equal delegated '(nil "bad thing"))))))

(ert-deftest agent-repl-config-test-boundary-guards-survive-the-config-reload ()
  "This file's `config.el' load leaves the external-boundary guards armed.
Regression guard: the load used to re-`defun' the production wrappers and
disarm every guard for the rest of the batch session, so tests in files
loaded after this one silently reached the real `git' / `gh' / daemon."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--early-git-string "rev-parse" "HEAD")
                :type 'error)
  (should-error (agent-repl--uds-probe "/tmp/agent-repl-probe.sock")
                :type 'error))

(provide 'test-config)

;;; test-config.el ends here
