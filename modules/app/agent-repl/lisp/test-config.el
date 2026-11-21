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

(defvar agent-repl-config-test--harness-load-errors nil
  "`agent-repl--load-errors' as the harness\='s REAL load of config.el left it.
test-helpers.el, loaded just above, loads config.el and with it every
module config.el names, for real.  Captured here, before this file\='s own
stubbed reload of config.el (below) resets the accumulator.")
(setq agent-repl-config-test--harness-load-errors agent-repl--load-errors)

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
;; developer's real `agent-repld'.  The guards are re-armed below regardless,
;; since `config.el' itself defines the `--early-git-string' wrapper.
;;
;; The elisp build record is BOUND around the load: config.el resets it (as
;; it must on every real load) and re-records whatever this load reaches, so
;; the binding keeps the harness's record exactly what the harness itself
;; loaded, whatever this reload does or does not get to.
(cl-letf (((symbol-function 'message) #'ignore)
          ((symbol-function 'agent-repl--load-module) (lambda (&rest _args) nil)))
  (let ((dir (file-name-directory (or load-file-name buffer-file-name)))
        (agent-repl--elisp-module-builds agent-repl--elisp-module-builds))
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

(ert-deftest agent-repl-config-test-version/load-computes-nothing ()
  "LOADING the module must not shell out to git: the load happens before the
first frame is painted, and `git rev-parse' is a synchronous subprocess."
  (should-not agent-repl--version-computed))

(ert-deftest agent-repl-config-test-version-string/computes-on-first-use ()
  "The SHA is computed when it is first asked for, not when the file loads."
  (let ((agent-repl--version nil)
        (agent-repl--version-computed nil))
    (cl-letf (((symbol-function 'agent-repl--compute-version)
               (lambda () "cafebabe0001")))
      (should (equal (agent-repl--version-string) "cafebabe0001")))))

(ert-deftest agent-repl-config-test-version-string/computes-only-once ()
  "A repo that cannot answer is not re-probed on every call."
  (let ((agent-repl--version nil)
        (agent-repl--version-computed nil)
        (calls 0))
    (cl-letf (((symbol-function 'agent-repl--compute-version)
               (lambda () (setq calls (1+ calls)) nil)))
      (agent-repl--version-string)
      (agent-repl--version-string)
      (should (= calls 1)))))

(ert-deftest agent-repl-config-test-version-command/messages-and-returns-sha ()
  "`agent-repl-version' messages and returns the cached SHA."
  (let ((agent-repl--version "feedface1234")
        (agent-repl--version-computed t)
        (messaged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq messaged (apply #'format fmt args)))))
      (should (equal (agent-repl-version) "feedface1234"))
      (should (equal messaged "agent-repl version: feedface1234")))))

(ert-deftest agent-repl-config-test-version-command/unknown-when-nil ()
  "`agent-repl-version' reports the \"unknown\" sentinel when the cached
SHA is nil."
  (let ((agent-repl--version nil)
        (agent-repl--version-computed t)
        (messaged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq messaged (apply #'format fmt args)))))
      (should (equal (agent-repl-version) "unknown"))
      (should (equal messaged "agent-repl version: unknown")))))

(ert-deftest agent-repl-config-test-version-command/uses-central-log-scope ()
  "The process-wide version command never borrows an ambient workspace."
  ;; Arrange
  (let ((agent-repl--version "feedface1234")
        (agent-repl--version-computed t)
        logged-scope)
    (cl-letf (((symbol-function 'agent-repl--emit-log)
               (lambda (scope &rest _args) (setq logged-scope scope)))
              ((symbol-function 'message) #'ignore))
      ;; Act
      (agent-repl-version)
      ;; Assert
      (should
       (equal logged-scope
              '(:agent-repl-central
                "the loaded module version is process-wide"))))))

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
              (lambda () (agent-repl--boot-info "loaded {value} of {value_2}" :value 3 :value_2 7)))))
    (should (string-match-p "loaded 3 of 7" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-info/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-info' routes centrally through it."
  ;; Arrange.
  (let ((delegated nil)
        (agent-repl--global-log-scope :test-central-scope))
    (cl-letf (((symbol-function 'agent-repl--emit-info)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (agent-repl-log-site-render fmt args))))))
      ;; Act.
      (agent-repl--boot-info "hello {value}" :value "world")
      ;; Assert.
      (should (equal delegated '(:test-central-scope "hello world"))))))

(ert-deftest agent-repl-config-test-boot-warn/fallback-reaches-echo-area ()
  "Pre-core (ladder undefined), `--boot-warn' MUST still reach the echo area —
core.el failing to load breaks the whole logging system, which is exactly the
genuine fatal condition the user has to see.  The harness loads core.el, so
`agent-repl--emit-log-record', the ladder's record builder whose presence
`agent-repl--boot-emit' asks, is unbound here to force the true fallback
branch."
  (let ((orig (symbol-function 'agent-repl--emit-log-record)))
    (unwind-protect
        (progn
          (fmakunbound 'agent-repl--emit-log-record)
          (let ((res (agent-repl-test-config--capture-emission
                      (lambda () (agent-repl--boot-warn "core.el exploded")))))
            (should (plist-get res :echoed))
            (should (string-match-p "WARNING: core.el exploded" (plist-get res :text)))))
      (fset 'agent-repl--emit-log-record orig))))

(ert-deftest agent-repl-config-test-boot-warn/delegated-is-quiet ()
  "Post-core, `--boot-warn' delegates to the now-quiet `agent-repl--warn', so a
delegated boot-warning is recorded but must NOT reach the echo area / modeline."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-warn "recoverable {value}" :value "hiccup")))))
    (should-not (plist-get res :echoed))
    (should (string-match-p "WARNING: recoverable hiccup" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-warn/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-warn' routes centrally through it."
  ;; Arrange.
  (let ((delegated nil)
        (agent-repl--global-log-scope :test-central-scope))
    (cl-letf (((symbol-function 'agent-repl--emit-warn)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (agent-repl-log-site-render fmt args))))))
      ;; Act.
      (agent-repl--boot-warn "bad {value}" :value "thing")
      ;; Assert.
      (should (equal delegated '(:test-central-scope "bad thing"))))))

(ert-deftest agent-repl-config-test-boundary-guards-survive-the-config-reload ()
  "This file's `config.el' load leaves the external-boundary guards armed.
Regression guard: the load used to re-`defun' the production wrappers and
disarm every guard for the rest of the batch session, so tests in files
loaded after this one silently reached the real `git' / `gh' / daemon."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--early-git-string "rev-parse" "HEAD")
                :type 'error)
  (should-error (agent-repl--frontend-artifact-exists-p "list")
                :type 'error))


;;;; ---- The elisp build record ----

(defun agent-repl-config-test--write-bytes (file content)
  "Write CONTENT to FILE as its UTF-8 bytes, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string content 'utf-8))
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent))))

(defmacro agent-repl-config-test--with-root (var &rest body)
  "Bind VAR to a fresh temp module root for BODY, deleted afterwards."
  (declare (indent 1))
  `(agent-repl-test--with-temp-dir ,var "agent-repl-config-root" ,@body))

(ert-deftest agent-repl-config-test-file-sha256-hashes-the-bytes ()
  "A source's digest is the SHA-256 of its bytes on disk, non-ASCII included."
  (agent-repl-config-test--with-root root
    ;; Arrange
    (let ((file (expand-file-name "lisp/glyphs.el" root))
          (content ";; ⟢ — tree glyphs ├── └──\n"))
      (agent-repl-config-test--write-bytes file content)
      ;; Act / Assert
      (should (equal (agent-repl--elisp-file-sha256 file)
                     (secure-hash 'sha256 (encode-coding-string content 'utf-8)))))))

(ert-deftest agent-repl-config-test-file-sha256-of-an-absent-file-is-nil ()
  "An absent source has no digest."
  (agent-repl-config-test--with-root root
    ;; Arrange / Act / Assert
    (should (null (agent-repl--elisp-file-sha256 (expand-file-name "lisp/ghost.el" root))))))

(ert-deftest agent-repl-config-test-module-file-is-the-el-source ()
  "A module's source is `lisp/NAME.el' under the root, never a `.elc'."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--elisp-module-file "/root/" "core") "/root/lisp/core.el")))

(ert-deftest agent-repl-config-test-record-appends-in-load-order ()
  "Recording appends, so the record stays in load order."
  (agent-repl-config-test--with-root root
    ;; Arrange
    (agent-repl-config-test--write-bytes (expand-file-name "lisp/a.el" root) "a")
    (agent-repl-config-test--write-bytes (expand-file-name "lisp/b.el" root) "b")
    (let ((agent-repl--elisp-module-builds nil))
      ;; Act
      (agent-repl--elisp-record-module-build root "a")
      (agent-repl--elisp-record-module-build root "b")
      ;; Assert
      (should (equal (mapcar #'car agent-repl--elisp-module-builds) '("a" "b"))))))

(ert-deftest agent-repl-config-test-record-of-an-absent-module-is-nothing ()
  "A module whose file is absent records nothing."
  (agent-repl-config-test--with-root root
    ;; Arrange
    (let ((agent-repl--elisp-module-builds nil))
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--boot-emit) #'ignore))
        (agent-repl--elisp-record-module-build root "ghost"))
      ;; Assert
      (should (null agent-repl--elisp-module-builds)))))

(ert-deftest agent-repl-config-test-loader-records-the-module-it-loads ()
  "`agent-repl--load-module' records the name and digest of the file it loads."
  (agent-repl-config-test--with-root root
    ;; Arrange
    (let ((file (expand-file-name "lisp/probe.el" root)))
      (agent-repl-config-test--write-bytes file ";;; probe.el\n")
      (let ((agent-repl--elisp-module-builds nil)
            (agent-repl--load-errors nil)
            (load-file-name (expand-file-name "config.el" root)))
        ;; Act
        (cl-letf (((symbol-function 'agent-repl--boot-emit) #'ignore))
          (eval '(agent-repl--load-module "probe") t))
        ;; Assert
        (should (equal agent-repl--elisp-module-builds
                       (list (cons "probe" (agent-repl--elisp-file-sha256 file)))))))))

(ert-deftest agent-repl-config-test-loader-reports-a-module-whose-load-signals ()
  "A module whose load signals is recorded and reported loudly, never as loaded."
  (agent-repl-config-test--with-root root
    ;; Arrange
    (agent-repl-config-test--write-bytes (expand-file-name "lisp/probe.el" root)
                                         "(error \"probe boom\")\n")
    (let ((agent-repl--elisp-module-builds nil)
          (agent-repl--load-errors nil)
          (load-file-name (expand-file-name "config.el" root))
          (infos nil)
          (warns nil))
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--boot-emit)
                 (lambda (warn message)
                   (let ((text (agent-repl-log-site-render (car message) (cdr message))))
                     (if warn (push text warns) (push text infos))))))
        (eval '(agent-repl--load-module "probe") t))
      ;; Assert
      (should (equal agent-repl--load-errors '(("probe" error "probe boom"))))
      (should (equal warns '("FAILED to load probe.el: (error \"probe boom\")")))
      (should-not (member "probe.el loaded." infos)))))

(ert-deftest agent-repl-config-test-loader-does-not-go-through-doom-load ()
  "A load error reaches the loader even where `doom-load' would swallow it.
`doom-load' answers nil for a file outside Doom's directories (a symlinked
module); stubbed so here, the failing module is still recorded."
  (agent-repl-config-test--with-root root
    ;; Arrange
    (agent-repl-config-test--write-bytes (expand-file-name "lisp/probe.el" root)
                                         "(error \"probe boom\")\n")
    (let ((agent-repl--elisp-module-builds nil)
          (agent-repl--load-errors nil)
          (load-file-name (expand-file-name "config.el" root)))
      ;; Act
      (cl-letf (((symbol-function 'doom-load) #'ignore)
                ((symbol-function 'agent-repl--boot-emit) #'ignore))
        (eval '(agent-repl--load-module "probe") t))
      ;; Assert
      (should (equal (mapcar #'car agent-repl--load-errors) '("probe"))))))

(ert-deftest agent-repl-config-test-load-resets-the-record ()
  "Every load of config.el starts the record over, like its load errors.
Read from the source rather than by re-loading config.el here: a real load
re-loads every module, which this suite has no business doing again.  The
reset must be a top-level `setq' that precedes the first module load."
  ;; Arrange
  (let ((forms (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "config.el" agent-repl-config-test--module-root))
                 (let (acc)
                   (condition-case nil
                       (while t (push (read (current-buffer)) acc))
                     (end-of-file nil))
                   (nreverse acc)))))
    ;; Act
    (let ((reset (cl-position '(setq agent-repl--elisp-module-builds nil) forms
                              :test #'equal))
          (first-load (cl-position-if
                       (lambda (form) (eq (car-safe form) 'agent-repl--load-module))
                       forms)))
      ;; Assert
      (should (and reset first-load (< reset first-load))))))

;;;; ---- Every module loads ----

(ert-deftest agent-repl-config-test-every-module-loads-in-batch ()
  "The harness\='s real load of every module config.el names failed none.
A module whose load fails is reported only at boot, so without this a
broken module surfaced only as whatever downstream symptom it caused
\(2026-11-02: a bytecode `module-not-loaded' record)."
  (should (null agent-repl-config-test--harness-load-errors)))

;;;; ---- What runs once every module had its turn ----

(defmacro agent-repl-config-test--with-lifecycle (&rest body)
  "Run BODY with the lifecycle hooks empty and the boot reporters recorded.
Binds `infos' and `warns', the formatted boot records, newest first."
  (declare (indent 0))
  `(let ((emacs-startup-hook nil)
         (kill-emacs-hook nil)
         (agent-repl-frontend-auto-start t)
         (infos nil)
         (warns nil))
     (cl-letf (((symbol-function 'agent-repl--boot-emit)
                (lambda (warn message)
                  (let ((text (agent-repl-log-site-render (car message) (cdr message))))
                    (if warn (push text warns) (push text infos))))))
       ,@body)))

(ert-deftest agent-repl-config-test-lifecycle-registers-the-daemon-ensure ()
  "An interactive load registers the daemon ensure on the startup hook."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (agent-repl--register-lifecycle-hooks nil)
    ;; Assert
    (should (memq #'agent-repl-daemon-schedule-ensure emacs-startup-hook))))

(ert-deftest agent-repl-config-test-lifecycle-registers-the-composer-save ()
  "An interactive load registers the composer save on the shutdown hook."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (agent-repl--register-lifecycle-hooks nil)
    ;; Assert
    (should (memq #'agent-repl-input-save-all-on-shutdown kill-emacs-hook))))

(ert-deftest agent-repl-config-test-lifecycle-registers-the-log-retention-sweep ()
  "An interactive load registers the log retention sweep on the startup hook."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (agent-repl--register-lifecycle-hooks nil)
    ;; Assert
    (should (memq #'agent-repl-schedule-log-retention-sweep emacs-startup-hook))))

(ert-deftest agent-repl-config-test-lifecycle-registers-the-heap-census ()
  "An interactive load registers the heap census on the startup hook."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (agent-repl--register-lifecycle-hooks nil)
    ;; Assert
    (should (memq #'agent-repl-heap-census-schedule emacs-startup-hook))))

(ert-deftest agent-repl-config-test-lifecycle-auto-start-off-registers-no-ensure ()
  "With auto-start off the daemon ensure is not registered."
  (agent-repl-config-test--with-lifecycle
    ;; Arrange
    (setq agent-repl-frontend-auto-start nil)
    ;; Act
    (agent-repl--register-lifecycle-hooks nil)
    ;; Assert
    (should-not (memq #'agent-repl-daemon-schedule-ensure emacs-startup-hook))))

(ert-deftest agent-repl-config-test-lifecycle-batch-registers-nothing ()
  "A batch load registers no lifecycle hook."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (agent-repl--register-lifecycle-hooks t)
    ;; Assert
    (should (and (null emacs-startup-hook) (null kill-emacs-hook)))))

(ert-deftest agent-repl-config-test-lifecycle-hook-of-a-failed-module-is-reported ()
  "A hook whose module failed to load is reported loudly, never registered."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (agent-repl--register-lifecycle-hook
     'emacs-startup-hook 'agent-repl-config-test--never-defined nil "probe")
    ;; Assert
    (should (null emacs-startup-hook))
    (should (equal warns
                   '("lifecycle: probe NOT registered on emacs-startup-hook: `agent-repl-config-test--never-defined' is undefined, its module failed to load")))))

(ert-deftest agent-repl-config-test-load-errors-report-each-module-loudly ()
  "Each failed module is reported through the boot warn path, oldest first."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (ignore-error error
      (agent-repl--report-load-errors '(("splash" error "b") ("core" error "a"))))
    ;; Assert
    (should (equal (reverse warns)
                   '("Loaded with 2 ERROR(S):"
                     "  core.el: (error \"a\")"
                     "  splash.el: (error \"b\")")))))

(ert-deftest agent-repl-config-test-load-errors-signal-after-the-report ()
  "A failed load still ends in the FATAL error once it is reported."
  (agent-repl-config-test--with-lifecycle
    ;; Act
    (let ((err (should-error (agent-repl--report-load-errors '(("splash" error "b"))))))
      ;; Assert
      (should (string-search "FATAL: 1 module(s) failed to load" (cadr err))))))

(defun agent-repl-config-test--top-level-forms ()
  "Every top-level form of config.el, in order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "config.el" agent-repl-config-test--module-root))
    (let (acc)
      (condition-case nil
          (while t (push (read (current-buffer)) acc))
        (end-of-file nil))
      (nreverse acc))))

(ert-deftest agent-repl-config-test-lifecycle-is-registered-before-load-errors-signal ()
  "config.el registers the lifecycle hooks BEFORE it reports the load errors.
Regression, 2026-10-25: the report signalled first, and a cosmetic module
failing its load left Emacs with no daemon at all.  Read from the source:
a real load of config.el re-loads every module."
  ;; Arrange
  (let ((forms (agent-repl-config-test--top-level-forms)))
    ;; Act
    (let ((register (cl-position '(agent-repl--register-lifecycle-hooks noninteractive)
                                 forms :test #'equal))
          (report (cl-position-if
                   (lambda (form)
                     (and (eq (car-safe form) 'if)
                          (equal (nth 1 form) 'agent-repl--load-errors)))
                   forms))
          (last-load (cl-position-if
                      (lambda (form) (eq (car-safe form) 'agent-repl--load-module))
                      forms :from-end t)))
      ;; Assert
      (should (and register report last-load (< last-load register report))))))

;;;; ---- config.el's load list ----

(ert-deftest agent-repl-config-test-loads-verbs ()
  "verbs.el is in the load list: without it every workspace verb is unbound."
  (should (string-match-p "(agent-repl--load-module \"verbs\")"
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "config.el"
                                               agent-repl-config-test--module-root))
                            (buffer-string)))))

(ert-deftest agent-repl-config-test-loads-no-deleted-module ()
  "config.el names no module this branch deleted.
A load of a missing module file is a load ERROR, which fails the whole
module, so this is the cheapest place to catch a stale load line."
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "config.el" agent-repl-config-test--module-root))
                (buffer-string))))
    (dolist (name '("merge-handlers" "workspace-create-client"))
      (should-not (string-match-p (format "(agent-repl--load-module \"%s\")" name)
                                  text)))))

;;;; ---- The doctor's daemon probe ----
;;
;; THREE OUTCOMES, and the distinction between the last two is the point: a
;; daemon that is not there cannot be unhealthy, and conflating the two sends
;; the reader hunting for faults in a process that does not exist.

(defvar agent-repl-config-test--doctor-loaded nil
  "Non-nil once doctor.el has been loaded for these tests.")

(defvar agent-repl-config-test--module-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "The module root, captured at LOAD time.
`load-file-name' is bound only while a file is loading, so a test body
that reads it at run time gets nil.")

(defvar agent-repl-doctor-inhibit-checks)

(defun agent-repl-config-test--load-doctor ()
  "Load doctor.el once, with its top-level aggregation neutralized.
doctor.el is a SCRIPT, not a library: `doom doctor' loads it for the side
effect of running every check and reporting through `warn!'.  Loading it
to reach one function therefore means stubbing the reporters and giving
the probe a no-daemon answer, so the load itself finds nothing and says
nothing."
  (unless agent-repl-config-test--doctor-loaded
    (cl-letf (((symbol-function 'warn!) (lambda (&rest _) nil))
              ((symbol-function 'error!) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-connect-read-daemon-addr) (lambda () nil)))
      (let ((agent-repl-doctor-inhibit-checks t))
        (load (expand-file-name "doctor.el" agent-repl-config-test--module-root) nil t)))
    (setq agent-repl-config-test--doctor-loaded t)))

(defmacro agent-repl-config-test--with-doctor (&rest body)
  "Run BODY with doctor.el loaded and its probe dependencies stubbed."
  (declare (indent 0))
  `(progn
     (agent-repl-config-test--load-doctor)
     (cl-letf (((symbol-function 'agent-repl-connect-open) (lambda (_a) 'conn))
               ((symbol-function 'agent-repl-connect-close) (lambda (_c) nil)))
       ,@body)))

;;;; ---- The doctor's requirements check ----

(defun agent-repl-config-test--requirements-report (&rest entries)
  "A runner report of ENTRIES, each a plist of the report's fields."
  (json-serialize
   (vconcat (mapcar (lambda (e)
                      (list :id (plist-get e :id) :status (plist-get e :status)
                            :optional (if (plist-get e :optional) t :false)
                            :detail "d" :why "w." :fix "f"))
                    entries))))

(defvar agent-repl-config-test--doctor-records nil
  "Every doctor log record the current test made, rendered, oldest first.")

(defmacro agent-repl-config-test--with-requirements (exit output &rest body)
  "Run BODY with the doctor's runner answering EXIT and OUTPUT.
Every doctor log record lands in `agent-repl-config-test--doctor-records'."
  (declare (indent 2))
  `(agent-repl-config-test--with-doctor
     (setq agent-repl-config-test--doctor-records nil)
     (cl-letf (((symbol-function 'agent-repl--doctor-run-requirements)
                (lambda () (cons ,exit ,output)))
               ((symbol-function 'agent-repl--doctor-emit)
                (lambda (message)
                  (setq agent-repl-config-test--doctor-records
                        (append agent-repl-config-test--doctor-records
                                (list (agent-repl-log-site-render (car message) (cdr message))))))))
       ,@body)))

(ert-deftest agent-repl-config-test-doctor-requirements-all-met-is-no-finding ()
  "Every requirement met is no finding."
  (agent-repl-config-test--with-requirements 0
      (agent-repl-config-test--requirements-report '(:id "go" :status "ok"))
    (should-not (agent-repl--doctor-requirements-issues))))

(ert-deftest agent-repl-config-test-doctor-unmet-required-requirement-is-an-error ()
  "An unmet required entry is an error naming its status and fix."
  (agent-repl-config-test--with-requirements 1
      (agent-repl-config-test--requirements-report '(:id "go" :status "ok")
                                                   '(:id "terminal-notifier" :status "outdated"))
    (should (equal (agent-repl--doctor-requirements-issues)
                   '((error . "agent-repl: requirement terminal-notifier is outdated: d. w. Fix: f"))))))

(ert-deftest agent-repl-config-test-doctor-unmet-optional-requirement-is-a-warning ()
  "An unmet optional entry is a warning, marked optional."
  (agent-repl-config-test--with-requirements 0
      (agent-repl-config-test--requirements-report '(:id "prefix" :status "needs-user" :optional t))
    (should (equal (agent-repl--doctor-requirements-issues)
                   '((warn . "agent-repl: requirement prefix is needs-user (optional): d. w. Fix: f"))))))

(ert-deftest agent-repl-config-test-doctor-refused-manifests-are-an-error ()
  "A runner that refuses the manifests (exit 2) is an error carrying its reason."
  (agent-repl-config-test--with-requirements 2 "[requirements] ERROR: manifest: x: unknown kind magic\n"
    (should (equal (agent-repl--doctor-requirements-issues)
                   '((error . "agent-repl: bin/requirements.sh check could not run (exit 2): [requirements] ERROR: manifest: x: unknown kind magic"))))
    (should (member "doctor: requirements refused exit=2 output=\"[requirements] ERROR: manifest: x: unknown kind magic\n\""
                    agent-repl-config-test--doctor-records))))

(ert-deftest agent-repl-config-test-doctor-unparseable-requirements-report-is-an-error ()
  "A runner answer that is not its report is an error, never silence."
  (agent-repl-config-test--with-requirements 1 "not json"
    (should (equal (agent-repl--doctor-requirements-issues)
                   '((error . "agent-repl: bin/requirements.sh check answered no report (exit 1): not json"))))
    (should (seq-some (lambda (r) (string-prefix-p "doctor: requirements unparseable error=" r))
                      agent-repl-config-test--doctor-records))))

(ert-deftest agent-repl-config-test-doctor-runs-the-module-runner-check-json ()
  "The doctor runs the module's own runner with check --json."
  (agent-repl-config-test--with-doctor
    (let (called)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program _in _out _display &rest args)
                   (setq called (cons program args))
                   (insert "[]")
                   0)))
        (should (equal (agent-repl--doctor-run-requirements) '(0 . "[]")))
        (should (equal called (list (expand-file-name "bin/requirements.sh" agent-repl-config-test--module-root)
                                    "check" "--json")))))))

(ert-deftest agent-repl-config-test-doctor-aggregates-the-requirements-provider ()
  "The aggregation names the requirements provider, so doom doctor runs it."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "doctor.el" agent-repl-config-test--module-root))
    (should (search-forward "agent-repl--doctor-requirements-issues\n" nil t))))

;;;; ---- Spotlight and the agent worktrees ----

(defconst agent-repl-config-test--workspace-list
  (concat "{\"workspace\":{\"dir\":\"/Users/u/.config/doom-worktrees/a\"},\"repository\":{\"dir\":\"/Users/u/.config/doom\"}}\n"
          "{\"workspace\":{\"dir\":\"/Users/u/.config/doom\"},\"repository\":{\"dir\":\"/Users/u/.config/doom\"}}\n"
          "{\"workspace\":{\"dir\":\"/Users/u/work/ee-worktrees/b\"},\"repository\":{\"dir\":\"/Users/u/work/ee\"}}\n"
          "{\"workspace\":{\"dir\":\"/Users/u/work/ee-worktrees/c\"},\"repository\":{\"dir\":\"/Users/u/work/ee\"}}\n"
          "{\"workspace\":{\"dir\":\"/Users/u/work/gns.noindex/d\"},\"repository\":{\"dir\":\"/Users/u/work/gns\"}}\n")
  "A workspace list: one root under a dot folder, one indexed, one `.noindex'.")

(defmacro agent-repl-config-test--with-workspace-list (exit output &rest body)
  "Run BODY with the Spotlight check's workspace list answering EXIT and OUTPUT."
  (declare (indent 2))
  `(agent-repl-config-test--with-doctor
     (cl-letf (((symbol-function 'agent-repl--doctor-run-workspace-list) (lambda () (cons ,exit ,output)))
               ((symbol-function 'agent-repl--doctor-emit) #'ignore))
       ,@body)))

(ert-deftest agent-repl-config-test-doctor-spotlight-reads-each-worktree-root-once ()
  "Each root is a non-checkout workspace's parent, named once."
  (agent-repl-config-test--with-doctor
    (should (equal (agent-repl--doctor-worktree-roots agent-repl-config-test--workspace-list)
                   '("/Users/u/.config/doom-worktrees" "/Users/u/work/ee-worktrees" "/Users/u/work/gns.noindex")))))

(ert-deftest agent-repl-config-test-doctor-spotlight-skips-dot-and-noindex-paths ()
  "Spotlight never indexes a path with a dot folder or a `.noindex' folder."
  (agent-repl-config-test--with-doctor
    (should (equal (mapcar #'agent-repl--doctor-spotlight-skips-p
                           '("/Users/u/.config/doom-worktrees" "/Users/u/work/gns.noindex" "/Users/u/work/ee-worktrees"))
                   '(t t nil)))))

(ert-deftest agent-repl-config-test-doctor-spotlight-warns-of-an-indexed-root ()
  "An indexed root is a warning naming the one step."
  (agent-repl-config-test--with-workspace-list 0 agent-repl-config-test--workspace-list
    (let ((agent-repl-spotlight-excluded-roots nil))
      (should (equal (agent-repl--doctor-spotlight-issues)
                     '((warn . "agent-repl: Spotlight indexes the agent worktrees in /Users/u/work/ee-worktrees. Add that folder in System Settings > Spotlight > Search Privacy, then add it to `agent-repl-spotlight-excluded-roots'.")))))))

(ert-deftest agent-repl-config-test-doctor-spotlight-trusts-a-named-exclusion ()
  "A root named in `agent-repl-spotlight-excluded-roots' is no finding."
  (agent-repl-config-test--with-workspace-list 0 agent-repl-config-test--workspace-list
    (let ((agent-repl-spotlight-excluded-roots '("/Users/u/work/ee-worktrees/")))
      (should-not (agent-repl--doctor-spotlight-issues)))))

(ert-deftest agent-repl-config-test-doctor-spotlight-reports-an-unlistable-registry ()
  "A workspace list that fails is a warning carrying its output."
  (agent-repl-config-test--with-workspace-list 1 "no daemon\n"
    (should (equal (agent-repl--doctor-spotlight-issues)
                   '((warn . "agent-repl: the Spotlight check could not list the workspaces (exit 1): no daemon"))))))

(ert-deftest agent-repl-config-test-doctor-aggregates-the-spotlight-provider ()
  "The aggregation names the Spotlight provider, so doom doctor runs it."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "doctor.el" agent-repl-config-test--module-root))
    (should (search-forward "agent-repl--doctor-spotlight-issues\n" nil t))))

(ert-deftest agent-repl-config-test-doctor-loaded-as-a-library-runs-no-check ()
  "Loading doctor.el with the checks inhibited starts no process."
  (let ((processes 0))
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (cl-incf processes) 0))
              ((symbol-function 'warn!) (lambda (&rest _) nil))
              ((symbol-function 'error!) (lambda (&rest _) nil)))
      (let ((agent-repl-doctor-inhibit-checks t))
        (load (expand-file-name "doctor.el" agent-repl-config-test--module-root) nil t)))
    (should (= processes 0))))

(ert-deftest agent-repl-config-test-doctor-reports-no-daemon-addr ()
  "An absent daemon.addr means no daemon has been started."
  (agent-repl-config-test--with-doctor
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr) (lambda () nil))
              ((symbol-function 'agent-repl-rpc-daemon-health-sync) (lambda (&rest _) nil)))
      (let ((issues (agent-repl--doctor-daemon-issues)))
        (should (equal (length issues) 1))
        (should (string-match-p "no daemon.addr" (cdr (car issues))))))))

(ert-deftest agent-repl-config-test-doctor-reports-a-stale-addr ()
  "A transport failure is evidence about the FILE, never about health."
  (agent-repl-config-test--with-doctor
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
               (lambda () "127.0.0.1:1234"))
              ((symbol-function 'agent-repl-rpc-daemon-health-sync)
               (lambda (&rest _) (error "connection refused"))))
      (let ((issues (agent-repl--doctor-daemon-issues)))
        (should (equal (length issues) 1))
        (should (string-match-p "no daemon answering" (cdr (car issues))))
        (should (string-match-p "stale daemon.addr" (cdr (car issues))))))))

(ert-deftest agent-repl-config-test-doctor-stale-addr-is-not-unhealthy ()
  "A daemon that is not there is never REPORTED as an unhealthy one."
  (agent-repl-config-test--with-doctor
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
               (lambda () "127.0.0.1:1234"))
              ((symbol-function 'agent-repl-rpc-daemon-health-sync)
               (lambda (&rest _) (error "connection refused"))))
      (should-not (string-match-p "UNHEALTHY"
                                  (cdr (car (agent-repl--doctor-daemon-issues))))))))

(ert-deftest agent-repl-config-test-doctor-healthy-daemon-is-no-finding ()
  "The doctor reports findings, and a healthy daemon is not one."
  (agent-repl-config-test--with-doctor
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
               (lambda () "127.0.0.1:1234"))
              ((symbol-function 'agent-repl-rpc-daemon-health-sync)
               (lambda (&rest _) '(:arm :success :value (:arm :healthy :value nil)))))
      (should-not (agent-repl--doctor-daemon-issues)))))

(ert-deftest agent-repl-config-test-doctor-unhealthy-prints-each-fault ()
  "UNHEALTHY IS AN ANSWER: one issue for the verdict, one per fault."
  (agent-repl-config-test--with-doctor
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
               (lambda () "127.0.0.1:1234"))
              ((symbol-function 'agent-repl-rpc-daemon-health-sync)
               (lambda (&rest _)
                 '(:arm :success
                   :value (:arm :unhealthy
                           :value (:faults ((:detail "shim adoption stalled"))))))))
      (let ((issues (agent-repl--doctor-daemon-issues)))
        (should (equal (length issues) 2))
        (should (string-match-p "UNHEALTHY (1 fault" (cdr (nth 0 issues))))
        (should (string-match-p "shim adoption stalled" (cdr (nth 1 issues))))))))

(ert-deftest agent-repl-config-test-doctor-refused-question-still-proves-a-daemon ()
  "A daemon that REFUSES the health question is still a daemon that answered."
  (agent-repl-config-test--with-doctor
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
               (lambda () "127.0.0.1:1234"))
              ((symbol-function 'agent-repl-rpc-daemon-health-sync)
               (lambda (&rest _) '(:arm :error :value nil))))
      (let ((issues (agent-repl--doctor-daemon-issues)))
        (should (equal (length issues) 1))
        (should (string-match-p "refused the health question" (cdr (car issues))))))))

;;;; ---- Tests: the composer shutdown save ----

(ert-deftest agent-repl-config-test-batch-load-registers-no-composer-shutdown-save ()
  "A batch load leaves `kill-emacs-hook' without the composer shutdown save.
A batch run must never write a workspace's history as it exits."
  (should-not (memq #'agent-repl-input-save-all-on-shutdown kill-emacs-hook)))

(provide 'test-config)

;;; test-config.el ends here
