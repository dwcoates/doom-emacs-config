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
    (cl-letf (((symbol-function 'agent-repl--log)
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
              (lambda () (agent-repl--boot-info "loaded %d of %d" 3 7)))))
    (should (string-match-p "loaded 3 of 7" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-info/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-info' routes centrally through it."
  ;; Arrange.
  (let ((delegated nil)
        (agent-repl--global-log-scope :test-central-scope))
    (cl-letf (((symbol-function 'agent-repl--info)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (apply #'format fmt args))))))
      ;; Act.
      (agent-repl--boot-info "hello %s" "world")
      ;; Assert.
      (should (equal delegated '(:test-central-scope "hello world"))))))

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
  "Once core.el defines the ladder, `--boot-warn' routes centrally through it."
  ;; Arrange.
  (let ((delegated nil)
        (agent-repl--global-log-scope :test-central-scope))
    (cl-letf (((symbol-function 'agent-repl--warn)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (apply #'format fmt args))))))
      ;; Act.
      (agent-repl--boot-warn "bad %s" "thing")
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
  (should-error (agent-repl--launchctl-call "list")
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
  `(let ((,var (file-name-as-directory (make-temp-file "agent-repl-config-root" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

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
      (cl-letf (((symbol-function 'agent-repl--boot-info) #'ignore))
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
        (cl-letf (((symbol-function 'agent-repl--boot-info) #'ignore))
          (eval '(agent-repl--load-module "probe") t))
        ;; Assert
        (should (equal agent-repl--elisp-module-builds
                       (list (cons "probe" (agent-repl--elisp-file-sha256 file)))))))))

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
A `load!' of a missing file is a load ERROR, which fails the whole
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
      (load (expand-file-name "doctor.el" agent-repl-config-test--module-root) nil t))
    (setq agent-repl-config-test--doctor-loaded t)))

(defmacro agent-repl-config-test--with-doctor (&rest body)
  "Run BODY with doctor.el loaded and its probe dependencies stubbed."
  (declare (indent 0))
  `(progn
     (agent-repl-config-test--load-doctor)
     (cl-letf (((symbol-function 'agent-repl-connect-open) (lambda (_a) 'conn))
               ((symbol-function 'agent-repl-connect-close) (lambda (_c) nil)))
       ,@body)))

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

(provide 'test-config)

;;; test-config.el ends here
