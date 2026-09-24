;;; elisp-build.el --- the elisp build Emacs reports, and the deploy's reload -*- lexical-binding: t; -*-

;;; Commentary:

;; THE DAEMON OWNS DEPLOYS, and every process reports its build when it
;; connects.  Emacs's build is the content hash of the elisp it LOADED, sent
;; as `WatchDaemonEmacs.elisp_build' on every WatchDaemon it opens.  A deploy
;; that finds that build older than the checkout's pushes `reload_elisp' on
;; the same stream, and Emacs hot-loads the module set.
;;
;; THE ALGORITHM IS THE CONTRACT (endpoint_watch_daemon.proto): the lowercase
;; hex SHA-256 of the concatenated lines `<module>\t<sha256>\n', one per
;; module config.el loads through `agent-repl--load-module', in that load
;; order, where <sha256> is the SHA-256 of `lisp/<module>.el''s bytes.  A
;; module whose file is absent contributes no line.  The daemon computes the
;; same answer from the checkout, and `proto/vocab/elisp-build.json' holds
;; both implementations to it.
;;
;; config.el records the (MODULE . SHA256) list as it loads
;; (`agent-repl--elisp-module-builds'); this file turns such a list into the
;; build, answers the running Emacs's build, and carries out a pushed reload.
;;
;; THE RELOAD LOADS THE WHOLE SET, never a partial one: core.el cancels every
;; module timer at load and only the owner files re-arm them, so a partial set
;; once left a live Emacs with a dead heartbeat.  After the load the heartbeat
;; assertion re-arms anything stranded and reports what it could not.  The
;; load runs OUT of the process filter that delivered the push: loading from
;; inside a filter runs arbitrary top-level code (and the event loop) in the
;; middle of the WatchDaemon stream's own read.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl--assert-heartbeat-armed "core" ())
(declare-function agent-repl--elisp-file-sha256 "config" (file))
(declare-function agent-repl--elisp-module-file "config" (root module))

(defvar agent-repl--elisp-module-builds)
(defvar agent-repl--frontend-root)

(defconst agent-repl--elisp-build-log-scope
  '(:agent-repl-central "the loaded elisp is process-wide")
  "Log scope of every record this file writes.
The elisp an Emacs has loaded, and a reload of it, belong to the process,
never to one workspace.")

;;;; ---- The build ----

(defun agent-repl-elisp-build-of (entries)
  "Return the elisp build of ENTRIES, a list of (MODULE . SHA256) in load order.
The lowercase hex SHA-256 of the concatenated lines `MODULE\\tSHA256\\n'.
PURE: no file is read here, so the one algorithm is testable against the
cross-language vector on its own."
  (secure-hash
   'sha256
   (encode-coding-string
    (mapconcat (lambda (entry) (format "%s\t%s\n" (car entry) (cdr entry)))
               entries "")
    'utf-8)))

(defun agent-repl-elisp-build-entries (root modules)
  "Return the (MODULE . SHA256) entries of MODULES' sources under ROOT, in order.
A module whose `lisp/MODULE.el' is absent contributes no entry, as the
contract says."
  (delq nil
        (mapcar (lambda (module)
                  (let ((sha (agent-repl--elisp-file-sha256
                              (agent-repl--elisp-module-file root module))))
                    (and sha (cons module sha))))
                modules)))

(defun agent-repl-elisp-build ()
  "Return the elisp build this Emacs has loaded.
Computed from `agent-repl--elisp-module-builds', which config.el records as
it loads and a pushed reload recomputes.  An EMPTY record is a loud
failure, never an empty build: WatchDaemon's elisp_build is REQUIRED, and a
daemon told nothing could not tell whether this Emacs runs its elisp."
  (unless agent-repl--elisp-module-builds
    (agent-repl--fatal agent-repl--elisp-build-log-scope
                       "elisp.elisp-build.no-modules-recorded reason=%s"
                       "config.el recorded no loaded module, so this Emacs has no elisp build to report"))
  (let ((build (agent-repl-elisp-build-of agent-repl--elisp-module-builds)))
    (agent-repl--log agent-repl--elisp-build-log-scope
                     "elisp.elisp-build.current modules=%d build=%s"
                     (length agent-repl--elisp-module-builds) build)
    build))

;;;; ---- config.el's module list ----

(defconst agent-repl--elisp-config-module-regexp
  "^(agent-repl--load-module \"\\([^\"]*\\)\")"
  "A TOP-LEVEL `agent-repl--load-module' form in config.el, capturing its name.
Anchored at the line start, so the macro's own definition and any commented
or nested mention is not a module.  The daemon reads the same lines.")

(defun agent-repl--elisp-config-modules (config-file)
  "Return the modules CONFIG-FILE loads, in its `agent-repl--load-module' order."
  (with-temp-buffer
    (insert-file-contents config-file)
    (goto-char (point-min))
    (let (modules)
      (while (re-search-forward agent-repl--elisp-config-module-regexp nil t)
        (push (match-string-no-properties 1) modules))
      (nreverse modules))))

;;;; ---- The pushed reload ----

(defun agent-repl--elisp-normalize-root (root)
  "Return ROOT as an absolute directory name, for comparing module roots."
  (file-name-as-directory (expand-file-name root)))

(defun agent-repl--elisp-reload-load-file (file)
  "External-boundary wrapper: `load' the elisp source FILE into this Emacs.
The exact FILE, never a `.elc' beside it (NOSUFFIX), quietly."
  (load file nil t t))

(defun agent-repl-elisp-reload-handle (reload)
  "Handle a pushed `DaemonReloadElisp' RELOAD, a plist (:module-root :build).
The daemon may only reload THIS Emacs's checkout: a root naming any other
is REFUSED loudly and nothing is loaded, because it would leave the editor
on one checkout's elisp and the daemon on another's.  Otherwise the reload
is SCHEDULED out of the process filter that delivered the push.  Returns
non-nil when the reload was scheduled."
  (let* ((pushed (plist-get reload :module-root))
         (build (plist-get reload :build))
         (theirs (agent-repl--elisp-normalize-root pushed))
         (ours (agent-repl--elisp-normalize-root agent-repl--frontend-root)))
    (if (not (string= theirs ours))
        (progn
          (agent-repl--error agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.reload-refused reason=root-mismatch pushed-root=%S running-root=%S build=%S"
                             theirs ours build)
          (message "agent-repl: elisp reload REFUSED -- the deploy is for %s, this Emacs runs %s"
                   theirs ours)
          nil)
      (agent-repl--info agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.reload-scheduled root=%S build=%S"
                        ours build)
      (run-at-time 0 nil #'agent-repl--elisp-reload-run ours build)
      t)))

(defun agent-repl--elisp-reload-load-modules (root modules)
  "Load MODULES' sources under ROOT in order, and record what was loaded.
Returns (ENTRIES . FAILURES): ENTRIES the (MODULE . SHA256) of every source
found, in order, and FAILURES a list of (MODULE . REASON).  A module that
fails is logged at ERROR and the rest still load: stopping at the first
failure would strand every later module's timers too."
  (let (entries failures)
    (dolist (module modules)
      (let* ((file (agent-repl--elisp-module-file root module))
             (test-p (string-prefix-p "test-" module))
             (sha (and (not test-p) (agent-repl--elisp-file-sha256 file))))
        (cond
         (test-p
          ;; A test file is a batch-only harness; loading one into a live
          ;; Emacs once disarmed its external boundaries and redirected its
          ;; state.  config.el never names one, so this is a broken loader.
          (push (cons module "a test file is never loaded into a running Emacs") failures)
          (agent-repl--error agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.reload-test-module-refused module=%s file=%S"
                             module file))
         ((null sha)
          (push (cons module "the file is absent") failures)
          (agent-repl--error agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.reload-module-absent module=%s file=%S"
                             module file))
         (t
          (push (cons module sha) entries)
          (condition-case err
              (progn
                (agent-repl--elisp-reload-load-file file)
                (agent-repl--log agent-repl--elisp-build-log-scope
                                 "elisp.elisp-build.reload-module-loaded module=%s sha256=%s"
                                 module sha))
            (error
             (push (cons module (error-message-string err)) failures)
             (agent-repl--error agent-repl--elisp-build-log-scope
                                "elisp.elisp-build.reload-module-failed module=%s file=%S error=%S"
                                module file err)))))))
    (cons (nreverse entries) (nreverse failures))))

(defun agent-repl--elisp-reload-report-heartbeat (result)
  "Log the heartbeat assertion RESULT of a reload; a failed timer is an ERROR."
  (let ((rearmed (plist-get result :rearmed))
        (failed (plist-get result :failed)))
    (agent-repl--info agent-repl--elisp-build-log-scope
                      "elisp.elisp-build.reload-heartbeat armed=%d rearmed=%d failed=%d unavailable=%d"
                      (length (plist-get result :armed)) (length rearmed)
                      (length failed) (length (plist-get result :unavailable)))
    (when rearmed
      (agent-repl--info agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.reload-heartbeat-rearmed keys=%S" rearmed))
    (when failed
      (agent-repl--error agent-repl--elisp-build-log-scope
                         "elisp.elisp-build.reload-heartbeat-failed keys=%S" failed)
      (message "agent-repl: elisp reload left required timers unarmed: %s"
               (mapconcat (lambda (key) (format "%s" key)) failed ", ")))))

(defun agent-repl--elisp-reload-run (root build)
  "Hot-load the module set from ROOT, which the daemon built as BUILD.
Reads config.el's `agent-repl--load-module' order, loads every source in
it, then runs the heartbeat assertion.  The recorded module builds are
recomputed from the bytes just loaded, so the next WatchDaemon reports the
new build; a recomputed build that differs from BUILD means the files moved
between the deploy and the load, and is an ERROR."
  (let ((config (expand-file-name "config.el" root))
        (modules nil))
    (agent-repl--info agent-repl--elisp-build-log-scope
                      "elisp.elisp-build.reload-begin root=%S build=%S" root build)
    (condition-case err
        (progn
          (setq modules (agent-repl--elisp-config-modules config))
          (unless modules
            (agent-repl--error agent-repl--elisp-build-log-scope
                               "elisp.elisp-build.reload-config-names-no-module config=%S"
                               config)
            (message "agent-repl: elisp reload FAILED -- %s names no module" config)))
      (error
       (agent-repl--error agent-repl--elisp-build-log-scope
                          "elisp.elisp-build.reload-config-unreadable config=%S error=%S"
                          config err)
       (message "agent-repl: elisp reload FAILED -- %s could not be read: %s"
                config (error-message-string err))))
    (when modules
      (let* ((loaded (agent-repl--elisp-reload-load-modules root modules))
             (entries (car loaded))
             (failures (cdr loaded)))
        (setq agent-repl--elisp-module-builds entries)
        (agent-repl--elisp-reload-report-heartbeat (agent-repl--assert-heartbeat-armed))
        (when failures
          (agent-repl--error agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.reload-module-failures count=%d failures=%S"
                             (length failures) failures)
          (message "agent-repl: elisp reload: %d module(s) failed to load: %s"
                   (length failures)
                   (mapconcat #'car failures ", ")))
        (let ((loaded-build (agent-repl-elisp-build-of entries)))
          (unless (string= loaded-build build)
            (agent-repl--error agent-repl--elisp-build-log-scope
                               "elisp.elisp-build.reload-build-mismatch pushed=%S loaded=%S reason=%s"
                               build loaded-build
                               "the files changed between the deploy and the load"))
          (agent-repl--info agent-repl--elisp-build-log-scope
                            "elisp.elisp-build.reload-complete modules=%d recorded=%d failed=%d build=%S"
                            (length modules) (length entries) (length failures)
                            loaded-build))))))

(provide 'elisp-build)

;;; elisp-build.el ends here
