;;; daemon.el --- Emacs owns the daemon's COLD START, and only that -*- lexical-binding: t; -*-

;;; Commentary:

;; EMACS OWNS COLD START.  Auto-start on boot, stale-binary rebuild, the
;; foreign-daemon question, and surfacing a build failure.  EVERYTHING
;; AFTER BOOT is the daemon's own blue-green rollout, which daemon-link.el
;; rides as a client — this file has no restart machinery, no readiness
;; probing, no expected-restart bookkeeping and no process supervision
;; beyond the one spawn.
;;
;; THE COLD-START DECISION, in one sentence: a daemon that ANSWERS is
;; adopted and never killed.
;;
;;   daemon.addr absent            → build, start, wait for the file
;;   daemon.addr present, answers  → ADOPT it, healthy or unhealthy alike
;;   daemon.addr present, silent   → a stale file; treat it as absent
;;
;; UNHEALTHY IS AN ANSWER.  `DaemonHealth' returns a typed verdict, never
;; a transport error, so an unhealthy daemon is a daemon — it is adopted
;; and its faults are surfaced, because starting a second daemon beside a
;; live one is strictly worse than running the one that is there.  Only a
;; TRANSPORT failure proves no daemon is listening.
;;
;; NO ARGV.  The daemon takes no required flags: the state root travels in
;; `AGENT_REPL_STATE_DIR', which is exported EXPLICITLY on the spawn so
;; the child cannot inherit a different root than the one Emacs resolved.
;; The listen address is the daemon's own choice, published by writing
;; `daemon.addr' — which is also the readiness signal this file waits on.
;;
;; NO SLEEPS AND NO BLOCKING.  The boot wait is a timer poll whose tick is
;; a named function, and the build is an ASYNCHRONOUS `make-process' whose
;; sentinel carries the continuation.  Nothing in this file blocks the
;; main thread, because the frame must paint before any of it runs:
;; `agent-repl-daemon-schedule-ensure' defers the whole cold start onto an
;; idle timer, so the command loop reaches its first redisplay first.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl--backend-phase "core" (ws fmt &rest args))
(declare-function agent-repl--logfile-path "core" ())
(declare-function agent-repl--global-state-dir "core" ())
(declare-function agent-repl--global-state-file "core" (relative))

(declare-function agent-repl-connect-open "connect" (address))
(declare-function agent-repl-connect-close "connect" (conn))
(declare-function agent-repl-connect-read-daemon-addr "connect" ())
(declare-function agent-repl-connect-daemon-addr-file "connect" ())
(declare-function agent-repl-rpc-daemon-health "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-update-shutdown-schedule "rpc" (conn request &rest keys))

(declare-function agent-repl-link-connect "daemon-link" ())
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-link-up-p "daemon-link" ())
(declare-function agent-repl-link-teardown "daemon-link" ())

;;;; ---- Paths ----

(defconst agent-repl--frontend-root
  (let ((module-dir (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory))))
    (file-name-as-directory (expand-file-name ".." module-dir)))
  "Absolute path to the `modules/app/agent-repl/' directory.
This file lives in `lisp/', one level below that root, hence the `..'.")

(defconst agent-repl--frontend-webapp-dir
  (expand-file-name "webapp/dist" agent-repl--frontend-root)
  "The built webapp's static directory.
The daemon serves these assets itself, on its own one origin; this
constant exists for the build-artifact identity frontend.el reads, not
for any flag — no webapp path rides the daemon's argv any more.")

(defconst agent-repl--frontend-daemon-buffer "*claude-repld*"
  "Buffer the spawned daemon's stdout and stderr are captured into.")

(defconst agent-repl-daemon-build-buffer "*agent-repl-build-frontend*"
  "Capture buffer for the build script; SHOWN to the user on a failure.")

;;;; ---- Customization ----

(defcustom agent-repl-frontend-auto-start t
  "When non-nil, Emacs ensures a daemon at startup.
Set to nil to require `agent-repl-frontend-daemon-ensure' by hand."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-daemon-build-script
  (expand-file-name "bin/build-frontend.sh" agent-repl--frontend-root)
  "The build-if-stale orchestrator run before a daemon is started.
It decides staleness itself; this file never second-guesses it."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-daemon-build-shell "bash"
  "Shell interpreter used to invoke `agent-repl-daemon-build-script'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-daemon-build-status-display-seconds 4.0
  "Seconds the \"stack built in N.Ns\" mode-line status stays up.
A completed build is worth SAYING and not worth keeping: the segment
clears itself after this many seconds so the mode line goes back to
whatever else lives there."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-startup-idle-seconds 0
  "Idle seconds `agent-repl-daemon-schedule-ensure' waits before the ensure.
Zero is not \"immediately\": an idle timer runs only once the command loop
has gone idle, which is AFTER the first redisplay.  That is the whole
point — the frame paints before the stack build is even considered."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-command
  (list (expand-file-name "daemon/bin/claude-repld" agent-repl--frontend-root))
  "The argv used to start the daemon.
NO REQUIRED FLAGS: the state root travels in `AGENT_REPL_STATE_DIR' and
the daemon picks and publishes its own listen address."
  :type '(repeat string)
  :group 'agent-repl)

(defcustom agent-repl-daemon-boot-timeout-seconds 30.0
  "Seconds to wait for a started daemon to publish `daemon.addr'.
A daemon that has not written its address by then has not booted; the
wait fails loudly rather than leaving Emacs pointed at nothing."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-boot-poll-interval-seconds 0.25
  "Seconds between polls of `daemon.addr' while a started daemon boots."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-health-timeout-seconds 3.0
  "Seconds the cold-start `DaemonHealth' probe waits for an answer.
Short by design: this call only decides whether ANYONE is listening, and
a daemon that cannot answer it in seconds is indistinguishable from an
address file left behind by a dead one."
  :type 'number
  :group 'agent-repl)

;;;; ---- State ----

(defvar agent-repl--frontend-daemon-process nil
  "The daemon process THIS Emacs spawned, or nil.
Nil is the ordinary state when the daemon was adopted: Emacs supervises
only what it started, and never kills what it did not.")

(defvar agent-repl-daemon-build-failure nil
  "The last build failure's detail string, or nil.
Drawn as `agent-repl-daemon-mode-line-segment' and cleared by the next
successful build.  There is NO automatic retry — the interactive
`agent-repl-frontend-daemon-ensure' is the retry.")

(defvar agent-repl-daemon-mode-line-segment nil
  "The `global-mode-string' segment reporting the stack build, or nil.
Three states, in precedence order: a standing failure, a build that is
running right now, and a build that just finished (shown for
`agent-repl-daemon-build-status-display-seconds' and then cleared).")
(put 'agent-repl-daemon-mode-line-segment 'risky-local-variable t)

(defvar agent-repl-daemon--build-state nil
  "`building', `built', or nil — what the status segment is reporting.")

(defvar agent-repl-daemon--build-duration nil
  "Wall seconds the last successful build took, or nil.")

(defvar agent-repl-daemon--build-status-timer nil
  "Timer that clears the post-build status segment, or nil.")

(defvar agent-repl-daemon--build-in-flight nil
  "Non-nil while a build subprocess is running.
This is the coalescing guard: a second build request while one is under
way joins that build rather than starting a rival one.")

(defvar agent-repl-daemon--build-process nil
  "The running build process, or nil.  Kept for introspection only.")

(defvar agent-repl-daemon--build-continuations nil
  "Continuations waiting on the running build, oldest first.
Each is called with nil on success, or the failure detail string.")

(defvar agent-repl-daemon--build-started nil
  "`float-time' the running build started at, or nil.")

(defvar agent-repl-daemon--build-labels nil
  "`(RUNNING-LABEL . DONE-LABEL)' naming what the running build covers.")

(defvar agent-repl-daemon--build-target-names nil
  "The TARGETS argument the running build was asked for, for the log.")

(defvar agent-repl-daemon--startup-timer nil
  "The idle timer the startup hook scheduled the ensure on, or nil.")

(defvar agent-repl-daemon--boot-timer nil
  "The pending `daemon.addr' boot poll timer, or nil.")

(defvar agent-repl-daemon--boot-deadline nil
  "Epoch seconds after which the boot wait gives up, or nil.")

(defvar agent-repl-daemon--boot-continuation nil
  "The continuation the current boot wait will call, or nil.")

(defvar agent-repl-daemon--departure-timer nil
  "The pending poll waiting for a stopped daemon to remove `daemon.addr'.")

(defvar agent-repl-daemon--departure-deadline nil
  "`float-time' after which the departure wait gives up.")

(defvar agent-repl-daemon--departure-continuation nil
  "The function the departure wait calls once the daemon is gone.")

(defvar agent-repl-daemon--ensure-in-flight nil
  "Non-nil while an ensure is between its start and its outcome.
A second ensure while one is under way would spawn a second daemon,
which is the one thing this file exists to prevent.")

;;;; ---- External boundaries ----
;;
;; Every process this file starts goes through one of these.  Each is
;; registered in `agent-repl--external-boundary-functions', so a test that
;; reaches one unstubbed fails loudly rather than shelling out.

(defun agent-repl--frontend-run-build-script (args on-exit)
  "External-boundary wrapper: run the build shell with ARGS asynchronously.
ARGS is the argument list following the interpreter.  Output — stdout and
stderr alike — is captured into `agent-repl-daemon-build-buffer'.
ON-EXIT is called from the process sentinel with the integer exit code
once the process is no longer live.  Returns the process.

ASYNCHRONOUS BY CONTRACT.  Its predecessor was a `call-process', which
froze Emacs for the whole build — including before the very first
redisplay, so a startup after any touched source showed no frame at all
until the stack finished building."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "agent-repl-build-frontend"
   :buffer (get-buffer-create agent-repl-daemon-build-buffer)
   :command (cons agent-repl-daemon-build-shell args)
   :connection-type 'pipe
   :noquery t
   :sentinel
   (lambda (process _event)
     (unless (process-live-p process)
       (funcall on-exit (process-exit-status process))))))

(defun agent-repl--frontend-file-mtime (path)
  "External-boundary wrapper: PATH's mtime as a float, or nil when absent."
  (let ((attributes (file-attributes path))) ;; ALLOW-EXTERNAL-BOUNDARY
    (and attributes (float-time (file-attribute-modification-time attributes)))))

(defun agent-repl--frontend-source-files (dir regexp)
  "External-boundary wrapper: DIR's source files matching REGEXP, recursively.
REGEXP nil means every file.  `node_modules', `dist' and `bin' are never
descended into: they hold artifacts, not sources, and an artifact newer
than itself would make every tree permanently stale."
  (when (file-directory-p dir) ;; ALLOW-EXTERNAL-BOUNDARY
    (directory-files-recursively
     dir (or regexp "") nil
     (lambda (subdir)
       (not (member (file-name-nondirectory (directory-file-name subdir))
                    '("node_modules" "dist" "bin")))))))

(defun agent-repl--frontend-spawn-daemon (argv environment)
  "External-boundary wrapper: spawn ARGV under ENVIRONMENT, return the process.
ENVIRONMENT is a complete `process-environment' value, which is how
`AGENT_REPL_STATE_DIR' is exported EXPLICITLY rather than inherited."
  (let ((process-environment environment))
    (make-process ;; ALLOW-EXTERNAL-BOUNDARY
     :name "claude-repld"
     :buffer agent-repl--frontend-daemon-buffer
     :command argv
     :noquery t
     :sentinel #'agent-repl-daemon--sentinel)))

(defun agent-repl--frontend-artifact-exists-p (path)
  "External-boundary wrapper: return non-nil when artifact PATH exists."
  (file-exists-p path)) ; ALLOW-EXTERNAL-BOUNDARY

;;;; ---- The mode-line segment ----

(defun agent-repl-daemon--refresh-segment ()
  "Recompute `agent-repl-daemon-mode-line-segment' from the build state.
Precedence: a standing failure outranks a running build, which outranks
the short-lived \"just built\" note."
  (setq agent-repl-daemon-mode-line-segment
        (cond
         (agent-repl-daemon-build-failure "daemon: build failed")
         ((eq agent-repl-daemon--build-state 'building)
          "building agent-repl stack\u2026")
         ((and (eq agent-repl-daemon--build-state 'built)
               agent-repl-daemon--build-duration)
          (format "agent-repl stack built in %.1fs"
                  agent-repl-daemon--build-duration))))
  (agent-repl--log nil "elisp.daemon.segment segment=%S"
                   agent-repl-daemon-mode-line-segment)
  (force-mode-line-update t)
  ;; THE TAB-BAR CAVEAT (status.el): the tab-bar caches its render by string
  ;; equality, so a segment change that the mode line picks up can still
  ;; leave a tab-bar-hosted `global-mode-string' showing the old text.  Only
  ;; reached when a tab-bar is actually up.
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'agent-repl--force-tab-bar-redraw))
    (agent-repl--force-tab-bar-redraw))
  agent-repl-daemon-mode-line-segment)

(defun agent-repl-daemon--clear-build-status ()
  "Take the post-build status note down and repaint."
  (setq agent-repl-daemon--build-status-timer nil
        agent-repl-daemon--build-state nil
        agent-repl-daemon--build-duration nil)
  (agent-repl--log nil "elisp.daemon.build-status-cleared")
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon--set-build-status (state duration)
  "Report STATE (`building', `built' or nil) with DURATION in the mode line.
A `built' status schedules its own clearing; any pending clear timer is
cancelled first so two builds in a row cannot have the earlier one's
timer wipe the later one's status."
  (when (timerp agent-repl-daemon--build-status-timer)
    (cancel-timer agent-repl-daemon--build-status-timer))
  (setq agent-repl-daemon--build-status-timer nil
        agent-repl-daemon--build-state state
        agent-repl-daemon--build-duration duration)
  (when (eq state 'built)
    (setq agent-repl-daemon--build-status-timer
          (run-with-timer agent-repl-daemon-build-status-display-seconds nil
                          #'agent-repl-daemon--clear-build-status)))
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon-install-segment ()
  "Add the build-status segment to `global-mode-string', once."
  (let ((current (if (listp global-mode-string)
                     global-mode-string
                   (list global-mode-string))))
    (if (memq 'agent-repl-daemon-mode-line-segment current)
        (agent-repl--log nil "elisp.daemon.segment-already-installed")
      (setq global-mode-string
            (append current (list 'agent-repl-daemon-mode-line-segment)))
      (agent-repl--log nil "elisp.daemon.segment-installed"))))

;;;; ---- The build ----

(defconst agent-repl-daemon--default-build-targets '("shim" "webapp" "daemon")
  "The targets `bin/build-frontend.sh' builds when given none.
Mirrors the script's own default set; the elisp pre-check has to know
which artifacts a default run would cover in order to decide whether one
is worth starting at all.")

(defconst agent-repl-daemon--go-source-regexp "\\(?:\\.go\\|go\\.mod\\|go\\.sum\\)\\'"
  "Which files under a Go module count as its sources.")

(defconst agent-repl-daemon--build-target-specs
  (let ((root agent-repl--frontend-root)
        (cache-bin (expand-file-name "~/.cache/agent-repl/bin")))
    (cl-flet ((at (relative) (expand-file-name relative root)))
      (list
       (list "shim"
             :artifact (at "agent-shim/claude/shim/dist/main.js")
             :sources (list (at "agent-shim/claude/shim/src"))
             :files (list (at "agent-shim/claude/shim/package.json")
                          (at "agent-shim/claude/shim/tsconfig.json")
                          (at "agent-shim/claude/shim/build.mjs")))
       (list "webapp"
             :artifact (at "webapp/dist/index.html")
             :sources (list (at "webapp/src"))
             :files (list (at "webapp/package.json")
                          (at "webapp/tsconfig.json")
                          (at "webapp/vite.config.ts")))
       (list "daemon"
             :artifact (at "daemon/bin/claude-repld")
             :sources (list (at "daemon"))
             :match agent-repl-daemon--go-source-regexp)
       (list "store"
             :artifact (expand-file-name "shim-store" cache-bin)
             :sources (list (at "agent-shim/shim-store") (at "agent-shim/wire")
                            (at "agent-shim/logging/go") (at "proto/gen/go"))
             :match agent-repl-daemon--go-source-regexp)
       (list "sidecar"
             :artifact (expand-file-name "shim-claude-sidecar" cache-bin)
             :sources (list (at "agent-shim/claude/shim-sidecar") (at "agent-shim/wire")
                            (at "agent-shim/logging/go") (at "proto/gen/go"))
             :match agent-repl-daemon--go-source-regexp))))
  "Per-target artifact and source set, mirroring `bin/build-frontend.sh'.

WHY A SECOND COPY OF THE STALENESS QUESTION.  The script owns staleness
and remains the authority INSIDE a build — it still re-decides every
target and skips the fresh ones.  This spec only answers the cheaper
question the script cannot answer without being started: is starting it
worth a subprocess at all?  On a fresh tree the script costs seconds of
`npm'/`go' startup to conclude nothing changed, and Emacs used to pay
that on every boot.")

(defun agent-repl-daemon--target-stale-p (spec)
  "Return non-nil when SPEC's artifact is missing or older than its sources."
  (let* ((plist (cdr spec))
         (artifact (plist-get plist :artifact))
         (artifact-mtime (agent-repl--frontend-file-mtime artifact)))
    (if (null artifact-mtime)
        t
      (let ((newest 0)
            (sources (append (plist-get plist :files)
                             (apply #'append
                                    (mapcar (lambda (dir)
                                              (agent-repl--frontend-source-files
                                               dir (plist-get plist :match)))
                                            (plist-get plist :sources))))))
        (dolist (file sources)
          (let ((mtime (agent-repl--frontend-file-mtime file)))
            (when (and mtime (> mtime newest))
              (setq newest mtime))))
        ;; `>=' and not `>': the script uses the same comparison, and a
        ;; source written in the same second as the artifact is a source the
        ;; build may not have seen.
        (>= newest artifact-mtime)))))

(defun agent-repl-daemon--stale-targets (targets)
  "Return the subset of TARGETS needing a build; nil for TARGETS's default set.
An UNKNOWN target name is reported stale: a target this spec cannot
reason about must never be silently skipped."
  (seq-filter
   (lambda (name)
     (let ((spec (assoc name agent-repl-daemon--build-target-specs)))
       (if (null spec)
           (progn
             (agent-repl--warn nil "elisp.daemon.build-target-unknown target=%S" name)
             t)
         (agent-repl-daemon--target-stale-p spec))))
   (or targets agent-repl-daemon--default-build-targets)))

(defun agent-repl-daemon--build-finished (exit-code)
  "Settle the running build with EXIT-CODE and answer every waiting caller."
  (let* ((duration (- (float-time) (or agent-repl-daemon--build-started (float-time))))
         (output (with-current-buffer (get-buffer-create agent-repl-daemon-build-buffer)
                   (string-trim-right (buffer-string))))
         (labels agent-repl-daemon--build-labels)
         (targets agent-repl-daemon--build-target-names)
         (continuations (nreverse agent-repl-daemon--build-continuations))
         (detail (unless (eq exit-code 0)
                   (format "build failed (exit %s) — see %s"
                           exit-code agent-repl-daemon-build-buffer))))
    (setq agent-repl-daemon--build-in-flight nil
          agent-repl-daemon--build-process nil
          agent-repl-daemon--build-continuations nil
          agent-repl-daemon--build-started nil
          agent-repl-daemon--build-labels nil
          agent-repl-daemon--build-target-names nil)
    (agent-repl--info nil "elisp.daemon.build script=%S targets=%S exit=%S output=%s"
                      agent-repl-daemon-build-script (or targets 'default) exit-code
                      (if (string-empty-p output) "<empty>" output))
    (if detail
        (agent-repl-daemon--set-build-status nil nil)
      (agent-repl--backend-phase nil "%s built (%.1fs)" (cdr labels) duration)
      (agent-repl-daemon--set-build-status 'built duration))
    (dolist (continuation continuations)
      (funcall continuation detail))))

(defun agent-repl-daemon--build (targets continuation)
  "Build TARGETS if stale, then call CONTINUATION with nil or a failure detail.
TARGETS is a list of build-script target names, or nil for the script's
own default set (the whole stack).

ASYNCHRONOUS.  CONTINUATION runs from the build process's sentinel, and
SYNCHRONOUSLY on the two paths that start no process at all: a missing
script, and a tree where nothing is stale.

COALESCING.  A request arriving while a build runs joins that build
instead of starting a second one; both continuations settle together.

A failure is SURFACED and not retried: the capture buffer is shown, the
mode-line segment is raised, and the interactive ensure is the retry."
  (let* ((running-label (if targets (string-join targets "/") "the stack"))
         (done-label (if targets (string-join targets "/") "stack")))
    (cond
     ((not (agent-repl--frontend-artifact-exists-p agent-repl-daemon-build-script))
      (let ((detail (format "build script not found: %s" agent-repl-daemon-build-script)))
        (agent-repl--error nil "elisp.daemon.build-script-missing script=%S"
                           agent-repl-daemon-build-script)
        (funcall continuation detail)))
     (agent-repl-daemon--build-in-flight
      (agent-repl--info nil "elisp.daemon.build-coalesced targets=%S"
                        (or targets 'default))
      (push continuation agent-repl-daemon--build-continuations))
     ((null (agent-repl-daemon--stale-targets targets))
      ;; Nothing moved, so nothing is spawned: the script's own check would
      ;; reach the same verdict, after paying a subprocess to do it.
      (agent-repl--info nil "elisp.daemon.build-skipped-fresh targets=%S"
                        (or targets 'default))
      (funcall continuation nil))
     (t
      (with-current-buffer (get-buffer-create agent-repl-daemon-build-buffer)
        (erase-buffer))
      (agent-repl--backend-phase nil "rebuilding %s if stale..." running-label)
      (setq agent-repl-daemon--build-in-flight t
            agent-repl-daemon--build-continuations (list continuation)
            agent-repl-daemon--build-started (float-time)
            agent-repl-daemon--build-labels (cons running-label done-label)
            agent-repl-daemon--build-target-names targets)
      (agent-repl-daemon--set-build-status 'building nil)
      ;; A signal out of the spawn must not leave the in-flight guard raised:
      ;; a stuck guard would coalesce every later build onto a process that
      ;; never existed.  The signal itself is re-raised, never swallowed.
      (let ((process (condition-case err
                         (agent-repl--frontend-run-build-script
                          (cons agent-repl-daemon-build-script targets)
                          #'agent-repl-daemon--build-finished)
                       (error
                        (setq agent-repl-daemon--build-in-flight nil
                              agent-repl-daemon--build-continuations nil)
                        (agent-repl-daemon--set-build-status nil nil)
                        (agent-repl--error nil "elisp.daemon.build-spawn-failed error=%S" err)
                        (signal (car err) (cdr err))))))
        ;; Only when the build is still running: a stub (or a process that
        ;; exited before this returned) may already have settled it.
        (when agent-repl-daemon--build-in-flight
          (setq agent-repl-daemon--build-process process)))))))

(defun agent-repl-daemon--report-build-failure (detail)
  "Surface build failure DETAIL: the buffer, a WARNING, an echo, the segment."
  (setq agent-repl-daemon-build-failure detail)
  (agent-repl--warn nil "elisp.daemon.build-failed detail=%s" detail)
  (display-buffer agent-repl-daemon-build-buffer)
  (message "agent-repl: %s" detail)
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon--clear-build-failure ()
  "Clear a recorded build failure and take its mode-line segment down."
  (when agent-repl-daemon-build-failure
    (agent-repl--info nil "elisp.daemon.build-failure-cleared")
    (setq agent-repl-daemon-build-failure nil)
    (agent-repl-daemon--refresh-segment)))

;;;; ---- The probe ----

(defun agent-repl-daemon--probe (address on-answer on-silence)
  "Ask `DaemonHealth' at ADDRESS over a FRESH connection.
ON-ANSWER receives the decoded verdict plist `(:arm :healthy | :unhealthy
:value V)', or nil when the daemon answered with an error arm — ANY
answer proves a daemon is there.  ON-SILENCE receives the transport
failure detail, which is the only evidence that nobody is listening.  The
probe connection is closed on both paths: it exists to ask one question."
  (let ((conn (agent-repl-connect-open address)))
    (cl-labels
        ((finish (fn arg)
           (agent-repl-connect-close conn)
           (funcall fn arg)))
      (agent-repl-rpc-daemon-health
       conn nil
       :timeout agent-repl-daemon-health-timeout-seconds
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success (finish on-answer (plist-get response :value)))
           (:error
            ;; A daemon that REFUSES the health question still answered it:
            ;; something is serving on that address, so it is adopted.
            (agent-repl--warn nil "elisp.daemon.health-error address=%S error=%S"
                              address (plist-get response :value))
            (finish on-answer nil))
           (arm
            (agent-repl--error nil "elisp.daemon.health-unknown-arm address=%S arm=%S"
                               address arm)
            (finish on-answer nil))))
       :on-failure
       (lambda (detail) (finish on-silence detail))))))

(defun agent-repl-daemon--render-health (address faults)
  "Write the adopted daemon at ADDRESS and its FAULTS into the health buffer."
  (with-current-buffer (get-buffer-create "*agent-repl-health*")
    (goto-char (point-max))
    (insert (format "\ndaemon %s: UNHEALTHY (%d fault(s))\n" address (length faults)))
    (dolist (fault faults)
      (insert (format "  - %s\n" (plist-get fault :detail))))))

(defun agent-repl-daemon--spawned-here-p ()
  "Return non-nil when the daemon THIS Emacs spawned is still running.
Nil is the ordinary answer: Emacs adopts whatever answers, and the one
process it started itself is the only daemon that is its own."
  (and agent-repl--frontend-daemon-process
       (process-live-p agent-repl--frontend-daemon-process)
       t))

(defun agent-repl-daemon--report-provenance (address)
  "Record whose daemon the adopted one at ADDRESS is.
A FOREIGN daemon — one this Emacs did not spawn — is the normal
cold-start outcome and not a fault, but which daemon a session attached
to is the first thing anyone reading the log needs, so it is STATED
rather than inferred from the absence of a start line."
  (if (agent-repl-daemon--spawned-here-p)
      (agent-repl--info nil "elisp.daemon.own-adopted address=%S" address)
    (agent-repl--info nil "elisp.daemon.foreign-adopted address=%S" address)))

(defun agent-repl-daemon--report-verdict (address verdict)
  "Log the adopted daemon at ADDRESS and surface VERDICT's faults, if any."
  (agent-repl-daemon--report-provenance address)
  (pcase (plist-get verdict :arm)
    (:healthy
     (agent-repl--info nil "elisp.daemon.adopted address=%S health=healthy" address))
    (:unhealthy
     (let ((faults (plist-get (plist-get verdict :value) :faults)))
       (agent-repl--warn nil "elisp.daemon.adopted-unhealthy address=%S faults=%d"
                         address (length faults))
       (agent-repl-daemon--render-health address faults)))
    (_
     (agent-repl--info nil "elisp.daemon.adopted address=%S health=unstated" address))))

;;;; ---- The boot wait ----

(defun agent-repl-daemon--cancel-boot-wait ()
  "Cancel any pending boot poll and forget its continuation."
  (when (timerp agent-repl-daemon--boot-timer)
    (cancel-timer agent-repl-daemon--boot-timer))
  (setq agent-repl-daemon--boot-timer nil
        agent-repl-daemon--boot-deadline nil
        agent-repl-daemon--boot-continuation nil))

(defun agent-repl-daemon--await-address (on-ready)
  "Poll `daemon.addr' until it appears, then call ON-READY with the address.
ON-READY receives nil when the boot timeout elapses first.  A TIMER poll,
never a sleep: the daemon writes its address atomically when it is ready
to serve, so the file's appearance IS readiness and nothing else has to
be probed."
  (agent-repl-daemon--cancel-boot-wait)
  (setq agent-repl-daemon--boot-deadline
        (+ (float-time) agent-repl-daemon-boot-timeout-seconds)
        agent-repl-daemon--boot-continuation on-ready)
  (agent-repl-daemon--boot-tick))

(defun agent-repl-daemon--boot-tick ()
  "One poll of the boot wait: the timer's whole body.
Named and argument-free so a test drives the wait by calling it, with no
dependence on the scheduler and no sleep anywhere."
  (setq agent-repl-daemon--boot-timer nil)
  (let ((address (condition-case err
                     (agent-repl-connect-read-daemon-addr)
                   (error
                    (agent-repl--warn nil "elisp.daemon.boot-addr-unreadable error=%S" err)
                    nil)))
        (on-ready agent-repl-daemon--boot-continuation)
        (deadline (or agent-repl-daemon--boot-deadline 0)))
    (cond
     (address
      (agent-repl--info nil "elisp.daemon.booted address=%S" address)
      (agent-repl-daemon--cancel-boot-wait)
      (when on-ready (funcall on-ready address)))
     ((>= (float-time) deadline)
      (agent-repl--error nil "elisp.daemon.boot-timeout seconds=%.1f file=%S"
                         agent-repl-daemon-boot-timeout-seconds
                         (agent-repl-connect-daemon-addr-file))
      (agent-repl-daemon--cancel-boot-wait)
      (when on-ready (funcall on-ready nil)))
     (t
      (agent-repl--log nil "elisp.daemon.boot-waiting remaining=%.3f"
                       (- deadline (float-time)))
      (setq agent-repl-daemon--boot-timer
            (run-with-timer agent-repl-daemon-boot-poll-interval-seconds nil
                            #'agent-repl-daemon--boot-tick))))))

(defun agent-repl-daemon--cancel-departure-wait ()
  "Cancel any pending departure poll and forget its continuation."
  (when (timerp agent-repl-daemon--departure-timer)
    (cancel-timer agent-repl-daemon--departure-timer))
  (setq agent-repl-daemon--departure-timer nil
        agent-repl-daemon--departure-deadline nil
        agent-repl-daemon--departure-continuation nil))

(defun agent-repl-daemon--await-departure (on-gone)
  "Poll until the stopped daemon removes `daemon.addr', then call ON-GONE.
ON-GONE receives non-nil when the file went away and nil when the wait
timed out with it still there.  A TIMER poll, never a sleep, and the
mirror image of `agent-repl-daemon--await-address': a daemon asked to
shut down removes its address as the last thing it does, so the file's
disappearance IS its departure."
  (agent-repl-daemon--cancel-departure-wait)
  (setq agent-repl-daemon--departure-deadline
        (+ (float-time) agent-repl-daemon-boot-timeout-seconds)
        agent-repl-daemon--departure-continuation on-gone)
  (agent-repl-daemon--departure-tick))

(defun agent-repl-daemon--departure-tick ()
  "One poll of the departure wait: the timer's whole body.
Named and argument-free so a test drives the wait by calling it, with no
dependence on the scheduler and no sleep anywhere."
  (setq agent-repl-daemon--departure-timer nil)
  (let ((address (condition-case err
                     (agent-repl-connect-read-daemon-addr)
                   (error
                    (agent-repl--warn nil "elisp.daemon.departure-addr-unreadable error=%S" err)
                    nil)))
        (on-gone agent-repl-daemon--departure-continuation)
        (deadline (or agent-repl-daemon--departure-deadline 0)))
    (cond
     ((null address)
      (agent-repl--info nil "elisp.daemon.departed")
      (agent-repl-daemon--cancel-departure-wait)
      (when on-gone (funcall on-gone t)))
     ((>= (float-time) deadline)
      (agent-repl--warn nil "elisp.daemon.departure-timeout seconds=%.1f address=%S file=%S"
                        agent-repl-daemon-boot-timeout-seconds address
                        (agent-repl-connect-daemon-addr-file))
      (agent-repl-daemon--cancel-departure-wait)
      (when on-gone (funcall on-gone nil)))
     (t
      (agent-repl--log nil "elisp.daemon.departure-waiting address=%S remaining=%.3f"
                       address (- deadline (float-time)))
      (setq agent-repl-daemon--departure-timer
            (run-with-timer agent-repl-daemon-boot-poll-interval-seconds nil
                            #'agent-repl-daemon--departure-tick))))))

;;;; ---- The start ----

(defun agent-repl-daemon--sentinel (proc event)
  "Record the daemon PROC's EVENT.  Emacs supervises, it does not restart.
Everything after boot is the daemon's own blue-green rollout, so a daemon
that exits is a fact to record — daemon-link.el's reconnect is what
notices and recovers."
  (unless (process-live-p proc)
    (agent-repl--warn nil "elisp.daemon.exited status=%S event=%s"
                      (process-exit-status proc) (string-trim (or event "")))
    (when (eq proc agent-repl--frontend-daemon-process)
      (setq agent-repl--frontend-daemon-process nil))))

(defun agent-repl-daemon--environment ()
  "Return the spawn environment, with `AGENT_REPL_STATE_DIR' set EXPLICITLY.
ONE state root is the cross-system contract; a child that inherited a
different one would be a silent split-brain, so the value Emacs resolved
is stated on the spawn rather than assumed."
  (cons (format "AGENT_REPL_STATE_DIR=%s"
                (directory-file-name (agent-repl--global-state-dir)))
        (cl-remove-if (lambda (entry) (string-prefix-p "AGENT_REPL_STATE_DIR=" entry))
                      process-environment)))

(defun agent-repl-daemon--start ()
  "Spawn the daemon and return its process, or nil when the binary is absent."
  (let ((binary (car agent-repl-daemon-command)))
    (if (not (agent-repl--frontend-artifact-exists-p binary))
        (progn
          (agent-repl--error nil "elisp.daemon.binary-missing binary=%S" binary)
          nil)
      (agent-repl--backend-phase nil "starting the daemon...")
      (let ((proc (agent-repl--frontend-spawn-daemon
                   agent-repl-daemon-command
                   (agent-repl-daemon--environment))))
        (setq agent-repl--frontend-daemon-process proc)
        (agent-repl--info nil "elisp.daemon.started argv=%S state-dir=%S"
                          agent-repl-daemon-command (agent-repl--global-state-dir))
        proc))))

;;;; ---- The entry point ----

(defun agent-repl-daemon--settle (on-ready outcome)
  "End the ensure with OUTCOME and hand it to ON-READY."
  (setq agent-repl-daemon--ensure-in-flight nil)
  (agent-repl--info nil "elisp.daemon.ensure-settled ready=%s"
                    (if outcome "t" "nil"))
  (when on-ready (funcall on-ready outcome))
  outcome)

(defun agent-repl-daemon--build-and-start (on-ready)
  "Build, start a daemon, wait for its address, and link to it.
ON-READY receives the connection, or nil.  The build is asynchronous, so
everything after it lives in the continuation."
  (agent-repl-daemon--build
   nil
   (lambda (failure)
     (if failure
         (progn
           (agent-repl-daemon--report-build-failure failure)
           (agent-repl-daemon--settle on-ready nil))
       (agent-repl-daemon--clear-build-failure)
       (if (null (agent-repl-daemon--start))
           (agent-repl-daemon--settle on-ready nil)
         (agent-repl-daemon--await-address
          (lambda (address)
           (if (null address)
               (agent-repl-daemon--settle on-ready nil)
             ;; STATE whose daemon this is on the own-spawn path too.  The
             ;; adopt path reports provenance from the probe verdict, but a
             ;; cold start never probes -- and a log that only ever carries
             ;; `foreign-adopted' would say the session attached to someone
             ;; else's daemon every single time it started its own.
             (agent-repl-daemon--report-provenance address)
              (agent-repl--info nil "elisp.daemon.linking address=%S" address)
              (agent-repl-daemon--settle on-ready (agent-repl-link-connect))))))))))

(defun agent-repl-daemon--begin (on-ready)
  "Take the cold-start decision for `agent-repl-daemon-ensure'.
Separated from the entry point so the in-flight flag's release covers the
WHOLE decision, signals included."
  (let ((address (condition-case err
                     (agent-repl-connect-read-daemon-addr)
                   (error
                    (agent-repl--warn nil "elisp.daemon.addr-unreadable error=%S" err)
                    nil))))
    (if (null address)
        (progn
          (agent-repl--info nil "elisp.daemon.addr-absent")
          (agent-repl-daemon--build-and-start on-ready))
      (agent-repl-daemon--probe
       address
       (lambda (verdict)
         (agent-repl-daemon--report-verdict address verdict)
         (agent-repl-daemon--settle on-ready (agent-repl-link-connect)))
       (lambda (detail)
         (agent-repl--warn nil "elisp.daemon.stale-addr address=%S detail=%S"
                           address detail)
         (agent-repl-daemon--build-and-start on-ready))))))

(defun agent-repl-daemon-schedule-ensure ()
  "Schedule `agent-repl-daemon-ensure' on an idle timer and return the timer.
THE FRAME PAINTS FIRST.  This is what the startup hook registers, and the
whole reason it exists: `emacs-startup-hook' runs BEFORE Doom's UI init
and before the first redisplay, so anything it does synchronously — even
deciding whether a build is needed — happens while there is no frame on
screen.  An idle timer cannot run until the command loop is idle, which
is after that first redisplay."
  (setq agent-repl-daemon--startup-timer
        (run-with-idle-timer agent-repl-daemon-startup-idle-seconds nil
                             #'agent-repl-daemon-ensure))
  (agent-repl--info nil "elisp.daemon.ensure-scheduled idle=%s"
                    agent-repl-daemon-startup-idle-seconds)
  agent-repl-daemon--startup-timer)

(defun agent-repl-daemon-ensure (&optional on-ready)
  "Make sure a daemon is serving, adopting one wherever one answers.
ON-READY receives the primary connection, or nil when no daemon could be
brought up.  The whole decision, in order:

  - a link already stands            => nothing to do
  - `daemon.addr' absent             => build, start, wait, link
  - present and the daemon ANSWERS   => ADOPT it (healthy or not) and link
  - present and NOTHING answers      => a stale file; treat it as absent

Emacs NEVER kills a daemon that answers.  Idempotent, and refuses to run
twice at once: a concurrent ensure is exactly how a second daemon gets
spawned beside a live one."
  (cond
   ((agent-repl-link-up-p)
    (agent-repl--log nil "elisp.daemon.ensure-noop reason=link-up")
    (when on-ready (funcall on-ready (agent-repl-link-primary)))
    (agent-repl-link-primary))
   (agent-repl-daemon--ensure-in-flight
    (agent-repl--info nil "elisp.daemon.ensure-already-in-flight")
    nil)
   (t
    (setq agent-repl-daemon--ensure-in-flight t)
    ;; A SIGNAL out of the ensure must not leave the in-flight flag raised:
    ;; the flag is what refuses a second ensure, so one stuck at t disables
    ;; cold start for the rest of the session — silently, because every
    ;; later ensure would look like an ordinary concurrent one.  The signal
    ;; itself is re-raised, never swallowed.
    (condition-case err
        (agent-repl-daemon--begin on-ready)
      (error
       (setq agent-repl-daemon--ensure-in-flight nil)
       (agent-repl--error nil "elisp.daemon.ensure-failed error=%S" err)
       (signal (car err) (cdr err)))))))

;;;; ---- Interactive commands ----

(defun agent-repl-frontend-daemon-ensure ()
  "Ensure a daemon is serving; also the retry after a build failure."
  (interactive)
  (agent-repl--info nil "elisp.daemon.ensure-command")
  (agent-repl-daemon-ensure
   (lambda (conn)
     (message "agent-repl: %s"
              (if conn "daemon ready" "daemon NOT ready - see the log")))))

(defun agent-repl-frontend-daemon-stop (&optional on-done)
  "Ask the daemon to shut down now.  EMACS NEVER KILLS A DAEMON.
The stop is `UpdateShutdownSchedule{now}' with an operator reason naming
this editor — the daemon stops accepting work, flushes its in-flight
writes and exits itself, which is the only shutdown that strands
nothing.

ON-DONE, when given, is called with non-nil once the daemon ACCEPTED the
shutdown and nil on any refusal or transport failure; interactive callers
leave it out."
  (interactive)
  (let ((conn (agent-repl-link-primary)))
    (if (null conn)
        (progn
          (agent-repl--warn nil "elisp.daemon.stop-skipped reason=no-link")
          (message "agent-repl: no daemon link to stop")
          (when on-done (funcall on-done nil))
          nil)
      (agent-repl--info nil "elisp.daemon.stop")
      (agent-repl-rpc-update-shutdown-schedule
       conn (list :action
                  (list :arm :now
                        :value (list :reason (list :arm :operator
                                                   :value (list :note "emacs")))))
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success
            (agent-repl--info nil "elisp.daemon.stop-accepted")
            (message "agent-repl: daemon shutting down")
            (when on-done (funcall on-done t)))
           (:error
            (agent-repl--error nil "elisp.daemon.stop-refused error=%S"
                               (plist-get response :value))
            (message "agent-repl: daemon refused the shutdown")
            (when on-done (funcall on-done nil)))
           (arm
            (agent-repl--error nil "elisp.daemon.stop-unknown-arm arm=%S" arm)
            (when on-done (funcall on-done nil)))))
       :on-failure
       (lambda (detail)
         (agent-repl--error nil "elisp.daemon.stop-failed detail=%S" detail)
         (message "agent-repl: could not reach the daemon to stop it")
         (when on-done (funcall on-done nil))))
      t)))

(defun agent-repl-frontend-daemon-restart ()
  "Stop the daemon and ensure one again.
Emacs's own restart, distinct from the daemon's blue-green rollout: the
daemon is ASKED to exit, the link tears down, and the ensure brings a
fresh one up from a fresh build.

The three steps are SEQUENCED on the stop, not merely written in order.
The stop is an async rpc, so an ensure fired beside it runs while the
departing daemon is still answering -- and an ensure that finds a daemon
ADOPTS it, because Emacs never kills a daemon that answers.  The restart
would then re-adopt the daemon it had just asked to leave, no fresh build
would ever run, and the only visible trace would be a daemon exiting some
moments later with nothing left to reconnect to.  So: the stop's ack,
then the teardown, then the wait for the address to go away, and only
then the ensure.

With no link standing there is nothing to stop, and the restart is just
the ensure."
  (interactive)
  (agent-repl--info nil "elisp.daemon.restart")
  (if (null (agent-repl-link-primary))
      (progn
        (agent-repl--info nil "elisp.daemon.restart-nothing-to-stop reason=no-link")
        (agent-repl-daemon-ensure))
    (agent-repl-frontend-daemon-stop
     (lambda (accepted)
       (if (not accepted)
           ;; The daemon refused or never answered; it is still serving.
           ;; Ensuring here would adopt it and report a restart that did
           ;; not happen, so the refusal stands and the link is left alone.
           (progn
             (agent-repl--error nil "elisp.daemon.restart-abandoned reason=stop-not-accepted accepted=%S"
                                accepted)
             (message "agent-repl: the daemon did not accept the stop; not restarting"))
         (agent-repl--info nil "elisp.daemon.restart-stop-accepted")
         (agent-repl-link-teardown)
         (agent-repl-daemon--await-departure
          (lambda (gone)
            (unless gone
              ;; The address outlived the wait.  Ensure anyway -- the link
              ;; is already down, and leaving the user with no daemon at
              ;; all is strictly worse than an ensure that may re-adopt.
              (agent-repl--warn nil "elisp.daemon.restart-ensures-despite-timeout gone=%S" gone))
            (agent-repl--info nil "elisp.daemon.restart-ensure gone=%S" gone)
            (agent-repl-daemon-ensure))))))))

(add-hook 'agent-repl-link-no-daemon-functions #'agent-repl-daemon-ensure)
(agent-repl-daemon-install-segment)

(provide 'daemon)

;;; daemon.el ends here
