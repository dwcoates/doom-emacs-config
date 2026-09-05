;;; config.el --- sandbox profile config -*- lexical-binding: t; -*-

;;; Commentary:
;; Deliberately near-empty: the sandbox must not add behavior the host
;; profile does not have, or a test could pass here and fail there. The only
;; settings are the two the module's own templates read from the
;; environment, defaulted so nothing prompts.
;;; Code:

(setq user-full-name    (or (getenv "EMACS_FULL_NAME") "agent-repl sandbox")
      user-mail-address (or (getenv "EMACS_EMAIL") "sandbox@invalid"))

;; The e2e suite never runs real git, and a sandbox run has no network:
;; refuse to let anything block on a prompt.
(setq confirm-kill-emacs nil)

;; ---------------------------------------------------------------------------
;; The e2e Emacs client layer's boot hook
;; ---------------------------------------------------------------------------
;;
;; `e2e/emacs_test.go' drives the module through THIS Doom profile rather than
;; through a bare `emacs -Q', so `map!', `set-popup-rule!' and Doom's own
;; module load order are all real. What the Go layer needs back from Doom is
;; exactly two things, and they are the only two things this block adds:
;;
;;   (a) a way to know Doom FINISHED initializing. The stamp file named by
;;       AGENT_REPL_E2E_READY is written last, after the server socket exists,
;;       so its appearance means "Doom is up AND emacsclient will answer".
;;       A failure during boot writes the SAME file with "ok": false and the
;;       elisp error, so the Go side reports the elisp error instead of timing
;;       out on a socket that is never coming.
;;
;;   (b) the file-based readback the harness already uses:
;;       `agent-repl-e2e--eval' reads a form from one file and writes a JSON
;;       result to another, so nothing has to survive shell quoting or elisp
;;       print escaping, and an elisp error becomes a NAMED Go failure rather
;;       than an opaque emacsclient exit status.
;;
;; Everything here is gated on AGENT_REPL_E2E_EMACS, which only the Go layer
;; sets: an interactive `e2e-sandbox.sh shell' Emacs is unaffected.

(agent-repl-e2e--breadcrumb "config.el reached")

(defun agent-repl-e2e--eval (in out)
  "Evaluate the form in file IN and write a JSON result to file OUT.
The result object carries an \"ok\" boolean plus either a \"value\" or an
\"error\", so an elisp failure reaches the Go side as itself."
  (require 'json)
  (let ((payload
         (condition-case err
             (let ((value (eval (car (read-from-string
                                      (with-temp-buffer
                                        (insert-file-contents in)
                                        (buffer-string))))
                                t)))
               ;; A value JSON cannot carry (a process, a buffer, a marker)
               ;; is handed over PRINTED rather than signalled: the form ran,
               ;; and a signal here would look to the Go side exactly like a
               ;; hung Emacs -- emacsclient never returns.
               (list (cons "ok" t)
                     (cons "value" (condition-case nil
                                       (progn (json-encode value) value)
                                     (error (format "%S" value))))))
           (error (list (cons "ok" :json-false)
                        (cons "error" (error-message-string err)))))))
    (with-temp-file out
      (insert (json-encode payload))))
  t)

(defun agent-repl-e2e--stack ()
  "Answer the CURRENT elisp stack as a one-line chain of function names.

WHY THIS EXISTS. When a scenario's own eval runs past `evalBound' the Go
side kills its `emacsclient' and reports \"signal: killed\", which names
neither what Emacs was doing nor where.  Emacs is single-threaded and
serves `emacsclient --eval' from a process filter, so a probe sent WHILE
the slow form is still running is evaluated NESTED INSIDE it -- and its
own backtrace therefore carries the stuck form's frames underneath.  That
makes a second, ordinary eval a complete diagnosis of the first, with no
signal, no debugger and no `debug-on-event' frame to interpret.

Frames are reported innermost first, as bare function names: what is
wanted is which call is standing, and printing the arguments of a frame
holding a whole roster would bury it."
  (require 'backtrace)
  (mapconcat (lambda (frame)
               (let ((fun (backtrace-frame-fun frame)))
                 (if (symbolp fun) (symbol-name fun) "<lambda>")))
             (backtrace-get-frames)
             " <- "))

(defun agent-repl-e2e--cpu-profile (n)
  "Answer the N hottest sampled call chains, one per line, hottest first.

Read from `profiler-cpu-log' rather than rendered through
`profiler-report': the report buffer is a collapsed interactive tree, and
what a failing run needs is the flat text of where the samples landed.

Each line is a sample count and the chain innermost-first.  Calling this
takes the accumulated log and leaves the profiler running, so a second
call answers only what has happened since the first."
  (require 'profiler)
  (let (entries)
    (maphash (lambda (chain count) (push (cons count chain) entries))
             (profiler-cpu-log))
    (setq entries (sort entries (lambda (a b) (> (car a) (car b)))))
    (mapconcat
     (lambda (entry)
       (format "%6d  %s"
               (car entry)
               (mapconcat (lambda (frame) (format "%s" frame))
                          (seq-remove #'null (append (cdr entry) nil))
                          " <- ")))
     (seq-take entries n)
     "\n")))

(defun agent-repl-e2e--socket-occupant (path)
  "Describe whatever already occupies PATH, or nil when nothing does.

WHY THIS EXISTS, and it is a measurement rather than a precaution.
`server-start' answers EXACTLY ONE failure with \"Cannot bind server
socket: Address already in use\", and it is not the one a reader assumes.
A stale socket at the path is deleted by `server-stop' and rebound; a LIVE
socket -- even one another process owns -- is reported as a warning
(\"There is an existing Emacs server\") and never as that error.  The bind
only fails that way when the entry could not be deleted, which
`server-stop' swallows in an `ignore-errors': a DIRECTORY at the path, or
an entry this user may not unlink.  Verified against Emacs 30's own
`server.el' by running all three cases.

So the error names nothing, and the one fact that would explain it -- what
is actually sitting there -- is discarded.  This reads it BEFORE the bind,
while it is still there to read.  The scratch root is unique per scenario,
so anything at all at this path is a defect in this harness and the
description is the whole of the evidence for it."
  (let ((attrs (file-attributes path 'string)))
    (when attrs
      (format "type=%s mode=%s owner=%s size=%s modified=%s"
              (cond ((eq (file-attribute-type attrs) t) "directory")
                    ((stringp (file-attribute-type attrs))
                     (format "symlink->%s" (file-attribute-type attrs)))
                    ((file-attribute-type attrs) "other")
                    (t "regular-or-socket"))
              (file-attribute-modes attrs)
              (file-attribute-user-id attrs)
              (file-attribute-size attrs)
              (format-time-string "%FT%T" (file-attribute-modification-time attrs))))))

(defun agent-repl-e2e--write-stamp (path payload)
  "Write PAYLOAD as JSON to PATH, atomically via a rename."
  (require 'json)
  (let ((tmp (concat path ".partial")))
    (with-temp-file tmp
      (insert (json-encode payload)))
    (rename-file tmp path t)))

(defun agent-repl-e2e--unblind-server-errors ()
  "Stop a failed `emacsclient --eval' from freezing Emacs for ten seconds.

WHAT THIS IS FOR. `server-execute' answers a form that SIGNALS by calling
`server-return-error', which sends the error to the client, deletes the
connection -- and then calls `sit-for' to give the socket time to drain.
`sit-for' is a wait on input: for its whole duration Emacs serves NOTHING,
not the next scenario step and not the heartbeat probe, whose bound is
`HeartbeatBound' and is a small fraction of it.

So in this layer one signalling eval did not READ as a signalling eval.  It
read as a wedged editor: the heartbeat missed, `declareWedged' fired, the
scenario was reported as the sentinel-recursion hang this layer exists to
catch, and the error that actually caused it reached the log only afterwards
as \"signal: killed\".  Measured 2026-09-04 on
TestEmacsHistoryRecallRestoresTheLastPrompt, whose native backtrace stood in
`sit-for' under `server-return-error' with `seconds=10'.

The drain the `sit-for' buys is worth nothing here -- the Go side reads the
error off `emacsclient''s own exit, and the process it would drain to is
already being torn down -- while the blindness it buys is total.  So the
wait is dropped for the duration of that one function and nothing else:
errors are still returned, still logged, and still fail their scenario, as
themselves."
  (require 'cl-lib)
  (advice-add
   'server-return-error :around
   (lambda (fn &rest args)
     (cl-letf (((symbol-function 'sit-for) (lambda (&rest _) t)))
       (apply fn args)))
   '((name . agent-repl-e2e--no-sit-for))))

(defun agent-repl-e2e--boot ()
  "Arm the e2e server and publish the readiness stamp.
Runs after Doom has finished initializing, which is the earliest moment at
which `map!' bindings, popup rules and every module's `config.el' are all in
effect."
  (agent-repl-e2e--breadcrumb "boot hook entered")
  (let ((ready (getenv "AGENT_REPL_E2E_READY"))
        (socket (getenv "AGENT_REPL_E2E_SERVER")))
    ;; THE HANDLER IS `t', NOT `error', AND `(require 'server)' IS INSIDE IT.
    ;;
    ;; A boot that leaves this hook by ANY non-local exit publishes no stamp
    ;; and writes no further breadcrumb, and the Go side then reports only
    ;; "no readiness stamp" for a socket that is never coming -- the exact
    ;; blindness the stamp's failed arm exists to prevent. Two ways out were
    ;; still uncovered: `(require 'server)' sat OUTSIDE the guard entirely,
    ;; and a `quit' -- which is a signal but not an `error' -- would have
    ;; walked straight through an `error' handler. Both now land in the arm
    ;; that names what happened.
    (condition-case err
        (progn
          (require 'server)
          (agent-repl-e2e--breadcrumb "server feature loaded")
          (agent-repl-e2e--unblind-server-errors)
          (unless (and socket (not (string-empty-p socket)))
            (error "AGENT_REPL_E2E_SERVER is unset"))
          ;; A real frame with a tab-bar: the layer asserts window and tab
          ;; state, and both need the modes actually on.
          ;;
          ;; persp-mode IS LOADED FIRST, and that ordering is load-bearing.
          ;; Doom's `:ui workspaces' hangs
          ;; `+workspaces-set-up-tab-bar-integration-h' on
          ;; `tab-bar-mode-hook', and that handler calls straight into
          ;; persp-mode (`safe-persp-name', `get-current-persp'). persp-mode
          ;; itself is deferred by Doom until a file or buffer edge that a
          ;; headless boot never reaches, so turning the tab bar on first
          ;; aborted this hook with "Symbol's function definition is void:
          ;; safe-persp-name" -- and the boot published a failed stamp
          ;; instead of a server. Requiring it is also honest about what the
          ;; scenarios need: `workspace.el' drives perspectives, so a
          ;; persp-mode that was never loaded would not have served them
          ;; anyway.
          (require 'persp-mode)
          (agent-repl-e2e--breadcrumb "persp-mode loaded")
          (tab-bar-mode 1)
          (setq server-name socket)
          ;; THE OCCUPANT IS READ BEFORE THE BIND, AND THE BIND STILL RUNS.
          ;; `server-start' RECOVERS from a stale socket by deleting it, so an
          ;; occupant is not by itself a failure and this must not turn one
          ;; into a failure.  What it must not do is let the one failure it
          ;; cannot recover from report nothing: the occupant is only readable
          ;; while it is still there, and `server-start' deletes it on the way
          ;; past.  So it is read here and reported only if the bind then
          ;; fails -- at which point it is the whole of the evidence, because
          ;; the scratch root is unique per scenario and nothing outside this
          ;; run can have written to it.
          (let ((occupant (agent-repl-e2e--socket-occupant socket)))
            (when occupant
              (agent-repl-e2e--breadcrumb
               (format "server socket path is already occupied: %s -- %s" socket occupant)))
            (condition-case bind-err
                (server-start)
              (error
               (signal (car bind-err)
                       (list (format "%s [socket %s; occupant before the bind: %s]"
                                     (error-message-string bind-err) socket
                                     (or occupant "nothing")))))))
          (agent-repl-e2e--breadcrumb "server started")
          (when ready
            (agent-repl-e2e--write-stamp
             ready
             ;; Facts the Go side asserts rather than assumes: that this is
             ;; the Doom boot and not a `-Q' one, and that `map!' really did
             ;; expand, which is what puts keybindings back in scope.
             (list (cons "ok" t)
                   (cons "pid" (emacs-pid))
                   (cons "emacs_version" emacs-version)
                   (cons "doom" (if (featurep 'doom) t :json-false))
                   (cons "doom_version" (if (boundp 'doom-version) doom-version ""))
                   (cons "map_bang" (if (fboundp 'map!) t :json-false))
                   (cons "popup_rule" (if (fboundp 'set-popup-rule!) t :json-false))
                   (cons "agent_repl" (if (featurep 'agent-repl) t :json-false))
                   (cons "server_name" server-name))))
          ;; THE SAMPLING PROFILER, ARMED FOR THE WHOLE SCENARIO, AND ARMED
          ;; AFTER THE STAMP. A wedge in this layer is a command loop burning
          ;; CPU (`state=R' in the Go side's kernel snapshot), and while it
          ;; burns Emacs answers NOTHING -- not the server socket, not a
          ;; nested eval, not the debugger. Nothing can be ASKED of a wedged
          ;; Emacs; what can be done is to have it already recording, and read
          ;; the recording out afterwards.
          ;;
          ;; It comes after the stamp because the stamp is what `doomBootBound'
          ;; is measured against, and a diagnostic must not be inside the
          ;; phase it exists to explain.
          (require 'profiler)
          (profiler-start 'cpu))
      (t
       ;; Never leave the Go side waiting on a socket that is not coming.
       ;; The breadcrumb is written FIRST and separately from the stamp: the
       ;; stamp write is itself a way this can come up empty, and the
       ;; breadcrumb file is one `write-region' with nothing else to fail.
       (agent-repl-e2e--breadcrumb
        (format "boot hook aborted: %s" (error-message-string err)))
       (when ready
         (ignore-errors
           (agent-repl-e2e--write-stamp
            ready
            (list (cons "ok" :json-false)
                  (cons "error" (error-message-string err))))))
       (signal (car err) (cdr err))))))

(when (getenv "AGENT_REPL_E2E_EMACS")
  (setq inhibit-startup-screen t
        confirm-kill-processes nil)
  ;; `doom-after-init-hook' is Doom's own "everything is loaded" edge; older
  ;; Doom revisions do not have it, and `emacs-startup-hook' is the stock
  ;; equivalent that runs after `init.el' -- and therefore after every Doom
  ;; module's `config.el' -- has finished. DOOM_REF is a build argument, so
  ;; the profile must not assume which of the two exists.
  (add-hook (if (boundp 'doom-after-init-hook)
                'doom-after-init-hook
              'emacs-startup-hook)
            #'agent-repl-e2e--boot
            ;; Last, so the module's own cold-start registration on
            ;; `emacs-startup-hook' has already run when the stamp lands.
            90))

;; ---------------------------------------------------------------------------
;; The subject under test
;; ---------------------------------------------------------------------------
;;
;; Loaded HERE rather than through `:app agent-repl' in init.el, mirroring the
;; host profile exactly (see the comment where init.el's `:app' section used
;; to be). $DOOMDIR/config.el is the last thing Doom loads, after every
;; module's own config.el including `:config default +bindings', so the
;; module's `map! :leader' forms are the ones that survive.
;;
;; `doom-after-modules-config-hook' is NOT an option: it has already fired by
;; the time this file runs, so an `add-hook' here would be dead code on a cold
;; boot -- the same trap documented in the host's config.el.
(unless (featurep 'agent-repl)
  (load! "modules/app/agent-repl/config"))
