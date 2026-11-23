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
;;
;; A LOAD NEVER UNDEFINES, so the reload retires what the new build dropped.
;; Loading a file replaces its `load-history' entry and defines what the new
;; source defines, but a function the new source no longer defines stays
;; callable, still on every hook and timer that named it.  On 2026-11-22 a
;; retired `agent-repl-page-unload-on-reattached' stayed on
;; `agent-repl-host-reattached-functions' after a deploy and failed every
;; reattach with the old signature's arity, inside RegisterWorkspace's
;; answer.  After a clean load, an agent-repl function no loaded file
;; defines (`symbol-file' answers nil) is exactly such a leftover: it is
;; taken off every hook and timer and unbound
;; \=(`agent-repl--elisp-retire-undefined').  bytecode.el's compile installs
;; its byte-code with `fset' from a timer, outside any load, so a compiled
;; function keeps the `symbol-file' its load recorded.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--now "core" ())
(declare-function agent-repl--assert-heartbeat-armed "core" ())
(declare-function agent-repl--elisp-file-sha256 "config" (file))
(declare-function agent-repl--elisp-module-file "config" (root module))
(declare-function agent-repl-latency-report-editor "latency-report" (kind seconds))
(declare-function agent-repl-bytecode-schedule "bytecode" (root modules))

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
                       "elisp.elisp-build.no-modules-recorded reason={reason}"
                       :reason "config.el recorded no loaded module, so this Emacs has no elisp build to report"))
  (let ((build (agent-repl-elisp-build-of agent-repl--elisp-module-builds)))
    (agent-repl--log agent-repl--elisp-build-log-scope
                     "elisp.elisp-build.current modules={modules} build={build}"
                     :modules (length agent-repl--elisp-module-builds) :build build)
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

;;;; ---- A reload's changed defaults ----
;;
;; RE-EVALUATING A DEFCUSTOM NEVER RESETS A BOUND VARIABLE: the load
;; records the new default (its `standard-value') and leaves the value as
;; it was, so a landed default never reached a running editor until a
;; restart (the composer's ground, 2026-11-02).  So every reload snapshots
;; each agent-repl option's declared default before it loads and, after,
;; gives each option whose default changed the new one through Custom
;; \=(`custom-reevaluate-setting', which runs the option's `:set').
;;
;; AN OPTION SOMEONE SET KEEPS ITS VALUE.  One Custom recorded a value for
;; (saved, customized or set by a theme) is customized.  One whose value is
;; no longer the default the previous load declared was set outside Custom
;; (a `setq' or `add-to-list' in an init file, a hook's functions, a minor
;; mode turned on), and is just as much the user's.  Only an option still
;; holding the old default takes the new one.

(defun agent-repl--elisp-option-p (symbol)
  "Return non-nil when SYMBOL is a bound agent-repl user option."
  (and (custom-variable-p symbol)
       (boundp symbol)
       (string-prefix-p "agent-repl" (symbol-name symbol))
       (get symbol 'standard-value)))

(defun agent-repl--elisp-option-default (symbol)
  "Return (t . VALUE), SYMBOL's declared default evaluated, or nil on an error.
A default that does not evaluate is an ERROR, and the option is left out
of the comparison, since no change of it can be told."
  (condition-case err
      (cons t (eval (car (get symbol 'standard-value)) t))
    (error
     (agent-repl--error agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.default-unevaluable symbol={symbol} form={form} error={error}"
                        :symbol symbol :form (prin1-to-string (car (get symbol 'standard-value))) :error (prin1-to-string err))
     nil)))

(defun agent-repl--elisp-defaults-snapshot ()
  "Return (SYMBOL . DEFAULT) for every agent-repl option, DEFAULT its value now."
  (let (snapshot)
    (mapatoms (lambda (symbol)
                (when (agent-repl--elisp-option-p symbol)
                  (when-let ((default (agent-repl--elisp-option-default symbol)))
                    (push (cons symbol (cdr default)) snapshot)))))
    snapshot))

(defun agent-repl--elisp-defaults-apply (snapshot)
  "Give every option of SNAPSHOT whose default changed the new default.
SNAPSHOT is `agent-repl--elisp-defaults-snapshot''s answer from before
the load.  An option customized, or set outside Custom, keeps its value,
and the kept change is recorded.  Returns the options given the new
default, in SNAPSHOT order."
  (let (applied)
    (dolist (entry snapshot)
      (let* ((symbol (car entry))
             (old (cdr entry))
             (new (agent-repl--elisp-option-default symbol)))
        (when (and new (not (equal (cdr new) old)))
          (cond
           ((or (get symbol 'saved-value) (get symbol 'customized-value) (get symbol 'theme-value))
            (agent-repl--log agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.default-kept symbol={symbol} reason=customized default={default}"
                             :symbol symbol :default (prin1-to-string (cdr new))))
           ((not (equal (default-value symbol) old))
            (agent-repl--log agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.default-kept symbol={symbol} reason=set-outside-custom value={value} default={default}"
                             :symbol symbol :value (prin1-to-string (default-value symbol)) :default (prin1-to-string (cdr new))))
           (t
            (agent-repl--info agent-repl--elisp-build-log-scope
                              "elisp.elisp-build.default-applied symbol={symbol} from={from} to={to}"
                              :symbol symbol :from (prin1-to-string old) :to (prin1-to-string (cdr new)))
            (custom-reevaluate-setting symbol)
            (push symbol applied))))))
    (nreverse applied)))

;;;; ---- Loading a build's files ----
;;
;; FROM THE FIRST LOAD TO THE LAST, NOTHING OF AGENT-REPL'S IS CALLED BY
;; NAME.  The code loading the files is the build being replaced, and any
;; load may redefine any agent-repl symbol, changing its kind or its
;; signature.  On 2026-11-22 the first module of a deploy's reload
;; \(log-site.el) turned `agent-repl--log' from a function into a macro; the
;; replaced loop's next log call signalled `invalid-function', and the reload
;; died after one module, leaving the editor half-reloaded with its logging
;; dead.  So everything the loop needs is computed before the first load
;; \(each file, its hash, the load wrapper's function object), the loop runs
;; only Emacs primitives and that captured object and answers each file's
;; outcome as plain data, and everything after the last load is reached by a
;; call by name, so it is the code of the build just loaded: ONE call, to
;; `agent-repl--elisp-reload-finish' (a hot load's
;; `agent-repl--elisp-hot-load-finish'), with ONE plist whose keys are only
;; ever added, since the build calling it is always the one before.
;;
;; A CLOSURE MADE BEFORE THE LOAD STILL CALLS BY THE OLD KIND.  A timer, a
;; process filter or a hook closure that calls a symbol the load turned from
;; a function into a macro (or back) fails until it is made again, so the
;; finish names every such symbol at ERROR
;; \(`agent-repl--elisp-report-kind-changes'); it repairs nothing.

(defun agent-repl--elisp-normalize-root (root)
  "Return ROOT as an absolute directory name, for comparing module roots."
  (file-name-as-directory (expand-file-name root)))

(defun agent-repl--elisp-reload-load-file (file)
  "External-boundary wrapper: `load' the elisp source FILE into this Emacs.
The exact FILE, never a `.elc' beside it (NOSUFFIX), quietly."
  (load file nil t t))

(defun agent-repl--elisp-load-files (plan)
  "Load PLAN's files in order, calling nothing of agent-repl's by name.
PLAN is a list of plists (:module MODULE :file FILE :sha SHA256 :skip
OUTCOME), only :file required; an item whose :skip is non-nil is not loaded
and has that outcome.  THE ONE LOOP both a deploy's reload and the
runtime's hot load run.

The load wrapper `agent-repl--elisp-reload-load-file' is captured as a
function object once, before the first load, so a load that redefines it
cannot change what the loop calls; past that, the loop uses Emacs
primitives alone (see the section's commentary).  A file that signals is
a `failed' outcome and the rest still load: stopping at the first failure
would strand every later module's timers.

Returns one plist per item, in PLAN's order: (:module :file :sha :outcome
OUTCOME :message MESSAGE :error ERROR), OUTCOME `loaded', `failed' or the
item's :skip, and for `failed' MESSAGE the error's message and ERROR its
`prin1' text."
  (let ((load (symbol-function 'agent-repl--elisp-reload-load-file))
        (outcomes nil))
    (dolist (item plan)
      (let ((file (plist-get item :file))
            (outcome (plist-get item :skip))
            (message nil)
            (printed nil))
        (unless outcome
          (condition-case err
              (progn (funcall load file)
                     (setq outcome 'loaded))
            (error
             (setq outcome 'failed
                   message (error-message-string err)
                   printed (prin1-to-string err)))))
        (setq outcomes (cons (list :module (plist-get item :module) :file file
                                   :sha (plist-get item :sha) :outcome outcome
                                   :message message :error printed)
                             outcomes))))
    (nreverse outcomes)))

(defun agent-repl--elisp-function-kind (symbol)
  "Return SYMBOL's kind as a caller sees it: `macro' or `function'."
  (if (macrop symbol) 'macro 'function))

(defun agent-repl--elisp-function-kinds ()
  "Return (SYMBOL . KIND) for every fbound agent-repl symbol.
KIND is `agent-repl--elisp-function-kind''s answer.  Taken before a load,
so the build loaded after it can be compared with
`agent-repl--elisp-report-kind-changes'."
  (let (kinds)
    (mapatoms (lambda (symbol)
                (when (and (fboundp symbol)
                           (string-prefix-p "agent-repl" (symbol-name symbol)))
                  (push (cons symbol (agent-repl--elisp-function-kind symbol)) kinds))))
    kinds))

(defun agent-repl--elisp-report-kind-changes (kinds)
  "Name at ERROR every symbol of KINDS whose kind the load changed.
KINDS is `agent-repl--elisp-function-kinds''s answer from before the load.
A symbol that went from function to macro or from macro to function is
called wrongly by every closure, timer and process filter made before the
load that calls it, until that caller is made again, so the change is
loud.  Nothing is repaired.  Returns the changed symbols, in name order."
  (let (changed)
    (dolist (entry kinds)
      (let ((symbol (car entry)))
        (when (and (fboundp symbol)
                   (not (eq (cdr entry) (agent-repl--elisp-function-kind symbol))))
          (push symbol changed))))
    (setq changed (sort changed #'string<))
    (when changed
      (agent-repl--error agent-repl--elisp-build-log-scope
                         "elisp.elisp-build.reload-kind-changed count={count} symbols={symbols}"
                         :count (length changed) :symbols (prin1-to-string changed)))
    changed))

(defun agent-repl-elisp-hot-load (files)
  "Load the elisp sources FILES in order, applying the defaults they changed.
The runtime\='s `hot-reload' verb asks this, as a deploy\='s reload loads
the whole set, through the same loop (`agent-repl--elisp-load-files'), and
everything after the last load is `agent-repl--elisp-hot-load-finish', the
loaded build's: a default one of FILES changed reaches the running editor.
A file that fails to load signals, so the verb fails loudly.  Returns t."
  (let* ((plan (mapcar (lambda (file) (list :file file)) files))
         (snapshot (agent-repl--elisp-defaults-snapshot))
         (kinds (agent-repl--elisp-function-kinds))
         (outcomes (agent-repl--elisp-load-files plan)))
    ;; Called BY NAME after the last load, so the definition that runs is
    ;; the one just loaded.
    (agent-repl--elisp-hot-load-finish
     (list :outcomes outcomes :defaults-snapshot snapshot :kinds kinds))))

(defun agent-repl--elisp-hot-load-finish (result)
  "Finish a hot load whose files are loaded, from RESULT; return t or signal.
A REPLACED BUILD CALLS THIS IN THE NEW ONE, as it calls
`agent-repl--elisp-reload-finish': RESULT is ONE plist, and its keys are
only ever added, never removed or renamed, since the caller is the build
before this one:

  :outcomes           `agent-repl--elisp-load-files''s answer;
  :defaults-snapshot  `agent-repl--elisp-defaults-snapshot' from before
                      the first load;
  :kinds              `agent-repl--elisp-function-kinds' from before the
                      first load.

Each file is recorded at INFO, a file that failed at ERROR as well; a
symbol whose kind changed is named at ERROR; the changed defaults are
applied; then a failed file signals, naming each that failed."
  (let ((failed nil))
    (dolist (outcome (plist-get result :outcomes))
      (agent-repl--info agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.hot-load file={file}" :file (prin1-to-string (plist-get outcome :file)))
      (pcase-exhaustive (plist-get outcome :outcome)
        ('loaded nil)
        ('failed
         (agent-repl--error agent-repl--elisp-build-log-scope
                            "elisp.elisp-build.hot-load-failed file={file} error={error}"
                            :file (prin1-to-string (plist-get outcome :file)) :error (plist-get outcome :error))
         (push outcome failed))))
    (agent-repl--elisp-report-kind-changes (plist-get result :kinds))
    (agent-repl--elisp-defaults-apply (plist-get result :defaults-snapshot))
    (when failed
      (error "agent-repl: hot load: %d file(s) failed to load: %s"
             (length failed)
             (mapconcat (lambda (outcome)
                          (format "%s: %s" (plist-get outcome :file) (plist-get outcome :message)))
                        (nreverse failed) "; ")))
    t))

;;;; ---- The pushed reload ----

(defun agent-repl-elisp-reload-handle (reload &optional on-done)
  "Handle a pushed `DaemonReloadElisp' RELOAD, a plist (:module-root :build).
The daemon may only reload THIS Emacs's checkout: a root naming any other
is REFUSED loudly and nothing is loaded, because it would leave the editor
on one checkout's elisp and the daemon on another's.  Otherwise the reload
is SCHEDULED out of the process filter that delivered the push, and
ON-DONE, when given, is called once it has run with whether it succeeded
\=(`agent-repl--elisp-reload-then').  Returns non-nil when the reload was
scheduled."
  (let* ((pushed (plist-get reload :module-root))
         (build (plist-get reload :build))
         (theirs (agent-repl--elisp-normalize-root pushed))
         (ours (agent-repl--elisp-normalize-root agent-repl--frontend-root)))
    (if (not (string= theirs ours))
        (progn
          (agent-repl--error agent-repl--elisp-build-log-scope
                             "elisp.elisp-build.reload-refused reason=root-mismatch pushed-root={pushed_root} running-root={running_root} build={build}"
                             :pushed_root (prin1-to-string theirs) :running_root (prin1-to-string ours) :build (prin1-to-string build))
          (message "agent-repl: elisp reload REFUSED -- the deploy is for %s, this Emacs runs %s"
                   theirs ours)
          nil)
      (agent-repl--info agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.reload-scheduled root={root} build={build}"
                        :root (prin1-to-string ours) :build (prin1-to-string build))
      (run-at-time 0 nil #'agent-repl--elisp-reload-then ours build on-done)
      t)))

(defun agent-repl--elisp-reload-then (root build on-done)
  "Run the reload of BUILD from ROOT, then call ON-DONE with its success.
ON-DONE, when non-nil, runs even when the reload signals, so a caller
waiting on it (a link awaiting its redial) is never left waiting; the
signal still propagates."
  (let (loaded)
    (unwind-protect
        (setq loaded (agent-repl--elisp-reload-run root build))
      (when on-done (funcall on-done loaded)))))

(defun agent-repl--elisp-reload-plan (root modules)
  "Return the load plan of MODULES' sources under ROOT, for the shared loop.
One item per module, in order (`agent-repl--elisp-load-files'): its file,
its SHA-256, and a :skip of `refused-test' for a `test-' module and of
`absent' for a module whose file is absent.  Reads the files; loads
nothing."
  (mapcar (lambda (module)
            (let* ((file (agent-repl--elisp-module-file root module))
                   (test-p (string-prefix-p "test-" module))
                   (sha (and (not test-p) (agent-repl--elisp-file-sha256 file))))
              (list :module module :file file :sha sha
                    :skip (cond (test-p 'refused-test)
                                ((null sha) 'absent)))))
          modules))

(defun agent-repl--elisp-reload-record-outcomes (outcomes)
  "Record each module's load OUTCOMES, once every module has been tried.
OUTCOMES is `agent-repl--elisp-load-files''s answer for a reload's plan.
A loaded module is recorded at the debug rung; a refused, absent or failed
one at ERROR.  Returns (ENTRIES . FAILURES): ENTRIES the (MODULE . SHA256)
of every source found, in order, and FAILURES a list of (MODULE . REASON)."
  (let (entries failures)
    (dolist (outcome outcomes)
      (let ((module (plist-get outcome :module))
            (file (plist-get outcome :file))
            (sha (plist-get outcome :sha)))
        (pcase-exhaustive (plist-get outcome :outcome)
          ('refused-test
           ;; A test file is a batch-only harness; loading one into a live
           ;; Emacs once disarmed its external boundaries and redirected its
           ;; state.  config.el never names one, so this is a broken loader.
           (push (cons module "a test file is never loaded into a running Emacs") failures)
           (agent-repl--error agent-repl--elisp-build-log-scope
                              "elisp.elisp-build.reload-test-module-refused module={module} file={file}"
                              :module module :file (prin1-to-string file)))
          ('absent
           (push (cons module "the file is absent") failures)
           (agent-repl--error agent-repl--elisp-build-log-scope
                              "elisp.elisp-build.reload-module-absent module={module} file={file}"
                              :module module :file (prin1-to-string file)))
          ('loaded
           (push (cons module sha) entries)
           (agent-repl--log agent-repl--elisp-build-log-scope
                            "elisp.elisp-build.reload-module-loaded module={module} sha256={sha256}"
                            :module module :sha256 sha))
          ('failed
           (push (cons module sha) entries)
           (push (cons module (plist-get outcome :message)) failures)
           (agent-repl--error agent-repl--elisp-build-log-scope
                              "elisp.elisp-build.reload-module-failed module={module} file={file} error={error}"
                              :module module :file (prin1-to-string file) :error (plist-get outcome :error))))))
    (cons (nreverse entries) (nreverse failures))))

(defun agent-repl--elisp-reload-report-heartbeat (result)
  "Log the heartbeat assertion RESULT of a reload; a failed timer is an ERROR."
  (let ((rearmed (plist-get result :rearmed))
        (failed (plist-get result :failed)))
    (agent-repl--info agent-repl--elisp-build-log-scope
                      "elisp.elisp-build.reload-heartbeat armed={armed} rearmed={rearmed} failed={failed} unavailable={unavailable}"
                      :armed (length (plist-get result :armed)) :rearmed (length rearmed)
                      :failed (length failed) :unavailable (length (plist-get result :unavailable)))
    (when rearmed
      (agent-repl--info agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.reload-heartbeat-rearmed keys={keys}" :keys (prin1-to-string rearmed)))
    (when failed
      (agent-repl--error agent-repl--elisp-build-log-scope
                         "elisp.elisp-build.reload-heartbeat-failed keys={keys}" :keys (prin1-to-string failed))
      (message "agent-repl: elisp reload left required timers unarmed: %s"
               (mapconcat (lambda (key) (format "%s" key)) failed ", ")))))

;;;; ---- A reload's retired definitions ----

(defun agent-repl--elisp-undefined-functions ()
  "Return every agent-repl function no loaded file defines, in name order.
An interned symbol named with the `agent-repl' prefix, `fboundp', whose
`symbol-file' as a `defun' is nil: the load that last read its file no
longer defined it.  An autoload answers its file, so it is never listed.
Reads only; nothing is changed."
  (let (found)
    (mapatoms (lambda (symbol)
                (when (and (fboundp symbol)
                           (string-prefix-p "agent-repl" (symbol-name symbol))
                           (null (symbol-file symbol 'defun)))
                  (push symbol found))))
    (sort found #'string<)))

(defun agent-repl--elisp-retire-undefined ()
  "Retire every agent-repl function no loaded file defines.
Each function `agent-repl--elisp-undefined-functions' answers is removed
from the GLOBAL value of every variable whose default value is a list
holding it (`remove-hook', run from a temporary buffer so no buffer-local
value is the one changed), every timer of `timer-list' and
`timer-idle-list' whose function is the symbol itself is cancelled, and
the symbol is unbound (`fmakunbound').  Buffer-local hook values, and a
timer or hook whose function is a closure calling the symbol, are out of
scope: neither names the symbol where this can see it.

Returns (:functions SYMBOLS :hooks ((VARIABLE . SYMBOL) ...) :timers
SYMBOLS): the functions retired, each hook entry cleared, and the function
of each timer cancelled, one per timer."
  (let ((retired (agent-repl--elisp-undefined-functions))
        (hooks nil)
        (timers nil))
    (when retired
      (with-temp-buffer
        (mapatoms (lambda (variable)
                    (when (default-boundp variable)
                      (let ((value (default-value variable)))
                        (when (proper-list-p value)
                          (dolist (symbol retired)
                            (when (memq symbol value)
                              (remove-hook variable symbol)
                              (push (cons variable symbol) hooks)))))))))
      (dolist (timer (append timer-list timer-idle-list))
        (when (memq (timer--function timer) retired)
          (push (timer--function timer) timers)
          (cancel-timer timer)))
      (mapc #'fmakunbound retired))
    (list :functions retired
          :hooks (sort hooks (lambda (a b) (string< (car a) (car b))))
          :timers (nreverse timers))))

(defun agent-repl--elisp-reload-report-retired (result)
  "Record the retirement RESULT of a reload: INFO when it retired anything.
A retirement that found nothing to retire is the quiet outcome, recorded
at the debug rung."
  (let ((functions (plist-get result :functions)))
    (if functions
        (agent-repl--info agent-repl--elisp-build-log-scope
                          "elisp.elisp-build.retired count={count} functions={functions} hooks={hooks} timers={timers}"
                          :count (length functions) :functions (prin1-to-string functions) :hooks (prin1-to-string (plist-get result :hooks)) :timers (prin1-to-string (plist-get result :timers)))
      (agent-repl--log agent-repl--elisp-build-log-scope
                       "elisp.elisp-build.retired-none reason={reason}"
                       :reason "every agent-repl function is defined by a loaded file"))))

(defun agent-repl--elisp-reload-run (root build)
  "Hot-load the module set from ROOT, which the daemon built as BUILD.
Reads config.el's `agent-repl--load-module' order, loads every source in
it, then runs the heartbeat assertion.  When every module loaded, the
functions the new sources no longer define are retired
\=(`agent-repl--elisp-retire-undefined'); a failed module skips that, at
INFO.  The recorded module builds are
recomputed from the bytes just loaded, so the next WatchDaemon reports the
new build; a recomputed build that differs from BUILD means the files moved
between the deploy and the load, and is an ERROR.  Returns non-nil when
every module loaded and the loaded build is BUILD, nil otherwise; every
failure is already recorded at ERROR.

Only reading config.el runs here before the loads; everything after the
last load is `agent-repl--elisp-reload-finish', called by name, so it is
the code of the build just loaded."
  (let ((config (expand-file-name "config.el" root))
        (modules nil))
    (agent-repl--info agent-repl--elisp-build-log-scope
                      "elisp.elisp-build.reload-begin root={root} build={build}" :root (prin1-to-string root) :build (prin1-to-string build))
    (condition-case err
        (progn
          (setq modules (agent-repl--elisp-config-modules config))
          (unless modules
            (agent-repl--error agent-repl--elisp-build-log-scope
                               "elisp.elisp-build.reload-config-names-no-module config={config}"
                               :config (prin1-to-string config))
            (message "agent-repl: elisp reload FAILED -- %s names no module" config)))
      (error
       (agent-repl--error agent-repl--elisp-build-log-scope
                          "elisp.elisp-build.reload-config-unreadable config={config} error={error}"
                          :config (prin1-to-string config) :error (prin1-to-string err))
       (message "agent-repl: elisp reload FAILED -- %s could not be read: %s"
                config (error-message-string err))))
    (when modules
      ;; FROM HERE TO THE LAST LOAD NOTHING OF AGENT-REPL'S IS CALLED BY NAME
      ;; (see "Loading a build's files"): what the finish needs from before
      ;; the loads is taken now, and the finish is called by name after
      ;; them, so it is the build just loaded.
      (let* ((plan (agent-repl--elisp-reload-plan root modules))
             (snapshot (agent-repl--elisp-defaults-snapshot))
             (kinds (agent-repl--elisp-function-kinds))
             (started (agent-repl--now))
             (outcomes (agent-repl--elisp-load-files plan)))
        (agent-repl--elisp-reload-finish
         (list :root root :build build :outcomes outcomes
               :defaults-snapshot snapshot :started started :kinds kinds))))))

(defun agent-repl--elisp-reload-finish (result)
  "Finish a reload whose modules are loaded, from RESULT; return its success.
THE ONE FUNCTION A REPLACED BUILD CALLS IN THE NEW ONE.  The replaced
build's `agent-repl--elisp-reload-run' calls it by name after the last
load, so the definition that runs is the one just loaded, and it is
handed what was taken before the first load.  So RESULT is ONE plist,
and its keys are only ever ADDED, never removed or renamed: the caller is
always the build before this one.  Its keys:

  :root               the module root the modules were loaded from;
  :build              the build the daemon pushed;
  :outcomes           `agent-repl--elisp-load-files''s answer for the
                      reload's plan, one per module config.el names;
  :defaults-snapshot  `agent-repl--elisp-defaults-snapshot' from before
                      the first load;
  :started            `agent-repl--now' just before the first load;
  :kinds              `agent-repl--elisp-function-kinds' from before the
                      first load.

Records each module's outcome, names every symbol whose kind changed,
applies the changed defaults, recomputes the recorded module builds, runs
the heartbeat assertion, schedules the compile, reports the reload's span,
reports the failures, retires what the new build no longer defines (or
records why not), and compares the loaded build with the pushed one.
Returns non-nil when every module loaded and the loaded build is the
pushed one, nil otherwise; every failure is already recorded at ERROR."
  (let* ((root (plist-get result :root))
         (build (plist-get result :build))
         (outcomes (plist-get result :outcomes))
         (recorded (agent-repl--elisp-reload-record-outcomes outcomes))
         (entries (car recorded))
         (failures (cdr recorded)))
    (agent-repl--elisp-report-kind-changes (plist-get result :kinds))
    (agent-repl--elisp-defaults-apply (plist-get result :defaults-snapshot))
    (setq agent-repl--elisp-module-builds entries)
    (agent-repl--elisp-reload-report-heartbeat (agent-repl--assert-heartbeat-armed))
    ;; The load replaced every compiled function with its interpreted
    ;; source; bytecode.el compiles them again, never ahead of input.
    (agent-repl-bytecode-schedule root (mapcar #'car entries))
    ;; THE EDITOR RAN NOTHING ELSE from the first load to here, so this
    ;; span is how long the reload held the person up (`elisp_reload').
    (agent-repl-latency-report-editor
     :elisp-reload (max 0.0 (- (agent-repl--now) (plist-get result :started))))
    (when failures
      (agent-repl--error agent-repl--elisp-build-log-scope
                         "elisp.elisp-build.reload-module-failures count={count} failures={failures}"
                         :count (length failures) :failures (prin1-to-string failures))
      (message "agent-repl: elisp reload: %d module(s) failed to load: %s"
               (length failures)
               (mapconcat #'car failures ", ")))
    ;; A module that failed to load still needs the functions its last
    ;; good load defined, which its new `load-history' entry may not name.
    (if failures
        (agent-repl--info agent-repl--elisp-build-log-scope
                          "elisp.elisp-build.retire-skipped failed={failed} reason={reason}"
                          :failed (length failures) :reason "a module failed to load, so its functions may still be needed")
      (agent-repl--elisp-reload-report-retired (agent-repl--elisp-retire-undefined)))
    (let ((loaded-build (agent-repl-elisp-build-of entries)))
      (unless (string= loaded-build build)
        (agent-repl--error agent-repl--elisp-build-log-scope
                           "elisp.elisp-build.reload-build-mismatch pushed={pushed} loaded={loaded} reason={reason}"
                           :pushed (prin1-to-string build) :loaded (prin1-to-string loaded-build)
                           :reason "the files changed between the deploy and the load"))
      (agent-repl--info agent-repl--elisp-build-log-scope
                        "elisp.elisp-build.reload-complete modules={modules} recorded={recorded} failed={failed} build={build}"
                        :modules (length outcomes) :recorded (length entries) :failed (length failures)
                        :build (prin1-to-string loaded-build))
      (and (null failures) (string= loaded-build build)))))

(defun agent-repl-elisp-reload-if-stale (root)
  "Hot-load the module set from ROOT when this Emacs runs older elisp.
The hard bounce asks this BEFORE it stops anything: the outgoing Emacs
witnesses the stand-down with whatever elisp it had loaded, so an elisp fix
merged since its last load would otherwise not be in force for the very
stand-down it fixes.  Returns a string the caller prints:

  \"current\"     the loaded elisp is ROOT\='s already; nothing loaded;
  \"reloaded\"    the module set was hot-loaded (`agent-repl--elisp-reload-run');
  \"failed\"      the hot load recorded a failure at ERROR;
  \"other-root\"  this Emacs runs another checkout\='s elisp, which is not
                ROOT\='s to replace; nothing loaded."
  (let ((theirs (agent-repl--elisp-normalize-root root))
        (ours (agent-repl--elisp-normalize-root agent-repl--frontend-root)))
    (if (not (string= theirs ours))
        (progn
          (agent-repl--info agent-repl--elisp-build-log-scope
                            "elisp.elisp-build.reload-if-stale outcome=other-root asked-root={asked_root} running-root={running_root}"
                            :asked_root (prin1-to-string theirs) :running_root (prin1-to-string ours))
          "other-root")
      (let* ((modules (agent-repl--elisp-config-modules (expand-file-name "config.el" ours)))
             (fresh (agent-repl-elisp-build-of (agent-repl-elisp-build-entries ours modules))))
        (cond
         ((string= fresh (agent-repl-elisp-build))
          (agent-repl--info agent-repl--elisp-build-log-scope
                            "elisp.elisp-build.reload-if-stale outcome=current build={build}" :build (prin1-to-string fresh))
          "current")
         ((agent-repl--elisp-reload-run ours fresh)
          (agent-repl--info agent-repl--elisp-build-log-scope
                            "elisp.elisp-build.reload-if-stale outcome=reloaded build={build}" :build (prin1-to-string fresh))
          "reloaded")
         (t "failed"))))))

(provide 'elisp-build)

;;; elisp-build.el ends here
