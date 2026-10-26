;;; bytecode.el --- the loaded elisp runs byte-compiled -*- lexical-binding: t; -*-

;;; Commentary:

;; EVERY MODULE IS LOADED FROM ITS SOURCE (config.el's `agent-repl--load-module'
;; and the deploy's reload, elisp-build.el), so every function starts out
;; INTERPRETED.  Interpreted lexical code conses its environment for every
;; `let' it enters: decoding one roster push took 27.6ms and 554K conses
;; interpreted against 2.8ms and 36K byte-compiled (2026-10-25, a 298-row
;; roster).  On a long-lived Emacs with a large heap that allocation is a
;; garbage collection every push or two, and the burst of pushes a deploy's
;; handover sends held the editor for seconds while keystrokes waited.
;;
;; SO THE FUNCTIONS JUST LOADED ARE COMPILED IN PLACE, in this Emacs, after
;; every load.  Compiling the live definitions rather than building `.elc'
;; files means there is nothing to go stale (each compile reads the very
;; definition the load made) and no second build environment to keep in
;; step with Doom's macros (this Emacs has them all).
;;
;; THE COMPILE HOLDS A KEYSTROKE FOR ONE FUNCTION AT MOST.  It runs in slices
;; off a timer, each under `agent-repl-bytecode-slice-seconds', and a slice
;; asks `input-pending-p' before each function: input that arrived ends the
;; slice, and the next one picks the queue up where it stopped.
;;
;; NEVER `while-no-input'.  Under it, input stored while the compile runs
;; THROWS: a GTK (or Cocoa) callback that stores an xwidget event reaches
;; `kbd_buffer_store_event', whose `while-no-input-ignore-events' lookup
;; quits, and the throw unwinds through the callback's live GSignal emission.
;; GLib's emission list then names a dead stack frame, and the next
;; `notify' signal anywhere in the process segfaults walking it (the e2e
;; sandbox's Emacs, 2026-10-25 on, caught in the act by a longjmp
;; interposer).  `test-no-throw-on-input.el' keeps every module off it.
;;
;; A FUNCTION THAT CLOSES OVER A LEXICAL BINDING IS LEFT INTERPRETED.
;; Compiling such a closure copies the captured values, so two functions
;; sharing one `let' would stop sharing it.  Only a definition whose
;; environment binds nothing is compiled (`agent-repl-bytecode--top-level-p').

;;; Code:

(require 'cl-lib)
(require 'bytecomp)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--now "core" ())
(declare-function agent-repl--register-timer "core" (key timer))
(declare-function agent-repl--cancel-timer-key "core" (key))
(declare-function agent-repl--elisp-module-file "config" (root module))

(defconst agent-repl-bytecode--log-scope
  '(:agent-repl-central "the loaded elisp is process-wide")
  "Log scope of every record this file writes.")

(defvar agent-repl-bytecode-slice-seconds 0.01
  "The longest one compile slice runs before it yields, in seconds.
A slice stops after the function that crosses it, so a slice is this plus
at most one function's compile (14ms was the slowest measured).")

(defvar agent-repl-bytecode-interval 0.02
  "Seconds between compile slices while functions remain.")

(defvar agent-repl-bytecode--queue nil
  "The functions left to compile, each `(SYMBOL . FILE)', in load order.")

(defvar agent-repl-bytecode--run nil
  "The compile run under way, a plist, or nil.
`:started' (`agent-repl--now'), `:files', `:compiled', `:failed',
`:closures', `:slices', `:interrupted' and `:cpu' (seconds spent in slices).")

(defun agent-repl-bytecode--top-level-p (fn)
  "Return non-nil when interpreted function FN captured no lexical binding.
Its environment holds only symbols: `t' marks it lexical, and any other
symbol is a variable the file declared special with a bare `defvar' (every
`cl-defstruct' declares its tags so), which the compile keeps special.  A
captured binding is a cons, `(VAR . VALUE)'."
  (not (cl-some #'consp (aref fn 2))))

(defun agent-repl-bytecode-file-functions (file)
  "Return FILE's functions as the last load of FILE defined them.
Answers `(:functions SYMBOLS :closures N)': SYMBOLS are the interpreted
top-level definitions `load-history' records for FILE, in definition
order, and N counts the definitions left interpreted because they closed
over a lexical binding.  A definition replaced since (advice, a later
load elsewhere) is no longer interpreted and is not listed.  A FILE with
no `load-history' entry was never loaded, which the caller's own load
guarantees it was: that answers nil."
  (let ((entry (assoc file load-history))
        (functions nil)
        (closures 0))
    (when entry
      (dolist (item (cdr entry))
        (when (and (eq (car-safe item) 'defun) (fboundp (cdr item)))
          (let ((fn (symbol-function (cdr item))))
            (when (interpreted-function-p fn)
              (if (agent-repl-bytecode--top-level-p fn)
                  (push (cdr item) functions)
                (setq closures (1+ closures)))))))
      (list :functions (nreverse functions) :closures closures))))

(defun agent-repl-bytecode-schedule (root modules)
  "Compile the functions MODULES' sources under ROOT just defined.
MODULES are module names in load order (`agent-repl--elisp-module-file').
Replaces any run still under way: a load supersedes the definitions the
earlier run was compiling.  A module with no `load-history' entry was not
loaded by the caller that names it, a broken caller, and is an ERROR."
  (let ((queue nil)
        (closures 0)
        (files 0))
    (dolist (module modules)
      (let* ((file (agent-repl--elisp-module-file root module))
             (found (agent-repl-bytecode-file-functions file)))
        (if (null found)
            (agent-repl--error agent-repl-bytecode--log-scope
                               "elisp.bytecode.module-not-loaded module=%s file=%S"
                               module file)
          (setq files (1+ files)
                closures (+ closures (plist-get found :closures)))
          (dolist (symbol (plist-get found :functions))
            (push (cons symbol file) queue)))))
    (setq agent-repl-bytecode--queue (nreverse queue)
          agent-repl-bytecode--run (list :started (agent-repl--now) :files files
                                         :compiled 0 :failed 0 :closures closures
                                         :slices 0 :interrupted 0 :cpu 0.0))
    (agent-repl--info agent-repl-bytecode--log-scope
                      "elisp.bytecode.scheduled functions=%d files=%d closures=%d"
                      (length agent-repl-bytecode--queue) files closures)
    (agent-repl--register-timer
     :bytecode
     (run-with-timer 0 agent-repl-bytecode-interval #'agent-repl-bytecode--slice))))

(defun agent-repl-bytecode--compile (symbol file)
  "Byte-compile SYMBOL's definition in place, the one defined from FILE.
Answers `compiled', `skipped' (no longer an interpreted definition: it was
redefined or advised since it was queued) or `failed' (logged at ERROR)."
  (let ((fn (and (fboundp symbol) (symbol-function symbol))))
    (if (not (and (interpreted-function-p fn)
                  (agent-repl-bytecode--top-level-p fn)))
        'skipped
      (condition-case err
          (let ((byte-compile-warnings nil)
                (inhibit-message t)
                (message-log-max nil))
            (byte-compile symbol)
            (if (byte-code-function-p (symbol-function symbol))
                'compiled
              (agent-repl--error agent-repl-bytecode--log-scope
                                 "elisp.bytecode.compile-failed function=%s file=%S error=%s"
                                 symbol file "the compile installed no byte-code")
              'failed))
        (error
         (agent-repl--error agent-repl-bytecode--log-scope
                            "elisp.bytecode.compile-failed function=%s file=%S error=%S"
                            symbol file err)
         'failed)))))

(defun agent-repl-bytecode--slice ()
  "Compile queued functions until the slice's time is up or input is pending.
Input is checked BETWEEN functions (`input-pending-p'), never awaited with
`while-no-input' (see the Commentary): a function's compile always runs to
its end, and the functions after it stay queued for the next slice.  The
run ends, and is recorded, when the queue is empty."
  (let* ((run agent-repl-bytecode--run)
         (started (agent-repl--now))
         (deadline (+ started agent-repl-bytecode-slice-seconds))
         (interrupted nil))
    (while (and agent-repl-bytecode--queue
                (< (agent-repl--now) deadline)
                (not (setq interrupted (input-pending-p))))
      (let* ((head (car agent-repl-bytecode--queue))
             (outcome (agent-repl-bytecode--compile (car head) (cdr head))))
        (setq agent-repl-bytecode--queue (cdr agent-repl-bytecode--queue))
        (pcase outcome
          ('compiled (plist-put run :compiled (1+ (plist-get run :compiled))))
          ('failed (plist-put run :failed (1+ (plist-get run :failed)))))))
    (plist-put run :slices (1+ (plist-get run :slices)))
    (plist-put run :cpu (+ (plist-get run :cpu) (- (agent-repl--now) started)))
    (when interrupted
      (plist-put run :interrupted (1+ (plist-get run :interrupted))))
    (unless agent-repl-bytecode--queue
      (agent-repl--cancel-timer-key :bytecode)
      (agent-repl--info agent-repl-bytecode--log-scope
                        "elisp.bytecode.compiled functions=%d failed=%d closures=%d files=%d slices=%d interrupted=%d cpu-ms=%d wall-ms=%d"
                        (plist-get run :compiled) (plist-get run :failed)
                        (plist-get run :closures) (plist-get run :files)
                        (plist-get run :slices) (plist-get run :interrupted)
                        (round (* 1000 (plist-get run :cpu)))
                        (round (* 1000 (- (agent-repl--now) (plist-get run :started)))))
      (setq agent-repl-bytecode--run nil))))

(provide 'bytecode)

;;; bytecode.el ends here
