;;; log-site.el --- every log call is a site: a literal template and its named values -*- lexical-binding: t; -*-

;;; Commentary:

;; Every log call is a SITE (logging-contract.md "Log sites").  A call
;; passes its rung a TEMPLATE -- a string literal whose placeholders
;; (`{name}', a lowercase identifier; `{{' and `}}' are literal braces) are
;; filled from the call's NAMED VALUES -- and every record it writes carries
;; the site's KEY and template beside the rendered message:
;;
;;   (agent-repl--warn ws "timed out connecting to shim {shim} after {elapsed}"
;;                     :shim id :elapsed d)
;;
;; THE RUNGS ARE MACROS, and everything about a site is settled when the
;; call is macroexpanded (at load from source, and at byte or native
;; compile): the template is checked to be a literal that parses, its
;; placeholders are checked against the named values, and the site -- key,
;; function, template, value names, operation -- becomes one constant
;; vector in the expansion.  A call that breaks a rule fails its file's load
;; or compile, naming itself; a call that expands costs nothing at run time
;; beyond its values.
;;
;; A SITE'S KEY is the first 12 hex characters of the sha256 of the
;; runtime ("emacs"), the function the call is written in, and the template,
;; joined by NUL bytes -- the Go reference's derivation exactly
;; (agent-shim/logging/go/logsite).
;;
;; THE FUNCTION is the call's innermost enclosing named definition -- a
;; `defun', `cl-defun', `cl-defmethod', `defmacro', `define-minor-mode',
;; `ert-deftest', `defvar' and their kin, or the `defalias' they expand to --
;; as found in the TOP-LEVEL FORM being expanded, and the file's name
;; (`core.el') for a call no definition encloses (a top-level hook lambda).
;; Emacs exposes no enclosing definition to a macro directly: the byte
;; compiler's `byte-compile-current-form' is bound only after the body was
;; already macroexpanded, and a load from source binds nothing at all.  What
;; BOTH modes do expose is the top-level form, as the first argument of the
;; frame expanding it: `internal-macroexpand-for-load' when a file is loaded
;; (or `eval-buffer'ed) from source, `byte-compile-toplevel-file-form' when
;; it is byte- or native-compiled.  The macro finds that frame and walks the
;; form down to its own call (by `eq': a macro embeds its arguments, it does
;; not copy them), keeping the innermost definition on the way.  The two
;; modes present the form at different stages -- a load from source sees
;; `(defalias 'f ...)' where the compiler sees `(defun f ...)' -- so both
;; spellings of each definition are recognized, and either gives the same
;; name.  One call is found otherwise: a call in an interpreted function
;; whose body was `eval'ed unexpanded (`eval-and-compile' loaded from
;; source) expands only when the function runs, inside whatever top-level
;; form is expanding then; it is keyed by that running function, the one
;; on the stack whose body holds it.  `test-log-site.el' compiles and loads one fixture both ways and
;; requires every key to agree, and its scan finds every call's function
;; from source the same way.  A form expanded outside any file
;; (`macroexpand' at run time) has no function unless
;; `agent-repl-log-site-function' names one, and refuses to expand.
;;
;; A NAMED VALUE renders into the message as `%s' prints it (a string as
;; itself, anything else by `princ'), and is recorded as a `context' field
;; under its name with the same text (a number stays a JSON number).  A
;; value written `(prin1-to-string EXPR)' renders as `%S' printed it, and
;; the printing is DEFERRED to the moment the record is built, so a dropped
;; record never prints it.  Every other value form is evaluated as written.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--emit-log-record "core"
                  (ws level verbosity site values &rest keys))
(declare-function agent-repl--warn-once-claim "core" (ws fingerprint))
(declare-function agent-repl--log-decision-claim "core" (key decision))
(declare-function agent-repl--minibuffer-busy-p "core" ())

(defconst agent-repl-log-site-runtime "emacs"
  "The runtime every Emacs site's key is derived for.")

(defconst agent-repl-log-site-key-length 12
  "How many hex characters of the sha256 a site key keeps.")

(defvar agent-repl-log-site-function nil
  "When non-nil, the function name every log macro expanded now is keyed by.
For a form expanded outside any file load or compile -- a test's
`macroexpand' -- which otherwise has no enclosing definition to find.")

(defvar agent-repl-log-site--expanded (make-hash-table :test #'equal)
  "KEY -> (FUNCTION TEMPLATE FILE) for every site expanded in this Emacs.
The scan in `test-log-site.el' holds every call it finds in source to the
key its expansion derived here.")

;;;; ---- Keys ----

(defun agent-repl-log-site-key (runtime function template)
  "Return the site key of the call in FUNCTION writing TEMPLATE, of RUNTIME.
The first `agent-repl-log-site-key-length' hex characters of the sha256 of
the UTF-8 bytes of RUNTIME, FUNCTION and TEMPLATE joined by NUL bytes."
  (substring (secure-hash 'sha256
                          (encode-coding-string
                           (concat runtime "\0" function "\0" template)
                           'utf-8 t))
             0 agent-repl-log-site-key-length))

;;;; ---- Templates ----

(defconst agent-repl-log-site--name-regexp "\\`[a-z_][a-z0-9_]*\\'"
  "A placeholder's name: a lowercase identifier.")

(defun agent-repl-log-site-parse (template)
  "Return TEMPLATE's parts in order: literal strings and (NAME) placeholders.
A doubled brace is a literal one.  Signals an error naming the fault for a
placeholder that never closes, one whose name is not a lowercase
identifier, and a lone closing brace -- the Go reference's grammar."
  (unless (stringp template)
    (error "agent-repl log site: the template %S is not a string" template))
  (let ((parts nil) (run nil) (i 0) (n (length template)))
    (cl-flet ((flush () (when run
                          (push (apply #'string (nreverse run)) parts)
                          (setq run nil))))
      (while (< i n)
        (let ((c (aref template i)))
          (cond
           ((and (eq c ?{) (< (1+ i) n) (eq (aref template (1+ i)) ?{))
            (push ?{ run) (setq i (+ i 2)))
           ((and (eq c ?}) (< (1+ i) n) (eq (aref template (1+ i)) ?}))
            (push ?} run) (setq i (+ i 2)))
           ((eq c ?{)
            (let ((end (string-search "}" template (1+ i))))
              (unless end
                (error "agent-repl log site: the template %S opens a placeholder at %d that never closes (a literal brace is written {{)"
                       template i))
              (let ((name (substring template (1+ i) end)))
                (unless (let ((case-fold-search nil))
                          (string-match-p agent-repl-log-site--name-regexp name))
                  (error "agent-repl log site: the template %S names the placeholder {%s}, which is not a lowercase identifier (a literal brace is written {{)"
                         template name))
                (flush)
                (push (list name) parts)
                (setq i (1+ end)))))
           ((eq c ?})
            (error "agent-repl log site: the template %S has a lone closing brace at %d (a literal brace is written }})"
                   template i))
           (t (push c run) (setq i (1+ i))))))
      (flush))
    (nreverse parts)))

(defun agent-repl-log-site-placeholders (template)
  "Return TEMPLATE's placeholder names in order of first appearance, each once."
  (let ((names nil))
    (dolist (part (agent-repl-log-site-parse template))
      (when (consp part)
        (cl-pushnew (car part) names :test #'equal)))
    (nreverse names)))

;;;; ---- Sites ----

;; A site is one constant vector, made at expansion time:
;; [KEY FUNCTION TEMPLATE NAMES PRINTED OPERATION PARTS]
;; NAMES is a vector of the value names in the order the call wrote them
;; (the order the values are passed), PRINTED a parallel vector of flags
;; (non-nil: render with `prin1'), PARTS the parsed template.

(defsubst agent-repl-log-site-key-of (site) "SITE's key." (aref site 0))
(defsubst agent-repl-log-site-function-of (site) "SITE's function." (aref site 1))
(defsubst agent-repl-log-site-template (site) "SITE's template." (aref site 2))
(defsubst agent-repl-log-site-names (site) "SITE's value names." (aref site 3))
(defsubst agent-repl-log-site-printed (site) "SITE's prin1 flags." (aref site 4))
(defsubst agent-repl-log-site-operation (site) "SITE's operation." (aref site 5))
(defsubst agent-repl-log-site-parts (site) "SITE's parsed template." (aref site 6))

(defun agent-repl-log-site-p (object)
  "Non-nil when OBJECT has a site's shape."
  (and (vectorp object) (= (length object) 7) (stringp (aref object 0))
       (stringp (aref object 2))))

(defun agent-repl-log-site-operation-name (template)
  "Return the stable operation name TEMPLATE's site records.
TEMPLATE's literal text, each placeholder spelled `%s' as the format
string it replaced spelled it, lowercased, with every run of other
characters made one `-', under the `agent-repl.' prefix -- so a site's
operation is the one its format string had."
  (let* ((source (mapconcat (lambda (part) (if (consp part) "%s" part))
                            (agent-repl-log-site-parse template) ""))
         (normalized (replace-regexp-in-string
                      "[^[:alnum:]]+" "-" (downcase source))))
    (setq normalized (replace-regexp-in-string "\\`-+\\|-+\\'" "" normalized))
    (concat "agent-repl." (if (string-empty-p normalized) "log" normalized))))

(defun agent-repl-log-site-make (function template names printed)
  "Return the site of a call in FUNCTION writing TEMPLATE with NAMES.
NAMES lists the value names in the order the call passes them, PRINTED the
names rendered with `prin1'.  Signals when TEMPLATE does not parse or its
placeholders and NAMES differ."
  (let* ((parts (agent-repl-log-site-parse template))
         (placeholders (agent-repl-log-site-placeholders template))
         (missing (cl-set-difference placeholders names :test #'equal))
         (extra (cl-set-difference names placeholders :test #'equal)))
    (when missing
      (error "agent-repl log site: the template %S names {%s}, which the call's named values do not hold"
             template (string-join missing "}, {")))
    (when extra
      (error "agent-repl log site: the call names the value(s) :%s, which the template %S never places"
             (string-join extra ", :") template))
    (vector (agent-repl-log-site-key agent-repl-log-site-runtime function template)
            function template (vconcat names)
            (vconcat (mapcar (lambda (name) (and (member name printed) t)) names))
            (agent-repl-log-site-operation-name template)
            parts)))

;;;; ---- Rendering ----

(defun agent-repl-log-site-text (value printed)
  "Return VALUE as a message spells it; with PRINTED, as `prin1' prints it."
  (cond (printed (prin1-to-string value))
        ((stringp value) value)
        (t (format "%s" value))))

(defun agent-repl-log-site-render (site values)
  "Return SITE's template rendered from VALUES, passed in SITE's name order."
  (let ((names (agent-repl-log-site-names site))
        (printed (agent-repl-log-site-printed site)))
    (mapconcat
     (lambda (part)
       (if (consp part)
           (let ((i (cl-position (car part) names :test #'equal)))
             (agent-repl-log-site-text (nth i values) (aref printed i)))
         part))
     (agent-repl-log-site-parts site) "")))

(defun agent-repl-log-site-context-value (value printed)
  "Return VALUE as its record's `context' field holds it.
A number stays a JSON number (a non-finite float is spelled as text);
everything else is its rendered text."
  (if (and (not printed)
           (or (integerp value)
               (and (floatp value) (not (isnan value))
                    (< (abs value) 1.0e+INF))))
      value
    (agent-repl-log-site-text value printed)))

(defun agent-repl-log-site-context-pairs (site values)
  "Return SITE's named VALUES as ((NAME . CONTEXT-VALUE) ...)."
  (let ((names (agent-repl-log-site-names site))
        (printed (agent-repl-log-site-printed site))
        (pairs nil))
    (dotimes (i (length names))
      (push (cons (aref names i)
                  (agent-repl-log-site-context-value (nth i values) (aref printed i)))
            pairs))
    (nreverse pairs)))

;;;; ---- The enclosing function, at expansion ----

(defconst agent-repl-log-site--definers
  '(defun defmacro defsubst define-inline cl-defun cl-defmacro cl-defsubst
    cl-defmethod cl-defgeneric define-minor-mode define-globalized-minor-mode
    define-derived-mode ert-deftest defvar defconst defcustom defvar-local
    defface)
  "Definitions spelled (DEFINER NAME ...), as source and the compiler see them.")

(defconst agent-repl-log-site--quoted-definers
  '(defalias ert-set-test cl-generic-define-method custom-declare-variable
    defvaralias)
  "Definitions spelled (DEFINER \\='NAME ...), as a load from source sees them.")

(defun agent-repl-log-site--bare (object)
  "OBJECT with a symbol's position stripped (the compiler reads positions)."
  (if (and (fboundp 'symbol-with-pos-p) (symbol-with-pos-p object))
      (bare-symbol object)
    object))

(defun agent-repl-log-site-definition-name (form)
  "Return the name FORM defines, as a string, or nil when it defines nothing."
  (when (consp form)
    (let ((head (agent-repl-log-site--bare (car form))))
      (when (symbolp head)
        (let ((name
               (cond
                ((memq head agent-repl-log-site--definers)
                 (and (consp (cdr form)) (cadr form)))
                ((memq head agent-repl-log-site--quoted-definers)
                 (let ((arg (and (consp (cdr form)) (cadr form))))
                   (and (consp arg)
                        (eq (agent-repl-log-site--bare (car arg)) 'quote)
                        (consp (cdr arg))
                        (cadr arg)))))))
          (setq name (agent-repl-log-site--bare name))
          (and name (symbolp name) (symbol-name name)))))))

(defun agent-repl-log-site--find (form target)
  "Return the innermost definition name on the path from FORM down to TARGET.
`:none' when TARGET is reached with no definition on the path, nil when
TARGET is not inside FORM at all.  TARGET is found by `eq'."
  (let ((seen (make-hash-table :test #'eq)))
    (cl-labels
        ((walk (node enclosing)
           (cond
            ((eq node target) (or enclosing :none))
            ((not (consp node)) nil)
            ((gethash node seen) nil)
            (t
             (puthash node t seen)
             (let ((inner (or (agent-repl-log-site-definition-name node) enclosing))
                   (found nil))
               ;; Walk the list's elements; a dotted tail is a node too.
               (while (and (consp node) (not found))
                 (setq found (walk (car node) inner)
                       node (cdr node))
                 (when (and (consp node) (gethash node seen))
                   (setq node nil)))
               (or found (and node (walk node inner))))))))
      (walk form nil))))

(defun agent-repl-log-site--defines-anything-p (form)
  "Non-nil when any definition lies anywhere inside FORM."
  (let ((seen (make-hash-table :test #'eq)))
    (cl-labels ((walk (node)
                  (and (consp node)
                       (not (gethash node seen))
                       (progn (puthash node t seen)
                              (or (agent-repl-log-site-definition-name node)
                                  (walk (car node))
                                  (walk (cdr node)))))))
      (walk form))))

(defun agent-repl-log-site--toplevel-form ()
  "Return the top-level form being expanded, or nil outside any file's expansion.
The first argument of the innermost `internal-macroexpand-for-load' (a load
from source) or `byte-compile-toplevel-file-form' (a compile) frame."
  (catch 'found
    (mapbacktrace (lambda (_evald fn args _flags)
                    (when (and (memq fn '(internal-macroexpand-for-load
                                          byte-compile-toplevel-file-form))
                               args)
                      (throw 'found (car args)))))
    nil))

(defun agent-repl-log-site--interpreted-body (definition)
  "Return the body of DEFINITION when it is interpreted code, else nil.
A macro's definition is its expander's."
  (when (and (consp definition) (eq (car definition) 'macro))
    (setq definition (cdr definition)))
  (cond ((and (fboundp 'interpreted-function-p)
              (interpreted-function-p definition))
         (aref definition 1))
        ((and (consp definition) (memq (car definition) '(closure lambda)))
         definition)))

(defun agent-repl-log-site--interpreted-caller (template)
  "Return the named interpreted function, now running, whose body holds TEMPLATE.
Searched from the innermost frame out to the top-level form's frame.  An
interpreted function's body is expanded LAZILY, each time it runs: a
definition `eval'ed rather than loaded form by form (the body of
`eval-and-compile' loaded from source) expands its log calls when it is
called, inside whatever top-level form is expanding then.  Its own frame
is still on the stack, and its body holds the call's TEMPLATE by `eq'."
  (catch 'found
    (mapbacktrace
     (lambda (_evald fn _args _flags)
       (when (memq fn '(internal-macroexpand-for-load
                        byte-compile-toplevel-file-form))
         (throw 'found nil))
       (when (and (symbolp fn) fn (fboundp fn))
         (let ((body (agent-repl-log-site--interpreted-body (symbol-function fn))))
           (when (and body (agent-repl-log-site--find body template))
             (throw 'found (symbol-name fn)))))))
    nil))

(defun agent-repl-log-site--file-name ()
  "The file name (without directory) of the code being expanded, or nil."
  (when-let ((file (macroexp-file-name)))
    (file-name-nondirectory file)))

(defun agent-repl-log-site-enclosing-function (template whole)
  "Return the function the log call WHOLE, writing TEMPLATE, is written in.
The call is found in its top-level form by its TEMPLATE string, the very
object the reader made for the call: Emacs macros get no `&whole', and a
macro embeds its arguments where it does not copy them."
  (or agent-repl-log-site-function
      (let* ((toplevel (agent-repl-log-site--toplevel-form))
             (found (and toplevel (agent-repl-log-site--find toplevel template))))
        (cond
         ((stringp found) found)
         ((and (null found) (agent-repl-log-site--interpreted-caller template)))
         ((and (null found) toplevel (agent-repl-log-site-definition-name toplevel)))
         ((and toplevel
               (or (eq found :none)
                   (not (agent-repl-log-site--defines-anything-p toplevel))))
          (or (agent-repl-log-site--file-name)
              (error "agent-repl log site: %S is expanded in no file, so its site has no function"
                     whole)))
         (toplevel
          (error "agent-repl log site: %S is not written inside the top-level form expanding it, which defines several functions, so its function is ambiguous"
                 whole))
         (t
          (error "agent-repl log site: %S is expanded outside any file load or compile, so its site has no function (bind `agent-repl-log-site-function')"
                 whole))))))

;;;; ---- The macros ----

(defun agent-repl-log-site--expand-site (rung template named)
  "Return (SITE . VALUE-FORMS) for the RUNG call writing TEMPLATE with NAMED.
NAMED is the call's `:name VALUE' list.  Signals at expansion for a
template that is not a string literal, one that does not parse, a
malformed NAMED list, or names that do not match the placeholders."
  (let ((whole (cl-list* rung template named)))
  (unless (stringp template)
    (error "agent-repl log site: the template of %S is not a string literal; a template is the site's summary and part of its key, so every dynamic value goes in a named argument"
           whole))
  (let ((names nil) (printed nil) (forms nil) (rest named))
    (while rest
      (let ((key (car rest)))
        (unless (keywordp key)
          (error "agent-repl log site: %S passes %S where a :name keyword is expected"
                 whole key))
        (unless (cdr rest)
          (error "agent-repl log site: %S names %S with no value" whole key))
        (let ((name (substring (symbol-name key) 1))
              (value (cadr rest)))
          (unless (let ((case-fold-search nil))
                    (string-match-p agent-repl-log-site--name-regexp name))
            (error "agent-repl log site: %S names the value %S, which is not a lowercase identifier"
                   whole key))
          (when (member name names)
            (error "agent-repl log site: %S names the value %S twice" whole key))
          (push name names)
          (if (and (consp value)
                   (eq (agent-repl-log-site--bare (car value)) 'prin1-to-string)
                   (consp (cdr value)) (null (cddr value)))
              (progn (push name printed) (push (cadr value) forms))
            (push value forms)))
        (setq rest (cddr rest))))
    (setq names (nreverse names) forms (nreverse forms))
    ;; The template is checked before the function is looked for, so a bad
    ;; call names its own fault wherever it is expanded.
    (agent-repl-log-site-make "-" template names printed)
    (let* ((function (agent-repl-log-site-enclosing-function template whole))
           (site (agent-repl-log-site-make function template names printed)))
      (puthash (agent-repl-log-site-key-of site)
               (list function template (agent-repl-log-site--file-name))
               agent-repl-log-site--expanded)
      (cons site forms)))))

(defmacro agent-repl-log-site--define-rung (name emitter lead doc)
  "Define log macro NAME expanding to EMITTER, taking LEAD arguments first.
LEAD is the list of the rung's own arguments before the template; DOC
heads the macro's docstring."
  `(defmacro ,name (,@lead template &rest named)
     ,(concat doc "\n\n"
              "TEMPLATE must be a string literal: it is the site's summary and part
of its key, so rewording it gives the site a new key.  Its placeholders
are {name} (a lowercase identifier; {{ and }} are literal braces), and
every dynamic value goes in a NAMED argument, :name VALUE, one per
placeholder; a value written (prin1-to-string EXPR) renders as `%S' would.
A non-literal template, a malformed one, or names that do not match its
placeholders fail at macroexpansion.  See logging-contract.md \"Log
sites\" and log-site.el.")
     (let ((expanded (agent-repl-log-site--expand-site ',name template named)))
       (cl-list* ',emitter ,@lead (list 'quote (car expanded)) (cdr expanded)))))

(agent-repl-log-site--define-rung agent-repl--log agent-repl--emit-log (ws)
  "Log TEMPLATE for WS at debug, always to file, to *Messages* under debug.")
(agent-repl-log-site--define-rung agent-repl--log-verbose agent-repl--emit-log-verbose (ws)
  "Log high-frequency TEMPLATE for WS at debug/verbose, shown only in verbose mode.")
(agent-repl-log-site--define-rung agent-repl--info agent-repl--emit-info (ws)
  "Log TEMPLATE for WS at info to the quiet sink (file and *Messages*).")
(agent-repl-log-site--define-rung agent-repl--warn agent-repl--emit-warn (ws)
  "Log TEMPLATE for WS at warn to the quiet sink, tagged `WARNING: '.")
(agent-repl-log-site--define-rung agent-repl--warn-once agent-repl--emit-warn-once (ws fingerprint)
  "Log TEMPLATE for WS at warn once per stable FINGERPRINT.
Answers non-nil when the warning was emitted.")
(agent-repl-log-site--define-rung agent-repl--error agent-repl--emit-error (ws)
  "Log TEMPLATE for WS at error to the quiet sink, tagged `ERROR: '.
Never signals: `agent-repl--fatal' is the abort.")
(agent-repl-log-site--define-rung agent-repl--fatal agent-repl--emit-fatal (ws)
  "Record TEMPLATE for WS at error, then SIGNAL it: the abort, not a rung.")
(agent-repl-log-site--define-rung agent-repl--log-decision agent-repl--emit-log-decision (key decision ws)
  "Log TEMPLATE for WS at debug when DECISION differs from KEY's last one.
Answers non-nil when the record was written.")
(agent-repl-log-site--define-rung agent-repl--backend-phase agent-repl--emit-backend-phase (ws)
  "Record a backend-initiation phase TEMPLATE for WS at info and echo it.")
(agent-repl-log-site--define-rung agent-repl--phase-echo agent-repl--emit-phase-echo (ws)
  "Record a startup phase TEMPLATE for WS at info.
It is echoed unless the user is working in the minibuffer.")
(agent-repl-log-site--define-rung agent-repl--log-at agent-repl--emit-at (level ws)
  "Log TEMPLATE for WS at LEVEL, one of `debug', `info', `warn' or `error'.
For ONE site whose level is decided at run time -- one template the level
alone tells apart -- where a rung per level would be several calls writing
one template.  Each level behaves as its own rung does.")
(agent-repl-log-site--define-rung agent-repl--log-msg list ()
  "Return a log MESSAGE, (SITE . VALUES), written as TEMPLATE here.
For a helper that logs a message its caller words: the caller makes the
message, so the site is keyed by the caller's function, and the helper
passes it on with (apply #\\='agent-repl--emit-info WS MESSAGE).")

;;;; ---- The rungs' run-time halves ----
;;
;; Each macro above expands to one of these, (EMITTER LEAD... SITE VALUES...).
;; A test stubs a rung by stubbing its emitter; `agent-repl-log-site-render'
;; renders what it was handed.

(defun agent-repl--emit-log (ws site &rest values)
  "Persist SITE's record of VALUES for WS at debug; *Messages* under debug."
  (agent-repl--emit-log-record ws "debug" "normal" site values
                               :message-mode (and (bound-and-true-p agent-repl-debug) 'quiet)))

(defun agent-repl--emit-log-verbose (ws site &rest values)
  "Persist SITE's record of VALUES for WS at debug/verbose."
  (agent-repl--emit-log-record ws "debug" "verbose" site values
                               :message-mode (and (eq (bound-and-true-p agent-repl-debug) 'verbose)
                                                  'quiet)))

(defun agent-repl--emit-info (ws site &rest values)
  "Persist SITE's record of VALUES for WS at info, quietly."
  (agent-repl--emit-log-record ws "info" "normal" site values :message-mode 'quiet))

(defun agent-repl--emit-warn (ws site &rest values)
  "Persist SITE's record of VALUES for WS at warn, tagged, quietly."
  (agent-repl--emit-log-record ws "warn" "normal" site values
                               :tag "WARNING: " :message-mode 'quiet))

(defun agent-repl--emit-warn-once (ws fingerprint site &rest values)
  "Persist SITE's warning of VALUES for WS once per FINGERPRINT.
Answers non-nil when the warning was emitted."
  (when (agent-repl--warn-once-claim ws fingerprint)
    (agent-repl--emit-log-record ws "warn" "normal" site values
                                 :tag "WARNING: " :message-mode 'quiet)
    t))

(defun agent-repl--emit-error (ws site &rest values)
  "Persist SITE's record of VALUES for WS at error, tagged, quietly."
  (agent-repl--emit-log-record ws "error" "normal" site values
                               :tag "ERROR: " :message-mode 'quiet))

(defun agent-repl--emit-at (level ws site &rest values)
  "Persist SITE's record of VALUES for WS at LEVEL, as that level's rung does."
  (apply (pcase level
           ('debug #'agent-repl--emit-log)
           ('info #'agent-repl--emit-info)
           ('warn #'agent-repl--emit-warn)
           ('error #'agent-repl--emit-error)
           (_ (error "agent-repl--log-at: %S is not a log level (debug, info, warn, error)"
                     level)))
         ws site values))

(defun agent-repl--emit-fatal (ws site &rest values)
  "Persist SITE's record of VALUES for WS at error, then signal it."
  (agent-repl--emit-log-record ws "error" "normal" site values
                               :message-mode 'quiet :fatal t))

(defun agent-repl--emit-log-decision (key decision ws site &rest values)
  "Persist SITE's record of VALUES for WS at debug when DECISION is KEY's new one."
  (when (agent-repl--log-decision-claim key decision)
    (apply #'agent-repl--emit-log ws site values)
    t))

(defun agent-repl--emit-backend-phase (ws site &rest values)
  "Persist SITE's phase record of VALUES for WS at info and echo it."
  (agent-repl--emit-log-record ws "info" "normal" site values :message-mode 'backend))

(defun agent-repl--emit-phase-echo (ws site &rest values)
  "Persist SITE's phase record of VALUES for WS; echo it unless the user is typing."
  (agent-repl--emit-log-record ws "info" "normal" site values
                               :message-mode (if (agent-repl--minibuffer-busy-p) 'quiet 'backend)))

(provide 'log-site)

;;; log-site.el ends here
