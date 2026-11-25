;;; test-log-site.el --- ERT tests for log-site.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that every log call is a site (logging-contract.md "Log
;; sites") -- its key derived exactly as the Go reference derives it, its
;; template rendered from its named values, every malformed call refused at
;; macroexpansion, its function found the same whether its file is loaded
;; from source or compiled -- and, by a static scan of every source, that no
;; two calls in one function write one template and no two sites' keys
;; collide.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)
(require 'log-site)

(defmacro agent-repl-test-log-site--expanding (&rest body)
  "Run BODY with every log macro it expands keyed by the function `f'."
  (declare (indent 0))
  `(let ((agent-repl-log-site-function "f"))
     ,@body))

(defun agent-repl-test-log-site--site (form)
  "Return the site FORM, a log macro call, expands to under the function `f'."
  (agent-repl-test-log-site--expanding
    (cadr (nth 2 (macroexpand form)))))

;;;; ---- Keys ----

(ert-deftest agent-repl-test-log-site-key-matches-the-go-reference ()
  "The key of (daemon, pkg.F, hello {name}) is the one the Go reference pins."
  ;; Act / Assert
  (should (equal (agent-repl-log-site-key "daemon" "pkg.F" "hello {name}")
                 "074892b7d933")))

(ert-deftest agent-repl-test-log-site-key-is-twelve-lowercase-hex ()
  "A key is twelve lowercase hex characters, whatever its template holds."
  ;; Act
  (let ((key (agent-repl-log-site-key "emacs" "f" "ws={ws} … opened")))
    ;; Assert
    (should (string-match-p "\\`[0-9a-f]\\{12\\}\\'" key))))

(ert-deftest agent-repl-test-log-site-macro-site-carries-the-emacs-key ()
  "A call's site holds the key of the emacs runtime, its function and template."
  ;; Act
  (let ((site (agent-repl-test-log-site--site '(agent-repl--info nil "opened {ws}" :ws w))))
    ;; Assert
    (should (equal (agent-repl-log-site-key-of site)
                   (agent-repl-log-site-key "emacs" "f" "opened {ws}")))))

(ert-deftest agent-repl-test-log-site-operation-spells-placeholders-as-s ()
  "A site's operation is its format string's: each placeholder spelled %s."
  ;; Act
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "elisp.x.opened ws={ws} n={n}" :ws w :n 1))))
    ;; Assert
    (should (equal (agent-repl-log-site-operation site)
                   "agent-repl.elisp-x-opened-ws-s-n-s"))))

;;;; ---- A record's operation ----

(ert-deftest agent-repl-test-log-site-record-operation-of-a-static-slug-is-the-sites ()
  "A slug naming no placeholder records the site's one static operation."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "elisp.x.opened ws={ws} n={n}" :ws w :n 1))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-record-operation site '("alpha" 3))
                   (agent-repl-log-site-operation site)))))

(ert-deftest agent-repl-test-log-site-record-operation-spells-a-slug-placeholder-with-its-value ()
  "A placeholder in the leading slug is spelled with its value, the rest %s."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--warn nil "elisp.verbs.{op}-refused ws={ws}" :op op :ws w))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-record-operation site '("finish" "alpha"))
                   "agent-repl.elisp-verbs-finish-refused-ws-s"))))

(ert-deftest agent-repl-test-log-site-record-operation-spells-a-printed-slug-value-as-printed ()
  "A slug placeholder whose value is printed is spelled as `prin1' prints it."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "elisp.host.{trigger}-registered" :trigger (prin1-to-string tr)))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-record-operation site '(link-up))
                   "agent-repl.elisp-host-link-up-registered"))))

(ert-deftest agent-repl-test-log-site-record-operation-is-what-a-record-carries ()
  "A written record's operation is its record operation, not the static one."
  ;; Arrange
  (let* ((message (agent-repl--log-msg "elisp.verbs.{op}-refused ws={ws}" :op "finish" :ws "alpha"))
         ;; Act
         (record (json-parse-string
                  (agent-repl--log-record nil "warn" "normal" (car message) (cdr message))
                  :object-type 'alist)))
    ;; Assert
    (should (equal (alist-get 'operation record) "agent-repl.elisp-verbs-finish-refused-ws-s"))))

;;;; ---- Rendering ----

(ert-deftest agent-repl-test-log-site-render-fills-each-placeholder ()
  "Each placeholder renders its named value, a string as itself."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "ws={ws} rows={rows}" :rows n :ws w))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-render site '(3 "alpha"))
                   "ws=alpha rows=3"))))

(ert-deftest agent-repl-test-log-site-render-prints-a-prin1-value ()
  "A value written (prin1-to-string X) renders as %S printed it."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "dir={dir}" :dir (prin1-to-string d)))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-render site '("/tmp/a b")) "dir=\"/tmp/a b\""))))

(ert-deftest agent-repl-test-log-site-prin1-value-is-printed-only-when-rendered ()
  "A (prin1-to-string X) value passes X itself: the printing is deferred."
  ;; Act
  (let ((expansion (agent-repl-test-log-site--expanding
                     (macroexpand '(agent-repl--info nil "dir={dir}" :dir (prin1-to-string d))))))
    ;; Assert
    (should (equal (nthcdr 3 expansion) '(d)))))

(ert-deftest agent-repl-test-log-site-render-renders-a-repeated-placeholder-twice ()
  "A placeholder written twice renders its one value at both places."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "{ws} then {ws}" :ws w))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-render site '("a")) "a then a"))))

(ert-deftest agent-repl-test-log-site-render-undoubles-literal-braces ()
  "A doubled brace renders as one literal brace."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "{{x}} {ws} }}" :ws w))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-render site '("a")) "{x} a }"))))

(ert-deftest agent-repl-test-log-site-context-keeps-a-number-a-number ()
  "A number's context field is the number; other values are their text."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "n={n} ws={ws} sym={sym}" :n n :ws w :sym s))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-context-pairs site '(3 "a" foo))
                   '(("n" . 3) ("ws" . "a") ("sym" . "foo"))))))

(ert-deftest agent-repl-test-log-site-context-holds-a-prin1-value-printed ()
  "A prin1 value's context field is its printed text, as the message shows it."
  ;; Arrange
  (let ((site (agent-repl-test-log-site--site
               '(agent-repl--info nil "n={n}" :n (prin1-to-string n)))))
    ;; Act / Assert
    (should (equal (agent-repl-log-site-context-pairs site '("3")) '(("n" . "\"3\""))))))

;;;; ---- Refused at macroexpansion ----

(ert-deftest agent-repl-test-log-site-refuses-a-non-literal-template ()
  "A template that is not a string literal fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil (format "x %s" y))))))

(ert-deftest agent-repl-test-log-site-refuses-a-variable-template ()
  "A template held in a variable fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--warn nil template)))))

(ert-deftest agent-repl-test-log-site-refuses-an-unclosed-placeholder ()
  "A placeholder that never closes fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={ws" :ws w)))))

(ert-deftest agent-repl-test-log-site-refuses-a-placeholder-that-is-no-identifier ()
  "A placeholder whose name is not a lowercase identifier fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={Ws}" :Ws w)))))

(ert-deftest agent-repl-test-log-site-refuses-a-hyphenated-placeholder ()
  "A hyphen is no identifier character: {a-b} fails, as in the Go grammar."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "x={a-b}" :a-b w)))))

(ert-deftest agent-repl-test-log-site-refuses-an-empty-placeholder ()
  "An empty placeholder fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "x={}")))))

(ert-deftest agent-repl-test-log-site-refuses-a-lone-closing-brace ()
  "A lone closing brace fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "x} y")))))

(ert-deftest agent-repl-test-log-site-refuses-a-placeholder-with-no-value ()
  "A placeholder no named value fills fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={ws} n={n}" :ws w)))))

(ert-deftest agent-repl-test-log-site-refuses-a-value-no-placeholder-places ()
  "A named value the template never places fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={ws}" :ws w :n 1)))))

(ert-deftest agent-repl-test-log-site-refuses-a-value-named-twice ()
  "A value named twice fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={ws}" :ws w :ws v)))))

(ert-deftest agent-repl-test-log-site-refuses-a-positional-value ()
  "A value with no :name before it fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={ws}" w)))))

(ert-deftest agent-repl-test-log-site-refuses-a-name-with-no-value ()
  "A trailing :name with no value fails at macroexpansion."
  (agent-repl-test-log-site--expanding
    (should-error (macroexpand '(agent-repl--info nil "ws={ws}" :ws)))))

(ert-deftest agent-repl-test-log-site-refuses-expansion-outside-any-file ()
  "A call expanded outside any file load or compile has no function, and fails."
  ;; Act / Assert
  (let ((agent-repl-log-site-function nil))
    (should-error (macroexpand '(agent-repl--info nil "x")))))

;;;; ---- The enclosing function ----

(defconst agent-repl-test-log-site--fixture
  ";;; fixture -*- lexical-binding: t -*-
(require 'log-site)
(defun lsf-defun () (when t (agent-repl--log nil \"lsf defun\")))
(cl-defun lsf-cl-defun (&key a) (agent-repl--log nil \"lsf cl-defun {a}\" :a a))
(cl-defmethod lsf-method ((x integer)) (agent-repl--log nil \"lsf method {x}\" :x x))
(define-minor-mode lsf-mode \"Doc.\" :global t :group 'lsf (agent-repl--log nil \"lsf mode\"))
(with-eval-after-load 'log-site (defun lsf-nested () (agent-repl--log nil \"lsf nested\")))
(add-hook 'lsf-hook (lambda () (agent-repl--log nil \"lsf toplevel lambda\")))
(defvar lsf-var (lambda () (agent-repl--log nil \"lsf defvar\")))
(defcustom lsf-custom nil \"Doc.\" :type 'sexp :group 'lsf
  :set (lambda (s v) (agent-repl--log nil \"lsf defcustom\") (set-default s v)))
(defsubst lsf-subst () (agent-repl--log nil \"lsf defsubst\"))
(defmacro lsf-macro () (agent-repl--log nil \"lsf defmacro\") nil)
(defun lsf-pcase (x) (pcase x (1 (agent-repl--log nil \"lsf pcase one\")) (_ (agent-repl--log nil \"lsf pcase other\"))))
(progn (defun lsf-progn-a () (agent-repl--log nil \"lsf progn\"))
       (defun lsf-progn-b () (agent-repl--log nil \"lsf progn\")))
(eval-and-compile (defun lsf-expansion-time () (agent-repl--log-msg \"lsf expansion time\") nil))
(defmacro lsf-expanding () (lsf-expansion-time) nil)
(lsf-expanding)
"
  "A source whose every log call's expected function is named beside it.")

(defconst agent-repl-test-log-site--fixture-functions
  '(("lsf defun" . "lsf-defun")
    ("lsf cl-defun {a}" . "lsf-cl-defun")
    ("lsf method {x}" . "lsf-method")
    ("lsf mode" . "lsf-mode")
    ("lsf nested" . "lsf-nested")
    ("lsf toplevel lambda" . "lsf-fixture.el")
    ("lsf defvar" . "lsf-var")
    ("lsf defcustom" . "lsf-custom")
    ("lsf defsubst" . "lsf-subst")
    ("lsf defmacro" . "lsf-macro")
    ("lsf pcase one" . "lsf-pcase")
    ("lsf pcase other" . "lsf-pcase")
    ("lsf progn" . "lsf-progn-a")
    ("lsf progn" . "lsf-progn-b")
    ("lsf expansion time" . "lsf-expansion-time"))
  "(TEMPLATE . FUNCTION) for every call in `agent-repl-test-log-site--fixture'.")

(defun agent-repl-test-log-site--fixture-sites (expand)
  "Return the sorted (TEMPLATE . FUNCTION) of every site EXPAND expands.
EXPAND is called with the fixture's path; only the sites it registers in
`agent-repl-log-site--expanded' are answered."
  (agent-repl-test--with-temp-dir dir "log-site-fixture-"
    (let ((file (expand-file-name "lsf-fixture.el" dir))
          (agent-repl-log-site--expanded (make-hash-table :test #'equal))
          (sites nil))
      (with-temp-file file (insert agent-repl-test-log-site--fixture))
      (funcall expand file)
      (maphash (lambda (_key entry) (push (cons (nth 1 entry) (nth 0 entry)) sites))
               agent-repl-log-site--expanded)
      (sort sites (lambda (a b) (string< (format "%S" a) (format "%S" b)))))))

(defun agent-repl-test-log-site--expected-fixture-sites ()
  "The fixture's expected sites, sorted as `--fixture-sites' sorts them."
  (sort (copy-sequence agent-repl-test-log-site--fixture-functions)
        (lambda (a b) (string< (format "%S" a) (format "%S" b)))))

(ert-deftest agent-repl-test-log-site-function-is-the-enclosing-definition-on-load ()
  "Loaded from source, each call is keyed by its innermost definition, else its file."
  ;; Act
  (let ((sites (agent-repl-test-log-site--fixture-sites
                (lambda (file) (load file nil t t)))))
    ;; Assert
    (should (equal sites (agent-repl-test-log-site--expected-fixture-sites)))))

(ert-deftest agent-repl-test-log-site-function-is-the-same-when-compiled ()
  "Byte-compiled, each call is keyed by the same function as loaded from source."
  ;; Act
  (let ((sites (agent-repl-test-log-site--fixture-sites
                (lambda (file)
                  (let ((byte-compile-dest-file-function
                         (lambda (f) (concat (file-name-sans-extension f) ".elc")))
                        (byte-compile-warnings nil))
                    (should (byte-compile-file file)))))))
    ;; Assert
    (should (equal sites (agent-repl-test-log-site--expected-fixture-sites)))))

;;;; ---- The emitters ----

(ert-deftest agent-repl-test-log-site-log-at-refuses-an-unknown-level ()
  "`agent-repl--log-at' signals for a level no rung has."
  ;; Arrange
  (let ((message (agent-repl-test-log-site--expanding
                   (eval (macroexpand '(agent-repl--log-msg "x")) t))))
    ;; Act / Assert
    (should-error (apply #'agent-repl--emit-at 'loud nil message))))

(ert-deftest agent-repl-test-log-site-log-at-reaches-the-rung-of-its-level ()
  "`agent-repl--log-at' records through the emitter of the level it names."
  ;; Arrange
  (let ((message (agent-repl-test-log-site--expanding
                   (eval (macroexpand '(agent-repl--log-msg "x {n}" :n 1)) t)))
        (warned nil))
    (cl-letf (((symbol-function 'agent-repl--emit-warn)
               (agent-repl-test--record-into warned)))
      ;; Act
      (apply #'agent-repl--emit-at 'warn nil message))
    ;; Assert
    (should (equal warned '("x 1")))))

;;; test-log-site.el ends here
