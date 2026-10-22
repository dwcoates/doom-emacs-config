;;; prompt-entries.el --- a prompt's entries, marked in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A PROMPT ANSWERS A RESPONSE'S TREE BY LABEL (`3.1.2 - my answer'), and the
;; composer marks every such entry's label while it is typed, so the person
;; sees which lines the daemon will read as entries.  What an entry IS comes
;; from the one grammar every program reads it with
;; (tree-sitter-tldr/tldr_prompt): the daemon parses a sent prompt with it
;; (daemon/internal/prose), and the composer parses its own text with it
;; here, through `treesit'.  No regular expression stands in for it.
;;
;; THE GRAMMAR IS BUILT HERE, FROM THE CHECKOUT.  Emacs loads a tree-sitter
;; grammar as a native library, which nothing else in agent-repl builds:
;; the generated parser and its scanner are compiled with `cc' into
;; agent-repl's own cache (`agent-repl-prompt-entries--library-dir'), never
;; the user's own `~/.emacs.d/tree-sitter', and rebuilt whenever their
;; content changes (the source hash beside the library).  The build is
;; asynchronous: a composer opened before it finishes is marked the moment
;; it does.  A build that fails is recorded at ERROR with the compiler's
;; output, and the composer marks nothing while the grammar is broken.
;;
;; WHAT IS MARKED.  Every heading the grammar reads whose line carries no
;; marker: a line opening on a label and a marker (`1 🔧 text') is a pasted
;; tree's root, not an entry -- the daemon's own rule (ParsePrompt).  The
;; label and its separator are drawn in `agent-repl-prompt-entry-label'
;; through an overlay, restated after every edit.

;;; Code:

(require 'cl-lib)
(require 'treesit)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--async-command "core" (label dir command callback))
(defvar agent-repl--frontend-root)

(defface agent-repl-prompt-entry-label
  '((t :inherit font-lock-constant-face :weight bold))
  "The face of an entry's label in the composer: a line answering a label."
  :group 'agent-repl)

(defcustom agent-repl-input-entry-marking t
  "Non-nil marks a prompt's entries in the composer as you type.
Read when a composer is born, so a change applies to composers created
after it."
  :type 'boolean
  :group 'agent-repl)

(defconst agent-repl-prompt-entries--language 'tldr_prompt
  "The tree-sitter language of a prompt's entries.")

(defconst agent-repl-prompt-entries--sources '("parser.c" "scanner.c")
  "The grammar's C sources, in its `src' directory.")

(defvar agent-repl-prompt-entries--library-dir
  (expand-file-name ".cache/agent-repl/tree-sitter" "~")
  "Where the built grammar library lives, agent-repl's own.
It is put on `treesit-extra-load-path', never the user's own grammar dir.")

(defvar agent-repl-prompt-entries--compiler "cc"
  "The C compiler the grammar is built with.")

(defun agent-repl-prompt-entries--grammar-dir ()
  "The grammar's generated `src' directory in this checkout."
  (expand-file-name "tree-sitter-tldr/tldr_prompt/src" agent-repl--frontend-root))

(defun agent-repl-prompt-entries--library ()
  "The path of the built grammar library."
  (expand-file-name (format "libtree-sitter-%s%s" agent-repl-prompt-entries--language
                            (if (eq system-type 'darwin) ".dylib" ".so"))
                    agent-repl-prompt-entries--library-dir))

(defun agent-repl-prompt-entries--stamp ()
  "The file recording which sources the built library was compiled from."
  (concat (agent-repl-prompt-entries--library) ".source"))

(defun agent-repl-prompt-entries--source-hash ()
  "The content hash of the grammar's C sources, as they stand now."
  (let ((dir (agent-repl-prompt-entries--grammar-dir)))
    (secure-hash
     'sha256
     (mapconcat (lambda (name)
                  (with-temp-buffer
                    (insert-file-contents-literally (expand-file-name name dir))
                    (buffer-string)))
                agent-repl-prompt-entries--sources
                "\0"))))

(defun agent-repl-prompt-entries--fresh-p ()
  "Whether the built library stands and was compiled from today's sources."
  (and (file-exists-p (agent-repl-prompt-entries--library))
       (file-exists-p (agent-repl-prompt-entries--stamp))
       (equal (with-temp-buffer
                (insert-file-contents (agent-repl-prompt-entries--stamp))
                (string-trim (buffer-string)))
              (agent-repl-prompt-entries--source-hash))))

(defvar agent-repl-prompt-entries--building nil
  "Non-nil while the grammar library is being built.")

(defvar agent-repl-prompt-entries--waiting nil
  "What waits for the grammar: functions called once it loads.")

(defun agent-repl-prompt-entries--on-load-path ()
  "Put the library's directory on `treesit-extra-load-path'."
  (add-to-list 'treesit-extra-load-path agent-repl-prompt-entries--library-dir))

(defun agent-repl-prompt-entries-ensure (on-ready)
  "Call ON-READY once the prompt grammar is loadable, building it first when stale.
A fresh library is put on the load path and ON-READY called at once;
otherwise ON-READY waits for the one build in flight, which this starts
when none is."
  (if (agent-repl-prompt-entries--fresh-p)
      (progn
        (agent-repl-prompt-entries--on-load-path)
        (funcall on-ready))
    (push on-ready agent-repl-prompt-entries--waiting)
    (unless agent-repl-prompt-entries--building
      (agent-repl-prompt-entries--start-build))))

(defun agent-repl-prompt-entries--start-build ()
  "Compile the grammar library, asynchronously, and answer the waiting on its end.
The compiler runs through `agent-repl--async-command', the one spawn of a
program that is not git."
  (let* ((dir (agent-repl-prompt-entries--grammar-dir))
         (library (agent-repl-prompt-entries--library))
         (hash (agent-repl-prompt-entries--source-hash))
         (command (append (list agent-repl-prompt-entries--compiler
                                "-std=c11" "-fPIC" "-shared" "-O2" "-I" dir "-o" library)
                          (mapcar (lambda (name) (expand-file-name name dir))
                                  agent-repl-prompt-entries--sources))))
    (make-directory agent-repl-prompt-entries--library-dir t)
    (agent-repl--info nil "elisp.prompt-entries.build-started library=%s source=%s" library hash)
    (setq agent-repl-prompt-entries--building t)
    (agent-repl--async-command
     "prompt-grammar" dir command
     (lambda (ok output)
       (agent-repl-prompt-entries--build-ended ok library hash output)))))

(defun agent-repl-prompt-entries--build-ended (ok library hash output)
  "Finish the build into LIBRARY from sources HASH, which succeeded when OK.
OUTPUT is the compiler's own words.  A success stamps HASH beside LIBRARY
and calls everything waiting; a failure is recorded at ERROR with OUTPUT,
and nothing waiting is called."
  (setq agent-repl-prompt-entries--building nil)
  (let ((waiting (nreverse agent-repl-prompt-entries--waiting)))
    (setq agent-repl-prompt-entries--waiting nil)
    (if (not ok)
        (agent-repl--error nil "elisp.prompt-entries.build-failed library=%s output=%S"
                           library (string-trim output))
      (with-temp-file (agent-repl-prompt-entries--stamp) (insert hash "\n"))
      (agent-repl-prompt-entries--on-load-path)
      (agent-repl--info nil "elisp.prompt-entries.built library=%s waiting=%d" library (length waiting))
      (mapc #'funcall waiting))))

;;;; ---- Marking the composer ----

(defvar-local agent-repl-prompt-entries--parser nil
  "This composer's parser of the prompt grammar, or nil before it loads.")

(defun agent-repl-prompt-entries-enable (ws)
  "Mark the entries of WS's composer, the current buffer, from now on.
Marking starts once the grammar loads, which may be after this returns.
Off when `agent-repl-input-entry-marking' is nil."
  (if (not agent-repl-input-entry-marking)
      (agent-repl--info ws "elisp.prompt-entries.enable ws=%s outcome=off reason=agent-repl-input-entry-marking-nil" ws)
    (let ((buf (current-buffer)))
      (agent-repl-prompt-entries-ensure
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq agent-repl-prompt-entries--parser
                 (treesit-parser-create agent-repl-prompt-entries--language))
           (add-hook 'after-change-functions #'agent-repl-prompt-entries--on-change nil t)
           (agent-repl-prompt-entries-mark)
           (agent-repl--info ws "elisp.prompt-entries.enabled ws=%s" ws))))))))

(defun agent-repl-prompt-entries--on-change (_beg _end _len)
  "Re-mark the composer's entries after an edit.
The composer's `after-change-functions' entry; unlogged, as it runs per
keystroke.  A whole-buffer pass: an edit can move where every later entry
begins, and a composer is short."
  (agent-repl-prompt-entries-mark))

(defun agent-repl-prompt-entries--label-spans ()
  "The (BEG . END) of every entry's label and separator in the current buffer.
A heading carrying a marker is a pasted tree's root and is left out."
  (let (spans)
    (dolist (capture (treesit-query-capture agent-repl-prompt-entries--parser
                                            '((heading) @heading)))
      (let* ((heading (cdr capture))
             (marker (treesit-search-subtree heading (lambda (n) (equal (treesit-node-type n) "marker")) nil nil 1))
             (label (treesit-search-subtree heading (lambda (n) (equal (treesit-node-type n) "label")) nil nil 1))
             (separator (treesit-search-subtree heading (lambda (n) (equal (treesit-node-type n) "separator")) nil nil 1)))
        (when (and label (null marker))
          (push (cons (treesit-node-start label)
                      (treesit-node-end (or separator label)))
                spans))))
    (nreverse spans)))

(defun agent-repl-prompt-entries-mark ()
  "Restate the overlays marking the current composer's entry labels."
  (when agent-repl-prompt-entries--parser
    (remove-overlays (point-min) (point-max) 'agent-repl-prompt-entry t)
    (dolist (span (agent-repl-prompt-entries--label-spans))
      (let ((ov (make-overlay (car span) (cdr span))))
        (overlay-put ov 'agent-repl-prompt-entry t)
        (overlay-put ov 'face 'agent-repl-prompt-entry-label)))))

(provide 'prompt-entries)

;;; prompt-entries.el ends here
