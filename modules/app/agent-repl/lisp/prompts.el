;;; prompts.el --- file-backed automatic prompt texts -*- lexical-binding: t; -*-

;;; Commentary:
;; Loader for the AUTOMATIC prompt texts this module sends to agents —
;; the briefs no human types, composed by the harness itself.
;;
;; The texts live as plain files under `agent-repl-prompts-dir' so a user
;; can customize what the system says without editing Elisp.  That is the
;; whole point of this file, and it drives every decision here:
;;
;;   - The file is read AT USE TIME, never captured into a `defconst' at
;;     load time.  Editing a prompt takes effect on the very next send,
;;     with no `doom/reload' and no Emacs restart.
;;   - A missing, unreadable, or empty prompt file signals a plain
;;     `error'.  It is never papered over with a baked-in copy: silently
;;     sending a different brief than the one on disk is exactly the
;;     failure this file exists to prevent, and callers that compose a
;;     workspace's first message would rather fail loudly than spawn a
;;     workspace around nothing.
;;   - A placeholder left unsubstituted signals too, naming both what the
;;     file still wants and what the call site supplied.  A customized
;;     file with a typo would otherwise ship "{{raw_prmopt}}" to an agent
;;     as if it were prose.
;;
;; Placeholder syntax is `{{name}}', uniform across every prompt file and
;; every runtime (Elisp here, Go in daemon/internal/prompts).

;;; Code:

(require 'subr-x)
(require 'seq)

(defvar agent-repl-prompts-dir
  (expand-file-name "../prompts"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the directory holding this module's prompt files.
`prompts/' sits at the module root, one level above this file's `lisp/'.
Captured at file-load time because `load-file-name' is only bound
during load.  It names a DIRECTORY, not any file's contents, so
capturing it is not the load-time snapshot this file exists to avoid:
the texts themselves are still read on every use.")

(defconst agent-repl--prompt-header-re
  "\\`<!--\\(.\\|\n\\)*?-->\n"
  "Regexp matching a prompt file's leading `<!-- used by: ... -->' header.
The header documents the file for whoever edits it and is stripped
before the text reaches an agent.  Only a LEADING comment is a header,
so ordinary prose may contain HTML comments of its own.")

(defconst agent-repl--prompt-placeholder-re
  "{{\\([a-z0-9_]+\\)}}"
  "Regexp matching one `{{name}}' substitution point.
The name charset is deliberately narrow so ordinary prose containing
braces (a code sample, a shell expansion) is not mistaken for a
placeholder.")

(defun agent-repl--prompt-file (name)
  "Absolute path of the prompt file NAME inside `agent-repl-prompts-dir'."
  (expand-file-name name agent-repl-prompts-dir))

(defun agent-repl--prompt-read (name)
  "Return the raw contents of prompt file NAME.
Signals a plain `error' naming the path when the file is absent or
unreadable.  There is no fallback: see this file's Commentary."
  (let ((path (agent-repl--prompt-file name)))
    (unless (file-readable-p path)
      (error "agent-repl: prompt file %s is missing or unreadable; \
no baked-in copy exists, so the operation that needed it cannot proceed"
             path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun agent-repl--prompt-strip-header (text)
  "Return TEXT with its leading prompt-file header comment removed."
  (if (string-match agent-repl--prompt-header-re text)
      (substring text (match-end 0))
    text))

(defun agent-repl--prompt-strip-terminator (text)
  "Return TEXT with exactly one trailing newline removed.
A prompt file ends with a newline because every text editor puts one
there; the prompts themselves do not.  A file that wants a trailing
newline in the prompt itself carries two."
  (if (string-suffix-p "\n" text)
      (substring text 0 (1- (length text)))
    text))

(defun agent-repl--prompt-placeholders (text)
  "Return the sorted, de-duplicated `{{name}}' placeholders found in TEXT.

Callers scan the TEMPLATE with this, never the substituted result: a
user prompt or a failing test's output can itself contain something
shaped like `{{foo}}', and diagnosing that as an unsubstituted
placeholder would fail a perfectly good send over the user's own text."
  (let ((found nil)
        (start 0))
    (while (string-match agent-repl--prompt-placeholder-re text start)
      (let ((hit (match-string 0 text)))
        (unless (member hit found)
          (push hit found)))
      (setq start (match-end 0)))
    (sort found #'string<)))

(defun agent-repl--prompt-expected (substitutions)
  "Render SUBSTITUTIONS' placeholder names for an error message."
  (if (null substitutions)
      "no placeholders"
    (string-join (sort (mapcar (lambda (cell) (format "{{%s}}" (car cell)))
                               substitutions)
                       #'string<)
                 ", ")))

(defun agent-repl--prompt (name &optional substitutions)
  "Return the prompt in file NAME with SUBSTITUTIONS applied.

NAME is a basename inside `agent-repl-prompts-dir', e.g.
\"workspace-generation.md\".  SUBSTITUTIONS is an alist of
\(PLACEHOLDER-NAME . REPLACEMENT), where PLACEHOLDER-NAME is the bare
name inside the braces and REPLACEMENT is inserted literally.

Signals a plain `error' — never returns an approximation — when:
  - the file is missing or unreadable,
  - the file is empty once its header is stripped,
  - a `{{...}}' placeholder survives substitution (a typo in a
    customized file), or
  - a supplied placeholder appears nowhere in the file, which would
    silently drop its value out of the prompt."
  (let* ((path (agent-repl--prompt-file name))
         (body (agent-repl--prompt-strip-terminator
                (agent-repl--prompt-strip-header
                 (agent-repl--prompt-read name))))
         (wanted (agent-repl--prompt-placeholders body))
         (supplied (mapcar (lambda (cell) (format "{{%s}}" (car cell)))
                           substitutions))
         (unknown (seq-remove (lambda (hit) (member hit supplied)) wanted))
         (unused (seq-remove (lambda (hit) (member hit wanted)) supplied)))
    (when (string-empty-p (string-trim body))
      (error "agent-repl: prompt file %s is empty once its header is stripped; \
a blank prompt would send an empty turn" path))
    ;; Both checks run against the TEMPLATE, before any substitution, so a
    ;; user prompt that happens to contain "{{foo}}" is carried through as
    ;; the prose it is instead of being diagnosed as a typo.
    (when unknown
      (error "agent-repl: prompt file %s contains placeholder(s) %s this call \
site cannot substitute; it supplies %s — fix the placeholder spelling in the file"
             path (string-join unknown ", ")
             (agent-repl--prompt-expected substitutions)))
    (when unused
      (error "agent-repl: prompt file %s uses none of the placeholder(s) %s, \
so the value(s) behind them would be silently dropped from the prompt; this \
call site supplies %s"
             path (string-join (sort unused #'string<) ", ")
             (agent-repl--prompt-expected substitutions)))
    (dolist (cell substitutions)
      ;; A literal replacement keeps backslashes and ampersands in the
      ;; substituted VALUE from being read as match references.
      (setq body (replace-regexp-in-string
                  (regexp-quote (format "{{%s}}" (car cell)))
                  (cdr cell) body t t)))
    body))

(provide 'agent-repl-prompts)
;;; prompts.el ends here
