;;; test-spell.el --- ERT tests for spell.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-spell.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Nothing here starts hunspell.  The three boundaries spell.el crosses
;; (`agent-repl--executable-find', `agent-repl--spell-flyspell-on' and
;; `agent-repl--spell-check-region') are stubbed per case, and the
;; dictionary's files live in a temporary cache dir the case names through
;; `XDG_CACHE_HOME'.  The harness turns `agent-repl-input-spellcheck' off for
;; every suite, so each case here that turns a composer on binds it back.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'flyspell)

(defvar ispell-current-dictionary)

;;;; ---- Fixtures ----

(defvar agent-repl-test-spell--logged nil
  "Records the stubbed logging ladder saw, oldest first, as (LEVEL . ARGS).")

(defmacro agent-repl-test-spell--with-logging (&rest body)
  "Run BODY with the logging ladder recording into `agent-repl-test-spell--logged'.
`agent-repl--fatal' records and then signals, as the real one does."
  (declare (indent 0))
  `(let ((agent-repl-test-spell--logged nil))
     (cl-letf (((symbol-function 'agent-repl--log)
                (lambda (&rest args) (push (cons 'log args) agent-repl-test-spell--logged)))
               ((symbol-function 'agent-repl--warn-once)
                (lambda (&rest args) (push (cons 'warn-once args) agent-repl-test-spell--logged) t))
               ((symbol-function 'agent-repl--fatal)
                (lambda (&rest args)
                  (push (cons 'fatal args) agent-repl-test-spell--logged)
                  (error "fatal: %s" (car (cdr args))))))
       ,@body)))

(defun agent-repl-test-spell--records (level)
  "Return the recorded calls at LEVEL, oldest first, each as its ARGS."
  (mapcar #'cdr (seq-filter (lambda (r) (eq (car r) level))
                            (reverse agent-repl-test-spell--logged))))

(defmacro agent-repl-test-spell--with-cache (var &rest body)
  "Run BODY with a fresh `XDG_CACHE_HOME' bound to VAR, removed afterwards."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "agent-repl-test-spell-cache-" t))
          (process-environment (cons (concat "XDG_CACHE_HOME=" ,var) process-environment)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun agent-repl-test-spell--manifest (files)
  "Write a manifest whose `spell-dictionary' entry installs FILES; return its path.
FILES is a list of installed names (the entries' `to')."
  (let ((path (make-temp-file "agent-repl-test-spell-manifest-" nil ".json")))
    (with-temp-file path
      (insert (json-serialize
               `((requirements
                  . [((id . "other") (kind . "program"))
                     ((id . "spell-dictionary") (kind . "download") (dest . "dicts")
                      (files . ,(vconcat (mapcar (lambda (f) `((from . "x") (to . ,f) (sha256 . "0")))
                                                 files))))])))))
    path))

(defun agent-repl-test-spell--install (cache)
  "Create the real manifest's dictionary files under CACHE; return its dir."
  (let ((dir (expand-file-name "agent-repl/dictionaries" cache)))
    (make-directory dir t)
    (dolist (ext '(".aff" ".dic"))
      (with-temp-file (expand-file-name (concat "en_US-scowl80" ext) dir) (insert "x")))
    dir))

;;;; ---- Tests: the cache root ----

(ert-deftest agent-repl-test-spell-cache-root-follows-xdg-cache-home ()
  "A set XDG_CACHE_HOME holds the cache, as the runner's CACHE_ROOT does."
  (let ((process-environment (cons "XDG_CACHE_HOME=/tmp/xdg" process-environment)))
    (should (equal (agent-repl--spell-cache-root) "/tmp/xdg/agent-repl"))))

(ert-deftest agent-repl-test-spell-cache-root-defaults-to-dot-cache ()
  "An unset XDG_CACHE_HOME means ~/.cache."
  (let ((process-environment (cons "XDG_CACHE_HOME" process-environment)))
    (should (equal (agent-repl--spell-cache-root) (expand-file-name "~/.cache/agent-repl")))))

(ert-deftest agent-repl-test-spell-cache-root-treats-empty-xdg-as-unset ()
  "An empty XDG_CACHE_HOME means ~/.cache, as the runner's ${VAR:-default} does."
  (let ((process-environment (cons "XDG_CACHE_HOME=" process-environment)))
    (should (equal (agent-repl--spell-cache-root) (expand-file-name "~/.cache/agent-repl")))))

;;;; ---- Tests: the dictionary, as the manifest declares it ----

(ert-deftest agent-repl-test-spell-dictionary-reads-the-real-manifest ()
  "The Emacs layer's own manifest names the dictionary the runner installs."
  (agent-repl-test-spell--with-cache cache
    (let ((dictionary (agent-repl--spell-dictionary "ws")))
      (should (equal (plist-get dictionary :name) "en_US-scowl80"))
      (should (equal (plist-get dictionary :dir)
                     (expand-file-name "agent-repl/dictionaries" cache)))
      (should (equal (sort (mapcar #'file-name-nondirectory (plist-get dictionary :files)) #'string<)
                     '("en_US-scowl80.aff" "en_US-scowl80.dic"))))))

(ert-deftest agent-repl-test-spell-dictionary-without-an-entry-is-fatal ()
  "A manifest with no spell-dictionary entry is a broken checkout, signalled and logged."
  (let ((manifest (make-temp-file "agent-repl-test-spell-manifest-" nil ".json"
                                  "{\"requirements\": [{\"id\": \"other\"}]}")))
    (unwind-protect
        (agent-repl-test-spell--with-logging
          (should-error (agent-repl--spell-dictionary "ws" manifest))
          (let ((fatal (car (agent-repl-test-spell--records 'fatal))))
            (should (equal (car fatal) "ws"))
            (should (string-match-p "outcome=no-entry" (nth 1 fatal)))
            (should (member manifest fatal))))
      (delete-file manifest))))

(ert-deftest agent-repl-test-spell-dictionary-with-two-affix-files-is-fatal ()
  "An entry installing two .aff files is not one hunspell dictionary."
  (let ((manifest (agent-repl-test-spell--manifest '("a.aff" "a.dic" "b.aff"))))
    (unwind-protect
        (agent-repl-test-spell--with-logging
          (should-error (agent-repl--spell-dictionary "ws" manifest))
          (let ((fatal (car (agent-repl-test-spell--records 'fatal))))
            (should (string-match-p "outcome=not-one-affix" (nth 1 fatal)))
            (should (member 2 fatal))))
      (delete-file manifest))))

(ert-deftest agent-repl-test-spell-dictionary-without-a-matching-dic-is-fatal ()
  "An entry whose .aff has no .dic of the same name is not a hunspell dictionary."
  (let ((manifest (agent-repl-test-spell--manifest '("a.aff" "b.dic"))))
    (unwind-protect
        (agent-repl-test-spell--with-logging
          (should-error (agent-repl--spell-dictionary "ws" manifest))
          (let ((fatal (car (agent-repl-test-spell--records 'fatal))))
            (should (string-match-p "outcome=no-dic" (nth 1 fatal)))
            (should (member "a" fatal))))
      (delete-file manifest))))

(ert-deftest agent-repl-test-spell-unmet-names-a-missing-program ()
  "No hunspell on PATH is unmet."
  (should (equal (agent-repl--spell-unmet nil '(:files nil))
                 '("hunspell is not on PATH"))))

(ert-deftest agent-repl-test-spell-unmet-names-each-absent-file ()
  "Every dictionary file not on disk is unmet, by path."
  (agent-repl-test-spell--with-cache cache
    (let ((present (expand-file-name "here.aff" cache))
          (absent (expand-file-name "gone.dic" cache)))
      (with-temp-file present (insert "x"))
      (should (equal (agent-repl--spell-unmet "/bin/hunspell" (list :files (list present absent)))
                     (list (format "%s is absent" absent)))))))

(ert-deftest agent-repl-test-spell-unmet-is-nil-when-everything-is-there ()
  "With the program and every file present nothing is unmet."
  (agent-repl-test-spell--with-cache cache
    (let ((present (expand-file-name "here.aff" cache)))
      (with-temp-file present (insert "x"))
      (should (null (agent-repl--spell-unmet "/bin/hunspell" (list :files (list present))))))))

;;;; ---- Tests: what is prose ----

(defun agent-repl-test-spell--prose-p (text)
  "Return whether the word marked by | in TEXT is prose."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (agent-repl--spell-prose-at-p (point))))

(ert-deftest agent-repl-test-spell-prose-a-plain-word ()
  "A word of a sentence is prose."
  (should (agent-repl-test-spell--prose-p "please re|cieve this")))

(ert-deftest agent-repl-test-spell-prose-a-sentence-final-word ()
  "A word ending its sentence is prose: a trailing dot is punctuation."
  (should (agent-repl-test-spell--prose-p "this is the en|d.")))

(ert-deftest agent-repl-test-spell-prose-a-contraction ()
  "A word with an apostrophe inside it is prose."
  (should (agent-repl-test-spell--prose-p "it does|n't work")))

(ert-deftest agent-repl-test-spell-prose-not-inside-a-fence ()
  "A word inside a fenced block is code."
  (should-not (agent-repl-test-spell--prose-p "look:\n```\nde|fun foo\n```\n")))

(ert-deftest agent-repl-test-spell-prose-not-a-fence-info-string ()
  "A fence's language name is not prose."
  (should-not (agent-repl-test-spell--prose-p "```pyth|on\nx = 1\n```\n")))

(ert-deftest agent-repl-test-spell-prose-after-a-closed-fence ()
  "Prose resumes once the fence closes."
  (should (agent-repl-test-spell--prose-p "```\nx\n```\nthen re|cieve")))

(ert-deftest agent-repl-test-spell-prose-not-inside-inline-code ()
  "A word inside an inline code span is code."
  (should-not (agent-repl-test-spell--prose-p "run `git sta|tus` now")))

(ert-deftest agent-repl-test-spell-prose-after-closed-inline-code ()
  "Prose resumes after an inline code span closes on the same line."
  (should (agent-repl-test-spell--prose-p "run `git` and re|cieve")))

(ert-deftest agent-repl-test-spell-prose-not-a-path ()
  "A path is code."
  (should-not (agent-repl-test-spell--prose-p "open ~/src/foo|bar/baz")))

(ert-deftest agent-repl-test-spell-prose-not-a-url ()
  "A URL is code."
  (should-not (agent-repl-test-spell--prose-p "see https://exam|ple.com/x")))

(ert-deftest agent-repl-test-spell-prose-not-a-slash-command ()
  "A slash command is code."
  (should-not (agent-repl-test-spell--prose-p "/merge-que|ue own")))

(ert-deftest agent-repl-test-spell-prose-not-a-snake-case-identifier ()
  "A snake_case identifier is code."
  (should-not (agent-repl-test-spell--prose-p "call foo_ba|r here")))

(ert-deftest agent-repl-test-spell-prose-not-a-camel-case-identifier ()
  "A camelCase identifier is code."
  (should-not (agent-repl-test-spell--prose-p "call ingestAnd|Fan here")))

(ert-deftest agent-repl-test-spell-prose-not-a-dotted-file-name ()
  "A file name with an extension is code."
  (should-not (agent-repl-test-spell--prose-p "edit spe|ll.el now")))

(ert-deftest agent-repl-test-spell-prose-not-an-address ()
  "An email address is code."
  (should-not (agent-repl-test-spell--prose-p "mail som|eone@example.com")))

(ert-deftest agent-repl-test-spell-prose-not-an-assignment ()
  "A KEY=VALUE token is code."
  (should-not (agent-repl-test-spell--prose-p "set FO|O=bar first")))

(ert-deftest agent-repl-test-spell-check-word-p-asks-about-the-word-start ()
  "The flyspell predicate asks `agent-repl--spell-prose-at-p' about the word's start."
  (with-temp-buffer
    (insert "call foo_bar here")
    (goto-char 9)
    (let ((asked nil)
          (ispell-current-dictionary "en_US-scowl80")
          (ispell-local-dictionary-alist
           (list (agent-repl--spell-dictionary-entry "en_US-scowl80"))))
      (cl-letf (((symbol-function 'agent-repl--spell-prose-at-p)
                 (lambda (pos) (setq asked pos) nil)))
        (should-not (agent-repl--spell-check-word-p))
        (should (equal asked 6))))))

;;;; ---- Tests: keys ----

(ert-deftest agent-repl-test-spell-keymap-releases-completion-and-embark ()
  "The composer's flyspell map binds neither C-M-i nor C-;."
  (let ((map (agent-repl--spell-keymap)))
    (should-not (keymap-lookup map "C-M-i"))
    (should-not (keymap-lookup map "C-;"))))

(ert-deftest agent-repl-test-spell-keymap-keeps-flyspells-other-keys ()
  "flyspell's correct and next-error keys stay."
  (let ((map (agent-repl--spell-keymap)))
    (should (eq (keymap-lookup map "C-.") (keymap-lookup flyspell-mode-map "C-.")))
    (should (eq (keymap-lookup map "C-,") (keymap-lookup flyspell-mode-map "C-,")))))

(ert-deftest agent-repl-test-spell-keymap-leaves-flyspells-own-map-alone ()
  "Releasing keys for the composer never edits `flyspell-mode-map' itself."
  (let ((before (keymap-lookup flyspell-mode-map "C-;")))
    (agent-repl--spell-keymap)
    (should before)
    (should (eq (keymap-lookup flyspell-mode-map "C-;") before))))

;;;; ---- Tests: turning it on ----

(defmacro agent-repl-test-spell--enabling (program &rest body)
  "Run BODY with PATH answering PROGRAM for hunspell and flyspell's start recorded.
Binds `started' to the number of flyspell starts.  Spellchecking is on,
which the harness turns off for every other suite, and Emacs's global
ispell state is bound afresh, so a case never leaks it."
  (declare (indent 1))
  `(let ((started 0)
         (agent-repl-input-spellcheck t)
         (ispell-program-name "ispell")
         (ispell-local-dictionary-alist nil)
         (process-environment (seq-remove (lambda (v) (string-match-p "\\`\\(DICPATH\\|DICTIONARY\\)=" v))
                                          process-environment)))
     (cl-letf (((symbol-function 'agent-repl--executable-find)
                (lambda (name) (and (equal name "hunspell") ,program)))
               ((symbol-function 'agent-repl--spell-flyspell-on)
                (lambda () (setq started (1+ started))))
               ((symbol-function 'agent-repl--info)
                (lambda (&rest args) (push (cons 'info args) agent-repl-test-spell--logged))))
       ,@body)))

(ert-deftest agent-repl-test-spell-enable-off-starts-nothing ()
  "With `agent-repl-input-spellcheck' nil the composer is not spellchecked, and the log says so."
  (agent-repl-test-spell--with-logging
    (agent-repl-test-spell--enabling "/bin/hunspell"
      (agent-repl-test--with-temp-buffer " *test-spell-composer*"
        (let ((agent-repl-input-spellcheck nil))
          (agent-repl-spell-enable "ws")
          (should (= started 0))
          (should (equal ispell-program-name "ispell"))
          (should-not (local-variable-p 'ispell-local-dictionary))
          (should (string-match-p "outcome=off"
                                  (nth 1 (car (agent-repl-test-spell--records 'log))))))))))

(ert-deftest agent-repl-test-spell-enable-without-hunspell-warns-with-the-fix ()
  "No hunspell on PATH: no flyspell, no composer settings, one warning naming the fix."
  (agent-repl-test-spell--with-cache cache
    (agent-repl-test-spell--install cache)
    (agent-repl-test-spell--with-logging
      (agent-repl-test-spell--enabling nil
        (agent-repl-test--with-temp-buffer " *test-spell-composer*"
          (agent-repl-spell-enable "ws")
          (should (= started 0))
          (should (equal ispell-program-name "ispell"))
          (should-not (getenv "DICPATH"))
          (should-not ispell-local-dictionary-alist)
          (should-not (local-variable-p 'ispell-local-dictionary))
          (should-not (memq #'agent-repl--spell-on-change after-change-functions))
          (let ((warning (car (agent-repl-test-spell--records 'warn-once))))
            (should (equal (car warning) "ws"))
            (should (string-match-p "outcome=unavailable" (nth 2 warning)))
            (should (member '("hunspell is not on PATH") warning))
            (should (seq-find (lambda (a) (and (stringp a) (string-match-p "bin/requirements.sh install" a)))
                              warning))))))))

(ert-deftest agent-repl-test-spell-enable-without-the-dictionary-warns-naming-each-file ()
  "A dictionary not installed: no flyspell, and the warning names every absent file."
  (agent-repl-test-spell--with-cache cache
    (agent-repl-test-spell--with-logging
      (agent-repl-test-spell--enabling "/bin/hunspell"
        (agent-repl-test--with-temp-buffer " *test-spell-composer*"
          (agent-repl-spell-enable "ws")
          (should (= started 0))
          (should (equal ispell-program-name "ispell"))
          (let ((unmet (nth 4 (car (agent-repl-test-spell--records 'warn-once)))))
            (should (= (length unmet) 2))
            (should (seq-every-p (lambda (u) (string-match-p "en_US-scowl80\\.\\(aff\\|dic\\) is absent" u))
                                 unmet))))))))

(ert-deftest agent-repl-test-spell-enable-sets-the-composer-up-and-starts-flyspell ()
  "Everything present: Emacs's spellchecker is hunspell, the composer picks the dictionary, then flyspell starts."
  (agent-repl-test-spell--with-cache cache
    (let ((dir (agent-repl-test-spell--install cache)))
      (agent-repl-test-spell--with-logging
        (agent-repl-test-spell--enabling "/opt/bin/hunspell"
          (agent-repl-test--with-temp-buffer " *test-spell-composer*"
            (agent-repl-spell-enable "ws")
            (should (= started 1))
            (should (equal ispell-program-name "/opt/bin/hunspell"))
            (should (equal (getenv "DICPATH") dir))
            (should (local-variable-p 'ispell-local-dictionary))
            (should (equal ispell-local-dictionary "en_US-scowl80"))
            (should (eq flyspell-generic-check-word-predicate #'agent-repl--spell-check-word-p))
            (should (assq 'flyspell-mode minor-mode-overriding-map-alist))
            (should (memq #'agent-repl--spell-on-change after-change-functions))
            (should (string-match-p "outcome=on"
                                    (nth 1 (car (agent-repl-test-spell--records 'log)))))))))))

(ert-deftest agent-repl-test-spell-enable-leaves-other-buffers-dictionary-alone ()
  "The composer's dictionary choice is its own: another buffer's stays as it was."
  (agent-repl-test-spell--with-cache cache
    (agent-repl-test-spell--install cache)
    (agent-repl-test-spell--with-logging
      (agent-repl-test-spell--enabling "/opt/bin/hunspell"
        (let ((before (default-value 'ispell-local-dictionary)))
          (agent-repl-test--with-temp-buffer " *test-spell-composer*"
            (agent-repl-spell-enable "ws"))
          (should (equal (default-value 'ispell-local-dictionary) before)))))))

;;;; ---- Tests: Emacs's spellchecker ----

(ert-deftest agent-repl-test-spell-configure-points-ispell-at-hunspell-and-says-so ()
  "A different program becomes hunspell, recorded with both programs."
  (agent-repl-test-spell--with-logging
    (agent-repl-test-spell--enabling "/opt/bin/hunspell"
      (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
      (should (equal ispell-program-name "/opt/bin/hunspell"))
      (let ((info (car (agent-repl-test-spell--records 'info))))
        (should (equal (car info) "ws"))
        (should (member "ispell" info))
        (should (member "/opt/bin/hunspell" info))))))

(ert-deftest agent-repl-test-spell-configure-again-records-nothing ()
  "Configuring the program it already is records nothing."
  (agent-repl-test-spell--with-logging
    (agent-repl-test-spell--enabling "/opt/bin/hunspell"
      (setq ispell-program-name "/opt/bin/hunspell")
      (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
      (should-not (agent-repl-test-spell--records 'info)))))

(ert-deftest agent-repl-test-spell-configure-puts-the-dir-first-on-dicpath ()
  "The dictionary's dir leads hunspell's DICPATH, ahead of the user's own dirs."
  (agent-repl-test-spell--enabling "/opt/bin/hunspell"
    (setenv "DICPATH" "/mine:/c/dicts")
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (should (equal (getenv "DICPATH") "/c/dicts:/mine"))))

(ert-deftest agent-repl-test-spell-configure-twice-keeps-one-dicpath-entry ()
  "Configuring again leaves DICPATH as it was."
  (agent-repl-test-spell--enabling "/opt/bin/hunspell"
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (should (equal (getenv "DICPATH") "/c/dicts"))))

(ert-deftest agent-repl-test-spell-configure-names-the-default-dictionary ()
  "With no DICTIONARY of the user's, hunspell's default is the composer's dictionary."
  (agent-repl-test-spell--enabling "/opt/bin/hunspell"
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (should (equal (getenv "DICTIONARY") "d"))))

(ert-deftest agent-repl-test-spell-configure-keeps-the-users-default-dictionary ()
  "A DICTIONARY the user set is theirs, and stays."
  (agent-repl-test-spell--enabling "/opt/bin/hunspell"
    (setenv "DICTIONARY" "en_GB")
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (should (equal (getenv "DICTIONARY") "en_GB"))))

(ert-deftest agent-repl-test-spell-configure-registers-the-dictionary-once ()
  "The dictionary is registered in `ispell-local-dictionary-alist' exactly once."
  (agent-repl-test-spell--enabling "/opt/bin/hunspell"
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (agent-repl--spell-configure-ispell "ws" "/opt/bin/hunspell" "d" "/c/dicts")
    (should (equal ispell-local-dictionary-alist
                   (list (agent-repl--spell-dictionary-entry "d"))))))

(ert-deftest agent-repl-test-spell-dictionary-entry-is-a-utf-8-letter-dictionary ()
  "The registered dictionary reads letters and inner apostrophes, in UTF-8, with no extra args."
  (should (equal (agent-repl--spell-dictionary-entry "d")
                 '("d" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t nil nil utf-8))))

;;;; ---- Tests: text that arrives whole ----

(defmacro agent-repl-test-spell--queueing (&rest body)
  "Run BODY in a composer whose after-change queues checks, with checks recorded.
Binds `checked' to the (BEG . END) spans checked, oldest first.  The
global queue and `post-command-hook' are bound afresh."
  (declare (indent 0))
  `(let ((checked nil)
         (agent-repl--spell-pending-buffers nil)
         (post-command-hook nil))
     (cl-letf (((symbol-function 'agent-repl--spell-check-region)
                (lambda (beg end) (setq checked (append checked (list (cons (+ 0 beg) (+ 0 end))))))))
       (agent-repl-test--with-temp-buffer " *test-spell-composer*"
         (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
         ,@body))))

(ert-deftest agent-repl-test-spell-a-typed-character-is-flyspells-own ()
  "A one-character insertion queues nothing."
  (agent-repl-test-spell--queueing
    (insert "a")
    (should-not agent-repl--spell-pending)
    (should-not agent-repl--spell-pending-buffers)
    (should-not (memq #'agent-repl--spell-check-pending-buffers post-command-hook))))

(ert-deftest agent-repl-test-spell-an-insertion-is-queued-for-the-next-command ()
  "A longer insertion is queued, and the check joins `post-command-hook'."
  (agent-repl-test-spell--queueing
    (insert "hello world")
    (should (equal (cons (marker-position (car agent-repl--spell-pending))
                         (marker-position (cdr agent-repl--spell-pending)))
                   '(1 . 12)))
    (should (equal agent-repl--spell-pending-buffers (list (current-buffer))))
    (should (memq #'agent-repl--spell-check-pending-buffers post-command-hook))))

(ert-deftest agent-repl-test-spell-a-second-insertion-widens-the-queue ()
  "Two insertions before the check are checked as one span covering both."
  (agent-repl-test-spell--queueing
    (insert "hello world")
    (goto-char (point-min))
    (insert "say ")
    (goto-char (point-max))
    (insert " again")
    (should (equal (cons (marker-position (car agent-repl--spell-pending))
                         (marker-position (cdr agent-repl--spell-pending)))
                   (cons 1 (point-max))))
    (should (equal agent-repl--spell-pending-buffers (list (current-buffer))))))

(ert-deftest agent-repl-test-spell-the-command-end-checks-and-forgets ()
  "The end of the next command checks the queued span once, then leaves the hook."
  (agent-repl-test-spell--queueing
    (setq-local flyspell-mode t)
    (insert "hello world")
    (run-hooks 'post-command-hook)
    (should (equal checked '((1 . 12))))
    (should-not agent-repl--spell-pending)
    (should-not agent-repl--spell-pending-buffers)
    (should-not (memq #'agent-repl--spell-check-pending-buffers post-command-hook))
    (agent-repl--spell-check-pending (current-buffer))
    (should (equal checked '((1 . 12))))))

(ert-deftest agent-repl-test-spell-the-command-end-checks-every-queued-composer ()
  "Two composers queued before a command ends are both checked."
  (agent-repl-test-spell--queueing
    (setq-local flyspell-mode t)
    (insert "hello world")
    (let ((other (generate-new-buffer " *test-spell-other*")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (setq-local flyspell-mode t)
              (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
              (insert "abc def"))
            (run-hooks 'post-command-hook)
            (should (equal checked '((1 . 12) (1 . 8)))))
        (kill-buffer other)))))

(ert-deftest agent-repl-test-spell-the-pending-check-skips-a-composer-without-flyspell ()
  "A composer whose flyspell was turned off checks nothing, and forgets the queue."
  (agent-repl-test-spell--queueing
    (setq-local flyspell-mode nil)
    (insert "hello world")
    (run-hooks 'post-command-hook)
    (should-not checked)
    (should-not agent-repl--spell-pending)))

(ert-deftest agent-repl-test-spell-a-killed-composer-is-not-checked ()
  "A composer killed before the command ended is dropped from the queue unchecked."
  (agent-repl-test-spell--queueing
    (let ((buf (generate-new-buffer " *test-spell-killed*")))
      (with-current-buffer buf
        (setq-local flyspell-mode t)
        (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
        (insert "hello world"))
      (kill-buffer buf)
      (run-hooks 'post-command-hook)
      (should-not checked)
      (should-not agent-repl--spell-pending-buffers))))

;;;; ---- Tests: the boundary ----

(ert-deftest agent-repl-test-spell-boundaries-are-registered ()
  "Starting flyspell and checking a region are registered boundaries the harness guards."
  (should (memq 'agent-repl--spell-flyspell-on agent-repl--external-boundary-functions))
  (should (memq 'agent-repl--spell-check-region agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-test-spell-the-harness-starts-composers-unspellchecked ()
  "Every other suite's composers are born unspellchecked, so none reaches hunspell."
  (should-not (default-value 'agent-repl-input-spellcheck)))

(provide 'test-spell)

;;; test-spell.el ends here
