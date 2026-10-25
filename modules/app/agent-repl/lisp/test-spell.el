;;; test-spell.el --- ERT tests for spell.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-spell.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Nothing here starts hunspell.  The three boundaries spell.el crosses
;; (`agent-repl--executable-find', `agent-repl--spell-flyspell-on' and
;; `agent-repl--spell-run-hunspell') are stubbed per case, and the
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
  `(agent-repl-test--with-temp-dir ,var "agent-repl-test-spell-cache-"
     (let ((process-environment (cons (concat "XDG_CACHE_HOME=" ,var) process-environment)))
       ,@body)))

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
            (should (equal agent-repl--spell-ws "ws"))
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
  "Run BODY in a composer whose after-change queues checks, with hunspell recorded.
Binds `ran' to the (INPUT . CALLBACK) of each hunspell run, oldest first.
The global queue is bound afresh, and a timer it arms is cancelled after."
  (declare (indent 0))
  `(let ((ran nil)
         (agent-repl--spell-pending-buffers nil)
         (agent-repl--spell-check-timer nil))
     (cl-letf (((symbol-function 'agent-repl--spell-run-hunspell)
                (lambda (input callback) (setq ran (append ran (list (cons input callback)))))))
       (unwind-protect
           (agent-repl-test--with-temp-buffer " *test-spell-composer*"
             (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
             ,@body)
         (when agent-repl--spell-check-timer
           (cancel-timer agent-repl--spell-check-timer))))))

(defun agent-repl-test-spell--sent (input)
  "Return the text hunspell INPUT checks, without the `!' and `^' framing."
  (mapconcat (lambda (line) (substring line 1))
             (cdr (split-string (string-remove-suffix "\n" input) "\n")) "\n"))

(ert-deftest agent-repl-test-spell-a-typed-character-is-flyspells-own ()
  "A one-character insertion queues nothing and arms no check."
  (agent-repl-test-spell--queueing
    (insert "a")
    (should-not agent-repl--spell-pending)
    (should-not agent-repl--spell-pending-buffers)
    (should-not agent-repl--spell-check-timer)))

(ert-deftest agent-repl-test-spell-an-insertion-is-queued-for-the-next-loop-turn ()
  "A longer insertion is queued, and a zero-second timer is armed to check it."
  (agent-repl-test-spell--queueing
    (insert "hello world")
    (should (equal (cons (marker-position (car agent-repl--spell-pending))
                         (marker-position (cdr agent-repl--spell-pending)))
                   '(1 . 12)))
    (should (equal agent-repl--spell-pending-buffers (list (current-buffer))))
    (should (timerp agent-repl--spell-check-timer))
    (should (equal (timer--function agent-repl--spell-check-timer)
                   #'agent-repl--spell-check-pending-buffers))))

(ert-deftest agent-repl-test-spell-a-second-insertion-widens-the-queue ()
  "Two insertions before the check are one span covering both, under one timer."
  (agent-repl-test-spell--queueing
    (insert "hello world")
    (let ((timer agent-repl--spell-check-timer))
      (goto-char (point-min))
      (insert "say ")
      (goto-char (point-max))
      (insert " again")
      (should (eq agent-repl--spell-check-timer timer)))
    (should (equal (cons (marker-position (car agent-repl--spell-pending))
                         (marker-position (cdr agent-repl--spell-pending)))
                   (cons 1 (point-max))))
    (should (equal agent-repl--spell-pending-buffers (list (current-buffer))))))

(ert-deftest agent-repl-test-spell-the-check-sends-the-span-and-forgets ()
  "The timer's run sends the queued span to hunspell once, then disarms."
  (agent-repl-test-spell--queueing
    (setq-local flyspell-mode t)
    (insert "hello world")
    (agent-repl--spell-check-pending-buffers)
    (should (equal (mapcar (lambda (r) (agent-repl-test-spell--sent (car r))) ran)
                   '("hello world")))
    (should-not agent-repl--spell-pending)
    (should-not agent-repl--spell-pending-buffers)
    (should-not agent-repl--spell-check-timer)
    (agent-repl--spell-check-pending (current-buffer))
    (should (= (length ran) 1))))

(ert-deftest agent-repl-test-spell-the-check-holds-nothing-up ()
  "Sending the span returns before hunspell answers: nothing is highlighted yet."
  (agent-repl-test-spell--queueing
    (setq-local flyspell-mode t)
    (insert "wrold")
    (agent-repl--spell-check-pending-buffers)
    (should (= (length ran) 1))
    (should-not (seq-filter #'flyspell-overlay-p (overlays-in (point-min) (point-max))))))

(ert-deftest agent-repl-test-spell-the-check-covers-every-queued-composer ()
  "Two composers queued before the timer runs are both sent, oldest first."
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
            (agent-repl--spell-check-pending-buffers)
            (should (equal (mapcar (lambda (r) (agent-repl-test-spell--sent (car r))) ran)
                           '("hello world" "abc def"))))
        (kill-buffer other)))))

(ert-deftest agent-repl-test-spell-the-check-skips-a-composer-without-flyspell ()
  "A composer whose flyspell was turned off sends nothing, and forgets the queue."
  (agent-repl-test-spell--queueing
    (setq-local flyspell-mode nil)
    (insert "hello world")
    (let ((span agent-repl--spell-pending))
      (agent-repl--spell-check-pending-buffers)
      (should-not ran)
      (should-not agent-repl--spell-pending)
      (should-not (marker-buffer (car span))))))

(ert-deftest agent-repl-test-spell-a-killed-composer-is-not-checked ()
  "A composer killed before the timer ran is dropped from the queue unsent."
  (agent-repl-test-spell--queueing
    (let ((buf (generate-new-buffer " *test-spell-killed*")))
      (with-current-buffer buf
        (setq-local flyspell-mode t)
        (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
        (insert "hello world"))
      (kill-buffer buf)
      (agent-repl--spell-check-pending-buffers)
      (should-not ran)
      (should-not agent-repl--spell-pending-buffers))))

(ert-deftest agent-repl-test-spell-hunspell-input-is-terse-and-escapes-each-line ()
  "The input asks for terse answers and puts `^' before every line."
  (should (equal (agent-repl--spell-hunspell-input "one\n*two")
                 "!\n^one\n^*two\n")))

(ert-deftest agent-repl-test-spell-hunspell-input-keeps-an-empty-line ()
  "An empty line is still a line, so the answer's lines keep their count."
  (should (equal (agent-repl--spell-hunspell-input "a\n\nb") "!\n^a\n^\n^b\n")))

(ert-deftest agent-repl-test-spell-misspelled-words-reads-each-answer-kind ()
  "`&', `#' and `?' lines name a misspelling; the banner and blank lines do not."
  (should (equal (agent-repl--spell-misspelled-words
                  (concat "@(#) International Ispell Version 3.2.06 (but really Hunspell 1.7.2)\n"
                          "& wrold 2 7: world, wold\n"
                          "# xyzzyq 3\n"
                          "\n"
                          "? teh 0 9: the\n"
                          "\n"))
                 '("wrold" "xyzzyq" "teh"))))

(ert-deftest agent-repl-test-spell-misspelled-words-names-a-word-once ()
  "A word misspelled on two lines is named once."
  (should (equal (agent-repl--spell-misspelled-words "& wrold 1 0: world\n\n& wrold 1 0: world\n")
                 '("wrold"))))

(ert-deftest agent-repl-test-spell-misspelled-words-of-a-clean-answer-is-nil ()
  "An answer with nothing misspelled names nothing."
  (should-not (agent-repl--spell-misspelled-words "@(#) banner\n\n\n")))

(defmacro agent-repl-test-spell--answering (text &rest body)
  "Run BODY in a spellchecked composer holding TEXT, its whole span queued and sent.
Binds `answer' to the sent check's callback, `span' to its markers, and
`marks' to a function returning the highlighted words, in buffer order."
  (declare (indent 1))
  `(agent-repl-test-spell--queueing
     (setq-local flyspell-mode t)
     (setq-local agent-repl--spell-ws "ws-a")
     (setq-local ispell-local-dictionary "en_US-scowl80")
     (let ((ispell-current-dictionary "en_US-scowl80")
           (ispell-local-dictionary-alist
            (list (agent-repl--spell-dictionary-entry "en_US-scowl80"))))
       (insert ,text)
       (let ((span agent-repl--spell-pending))
         (agent-repl--spell-check-pending-buffers)
         (let ((answer (cdr (car ran)))
               (marks (lambda ()
                        (mapcar (lambda (o) (buffer-substring-no-properties
                                             (overlay-start o) (overlay-end o)))
                                (sort (seq-filter #'flyspell-overlay-p
                                                  (overlays-in (point-min) (point-max)))
                                      (lambda (a b) (< (overlay-start a) (overlay-start b))))))))
           ,@body)))))

(ert-deftest agent-repl-test-spell-an-answer-highlights-each-misspelled-word ()
  "Every prose occurrence of a word hunspell names is highlighted."
  (agent-repl-test-spell--answering "say wrold and wrold again"
    (funcall answer t "@(#) banner\n& wrold 1 4: world\n\n")
    (should (equal (funcall marks) '("wrold" "wrold")))))

(ert-deftest agent-repl-test-spell-an-answer-leaves-code-unmarked ()
  "A named word inside inline code or a code token is not highlighted."
  (agent-repl-test-spell--answering "wrold `wrold` wrold_x"
    (funcall answer t "& wrold 1 0: world\n\n")
    (should (equal (funcall marks) '("wrold")))))

(ert-deftest agent-repl-test-spell-an-answer-marks-only-whole-words ()
  "A named word standing inside a longer word is not highlighted there."
  (agent-repl-test-spell--answering "teh tehran"
    (funcall answer t "& teh 1 0: the\n\n")
    (should (equal (funcall marks) '("teh")))))

(ert-deftest agent-repl-test-spell-an-answer-follows-an-edit-made-meanwhile ()
  "Text typed before the answer came moves the word, and the mark follows it."
  (agent-repl-test-spell--answering "a wrold"
    (goto-char (point-min))
    (insert "x")
    (funcall answer t "& wrold 1 2: world\n\n")
    (should (equal (funcall marks) '("wrold")))
    (should (equal (overlay-start (car (seq-filter #'flyspell-overlay-p
                                                   (overlays-in (point-min) (point-max)))))
                   4))))

(ert-deftest agent-repl-test-spell-an-answer-stays-inside-its-span ()
  "A named word outside the checked span is flyspell's own, not the answer's."
  (agent-repl-test-spell--answering "a wrold"
    (set-marker (cdr span) 3)
    (funcall answer t "& wrold 1 2: world\n\n")
    (should-not (funcall marks))))

(ert-deftest agent-repl-test-spell-an-answer-releases-the-span ()
  "Once applied, the span's markers point nowhere."
  (agent-repl-test-spell--answering "a wrold"
    (funcall answer t "\n")
    (should-not (marker-buffer (car span)))
    (should-not (marker-buffer (cdr span)))))

(ert-deftest agent-repl-test-spell-an-answer-after-flyspell-stopped-marks-nothing ()
  "A composer whose flyspell was turned off while hunspell worked takes nothing."
  (agent-repl-test-spell--answering "a wrold"
    (setq-local flyspell-mode nil)
    (funcall answer t "& wrold 1 2: world\n\n")
    (should-not (funcall marks))
    (should-not (marker-buffer (car span)))))

(ert-deftest agent-repl-test-spell-an-answer-for-a-killed-composer-is-dropped ()
  "A composer killed while hunspell worked takes nothing and raises nothing."
  (agent-repl-test-spell--queueing
    (let ((buf (generate-new-buffer " *test-spell-killed*"))
          (span nil))
      (with-current-buffer buf
        (setq-local flyspell-mode t)
        (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
        (insert "a wrold")
        (setq span agent-repl--spell-pending))
      (agent-repl--spell-check-pending-buffers)
      (kill-buffer buf)
      (funcall (cdr (car ran)) t "& wrold 1 2: world\n\n")
      (should-not (marker-buffer (car span))))))

(ert-deftest agent-repl-test-spell-a-failed-check-warns-and-marks-nothing ()
  "A hunspell that failed is recorded at WARN with what it said; nothing is marked."
  (agent-repl-test-spell--with-logging
    (cl-letf (((symbol-function 'agent-repl--warn)
               (lambda (&rest args) (push (cons 'warn args) agent-repl-test-spell--logged))))
      (agent-repl-test-spell--answering "a wrold"
        (funcall answer nil "Can't open affix or dictionary files\n")
        (should-not (funcall marks))
        (should-not (marker-buffer (car span)))
        (let ((warned (agent-repl-test-spell--records 'warn)))
          (should (= (length warned) 1))
          (should (equal (car warned)
                         (list "ws-a" "elisp.spell.check-failed ws=%s buffer=%s span-length=%d output=%S"
                               "ws-a" " *test-spell-composer*" 7
                               "Can't open affix or dictionary files"))))))))

(ert-deftest agent-repl-test-spell-run-hunspell-runs-through-async-command ()
  "hunspell shares the one async runner, with the composer's dictionary and stdin."
  (let ((real (cdr (assq 'agent-repl--spell-run-hunspell
                         agent-repl-test--external-original-functions)))
        (ran nil))
    (cl-letf (((symbol-function 'agent-repl--async-command)
               (lambda (&rest args) (setq ran args))))
      (with-temp-buffer
        (setq-local ispell-local-dictionary "en_US-scowl80")
        (let ((ispell-program-name "/bin/hunspell"))
          (funcall real "!\n^hi\n" #'ignore))))
    (should (equal ran (list "spell" temporary-file-directory
                             '("/bin/hunspell" "-a" "-i" "utf-8" "-d" "en_US-scowl80")
                             #'ignore "!\n^hi\n")))))

;;;; ---- Tests: the boundary ----

(ert-deftest agent-repl-test-spell-boundaries-are-registered ()
  "Starting flyspell and running hunspell are registered boundaries the harness guards."
  (should (memq 'agent-repl--spell-flyspell-on agent-repl--external-boundary-functions))
  (should (memq 'agent-repl--spell-run-hunspell agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-test-spell-the-harness-starts-composers-unspellchecked ()
  "Every other suite's composers are born unspellchecked, so none reaches hunspell."
  (should-not (default-value 'agent-repl-input-spellcheck)))

(provide 'test-spell)

;;; test-spell.el ends here
