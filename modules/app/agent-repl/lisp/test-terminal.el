;;; test-terminal.el --- ERT tests for agent-repl terminal.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-terminal.el -f ert-run-tests-batch-and-exit
;;
;; terminal.el runs a shell code block's command in the terminal popup and
;; closes the popup when the shell reports the command's exit.  Batch Emacs
;; has no vterm and no Doom popup, so the toggle, the send and the popup's
;; window are stubbed: what these tests pin is which buffer is shown, what
;; text reaches it, which run is awaited, and when the window is closed.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defvar vterm-eval-cmds)

(defvar agent-repl-test-terminal--shown nil
  "Terminal buffers whose popup window is shown.")

(defvar agent-repl-test-terminal--sent nil
  "Each (BUFFER TEXT) the send function was handed, newest first.")

(defvar agent-repl-test-terminal--deleted nil
  "Each buffer whose window was deleted, newest first.")

(defvar agent-repl-test-terminal--logs nil
  "Each (LEVEL MESSAGE) logged during a test, newest first.")

(defvar agent-repl-test-terminal--messages nil
  "Each minibuffer message during a test, newest first.")

(defvar agent-repl-test-terminal--agent-sends nil
  "Each (ORIGIN TEXT WS) sent to the agent during a test, newest first.")

(defmacro agent-repl-test-terminal--with-harness (toggle &rest body)
  "Run BODY with the terminal's edges stubbed and TOGGLE as `SPC o t'.
TOGGLE is a function of the shown list answering the buffer it toggles."
  (declare (indent 1))
  `(let ((agent-repl-test-terminal--shown nil)
         (agent-repl-test-terminal--sent nil)
         (agent-repl-test-terminal--deleted nil)
         (agent-repl-test-terminal--logs nil)
         (agent-repl-test-terminal--messages nil)
         (agent-repl-test-terminal--agent-sends nil)
         (agent-repl-terminal--runs (make-hash-table :test 'eq))
         (agent-repl-terminal-toggle-function (lambda (_arg) (funcall ,toggle)))
         (agent-repl-terminal-send-function
          (lambda (buffer text) (push (list buffer text) agent-repl-test-terminal--sent))))
     (cl-letf (((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/work/tree"))
               ((symbol-function 'get-buffer-window)
                (lambda (buffer &rest _) (and (memq buffer agent-repl-test-terminal--shown) 'window)))
               ((symbol-function 'delete-window)
                (lambda (_window)
                  (let ((buffer (car agent-repl-test-terminal--shown)))
                    (push buffer agent-repl-test-terminal--deleted)
                    (setq agent-repl-test-terminal--shown (cdr agent-repl-test-terminal--shown)))))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args) (push (list :info (apply #'format fmt args)) agent-repl-test-terminal--logs)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args) (push (list :error (apply #'format fmt args)) agent-repl-test-terminal--logs)))
               ((symbol-function 'agent-repl--send)
                (lambda (origin text ws) (push (list origin text ws) agent-repl-test-terminal--agent-sends)))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) agent-repl-test-terminal--messages))))
       ,@body)))

(defun agent-repl-test-terminal--toggling (buffer)
  "A toggle of BUFFER's popup: shows it when hidden, hides it when shown."
  (lambda ()
    (if (memq buffer agent-repl-test-terminal--shown)
        (setq agent-repl-test-terminal--shown (delq buffer agent-repl-test-terminal--shown))
      (push buffer agent-repl-test-terminal--shown))
    buffer))

(defun agent-repl-test-terminal--logged (level prefix)
  "The first LEVEL record whose message starts with PREFIX, or nil."
  (cl-find-if (lambda (record) (and (eq (car record) level) (string-prefix-p prefix (cadr record))))
              agent-repl-test-terminal--logs))

(defmacro agent-repl-test-terminal--with-buffer (var &rest body)
  "Run BODY with VAR bound to a fresh stand-in terminal buffer."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer " *test-terminal*")))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,var) (kill-buffer ,var)))))

;;;; ---- The wrapped command ----

;; Temp-file names with a space exercise the shell quoting.
(ert-deftest agent-repl-test-terminal-wrap-groups-tees-and-reports-the-command ()
  "The command runs in a group teed to the output file, then printf reports the token and status."
  (should (equal (agent-repl-terminal-wrap "ls -la" "7" "/tmp/o u" "/tmp/s")
                 (concat "{ ( ls -la\n); echo $? >/tmp/s; } 2>&1 | tee /tmp/o\\ u; "
                         "printf '\\033]51;Eagent-repl-terminal-finished "
                         "\"%s\" \"%s\"\\033\\\\' 7 \"$(cat /tmp/s)\""))))

(ert-deftest agent-repl-test-terminal-wrap-drops-the-command-s-trailing-newline ()
  "A code block's trailing newline does not leave an empty line in the group."
  (should (string-prefix-p "{ ( make all\n); " (agent-repl-terminal-wrap "make all\n\n" "1" "/o" "/s"))))

;;;; ---- Running a command ----

(ert-deftest agent-repl-test-terminal-run-shows-the-popup-and-sends-the-wrapped-command ()
  "A hidden popup is toggled open once, and the wrapped command is sent to it."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (agent-repl-terminal-run "ws-1" "ls -la")
      ;; Assert
      (let ((token (plist-get (gethash buffer agent-repl-terminal--runs) :token)))
        (should (equal agent-repl-test-terminal--shown (list buffer)))
        (should (equal agent-repl-test-terminal--sent
                       (list (list buffer (agent-repl-terminal-wrap
                                           "ls -la" token
                                           (plist-get (gethash buffer agent-repl-terminal--runs) :output)
                                           (plist-get (gethash buffer agent-repl-terminal--runs) :status))))))
        (should (agent-repl-test-terminal--logged :info "elisp.terminal.run ws=ws-1"))))))

(ert-deftest agent-repl-test-terminal-run-leaves-a-shown-popup-shown ()
  "A popup already shown is toggled closed and back open, ending shown."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (push buffer agent-repl-test-terminal--shown)
      ;; Act
      (agent-repl-terminal-run "ws-1" "ls")
      ;; Assert
      (should (equal agent-repl-test-terminal--shown (list buffer)))
      (should (= (length agent-repl-test-terminal--sent) 1)))))

(ert-deftest agent-repl-test-terminal-run-that-shows-no-terminal-is-an-error ()
  "A toggle that never shows a terminal is logged, and nothing is sent or awaited."
  (agent-repl-test-terminal--with-harness (lambda () nil)
    ;; Act
    (agent-repl-terminal-run "ws-1" "ls")
    ;; Assert
    (should (agent-repl-test-terminal--logged :error "elisp.terminal.show-failed ws=ws-1 dir=\"/work/tree\""))
    (should (null agent-repl-test-terminal--sent))
    (should (zerop (hash-table-count agent-repl-terminal--runs)))))

(ert-deftest agent-repl-test-terminal-run-replaces-the-awaited-run ()
  "A second run in the same terminal is the one awaited, naming what it replaced."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "sleep 100")
      (let ((first (plist-get (gethash buffer agent-repl-terminal--runs) :token)))
        ;; Act
        (agent-repl-terminal-run "ws-1" "ls")
        ;; Assert
        (should-not (equal (plist-get (gethash buffer agent-repl-terminal--runs) :token) first))
        (should (agent-repl-test-terminal--logged :info "elisp.terminal.run ws=ws-1"))
        (should (string-suffix-p (format "replaced=%s" first)
                                 (cadr (agent-repl-test-terminal--logged :info "elisp.terminal.run"))))))))

(ert-deftest agent-repl-test-terminal-run-replacing-a-run-deletes-its-temp-files ()
  "The replaced run's output and status files are deleted, the new run's exist."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "sleep 100")
      (let ((first (gethash buffer agent-repl-terminal--runs)))
        ;; Act
        (agent-repl-terminal-run "ws-1" "ls")
        ;; Assert
        (should-not (file-exists-p (plist-get first :output)))
        (should-not (file-exists-p (plist-get first :status)))
        (should (file-exists-p (plist-get (gethash buffer agent-repl-terminal--runs) :output)))))))

(ert-deftest agent-repl-test-terminal-killing-the-terminal-forgets-its-run ()
  "A terminal buffer that dies takes its awaited run with it."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "ls")
      (let ((run (gethash buffer agent-repl-terminal--runs)))
        ;; Act
        (kill-buffer buffer)
        ;; Assert
        (should (zerop (hash-table-count agent-repl-terminal--runs)))
        (should-not (file-exists-p (plist-get run :output)))
        (should-not (file-exists-p (plist-get run :status)))))))

;;;; ---- The shell's finish report ----

(defun agent-repl-test-terminal--finish (buffer status output)
  "Run \"ls\" in BUFFER, write OUTPUT as its output, and finish it with STATUS.
Answers the finished run's plist."
  (agent-repl-terminal-run "ws-1" "ls")
  (let ((run (gethash buffer agent-repl-terminal--runs)))
    (write-region output nil (plist-get run :output) nil 'silent)
    (with-current-buffer buffer (agent-repl-terminal-finished (plist-get run :token) status))
    run))

(defun agent-repl-test-terminal--words (n)
  "A string of N words."
  (mapconcat (lambda (i) (format "w%d" i)) (number-sequence 1 n) " "))

(ert-deftest agent-repl-test-terminal-finished-closes-the-popup ()
  "The awaited run's finish closes the popup, forgets the run, and is logged."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "ls")
      (let ((token (plist-get (gethash buffer agent-repl-terminal--runs) :token)))
        ;; Act
        (with-current-buffer buffer (agent-repl-terminal-finished token "0"))
        ;; Assert
        (should (equal agent-repl-test-terminal--deleted (list buffer)))
        (should (zerop (hash-table-count agent-repl-terminal--runs)))
        (should (agent-repl-test-terminal--logged :info "elisp.terminal.finished ws=ws-1"))
        (should (null agent-repl-test-terminal--messages))))))

(ert-deftest agent-repl-test-terminal-finished-with-a-failure-still-closes-and-says-so ()
  "A non-zero status closes the popup too, and the minibuffer names the status."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "false")
      (let ((token (plist-get (gethash buffer agent-repl-terminal--runs) :token)))
        ;; Act
        (with-current-buffer buffer (agent-repl-terminal-finished token "1"))
        ;; Assert
        (should (equal agent-repl-test-terminal--deleted (list buffer)))
        (should (equal agent-repl-test-terminal--messages
                       '("agent-repl: the command exited with status 1; SPC o t shows its output")))))))

(ert-deftest agent-repl-test-terminal-finished-of-a-stale-run-closes-nothing ()
  "A finish naming a run no longer awaited leaves the popup and the run alone."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "ls")
      ;; Act
      (with-current-buffer buffer (agent-repl-terminal-finished "stale" "0"))
      ;; Assert
      (should (null agent-repl-test-terminal--deleted))
      (should (= (hash-table-count agent-repl-terminal--runs) 1))
      (should (agent-repl-test-terminal--logged :info "elisp.terminal.stale-finish")))))

(ert-deftest agent-repl-test-terminal-finished-sends-short-output-inline ()
  "Output under the word limit reaches the agent in the message, and the files go."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (let ((run (agent-repl-test-terminal--finish buffer "0" "hello world\n")))
        ;; Assert
        (should (= (length agent-repl-test-terminal--agent-sends) 1))
        (pcase-let ((`(,origin ,text ,ws) (car agent-repl-test-terminal--agent-sends)))
          (should (eq origin :command-terminal-output))
          (should (equal ws "ws-1"))
          (should (string-match-p "It succeeded (exit status 0)" text))
          (should (string-match-p "```sh\nls\n```" text))
          (should (string-match-p "Output:\n```\nhello world\n```" text)))
        (should-not (file-exists-p (plist-get run :output)))
        (should-not (file-exists-p (plist-get run :status)))))))

(ert-deftest agent-repl-test-terminal-finished-sends-short-failure-output-inline ()
  "A failed command's short output is sent inline, the failure stated."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (agent-repl-test-terminal--finish buffer "2" "boom")
      ;; Assert
      (let ((text (cadr (car agent-repl-test-terminal--agent-sends))))
        (should (string-match-p "It failed (exit status 2)" text))
        (should (string-match-p "Output:\n```\nboom\n```" text))))))

(ert-deftest agent-repl-test-terminal-finished-with-no-output-says-so ()
  "A command that printed nothing is reported as printing nothing."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (agent-repl-test-terminal--finish buffer "0" "")
      ;; Assert
      (should (string-match-p "It printed no output\\."
                              (cadr (car agent-repl-test-terminal--agent-sends)))))))

(ert-deftest agent-repl-test-terminal-finished-499-words-is-still-inline ()
  "The boundary below the limit: 499 words go inline."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (agent-repl-test-terminal--finish buffer "0" (agent-repl-test-terminal--words 499))
      ;; Assert
      (should (string-match-p "Output:" (cadr (car agent-repl-test-terminal--agent-sends)))))))

(ert-deftest agent-repl-test-terminal-finished-500-words-goes-by-path-and-says-skip-on-success ()
  "At the limit the output stays in its file; success tells the agent not to read it."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (let* ((run (agent-repl-test-terminal--finish buffer "0" (agent-repl-test-terminal--words 500)))
             (text (cadr (car agent-repl-test-terminal--agent-sends))))
        ;; Assert
        (should (file-exists-p (plist-get run :output)))
        (should-not (file-exists-p (plist-get run :status)))
        (should (string-match-p "500 words, [0-9]+ bytes" text))
        (should (string-match-p (regexp-quote (plist-get run :output)) text))
        (should (string-match-p "you do not need to read the file" text))
        (should-not (string-match-p "w250" text))
        (delete-file (plist-get run :output))))))

(ert-deftest agent-repl-test-terminal-finished-large-failure-tells-the-agent-to-read ()
  "A large failing output names its file and says to read it."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Act
      (let* ((run (agent-repl-test-terminal--finish buffer "1" (agent-repl-test-terminal--words 800)))
             (text (cadr (car agent-repl-test-terminal--agent-sends))))
        ;; Assert
        (should (string-match-p "It failed (exit status 1)" text))
        (should (string-match-p "read the file to find out why" text))
        (delete-file (plist-get run :output))))))

(ert-deftest agent-repl-test-terminal-report-strips-ansi-escapes ()
  "Color escapes in the captured output do not reach the agent."
  (let ((file (make-temp-file "agent-repl-test-terminal-")))
    (unwind-protect
        (progn
          (write-region "\e[31mred\e[0m text" nil file nil 'silent)
          ;; Act / Assert
          (should (string-match-p "Output:\n```\nred text\n```"
                                  (car (agent-repl-terminal-report "ls" "0" file)))))
      (delete-file file))))

(ert-deftest agent-repl-test-terminal-finished-with-an-unreadable-output-logs-and-sends-nothing ()
  "A missing output file is an error in the log, and nothing reaches the agent."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "ls")
      (let ((run (gethash buffer agent-repl-terminal--runs)))
        (delete-file (plist-get run :output))
        ;; Act
        (with-current-buffer buffer (agent-repl-terminal-finished (plist-get run :token) "0"))
        ;; Assert
        (should (null agent-repl-test-terminal--agent-sends))
        (should (agent-repl-test-terminal--logged :error "elisp.terminal.deliver-failed ws=ws-1"))))))

(ert-deftest agent-repl-test-terminal-finished-of-a-stale-run-sends-nothing ()
  "A stale finish does not reach the agent."
  (agent-repl-test-terminal--with-buffer buffer
    (agent-repl-test-terminal--with-harness (agent-repl-test-terminal--toggling buffer)
      ;; Arrange
      (agent-repl-terminal-run "ws-1" "ls")
      ;; Act
      (with-current-buffer buffer (agent-repl-terminal-finished "stale" "0"))
      ;; Assert
      (should (null agent-repl-test-terminal--agent-sends)))))

(ert-deftest agent-repl-test-terminal-install-lets-vterm-call-the-finish ()
  "Installing lists the finish function in `vterm-eval-cmds', once."
  (let ((vterm-eval-cmds '(("find-file" find-file))))
    ;; Act
    (agent-repl-terminal-install)
    (agent-repl-terminal-install)
    ;; Assert
    (should (equal vterm-eval-cmds
                   (list (list "agent-repl-terminal-finished" #'agent-repl-terminal-finished)
                         '("find-file" find-file))))))

;;; test-terminal.el ends here
