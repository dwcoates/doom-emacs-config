;;; test-command-completion.el --- ERT tests for command-completion.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that a word starting with `/' in the composer completes
;; against exactly the commands the workspace's runtime listed, names and
;; aliases alike; that the composer asks the daemon again each time a slash
;; is typed, so a skill added mid-session is offered; and that a refusal or
;; a daemon that never answers is logged and leaves the catalog in hand as
;; it stood.  `ListSlashCommands' is stubbed and answers synchronously, so
;; every arm is exercised with no daemon anywhere.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fixtures ----

(defvar agent-repl-test-command-completion--requests nil
  "`ListSlashCommands' requests made, newest first.")

(defconst agent-repl-test-command-completion--catalog
  '((:name "merge-queue" :description "Land work on master" :argument-hint "<own|branch>" :aliases nil)
    (:name "usage" :description "Show plan usage" :argument-hint nil :aliases ("cost" "stats"))
    (:name "clear" :description nil :argument-hint nil :aliases nil))
  "A decoded catalog: one command with both facts, one with aliases, one bare.")

(defun agent-repl-test-command-completion--stated (commands)
  "A `ListSlashCommands' success answer stating COMMANDS."
  (list :arm :success
        :value (list :arm :stated :value (list :catalog (list :commands commands)))))

(defmacro agent-repl-test-command-completion--with (answer &rest body)
  "Run BODY in a composer owned by \"ws-1\" whose `ListSlashCommands' answers ANSWER.
ANSWER is `(:response PLIST)', `(:failure DETAIL)', or nil to answer nothing."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local agent-repl--owning-workspace "ws-1")
     (let ((agent-repl-test-command-completion--requests nil)
           (answer ,answer))
       (cl-letf (((symbol-function 'agent-repl-host-ref)
                  (lambda (_ws) '(:id "id-1" :dir "/ws")))
                 ((symbol-function 'agent-repl-host-conn-or-primary)
                  (lambda (_ws) 'conn))
                 ((symbol-function 'agent-repl-rpc-list-slash-commands)
                  (lambda (_conn request &rest keys)
                    (push request agent-repl-test-command-completion--requests)
                    (pcase (car answer)
                      (:response (funcall (plist-get keys :on-response) (cadr answer)))
                      (:failure (funcall (plist-get keys :on-failure) (cadr answer)))))))
         ,@body))))

(defun agent-repl-test-command-completion--type (text)
  "Insert TEXT at point as typed input."
  (insert text))

;;;; ---- The slash word at point ----

(ert-deftest agent-repl-test-command-completion-bounds-at-line-start ()
  "A slash word at the start of the line is found."
  (with-temp-buffer
    (insert "/merge")
    (should (equal (agent-repl--command-completion-bounds) '(1 . 7)))))

(ert-deftest agent-repl-test-command-completion-bounds-after-whitespace ()
  "A slash word after prose is found."
  (with-temp-buffer
    (insert "please run /mer")
    (should (equal (agent-repl--command-completion-bounds) '(12 . 16)))))

(ert-deftest agent-repl-test-command-completion-bounds-with-point-inside ()
  "Point inside a slash word finds the whole word."
  (with-temp-buffer
    (insert "/merge-queue now")
    (goto-char 4)
    (should (equal (agent-repl--command-completion-bounds) '(1 . 13)))))

(ert-deftest agent-repl-test-command-completion-bounds-ignore-a-mid-word-slash ()
  "A slash inside a word (a path) is not a slash word."
  (with-temp-buffer
    (insert "see lisp/input")
    (should-not (agent-repl--command-completion-bounds))))

(ert-deftest agent-repl-test-command-completion-bounds-ignore-plain-words ()
  "A word with no slash is not a slash word."
  (with-temp-buffer
    (insert "merge")
    (should-not (agent-repl--command-completion-bounds))))

(ert-deftest agent-repl-test-command-completion-bounds-ignore-whitespace ()
  "Point on whitespace is in no word."
  (with-temp-buffer
    (insert "/merge ")
    (should-not (agent-repl--command-completion-bounds))))

(ert-deftest agent-repl-test-command-completion-bounds-after-an-opening-paren ()
  "A slash word right after an opening paren starts at its slash."
  (with-temp-buffer
    (insert "see (/mer")
    (should (equal (agent-repl--command-completion-bounds) '(6 . 10)))))

(ert-deftest agent-repl-test-command-completion-bounds-after-opening-quotes ()
  "A slash word after several opening quotes and brackets starts at its slash."
  (with-temp-buffer
    (insert "[\"/mer")
    (should (equal (agent-repl--command-completion-bounds) '(3 . 7)))))

(ert-deftest agent-repl-test-command-completion-bounds-drop-trailing-punctuation ()
  "Trailing closing brackets and sentence punctuation are outside the word."
  (with-temp-buffer
    (insert "(/merge-queue).")
    (goto-char 5)
    (should (equal (agent-repl--command-completion-bounds) '(2 . 14)))))

(ert-deftest agent-repl-test-command-completion-bounds-keep-a-bare-slash-in-parens ()
  "A bare slash before a closing paren is a one-character slash word."
  (with-temp-buffer
    (insert "(/)")
    (goto-char 3)
    (should (equal (agent-repl--command-completion-bounds) '(2 . 3)))))

(ert-deftest agent-repl-test-command-completion-bounds-ignore-point-past-trailing-punctuation ()
  "Point after a slash word's trailing punctuation is not in the word."
  (with-temp-buffer
    (insert "(/mer)")
    (should-not (agent-repl--command-completion-bounds))))

(ert-deftest agent-repl-test-command-completion-bounds-ignore-point-on-opening-punctuation ()
  "Point before a slash word's opening punctuation is not in the word."
  (with-temp-buffer
    (insert "(/mer")
    (goto-char 1)
    (should-not (agent-repl--command-completion-bounds))))

(ert-deftest agent-repl-test-command-completion-bounds-ignore-a-slash-after-other-punctuation ()
  "A slash after punctuation that opens nothing (a relative path) is not a slash word."
  (with-temp-buffer
    (insert "../mer")
    (should-not (agent-repl--command-completion-bounds))))

;;;; ---- Candidates ----

(ert-deftest agent-repl-test-command-completion-candidates-name-and-alias ()
  "Every command and every alias is offered, slashed, in the runtime's order."
  (should (equal (agent-repl--command-completion-candidates
                  agent-repl-test-command-completion--catalog)
                 '("/merge-queue" "/usage" "/cost" "/stats" "/clear"))))

(ert-deftest agent-repl-test-command-completion-candidates-empty-catalog ()
  "No catalog offers nothing."
  (should-not (agent-repl--command-completion-candidates nil)))

(ert-deftest agent-repl-test-command-completion-annotation-hint-and-description ()
  "The annotation is the argument hint, then the description."
  ;; Arrange: wide enough that the cut (tested below) plays no part.
  (let ((agent-repl-command-completion-annotation-width 100))
    ;; Act / Assert
    (should (equal (agent-repl--command-completion-annotation
                    agent-repl-test-command-completion--catalog "/merge-queue")
                   " <own|branch>  Land work on master"))))

(ert-deftest agent-repl-test-command-completion-annotation-of-an-alias ()
  "An alias is annotated with the command it names."
  (should (equal (agent-repl--command-completion-annotation
                  agent-repl-test-command-completion--catalog "/cost")
                 " Show plan usage")))

(ert-deftest agent-repl-test-command-completion-annotation-when-nothing-stated ()
  "A command stating neither fact is annotated with nothing."
  (should (equal (agent-repl--command-completion-annotation
                  agent-repl-test-command-completion--catalog "/clear")
                 "")))

(ert-deftest agent-repl-test-command-completion-description ()
  "The doc signature is the runtime's description."
  (should (equal (agent-repl--command-completion-description
                  agent-repl-test-command-completion--catalog "/usage")
                 "Show plan usage")))

(ert-deftest agent-repl-test-command-completion-description-absent ()
  "A command stating no description has no doc signature."
  (should-not (agent-repl--command-completion-description
               agent-repl-test-command-completion--catalog "/clear")))

;;;; ---- The annotation's width ----

(ert-deftest agent-repl-test-command-completion-annotation-is-cut-to-the-width ()
  "An annotation longer than the width is cut there, ending in an ellipsis."
  ;; Arrange
  (let ((agent-repl-command-completion-annotation-width 10)
        (catalog '((:name "x" :description "abcdefghijklmnop" :argument-hint nil :aliases nil))))
    ;; Act / Assert
    (should (equal (agent-repl--command-completion-annotation catalog "/x")
                   " abcdefghi…"))))

(ert-deftest agent-repl-test-command-completion-annotation-at-the-width-is-whole ()
  "An annotation exactly the width long is not cut."
  ;; Arrange
  (let ((agent-repl-command-completion-annotation-width 10)
        (catalog '((:name "x" :description "abcdefghij" :argument-hint nil :aliases nil))))
    ;; Act / Assert
    (should (equal (agent-repl--command-completion-annotation catalog "/x")
                   " abcdefghij"))))

(ert-deftest agent-repl-test-command-completion-annotation-width-defaults-to-25 ()
  "The default width is 25 columns."
  (should (= (default-value 'agent-repl-command-completion-annotation-width) 25)))

;;;; ---- Marking known commands ----

(defun agent-repl-test-command-completion--marked ()
  "Return the strings the current buffer marks as known commands, in order."
  (mapcar (lambda (ov) (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
          (sort (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'agent-repl-command-completion-known))
                 (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defmacro agent-repl-test-command-completion--marking (&rest body)
  "Run BODY in a buffer holding the fixture catalog and marking on change."
  (declare (indent 0))
  `(with-temp-buffer
     (setq agent-repl--command-completion-catalog agent-repl-test-command-completion--catalog)
     (add-hook 'after-change-functions #'agent-repl--command-completion-on-change nil t)
     ,@body))

(ert-deftest agent-repl-test-command-completion-mark-a-typed-known-command ()
  "A typed slash word naming a known command is marked with the known face."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "please /merge-queue now")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/merge-queue")))
    (should (eq (get-char-property 9 'face) 'agent-repl-command-completion-known))))

(ert-deftest agent-repl-test-command-completion-mark-an-alias ()
  "A slash word naming an alias is marked."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "/cost")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/cost")))))

(ert-deftest agent-repl-test-command-completion-mark-skips-an-unknown-word ()
  "A slash word naming no command is not marked."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "/usr/local /nope")
    ;; Assert
    (should-not (agent-repl-test-command-completion--marked))))

(ert-deftest agent-repl-test-command-completion-mark-skips-a-mid-word-slash ()
  "A slash inside a word is not a slash word, even naming a command."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "see a/usage")
    ;; Assert
    (should-not (agent-repl-test-command-completion--marked))))

(ert-deftest agent-repl-test-command-completion-mark-adjacent-words ()
  "Two known slash words separated by one space are both marked."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "/usage /clear")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/usage" "/clear")))))

(ert-deftest agent-repl-test-command-completion-mark-at-a-line-start ()
  "A known slash word starting a later line is marked."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "first line\n/clear")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/clear")))))

(ert-deftest agent-repl-test-command-completion-mark-inside-parens ()
  "A known slash word wrapped in parens is marked without the parens."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "hello (/merge-queue) here")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/merge-queue")))))

(ert-deftest agent-repl-test-command-completion-mark-before-a-period ()
  "A known slash word ending a sentence is marked without the period."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "hello /merge-queue.")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/merge-queue")))))

(ert-deftest agent-repl-test-command-completion-mark-inside-quotes ()
  "A known slash word in quotes followed by a comma is marked without them."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "run \"/clear\", then")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/clear")))))

(ert-deftest agent-repl-test-command-completion-mark-when-closing-punctuation-is-typed ()
  "Typing a closing paren after a known slash word keeps it marked."
  (agent-repl-test-command-completion--marking
    ;; Arrange
    (insert "(/usage")
    ;; Act
    (insert ")")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/usage")))))

(ert-deftest agent-repl-test-command-completion-mark-skips-mid-word-closing-punctuation ()
  "A closing paren inside the run, not trailing it, keeps the word unknown."
  (agent-repl-test-command-completion--marking
    ;; Act
    (insert "/clear)x")
    ;; Assert
    (should-not (agent-repl-test-command-completion--marked))))

(ert-deftest agent-repl-test-command-completion-mark-is-removed-when-the-word-changes ()
  "Typing on past a known command unmarks the slash word."
  (agent-repl-test-command-completion--marking
    ;; Arrange
    (insert "/usage")
    ;; Act
    (insert "x")
    ;; Assert
    (should-not (agent-repl-test-command-completion--marked))))

(ert-deftest agent-repl-test-command-completion-mark-is-not-duplicated ()
  "Editing elsewhere on a marked line leaves exactly one mark."
  (agent-repl-test-command-completion--marking
    ;; Arrange
    (insert "/usage ")
    ;; Act
    (insert "more")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/usage")))))

(ert-deftest agent-repl-test-command-completion-mark-without-a-catalog ()
  "With no catalog in hand nothing is marked."
  (with-temp-buffer
    (add-hook 'after-change-functions #'agent-repl--command-completion-on-change nil t)
    ;; Act
    (insert "/usage")
    ;; Assert
    (should-not (agent-repl-test-command-completion--marked))))

(ert-deftest agent-repl-test-command-completion-stated-catalog-marks-the-composer ()
  "Installing a stated catalog marks the known words already in the composer."
  (agent-repl-test-command-completion--with
      (list :response (agent-repl-test-command-completion--stated
                       agent-repl-test-command-completion--catalog))
    ;; Arrange
    (insert "run /merge-queue")
    ;; Act
    (agent-repl-command-completion-refresh "ws-1")
    ;; Assert
    (should (equal (agent-repl-test-command-completion--marked) '("/merge-queue")))))

(ert-deftest agent-repl-test-command-completion-unstated-unmarks-the-composer ()
  "An unstated answer removes every mark from the composer."
  (agent-repl-test-command-completion--with
      '(:response (:arm :success :value (:arm :unstated :value nil)))
    ;; Arrange
    (insert "run /merge-queue")
    (agent-repl--command-completion-install
     (current-buffer) agent-repl-test-command-completion--catalog)
    ;; Act
    (agent-repl-command-completion-refresh "ws-1")
    ;; Assert
    (should-not (agent-repl-test-command-completion--marked))))

;;;; ---- completion-at-point ----

(ert-deftest agent-repl-test-command-completion-capf-completes-from-the-catalog ()
  "A slash word completes over the stated catalog, non-exclusively."
  (agent-repl-test-command-completion--with
      (list :response (agent-repl-test-command-completion--stated
                       agent-repl-test-command-completion--catalog))
    ;; Arrange
    (agent-repl-test-command-completion--type "/")
    (agent-repl-command-completion-at-point)
    (agent-repl-test-command-completion--type "me")
    ;; Act
    (let ((capf (agent-repl-command-completion-at-point)))
      ;; Assert
      (should (equal (list (nth 0 capf) (nth 1 capf)) '(1 4)))
      (should (equal (all-completions "/me" (nth 2 capf)) '("/merge-queue")))
      (should (eq (plist-get (nthcdr 3 capf) :exclusive) 'no)))))

(ert-deftest agent-repl-test-command-completion-capf-completes-inside-parens ()
  "A slash word inside parens completes over its slash and name only."
  (agent-repl-test-command-completion--with
      (list :response (agent-repl-test-command-completion--stated
                       agent-repl-test-command-completion--catalog))
    ;; Arrange
    (agent-repl-test-command-completion--type "(/")
    (agent-repl-command-completion-at-point)
    (agent-repl-test-command-completion--type "me)")
    (backward-char)
    ;; Act
    (let ((capf (agent-repl-command-completion-at-point)))
      ;; Assert
      (should (equal (list (nth 0 capf) (nth 1 capf)) '(2 5)))
      (should (equal (all-completions "/me" (nth 2 capf)) '("/merge-queue"))))))

(ert-deftest agent-repl-test-command-completion-capf-is-nil-outside-a-slash-word ()
  "Outside a slash word the capf declines and asks nothing."
  (agent-repl-test-command-completion--with nil
    ;; Arrange
    (agent-repl-test-command-completion--type "hello")
    ;; Act / Assert
    (should-not (agent-repl-command-completion-at-point))
    (should-not agent-repl-test-command-completion--requests)))

(ert-deftest agent-repl-test-command-completion-capf-asks-on-a-typed-slash ()
  "Typing a slash asks the daemon, with the workspace's ref."
  (agent-repl-test-command-completion--with nil
    ;; Act
    (agent-repl-test-command-completion--type "/")
    (agent-repl-command-completion-at-point)
    ;; Assert
    (should (equal agent-repl-test-command-completion--requests
                   '((:workspace (:id "id-1" :dir "/ws")))))))

(ert-deftest agent-repl-test-command-completion-capf-asks-once-per-typed-slash ()
  "Repeated completion at the same typed slash asks once."
  (agent-repl-test-command-completion--with nil
    ;; Act
    (agent-repl-test-command-completion--type "/")
    (agent-repl-command-completion-at-point)
    (agent-repl-command-completion-at-point)
    ;; Assert
    (should (= (length agent-repl-test-command-completion--requests) 1))))

(ert-deftest agent-repl-test-command-completion-capf-does-not-ask-while-the-word-grows ()
  "Typing on past the slash asks nothing more."
  (agent-repl-test-command-completion--with nil
    ;; Act
    (agent-repl-test-command-completion--type "/")
    (agent-repl-command-completion-at-point)
    (agent-repl-test-command-completion--type "merge")
    (agent-repl-command-completion-at-point)
    ;; Assert
    (should (= (length agent-repl-test-command-completion--requests) 1))))

(ert-deftest agent-repl-test-command-completion-capf-asks-again-for-a-retyped-slash ()
  "A slash typed again at the same position, as after a send, asks again."
  (agent-repl-test-command-completion--with nil
    ;; Arrange
    (agent-repl-test-command-completion--type "/")
    (agent-repl-command-completion-at-point)
    (erase-buffer)
    ;; Act
    (agent-repl-test-command-completion--type "/")
    (agent-repl-command-completion-at-point)
    ;; Assert
    (should (= (length agent-repl-test-command-completion--requests) 2))))

(ert-deftest agent-repl-test-command-completion-capf-asks-for-a-pasted-word-when-never-asked ()
  "A composer that never asked asks for a slash word pasted whole."
  (agent-repl-test-command-completion--with nil
    ;; Arrange
    (agent-repl-test-command-completion--type "/merge")
    ;; Act
    (agent-repl-command-completion-at-point)
    ;; Assert
    (should (= (length agent-repl-test-command-completion--requests) 1))))

;;;; ---- Asking the daemon ----

(ert-deftest agent-repl-test-command-completion-stated-catalog-is-installed ()
  "A stated catalog becomes the composer's catalog."
  (agent-repl-test-command-completion--with
      (list :response (agent-repl-test-command-completion--stated
                       agent-repl-test-command-completion--catalog))
    ;; Act
    (agent-repl-command-completion-refresh "ws-1")
    ;; Assert
    (should (equal agent-repl--command-completion-catalog
                   agent-repl-test-command-completion--catalog))))

(ert-deftest agent-repl-test-command-completion-unstated-clears-the-catalog ()
  "An unstated answer leaves the composer offering nothing."
  (agent-repl-test-command-completion--with
      '(:response (:arm :success :value (:arm :unstated :value nil)))
    ;; Arrange
    (setq agent-repl--command-completion-catalog agent-repl-test-command-completion--catalog)
    ;; Act
    (agent-repl-command-completion-refresh "ws-1")
    ;; Assert
    (should-not agent-repl--command-completion-catalog)))

(ert-deftest agent-repl-test-command-completion-refusal-keeps-the-catalog ()
  "A refusal keeps the catalog in hand."
  (agent-repl-test-command-completion--with
      '(:response (:arm :error :value (:cause (:arm :unknown-workspace :value nil))))
    ;; Arrange
    (setq agent-repl--command-completion-catalog agent-repl-test-command-completion--catalog)
    (cl-letf (((symbol-function 'agent-repl-host-route-handover) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--warn) #'ignore))
      ;; Act
      (agent-repl-command-completion-refresh "ws-1"))
    ;; Assert
    (should (equal agent-repl--command-completion-catalog
                   agent-repl-test-command-completion--catalog))))

(ert-deftest agent-repl-test-command-completion-refusal-is-logged ()
  "A refusal that is not a handover is logged at WARN with its cause."
  (agent-repl-test-command-completion--with
      '(:response (:arm :error :value (:cause (:arm :unknown-workspace :value nil))))
    ;; Arrange
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl-host-route-handover) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "command-completion.refused.*unknown-workspace" m))
                       warned)))))

(ert-deftest agent-repl-test-command-completion-handover-refusal-is-routed ()
  "A handover arm is handed to host.el and not logged as a refusal."
  (agent-repl-test-command-completion--with
      '(:response (:arm :error :value (:cause (:arm :transferring-away :value (:address "a")))))
    ;; Arrange
    (let ((routed nil) (warned nil))
      (cl-letf (((symbol-function 'agent-repl-host-route-handover)
                 (lambda (ws cause slug) (push (list ws cause slug) routed) t))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should (equal routed '(("ws-1" (:arm :transferring-away :value (:address "a"))
                               "elisp.command-completion.handover-refusal"))))
      (should-not warned))))

(ert-deftest agent-repl-test-command-completion-transport-failure-is-logged ()
  "A daemon that never answered is logged at WARN and the catalog stands."
  (agent-repl-test-command-completion--with '(:failure (:kind :transport :message "down"))
    ;; Arrange
    (setq agent-repl--command-completion-catalog agent-repl-test-command-completion--catalog)
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "command-completion.transport-failure.*down" m))
                       warned))
      (should (equal agent-repl--command-completion-catalog
                     agent-repl-test-command-completion--catalog)))))

(ert-deftest agent-repl-test-command-completion-unknown-catalog-arm-is-an-error ()
  "A success arm this file does not know is logged at ERROR and installs nothing."
  (agent-repl-test-command-completion--with
      '(:response (:arm :success :value (:arm :mystery :value nil)))
    ;; Arrange
    (setq agent-repl--command-completion-catalog agent-repl-test-command-completion--catalog)
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "unknown-catalog-arm.*:mystery" m)) errors))
      (should (equal agent-repl--command-completion-catalog
                     agent-repl-test-command-completion--catalog)))))

(ert-deftest agent-repl-test-command-completion-unknown-response-arm-is-an-error ()
  "A response arm this file does not know is logged at ERROR."
  (agent-repl-test-command-completion--with '(:response (:arm :accepted :value nil))
    ;; Arrange
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "unknown-response-arm.*:accepted" m)) errors)))))

(ert-deftest agent-repl-test-command-completion-answer-for-a-dead-composer-is-dropped ()
  "An answer arriving after the composer died touches no other buffer."
  (let ((dead (generate-new-buffer " *dead-composer*")))
    (kill-buffer dead)
    ;; Act / Assert: nothing signals, and nothing is installed here.
    (with-temp-buffer
      (agent-repl--command-completion-answered
       "ws-1" dead (agent-repl-test-command-completion--stated
                    agent-repl-test-command-completion--catalog))
      (should-not agent-repl--command-completion-catalog))))

(ert-deftest agent-repl-test-command-completion-no-ref-asks-nothing ()
  "A workspace the daemon has not registered asks nothing and logs why."
  (agent-repl-test-command-completion--with nil
    ;; Arrange
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should-not agent-repl-test-command-completion--requests)
      (should (cl-some (lambda (m) (string-match-p "refresh-skipped.*reason=no-ref" m)) logged)))))

(ert-deftest agent-repl-test-command-completion-no-connection-asks-nothing ()
  "No daemon connection asks nothing and logs why."
  (agent-repl-test-command-completion--with nil
    ;; Arrange
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl-host-conn-or-primary) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-command-completion-refresh "ws-1"))
      ;; Assert
      (should-not agent-repl-test-command-completion--requests)
      (should (cl-some (lambda (m) (string-match-p "refresh-skipped.*reason=no-connection" m))
                       logged)))))

(provide 'test-command-completion)
;;; test-command-completion.el ends here
