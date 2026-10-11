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
  (should (equal (agent-repl--command-completion-annotation
                  agent-repl-test-command-completion--catalog "/merge-queue")
                 " <own|branch>  Land work on master")))

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
