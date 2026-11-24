;;; test-completion-order.el --- ERT tests for completion-order.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: the one order both composer completions offer their
;; candidates in -- taken before, the most recently taken first; then
;; mentioned by the composer's own workspace, in the record's order; then the
;; rest in the table's order -- and that only a finished completion counts as
;; taken.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defmacro agent-repl-test-completion-order--fresh (&rest body)
  "Run BODY with nothing taken yet, behind the fake composer selections daemon."
  (declare (indent 0))
  `(agent-repl-test--with-composer-selections nil
     ,@body))

(defun agent-repl-test-completion-order--sort (kind mentioned candidates)
  "CANDIDATES of KIND in order, MENTIONED by the workspace, keyed by name."
  (agent-repl-completion-order-sort kind mentioned #'identity candidates))

(defun agent-repl-test-completion-order--take (kind name)
  "Take NAME of KIND in ws-1's composer, through its exit function."
  (funcall (agent-repl-completion-order-exit-function "ws-1" kind #'identity) name 'finished))

(ert-deftest agent-repl-test-completion-order-keeps-the-tables-order-for-the-rest ()
  "Nothing taken and nothing mentioned leaves the table's order."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command nil '("c" "a" "b"))
                   '("c" "a" "b")))))

(ert-deftest agent-repl-test-completion-order-puts-mentioned-first-in-the-records-order ()
  "Mentioned candidates come first, most recently mentioned first."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command '("b" "d") '("a" "d" "c" "b"))
                   '("b" "d" "a" "c")))))

(ert-deftest agent-repl-test-completion-order-ignores-a-mention-no-candidate-is ()
  "A mentioned name the table does not offer changes nothing."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command '("gone") '("a" "b"))
                   '("a" "b")))))

(ert-deftest agent-repl-test-completion-order-orders-by-the-daemons-record ()
  "Taken candidates come in the order of the daemon's record, newest first."
  (agent-repl-test--with-composer-selections '(:commands ("compact" "autocompact"))
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort
                    'command nil '("autocompact" "config" "compact"))
                   '("compact" "autocompact" "config")))))

(ert-deftest agent-repl-test-completion-order-puts-taken-before-mentioned ()
  "A taken candidate comes before every mentioned one."
  (agent-repl-test--with-composer-selections '(:commands ("c"))
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command '("b") '("a" "b" "c"))
                   '("c" "b" "a")))))

(ert-deftest agent-repl-test-completion-order-puts-the-latest-taken-first ()
  "Of two taken candidates, the one taken last comes first."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (agent-repl-test-completion-order--take 'command "a")
    (agent-repl-test-completion-order--take 'command "b")
    (agent-repl-test-completion-order--take 'command "a")
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command nil '("b" "c" "a"))
                   '("a" "b" "c")))))

(ert-deftest agent-repl-test-completion-order-keeps-kinds-apart ()
  "A workspace taken under one name orders no command of that name."
  (agent-repl-test--with-composer-selections '(:workspaces ("b"))
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command nil '("a" "b"))
                   '("a" "b")))))

(ert-deftest agent-repl-test-completion-order-orders-a-taken-channel-first ()
  "A Slack channel taken before is offered first, apart from the other kinds."
  (agent-repl-test--with-composer-selections '(:channels ("random") :commands ("general"))
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'channel nil '("general" "random"))
                   '("random" "general")))))

(ert-deftest agent-repl-test-completion-order-keys-candidates-by-name ()
  "KEY maps a candidate to the name it is taken and mentioned under."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-completion-order-sort
                    'command '("b") (lambda (c) (string-remove-prefix "/" c)) '("/a" "/b"))
                   '("/b" "/a")))))

(ert-deftest agent-repl-test-completion-order-leaves-its-input-whole ()
  "Sorting does not modify the candidates it was given."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (let ((candidates (list "a" "b" "c")))
      ;; Act
      (agent-repl-test-completion-order--sort 'command '("c") candidates)
      ;; Assert
      (should (equal candidates '("a" "b" "c"))))))

(ert-deftest agent-repl-test-completion-order-sorter-is-the-sort ()
  "The display-sort-function a table carries is the sort over its candidates."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (funcall (agent-repl-completion-order-sorter 'workspace '("b") #'identity)
                            '("a" "b"))
                   '("b" "a")))))

(ert-deftest agent-repl-test-completion-order-sorter-reads-a-push-since-it-was-built ()
  "A record pushed after the sorter was built orders its next sort."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (let ((sorter (agent-repl-completion-order-sorter 'command nil #'identity)))
      (setq agent-repl-host--composer-selections '(:commands ("b")))
      ;; Act / Assert
      (should (equal (funcall sorter '("a" "b")) '("b" "a"))))))

(ert-deftest agent-repl-test-completion-order-exit-reports-a-finished-completion ()
  "A `finished' completion is reported to the daemon, under the name KEY gives it."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (funcall (agent-repl-completion-order-exit-function
              "ws-1" 'command (lambda (c) (string-remove-prefix "/" c)))
             "/usage" 'finished)
    ;; Assert
    (should (equal agent-repl-test--composer-selection-reports
                   '((:selection (:arm :command :value (:name "usage"))))))))

(ert-deftest agent-repl-test-completion-order-exit-reports-a-sole-completion ()
  "A `sole' completion is reported, as its kind's arm."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (funcall (agent-repl-completion-order-exit-function "ws-1" 'workspace #'identity) "alpha" 'sole)
    ;; Assert
    (should (equal agent-repl-test--composer-selection-reports
                   '((:selection (:arm :workspace :value (:name "alpha"))))))))

(ert-deftest agent-repl-test-completion-order-exit-reports-a-channel ()
  "A taken Slack channel is reported as the channel arm."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (agent-repl-test-completion-order--take 'channel "eng")
    ;; Assert
    (should (equal agent-repl-test--composer-selection-reports
                   '((:selection (:arm :channel :value (:name "eng"))))))))

(ert-deftest agent-repl-test-completion-order-exit-ignores-an-exact-match ()
  "An `exact' match leaves the reader still choosing, so nothing is reported."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (funcall (agent-repl-completion-order-exit-function "ws-1" 'workspace #'identity) "alpha" 'exact)
    ;; Assert
    (should-not agent-repl-test--composer-selection-reports)))

(ert-deftest agent-repl-test-completion-order-refuses-an-unknown-kind ()
  "Reporting a selection of a kind no completion has is a defect, refused loudly."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should-error (agent-repl-completion-order-report-selected "ws-1" 'skill "x"))
    (should-not agent-repl-test--composer-selection-reports)))

(ert-deftest agent-repl-test-completion-order-logs-a-recorded-selection ()
  "A selection the daemon recorded is logged with its kind and name."
  (agent-repl-test-completion-order--fresh
    (let ((records nil))
      (cl-letf (((symbol-function 'agent-repl--emit-log) (agent-repl-test--record-into records)))
        ;; Act
        (agent-repl-test-completion-order--take 'command "compact"))
      ;; Assert
      (should (member "elisp.completion-order.recorded ws=ws-1 kind=command name=compact" records)))))

(ert-deftest agent-repl-test-completion-order-warns-a-selection-not-recorded ()
  "A selection the daemon could not record is a warning carrying its cause, and orders nothing."
  (agent-repl-test-completion-order--fresh
    (let ((records nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-report-composer-selection)
                 (lambda (_conn _request &rest keys)
                   (funcall (plist-get keys :on-response)
                            '(:arm :error :value (:cause (:arm :not-recorded :value (:detail "disk full")))))))
                ((symbol-function 'agent-repl--emit-warn) (agent-repl-test--record-into records)))
        ;; Act
        (agent-repl-test-completion-order--take 'command "compact"))
      ;; Assert
      (should (cl-find-if (lambda (r) (string-prefix-p "elisp.completion-order.not-recorded ws=ws-1 kind=command name=compact cause=" r))
                          records))
      (should (cl-find-if (lambda (r) (string-match-p "disk full" r)) records))
      (should-not agent-repl-host--composer-selections))))

(ert-deftest agent-repl-test-completion-order-warns-a-transport-failure ()
  "A report the transport failed to deliver is a warning carrying its detail."
  (agent-repl-test-completion-order--fresh
    (let ((records nil))
      (cl-letf (((symbol-function 'agent-repl-rpc-report-composer-selection)
                 (lambda (_conn _request &rest keys)
                   (funcall (plist-get keys :on-failure) '(:kind :refused))))
                ((symbol-function 'agent-repl--emit-warn) (agent-repl-test--record-into records)))
        ;; Act
        (agent-repl-test-completion-order--take 'command "compact"))
      ;; Assert
      (should (cl-find-if (lambda (r) (string-prefix-p "elisp.completion-order.transport-failure ws=ws-1 kind=command name=compact detail=" r))
                          records))
      (should-not agent-repl-host--composer-selections))))

(ert-deftest agent-repl-test-completion-order-warns-when-no-connection ()
  "With no connection nothing is reported, and the skip is a warning."
  (agent-repl-test-completion-order--fresh
    (let ((records nil))
      (cl-letf (((symbol-function 'agent-repl-host-conn-or-primary) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--emit-warn) (agent-repl-test--record-into records)))
        ;; Act
        (agent-repl-test-completion-order--take 'command "compact"))
      ;; Assert
      (should (member "elisp.completion-order.report-skipped ws=ws-1 kind=command name=compact reason=no-connection" records))
      (should-not agent-repl-test--composer-selection-reports))))

(provide 'test-completion-order)
;;; test-completion-order.el ends here
