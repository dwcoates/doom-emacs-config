;;; test-rules-wire.el --- Rule tests: the daemon link and the wire -*- lexical-binding: t; -*-

;;; Commentary:

;; Rules over the sources that talk to the daemon: the link, the host stream,
;; the unary transport, the codecs and the verbs each route one concern through
;; one helper.
;;
;; A rule test reads the module's elisp sources as data and holds them to a
;; structural rule.  Rule tests are no one module's tests, so they live by
;; rule area in `test-rules-*.el', the `ert-rules' suite every branch runs
;; before its merge is admitted (lisp/AGENTS.md, "Rule tests").

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- connect.el ----

(defconst agent-repl-test-connect--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The `lisp/' directory these tests were loaded from.")

(ert-deftest agent-repl-test-connect-connection-at-p-is-the-one-spelling ()
  "No source compares a connection\='s address to ADDRESS by hand."
  (let ((dir agent-repl-test-connect--lisp-dir))
    (dolist (file (directory-files dir t "\\`[a-z-]+\\.el\\'"))
      (unless (or (string-prefix-p "test-" (file-name-nondirectory file))
                  (equal (file-name-nondirectory file) "connect.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (should-not (re-search-forward
                       "(equal address (agent-repl-connect-connection-address" nil t)))))))

;;;; ---- daemon-link.el ----

(defconst agent-repl-test-link--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The lisp/ directory, captured at load: a test body runs with no `load-file-name'.")

(defun agent-repl-test-link--defun-form (file name)
  "Return the top-level `defun' of NAME read from FILE, a source under lisp/."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file agent-repl-test-link--lisp-dir))
    (goto-char (point-min))
    (let (found)
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'defun) (eq (nth 1 form) name))
                (setq found form))))
        (end-of-file nil))
      found)))

(ert-deftest agent-repl-test-link-every-follow-of-the-live-daemon-asks-departing-p ()
  "host.el and roster.el ask the one departing question, never a hand-rolled copy."
  ;; Arrange
  (let ((host (agent-repl-test-link--defun-form "host.el" 'agent-repl-host--follow-live-daemon))
        (roster (agent-repl-test-link--defun-form "roster.el" 'agent-repl-roster--follow-live-daemon)))
    ;; Act
    (let ((asks (list (memq 'agent-repl-link-departing-p (flatten-tree host))
                      (memq 'agent-repl-link-departing-p (flatten-tree roster)))))
      ;; Assert
      (should (and host roster (car asks) (cadr asks))))))

(ert-deftest agent-repl-test-link-every-daemon-wide-call-asks-newest ()
  "The latency, focus and recency reach reports ask the one newest question, never the live daemon.
The focus and recency reach reports ask it through their shared dispatch,
`agent-repl-serial-send-to-newest', which must ask it itself."
  ;; Arrange
  (let ((latency (agent-repl-test-link--defun-form "latency-report.el" 'agent-repl-latency-report-flush))
        (focus (agent-repl-test-link--defun-form "notifications.el" 'agent-repl--focus-report))
        (reach (agent-repl-test-link--defun-form "recency-reach.el" 'agent-repl--recency-reach-report))
        (dispatch (agent-repl-test-link--defun-form "core.el" 'agent-repl-serial-send-to-newest)))
    ;; Act
    (let ((asks-newest (lambda (tree)
                         (or (memq 'agent-repl-link-newest tree)
                             (memq 'agent-repl-serial-send-to-newest tree)))))
      ;; Assert
      (should (and latency focus reach dispatch))
      (should (memq 'agent-repl-link-newest (flatten-tree dispatch)))
      (dolist (tree (mapcar #'flatten-tree (list latency focus reach dispatch)))
        (should (funcall asks-newest tree))
        (should-not (memq 'agent-repl-link-live tree))))))

(ert-deftest agent-repl-test-link-up-hooks-run-through-one-helper ()
  "Every way the link comes up runs the up hooks through
`agent-repl-link--stood-up', so no link-up skips a step of the walk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "daemon-link.el" agent-repl-test-link--lisp-dir))
    (let ((runs 0) (calls 0))
      (while (search-forward "(agent-repl-link--run-hook 'agent-repl-link-up-functions" nil t)
        (setq runs (1+ runs)))
      (goto-char (point-min))
      (while (search-forward "(agent-repl-link--stood-up" nil t)
        (setq calls (1+ calls)))
      ;; The helper's own run, and the two ways up: acceptance and a
      ;; successor dialed while the link was down.
      (should (= runs 1))
      (should (= calls 2)))))

(ert-deftest agent-repl-test-link-handovers-complete-through-one-helper ()
  "Both orders complete the handover through `agent-repl-link--complete-handover'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "daemon-link.el" agent-repl-test-link--lisp-dir))
    (let ((promotes 0) (calls 0))
      (while (search-forward "(agent-repl-link--promote-successor)" nil t)
        (setq promotes (1+ promotes)))
      (goto-char (point-min))
      (while (search-forward "(agent-repl-link--complete-handover " nil t)
        (setq calls (1+ calls)))
      (should (= promotes 1))
      (should (= calls 2)))))

(ert-deftest agent-repl-test-link-primary-ends-go-down-through-one-helper ()
  "Every primary end with no successor to promote takes the link down
through `agent-repl-link--primary-ended-down', so the planned and the lost
levels are decided in one place."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "daemon-link.el" agent-repl-test-link--lisp-dir))
    (let ((count 0))
      (while (search-forward "(agent-repl-link--primary-down " nil t)
        (setq count (1+ count)))
      ;; The helper's two calls, planned and lost, and no others.
      (should (= count 2)))))

(ert-deftest agent-repl-test-link-successor-waits-are-spelled-once ()
  "No other source hangs its own self-removing handover hook or re-checks a
standing successor's address: both go through daemon-link's helpers."
  (let ((dir agent-repl-test-link--lisp-dir))
    (dolist (file (directory-files dir t "\\`[a-z-]+\\.el\\'"))
      (unless (or (string-prefix-p "test-" (file-name-nondirectory file))
                  (equal (file-name-nondirectory file) "daemon-link.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (should-not (search-forward "(remove-hook 'agent-repl-link-handover-functions" nil t)))))))

(ert-deftest agent-repl-test-link-attach-successor-drops-through-the-one-helper ()
  "Every successor attach-successor drops goes through the one helper, never a hand-rolled clear."
  (let ((body (format "%S" (agent-repl-test-link--defun-form
                            "daemon-link.el" 'agent-repl-link--attach-successor))))
    (should (string-search "agent-repl-link--drop-standing-successor" body))
    (should-not (string-search "(setq agent-repl-link--pending-successor" body))
    (should-not (string-search "(setq agent-repl-link--successor" body))))

;;;; ---- held-edit.el ----

(defconst agent-repl-test-he--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory holding the module's elisp sources and suites.")

(ert-deftest agent-repl-held-edit-the-retired-release-label-is-labelled-by-no-emacs-source ()
  "No Emacs source holds a string literal that is exactly \"Release\".
The held prompt's Release action is retired; the suites are excluded,
because they are not labels."
  (let* ((sources (seq-remove (lambda (file)
                               (string-prefix-p "test-" (file-name-nondirectory file)))
                             (directory-files agent-repl-test-he--lisp-dir t "\\.el\\'")))
         (holding (seq-filter (lambda (file)
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (goto-char (point-min))
                                  (re-search-forward "\\([\"'`]\\)Release\\1" nil t)))
                              sources)))
    (should (equal (list (> (length sources) 0) holding) (list t nil)))))

;;;; ---- host.el ----

(ert-deftest agent-repl-test-host-conn-or-primary-is-the-only-spelling ()
  "No source hand-rolls the workspace-conn-else-primary fallback.
Every per-workspace call resolves its connection through
`agent-repl-host-conn-or-primary', so a site that spells the fallback
itself is a second copy free to drift from it."
  (let ((pattern "(or (\\(?:and ws (\\)?agent-repl-host-conn ws))?[[:space:]\n]*(agent-repl-link-primary)")
        (offenders nil))
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (unless (and (string= (file-name-nondirectory file) "host.el")
                         (save-excursion
                           (beginning-of-defun)
                           (looking-at "(defun agent-repl-host-conn-or-primary ")))
              (push (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos))
                    offenders))))))
    (should-not offenders)))

;;;; ---- input.el ----

(ert-deftest agent-repl-input-no-arm-hand-rolls-the-refusal-detail ()
  "Every refusal arm reads its `detail' through the one helper."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "input.el" (file-name-directory
                                                 (symbol-file (quote agent-repl--input-refusal-detail) (quote defun)))))
                  (buffer-string))))
    (should-not (string-match-p (regexp-quote "(plist-get (plist-get reason :value) :detail)")
                                (replace-regexp-in-string
                                 (regexp-quote "(or (plist-get (plist-get reason :value) :detail) \"\")")
                                 "" source)))))

(ert-deftest agent-repl-test-input-every-composer-verb-goes-through-the-one-call ()
  "No send site in input.el hand-rolls the verb call's handover routing."
  (let ((source (with-temp-buffer
                  (insert-file-contents (expand-file-name "input.el" agent-repl-test--module-dir))
                  (buffer-string)))
        (count 0) (start 0))
    (while (string-match "(agent-repl-host-route-handover" source start)
      (setq count (1+ count) start (match-end 0)))
    (should (= count 1))))

;;;; ---- prompt-queue.el ----

(defconst agent-repl-test-pq--retired-in-memory-names
  '(;; The retired outage queue and its release edges.
    "agent-repl-prompt-queue-offer"
    "agent-repl-prompt-queue-drain"
    "agent-repl--prompt-queue-draining"
    "agent-repl--prompt-queue-on-link-up"
    "agent-repl--prompt-queue-on-link-promote"
    "agent-repl--prompt-queue-on-reattached"
    ":outage"
    ;; The retired in-memory deferral queue, its finish-edge release and its
    ;; liveness gate.
    "agent-repl--prompt-queue-enqueue"
    "agent-repl--prompt-queue-on-finish"
    "agent-repl-prompt-queue-pending"
    "agent-repl-prompt-queue-deliverable-p"
    "agent-repl--prompt-queue-blocked-gates"
    ":deferred-prompts")
  "Names of the retired in-memory prompt holding and its release edges.")

(ert-deftest agent-repl-pq-no-production-source-names-a-retired-in-memory-queue ()
  "No production source holds a prompt in Emacs memory.
Owner ruling, 2026-09-28: held prompts survive outages and restarts, so a
prompt the daemon did not take goes to the durable held-prompt ingress
\(held-ingress.el), and a deferral is held by the daemon itself.  A source
naming a retired queue or one of its release edges has reopened a second
path."
  ;; Arrange
  (let* ((dir (file-name-directory (symbol-file 'agent-repl-queue-deferred-prompt)))
         (sources (cl-remove-if (lambda (f) (string-prefix-p "test-" (file-name-nondirectory f)))
                                (directory-files dir t "\\.el\\'")))
         (offenders nil))
    ;; Act
    (dolist (file sources)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (name agent-repl-test-pq--retired-in-memory-names)
          (goto-char (point-min))
          (when (search-forward name nil t)
            (push (format "%s: %s" (file-name-nondirectory file) name) offenders)))))
    ;; Assert
    (should (> (length sources) 10))
    (should-not offenders)))

;;;; ---- rpc.el ----

(ert-deftest agent-repl-test-rpc-only-the-gates-send-reaches-the-unary-transport ()
  "No elisp source but rpc.el\\='s transport half calls the async unary transport.
A call site that reached `agent-repl-connect-unary' itself would skip the
call gate, and its call about a workspace mid-adopt would reach a daemon
that has not adopted it."
  ;; Arrange
  (let ((callers nil))
    ;; Act
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`[^.].*\\.el\\'"))
      (unless (or (string-prefix-p "test-" (file-name-nondirectory file))
                  (member (file-name-nondirectory file) '("connect.el")))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "(agent-repl-connect-unary[ \n]" nil t)
            (push (cons (file-name-nondirectory file)
                        (save-excursion
                          (beginning-of-defun)
                          (buffer-substring-no-properties (point) (line-end-position))))
                  callers)))))
    ;; Assert
    (should (equal callers
                   '(("rpc.el" . "(defun agent-repl-rpc--send-unary (conn method json decoder log-scope"))))))

;;;; ---- verbs.el ----

(defconst agent-repl-test-verbs--source
  (expand-file-name "verbs.el" (file-name-directory (or load-file-name buffer-file-name)))
  "verbs.el beside this suite, for the tests that read its source.")

(ert-deftest agent-repl-verbs-roster-defaulting-has-one-home ()
  "Every roster reader in verbs.el defaults through `agent-repl-verbs--roster-or-view'.
A reader that hand-rolls the defaulting reads the view a second time and fails here."
  ;; Arrange.
  (with-temp-buffer
    (insert-file-contents agent-repl-test-verbs--source)
    ;; Act.
    (let ((reads (how-many (regexp-quote "(boundp 'agent-repl-roster-view)")
                           (point-min) (point-max))))
      ;; Assert.
      (should (= reads 1)))))

(ert-deftest agent-repl-verbs-no-verb-forgets-its-op-by-hand ()
  "Every verb that registers an op sends through `agent-repl-verbs--send-op'.
A verb that forgets its op by hand is the one that misses a path."
  (dolist (expected '(("verbs.el" . 1) ("conversations.el" . 0)))
    (let ((source (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name (car expected)
                                       (file-name-directory
                                        (symbol-file 'agent-repl-verbs--send-op 'defun))))
                    (buffer-string)))
          (count 0) (start 0))
      (while (setq start (string-search "(agent-repl-mutation-progress-forget op-id)" source start))
        (setq count (1+ count) start (1+ start)))
      ;; The one occurrence in verbs.el is `agent-repl-verbs--send-op' itself.
      (should (equal (cons (car expected) count) expected)))))

(ert-deftest agent-repl-test-verbs-reads-rows-through-the-roster-readers ()
  "verbs.el spells no row reader of its own: every read of a row's ref,
name or closed flag is roster.el's, so the two files cannot disagree."
  ;; Arrange
  (let ((source (with-temp-buffer
                  (insert-file-contents agent-repl-test-verbs--source)
                  (buffer-string))))
    ;; Act / Assert
    (should-not (string-match-p "(defun agent-repl-verbs--row-\\(ref\\|name\\|closed-p\\) " source))))

(ert-deftest agent-repl-verbs-every-create-progress-is-registered-by-the-one-helper ()
  "Only the shared registration renders a create's stages."
  (let ((count 0))
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
       (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (search-forward "(agent-repl-verbs--create-stage-message stage)" nil t)
          (setq count (1+ count))))))
    (should (= count 1))))

;;;; ---- wire-common.el ----

(ert-deftest agent-repl-test-wire-common-every-required-string-is-checked-by-the-helper ()
  "No codec file hand-rolls the empty-string breach: only the helpers raise it."
  (dolist (file '("wire-host.el" "wire-common.el"))
    (let ((source (with-temp-buffer
                    (insert-file-contents (expand-file-name file agent-repl-test--module-dir))
                    (buffer-string))))
      (should (equal (list file (cl-count-if (lambda (_) t)
                                             (let (hits (start 0))
                                               (while (string-match "\"required string is empty\"" source start)
                                                 (push (match-beginning 0) hits)
                                                 (setq start (match-end 0)))
                                               hits)))
                     (list file (if (equal file "wire-common.el") 1 0)))))))

(ert-deftest agent-repl-test-wire-common-every-required-field-is-checked-by-the-helper ()
  "No encoder hand-rolls the absent-field breach: the helpers raise it.
The one other site, AnswerEditorEvaluation's, refuses a nil value as well
as a missing key, so it is not the helper's check."
  (dolist (file '(("wire-common.el" . 2) ("wire-host.el" . 1) ("wire-roster.el" . 0)
                  ("wire-chat.el" . 0) ("wire-verbs.el" . 0)))
    (let ((source (with-temp-buffer
                    (insert-file-contents (expand-file-name (car file) agent-repl-test--module-dir))
                    (buffer-string)))
          (hits 0) (start 0))
      (while (string-match "\"required message field is absent\"" source start)
        (setq hits (1+ hits) start (match-end 0)))
      (should (equal (cons (car file) hits) file)))))

;;;; ---- wire-verbs.el ----

(defun agent-repl-test-wire-verbs--occurrences (needle haystack)
  "Count the non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let ((count 0) (start 0))
    (while (setq start (string-search needle haystack start))
      (setq count (1+ count) start (+ start (length needle))))
    count))

(ert-deftest agent-repl-test-wire-verbs-no-codec-hand-rolls-the-op-id-or-accepted-result ()
  "Every op id and every accepted result goes through the shared helpers.
A hand-rolled site is exactly the one that drifts: it spells `opId' or the
three arms its own way."
  (let* ((file (file-name-with-extension
                (symbol-file 'agent-repl-wire-verbs--append-op-id 'defun) "el"))
         (source (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (should (= (agent-repl-test-wire-verbs--occurrences "(cons 'opId" source) 1))
    (should (= (agent-repl-test-wire-verbs--occurrences "'(success error accepted)" source) 1))))

(ert-deftest agent-repl-test-wire-verbs-no-codec-hand-rolls-a-lone-detail ()
  "Every message carrying only `detail' decodes through the one helper.
A hand-rolled site is a second spelling of the same shape, which drifts."
  (let* ((file (file-name-with-extension
                (symbol-file 'agent-repl-wire-verbs--decode-detail 'defun) "el"))
         (source (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (should (= (agent-repl-test-wire-verbs--occurrences "'(detail))" source) 1))))

(ert-deftest agent-repl-test-wire-verbs-empty-arms-share-one-decoder ()
  "Every empty arm in the verbs codec is decoded through
`agent-repl-wire-verbs--empty-arm', so no site hand-rolls a lambda around
`agent-repl-wire-verbs--decode-empty' that could drift from it."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (concat (file-name-sans-extension
                            (symbol-file 'agent-repl-wire-verbs--empty-arm 'defun))
                           ".el"))
                  (buffer-string))))
    (should-not (string-match-p
                 "(lambda (\\([a-z]+\\))[ \t\n]+(agent-repl-wire-verbs--decode-empty[ \t\n]+\"[A-Za-z]+\"[ \t\n]+\\1))"
                 source))))

(ert-deftest agent-repl-test-wire-verbs-every-editor-duration-shares-one-check ()
  "No encoder in wire-verbs.el checks a duration by hand beside the shared helper."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "wire-verbs.el" agent-repl-test--module-dir))
    (should (= (how-many "1\\.0e\\+INF" (point-min) (point-max)) 1))))

;;; test-rules-wire.el ends here
