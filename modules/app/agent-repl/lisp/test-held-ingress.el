;;; test-held-ingress.el --- ERT tests for agent-repl held-ingress.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-held-ingress.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Every test runs against a private `AGENT_REPL_STATE_DIR' under
;; `temporary-file-directory', so the ingress directory is real and the
;; writes, names and counts are the ones production makes.  Nothing here
;; talks to a daemon: ingestion is the daemon's, pinned by its own suites
;; (daemon/internal/heldingress, daemon/integration/heldingress_test.go).

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defun agent-repl-test-hi--said (text)
  "Return a `UserSaid' carrying TEXT, as the composer builds it."
  (list :content (list :blocks (list (list :arm :text :value (list :text text))))))

(defmacro agent-repl-test-hi--with (&rest body)
  "Run BODY with a private state dir and two workspaces, ws-one and ws-two.
Each has a project directory; ws-one has a live composer buffer."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let* ((state (make-temp-file "agent-repl-hi-state-" t))
            (hi--one (make-temp-file "hi-one-" t))
            (hi--two (make-temp-file "hi-two-" t))
            (process-environment (cons (concat "AGENT_REPL_STATE_DIR=" state)
                                       process-environment))
            (composer (generate-new-buffer " *hi-composer*")))
       (unwind-protect
           (progn
             (agent-repl--ws-put "ws-one" :project-dir hi--one)
             (agent-repl--ws-put "ws-two" :project-dir hi--two)
             (agent-repl--ws-put "ws-one" :input-buffer composer)
             ,@body)
         (when (buffer-live-p composer) (kill-buffer composer))
         (delete-directory hi--one t)
         (delete-directory hi--two t)
         (delete-directory state t)))))

(defun agent-repl-test-hi--files ()
  "Return every file name in the ingress directory, dot files included."
  (let ((dir (agent-repl-held-ingress-dir)))
    (and (file-directory-p dir)
         (directory-files dir nil "\\`[^.]\\|\\`\\.[^.]"))))

(defun agent-repl-test-hi--shown (ws)
  "Return the waiting line WS's composer shows."
  (agent-repl--input-waiting ws))

;;;; ---- Writing ----

(ert-deftest agent-repl-held-ingress-write-names-the-entry-by-time-hash-and-key ()
  "The entry is named held_<UTC stamp>_<dir hash>_<key>.json."
  (agent-repl-test-hi--with
    ;; Act
    (let ((path (agent-repl-held-ingress-write
                 "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")))
      ;; Assert
      (should (string-match-p
               (format "\\`held_[0-9]\\{8\\}T[0-9]\\{6\\}\\.[0-9]\\{9\\}_%s_k-1\\.json\\'"
                       (agent-repl--ws-dir-hash-cached "ws-one"))
               (file-name-nondirectory path))))))

(ert-deftest agent-repl-held-ingress-write-lands-under-the-state-root ()
  "The entry lands in `$AGENT_REPL_STATE_DIR/held-prompts/'."
  (agent-repl-test-hi--with
    ;; Act
    (let ((path (agent-repl-held-ingress-write
                 "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")))
      ;; Assert
      (should (equal (file-name-directory path)
                     (file-name-as-directory
                      (expand-file-name "held-prompts" (getenv "AGENT_REPL_STATE_DIR"))))))))

(ert-deftest agent-repl-held-ingress-write-carries-the-daemon-format ()
  "The body is the format the daemon's ingress reads, field for field."
  (agent-repl-test-hi--with
    ;; Act
    (let* ((path (agent-repl-held-ingress-write
                  "ws-one" (agent-repl-test-hi--said "hello") :deferred-prompt "k-1"))
           (body (with-temp-buffer
                   (insert-file-contents path)
                   (json-parse-buffer :object-type 'alist))))
      ;; Assert
      (should (equal (alist-get 'version body) 1))
      (should (equal (alist-get 'project_dir body)
                     (directory-file-name
                      (expand-file-name (agent-repl--ws-get "ws-one" :project-dir)))))
      (should (equal (alist-get 'idempotency_key body) "k-1"))
      (should (equal (alist-get 'origin body) "PROMPT_ORIGIN_DEFERRED_PROMPT"))
      (should (stringp (alist-get 'queued_at body)))
      (should (equal (alist-get 'text (alist-get 'text (aref (alist-get 'blocks (alist-get 'content (alist-get 'said body))) 0)))
                     "hello")))))

(defun agent-repl-test-hi--body-delivery (delivery)
  "Write one entry under DELIVERY and return its body's `delivery' member.
Answers the symbol `absent' when the body carries no such member."
  (let* ((path (agent-repl-held-ingress-write
                "ws-one" (agent-repl-test-hi--said "hi") :deferred-prompt "k-1" delivery))
         (body (with-temp-buffer
                 (insert-file-contents path)
                 (json-parse-buffer :object-type 'alist))))
    (if (assq 'delivery body) (alist-get 'delivery body) 'absent)))

(ert-deftest agent-repl-held-ingress-write-omits-an-ordinary-delivery ()
  "An ordinary prompt's entry carries no `delivery': absence is the fact."
  (agent-repl-test-hi--with
    ;; Act / Assert
    (should (eq (agent-repl-test-hi--body-delivery nil) 'absent))))

(ert-deftest agent-repl-held-ingress-write-carries-a-deferred-delivery ()
  "A deferred prompt's entry names its delivery, so the daemon keeps it deferred."
  (agent-repl-test-hi--with
    ;; Act / Assert
    (should (equal (agent-repl-test-hi--body-delivery :deferred)
                   "SUBMIT_PROMPT_DELIVERY_DEFERRED"))))

(ert-deftest agent-repl-held-ingress-write-keeps-non-ascii-words-intact ()
  "The file is UTF-8, so words outside ASCII reach the daemon unchanged."
  (agent-repl-test-hi--with
    ;; Act
    (let* ((path (agent-repl-held-ingress-write
                  "ws-one" (agent-repl-test-hi--said "héllo ✓ 日本") :user-sent "k-1"))
           (body (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8))
                     (insert-file-contents path))
                   (json-parse-buffer :object-type 'alist))))
      ;; Assert
      (should (equal (alist-get 'text (alist-get 'text (aref (alist-get 'blocks (alist-get 'content (alist-get 'said body))) 0)))
                     "héllo ✓ 日本")))))

(ert-deftest agent-repl-held-ingress-write-leaves-no-temporary-file ()
  "The body is renamed into place, so no dot-prefixed temp file remains."
  (agent-repl-test-hi--with
    ;; Act
    (agent-repl-held-ingress-write "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")
    ;; Assert
    (should (equal (length (agent-repl-test-hi--files)) 1))
    (should-not (seq-some (lambda (f) (string-prefix-p "." f)) (agent-repl-test-hi--files)))))

(ert-deftest agent-repl-held-ingress-write-of-a-key-already-written-keeps-one-entry ()
  "One attempt is one prompt, however many failure paths report it."
  (agent-repl-test-hi--with
    ;; Arrange
    (let ((first (agent-repl-held-ingress-write
                  "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")))
      ;; Act
      (let ((second (agent-repl-held-ingress-write
                     "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")))
        ;; Assert
        (should (equal second first))
        (should (equal (agent-repl-held-ingress-waiting "ws-one") 1))))))

(ert-deftest agent-repl-held-ingress-write-refuses-a-key-that-cannot-name-a-file ()
  "A key outside the UUID alphabet is refused, never escaped."
  (agent-repl-test-hi--with
    ;; Act, Assert
    (should-error (agent-repl-held-ingress-write
                   "ws-one" (agent-repl-test-hi--said "hi") :user-sent "../k"))))

(ert-deftest agent-repl-held-ingress-write-refuses-a-workspace-with-no-directory ()
  "An entry must name its workspace, so one with no directory signals."
  (agent-repl-test-hi--with
    ;; Act, Assert
    (should-error (agent-repl-held-ingress-write
                   "ws-nowhere" (agent-repl-test-hi--said "hi") :user-sent "k-1"))))

(ert-deftest agent-repl-held-ingress-write-refuses-an-unknown-origin ()
  "The origin vocabulary is closed; an unknown one is refused before writing."
  (agent-repl-test-hi--with
    ;; Act, Assert
    (should-error (agent-repl-held-ingress-write
                   "ws-one" (agent-repl-test-hi--said "hi") :no-such-origin "k-1"))
    (should-not (agent-repl-test-hi--files))))

(ert-deftest agent-repl-held-ingress-write-draws-the-waiting-line ()
  "A written entry shows at once in the composer's waiting line."
  (agent-repl-test-hi--with
    ;; Act
    (agent-repl-held-ingress-write "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")
    ;; Assert
    (should (equal (agent-repl-test-hi--shown "ws-one") "1 prompt waiting for the daemon"))))

;;;; ---- Counting ----

(ert-deftest agent-repl-held-ingress-entries-are-in-write-order ()
  "Entries come back oldest first, the order the daemon ingests them in."
  (agent-repl-test-hi--with
    ;; Arrange
    (let ((first (agent-repl-held-ingress-write
                  "ws-one" (agent-repl-test-hi--said "a") :user-sent "k-b"))
          (second (agent-repl-held-ingress-write
                   "ws-one" (agent-repl-test-hi--said "b") :user-sent "k-a")))
      ;; Act, Assert
      (should (equal (agent-repl-held-ingress-entries "ws-one") (list first second))))))

(ert-deftest agent-repl-held-ingress-waiting-counts-only-its-own-workspace ()
  "Another workspace's entries are not this one's waiting prompts."
  (agent-repl-test-hi--with
    ;; Arrange
    (agent-repl-held-ingress-write "ws-two" (agent-repl-test-hi--said "a") :user-sent "k-1")
    ;; Act, Assert
    (should (equal (agent-repl-held-ingress-waiting "ws-one") 0))))

(ert-deftest agent-repl-held-ingress-waiting-is-zero-with-no-ingress-directory ()
  "No directory yet is no waiting prompt, not an error."
  (agent-repl-test-hi--with
    ;; Act, Assert
    (should (equal (agent-repl-held-ingress-waiting "ws-one") 0))))

(ert-deftest agent-repl-held-ingress-waiting-text-by-count ()
  "The waiting line's words for each count."
  (dolist (case '((0 . nil)
                  (1 . "1 prompt waiting for the daemon")
                  (3 . "3 prompts waiting for the daemon")))
    ;; Act, Assert
    (should (equal (agent-repl-held-ingress--waiting-text (car case)) (cdr case)))))

;;;; ---- Surviving a restart ----

(ert-deftest agent-repl-held-ingress-a-restarted-emacs-shows-the-waiting-line-from-disk ()
  "After an Emacs restart, re-reading the ingress shows the waiting line.
Everything in memory is gone -- the workspace's cached hash, the composer
and its line -- and only the files remain."
  (agent-repl-test-hi--with
    ;; Arrange: two prompts held by the Emacs that is about to die.
    (agent-repl-held-ingress-write "ws-one" (agent-repl-test-hi--said "a") :user-sent "k-1")
    (agent-repl-held-ingress-write "ws-one" (agent-repl-test-hi--said "b") :user-sent "k-2")
    (let ((dir (agent-repl--ws-get "ws-one" :project-dir))
          (reborn (generate-new-buffer " *hi-reborn*")))
      (unwind-protect
          (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
            (agent-repl--ws-put "ws-one" :project-dir dir)
            (agent-repl--ws-put "ws-one" :input-buffer reborn)
            ;; Act
            (agent-repl-held-ingress-refresh "ws-one")
            ;; Assert
            (should (equal (agent-repl-test-hi--shown "ws-one")
                           "2 prompts waiting for the daemon")))
        (kill-buffer reborn)))))

;;;; ---- Clearing on the daemon's push ----

(ert-deftest agent-repl-held-ingress-host-push-clears-the-line-once-the-daemon-ingested ()
  "The daemon removes the entry, then pushes; that push clears the line."
  (agent-repl-test-hi--with
    ;; Arrange
    (let ((path (agent-repl-held-ingress-write
                 "ws-one" (agent-repl-test-hi--said "hi") :user-sent "k-1")))
      (delete-file path)
      ;; Act
      (run-hook-with-args 'agent-repl-host-update-functions "ws-one" nil)
      ;; Assert
      (should-not (agent-repl-test-hi--shown "ws-one")))))

(ert-deftest agent-repl-held-ingress-host-push-with-no-line-shown-lists-nothing ()
  "With no line standing, a host push never lists the directory."
  (agent-repl-test-hi--with
    ;; Arrange
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest _) (error "the directory was listed"))))
      ;; Act, Assert
      (agent-repl-held-ingress--on-host-update "ws-one" nil))))

(ert-deftest agent-repl-held-ingress-registers-on-the-host-update-hook ()
  "The clearing edge is wired at load time."
  (should (memq #'agent-repl-held-ingress--on-host-update agent-repl-host-update-functions)))

(ert-deftest agent-repl-held-ingress-fixture-removes-its-project-directories ()
  "The fixture's two project directories are gone once it exits."
  ;; Arrange
  (let (dirs)
    ;; Act
    (agent-repl-test-hi--with
      (setq dirs (list (agent-repl--ws-get "ws-one" :project-dir)
                       (agent-repl--ws-get "ws-two" :project-dir))))
    ;; Assert
    (should (equal (mapcar #'file-exists-p dirs) '(nil nil)))))

(provide 'test-held-ingress)

;;; test-held-ingress.el ends here
