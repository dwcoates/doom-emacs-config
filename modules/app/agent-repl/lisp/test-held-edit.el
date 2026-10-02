;;; test-held-edit.el --- ERT tests for agent-repl held-edit.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-held-edit.el -f ert-run-tests-batch-and-exit
;;
;; The composer is a REAL `agent-repl-input-mode' buffer, because the edit
;; state, the attachments and the history ring are buffer-local; the wire is
;; the one stub, `agent-repl-rpc-edit-held-prompt', which records each
;; request and answers with the scripted response.  The host push is driven
;; by calling the hook function directly: it is state, and a push is a call.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-he--buffer nil
  "The composer buffer for the workspace under test, or nil for none.")

(defvar agent-repl-test-he--requests nil
  "EditHeldPrompt requests sent, newest first.")

(defvar agent-repl-test-he--answer '(:response (:arm :success :value nil))
  "The scripted answer: `(:response PLIST)' or `(:failure PLIST)'.")

(defvar agent-repl-test-he--messages nil
  "Strings passed to `message', newest first.")

(defvar agent-repl-test-he--info nil
  "Formatted `agent-repl--info' records, newest first.")

(defvar agent-repl-test-he--handovers nil
  "Refusal arms handed to host.el's handover path.")

(defconst agent-repl-test-he--ref '(:id "ws-id-1" :dir "/tmp/agent-repl-test/ws-1")
  "The decoded `WorkspaceRef' the requests echo.")

(defun agent-repl-test-he--said (text)
  "Return a `UserSaid' of TEXT alone."
  (list :content (list :blocks (list (list :arm :text :value (list :text text))))))

(defun agent-repl-test-he--edit (text &optional id)
  "Return a host view carrying a standing edit of turn t-1 with TEXT and ID."
  (list :held-prompt-edit (list :turn '(:value "t-1")
                                :said (agent-repl-test-he--said text)
                                :edit (or id 1))))

(defmacro agent-repl-test-he--with (&rest body)
  "Run BODY against a live composer with the edit wire stubbed."
  (declare (indent 0))
  `(let ((agent-repl-test-he--requests nil)
         (agent-repl-test-he--answer '(:response (:arm :success :value nil)))
         (agent-repl-test-he--messages nil)
         (agent-repl-test-he--info nil)
         (agent-repl-test-he--handovers nil)
         (agent-repl-test-he--buffer (generate-new-buffer " *agent-repl-test-held-edit*")))
     (unwind-protect
         (progn
           (with-current-buffer agent-repl-test-he--buffer
             (agent-repl-input-mode)
             (setq-local agent-repl--owning-workspace "ws-one"))
           (cl-letf* (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                      ((symbol-function 'agent-repl--ws-get)
                       (lambda (_ws key)
                         (pcase key
                           (:input-buffer agent-repl-test-he--buffer)
                           (:project-dir "/tmp/agent-repl-test/ws-1")
                           (_ nil))))
                      ((symbol-function 'agent-repl--history-save) (lambda (&optional _ws) nil))
                      ((symbol-function 'agent-repl-host-ref) (lambda (_ws) agent-repl-test-he--ref))
                      ((symbol-function 'agent-repl-verbs--conn) (lambda (&optional _ws) 'test-conn))
                      ((symbol-function 'agent-repl-host-handle-refusal)
                       (lambda (_ws arm) (push arm agent-repl-test-he--handovers)))
                      ((symbol-function 'agent-repl--info)
                       (lambda (_ws fmt &rest args)
                         (push (apply #'format fmt args) agent-repl-test-he--info)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (if args (apply #'format fmt args) fmt) agent-repl-test-he--messages)
                         nil))
                      ((symbol-function 'agent-repl-rpc-edit-held-prompt)
                       (lambda (_conn request &rest keys)
                         (push request agent-repl-test-he--requests)
                         (let ((failure (plist-get agent-repl-test-he--answer :failure)))
                           (if failure
                               (funcall (plist-get keys :on-failure) failure)
                             (funcall (plist-get keys :on-response)
                                      (plist-get agent-repl-test-he--answer :response)))))))
             ,@body))
       (when (buffer-live-p agent-repl-test-he--buffer)
         (kill-buffer agent-repl-test-he--buffer)))))

(defun agent-repl-test-he--type (text)
  "Put TEXT into the composer."
  (with-current-buffer agent-repl-test-he--buffer
    (erase-buffer)
    (insert text)))

(defun agent-repl-test-he--text ()
  "Return the composer's contents."
  (with-current-buffer agent-repl-test-he--buffer (buffer-string)))

(defun agent-repl-test-he--history ()
  "Return the composer's history ring."
  (buffer-local-value 'agent-repl--input-history agent-repl-test-he--buffer))

(defun agent-repl-test-he--step ()
  "Return the newest request's step keyword."
  (plist-get (plist-get (car agent-repl-test-he--requests) :action) :arm))

(defun agent-repl-test-he--logged-p (prefix)
  "Return non-nil when an info record starts with PREFIX."
  (and (cl-find-if (lambda (m) (string-prefix-p prefix m)) agent-repl-test-he--info) t))

(defun agent-repl-test-he--refusal (arm)
  "Return a scripted EditHeldPrompt refusal of ARM."
  (list :response (list :arm :error :value (list :cause (list :arm arm :value nil)))))

;;;; ---- Beginning: the composer takes the edit ----

(ert-deftest agent-repl-held-edit-a-new-edit-replaces-the-composer ()
  "A new standing edit puts the held prompt's words into the composer."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-test-he--type "a draft")
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "the held words"))
    ;; Assert
    (should (equal (agent-repl-test-he--text) "the held words"))))

(ert-deftest agent-repl-held-edit-a-non-blank-draft-is-saved-to-history ()
  "The words the user had typed are saved as a send would save them."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-test-he--type "a draft")
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "the held words"))
    ;; Assert
    (should (equal (agent-repl-test-he--history) '("a draft")))))

(ert-deftest agent-repl-held-edit-a-whitespace-draft-is-not-saved ()
  "A blank draft is not a prompt, so history gains nothing."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-test-he--type "  \n\t ")
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "the held words"))
    ;; Assert
    (should (null (agent-repl-test-he--history)))))

(ert-deftest agent-repl-held-edit-a-new-edit-marks-the-edit-mode ()
  "The composer is in edit mode, naming the turn it echoes."
  (agent-repl-test-he--with
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w" 4))
    ;; Assert
    (should (equal (agent-repl-held-edit-state "ws-one") '(:turn (:value "t-1") :edit 4)))))

(ert-deftest agent-repl-held-edit-the-edit-mode-shows-its-indicator ()
  "The mode line carries the edit-mode indicator while the edit stands."
  (agent-repl-test-he--with
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Assert
    (with-current-buffer agent-repl-test-he--buffer
      (should (equal (agent-repl--held-edit-segment) " editing held prompt")))))

(ert-deftest agent-repl-held-edit-the-begin-is-logged-at-info ()
  "Taking an edit is recorded on the info rung."
  (agent-repl-test-he--with
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Assert
    (should (agent-repl-test-he--logged-p "elisp.held-edit.began ws=ws-one turn=t-1"))))

(ert-deftest agent-repl-held-edit-the-same-edit-is-taken-once ()
  "A re-push of the edit already taken leaves the user's typing alone."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w" 2))
    (agent-repl-test-he--type "my revision")
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w" 2))
    ;; Assert
    (should (equal (agent-repl-test-he--text) "my revision"))))

(ert-deftest agent-repl-held-edit-a-path-image-becomes-an-attachment ()
  "An image the held prompt carries by path is re-attached to the composer."
  (agent-repl-test-he--with
    ;; Arrange
    (let ((host (list :held-prompt-edit
                      (list :turn '(:value "t-1") :edit 1
                            :said (list :content
                                        (list :blocks
                                              (list (list :arm :text :value '(:text "see"))
                                                    (list :arm :image
                                                          :value '(:location (:arm :path :value (:path "/tmp/i.png"))
                                                                   :media-type "image/png")))))))))
      ;; Act
      (agent-repl-held-edit-on-host-update "ws-one" host)
      ;; Assert
      (should (equal (buffer-local-value 'agent-repl-input-attachments agent-repl-test-he--buffer)
                     '((:path "/tmp/i.png" :media-type "image/png")))))))

(ert-deftest agent-repl-held-edit-a-url-image-cancels-the-edit ()
  "Content no composer can hold is not half-taken: the edit is cancelled."
  (agent-repl-test-he--with
    ;; Arrange
    (let ((host (list :held-prompt-edit
                      (list :turn '(:value "t-1") :edit 1
                            :said (list :content
                                        (list :blocks
                                              (list (list :arm :image
                                                          :value '(:location (:arm :url :value (:url "https://x"))
                                                                   :media-type "image/png")))))))))
      ;; Act
      (agent-repl-held-edit-on-host-update "ws-one" host)
      ;; Assert
      (should (eq (agent-repl-test-he--step) :cancel))
      (should (null (agent-repl-held-edit-state "ws-one"))))))

(ert-deftest agent-repl-held-edit-with-no-composer-the-edit-is-cancelled ()
  "With no composer to edit in, the edit is cancelled and said."
  (agent-repl-test-he--with
    ;; Arrange
    (kill-buffer agent-repl-test-he--buffer)
    (setq agent-repl-test-he--buffer nil)
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Assert
    (should (eq (agent-repl-test-he--step) :cancel))
    (should (string-prefix-p "agent-repl: no composer is open" (car agent-repl-test-he--messages)))))

;;;; ---- Leaving ----

(ert-deftest agent-repl-held-edit-an-absent-edit-ends-the-edit-mode ()
  "The daemon stating no edit stands ends the edit mode."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (list :held-prompt-edit nil))
    ;; Assert
    (should (null (agent-repl-held-edit-state "ws-one")))))

(ert-deftest agent-repl-held-edit-ending-the-edit-mode-keeps-the-composer ()
  "The composer is never erased on the edit's end."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (agent-repl-test-he--type "unsent revision")
    ;; Act
    (agent-repl-held-edit-on-host-update "ws-one" (list :held-prompt-edit nil))
    ;; Assert
    (should (equal (agent-repl-test-he--text) "unsent revision"))))

;;;; ---- Commit ----

(ert-deftest agent-repl-held-edit-commit-sends-the-new-content-on-the-edited-turn ()
  "A commit echoes the edit's turn and carries the new content whole."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Act
    (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") nil)
    ;; Assert
    (should (equal (car agent-repl-test-he--requests)
                   (list :workspace agent-repl-test-he--ref :turn '(:value "t-1")
                         :action (list :arm :commit
                                       :value (list :said (agent-repl-test-he--said "revised"))))))))

(ert-deftest agent-repl-held-edit-a-refused-commit-restores-the-composer ()
  "A refused commit gives the user their words back."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (agent-repl-test-he--type "revised")
    (setq agent-repl-test-he--answer (agent-repl-test-he--refusal :already-delivered))
    (let ((snapshot (agent-repl--input-optimistic-clear "ws-one" "revised")))
      ;; Act
      (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") snapshot))
    ;; Assert
    (should (equal (agent-repl-test-he--text) "revised"))))

(ert-deftest agent-repl-held-edit-a-refusal-is-said-in-the-echo-area ()
  "The refusal's arm reaches the user."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (setq agent-repl-test-he--answer (agent-repl-test-he--refusal :not-held))
    ;; Act
    (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") nil)
    ;; Assert
    (should (equal (car agent-repl-test-he--messages)
                   "agent-repl: editing the held prompt was refused -- not-held"))))

(ert-deftest agent-repl-held-edit-a-refusal-is-logged-at-info ()
  "The refusal is recorded with its step and its arm."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (setq agent-repl-test-he--answer (agent-repl-test-he--refusal :already-delivered))
    ;; Act
    (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") nil)
    ;; Assert
    (should (agent-repl-test-he--logged-p
             "elisp.held-edit.refused ws=ws-one step=commit arm=:already-delivered"))))

(ert-deftest agent-repl-held-edit-a-refusal-mid-delivery-is-said-as-being-delivered ()
  "A commit refused while the prompt's delivery is in flight says so."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (setq agent-repl-test-he--answer (agent-repl-test-he--refusal :being-delivered))
    ;; Act
    (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") nil)
    ;; Assert
    (should (equal (car agent-repl-test-he--messages)
                   "agent-repl: editing the held prompt was refused -- being-delivered"))))

(ert-deftest agent-repl-held-edit-a-handover-refusal-goes-to-the-handover-path ()
  "A handover arm is the rollout's, not a refusal to show the user."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (setq agent-repl-test-he--answer (agent-repl-test-he--refusal :not-yet-adopted))
    ;; Act
    (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") nil)
    ;; Assert
    (should (equal agent-repl-test-he--handovers '((:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-held-edit-a-commit-the-daemon-never-answered-restores-the-composer ()
  "A transport failure is not a lost edit: the words come back."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    (agent-repl-test-he--type "revised")
    (setq agent-repl-test-he--answer '(:failure (:kind :transport :message "down")))
    (let ((snapshot (agent-repl--input-optimistic-clear "ws-one" "revised")))
      ;; Act
      (agent-repl-held-edit-commit "ws-one" (agent-repl-test-he--said "revised") snapshot))
    ;; Assert
    (should (equal (agent-repl-test-he--text) "revised"))))

;;;; ---- Cancel ----

(ert-deftest agent-repl-held-edit-cancel-sends-the-cancel-step ()
  "A cancel names the edited turn and carries no content."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Act
    (agent-repl-held-edit-cancel "ws-one")
    ;; Assert
    (should (equal (car agent-repl-test-he--requests)
                   (list :workspace agent-repl-test-he--ref :turn '(:value "t-1")
                         :action '(:arm :cancel :value nil))))))

(ert-deftest agent-repl-held-edit-cancel-is-logged-at-info ()
  "The cancel gesture is recorded on the info rung."
  (agent-repl-test-he--with
    ;; Arrange
    (agent-repl-held-edit-on-host-update "ws-one" (agent-repl-test-he--edit "w"))
    ;; Act
    (agent-repl-held-edit-cancel "ws-one")
    ;; Assert
    (should (agent-repl-test-he--logged-p "elisp.held-edit.cancel ws=ws-one turn=t-1"))))

(provide 'test-held-edit)

;;; test-held-edit.el ends here
