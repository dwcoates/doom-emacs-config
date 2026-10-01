;;; test-popup.el --- ERT tests for agent-repl popup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-popup.el -f ert-run-tests-batch-and-exit
;;
;; popup.el is the ONE shared popup subroutine; these tests pin what every
;; call site inherits from it — the one Doom popup rule (right side, 40%
;; width, `q' closes it and kills the buffer), showing a buffer through that
;; rule, and opening a path as a file, file+line or directory — and that
;; every popup call site goes through it.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defvar agent-repl-test-popup--displayed nil
  "Buffers handed to `display-buffer' during a test, newest first.")

(defvar agent-repl-test-popup--errors nil
  "Messages handed to `agent-repl--error' during a test, newest first.")

(defmacro agent-repl-test-popup--with-stubbed-display (&rest body)
  "Run BODY with `display-buffer' recorded rather than performed.
Batch Emacs has no Doom popup system, so what the tests are about is WHICH
buffer reaches `display-buffer' (where the Doom popup rule takes over) and
in what state.  The stub answers the selected window, as a working popup
system does."
  (declare (indent 0))
  `(let ((agent-repl-test-popup--displayed nil))
     (cl-letf (((symbol-function 'display-buffer)
                (lambda (buffer &rest _)
                  (push buffer agent-repl-test-popup--displayed)
                  (selected-window))))
       ,@body)))

(defmacro agent-repl-test-popup--capturing-errors (&rest body)
  "Run BODY with every `agent-repl--error' record captured, not logged."
  (declare (indent 0))
  `(let ((agent-repl-test-popup--errors nil))
     (cl-letf (((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-popup--errors))))
       ,@body)))

(defmacro agent-repl-test-popup--with-tree (var &rest body)
  "Bind VAR to a fresh temp directory for BODY and delete it afterwards."
  (declare (indent 1))
  `(let ((,var (make-temp-file "agent-repl-popup-" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun agent-repl-test-popup--seed (dir name content)
  "Write CONTENT into NAME under DIR and return the path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert content))
    path))

;;;; ---- Files ----

(ert-deftest agent-repl-test-popup-open-visits-a-file ()
  "Opening a file yields that file's buffer."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\nbeta\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (agent-repl-popup-open path))))
        ;; Assert
        (unwind-protect
            (should (equal (file-truename (buffer-file-name buffer))
                           (file-truename path)))
          (kill-buffer buffer))))))

(ert-deftest agent-repl-test-popup-open-shows-the-file-through-the-shared-popup ()
  "The file's buffer is shown through `agent-repl-popup-show'."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\n"))
          (shown nil))
      ;; Act
      (let ((buffer (cl-letf (((symbol-function 'agent-repl-popup-show)
                               (lambda (buf) (push buf shown) (selected-window))))
                      (agent-repl-popup-open path))))
        ;; Assert
        (unwind-protect
            (should (equal shown (list (get-file-buffer path))))
          (kill-buffer buffer))))))

(ert-deftest agent-repl-test-popup-open-goes-to-the-given-line ()
  "LINE is 1-indexed, exactly as `HostOpenInEditor.line' is."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "one\ntwo\nthree\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (agent-repl-popup-open path 3))))
        ;; Assert
        (unwind-protect
            (should (equal (with-current-buffer buffer
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                           "three"))
          (kill-buffer buffer))))))

(ert-deftest agent-repl-test-popup-open-without-a-line-stays-at-the-top ()
  "No line means the file's top."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "one\ntwo\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (agent-repl-popup-open path))))
        ;; Assert
        (unwind-protect
            (should (equal (with-current-buffer buffer (point)) (point-min)))
          (kill-buffer buffer))))))

(ert-deftest agent-repl-test-popup-open-clamps-a-line-past-the-end ()
  "A stale line hint still opens the file, at its last line."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "one\ntwo")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (agent-repl-popup-open path 99))))
        ;; Assert
        (unwind-protect
            (should (equal (with-current-buffer buffer
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                           "two"))
          (kill-buffer buffer))))))

;;;; ---- Directories ----

(ert-deftest agent-repl-test-popup-open-uses-dired-for-a-directory ()
  "A directory opens in dired."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    ;; Act
    (let ((buffer (agent-repl-test-popup--with-stubbed-display
                    (agent-repl-popup-open dir))))
      ;; Assert
      (unwind-protect
          (should (eq (buffer-local-value 'major-mode buffer) 'dired-mode))
        (kill-buffer buffer)))))

;;;; ---- Refusals ----

(ert-deftest agent-repl-test-popup-open-refuses-a-missing-path ()
  "A path that is not there is a refusal, never a new empty buffer."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (expand-file-name "absent.txt" dir)))
      ;; Act / Assert
      (should-error (agent-repl-test-popup--with-stubbed-display
                      (agent-repl-popup-open path))
                    :type 'user-error))))

(ert-deftest agent-repl-test-popup-open-refuses-an-empty-path ()
  "An empty path names nothing to open."
  ;; Act / Assert
  (should-error (agent-repl-test-popup--with-stubbed-display
                  (agent-repl-popup-open ""))
                :type 'user-error))

(ert-deftest agent-repl-test-popup-open-logs-a-missing-path ()
  "A missing path's refusal is recorded with the path it named."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (expand-file-name "absent.txt" dir)))
      ;; Act
      (agent-repl-test-popup--capturing-errors
        (ignore-errors (agent-repl-popup-open path))
        ;; Assert
        (should (equal agent-repl-test-popup--errors
                       (list (format "elisp.popup.open: rejected reason=missing-path path=%s" path))))))))

;;;; ---- The one popup rule ----

(ert-deftest agent-repl-test-popup-rule-opens-on-the-right ()
  "Every agent-repl popup is a RIGHT side window."
  (should (eq (plist-get agent-repl-popup-rule :side) 'right)))

(ert-deftest agent-repl-test-popup-rule-takes-forty-percent-of-the-width ()
  "Every agent-repl popup takes 40% of the frame's width."
  (should (equal (plist-get agent-repl-popup-rule :size) 0.4)))

(ert-deftest agent-repl-test-popup-rule-kills-the-buffer-on-close ()
  "`:ttl 0': closing a popup kills its buffer at once."
  (should (equal (plist-get agent-repl-popup-rule :ttl) 0)))

(ert-deftest agent-repl-test-popup-rule-selects-the-popup ()
  "The popup is focused on open, so `q' reaches it without a window jump."
  (should (eq (plist-get agent-repl-popup-rule :select) t)))

(ert-deftest agent-repl-test-popup-install-rule-registers-the-rule ()
  "The rule is registered with Doom under the shared predicate."
  ;; Arrange
  (let ((registered nil))
    (cl-letf (((symbol-function 'set-popup-rule!)
               (lambda (predicate &rest plist) (setq registered (cons predicate plist)))))
      ;; Act
      (let ((installed (agent-repl-popup-install-rule)))
        ;; Assert
        (should installed)
        (should (equal registered (cons #'agent-repl-popup-buffer-p agent-repl-popup-rule)))))))

(ert-deftest agent-repl-test-popup-install-rule-without-doom-installs-nothing ()
  "Under `emacs -Q' there is no popup module, and installing says so."
  ;; Arrange
  (let ((had (fboundp 'set-popup-rule!))
        (saved (and (fboundp 'set-popup-rule!) (symbol-function 'set-popup-rule!))))
    (unwind-protect
        (progn
          (fmakunbound 'set-popup-rule!)
          ;; Act / Assert
          (should-not (agent-repl-popup-install-rule)))
      (when had (fset 'set-popup-rule! saved)))))

;;;; ---- Showing a buffer ----

(ert-deftest agent-repl-test-popup-show-hands-the-buffer-to-display ()
  "The buffer reaches `display-buffer', where the Doom popup rule takes over."
  ;; Arrange
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      ;; Act
      (agent-repl-test-popup--with-stubbed-display
        (agent-repl-popup-show buffer)
        ;; Assert
        (should (equal agent-repl-test-popup--displayed (list buffer)))))))

(ert-deftest agent-repl-test-popup-show-marks-the-buffer-for-the-rule ()
  "A shown buffer matches the shared rule's predicate."
  ;; Arrange
  (with-temp-buffer
    ;; Act
    (agent-repl-test-popup--with-stubbed-display
      (agent-repl-popup-show (current-buffer)))
    ;; Assert
    (should (agent-repl-popup-buffer-p (buffer-name)))))

(ert-deftest agent-repl-test-popup-show-enables-the-popup-mode ()
  "A shown buffer carries `agent-repl-popup-mode', whose map holds `q'."
  ;; Arrange
  (with-temp-buffer
    ;; Act
    (agent-repl-test-popup--with-stubbed-display
      (agent-repl-popup-show (current-buffer)))
    ;; Assert
    (should agent-repl-popup-mode)))

(ert-deftest agent-repl-test-popup-show-returns-the-window ()
  "The window the popup system answered is returned to the caller."
  ;; Arrange
  (with-temp-buffer
    ;; Act / Assert
    (agent-repl-test-popup--with-stubbed-display
      (should (eq (agent-repl-popup-show (current-buffer)) (selected-window))))))

(ert-deftest agent-repl-test-popup-buffer-p-rejects-an-unshown-buffer ()
  "A buffer never shown through the popup does not match the rule."
  (with-temp-buffer
    (should-not (agent-repl-popup-buffer-p (buffer-name)))))

(ert-deftest agent-repl-test-popup-buffer-p-rejects-a-dead-buffer ()
  "A name that resolves to no live buffer does not match the rule."
  (should-not (agent-repl-popup-buffer-p "agent-repl-test-popup-no-such-buffer")))

(ert-deftest agent-repl-test-popup-show-refuses-a-dead-buffer ()
  "A dead buffer is never displayed: the call signals and is logged."
  ;; Arrange
  (let ((buffer (generate-new-buffer "agent-repl-test-popup-dead")))
    (kill-buffer buffer)
    ;; Act
    (agent-repl-test-popup--capturing-errors
      (agent-repl-test-popup--with-stubbed-display
        (should-error (agent-repl-popup-show buffer))
        ;; Assert
        (should (null agent-repl-test-popup--displayed))
        (should (equal agent-repl-test-popup--errors
                       (list (format "elisp.popup.show: rejected reason=dead-buffer buffer=%S" buffer))))))))

(ert-deftest agent-repl-test-popup-show-signals-when-no-window-appears ()
  "A display that yields no window is a broken popup system: signal and log."
  ;; Arrange
  (with-temp-buffer
    (let ((name (buffer-name)))
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-test-popup--capturing-errors
          (should-error (agent-repl-popup-show (current-buffer)))
          ;; Assert
          (should (equal agent-repl-test-popup--errors
                         (list (format "elisp.popup.show: rejected reason=no-window buffer=%s" name)))))))))

;;;; ---- Closing ----

(ert-deftest agent-repl-test-popup-quit-closes-through-doom ()
  "`q' closes the popup through Doom, whose `:ttl 0' kills the buffer."
  ;; Arrange
  (let ((quit nil))
    (cl-letf (((symbol-function '+popup/quit-window) (lambda (&rest _) (setq quit t))))
      ;; Act
      (agent-repl-popup-quit)
      ;; Assert
      (should quit))))

(ert-deftest agent-repl-test-popup-binds-q-in-normal-state ()
  "`q' is bound to the popup quit in Evil normal state on the popup map."
  ;; Arrange
  (let ((bound nil))
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state keymap key def &rest _) (setq bound (list state keymap key def)))))
      ;; Act
      (agent-repl-popup--bind-keys)
      ;; Assert
      (should (equal bound (list 'normal agent-repl-popup-mode-map "q" #'agent-repl-popup-quit))))))

;;;; ---- Every popup call site shares the subroutine ----

(defconst agent-repl-test-popup--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The lisp/ directory the module sources live in.")

(defun agent-repl-test-popup--source (name)
  "Return the text of the module source NAME under lisp/."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name agent-repl-test-popup--lisp-dir))
    (buffer-string)))

(ert-deftest agent-repl-test-popup-no-module-displays-a-popup-itself ()
  "No module but popup.el calls `display-buffer' for a popup.
magit.el's same-window display is the one other call, and it is not a popup."
  ;; Arrange
  (let ((sources (cl-remove-if (lambda (name) (string-prefix-p "test-" name))
                               (directory-files agent-repl-test-popup--lisp-dir nil "\\.el\\'"))))
    ;; Act
    (let ((callers (cl-remove-if-not
                    (lambda (name)
                      (string-match-p "(display-buffer\\(-in-side-window\\)? "
                                      (agent-repl-test-popup--source name)))
                    sources)))
      ;; Assert
      (should (equal (sort callers #'string<) '("magit.el" "popup.el"))))))

(ert-deftest agent-repl-test-popup-information-views-use-the-shared-popup ()
  "The build log and the health report are shown through `agent-repl-popup-show'."
  (dolist (name '("daemon.el" "verbs.el"))
    (should (string-match-p "(agent-repl-popup-show " (agent-repl-test-popup--source name)))))

;;; test-popup.el ends here
