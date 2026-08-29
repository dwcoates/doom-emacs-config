;;; test-popup.el --- ERT tests for agent-repl popup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-popup.el -f ert-run-tests-batch-and-exit
;;
;; popup.el is the ONE shared editor-popup subroutine; these tests pin the
;; four behaviors every call site inherits from it — file, file+line,
;; directory, and the refusal on a path that is not there.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defvar agent-repl-test-popup--displayed nil
  "Buffers handed to `display-buffer-in-side-window' during a test.")

(defvar agent-repl-test-popup--display-alists nil
  "Alists handed to `display-buffer-in-side-window' during a test.")

(defmacro agent-repl-test-popup--with-stubbed-display (&rest body)
  "Run BODY with the side-window display recorded rather than performed.
Batch Emacs has one tiny frame, so a real side window is not a thing the
subject can be asked for; what the tests are about is WHICH buffer is
handed to the side-window action and with which geometry."
  (declare (indent 0))
  `(let ((agent-repl-test-popup--displayed nil)
         (agent-repl-test-popup--display-alists nil))
     (cl-letf (((symbol-function 'display-buffer-in-side-window)
                (lambda (buffer alist)
                  (push buffer agent-repl-test-popup--displayed)
                  (push alist agent-repl-test-popup--display-alists)
                  (selected-window))))
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

(ert-deftest agent-repl-test-popup-open-displays-the-buffer-in-a-side-window ()
  "The file's buffer is handed to the side-window display action."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (prog1 (agent-repl-popup-open path)
                        (should (equal (length agent-repl-test-popup--displayed) 1))
                        (should (eq (car agent-repl-test-popup--displayed)
                                    (get-file-buffer path)))))))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-popup-open-displays-on-the-right ()
  "The popup is a RIGHT side window — the one shared spec."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (prog1 (agent-repl-popup-open path)
                        ;; Assert
                        (should (eq (cdr (assq 'side (car agent-repl-test-popup--display-alists)))
                                    'right))))))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-popup-open-takes-half-the-frame-width ()
  "The popup's width is half the frame's — the one shared spec."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--with-stubbed-display
                      (prog1 (agent-repl-popup-open path)
                        ;; Assert
                        (should (equal (cdr (assq 'window-width
                                                  (car agent-repl-test-popup--display-alists)))
                                       (round (* 0.5 (frame-width)))))))))
        (kill-buffer buffer)))))

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

;;; test-popup.el ends here
