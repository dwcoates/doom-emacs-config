;;; test-popup.el --- ERT tests for agent-repl popup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-popup.el -f ert-run-tests-batch-and-exit
;;
;; popup.el is the ONE shared popup subroutine; these tests pin what every
;; call site inherits from it — the popup takes the view's window and no
;; other, `q' closes it, kills the buffer and puts the view back — and
;; opening a path as a file, file+line or directory, and that every popup
;; call site goes through it.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defvar agent-repl-test-popup--errors nil
  "Messages handed to `agent-repl--error' during a test, newest first.")

(defmacro agent-repl-test-popup--in-frame (&rest body)
  "Run BODY on a one-window frame with no workspace view, then restore the frame.
The window the popup takes is then the selected one, as on a frame that
shows no agent panels."
  (declare (indent 0))
  `(let ((wconf (current-window-configuration)))
     (unwind-protect
         (progn
           (delete-other-windows)
           (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
             ,@body))
       (set-window-configuration wconf))))

(defmacro agent-repl-test-popup--with-panels (bindings &rest body)
  "Run BODY over a view window above a composer window beside a left sidebar.
BINDINGS names five variables, bound in order to the view window, the
composer window, the sidebar window, the view buffer and the composer
buffer.  The view window is dedicated, as the mounted view's is, and it
is what the current workspace's view lookup answers."
  (declare (indent 1))
  (pcase-let ((`(,view-win ,input-win ,side-win ,view-buf ,input-buf) bindings))
    `(let ((wconf (current-window-configuration))
           (,view-buf (generate-new-buffer " *popup-test-view*"))
           (,input-buf (generate-new-buffer " *popup-test-input*"))
           (side-buf (generate-new-buffer " *popup-test-sidebar*")))
       (unwind-protect
           (progn
             (delete-other-windows)
             (let* ((,side-win (display-buffer-in-side-window
                                side-buf '((side . left) (window-width . 20))))
                    (,view-win (window-main-window))
                    (,input-win (split-window ,view-win nil 'below)))
               (ignore ,side-win)
               (set-window-buffer ,view-win ,view-buf)
               (set-window-buffer ,input-win ,input-buf)
               (set-window-dedicated-p ,view-win t)
               (select-window ,input-win)
               (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws"))
                         ((symbol-function 'agent-repl-window--panel-window)
                          (lambda (kind &rest _)
                            (and (eq kind :view) (get-buffer-window ,view-buf)))))
                 ,@body)))
         (set-window-configuration wconf)
         (dolist (buf (list ,view-buf ,input-buf side-buf))
           (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun agent-repl-test-popup--window-shapes ()
  "Return each live window of the frame with its total width and height."
  (mapcar (lambda (win) (list win (window-total-width win) (window-total-height win)))
          (window-list nil 'nomini (frame-first-window))))

(defmacro agent-repl-test-popup--capturing-errors (&rest body)
  "Run BODY with every `agent-repl--error' record captured, not logged."
  (declare (indent 0))
  `(let ((agent-repl-test-popup--errors nil))
     (cl-letf (((symbol-function 'agent-repl--emit-error)
                (agent-repl-test--record-into agent-repl-test-popup--errors)))
       ,@body)))

(defmacro agent-repl-test-popup--with-tree (var &rest body)
  "Bind VAR to a fresh temp directory for BODY and delete it afterwards."
  (declare (indent 1))
  `(agent-repl-test--with-temp-dir ,var "agent-repl-popup-" ,@body))

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
      (let ((buffer (agent-repl-test-popup--in-frame
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
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\n")))
      (agent-repl-test--recording-popups
        ;; Act
        (let ((buffer (agent-repl-popup-open path)))
          ;; Assert
          (unwind-protect
              (should (equal agent-repl-test--popups-shown (list (get-file-buffer path))))
            (kill-buffer buffer)))))))

(ert-deftest agent-repl-test-popup-open-goes-to-the-given-line ()
  "LINE is 1-indexed, exactly as `HostOpenInEditor.line' is."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "one\ntwo\nthree\n")))
      ;; Act
      (let ((buffer (agent-repl-test-popup--in-frame
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
      (let ((buffer (agent-repl-test-popup--in-frame
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
      (let ((buffer (agent-repl-test-popup--in-frame
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
    (let ((buffer (agent-repl-test-popup--in-frame
                    (agent-repl-popup-open dir))))
      ;; Assert
      (unwind-protect
          (should (eq (buffer-local-value 'major-mode buffer) 'dired-mode))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-popup-open-shows-a-directory-in-the-popup ()
  "The dired buffer for a directory is shown in the popup's window, like a file."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    ;; Act
    (agent-repl-test-popup--in-frame
      (let ((buffer (agent-repl-popup-open dir)))
        ;; Assert
        (unwind-protect
            (progn
              (should (agent-repl-popup-buffer-p buffer))
              (should (eq (window-buffer (selected-window)) buffer)))
          (kill-buffer buffer))))))

;;;; ---- Refusals ----

(ert-deftest agent-repl-test-popup-open-refuses-a-missing-path ()
  "A path that is not there is a refusal, never a new empty buffer."
  ;; Arrange
  (agent-repl-test-popup--with-tree dir
    (let ((path (expand-file-name "absent.txt" dir)))
      ;; Act / Assert
      (should-error (agent-repl-test-popup--in-frame
                      (agent-repl-popup-open path))
                    :type 'user-error))))

(ert-deftest agent-repl-test-popup-open-refuses-an-empty-path ()
  "An empty path names nothing to open."
  ;; Act / Assert
  (should-error (agent-repl-test-popup--in-frame
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

;;;; ---- Showing a buffer: the window it takes ----

(ert-deftest agent-repl-test-popup-show-takes-the-view-window ()
  "The popup is shown in the window the workspace's view held."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win _view-buf _input-buf)
    (with-temp-buffer
      ;; Act
      (let ((win (agent-repl-popup-show (current-buffer))))
        ;; Assert
        (should (eq win view-win))
        (should (eq (window-buffer view-win) (current-buffer)))))))

(ert-deftest agent-repl-test-popup-show-selects-the-popup ()
  "The popup is focused on open, so `q' reaches it without a window jump."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win _view-buf _input-buf)
    (with-temp-buffer
      ;; Act
      (agent-repl-popup-show (current-buffer))
      ;; Assert
      (should (eq (selected-window) view-win)))))

(ert-deftest agent-repl-test-popup-show-leaves-the-composer-alone ()
  "The composer's window keeps its buffer."
  (agent-repl-test-popup--with-panels (_view-win input-win _side-win _view-buf input-buf)
    (with-temp-buffer
      ;; Act
      (agent-repl-popup-show (current-buffer))
      ;; Assert
      (should (eq (window-buffer input-win) input-buf)))))

(ert-deftest agent-repl-test-popup-show-moves-and-resizes-no-window ()
  "Every window, the sidebar's included, keeps its size, and none is added."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win _view-buf _input-buf)
    ;; Arrange
    (let ((before (agent-repl-test-popup--window-shapes)))
      (with-temp-buffer
        ;; Act
        (agent-repl-popup-show (current-buffer))
        ;; Assert
        (should (equal (agent-repl-test-popup--window-shapes) before))))))

(ert-deftest agent-repl-test-popup-show-takes-the-selected-window-without-a-view ()
  "With no view shown, the popup takes the selected main-area window."
  (agent-repl-test-popup--in-frame
    (with-temp-buffer
      ;; Arrange
      (let ((selected (selected-window)))
        ;; Act / Assert
        (should (eq (agent-repl-popup-show (current-buffer)) selected))))))

(ert-deftest agent-repl-test-popup-show-leaves-a-selected-side-window-for-the-main-area ()
  "With no view and a side window selected, the popup takes the largest main-area window."
  (agent-repl-test-popup--in-frame
    ;; Arrange
    (let* ((side-buf (generate-new-buffer " *popup-test-side*"))
           (main (selected-window))
           (side (display-buffer-in-side-window side-buf '((side . left)))))
      (unwind-protect
          (with-temp-buffer
            (select-window side)
            (cl-letf (((symbol-function 'agent-repl--frontend-largest-main-area-window)
                       (lambda () main)))
              ;; Act / Assert
              (should (eq (agent-repl-popup-show (current-buffer)) main))))
        (kill-buffer side-buf)))))

(ert-deftest agent-repl-test-popup-show-signals-without-a-main-area-window ()
  "A frame with no main-area window is a broken frame: signal and log."
  (agent-repl-test-popup--in-frame
    ;; Arrange
    (with-temp-buffer
      (let ((shown (current-buffer)))
        (cl-letf (((symbol-function 'agent-repl-window--side-window-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-repl--frontend-largest-main-area-window) (lambda () nil)))
          (agent-repl-test-popup--capturing-errors
            ;; Act
            (should-error (agent-repl-popup-show shown))
            ;; Assert
            (should (equal agent-repl-test-popup--errors
                           (list (format "elisp.popup.host-window: rejected reason=no-main-area-window frame=%S"
                                         (selected-frame)))))
            (should-not (agent-repl-popup-buffer-p shown))))))))

(ert-deftest agent-repl-test-popup-show-marks-the-buffer ()
  "A shown buffer is an agent-repl popup."
  (agent-repl-test-popup--in-frame
    (with-temp-buffer
      ;; Act
      (agent-repl-popup-show (current-buffer))
      ;; Assert
      (should (agent-repl-popup-buffer-p (buffer-name))))))

(ert-deftest agent-repl-test-popup-show-enables-the-popup-mode ()
  "A shown buffer carries `agent-repl-popup-mode', whose map holds `q'."
  (agent-repl-test-popup--in-frame
    (with-temp-buffer
      ;; Act
      (agent-repl-popup-show (current-buffer))
      ;; Assert
      (should agent-repl-popup-mode))))

(ert-deftest agent-repl-test-popup-buffer-p-rejects-an-unshown-buffer ()
  "A buffer never shown through the popup is not one."
  (with-temp-buffer
    (should-not (agent-repl-popup-buffer-p (buffer-name)))))

(ert-deftest agent-repl-test-popup-buffer-p-rejects-a-dead-buffer ()
  "A name that resolves to no live buffer is not a popup."
  (should-not (agent-repl-popup-buffer-p "agent-repl-test-popup-no-such-buffer")))

(ert-deftest agent-repl-test-popup-show-refuses-a-dead-buffer ()
  "A dead buffer is never displayed: the call signals and is logged."
  ;; Arrange
  (let ((buffer (generate-new-buffer "agent-repl-test-popup-dead")))
    (kill-buffer buffer)
    (agent-repl-test-popup--in-frame
      (let ((before (window-buffer (selected-window))))
        ;; Act
        (agent-repl-test-popup--capturing-errors
          (should-error (agent-repl-popup-show buffer))
          ;; Assert
          (should (eq (window-buffer (selected-window)) before))
          (should (equal agent-repl-test-popup--errors
                         (list (format "elisp.popup.show: rejected reason=dead-buffer buffer=%S" buffer)))))))))

;;;; ---- A popup over a popup ----

(ert-deftest agent-repl-test-popup-show-replaces-a-standing-popup-in-its-window ()
  "A second popup takes the first popup's window."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win _view-buf _input-buf)
    ;; Arrange
    (let ((first (generate-new-buffer " *popup-test-first*"))
          (second (generate-new-buffer " *popup-test-second*")))
      (unwind-protect
          (progn
            (agent-repl-popup-show first)
            ;; Act / Assert
            (should (eq (agent-repl-popup-show second) view-win)))
        (dolist (b (list first second)) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest agent-repl-test-popup-show-kills-the-replaced-popup ()
  "The popup a second one replaced is closed, its buffer killed."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win _view-buf _input-buf)
    ;; Arrange
    (let ((first (generate-new-buffer " *popup-test-first*"))
          (second (generate-new-buffer " *popup-test-second*")))
      (unwind-protect
          (progn
            (agent-repl-popup-show first)
            ;; Act
            (agent-repl-popup-show second)
            ;; Assert
            (should-not (buffer-live-p first)))
        (dolist (b (list first second)) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest agent-repl-test-popup-quit-after-a-replacement-puts-the-view-back ()
  "Closing the replacing popup puts back the view, never the replaced popup."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win view-buf _input-buf)
    ;; Arrange
    (let ((first (generate-new-buffer " *popup-test-first*"))
          (second (generate-new-buffer " *popup-test-second*")))
      (unwind-protect
          (progn
            (agent-repl-popup-show first)
            (agent-repl-popup-show second)
            ;; Act
            (with-current-buffer second (agent-repl-popup-quit))
            ;; Assert
            (should (eq (window-buffer view-win) view-buf)))
        (dolist (b (list first second)) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest agent-repl-test-popup-show-again-keeps-what-the-popup-stands-in-for ()
  "Showing the standing popup again leaves it standing in for the view."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (agent-repl-popup-show popup)
      (with-current-buffer popup (agent-repl-popup-quit))
      ;; Assert
      (should (eq (window-buffer view-win) view-buf)))))

;;;; ---- Standing in for the view ----

(ert-deftest agent-repl-test-popup-standing-in-for-names-the-view-window ()
  "While a popup stands in for the view, the view's old window is answered."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win view-buf _input-buf)
    (with-temp-buffer
      ;; Act
      (agent-repl-popup-show (current-buffer))
      ;; Assert
      (should (eq (agent-repl-popup-standing-in-for view-buf) view-win)))))

(ert-deftest agent-repl-test-popup-standing-in-for-rejects-another-buffer ()
  "A popup stands in only for the buffer its window showed."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win _view-buf input-buf)
    (with-temp-buffer
      ;; Act
      (agent-repl-popup-show (current-buffer))
      ;; Assert
      (should-not (agent-repl-popup-standing-in-for input-buf)))))

(ert-deftest agent-repl-test-popup-standing-in-for-ends-with-the-popup ()
  "Once the popup closes, nothing stands in for the view."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (with-current-buffer popup (agent-repl-popup-quit))
      ;; Assert
      (should-not (agent-repl-popup-standing-in-for view-buf)))))

;;;; ---- Closing ----

(ert-deftest agent-repl-test-popup-quit-puts-the-view-back ()
  "`q' shows the view in its window again."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (with-current-buffer popup (agent-repl-popup-quit))
      ;; Assert
      (should (eq (window-buffer view-win) view-buf)))))

(ert-deftest agent-repl-test-popup-quit-restores-the-view-window-dedication ()
  "The view's window is dedicated again, as it was before the popup."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win _view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (with-current-buffer popup (agent-repl-popup-quit))
      ;; Assert
      (should (eq (window-dedicated-p view-win) t)))))

(ert-deftest agent-repl-test-popup-quit-kills-the-buffer ()
  "`q' kills the popup's buffer."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win _view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (with-current-buffer popup (agent-repl-popup-quit))
      ;; Assert
      (should-not (buffer-live-p popup)))))

(ert-deftest agent-repl-test-popup-quit-moves-and-resizes-no-window ()
  "After `q' every window has the size it had before the popup."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win _view-buf _input-buf)
    ;; Arrange
    (let ((before (agent-repl-test-popup--window-shapes))
          (popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (with-current-buffer popup (agent-repl-popup-quit))
      ;; Assert
      (should (equal (agent-repl-test-popup--window-shapes) before)))))

(ert-deftest agent-repl-test-popup-killing-the-buffer-puts-the-view-back ()
  "A popup killed by any path, not only `q', puts the view back."
  (agent-repl-test-popup--with-panels (view-win _input-win _side-win view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*")))
      (agent-repl-popup-show popup)
      ;; Act
      (kill-buffer popup)
      ;; Assert
      (should (eq (window-buffer view-win) view-buf)))))

(ert-deftest agent-repl-test-popup-quit-with-a-dead-view-logs-and-kills ()
  "A view that died under the popup is not put back: the close logs it and still kills."
  (agent-repl-test-popup--with-panels (_view-win _input-win _side-win view-buf _input-buf)
    ;; Arrange
    (let ((popup (generate-new-buffer " *popup-test-popup*"))
          (infos nil))
      (agent-repl-popup-show popup)
      (kill-buffer view-buf)
      (cl-letf (((symbol-function 'agent-repl--emit-info)
                 (agent-repl-test--record-into infos)))
        ;; Act
        (with-current-buffer popup (agent-repl-popup-quit)))
      ;; Assert
      (should-not (buffer-live-p popup))
      (should (cl-some (lambda (msg) (string-prefix-p "elisp.popup.put-back: skipped reason=prior-buffer-dead" msg))
                       infos)))))

(ert-deftest agent-repl-test-popup-quit-saves-an-edited-file ()
  "Closing a file popup saves its unsaved edits first."
  (agent-repl-test-popup--with-tree dir
    (let ((path (agent-repl-test-popup--seed dir "notes.txt" "alpha\n")))
      (agent-repl-test-popup--in-frame
        ;; Arrange
        (let ((buffer (agent-repl-popup-open path)))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "beta\n")
            ;; Act
            (agent-repl-popup-quit))
          ;; Assert
          (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string))
                         "alpha\nbeta\n")))))))

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

;;; test-popup.el ends here
