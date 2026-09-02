;;; test-clipboard-image.el --- ERT tests for clipboard-image.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-clipboard-image.el -f ert-run-tests-batch-and-exit
;;
;; The ONE external boundary (`agent-repl--image-call-process', which shells
;; out to osascript and sips) is stubbed per test so each capture branch --
;; the PNG flavor, the TIFF-plus-conversion fallback, and an empty clipboard
;; -- runs with no subprocess.
;;
;; The behavior under test is the one the overhaul changed: the captured file
;; becomes an ATTACHMENT, registered through the composer's own entry point,
;; and the buffer text gets a MARKER rather than the path.  A path in the
;; text would have travelled as words, leaving the agent to infer that they
;; named an image, where the content model states it by arm.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-image--calls nil
  "Boundary invocations, oldest first, as (PROGRAM . ARGS).")

(defvar agent-repl-test-image--written nil
  "Paths the stubbed boundary should treat as successfully written.")

(defvar agent-repl-test-image--dir nil
  "The temporary workspace directory for the test.")

(defun agent-repl-test-image--stub (program &rest args)
  "Record the call and create the destination when it is a scripted success."
  (push (cons program args) agent-repl-test-image--calls)
  (let ((dest (cond
               ;; osascript writes the flavor to the path in its script.
               ((equal program "osascript")
                (when (string-match "POSIX file \"\\([^\"]+\\)\"" (cadr args))
                  (match-string 1 (cadr args))))
               ;; sips converts to the path after --out.
               ((equal program "sips") (car (last args))))))
    (if (and dest (member dest agent-repl-test-image--written))
        (progn (with-temp-file dest (insert "png-bytes")) 0)
      1)))

(defmacro agent-repl-test-image--with (&rest body)
  "Run BODY in a live composer buffer with the capture boundary stubbed."
  (declare (indent 0))
  `(let* ((agent-repl-test-image--calls nil)
          (agent-repl-test-image--written nil)
          (agent-repl-test-image--dir (make-temp-file "agent-repl-image-test" t))
          (buf (generate-new-buffer " *agent-repl-test-composer*")))
     (unwind-protect
         (progn
           (with-current-buffer buf (agent-repl-input-mode))
           (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                     ((symbol-function 'agent-repl--ws-dir)
                      (lambda (_ws) agent-repl-test-image--dir))
                     ((symbol-function 'agent-repl--ws-get)
                      (lambda (_ws key) (when (eq key :input-buffer) buf)))
                     ((symbol-function 'agent-repl--image-call-process)
                      #'agent-repl-test-image--stub)
                     ((symbol-function 'display-graphic-p) (lambda () nil))
                     ((symbol-function 'message) (lambda (&rest _) nil)))
             (with-current-buffer buf ,@body)))
       (kill-buffer buf)
       (delete-directory agent-repl-test-image--dir t))))

(defun agent-repl-test-image--allow-png ()
  "Script the PNG flavor to succeed at whatever path is allocated."
  (setq agent-repl-test-image--written
        (list (expand-file-name
               (car (directory-files agent-repl-test-image--dir nil "\\.png\\'"))
               agent-repl-test-image--dir))))

(defun agent-repl-test-image--programs ()
  "Return the programs the boundary was asked to run, oldest first."
  (mapcar #'car (reverse agent-repl-test-image--calls)))

;;;; ---- The capture branches ----

(ert-deftest agent-repl-image-png-flavor-is-tried-first ()
  "The PNG pasteboard flavor is what a macOS screenshot provides."
  (agent-repl-test-image--with
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (setq agent-repl-test-image--written (list dest))
      (should (equal (agent-repl--image-capture-clipboard dest "ws-one") dest))
      (should (equal (agent-repl-test-image--programs) '("osascript"))))))

(ert-deftest agent-repl-image-tiff-fallback-converts-with-sips ()
  "With only a TIFF flavor the capture converts it rather than giving up."
  (agent-repl-test-image--with
    (let* ((dest (expand-file-name "clip.png" agent-repl-test-image--dir))
           (tiff (expand-file-name "clip.tiff" agent-repl-test-image--dir)))
      ;; The PNG write fails; the TIFF write and the conversion succeed.
      (setq agent-repl-test-image--written (list tiff dest))
      (cl-letf* ((real #'agent-repl-test-image--stub)
                 ((symbol-function 'agent-repl--image-call-process)
                  (lambda (program &rest args)
                    (if (and (equal program "osascript")
                             (string-match-p "PNGf" (cadr args)))
                        (progn (push (cons program args) agent-repl-test-image--calls) 1)
                      (apply real program args)))))
        (should (equal (agent-repl--image-capture-clipboard dest "ws-one") dest))
        (should (equal (agent-repl-test-image--programs)
                       '("osascript" "osascript" "sips")))))))

(ert-deftest agent-repl-image-empty-clipboard-refuses ()
  "No image on the clipboard is a refusal, never a silent no-op."
  (agent-repl-test-image--with
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                    :type 'user-error))))

(ert-deftest agent-repl-image-an-empty-file-is-not-a-write ()
  "A zero-byte destination means the flavor was not really there."
  (agent-repl-test-image--with
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (_program &rest _args) (with-temp-file dest (insert "")) 0)))
        (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                      :type 'user-error)))))

;;;; ---- The attachment ----

(ert-deftest agent-repl-image-attach-registers-the-attachment ()
  "The captured file is registered on the composer, path and MIME type."
  (agent-repl-test-image--with
    (cl-letf (((symbol-function 'agent-repl--image-capture-clipboard)
               (lambda (dest &optional _ws) dest)))
      (let ((dest (agent-repl-attach-clipboard-image)))
        (should (equal agent-repl-input-attachments
                       (list (list :path dest
                                   :media-type agent-repl--image-media-type))))))))

(ert-deftest agent-repl-image-media-type-is-png ()
  "Both capture branches land as PNG, so the stated type is image/png."
  (should (equal agent-repl--image-media-type "image/png")))

(ert-deftest agent-repl-image-attach-inserts-a-marker-not-the-path ()
  "The buffer text is a MARKER: the path never travels as words."
  (agent-repl-test-image--with
    (cl-letf (((symbol-function 'agent-repl--image-capture-clipboard)
               (lambda (dest &optional _ws) dest)))
      (let ((dest (agent-repl-attach-clipboard-image)))
        (should (string-match-p (regexp-quote (agent-repl--image-marker-text dest))
                               (buffer-string)))
        (should-not (string-match-p (regexp-quote dest) (buffer-string)))))))

(ert-deftest agent-repl-image-marker-names-the-file ()
  "The marker names the file so the user can tell two attachments apart."
  (should (equal (agent-repl--image-marker-text "/tmp/x/clip-1.png")
                 "[image attached: clip-1.png]")))

(ert-deftest agent-repl-image-two-attachments-accumulate ()
  "Attaching twice attaches two images, in the order they were attached."
  (agent-repl-test-image--with
    (cl-letf (((symbol-function 'agent-repl--image-capture-clipboard)
               (lambda (dest &optional _ws) dest)))
      (let ((first (agent-repl-attach-clipboard-image))
            (second (agent-repl-attach-clipboard-image)))
        (should-not (equal first second))
        (should (equal (mapcar (lambda (a) (plist-get a :path))
                              agent-repl-input-attachments)
                       (list first second)))))))

(ert-deftest agent-repl-image-capture-failure-attaches-nothing ()
  "A failed capture leaves no attachment and no marker behind."
  (agent-repl-test-image--with
    (should-error (agent-repl-attach-clipboard-image) :type 'user-error)
    (should-not agent-repl-input-attachments)
    (should (equal (buffer-string) ""))))

;;;; ---- The capture destination ----

(ert-deftest agent-repl-image-dir-is-under-the-workspace ()
  "The file is written inside the workspace so the agent can read it."
  (agent-repl-test-image--with
    (let ((dir (agent-repl--image-dir "ws-one")))
      (should (file-directory-p dir))
      (should (string-prefix-p (file-name-as-directory agent-repl-test-image--dir) dir)))))

(ert-deftest agent-repl-image-new-path-is-a-png-under-the-dir ()
  "The allocated destination is a .png inside the capture directory."
  (agent-repl-test-image--with
    (let* ((dir (agent-repl--image-dir "ws-one"))
           (path (agent-repl--image-new-path dir "ws-one")))
      (should (equal (file-name-extension path) "png"))
      (should (string-prefix-p dir path))
      (should-not (file-exists-p path)))))

(ert-deftest agent-repl-image-new-path-is-unique-per-call ()
  "Two captures in the same second must not collide."
  (agent-repl-test-image--with
    (let ((dir (agent-repl--image-dir "ws-one")))
      (should-not (equal (agent-repl--image-new-path dir "ws-one")
                         (agent-repl--image-new-path dir "ws-one"))))))

(ert-deftest agent-repl-image-thumbnail-nil-without-graphics ()
  "A TTY frame draws no thumbnail, and the marker text carries on alone."
  (agent-repl-test-image--with
    (should-not (agent-repl--image-thumbnail "/tmp/x.png" "ws-one"))))

(provide 'test-clipboard-image)

;;; test-clipboard-image.el ends here
