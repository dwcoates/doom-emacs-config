;;; test-clipboard-image.el --- ERT tests for clipboard-image.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the clipboard-image attach capability.  The single external
;; boundary (`agent-repl--image-call-process', which shells out to
;; `osascript'/`sips') is stubbed with `cl-letf' per test, so nothing here
;; touches the real pasteboard or spawns a subprocess.  Filesystem effects
;; land under per-test temp directories that are cleaned up afterward.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-clipboard-image.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fixtures -------------------------------------------------------------

(defmacro agent-repl-test--with-image-project (dir-var &rest body)
  "Register workspace \"ws1\" with a fresh temp `:project-dir' bound to DIR-VAR.
Cleans the temp tree up after BODY."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((,dir-var (file-name-as-directory
                      (make-temp-file "agent-repl-image-test" t))))
       (unwind-protect
           (progn
             (agent-repl--ws-put "ws1" :project-dir ,dir-var)
             ,@body)
         (when (file-directory-p ,dir-var)
           (delete-directory ,dir-var t))))))

;;;; ---- Boundary registration ------------------------------------------------

(ert-deftest agent-repl-test-image-call-process-is-registered-boundary ()
  "The capture wrapper must be in the external-boundary registry so the
harness guards it (else a test could shell out to the real `osascript')."
  (should (memq 'agent-repl--image-call-process
                agent-repl--external-boundary-functions)))

;;;; ---- agent-repl--image-dir ------------------------------------------------

(ert-deftest agent-repl-test-image-dir-creates-under-project ()
  "The image dir is `<project>/.claude/emacs/images/' and is created."
  (agent-repl-test--with-image-project root
    (let ((dir (agent-repl--image-dir "ws1")))
      (should (equal dir (expand-file-name ".claude/emacs/images/" root)))
      (should (file-directory-p dir)))))

;;;; ---- agent-repl--image-new-path -------------------------------------------

(ert-deftest agent-repl-test-image-new-path-is-uncreated-png-under-dir ()
  "A fresh path is a `.png' under DIR that does not yet exist on disk."
  (agent-repl-test--with-image-project root
    (let* ((dir (agent-repl--image-dir "ws1"))
           (path (agent-repl--image-new-path dir)))
      (should (equal (file-name-directory path) dir))
      (should (equal (file-name-extension path) "png"))
      (should-not (file-exists-p path)))))

;;;; ---- agent-repl--image-nonempty-file-p ------------------------------------

(ert-deftest agent-repl-test-image-nonempty-file-p-true-for-bytes ()
  "A file holding bytes is reported non-empty."
  (agent-repl-test--with-image-project root
    (let ((f (expand-file-name "x.png" root)))
      (agent-repl-test--seed-file f "PNGDATA")
      (should (agent-repl--image-nonempty-file-p f)))))

(ert-deftest agent-repl-test-image-nonempty-file-p-nil-for-empty ()
  "A zero-byte file is not reported non-empty."
  (agent-repl-test--with-image-project root
    (let ((f (expand-file-name "x.png" root)))
      (agent-repl-test--seed-file f "")
      (should-not (agent-repl--image-nonempty-file-p f)))))

(ert-deftest agent-repl-test-image-nonempty-file-p-nil-for-missing ()
  "A missing file is not reported non-empty."
  (agent-repl-test--with-image-project root
    (should-not (agent-repl--image-nonempty-file-p
                 (expand-file-name "nope.png" root)))))

;;;; ---- agent-repl--image-write-flavor ---------------------------------------

(ert-deftest agent-repl-test-image-write-flavor-success ()
  "Exit 0 plus a non-empty DEST yields non-nil, and the PNG flavor is
passed to `osascript'."
  (agent-repl-test--with-image-project root
    (let ((dest (expand-file-name "clip.png" root))
          (seen nil))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (program &rest args)
                   (setq seen (cons program args))
                   (agent-repl-test--seed-file dest "PNGBYTES")
                   0)))
        (should (agent-repl--image-write-flavor "«class PNGf»" dest))
        (should (equal (car seen) "osascript"))
        (should (string-match-p "PNGf" (mapconcat #'identity (cdr seen) " ")))))))

(ert-deftest agent-repl-test-image-write-flavor-nil-on-nonzero-exit ()
  "A non-zero exit code yields nil even if a file happens to exist."
  (agent-repl-test--with-image-project root
    (let ((dest (expand-file-name "clip.png" root)))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (&rest _)
                   (agent-repl-test--seed-file dest "PNGBYTES")
                   1)))
        (should-not (agent-repl--image-write-flavor "«class PNGf»" dest))))))

(ert-deftest agent-repl-test-image-write-flavor-nil-on-empty-file ()
  "Exit 0 but an empty DEST yields nil (nothing was actually written)."
  (agent-repl-test--with-image-project root
    (let ((dest (expand-file-name "clip.png" root)))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (&rest _)
                   (agent-repl-test--seed-file dest "")
                   0)))
        (should-not (agent-repl--image-write-flavor "«class PNGf»" dest))))))

;;;; ---- agent-repl--image-capture-clipboard ----------------------------------

(ert-deftest agent-repl-test-image-capture-uses-png-flavor-first ()
  "When the PNG flavor writes, capture returns DEST and never calls `sips'."
  (agent-repl-test--with-image-project root
    (let ((dest (expand-file-name "clip.png" root))
          (programs nil))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (program &rest _)
                   (push program programs)
                   (when (equal program "osascript")
                     (agent-repl-test--seed-file dest "PNGBYTES"))
                   0)))
        (should (equal (agent-repl--image-capture-clipboard dest) dest))
        (should-not (member "sips" programs))))))

(ert-deftest agent-repl-test-image-capture-falls-back-to-tiff-plus-sips ()
  "When only a TIFF flavor is present, capture writes the TIFF and converts
it to PNG with `sips', returning DEST."
  (agent-repl-test--with-image-project root
    (let* ((dest (expand-file-name "clip.png" root))
           (tiff (expand-file-name "clip.tiff" root))
           (sips-args nil))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (program &rest args)
                   (cond
                    ;; The PNG-flavor write fails (no such flavor present).
                    ((and (equal program "osascript")
                          (string-match-p "PNGf" (mapconcat #'identity args " ")))
                     1)
                    ;; The TIFF-flavor write succeeds.
                    ((and (equal program "osascript")
                          (string-match-p "TIFF" (mapconcat #'identity args " ")))
                     (agent-repl-test--seed-file tiff "TIFFBYTES")
                     0)
                    ;; sips converts the TIFF into the PNG dest.
                    ((equal program "sips")
                     (setq sips-args args)
                     (agent-repl-test--seed-file dest "PNGFROMTIFF")
                     0)
                    (t 1)))))
        (should (equal (agent-repl--image-capture-clipboard dest) dest))
        (should (member "png" sips-args))
        (should (member tiff sips-args))
        (should (member dest sips-args))))))

(ert-deftest agent-repl-test-image-capture-signals-when-no-image ()
  "With neither flavor available, capture signals a `user-error'."
  (agent-repl-test--with-image-project root
    (let ((dest (expand-file-name "clip.png" root)))
      (cl-letf (((symbol-function 'agent-repl--image-call-process)
                 (lambda (&rest _) 1)))
        (should-error (agent-repl--image-capture-clipboard dest)
                      :type 'user-error)))))

;;;; ---- agent-repl--image-insert-token ---------------------------------------

(ert-deftest agent-repl-test-image-insert-token-inserts-path-text ()
  "The inserted buffer text is exactly the path (batch frame: no thumbnail)."
  (with-temp-buffer
    (let ((path "/tmp/proj/.claude/emacs/images/clip.png"))
      (should-not (agent-repl--image-insert-token path))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat path "\n"))))))

(ert-deftest agent-repl-test-image-insert-token-prepends-newline-off-bol ()
  "When point is not at line start, the path is pushed onto its own line."
  (with-temp-buffer
    (insert "describe this:")
    (agent-repl--image-insert-token "/tmp/clip.png")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "describe this:\n/tmp/clip.png\n"))))

(ert-deftest agent-repl-test-image-insert-token-overlays-thumbnail-when-graphic ()
  "On a graphic frame the path is overlaid with a thumbnail whose display is
the image, yet the underlying buffer text stays exactly the path."
  (with-temp-buffer
    (let ((path "/tmp/clip.png")
          (fake-image '(image :type png :fake t)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image) (lambda (&rest _) fake-image)))
        (let ((ov (agent-repl--image-insert-token path)))
          (should (overlayp ov))
          (should (equal (overlay-get ov 'display) fake-image))
          (should (equal (overlay-get ov 'agent-repl-image) path))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         (concat path "\n"))))))))

;;;; ---- agent-repl-attach-clipboard-image ------------------------------------

(ert-deftest agent-repl-test-attach-clipboard-image-inserts-captured-path ()
  "The command captures to the workspace image dir and inserts that path
into the buffer it runs in, returning the path."
  (agent-repl-test--with-image-project root
    (with-temp-buffer
      (let (captured-dest)
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "ws1"))
                  ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--image-capture-clipboard)
                   (lambda (dest &optional ws)
                     (should (equal ws "ws1"))
                     (setq captured-dest dest)
                     dest)))
          (let ((result (agent-repl-attach-clipboard-image)))
            (should (equal result captured-dest))
            (should (string-prefix-p
                     (expand-file-name ".claude/emacs/images/" root)
                     captured-dest))
            (should (string-match-p (regexp-quote captured-dest)
                                    (buffer-string)))))))))

(ert-deftest agent-repl-test-attach-clipboard-image-propagates-no-image-error ()
  "A capture that finds no image surfaces its `user-error' from the command."
  (agent-repl-test--with-image-project root
    (with-temp-buffer
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "ws1"))
                ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--image-capture-clipboard)
                 (lambda (_ &optional ws)
                   (should (equal ws "ws1"))
                   (user-error "agent-repl: no image found on the clipboard"))))
        (should-error (agent-repl-attach-clipboard-image) :type 'user-error)))))

(provide 'test-clipboard-image)

;;; test-clipboard-image.el ends here
