;;; test-clipboard-image.el --- ERT tests for clipboard-image.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-clipboard-image.el -f ert-run-tests-batch-and-exit
;;
;; Every external boundary is stubbed per test so each capture branch runs
;; with no subprocess and no real clipboard: `agent-repl--image-call-process'
;; (osascript/sips) for the macOS branches -- the PNG flavor, the
;; TIFF-plus-conversion fallback, and an empty clipboard -- and
;; `agent-repl--image-call-process-to-file' plus
;; `agent-repl--image-executable-find' for the Linux branches, where the
;; display server and what is installed on PATH are both stated by the test.
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
          ;; The AppleScript branch is a macOS branch, so these scenarios
          ;; state the platform rather than inheriting the host's.
          (system-type 'darwin)
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

;;;; ---- Which reader the platform gets ----

;; The capture used to be macOS-only: it shelled out to `osascript' on every
;; host, so on Linux -- the e2e sandbox and any Linux user -- the verb simply
;; failed.  The reader is now chosen BY PLATFORM at the source, and a host
;; with no reader installed is REPORTED, naming the tool it wanted.

(defvar agent-repl-test-image--to-file-calls nil
  "Calls to the stdout-to-file boundary, oldest first, as (PROGRAM . ARGS).")

(defvar agent-repl-test-image--installed nil
  "Program names the fake host has on PATH.")

(defvar agent-repl-test-image--errors nil
  "Formatted `agent-repl--error' records, oldest first.")

(defmacro agent-repl-test-image--on-linux (display installed &rest body)
  "Run BODY on a fake Linux host under DISPLAY with INSTALLED tools on PATH.
DISPLAY is `wayland' or `x11'; INSTALLED is a list of program names.  The
capture writes PNG bytes whenever it runs, so a scenario that reaches a
reader gets a successful capture unless it says otherwise."
  (declare (indent 2))
  `(let* ((agent-repl-test-image--to-file-calls nil)
          (agent-repl-test-image--installed ,installed)
          (agent-repl-test-image--errors nil)
          (agent-repl-test-image--dir (make-temp-file "agent-repl-image-test" t))
          (system-type 'gnu/linux)
          (process-environment
           (cons (if (eq ,display 'wayland) "WAYLAND_DISPLAY=wayland-0" "DISPLAY=:0")
                 (seq-remove (lambda (v) (or (string-prefix-p "DISPLAY=" v)
                                             (string-prefix-p "WAYLAND_DISPLAY=" v)))
                             process-environment))))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                   ((symbol-function 'agent-repl--ws-dir)
                    (lambda (_ws) agent-repl-test-image--dir))
                   ((symbol-function 'agent-repl--image-executable-find)
                    (lambda (program)
                      (when (member program agent-repl-test-image--installed)
                        (concat "/usr/bin/" program))))
                   ((symbol-function 'agent-repl--image-call-process-to-file)
                    (lambda (dest program &rest args)
                      (push (cons program args) agent-repl-test-image--to-file-calls)
                      (with-temp-file dest (insert "png-bytes"))
                      0))
                   ((symbol-function 'agent-repl--error)
                    (lambda (_ws fmt &rest args)
                      (push (apply #'format fmt args) agent-repl-test-image--errors)
                      nil))
                   ((symbol-function 'message) (lambda (&rest _) nil)))
           ,@body)
       (delete-directory agent-repl-test-image--dir t))))

(defun agent-repl-test-image--to-file-programs ()
  "Return the programs the stdout-to-file boundary ran, oldest first."
  (mapcar #'car (reverse agent-repl-test-image--to-file-calls)))

(ert-deftest agent-repl-image-macos-reads-through-osascript ()
  "On macOS the reader is the AppleScript pasteboard read, as it always was."
  (agent-repl-test-image--with
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (setq agent-repl-test-image--written (list dest))
      (should (equal (agent-repl--image-capture-clipboard dest "ws-one") dest))
      (should (equal (agent-repl-test-image--programs) '("osascript"))))))

(ert-deftest agent-repl-image-linux-x11-reads-through-xclip ()
  "Under X11 the reader is `xclip', asked for the clipboard's PNG target."
  (agent-repl-test-image--on-linux 'x11 '("xclip")
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should (equal (agent-repl--image-capture-clipboard dest "ws-one") dest))
      (should (equal (reverse agent-repl-test-image--to-file-calls)
                     '(("xclip" "-selection" "clipboard" "-t" "image/png" "-o")))))))

(ert-deftest agent-repl-image-linux-wayland-reads-through-wl-paste ()
  "Under Wayland the reader is `wl-paste', asked for the PNG mime type."
  (agent-repl-test-image--on-linux 'wayland '("wl-paste")
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should (equal (agent-repl--image-capture-clipboard dest "ws-one") dest))
      (should (equal (reverse agent-repl-test-image--to-file-calls)
                     '(("wl-paste" "-t" "image/png")))))))

(ert-deftest agent-repl-image-linux-falls-back-to-the-installed-tool ()
  "A Wayland session carrying only `xclip' uses it rather than refusing."
  (agent-repl-test-image--on-linux 'wayland '("xclip")
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should (equal (agent-repl--image-capture-clipboard dest "ws-one") dest))
      (should (equal (agent-repl-test-image--to-file-programs) '("xclip"))))))

(ert-deftest agent-repl-image-linux-without-a-tool-refuses ()
  "No clipboard tool on PATH is a refusal the user sees, never a no-op."
  (agent-repl-test-image--on-linux 'x11 '()
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                    :type 'user-error)
      (should-not agent-repl-test-image--to-file-calls))))

(ert-deftest agent-repl-image-linux-without-a-tool-names-the-tool ()
  "The refusal names the tool to install, so the user can act on it."
  (agent-repl-test-image--on-linux 'x11 '()
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should (string-match-p
               "xclip"
               (cadr (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                                   :type 'user-error)))))))

(ert-deftest agent-repl-image-linux-without-a-tool-is-recorded ()
  "The missing tool also lands on the error record, not only in the echo area."
  (agent-repl-test-image--on-linux 'wayland '()
    (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                    :type 'user-error)
      (should (seq-find (lambda (line) (string-match-p "wl-paste" line))
                        agent-repl-test-image--errors)))))

(ert-deftest agent-repl-image-linux-empty-clipboard-refuses ()
  "A reader that returns nothing is an empty clipboard, and it is reported."
  (agent-repl-test-image--on-linux 'x11 '("xclip")
    (cl-letf (((symbol-function 'agent-repl--image-call-process-to-file)
               (lambda (dest _program &rest _args)
                 (with-temp-file dest (insert ""))
                 1)))
      (let ((dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
        (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                      :type 'user-error)
        (should agent-repl-test-image--errors)))))

(ert-deftest agent-repl-image-unsupported-platform-refuses ()
  "A platform with no reader at all refuses instead of trying AppleScript."
  (agent-repl-test-image--on-linux 'x11 '("xclip")
    (let ((system-type 'windows-nt)
          (dest (expand-file-name "clip.png" agent-repl-test-image--dir)))
      (should-error (agent-repl--image-capture-clipboard dest "ws-one")
                    :type 'user-error)
      (should-not agent-repl-test-image--to-file-calls))))

(ert-deftest agent-repl-image-display-type-prefers-wayland ()
  "With both variables set the compositor wins: a Wayland session is Wayland."
  (let ((process-environment (append '("WAYLAND_DISPLAY=wayland-0" "DISPLAY=:0")
                                     process-environment)))
    (should (eq (agent-repl--image-display-type "ws-one") 'wayland))))

(ert-deftest agent-repl-image-display-type-is-x11-when-headless ()
  "With neither variable set the answer is X11, so a report names `xclip'."
  (let ((process-environment
         (seq-remove (lambda (v) (or (string-prefix-p "DISPLAY=" v)
                                     (string-prefix-p "WAYLAND_DISPLAY=" v)))
                     process-environment)))
    (should (eq (agent-repl--image-display-type "ws-one") 'x11))))

(provide 'test-clipboard-image)

;;; test-clipboard-image.el ends here
