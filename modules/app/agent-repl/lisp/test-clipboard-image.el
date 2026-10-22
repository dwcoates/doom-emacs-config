;;; test-clipboard-image.el --- ERT tests for clipboard-image.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-clipboard-image.el -f ert-run-tests-batch-and-exit
;;
;; Both external boundaries are stubbed per test, so no case reads the real
;; clipboard or runs a program: `agent-repl--image-clipboard-selection' is a
;; clipboard the test scripts (its TARGETS and each type's bytes), and
;; `agent-repl--image-call-process' records `sips' and the image opener.
;;
;; The behavior under test: a paste with an image (and no text) on the
;; clipboard puts ONE marker character at point, and that character is the
;; whole record of the image -- input.el reads it back in its place among
;; the words.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;; Evil is not loaded in the batch harness; its register variable is
;; special in production, so it is special here too.
(defvar evil-this-register)

;;;; ---- Fixtures ----

(defvar agent-repl-test-image--clipboard nil
  "The scripted clipboard: an alist of (TYPE . VALUE), TARGETS included.")

(defvar agent-repl-test-image--calls nil
  "Program invocations, oldest first, as (PROGRAM . ARGS).")

(defvar agent-repl-test-image--exit 0
  "The exit code the stubbed program boundary answers.")

(defvar agent-repl-test-image--errors nil
  "Error records, oldest first, as formatted lines.")

(defvar agent-repl-test-image--gone nil
  "Image paths whose files the current test says are gone.")

(defvar agent-repl-test-image--dir nil
  "The temporary workspace directory for the test.")

(defun agent-repl-test-image--run (program &rest args)
  "Record PROGRAM ARGS; a `sips' run writes its `--out' file when it succeeds."
  (setq agent-repl-test-image--calls
        (append agent-repl-test-image--calls (list (cons program args))))
  (when (and (equal program "sips") (eq 0 agent-repl-test-image--exit))
    (with-temp-file (car (last args)) (insert "converted-png")))
  agent-repl-test-image--exit)

(defun agent-repl-test-image--clip (&rest pairs)
  "Script the clipboard to hold PAIRS, alternating TYPE and VALUE.
TARGETS is derived from the types, as a vector, the way Emacs answers it."
  (let ((alist nil))
    (while pairs
      (push (cons (car pairs) (cadr pairs)) alist)
      (setq pairs (cddr pairs)))
    (setq alist (nreverse alist))
    (setq agent-repl-test-image--clipboard
          (cons (cons 'TARGETS (vconcat (cons 'TARGETS (mapcar #'car alist)))) alist))))

(defmacro agent-repl-test-image--with (&rest body)
  "Run BODY in a live composer buffer with both boundaries stubbed."
  (declare (indent 0))
  `(let* ((agent-repl-test-image--clipboard nil)
          (agent-repl-test-image--calls nil)
          (agent-repl-test-image--exit 0)
          (agent-repl-test-image--errors nil)
          (agent-repl-test-image--gone nil)
          (system-type 'darwin)
          (agent-repl-test-image--dir (make-temp-file "agent-repl-image-test" t))
          (buf (generate-new-buffer " *agent-repl-test-composer*")))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (setq-local agent-repl--owning-workspace "ws-one")
             (agent-repl-input-mode))
           (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                     ((symbol-function 'agent-repl--ws-dir)
                      (lambda (_ws) agent-repl-test-image--dir))
                     ((symbol-function 'agent-repl--ws-get)
                      (lambda (_ws key) (when (eq key :input-buffer) buf)))
                     ((symbol-function 'agent-repl--image-clipboard-selection)
                      (lambda (type) (cdr (assq type agent-repl-test-image--clipboard))))
                     ((symbol-function 'agent-repl--image-call-process)
                      #'agent-repl-test-image--run)
                     ((symbol-function 'agent-repl--error)
                      (lambda (_ws fmt &rest args)
                        (setq agent-repl-test-image--errors
                              (append agent-repl-test-image--errors
                                      (list (apply #'format fmt args))))))
                     ((symbol-function 'display-images-p) (lambda (&rest _) nil))
                     ;; Images name files no test writes: each is present
                     ;; unless a test says its file is gone.
                     ((symbol-function 'agent-repl--input-image-present-p)
                      (lambda (path) (not (member path agent-repl-test-image--gone))))
                     ((symbol-function 'message) (lambda (&rest _) nil)))
             (with-current-buffer buf ,@body)))
       (kill-buffer buf)
       (delete-directory agent-repl-test-image--dir t))))

(defun agent-repl-test-image--images ()
  "Return the images the current composer holds, in order."
  (agent-repl-input-attachments))

(defun agent-repl-test-image--kept (name)
  "Return the absolute path of NAME in the test's image directory."
  (expand-file-name (concat ".claude/emacs/images/" name) agent-repl-test-image--dir))

(defun agent-repl-test-image--kept-name (data extension)
  "Return the file name DATA is kept under, with EXTENSION."
  (format "clip-%s.%s" (substring (secure-hash 'sha256 data) 0 16) extension))

;;;; ---- What the clipboard holds ----

(ert-deftest agent-repl-image-paste-type-is-the-image-when-no-text-rides-with-it ()
  "An image with no text on the clipboard is pasted as the image."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG")
    (should (equal (car (agent-repl--image-clipboard-paste-type "ws-one")) 'image/png))))

(ert-deftest agent-repl-image-paste-type-is-nil-when-text-rides-with-the-image ()
  "Text on the clipboard makes the paste the ordinary one, image or not."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'STRING "report.png" 'image/tiff "TIFF")
    (should (null (agent-repl--image-clipboard-paste-type "ws-one")))))

(ert-deftest agent-repl-image-paste-type-is-nil-with-no-image ()
  "A clipboard holding only text pastes text."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'STRING "words")
    (should (null (agent-repl--image-clipboard-paste-type "ws-one")))))

(ert-deftest agent-repl-image-paste-type-is-nil-on-an-empty-clipboard ()
  "A clipboard answering no TARGETS at all pastes as usual."
  (agent-repl-test-image--with
    (should (null (agent-repl--image-clipboard-paste-type "ws-one")))))

(ert-deftest agent-repl-image-paste-type-prefers-png-over-tiff ()
  "PNG wins over the TIFF a macOS app offers beside it."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/tiff "TIFF" 'image/png "PNG")
    (should (equal (car (agent-repl--image-clipboard-paste-type "ws-one")) 'image/png))))

(ert-deftest agent-repl-image-targets-read-a-list-answer ()
  "A TARGETS answer given as a list is read like the vector."
  (agent-repl-test-image--with
    (setq agent-repl-test-image--clipboard '((TARGETS . (TARGETS image/gif))))
    (should (equal (agent-repl--image-clipboard-targets "ws-one") '(TARGETS image/gif)))))

;;;; ---- Capture ----

(ert-deftest agent-repl-image-capture-keeps-the-bytes-under-their-digest ()
  "The clipboard's bytes are written to a file named by their digest."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG-BYTES")
    (let ((kept (agent-repl--image-capture "ws-one" (assq 'image/png agent-repl--image-clipboard-types))))
      (should (equal kept (cons (agent-repl-test-image--kept
                                 (agent-repl-test-image--kept-name "PNG-BYTES" "png"))
                                "image/png")))
      (should (equal (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally (car kept))
                       (buffer-string))
                     "PNG-BYTES")))))

(ert-deftest agent-repl-image-capture-of-the-same-image-twice-is-one-file ()
  "Pasting the same bytes twice keeps one file."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG-BYTES")
    (let ((entry (assq 'image/png agent-repl--image-clipboard-types)))
      (should (equal (agent-repl--image-capture "ws-one" entry)
                     (agent-repl--image-capture "ws-one" entry)))
      (should (equal (length (directory-files (agent-repl-test-image--kept "") nil "\\`clip-")) 1)))))

(ert-deftest agent-repl-image-capture-keeps-a-jpeg-as-a-jpeg ()
  "A JPEG is kept and attached as `image/jpeg'."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/jpeg "JPEG")
    (should (equal (cdr (agent-repl--image-capture
                         "ws-one" (assq 'image/jpeg agent-repl--image-clipboard-types)))
                   "image/jpeg"))))

(ert-deftest agent-repl-image-capture-converts-a-tiff-with-sips ()
  "A TIFF is converted to PNG and attached as PNG."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/tiff "TIFF")
    (let* ((tiff (agent-repl-test-image--kept (agent-repl-test-image--kept-name "TIFF" "tiff")))
           (png (concat (file-name-sans-extension tiff) ".png")))
      (should (equal (agent-repl--image-capture
                      "ws-one" (assq 'image/tiff agent-repl--image-clipboard-types))
                     (cons png "image/png")))
      (should (equal agent-repl-test-image--calls
                     (list (list "sips" "-s" "format" "png" tiff "--out" png)))))))

(ert-deftest agent-repl-image-capture-refuses-a-failed-tiff-conversion ()
  "A `sips' that fails is a refusal, recorded at error."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/tiff "TIFF")
    (setq agent-repl-test-image--exit 1)
    (should-error (agent-repl--image-capture
                   "ws-one" (assq 'image/tiff agent-repl--image-clipboard-types))
                  :type 'user-error)
    (should (string-match-p "tiff conversion failed .* exit=1"
                            (car agent-repl-test-image--errors)))))

(ert-deftest agent-repl-image-capture-refuses-a-tiff-off-macos ()
  "Off macOS a TIFF cannot be converted: refused, recorded, nothing run."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/tiff "TIFF")
    (let ((system-type 'gnu/linux))
      (should-error (agent-repl--image-capture
                     "ws-one" (assq 'image/tiff agent-repl--image-clipboard-types))
                    :type 'user-error))
    (should (string-match-p "tiff conversion unsupported system-type=gnu/linux"
                            (car agent-repl-test-image--errors)))
    (should (null agent-repl-test-image--calls))))

(ert-deftest agent-repl-image-capture-refuses-a-clipboard-with-no-bytes ()
  "A type that answers no bytes is refused and recorded, and nothing is kept."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "")
    (should-error (agent-repl--image-capture
                   "ws-one" (assq 'image/png agent-repl--image-clipboard-types))
                  :type 'user-error)
    (should (string-match-p "no image bytes target=image/png"
                            (car agent-repl-test-image--errors)))
    (should (null (file-exists-p (agent-repl-test-image--kept ""))))))

(ert-deftest agent-repl-image-write-failure-is-recorded-and-resignalled ()
  "A write that fails is recorded at error with its path, then re-signalled."
  (agent-repl-test-image--with
    (cl-letf (((symbol-function 'agent-repl--write-file-atomically)
               (lambda (&rest _) (signal 'file-error '("Opening output file" "denied")))))
      (should-error (agent-repl--image-write "/nowhere/clip.png" "PNG" "ws-one")
                    :type 'file-error))
    (should (string-match-p "image write failed path=/nowhere/clip.png bytes=3"
                            (car agent-repl-test-image--errors)))))

(ert-deftest agent-repl-image-write-goes-through-the-shared-atomic-writer-as-binary ()
  "The bytes are written by the one atomic writer, unencoded."
  (agent-repl-test-image--with
    (let (written)
      (cl-letf (((symbol-function 'agent-repl--write-file-atomically)
                 (lambda (&rest args) (setq written args))))
        (agent-repl--image-write "/nowhere/clip.png" "PNG" "ws-one"))
      (should (equal written '("/nowhere/clip.png" "PNG" t))))))

;;;; ---- The image in the composer ----

(ert-deftest agent-repl-image-attach-inserts-one-character-at-point ()
  "An attached image is one character, inserted where point is."
  (agent-repl-test-image--with
    (insert "look  here")
    (goto-char 6)
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   (concat "look " agent-repl--image-marker-text " here")))))

(ert-deftest agent-repl-image-attach-records-the-path-and-media-type ()
  "The character carries the image's path and MIME type."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (should (equal (get-text-property (point-min) agent-repl--input-image-marker-property)
                   '(:path "/tmp/a.png" :media-type "image/png")))))

(ert-deftest agent-repl-image-attach-returns-the-character-position ()
  "The entry point answers where the image went."
  (agent-repl-test-image--with
    (insert "ab")
    (should (equal (agent-repl-input-attach-image "/tmp/a.png" "image/png") 3))))

(ert-deftest agent-repl-image-attach-outside-a-composer-is-fatal ()
  "Attaching anywhere but a composer is an invariant break."
  (agent-repl-test-image--with
    (with-temp-buffer
      (should-error (agent-repl-input-attach-image "/tmp/a.png" "image/png")))))

(ert-deftest agent-repl-image-words-typed-after-it-are-not-the-image ()
  "Text typed right after the image is the user's own, not more image."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (insert-and-inherit "x")
    (should (null (get-text-property (1- (point-max)) agent-repl--input-image-marker-property)))))

(defun agent-repl-test-image--drawn ()
  "Draw the whole composer as redisplay would, and return its first character's look."
  (agent-repl--image-decorate (point-min) (point-max))
  (list :display (get-text-property (point-min) 'display)
        :keymap (get-text-property (point-min) 'keymap)
        :help-echo (get-text-property (point-min) 'help-echo)))

(ert-deftest agent-repl-image-carries-only-its-record-until-drawn ()
  "The inserted character holds the record; how it looks is derived when drawn."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (should (null (get-text-property (point-min) 'display)))))

(ert-deftest agent-repl-image-is-drawn-as-copied-image-without-graphics ()
  "A frame that cannot draw images shows the fallback text."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (should (equal (plist-get (agent-repl-test-image--drawn) :display) "[Copied Image]"))))

(ert-deftest agent-repl-image-is-drawn-as-a-fixed-height-thumbnail-with-graphics ()
  "A frame that draws images shows the image scaled to the thumbnail height, width capped."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'create-image)
               (lambda (file &rest props) (append (list 'image :file file) (cddr props)))))
      (should (equal (plist-get (agent-repl-test-image--drawn) :display)
                     (list 'image :file "/tmp/a.png"
                           :height agent-repl-image-thumbnail-height
                           :max-width agent-repl-image-thumbnail-max-width
                           :ascent 'center))))))

(ert-deftest agent-repl-image-a-present-image-opens-on-a-click ()
  "An image whose file exists carries the click that opens it."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (should (eq (plist-get (agent-repl-test-image--drawn) :keymap) agent-repl--image-marker-map))))

(ert-deftest agent-repl-image-a-missing-image-is-drawn-as-no-longer-available ()
  "An image whose file is gone says so, and a click opens nothing."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/gone.png" "image/png")
    (setq agent-repl-test-image--gone '("/tmp/gone.png"))
    (should (equal (agent-repl-test-image--drawn)
                   '(:display "[Image no longer available]" :keymap nil
                     :help-echo "/tmp/gone.png no longer exists")))))

(ert-deftest agent-repl-image-an-image-whose-file-goes-is-redrawn-as-missing ()
  "Drawing again after the file went replaces the image and its click."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (agent-repl-test-image--drawn)
    (setq agent-repl-test-image--gone '("/tmp/a.png"))
    (should (equal (plist-get (agent-repl-test-image--drawn) :keymap) nil))))

(ert-deftest agent-repl-image-every-image-in-the-span-is-drawn ()
  "Two images side by side are each drawn from their own record."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (agent-repl-input-attach-image "/tmp/gone.png" "image/png")
    (setq agent-repl-test-image--gone '("/tmp/gone.png"))
    (agent-repl--image-decorate (point-min) (point-max))
    (should (equal (list (get-text-property 1 'display) (get-text-property 2 'display))
                   '("[Copied Image]" "[Image no longer available]")))))

(ert-deftest agent-repl-image-drawing-leaves-the-composer-unmodified ()
  "Drawing is not editing: the buffer's modified flag is untouched."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (set-buffer-modified-p nil)
    (agent-repl-test-image--drawn)
    (should-not (buffer-modified-p))))

(ert-deftest agent-repl-image-a-composer-draws-its-images-through-jit-lock ()
  "Every composer registers the drawing, so redisplay derives each image's look."
  (agent-repl-test-image--with
    (should (memq #'agent-repl--image-decorate jit-lock-functions))))

(defconst agent-repl-test-image--vocab
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (path (expand-file-name "../proto/vocab/image-thumbnail.json" dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((json-object-type 'alist)
            (json-key-type 'string))
        (json-read))))
  "The checked-in cross-language look of an image, decoded.")

(ert-deftest agent-repl-image-thumbnail-height-is-the-shared-height ()
  "Images are drawn at the feed's thumbnail height (image-thumbnail.json)."
  (should (equal (default-value 'agent-repl-image-thumbnail-height)
                 (cdr (assoc "height_px" agent-repl-test-image--vocab)))))

(ert-deftest agent-repl-image-missing-display-says-the-shared-words ()
  "A lost image says what the feed says (image-thumbnail.json), bracketed."
  (should (equal agent-repl--image-missing-display
                 (format "[%s]" (cdr (assoc "missing_text" agent-repl-test-image--vocab))))))

(ert-deftest agent-repl-image-is-drawn-again-after-kill-and-yank ()
  "A killed and yanked image keeps its record, and drawing gives it back its click."
  (agent-repl-test-image--with
    (agent-repl-input-attach-image "/tmp/a.png" "image/png")
    (kill-region (point-min) (point-max))
    (insert-for-yank (current-kill 0))
    (should (eq (plist-get (agent-repl-test-image--drawn) :keymap) agent-repl--image-marker-map))
    (should (equal (agent-repl-test-image--images)
                   '((:path "/tmp/a.png" :media-type "image/png"))))))

;;;; ---- Opening ----

(defconst agent-repl-test-image--png '(:path "/tmp/a.png" :media-type "image/png")
  "A composer image the tests open.")

(ert-deftest agent-repl-image-open-asks-the-daemon-to-show-it-over-the-feed ()
  "Opening a present image hands it, whole, to the composer's OpenFeedImage send."
  (agent-repl-test-image--with
    (let (sent)
      (cl-letf (((symbol-function 'agent-repl--input-open-feed-image)
                 (lambda (ws image) (setq sent (list ws image)))))
        (agent-repl--image-open agent-repl-test-image--png "ws-one"))
      (should (equal sent (list "ws-one" agent-repl-test-image--png))))))

(ert-deftest agent-repl-image-open-runs-no-program ()
  "The image opens in the webapp: no viewer process is run."
  (agent-repl-test-image--with
    (cl-letf (((symbol-function 'agent-repl--input-open-feed-image) #'ignore))
      (agent-repl--image-open agent-repl-test-image--png "ws-one"))
    (should (null agent-repl-test-image--calls))))

(ert-deftest agent-repl-image-open-refuses-a-missing-file ()
  "A file that is gone is refused, recorded below error, and the daemon is not asked."
  (agent-repl-test-image--with
    (setq agent-repl-test-image--gone '("/tmp/a.png"))
    (let (sent)
      (cl-letf (((symbol-function 'agent-repl--input-open-feed-image)
                 (lambda (&rest args) (setq sent args))))
        (should-error (agent-repl--image-open agent-repl-test-image--png "ws-one") :type 'user-error))
      (should (equal (list sent agent-repl-test-image--errors) '(nil nil))))))

(ert-deftest agent-repl-image-click-opens-the-clicked-image ()
  "A click on the image opens that image, path and MIME type."
  (agent-repl-test-image--with
    (let ((opened nil)
          (window-buffer (current-buffer)))
      (agent-repl-input-attach-image "/tmp/a.png" "image/png")
      (cl-letf (((symbol-function 'posn-window) (lambda (_) 'window))
                ((symbol-function 'window-buffer) (lambda (_) window-buffer))
                ((symbol-function 'posn-point) (lambda (_) 1))
                ((symbol-function 'agent-repl--image-open)
                 (lambda (image _ws) (setq opened image))))
        (agent-repl-input-open-image-at-mouse '(mouse-1 (window 1))))
      (should (equal opened agent-repl-test-image--png)))))

;;;; ---- Pasting ----

(ert-deftest agent-repl-image-yank-pastes-the-clipboard-image-at-point ()
  "`yank' with an image on the clipboard puts the image where point is."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG")
    (insert "see ")
    (agent-repl-input-yank)
    (should (equal (agent-repl-test-image--images)
                   (list (list :path (agent-repl-test-image--kept
                                      (agent-repl-test-image--kept-name "PNG" "png"))
                               :media-type "image/png"))))
    (should (equal (get-text-property 5 agent-repl--input-image-marker-property)
                   (car (agent-repl-test-image--images))))))

(ert-deftest agent-repl-image-yank-with-text-on-the-clipboard-yanks ()
  "`yank' with text on the clipboard is the ordinary yank."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'STRING "words")
    (let ((yanked nil))
      (cl-letf (((symbol-function 'yank) (lambda (&optional _) (interactive) (setq yanked t))))
        (agent-repl-input-yank))
      (should yanked)
      (should (null (agent-repl-test-image--images))))))

(ert-deftest agent-repl-image-yank-with-a-prefix-yanks-an-older-kill ()
  "A prefix argument asks for an older kill, so the clipboard image is not pasted."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG")
    (let ((yanked nil)
          (current-prefix-arg 2))
      (cl-letf (((symbol-function 'yank) (lambda (&optional _) (interactive) (setq yanked t))))
        (agent-repl-input-yank))
      (should yanked)
      (should (null (agent-repl-test-image--images))))))

(ert-deftest agent-repl-image-evil-paste-after-goes-after-the-cursor ()
  "Evil's `p' puts the image after the character under the cursor."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG")
    (insert "ab")
    (goto-char 1)
    (let ((evil-this-register nil))
      (agent-repl-input-evil-paste-after))
    (should (get-text-property 2 agent-repl--input-image-marker-property))))

(ert-deftest agent-repl-image-evil-paste-before-goes-at-the-cursor ()
  "Evil's `P' puts the image before the character under the cursor."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG")
    (insert "ab")
    (goto-char 1)
    (let ((evil-this-register nil))
      (agent-repl-input-evil-paste-before))
    (should (get-text-property 1 agent-repl--input-image-marker-property))))

(ert-deftest agent-repl-image-evil-paste-from-a-named-register-pastes-the-register ()
  "A named register holds what the user put in it, so evil pastes it."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'image/png "PNG")
    (let ((evil-this-register ?a)
          (pasted nil))
      (cl-letf (((symbol-function 'evil-paste-after)
                 (lambda (&rest _) (interactive) (setq pasted t))))
        (agent-repl-input-evil-paste-after))
      (should pasted)
      (should (null (agent-repl-test-image--images))))))

(ert-deftest agent-repl-image-attach-command-takes-the-image-beside-text ()
  "`C-c C-i' attaches the image even when text rides beside it."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'STRING "icon.png" 'image/png "PNG")
    (agent-repl-attach-clipboard-image)
    (should (equal (length (agent-repl-test-image--images)) 1))))

(ert-deftest agent-repl-image-attach-command-refuses-a-clipboard-with-no-image ()
  "`C-c C-i' with no image on the clipboard is refused, and nothing is attached."
  (agent-repl-test-image--with
    (agent-repl-test-image--clip 'STRING "words")
    (should-error (agent-repl-attach-clipboard-image) :type 'user-error)
    (should (null (agent-repl-test-image--images)))))

(ert-deftest agent-repl-image-paste-commands-are-remapped-in-the-composer ()
  "Every paste command the composer has runs the image-aware paste."
  (should (equal (list (lookup-key agent-repl-input-mode-map [remap yank])
                       (lookup-key agent-repl-input-mode-map [remap clipboard-yank])
                       (lookup-key agent-repl-input-mode-map [remap evil-paste-after])
                       (lookup-key agent-repl-input-mode-map [remap evil-paste-before]))
                 '(agent-repl-input-yank
                   agent-repl-input-yank
                   agent-repl-input-evil-paste-after
                   agent-repl-input-evil-paste-before))))

(ert-deftest agent-repl-image-boundaries-are-registered ()
  "Both external boundaries are registered, so the harness guards them."
  (should (memq 'agent-repl--image-call-process agent-repl--external-boundary-functions))
  (should (memq 'agent-repl--image-clipboard-selection agent-repl--external-boundary-functions)))

(provide 'test-clipboard-image)

;;; test-clipboard-image.el ends here
