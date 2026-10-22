;;; clipboard-image.el --- images in the composer, pasted from the clipboard -*- lexical-binding: t; -*-

;;; Commentary:

;; PASTING AN IMAGE IS PASTING.  When the system clipboard holds an image
;; and no text, the composer's ordinary paste commands (`yank', `C-y',
;; `s-v', evil's `p' and `P') put the IMAGE at point instead of text: a
;; screenshot (Cmd-Ctrl-Shift-4), an image copied in a browser, or a
;; picture copied from Preview all paste the way words do.  With text on
;; the clipboard the paste is the ordinary one.  `C-c C-i' attaches the
;; clipboard's image even when text rides beside it.
;;
;; AN IMAGE IS ONE CHARACTER OF THE COMPOSER'S TEXT.  It is inserted where
;; the paste happened, drawn as the image itself (a thumbnail
;; `agent-repl-image-thumbnail-height' pixels tall; a frame that cannot
;; draw images shows "[Copied Image]"), and clicking it opens it large over
;; the feed, in the webapp (`OpenFeedImage').  It carries
;; `agent-repl--input-image-marker-property' (input.el), whose value names
;; the file and its MIME type, and that property is the whole record:
;; input.el reads the buffer in order, so words and images are submitted
;; as the `UserSaid' blocks they stand as, and deleting, killing or
;; yanking the character detaches, removes or moves the image.  One
;; character, so one keystroke deletes it.
;;
;; THE CLIPBOARD IS READ THROUGH EMACS'S OWN SELECTION API, on every
;; platform: `gui-get-selection' names the clipboard's types (TARGETS) and
;; hands back an image type's bytes.  The bytes are written into the
;; workspace's image directory under a name derived from their digest, so
;; pasting the same image twice is one file.  A TIFF-only clipboard (some
;; macOS apps) is converted to PNG with `sips', because the vendor takes
;; PNG, JPEG, GIF and WebP only; elsewhere a TIFF-only clipboard is a
;; reported refusal.
;;
;; Every shell-out and every clipboard read goes through an external-
;; boundary wrapper -- `agent-repl--image-call-process' and
;; `agent-repl--image-clipboard-selection' -- each registered in
;; `agent-repl--external-boundary-functions' (core.el) so the batch test
;; harness stubs the boundary instead of reaching the host.

;;; Code:

(require 'image)
(require 'seq)
(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--fatal "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--input-image-present-p "agent-repl-input" (path))
(declare-function agent-repl--input-open-feed-image "agent-repl-input" (ws image))
(declare-function agent-repl--write-file-atomically "agent-repl-core" (file text &optional binary))
(declare-function agent-repl-refill-keymap "agent-repl-core" (map fill))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))
(declare-function evil-paste-after "evil-commands")
(declare-function evil-paste-before "evil-commands")
(defvar agent-repl-input-mode-map)
(defvar evil-this-register)
;; Defined by input.el, which loads first; the marker this file inserts
;; and the reader that turns it into an `ImageBlock' name the SAME
;; property, from one declaration.
(defvar agent-repl--input-image-marker-property)

(defcustom agent-repl-image-thumbnail-height 120
  "Pixel height every image in the composer is drawn at, large or small.
The same height the feed draws images at: both sides are asserted against
proto/vocab/image-thumbnail.json, so an image is one size wherever it is
shown.
Affects display only; the agent is sent the image file itself."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-image-thumbnail-max-width 480
  "Pixel width no image in the composer is drawn wider than.
A very wide image is scaled down to it, keeping its aspect ratio, rather
than drawn at the full thumbnail height."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl--image-subdir ".claude/emacs/images/"
  "Workspace-relative directory pasted images are written to.
Under `.claude/emacs/', which is the workspace's own Emacs-side data
directory: the file lives and dies with the workspace that pasted it.")

(defconst agent-repl--image-marker-text "\ufffc"
  "The one character that stands for an image in the composer's text.
U+FFFC OBJECT REPLACEMENT CHARACTER: what it is is said by the
`agent-repl--input-image-marker-property' it carries, and what is drawn
is said by its `display'.")

(defconst agent-repl--image-fallback-display "[Copied Image]"
  "What an image is drawn as in a frame that cannot draw images.")

(defconst agent-repl--image-missing-display "[Image no longer available]"
  "What an image whose file is gone is drawn as (its worktree was deleted).
The feed says the same words in the same place: both sides are asserted
against proto/vocab/image-thumbnail.json.")

;;;; ---- External boundary ----------------------------------------------------

(defun agent-repl--image-call-process (program &rest args)
  "Run PROGRAM with ARGS synchronously; return its exit code (output discarded).
An external-boundary wrapper for `sips';
registered in `agent-repl--external-boundary-functions' (core.el) so
tests stub it via `cl-letf' rather than shelling out."
  (apply #'call-process program nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--image-clipboard-selection (type)
  "Return the system clipboard's TYPE selection, or nil when it has none.
TYPE `TARGETS' answers the vector of types the clipboard holds; an image
MIME type such as `image/png' answers that image's bytes as a unibyte
string.  The clipboard is external state, so this is a boundary of its
own and is registered in `agent-repl--external-boundary-functions'
\(core.el): a test decides what the clipboard holds."
  (gui-get-selection 'CLIPBOARD type)) ;; ALLOW-EXTERNAL-BOUNDARY

;;;; ---- What the clipboard holds ---------------------------------------------

(defconst agent-repl--image-clipboard-types
  '((image/png "png" "image/png")
    (image/jpeg "jpg" "image/jpeg")
    (image/gif "gif" "image/gif")
    (image/webp "webp" "image/webp")
    (image/tiff "tiff" "image/png"))
  "The clipboard image types a paste takes, in preference order.
Each entry is (TARGET EXTENSION ATTACHED-MEDIA-TYPE).  The first four are
attached as they are; `image/tiff' is the last resort and is converted
to PNG (`agent-repl--image-convert-tiff'), so it is attached as PNG.")

(defconst agent-repl--image-text-targets
  '(STRING UTF8_STRING TEXT COMPOUND_TEXT text/plain text/plain\;charset=utf-8)
  "Clipboard types that mean the clipboard holds TEXT.
Text on the clipboard makes a paste the ordinary text paste even when an
image rides beside it (a file copied in Finder carries its name AND its
icon), because the words are what the user copied.")

(defun agent-repl--image-clipboard-targets (&optional ws)
  "Return the list of types the clipboard holds, as symbols."
  (let* ((targets (agent-repl--image-clipboard-selection 'TARGETS))
         (listed (cond ((vectorp targets) (append targets nil))
                       ((listp targets) targets)
                       (t nil))))
    (agent-repl--log ws "clipboard-image: clipboard targets=%S" listed)
    listed))

(defun agent-repl--image-clipboard-image-type (targets)
  "Return the entry of `agent-repl--image-clipboard-types' TARGETS offers, or nil."
  (seq-find (lambda (entry) (memq (car entry) targets))
            agent-repl--image-clipboard-types))

(defun agent-repl--image-clipboard-paste-type (&optional ws)
  "Return the image type a paste in WS's composer should take, or nil.
Nil when the clipboard holds text, or holds no image this file takes;
the paste is then the ordinary one."
  (let* ((targets (agent-repl--image-clipboard-targets ws))
         (text (seq-some (lambda (target) (memq target targets))
                         agent-repl--image-text-targets))
         (image (agent-repl--image-clipboard-image-type targets))
         (paste (and image (not text) image)))
    (agent-repl--log ws "clipboard-image: paste decision text=%s image=%s paste=%s"
                     (and text t) (car image) (car paste))
    paste))

;;;; ---- Capture --------------------------------------------------------------

(defun agent-repl--image-dir (ws)
  "Return (creating) the pasted-image directory for workspace WS."
  (let* ((dir (expand-file-name agent-repl--image-subdir (agent-repl--ws-dir ws)))
         (already-exists (file-directory-p dir)))
    (agent-repl--log ws
                     "clipboard-image: ensure image directory path=%s already-exists=%s"
                     dir already-exists)
    (make-directory dir t)
    dir))

(defun agent-repl--image-path (dir data extension)
  "Return the path under DIR that image bytes DATA are kept at.
Named by the bytes' digest, so the same image pasted twice is one file
and a name never stands for two different images."
  (expand-file-name (format "clip-%s.%s" (substring (secure-hash 'sha256 data) 0 16) extension)
                    dir))

(defun agent-repl--image-write (path data &optional ws)
  "Write image bytes DATA to PATH, unless PATH already holds them.
The write is ATOMIC, and that is what makes skipping an existing PATH
sound: a file under a digest's name is only ever the whole image.  A
failed write is recorded at error with its cause and re-signalled."
  (if (file-exists-p path)
      (agent-repl--log ws "clipboard-image: image already kept path=%s" path)
    (condition-case err
        (agent-repl--write-file-atomically path data t)
      (error
       (agent-repl--error ws "clipboard-image: image write failed path=%s bytes=%d err=%S"
                          path (length data) err)
       (signal (car err) (cdr err))))
    (agent-repl--log ws "clipboard-image: image written path=%s bytes=%d" path (length data))))

(defun agent-repl--image-convert-tiff (tiff &optional ws)
  "Convert the TIFF file at path TIFF to PNG beside it; return the PNG's path.
macOS only, through `sips'; anywhere else, and on a failed conversion,
the refusal is recorded at error and signalled as a `user-error'."
  (let ((png (concat (file-name-sans-extension tiff) ".png")))
    (unless (eq system-type 'darwin)
      (agent-repl--error ws "clipboard-image: tiff conversion unsupported system-type=%s tiff=%s"
                         system-type tiff)
      (user-error "agent-repl: the clipboard image is a TIFF, which cannot be converted on %s"
                  system-type))
    (if (file-exists-p png)
        (agent-repl--log ws "clipboard-image: converted image already kept path=%s" png)
      (let ((exit (agent-repl--image-call-process "sips" "-s" "format" "png" tiff "--out" png)))
        (agent-repl--log ws "clipboard-image: tiff conversion tiff=%s png=%s exit=%s" tiff png exit)
        (unless (and (eq 0 exit) (file-exists-p png))
          (agent-repl--error ws "clipboard-image: tiff conversion failed tiff=%s png=%s exit=%s"
                             tiff png exit)
          (user-error "agent-repl: the clipboard image could not be converted to PNG"))))
    png))

(defun agent-repl--image-capture (ws entry)
  "Keep the clipboard's ENTRY image for WS; return (PATH . MEDIA-TYPE).
ENTRY is one of `agent-repl--image-clipboard-types'.  A clipboard that
answers no bytes for it is recorded at error and signalled as a
`user-error' -- an empty attachment would be a silent no-op the user
could not tell from a successful one."
  (pcase-let* ((`(,target ,extension ,media-type) entry)
               (data (agent-repl--image-clipboard-selection target)))
    (unless (and (stringp data) (> (length data) 0))
      (agent-repl--error ws "clipboard-image: clipboard answered no image bytes target=%s" target)
      (user-error "agent-repl: no image found on the clipboard"))
    (let* ((path (agent-repl--image-path (agent-repl--image-dir ws) data extension)))
      (agent-repl--image-write path data ws)
      (cons (if (eq target 'image/tiff) (agent-repl--image-convert-tiff path ws) path)
            media-type))))

;;;; ---- The image in the composer ------------------------------------------

(defun agent-repl--image-display (path &optional ws)
  "Return the `display' an image at PATH is drawn with in the composer.
The image itself, scaled to `agent-repl-image-thumbnail-height' pixels
tall (up or down) and no wider than `agent-repl-image-thumbnail-max-width',
when this frame draws images; otherwise `agent-repl--image-fallback-display'."
  (let* ((drawable (display-images-p))
         (image (and drawable
                     (create-image path nil nil
                                   :height agent-repl-image-thumbnail-height
                                   :max-width agent-repl-image-thumbnail-max-width
                                   :ascent 'center))))
    (agent-repl--log ws "clipboard-image: display path=%s drawable=%s image=%s"
                     path drawable (and image t))
    (or image agent-repl--image-fallback-display)))

(defvar agent-repl--image-marker-map (make-sparse-keymap)
  "Keymap on an image in the composer: a click opens it.")

(agent-repl-refill-keymap
 agent-repl--image-marker-map
 (lambda (map) (define-key map [mouse-1] #'agent-repl-input-open-image-at-mouse)))

(defun agent-repl--image-marker (path media-type)
  "Return the one-character composer string that stands for the image at PATH.
See `agent-repl--image-marker-text'.  It carries the image's record and
nothing drawn: how it looks and what a click does are DERIVED from that
record each time it is drawn (`agent-repl--image-decorate'), so an image
recalled from history or yanked back is drawn from what is true now.
`rear-nonsticky' keeps the words typed after it the user's own."
  (propertize agent-repl--image-marker-text
              agent-repl--input-image-marker-property (list :path path :media-type media-type)
              'rear-nonsticky t))

(defun agent-repl--image-decorate (start end)
  "Draw every image character between START and END from its record.
A `jit-lock' function, so it runs whenever that text is about to be
drawn after a change -- an insertion, a yank, a history recall.  An
image whose file exists is drawn as the image and opens on a click; one
whose file is gone is drawn as `agent-repl--image-missing-display' and
opens nothing."
  (let ((ws (agent-repl--buffer-owner (current-buffer)))
        (pos start))
    (with-silent-modifications
      (while (< pos end)
        (let ((image (get-text-property pos agent-repl--input-image-marker-property))
              (next (min end (next-single-property-change
                              pos agent-repl--input-image-marker-property nil end))))
          (when image
            (dotimes (i (- next pos))
              (let* ((at (+ pos i))
                     (image (get-text-property at agent-repl--input-image-marker-property))
                     (path (plist-get image :path)))
                (if (agent-repl--input-image-present-p path)
                    (add-text-properties
                     at (1+ at)
                     (list 'display (agent-repl--image-display path ws)
                           'keymap agent-repl--image-marker-map
                           'mouse-face 'highlight
                           'help-echo (format "mouse-1: show %s over the feed" path)))
                  (agent-repl--log ws "clipboard-image: drawn as no longer available path=%s" path)
                  (remove-list-of-text-properties at (1+ at) '(keymap mouse-face))
                  (add-text-properties
                   at (1+ at)
                   (list 'display agent-repl--image-missing-display
                         'help-echo (format "%s no longer exists" path)))))))
          (setq pos next))))))

(defun agent-repl--image-enable-decoration ()
  "Have this composer draw its images (`agent-repl--image-decorate')."
  (jit-lock-register #'agent-repl--image-decorate))

(add-hook 'agent-repl-input-mode-hook #'agent-repl--image-enable-decoration)
;; A reload reaches composers that already exist, which the mode hook
;; never will again.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-repl-input-mode)
      (agent-repl--image-enable-decoration))))

;;;###autoload
(defun agent-repl-input-attach-image (path media-type)
  "Put the image at PATH (MIME MEDIA-TYPE) into THIS composer at point.
The ONE entry point that attaches an image: a paste, `C-c C-i' and a
rollback's refill all come through here, in the CURRENT buffer, which
must be a composer.  Returns the image character's position.

The file is NOT read here: an `ImageBlock' carries a REFERENCE, and the
daemon and the shim run on this same host, so the path is the whole
payload."
  (unless (derived-mode-p 'agent-repl-input-mode)
    (agent-repl--fatal '(:agent-repl-central "the attachment request has no composer workspace")
                       "elisp.input.attach-outside-composer buffer=%s path=%s"
                       (buffer-name) path))
  (let ((ws (agent-repl--buffer-owner (current-buffer)))
        (at (point)))
    (insert (agent-repl--image-marker path media-type))
    (agent-repl--info ws "elisp.input.attached ws=%s path=%s media-type=%s at=%d"
                      ws path media-type at)
    at))

(defun agent-repl--image-open (image ws)
  "Open IMAGE, a composer image (:path :media-type), large over WS's feed.
The webapp draws it over the feed's rows (`agent-repl--input-open-feed-image').
A file that is gone (its worktree was deleted) is recorded at info and
signalled as a `user-error'; nothing is asked of the daemon."
  (let ((path (plist-get image :path)))
    (unless (agent-repl--input-image-present-p path)
      (agent-repl--info ws "clipboard-image: open refused, file missing path=%s" path)
      (user-error "agent-repl: the image %s no longer exists" path))
    (agent-repl--log ws "clipboard-image: opening over the feed path=%s" path)
    (agent-repl--input-open-feed-image ws image)))

(defun agent-repl-input-open-image-at-mouse (event)
  "Open the composer image clicked in EVENT large over the workspace's feed."
  (interactive "e")
  (let* ((position (event-start event))
         (buffer (window-buffer (posn-window position)))
         (image (with-current-buffer buffer
                  (get-text-property (posn-point position)
                                     agent-repl--input-image-marker-property)))
         (ws (agent-repl--buffer-owner buffer)))
    (unless image
      (agent-repl--fatal ws "clipboard-image: a click opened no image buffer=%s point=%s"
                         (buffer-name buffer) (posn-point position)))
    (agent-repl--image-open image ws)))

;;;; ---- Pasting --------------------------------------------------------------

(defun agent-repl--image-attach-clipboard (ws entry)
  "Keep the clipboard's ENTRY image for WS and attach it at point; return its path."
  (pcase-let ((`(,path . ,media-type) (agent-repl--image-capture ws entry)))
    (agent-repl-input-attach-image path media-type)
    (agent-repl--info ws "clipboard-image: pasted ws=%s path=%s media-type=%s"
                      ws path media-type)
    path))

;;;###autoload
(defun agent-repl-attach-clipboard-image ()
  "Attach the clipboard's image at point, even when text rides beside it.
Signals a `user-error' when the clipboard holds no image."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (entry (agent-repl--image-clipboard-image-type (agent-repl--image-clipboard-targets ws))))
    (unless entry
      (agent-repl--log ws "clipboard-image: attach refused, no image on the clipboard")
      (user-error "agent-repl: no image found on the clipboard"))
    (agent-repl--image-attach-clipboard ws entry)))

(defun agent-repl-input-yank ()
  "Paste the clipboard's image at point, or `yank' as usual.
The image is pasted when the clipboard holds one and no text, and no
prefix argument asks for an older kill."
  (interactive "*")
  (let* ((ws (agent-repl--ws-current-name))
         (entry (and (null current-prefix-arg) (agent-repl--image-clipboard-paste-type ws))))
    (if entry
        (agent-repl--image-attach-clipboard ws entry)
      (setq this-command 'yank)
      (call-interactively #'yank))))

(defun agent-repl--image-evil-paste (command after)
  "Paste the clipboard's image for evil's COMMAND, or run COMMAND as usual.
AFTER says the image goes after the cursor's character, as `p' pastes.
The image is pasted only for the unnamed or a clipboard register: a named
register holds what the user put in it."
  (let* ((ws (agent-repl--ws-current-name))
         (register (bound-and-true-p evil-this-register))
         (entry (and (memq register '(nil ?+ ?*))
                     (agent-repl--image-clipboard-paste-type ws))))
    (if (null entry)
        (progn
          (setq this-command command)
          (call-interactively command))
      (when (and after (not (eolp))) (forward-char))
      (agent-repl--image-attach-clipboard ws entry))))

(defun agent-repl-input-evil-paste-after ()
  "Evil's `p' in the composer: the clipboard's image, or `evil-paste-after'."
  (interactive "*")
  (agent-repl--image-evil-paste #'evil-paste-after t))

(defun agent-repl-input-evil-paste-before ()
  "Evil's `P' in the composer: the clipboard's image, or `evil-paste-before'."
  (interactive "*")
  (agent-repl--image-evil-paste #'evil-paste-before nil))

;;;; ---- Keybinding -----------------------------------------------------------

;; Bound into the input mode map (defined in input.el, loaded first).  The
;; remaps make every paste key the composer already has paste images too.
(define-key agent-repl-input-mode-map [remap yank] #'agent-repl-input-yank)
(define-key agent-repl-input-mode-map [remap clipboard-yank] #'agent-repl-input-yank)
(define-key agent-repl-input-mode-map [remap evil-paste-after] #'agent-repl-input-evil-paste-after)
(define-key agent-repl-input-mode-map [remap evil-paste-before] #'agent-repl-input-evil-paste-before)
;; `map!' is a no-op stub in the batch harness, so this binding is not test-covered.
(map! :map agent-repl-input-mode-map
      :ni "C-c C-i" #'agent-repl-attach-clipboard-image)

(provide 'clipboard-image)

;;; clipboard-image.el ends here
