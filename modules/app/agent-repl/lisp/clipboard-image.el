;;; clipboard-image.el --- attach clipboard images to the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; Capture the system clipboard image into the workspace and ATTACH it to
;; the current composer, overlaid with a thumbnail.  This covers the macOS
;; screenshot-to-clipboard gesture (Cmd-Ctrl-Shift-4), which puts RAW
;; image bytes on the pasteboard (a `«class PNGf»' and usually a
;; `«class TIFF»' flavor) rather than a file on disk.
;;
;; THE IMAGE IS AN ATTACHMENT, NOT TEXT.  The captured file is registered
;; on the input buffer through `agent-repl-input-attach-image', and the
;; composer turns each attachment into one `ImageBlock{path, media_type}'
;; beside the text block of the `UserSaid' it submits.  Nothing is
;; inserted into the buffer text: a path token would have travelled as
;; WORDS and left the agent to guess that they named an image, where the
;; content model states it by ARM.
;;
;; The thumbnail is still drawn -- as an overlay on a marker line the
;; capture inserts -- so the user can see what they attached.  That line
;; is stripped from the composer along with everything else the moment the
;; daemon accepts the submission.
;;
;; AND IT IS NEVER SUBMITTED.  The marker span carries
;; `agent-repl--input-image-marker-property' (declared by input.el), and
;; `agent-repl--read-input-buffer' drops every span carrying it, so the
;; words that reach the agent are the words the user typed.  Without that
;; the marker line travelled INSIDE the text block of the `UserSaid' --
;; exactly the "path token as WORDS" this design refuses.
;;
;; Capture strategy is chosen BY PLATFORM at the source, never assumed.
;;
;; macOS: an AppleScript writes the clipboard's PNG flavor straight to a
;; file; when only a TIFF flavor is present, the TIFF is written and then
;; converted to PNG with `sips'.  The MIME type is read from the FLAVOR
;; that won, never sniffed from the extension.
;;
;; Linux: the clipboard is read by the tool that matches the display
;; server -- `wl-paste' under Wayland, `xclip' under X11 -- with the other
;; accepted as a second choice when the first is not installed, and the
;; PNG bytes are piped straight into the destination file.  A host with
;; NEITHER tool is a REPORTED failure that names the tool it wanted: an
;; `agent-repl--error' record plus a `user-error' the user sees.  It is
;; never a silent no-op, and it is never a macOS-only path that simply
;; fails on every other host.
;;
;; Every shell-out and every PATH lookup goes through an external-boundary
;; wrapper -- `agent-repl--image-call-process',
;; `agent-repl--image-call-process-to-file' and
;; `agent-repl--image-executable-find' -- each registered in
;; `agent-repl--external-boundary-functions' (core.el) so the batch test
;; harness stubs the boundary instead of shelling out.

;;; Code:

(require 'image)
(require 'seq)
(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))
(declare-function agent-repl-input-attach-image "agent-repl-input" (path media-type))
(declare-function agent-repl--input-buffer "agent-repl-input" (ws))
(defvar agent-repl-input-mode-map)
;; Defined by input.el, which loads first; the marker span and the strip
;; that removes it name the SAME property, from one declaration.
(defvar agent-repl--input-image-marker-property)

(defcustom agent-repl-image-thumbnail-max-height 220
  "Max pixel height of the thumbnail overlaid on an attached image path.
Affects display only; the text sent to the agent is always the file path."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl--image-subdir ".claude/emacs/images/"
  "Workspace-relative directory captured clipboard images are written to.
Under `.claude/emacs/' so the file sits inside the workspace the agent
runs in (its Read tool resolves the path without a cross-project prompt).")

;;;; ---- External boundary ----------------------------------------------------

(defun agent-repl--image-call-process (program &rest args)
  "Run PROGRAM with ARGS synchronously; return its exit code (output discarded).
An external-boundary wrapper for clipboard-image capture; registered in
`agent-repl--external-boundary-functions' (core.el) so tests stub it via
`cl-letf' rather than shelling out to `osascript'/`sips'."
  (apply #'call-process program nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--image-call-process-to-file (dest program &rest args)
  "Run PROGRAM with ARGS, writing its STDOUT to DEST; return its exit code.
The Linux readers (`wl-paste', `xclip') emit the clipboard's PNG bytes on
stdout rather than writing a file themselves, so this boundary exists to
land those bytes.  Registered in `agent-repl--external-boundary-functions'
(core.el) so tests stub it rather than shelling out."
  (apply #'call-process program nil (list :file dest) nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--image-executable-find (program)
  "Return the absolute path of PROGRAM on PATH, or nil when it is absent.
A PATH lookup is external state, so it is a boundary of its own and is
registered in `agent-repl--external-boundary-functions' (core.el): a test
decides which clipboard tools the host has rather than inheriting the
host's real PATH."
  (executable-find program)) ;; ALLOW-EXTERNAL-BOUNDARY

;;;; ---- Capture --------------------------------------------------------------

(defun agent-repl--image-dir (ws)
  "Return (creating) the captured-image directory for workspace WS."
  (let* ((dir (expand-file-name agent-repl--image-subdir (agent-repl--ws-dir ws)))
         (already-exists (file-directory-p dir)))
    (agent-repl--log ws
                     "clipboard-image: ensure image directory path=%s already-exists=%s"
                     dir already-exists)
    (make-directory dir t)
    (agent-repl--log ws "clipboard-image: image directory ready path=%s" dir)
    dir))

(defun agent-repl--image-new-path (dir &optional ws)
  "Return a fresh, non-created PNG path under DIR."
  (let ((path (expand-file-name
               (format "clip-%s-%04x.png" (format-time-string "%Y%m%d-%H%M%S")
                       (random #x10000))
               dir)))
    (agent-repl--log ws "clipboard-image: allocated capture destination dir=%s path=%s"
                     dir path)
    path))

(defun agent-repl--image-nonempty-file-p (path &optional ws)
  "Return non-nil when PATH exists and holds at least one byte."
  (let* ((exists (file-exists-p path))
         (size (and exists (file-attribute-size (file-attributes path))))
         (nonempty (and size (> size 0))))
    (agent-repl--log ws
                     "clipboard-image: inspected captured file path=%s exists=%s size=%s nonempty=%s"
                     path exists size nonempty)
    nonempty))

(defun agent-repl--image-write-flavor (as-class dest &optional ws)
  "Write the clipboard's AS-CLASS flavor to DEST via AppleScript.
AS-CLASS is an AppleScript class literal such as \"«class PNGf»\".  Return
non-nil only when the write leaves a non-empty DEST."
  (let ((script (concat
                 (format "set theData to (the clipboard as %s)\n" as-class)
                 (format "set fp to open for access (POSIX file %S) with write permission\n"
                         (expand-file-name dest))
                 "set eof fp to 0\n"
                 "write theData to fp\n"
                 "close access fp")))
    (agent-repl--log ws "clipboard-image: writing clipboard flavor=%s destination=%s"
                     as-class dest)
    (let* ((exit-code (agent-repl--image-call-process "osascript" "-e" script))
           (nonempty (and (eq 0 exit-code)
                          (agent-repl--image-nonempty-file-p dest ws)))
           (written (and (eq 0 exit-code) nonempty)))
      (agent-repl--log ws
                       "clipboard-image: clipboard flavor write finished flavor=%s destination=%s exit=%s nonempty=%s written=%s"
                       as-class dest exit-code nonempty written)
      written)))

(defconst agent-repl--image-media-type "image/png"
  "The MIME type every capture produces.
Both flavors land as PNG: the PNG flavor is written straight out, and the
TIFF flavor is converted by `sips' before it is attached.  The type is
therefore stated by the capture, never sniffed from the extension by a
later reader.")

(defconst agent-repl--image-linux-readers
  '((wayland ("wl-paste" "-t" "image/png")
             ("xclip" "-selection" "clipboard" "-t" "image/png" "-o"))
    (x11 ("xclip" "-selection" "clipboard" "-t" "image/png" "-o")
         ("wl-paste" "-t" "image/png")))
  "Linux clipboard readers, in preference order, keyed by display server.
The head of each list is the tool that BELONGS to that display server and
is the one named when nothing is installed; the tail is accepted when it
happens to be present (an Xwayland session may carry only `xclip', and a
`wl-paste' under XWayland still reads the same clipboard).  Each entry is
(PROGRAM . ARGS) and every one of them writes PNG bytes to stdout.")

(defun agent-repl--image-display-type (&optional ws)
  "Return the Linux display server in use: `wayland' or `x11'.
Decided by environment, because that is what the running compositor
actually sets.  With neither variable set (a headless host) the answer is
`x11', so the tool a failure names is the X11 one."
  (let* ((wayland (getenv "WAYLAND_DISPLAY"))
         (x11 (getenv "DISPLAY"))
         (type (cond ((and wayland (not (string-empty-p wayland))) 'wayland)
                     ((and x11 (not (string-empty-p x11))) 'x11)
                     (t 'x11))))
    (agent-repl--log ws
                     "clipboard-image: display type decided type=%s wayland-display=%s x11-display=%s"
                     type wayland x11)
    type))

(defun agent-repl--image-linux-reader (&optional ws)
  "Return the installed Linux clipboard reader as (PROGRAM . ARGS), or nil.
Preference comes from the display server; presence comes from PATH.  Nil
means NO reader is installed -- the caller reports that, naming the tool
the display server wanted."
  (let* ((display (agent-repl--image-display-type ws))
         (candidates (cdr (assq display agent-repl--image-linux-readers)))
         (reader (seq-find (lambda (candidate)
                             (agent-repl--image-executable-find (car candidate)))
                           candidates)))
    (agent-repl--log ws
                     "clipboard-image: linux reader selection display=%s candidates=%s chosen=%s"
                     display (mapcar #'car candidates) (car reader))
    reader))

(defun agent-repl--image-linux-preferred-program (&optional ws)
  "Return the program name this display server wants, installed or not.
This is what a missing-tool report names, so the user is told what to
install rather than that something unspecified went wrong."
  (car (car (cdr (assq (agent-repl--image-display-type ws)
                       agent-repl--image-linux-readers)))))

(defun agent-repl--image-capture-linux (dest &optional ws)
  "Capture the clipboard image to DEST on Linux; return DEST or signal.
Reads the clipboard through the display server's own tool and pipes the
PNG bytes into DEST.  An absent tool and an empty clipboard are both
REPORTED -- an `agent-repl--error' record and a `user-error' the user
sees -- never a silent no-op."
  (agent-repl--log ws "clipboard-image: linux capture started destination=%s" dest)
  (let ((reader (agent-repl--image-linux-reader ws)))
    (unless reader
      (let ((wanted (agent-repl--image-linux-preferred-program ws)))
        (agent-repl--error ws
                           "clipboard-image: no clipboard reader installed destination=%s wanted=%s"
                           dest wanted)
        (user-error "agent-repl: no clipboard image reader found; install %s" wanted)))
    (let* ((program (car reader))
           (exit-code (apply #'agent-repl--image-call-process-to-file
                             dest program (cdr reader)))
           (nonempty (and (eq 0 exit-code)
                          (agent-repl--image-nonempty-file-p dest ws))))
      (agent-repl--log ws
                       "clipboard-image: linux capture finished program=%s destination=%s exit=%s nonempty=%s"
                       program dest exit-code nonempty)
      (unless nonempty
        (agent-repl--error ws
                           "clipboard-image: linux capture produced no image program=%s destination=%s exit=%s"
                           program dest exit-code)
        (user-error "agent-repl: no image found on the clipboard"))
      dest)))

(defun agent-repl--image-capture-clipboard (dest &optional ws)
  "Capture the clipboard image to DEST (a .png path); return DEST or signal.
The reader is chosen BY PLATFORM: `agent-repl--image-capture-macos' on
macOS, `agent-repl--image-capture-linux' on Linux.  Any other platform is
a reported refusal rather than a macOS attempt that cannot work."
  (cond
   ((eq system-type 'darwin) (agent-repl--image-capture-macos dest ws))
   ((memq system-type '(gnu/linux gnu gnu/kfreebsd berkeley-unix))
    (agent-repl--image-capture-linux dest ws))
   (t
    (agent-repl--error ws
                       "clipboard-image: no clipboard reader for this platform system-type=%s destination=%s"
                       system-type dest)
    (user-error "agent-repl: clipboard image capture is unsupported on %s" system-type))))

(defun agent-repl--image-capture-macos (dest &optional ws)
  "Capture the clipboard image to DEST (a .png path) on macOS; return DEST.
Tries the PNG pasteboard flavor first (what a macOS screenshot provides),
then a TIFF flavor converted to PNG with `sips'.  Signals a `user-error'
when the clipboard holds no image at all -- an empty attachment would be
a silent no-op the user could not tell from a successful one."
  (agent-repl--log ws "clipboard-image: capture started destination=%s" dest)
  (let ((png-written (agent-repl--image-write-flavor "«class PNGf»" dest ws)))
    (cond
     (png-written
      (agent-repl--log ws "clipboard-image: capture selected PNG flavor destination=%s" dest)
      dest)
     ((let* ((tiff (concat (file-name-sans-extension dest) ".tiff"))
             (tiff-written (agent-repl--image-write-flavor "«class TIFF»" tiff ws))
             (sips-exit (and tiff-written
                             (agent-repl--image-call-process
                              "sips" "-s" "format" "png" tiff "--out" dest)))
             (png-written (and (eq 0 sips-exit)
                               (agent-repl--image-nonempty-file-p dest ws))))
        (agent-repl--log ws
                         "clipboard-image: TIFF capture branch tiff=%s tiff-written=%s sips-exit=%s png-written=%s"
                         tiff tiff-written sips-exit png-written)
        png-written)
      (agent-repl--log ws "clipboard-image: capture selected TIFF conversion destination=%s" dest)
      dest)
     (t
      (agent-repl--log ws "clipboard-image: capture failed destination=%s png-written=%s"
                       dest png-written)
      (user-error "agent-repl: no image found on the clipboard")))))

;;;; ---- Buffer marker and attachment ---------------------------------------

(defun agent-repl--image-thumbnail (path &optional ws)
  "Return an image descriptor for PATH scaled to the thumbnail height, or nil.
Nil when this frame cannot render PNG images (e.g. a TTY frame), so callers
fall back to the plain marker text."
  (let* ((graphic (display-graphic-p))
         (png-available (and graphic (image-type-available-p 'png)))
         (thumbnail (and png-available
                         (create-image path 'png nil
                                       :max-height agent-repl-image-thumbnail-max-height))))
    (agent-repl--log ws
                     "clipboard-image: thumbnail decision path=%s graphic=%s png-available=%s thumbnail-created=%s max-height=%s"
                     path graphic png-available (not (null thumbnail))
                     agent-repl-image-thumbnail-max-height)
    thumbnail))

(defun agent-repl--image-marker-text (path)
  "Return the composer marker line drawn for the attached image at PATH.
It is a MARKER, not the payload: the image travels as its own
`ImageBlock', and this line exists so the user can see that they attached
something.  It is cleared with the rest of the composer once the daemon
accepts the submission."
  (format "[image attached: %s]" (file-name-nondirectory path)))

(defun agent-repl--image-insert-marker (path &optional ws)
  "Insert the attachment marker for PATH at point, overlaying a thumbnail.
Returns the overlay, or nil when no thumbnail could be drawn.  The
inserted text is the marker and never the path: the composer does not
read this text to find the image.

EVERY character this function inserts carries
`agent-repl--input-image-marker-property' -- the marker line, its
trailing newline, and the leading newline that opens a line for it --
so `agent-repl--read-input-buffer' drops the whole drawing and nothing
about the image rides the words.  The leading newline is inserted HERE
and was never typed, so leaving it behind would put a line break in the
user's prose that they did not write.

The property is the STRIPPING contract and the overlay is the thumbnail;
they are not the same thing, because a TTY frame draws no overlay and
still must not send the marker -- which is also why the overlay covers
only the marker TEXT while the property covers the whole drawing.  The
span is marked `rear-nonsticky' so text typed after it is the user's
own, not more marker."
  (let ((drawn-from (point))
        (started-at-bol (bolp)))
    (unless started-at-bol (insert "\n"))
    (let ((start (point)))
      (insert (agent-repl--image-marker-text path))
      (let ((overlay
             (when-let ((thumb (agent-repl--image-thumbnail path ws)))
               (let ((ov (make-overlay start (point))))
                 (overlay-put ov 'display thumb)
                 (overlay-put ov 'agent-repl-image path)
                 (overlay-put ov 'help-echo path)
                 ov))))
        (insert "\n")
        (put-text-property drawn-from (point)
                           agent-repl--input-image-marker-property path)
        (put-text-property drawn-from (point) 'rear-nonsticky t)
        (agent-repl--log ws
                         "clipboard-image: inserted marker path=%s started-at-bol=%s thumbnail-overlay=%s"
                         path started-at-bol (not (null overlay)))
        overlay))))

;;;###autoload
(defun agent-repl-attach-clipboard-image ()
  "Attach the clipboard image to this workspace's composer.
Writes the clipboard image (a pasted file OR a macOS screenshot) into the
workspace image dir, REGISTERS it as an attachment on the input buffer,
and draws a thumbnail marker at point.  The composer submits it as an
`ImageBlock{path, media_type}' beside its text block; nothing about the
image rides the words.  Signals a `user-error' when the clipboard holds
no image."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (dir (agent-repl--image-dir ws))
         (path (agent-repl--image-new-path dir ws))
         (dest (agent-repl--image-capture-clipboard path ws)))
    (agent-repl-input-attach-image dest agent-repl--image-media-type)
    (agent-repl--image-insert-marker dest ws)
    (agent-repl--info ws "clipboard-image: attached ws=%s path=%s media-type=%s"
                      ws dest agent-repl--image-media-type)
    (message "agent-repl: attached image %s" (file-name-nondirectory dest))
    dest))

;;;; ---- Keybinding -----------------------------------------------------------

;; Bound into the input mode map (defined in input.el, loaded first).  `map!'
;; is a no-op stub in the batch harness, so this binding is not test-covered.
(map! :map agent-repl-input-mode-map
      :ni "C-c C-i" #'agent-repl-attach-clipboard-image)

(provide 'clipboard-image)

;;; clipboard-image.el ends here
