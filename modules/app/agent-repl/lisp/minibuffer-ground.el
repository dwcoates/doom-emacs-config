;;; minibuffer-ground.el --- The minibuffer wears the composer's color -*- lexical-binding: t; -*-

;;; Commentary:

;; THE MINIBUFFER AND THE ECHO AREA WEAR THE COMPOSER'S COLOR (owner
;; request, 2026-11-02): the dark sidebar's ground, which the composer
;; and the sidebar share.  The color is `agent-repl-input-background'
;; itself (input.el), never a second literal, so the three never drift.
;;
;; The minibuffer window shows one of two kinds of buffer: a minibuffer
;; (" *Minibuf-N*", active while something is read, inactive between)
;; and, while a message stands, an echo area buffer (" *Echo Area 0*" or
;; " *Echo Area 1*").  Each is painted with the same buffer-local remap
;; the composer uses (`agent-repl--set-buffer-background', core.el).
;;
;; A minibuffer loses its local variables, and with them the remap, each
;; time it is entered (`minibuffer-mode') and left
;; (`minibuffer-inactive-mode'), so it is painted again from both hooks.
;; The echo area buffers keep theirs, so they are painted at load.  Each
;; buffer records the color it was painted and its remap cookies, so a
;; repaint in the same color does nothing and one in a new color takes
;; the old remap back first.

;;; Code:

(require 'seq)

(declare-function agent-repl--set-buffer-background "core" (color))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(defvar agent-repl-input-background)

(defconst agent-repl--minibuffer-ground-log-scope
  '(:agent-repl-central "the minibuffer belongs to no one workspace")
  "The central log scope every minibuffer ground record is written under.")

(defconst agent-repl--minibuffer-ground-echo-buffers
  '(" *Echo Area 0*" " *Echo Area 1*")
  "The names of Emacs's two echo area buffers.")

(defvar-local agent-repl--minibuffer-ground-painted nil
  "The (COLOR . COOKIES) this buffer was painted with, or nil.")

(defun agent-repl-minibuffer-ground-paint ()
  "Paint the current buffer's background `agent-repl-input-background'.
A buffer already in that color is left as it is; one painted another
color has that remap taken back first."
  (let ((color agent-repl-input-background))
    (unless (equal (car agent-repl--minibuffer-ground-painted) color)
      (agent-repl--log agent-repl--minibuffer-ground-log-scope
                       "elisp.minibuffer-ground.paint buffer=%s color=%s from=%S"
                       (buffer-name) color (car agent-repl--minibuffer-ground-painted))
      (mapc #'face-remap-remove-relative (cdr agent-repl--minibuffer-ground-painted))
      (setq agent-repl--minibuffer-ground-painted
            (cons color (agent-repl--set-buffer-background color))))))

(defun agent-repl-minibuffer-ground-paint-all ()
  "Paint every minibuffer and both echo area buffers.
An echo area buffer Emacs has not made yet is made under its name, the
way Emacs itself makes it, so the first message finds it painted."
  (dolist (buf (append (seq-filter #'minibufferp (buffer-list))
                       (mapcar #'get-buffer-create agent-repl--minibuffer-ground-echo-buffers)))
    (with-current-buffer buf
      (agent-repl-minibuffer-ground-paint))))

(add-hook 'minibuffer-setup-hook #'agent-repl-minibuffer-ground-paint)
(add-hook 'minibuffer-inactive-mode-hook #'agent-repl-minibuffer-ground-paint)
;; A load (and a reload) finds the minibuffers and echo areas already made.
(agent-repl-minibuffer-ground-paint-all)

(provide 'minibuffer-ground)
;;; minibuffer-ground.el ends here
