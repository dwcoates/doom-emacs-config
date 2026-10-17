;;; config/doom-tomorrow-night/derive.el -*- lexical-binding: t; -*-
;;
;; Derives a doom theme from an upstream theme file by re-evaluating the
;; upstream `def-doom-theme' form with some palette entries swapped.  Every
;; color the upstream palette computes FROM a swapped entry (the mode line's
;; `(doom-darken (car bg-alt) 0.3)', say) follows the swap, because the
;; palette is evaluated afresh rather than patched face by face afterwards.

(require 'cl-lib)

(defun doom-tomorrow-night-read-forms (file)
  "Return every top-level form of FILE, in order."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun doom-tomorrow-night--derive-defs (defs grounds)
  "Return DEFS with each GROUNDS entry's GUI color swapped in.
GROUNDS is an alist of (VAR . GUI-COLOR).  Each VAR must name a palette
entry of the shape (VAR \\='(GUI ...)), else an error is signaled."
  (dolist (ground grounds)
    (let ((def (assq (car ground) defs)))
      (unless (and def (eq (car-safe (cadr def)) 'quote)
                   (stringp (car-safe (cadr (cadr def)))))
        (error "doom-tomorrow-night: palette has no literal entry %S to swap for %S (found %S)"
               (car ground) (cdr ground) def))))
  (mapcar (lambda (def)
            (let ((ground (assq (car def) grounds)))
              (if ground
                  `(,(car def) '(,(cdr ground) ,@(cdr (cadr (cadr def)))))
                def)))
          defs))

(defun doom-tomorrow-night-derive-theme (forms name grounds)
  "Return FORMS with its `def-doom-theme' form renamed to NAME.
GROUNDS swaps palette entries per `doom-tomorrow-night--derive-defs'.
Every other form is kept as is.  Exactly one `def-doom-theme' form must
be present, else an error is signaled."
  (let ((themes (cl-count-if (lambda (f) (eq (car-safe f) 'def-doom-theme)) forms)))
    (unless (= themes 1)
      (error "doom-tomorrow-night: expected one def-doom-theme form, found %d" themes)))
  (mapcar
   (lambda (form)
     (if (not (eq (car-safe form) 'def-doom-theme))
         form
       (pcase-let ((`(_ ,_name ,doc . ,rest) form)
                   (props nil))
         (while (keywordp (car rest))
           (push (pop rest) props)
           (push (pop rest) props))
         `(def-doom-theme ,name ,doc ,@(nreverse props)
            ,(doom-tomorrow-night--derive-defs (car rest) grounds)
            ,@(cdr rest)))))
   forms))

(provide 'doom-tomorrow-night-derive)
