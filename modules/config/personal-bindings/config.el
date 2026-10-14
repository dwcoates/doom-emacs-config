(map! :map general-override-mode-map
      "C-S-h" #'evil-window-left
      "C-S-l" #'evil-window-right)

;; `:config default' with `evil +everywhere' binds `C-k' to `previous-line' in
;; every plain minibuffer map (both the base map and its insert-state aux map),
;; so `C-k' at a one-line prompt only reports "Beginning of buffer".  Give it
;; back its Emacs meaning, `kill-line': kill to the end of the line.  The completion UIs (`ivy-minibuffer-map' and friends)
;; are left alone, since there `C-k' moves through the candidate list.  This
;; module loads after `:config default', so these bindings win.
(defvar +dwc/minibuffer-kill-line-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map)
  "Plain minibuffer keymaps where `C-k' kills to the end of the line.")

(defun +dwc/bind-minibuffer-kill-line ()
  "Bind `C-k' to `kill-line' in `+dwc/minibuffer-kill-line-maps'.
Binds the base keymaps, and their evil insert-state aux maps once evil
is loaded."
  (dolist (map-symbol +dwc/minibuffer-kill-line-maps)
    (unless (keymapp (symbol-value map-symbol))
      (error "Minibuffer keymap %s is not a keymap" map-symbol))
    (define-key (symbol-value map-symbol) (kbd "C-k") #'kill-line))
  (with-eval-after-load 'evil
    (dolist (map-symbol +dwc/minibuffer-kill-line-maps)
      (evil-define-key* 'insert (symbol-value map-symbol)
        (kbd "C-k") #'kill-line))))

(+dwc/bind-minibuffer-kill-line)
