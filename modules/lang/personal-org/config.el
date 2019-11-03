;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package! org
  :config
  (+org-pretty-mode)

  (defun my-org-hook ()
    (abbrev-mode t))
  (add-hook! 'org-mode-hook 'my-org-hook)

  (define-abbrev-table 'org-mode-abbrev-table
    (mapcar
     (lambda (char-string)
       (let ((character-property-elements
              (split-string (get-char-code-property (encode-char (string-to-char char-string) 'unicode) 'name) " ")))
         (list
          (concat
           (if (member "CAPITAL" character-property-elements)
               (capitalize (-last-item character-property-elements))
             (downcase (-last-item character-property-elements)))
           "x")
          char-string)))
     '("α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"
       "Α" "Β" "Γ" "Δ" "Ε" "Ζ" "Η" "Θ" "Ι" "Κ" "Λ" "Μ" "Ν" "Ξ" "Ο" "Π" "Ρ" "Σ" "Τ" "Υ" "Φ" "Χ" "Ψ" "Ω"))))
