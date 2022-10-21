;;; tools/personal-lsp/config.el -*- lexical-binding: t; -*-

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t
        lsp-ui-doc-max-height 40
        lsp-ui-doc-max-width 80
        lsp-ui-doc-delay 0.8))
