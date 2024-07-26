(use-package! protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :hook (protobuf-mode . lsp-deferred)
  :defer-incrementally t)
