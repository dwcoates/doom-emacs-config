(use-package! protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :defer-incrementally t
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
    (lambda () (c-add-style "my-style" my-protobuf-style t)))
  )
