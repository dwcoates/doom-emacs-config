;;; lang/personal-markdown/config.el -*- lexical-binding: t; -*-

(map!
   :map markdown-mode-map
   :nvieomrg
   "M-b" #'backward-word)

(map!
   :map gfm-mode-map
   :nvieomrg
   "M-b" #'backward-word)

(map!
   :map evil-markdown-mode-map
   :nvieomrg
   "M-b" #'backward-word)
