;;; lang/personal-emacs-lisp/config.el -*- lexical-binding: t; -*-

(after! ielm
  (set-popup-rule! "*ielm*" :quit nil :height 15))
