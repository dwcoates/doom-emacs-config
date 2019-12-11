;;; lang/personal-common-lisp/config.el -*- lexical-binding: t; -*-

(after! sly
  (set-popup-rule! "\*sly-description\*" :side 'right :width 80)
  (set-popup-rule! "\*sly-compilation\*" :side 'right :width 80)
  (set-popup-rule! "\*sly.mrepl.*" "" :side 'right :width 100)) ;; FIXME: this doesn't work for some reason.
