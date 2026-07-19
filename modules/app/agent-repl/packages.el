;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; app/agent-repl/packages.el

;; agent-repl itself needs no extra packages here — the gui/webview is
;; the only agent-repl frontend now, and it drives an xwidget webview
;; rather than a terminal.  vterm still comes from Doom's `:term vterm'
;; module, which stays enabled in init.el: the plain `SPC o t' terminal
;; uses it directly, and `sibling-popup.el' serves the `*doom:vterm*'
;; buffer it creates.
