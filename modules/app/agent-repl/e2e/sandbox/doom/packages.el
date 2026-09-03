;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; packages.el --- sandbox profile packages -*- lexical-binding: t; -*-

;; Intentionally empty. Every package the sandbox needs comes from the Doom
;; modules enabled in init.el, and `modules/app/agent-repl/packages.el`
;; declares no extra packages of its own. Adding anything here would mean the
;; sandbox tests against a package set the host does not have.
