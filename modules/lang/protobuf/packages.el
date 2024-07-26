;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! protobuf-mode
  :recipe (:host github :repo "protocolbuffers/protobuf"
           :files ("editors/protobuf-mode.el")))
