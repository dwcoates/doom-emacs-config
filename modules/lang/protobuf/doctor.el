;;; lang/protobuf/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "bufls")
  (warn! "bufls for Protobuf not found. LSP features not enabled."))
