// Package tldr links the tree-sitter-tldr grammar (../grammar.js and
// scanner.c) into a Go program.
//
// The generated parser.c and the hand-written scanner.c sit in this package's
// own directory, so cgo compiles both and Go's build cache tracks them: a
// regenerated parser always rebuilds every program that links it.
package tldr

// #cgo CFLAGS: -std=c11 -fPIC
// #include "tree_sitter/parser.h"
// const TSLanguage *tree_sitter_tldr(void);
import "C"

import "unsafe"

// Language is the grammar, for tree_sitter.NewLanguage.
func Language() unsafe.Pointer {
	return unsafe.Pointer(C.tree_sitter_tldr())
}
