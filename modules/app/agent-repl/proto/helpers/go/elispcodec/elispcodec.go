// Package elispcodec holds the check that Emacs's hand-written wire codec
// (lisp/wire-*.el) names the same fields the .proto files declare.
//
// THE ELISP CODEC IS WRITTEN BY HAND and refuses every unknown field, so a
// field the proto gains and the codec does not is a push Emacs refuses
// whole: an async merge's requester froze Emacs's roster for a day
// (2026-11-19), its tabs keeping killed workspaces and ignoring folds. The
// Go and TypeScript bindings are generated and cannot drift; this package's
// test is what keeps the elisp one from drifting.
package elispcodec

import (
	"fmt"
	"regexp"
	"sort"
	"strings"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// Claim is one message the codec decodes and the fields it lets through:
// a `agent-repl-wire--check-keys' allowlist, or a
// `agent-repl-wire--decode-empty', which lets none through.
type Claim struct {
	File    string
	Message string
	Fields  []string
}

var (
	checkKeys   = regexp.MustCompile(`\(agent-repl-wire--check-keys\s+"([A-Za-z0-9]+)"\s+[^\s()']+\s+'\(([^)]*)\)`)
	decodeEmpty = regexp.MustCompile(`\(agent-repl-wire--decode-empty\s+"([A-Za-z0-9]+)"`)
)

// Claims answers every literal claim the elisp SOURCE (named FILE) makes.
// A call whose message name or allowlist is not a literal is a helper's
// own, whose callers make the literal claim, so it is not read.
func Claims(file, source string) []Claim {
	var out []Claim
	for _, m := range checkKeys.FindAllStringSubmatch(source, -1) {
		out = append(out, Claim{File: file, Message: m[1], Fields: strings.Fields(m[2])})
	}
	for _, m := range decodeEmpty.FindAllStringSubmatch(source, -1) {
		out = append(out, Claim{File: file, Message: m[1], Fields: []string{}})
	}
	return out
}

// JSONFields answers MD's fields by their protojson names, sorted.
func JSONFields(md protoreflect.MessageDescriptor) []string {
	fields := md.Fields()
	out := make([]string, 0, fields.Len())
	for i := 0; i < fields.Len(); i++ {
		out = append(out, fields.Get(i).JSONName())
	}
	sort.Strings(out)
	return out
}

// Mismatch answers how CLAIM differs from the fields of the messages its
// bare name could be (CANDIDATES, one per package declaring the name), or
// "" when it matches one of them. A name no package declares is a mismatch.
// NARROWED names the declared fields the codec deliberately leaves out of
// this message, each one a field the proto says Emacs is never sent (a
// webview-only arm, a header variant that carries no count); a narrowed
// field the claim names is still a match.
func Mismatch(claim Claim, candidates []protoreflect.MessageDescriptor, narrowed map[string]bool) string {
	if len(candidates) == 0 {
		return fmt.Sprintf("%s: %s is declared by no .proto message", claim.File, claim.Message)
	}
	claimed := append([]string(nil), claim.Fields...)
	sort.Strings(claimed)
	var diffs []string
	for _, md := range candidates {
		declared := JSONFields(md)
		var missing []string
		for _, f := range difference(declared, claimed) {
			if !narrowed[f] {
				missing = append(missing, f)
			}
		}
		extra := difference(claimed, declared)
		if len(missing) == 0 && len(extra) == 0 {
			return ""
		}
		diffs = append(diffs, fmt.Sprintf("%s: the codec lacks %v and names %v the proto does not declare", md.FullName(), missing, extra))
	}
	return fmt.Sprintf("%s: %s matches no declaring message: %s", claim.File, claim.Message, strings.Join(diffs, "; "))
}

// difference answers the members of A not in B.
func difference(a, b []string) []string {
	in := make(map[string]bool, len(b))
	for _, s := range b {
		in[s] = true
	}
	var out []string
	for _, s := range a {
		if !in[s] {
			out = append(out, s)
		}
	}
	return out
}
