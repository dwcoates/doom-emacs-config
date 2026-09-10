package rollout

import (
	"sort"
	"strings"
)

// Subsystem is one deployable part of the stack. A landed range's changed paths
// classify into a SET of these, and each has its own rollout action.
type Subsystem string

// The subsystems, by the path prefix that identifies them.
const (
	// SubsystemProto is the contract. A proto-prefix change classifies as
	// touching EVERY consumer of the regenerated bindings.
	SubsystemProto Subsystem = "proto"
	// SubsystemDaemon is this binary: a blue-green handover.
	SubsystemDaemon Subsystem = "daemon"
	// SubsystemShim is the per-session shim: a per-workspace preemptive
	// relaunch.
	SubsystemShim Subsystem = "shim"
	// SubsystemWebapp is the served webview assets: a hot asset swap.
	SubsystemWebapp Subsystem = "webapp"
	// SubsystemElisp is the Emacs half. The daemon LOGS it and does nothing:
	// hot-loading elisp is Emacs's, never the daemon's.
	SubsystemElisp Subsystem = "elisp"
	// SubsystemStore is the store service: deliberately UNHANDLED, a
	// user-initiated full restart.
	SubsystemStore Subsystem = "store"
	// SubsystemSidecar is the sidecar service: deliberately UNHANDLED, the same
	// way.
	SubsystemSidecar Subsystem = "sidecar"
)

// ModuleRoot is the module's path inside the repository. Changed paths arrive
// repository-relative, so every prefix below is joined onto it.
const ModuleRoot = "modules/app/agent-repl/"

// prefixRule maps one path prefix onto the subsystem it identifies. The order
// is LONGEST PREFIX FIRST, because agent-shim/claude/shim/ and
// agent-shim/claude/shim-sidecar/ share a head and mean different things.
type prefixRule struct {
	prefix    string
	subsystem Subsystem
}

// prefixRules is the closed inventory, longest first.
var prefixRules = []prefixRule{
	{ModuleRoot + "agent-shim/claude/shim-sidecar/", SubsystemSidecar},
	{ModuleRoot + "agent-shim/claude/shim/", SubsystemShim},
	{ModuleRoot + "agent-shim/shim-store/", SubsystemStore},
	{ModuleRoot + "proto/", SubsystemProto},
	{ModuleRoot + "daemon/", SubsystemDaemon},
	{ModuleRoot + "webapp/", SubsystemWebapp},
	{ModuleRoot + "lisp/", SubsystemElisp},
}

// Classify maps a landed range's changed paths onto the subsystems they touch.
//
// A PROTO change expands to every consumer of the regenerated bindings — the
// daemon, the shim and the webapp — because the bindings are what the change
// actually deploys, and a proto commit that only regenerated one consumer's
// bindings would still have changed the other two's.
func Classify(paths []string) []Subsystem {
	set := map[Subsystem]bool{}
	for _, path := range paths {
		clean := strings.TrimPrefix(strings.TrimSpace(path), "./")
		if clean == "" {
			continue
		}
		if sub, ok := matchPrefix(clean); ok {
			set[sub] = true
			continue
		}
		if isModuleElisp(clean) {
			set[SubsystemElisp] = true
		}
	}
	if set[SubsystemProto] {
		set[SubsystemDaemon] = true
		set[SubsystemShim] = true
		set[SubsystemWebapp] = true
	}
	out := make([]Subsystem, 0, len(set))
	for sub := range set {
		out = append(out, sub)
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// matchPrefix answers the subsystem a path's longest matching prefix names.
func matchPrefix(path string) (Subsystem, bool) {
	for _, rule := range prefixRules {
		if strings.HasPrefix(path, rule.prefix) {
			return rule.subsystem, true
		}
	}
	return "", false
}

// isModuleElisp reports whether a path is one of the module's top-level elisp
// files — config.el, doctor.el, packages.el — which Doom's module loader
// resolves by exact path and which therefore live beside lisp/ rather than in
// it.
func isModuleElisp(path string) bool {
	rest, ok := strings.CutPrefix(path, ModuleRoot)
	if !ok {
		return false
	}
	return !strings.Contains(rest, "/") && strings.HasSuffix(rest, ".el")
}

// Unhandled reports the subsystems in a classification that the rollout
// deliberately does NOT act on. They are rare-change codebases by design and a
// change to one is a user-initiated full restart; the trigger names them in a
// WARNING rather than silently doing nothing.
func Unhandled(subsystems []Subsystem) []Subsystem {
	var out []Subsystem
	for _, sub := range subsystems {
		if sub == SubsystemStore || sub == SubsystemSidecar {
			out = append(out, sub)
		}
	}
	return out
}

// contains reports whether a classification names one subsystem.
func contains(subsystems []Subsystem, want Subsystem) bool {
	for _, sub := range subsystems {
		if sub == want {
			return true
		}
	}
	return false
}

// names renders a classification for a record's context.
func names(subsystems []Subsystem) []string {
	out := make([]string, 0, len(subsystems))
	for _, sub := range subsystems {
		out = append(out, string(sub))
	}
	return out
}
