package reload

import (
	"path"
	"slices"
	"strings"
)

// Component is one buildable-and-bounceable part of the agent-repl stack.
//
// The set is deliberately the set bin/deploy-all.sh knows how to build, because
// the classification exists for exactly one purpose: to decide whether a merge
// that just landed in this daemon's OWN checkout changed anything the running
// stack is executing, and therefore whether a redeploy is warranted at all.
type Component string

const (
	// ComponentDaemon is the Go daemon this process is running.
	ComponentDaemon Component = "daemon"
	// ComponentShim is the TypeScript agent shim bundle.
	ComponentShim Component = "shim"
	// ComponentStore is the shim-store service (launchd-run).
	ComponentStore Component = "shim-store"
	// ComponentSidecar is the shim-claude-sidecar service (launchd-run).
	ComponentSidecar Component = "shim-claude-sidecar"
	// ComponentWebapp is the browser frontend served by the daemon.
	ComponentWebapp Component = "webapp"
	// ComponentProto is the shared protobuf contract, whose regeneration feeds
	// the daemon, the shim, and the webapp alike.
	ComponentProto Component = "proto"
	// ComponentElisp is the Emacs side, hot-loaded rather than rebuilt.
	ComponentElisp Component = "elisp"
)

// stackPrefix is where the whole stack lives inside its host repository. It is
// the same path selfRepoRoot located bin/deploy-all.sh under, so the two can
// never disagree about which checkout layout is meant.
const stackPrefix = "modules/app/agent-repl/"

// componentRule maps a repository-relative path prefix to the component it
// belongs to. Order is significant and the rules are FIRST MATCH: the shim and
// the sidecar are siblings whose directory names share a prefix, so the
// trailing slash on each rule is what keeps agent-shim/claude/shim-sidecar/ out
// of the shim's bucket.
type componentRule struct {
	prefix    string
	component Component
}

var componentRules = []componentRule{
	{stackPrefix + "daemon/", ComponentDaemon},
	{stackPrefix + "agent-shim/claude/shim/", ComponentShim},
	{stackPrefix + "agent-shim/claude/shim-sidecar/", ComponentSidecar},
	{stackPrefix + "agent-shim/shim-store/", ComponentStore},
	{stackPrefix + "webapp/", ComponentWebapp},
	{stackPrefix + "proto/", ComponentProto},
}

// componentOrder is the stable report order for a classified set, mirroring
// deploy-all.sh's own dependency order (proto first, elisp last) so a logged
// classification reads like the deploy it will cause.
var componentOrder = []Component{
	ComponentProto,
	ComponentShim,
	ComponentDaemon,
	ComponentStore,
	ComponentSidecar,
	ComponentWebapp,
	ComponentElisp,
}

// Classify maps repository-relative changed paths onto the components they
// belong to, deduplicated and returned in componentOrder.
//
// A path matching NO rule contributes nothing. That is the ordinary case for a
// merge that touched some other part of the host repository (this daemon's own
// checkout is a whole Emacs configuration, most of which the running stack does
// not execute), and an empty result is the signal to skip the redeploy
// entirely rather than bounce the stack for an unrelated edit.
func Classify(paths []string) []Component {
	seen := map[Component]bool{}
	for _, p := range paths {
		if c, ok := classifyOne(p); ok {
			seen[c] = true
		}
	}
	out := make([]Component, 0, len(seen))
	for _, c := range componentOrder {
		if seen[c] {
			out = append(out, c)
		}
	}
	return out
}

// classifyOne resolves a single repository-relative path.
func classifyOne(p string) (Component, bool) {
	p = strings.TrimPrefix(path.Clean(p), "./")
	for _, rule := range componentRules {
		if strings.HasPrefix(p, rule.prefix) {
			return rule.component, true
		}
	}
	// Elisp is matched by extension rather than by directory: the stack's .el
	// files sit at the top of modules/app/agent-repl/ rather than in a
	// component directory of their own.
	if strings.HasPrefix(p, stackPrefix) && strings.HasSuffix(p, ".el") {
		// test-*.el is the batch-only ERT harness. Loading one into the live
		// Emacs is the recorded way to poison that Emacs, and deploy-all.sh
		// skips them for the same reason, so they are not an elisp change the
		// running stack has any use for.
		if strings.HasPrefix(path.Base(p), "test-") {
			return "", false
		}
		return ComponentElisp, true
	}
	return "", false
}

// NeedsElispReload reports whether a classified set contains the elisp
// component, which is the one component deploy-all.sh needs told about
// explicitly (via --elisp <range>).
func NeedsElispReload(components []Component) bool {
	return slices.Contains(components, ComponentElisp)
}

// Strings renders a component set for a log line.
func Strings(components []Component) []string {
	out := make([]string, 0, len(components))
	for _, c := range components {
		out = append(out, string(c))
	}
	return out
}
