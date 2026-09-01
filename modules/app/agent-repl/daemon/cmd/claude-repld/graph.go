package main

import (
	"context"
	"fmt"
	"sort"
	"strings"

	"claude-repld/internal/boot"
	"claude-repld/internal/server"
)

// unwiredCollaborator is one dependency of the component graph that NO LANDED
// COMPONENT PRODUCES. It is named here rather than improvised in this file
// because cmd is the composition root and holds no policy: a stand-in written
// here would be a second, undocumented implementation of a seam that belongs to
// the component it serves.
type unwiredCollaborator struct {
	// Field is the Deps field that has no source, fully qualified.
	Field string
	// Why says what is missing and where it belongs.
	Why string
}

// unwired is the exact list of collaborators the wave-3 graph cannot supply.
// Every entry was checked against the landed packages: the type exists, the
// consumer requires it, and nothing in the daemon implements it.
//
// THE GRAPH REFUSES TO BUILD RATHER THAN SUBSTITUTING. A daemon assembled with
// improvised stand-ins would answer rpcs from behavior nobody specified, and
// the substitution would be invisible at every call site.
var unwired = []unwiredCollaborator{
	{
		Field: "rollout.Deps.Shims (ShimFleet)",
		Why:   "workspace.Fleet has Start/Stop/Shim but no Prelaunch, Install, Adopt or Resume; the relaunch engine cannot run without them",
	},
	{
		Field: "rollout.Deps.Freeness / drain.Deps.Freeness",
		Why:   "sessionwatcher.Watcher answers Free() but nothing answers AwaitFree; there is no freeness change signal to wait on",
	},
	{
		Field: "rollout.Deps.Participants / Quiesce / DrainIntake / PublishViews / Pusher / Announcer",
		Why:   "all four are the server's and the queue's halves of the handover; internal/server lands them",
	},
	{
		Field: "workspace.Deps.Cards",
		Why:   "no landed component records what the daemon SERVED for a permission ask, a question batch or a permission-mode picker; the feed and topbar resolvers expose neither by ask id",
	},
	{
		Field: "workspace.Deps.Ownership",
		Why:   "rollout.Controller does not expose a per-workspace serving standing, so nothing answers StandingTransferringAway versus StandingNotYetAdopted",
	},
	{
		Field: "workspace.Deps.EvictLogSink",
		Why:   "the brief names it as the hook onto dlog Surfaces.Evict; workspace.Deps has no such field, so a closed workspace's sinks are never released",
	},
	{
		Field: "prompthandler.Deps.Panels",
		Why:   "nothing assembles the six recognized command panels; the server owns their facts",
	},
	{
		Field: "merge.Deps.AwaitTurnEnd / CaptureDisplaced / Occupy / StartSession",
		Why:   "the turn waiter, the displaced capture, the occupancy guard and the revival start have no landed producer above the queue and the fleet",
	},
	{
		Field: "feed.Deps.ResolveImage",
		Why:   "turning an image block into a fetchable src is the server's asset origin, which lands with internal/server",
	},
	{
		Field: "the checkout the binary was deployed from",
		Why:   "--shim-main, --webapp-dist, --prompts-dir and proto/vocab all default to paths inside it, and no landed helper resolves it",
	},
}

// buildGraph builds the component graph. It cannot yet: the collaborators in
// `unwired` have no landed source, so the graph refuses LOUDLY and names every
// one of them rather than starting a daemon with substituted behavior.
func buildGraph(_ context.Context, _ process) (server.Deps, boot.Deps, error) {
	names := make([]string, 0, len(unwired))
	for _, u := range unwired {
		names = append(names, fmt.Sprintf("%s (%s)", u.Field, u.Why))
	}
	sort.Strings(names)
	return server.Deps{}, boot.Deps{}, fmt.Errorf(
		"claude-repld: the component graph has %d collaborators with no landed source, and cmd substitutes none:\n  - %s",
		len(unwired), strings.Join(names, "\n  - "))
}
