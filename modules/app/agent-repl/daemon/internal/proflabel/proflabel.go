// Package proflabel tags the daemon's per-workspace work with a Go profiler
// label, Key, valued by the workspace's id, so a CPU profile of the daemon
// splits by workspace: `go tool pprof -tagfocus workspace=<id>` keeps one
// workspace's samples, `-tagshow workspace` lists them. A shared test daemon's
// profile therefore splits by test, since each test owns its workspaces
// (daemon/integration/harness/profile.go maps them).
//
// WHERE WORK IS LABELED: every rpc that names a workspace, from its handler's
// start to its return (server's request boundary), and every session watcher,
// from its start (sessionwatcher.Start). A goroutine inherits its creator's
// labels, so everything either starts is labeled too. Work the daemon does on
// its own (its boot, the roster, the merge queue's pump) carries no label.
//
// The cost is one pprof.Do per labeled call: measured in daemon/AGENTS.md
// "Profiler labels".
package proflabel

import (
	"context"
	"runtime/pprof"

	"agent-repld/internal/ids"
)

// Key is the profiler label naming the workspace a sample's work was for.
const Key = "workspace"

// Do runs f with the calling goroutine labeled Key=ws, and every goroutine f
// starts inheriting the label; the goroutine's own labels are restored when f
// returns. Work for no workspace (ws empty) runs unlabeled.
func Do(ctx context.Context, ws ids.WorkspaceID, f func(context.Context)) {
	if ws == "" {
		f(ctx)
		return
	}
	pprof.Do(ctx, pprof.Labels(Key, string(ws)), f)
}

// Of answers the workspace ctx's work is labeled for, and whether it is.
func Of(ctx context.Context) (ids.WorkspaceID, bool) {
	ws, ok := pprof.Label(ctx, Key)
	return ids.WorkspaceID(ws), ok
}
