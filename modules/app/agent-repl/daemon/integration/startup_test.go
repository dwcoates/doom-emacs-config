//go:build integration

package integration

import (
	"slices"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// startupLog reads a new Emacs's startup off its daemon stream until the
// finish, answering the go-ahead order and the finish.
func startupLog(t *testing.T, d *harness.Daemon, s *harness.Stream[*agentreplv1.WatchDaemonResponse]) ([]string, *agentreplv1.DaemonStartupFinished) {
	t.Helper()
	var opened []string
	var finished *agentreplv1.DaemonStartupFinished
	ctx, cancel := d.WaitCtx()
	defer cancel()
	harness.AwaitView(t, ctx, s, "the startup's finish", func(r *agentreplv1.WatchDaemonResponse) bool {
		if open := r.GetStartup().GetWorkspaceOpen(); open != nil {
			opened = append(opened, open.GetWorkspace().GetId())
		}
		finished = r.GetStartup().GetFinished()
		return finished != nil
	})
	return opened, finished
}

// tabOrderIDs is the roster's open rows in walk order: the registry order.
func tabOrderIDs(roster *frontendv1.WorkspaceRoster) []string {
	var out []string
	var walk func(rows []*frontendv1.RosterRow)
	walk = func(rows []*frontendv1.RosterRow) {
		for _, r := range rows {
			if !r.GetClosed().GetClosed() {
				out = append(out, r.GetWorkspace().GetWorkspace().GetId())
			}
			walk(r.GetChildren())
		}
	}
	for _, s := range roster.GetRepository().GetSections() {
		walk(s.GetRows().GetRows())
	}
	return out
}

func TestANewEmacsBringsEveryWorkspaceUpAndOpensThemInRegistryOrder(t *testing.T) {
	t.Parallel()
	// Arrange: two registered workspaces, neither opened.
	d := newDaemon(t, harness.Opts{})
	first := harness.Register(t, d, harness.NewRepo(t).Dir)
	second := harness.Register(t, d, harness.NewRepo(t).Dir)
	roster := awaitRoster(t, d, d.WatchRoster(), "both workspaces on the roster", func(r *frontendv1.WorkspaceRoster) bool {
		return len(tabOrderIDs(r)) == 2
	})
	want := tabOrderIDs(roster)

	// Act
	opened, finished := startupLog(t, d, d.WatchDaemonStream())

	// Assert
	if !slices.Equal(opened, want) {
		t.Fatalf("go-aheads = %v, want the registry order %v", opened, want)
	}
	if finished.GetReady() != 2 || finished.GetTotal() != 2 || len(finished.GetFailed()) != 0 {
		t.Fatalf("finished = %v, want 2 of 2 ready", finished)
	}
	awaitRoster(t, d, d.WatchRoster(), "both sessions up after the startup", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, first.GetId()).GetReady() != nil && rosterRow(r, second.GetId()).GetReady() != nil
	})
}

func TestAReconnectingEmacsIsToldNoStartup(t *testing.T) {
	t.Parallel()
	// Arrange: the same Emacs process has already had its startup.
	d := newDaemon(t, harness.Opts{})
	harness.Register(t, d, harness.NewRepo(t).Dir)
	first := d.WatchDaemonStream()
	startupLog(t, d, first)
	first.Close()

	// Act
	again := d.WatchDaemonStream()

	// Assert: the standing pushes arrive, and no startup event among them.
	ctx, cancel := d.WaitCtx()
	defer cancel()
	harness.AwaitView(t, ctx, again, "the persistent-wifi standing", func(r *agentreplv1.WatchDaemonResponse) bool {
		if r.GetStartup() != nil {
			t.Fatalf("a reconnect was told a startup event %v", r.GetStartup())
		}
		return r.GetPersistentWifi() != nil
	})
	harness.ExpectNoPush(t, again, harness.ProbeWindow, "a startup event for a reconnect")
}

func TestANetworkTheShimCannotReachIsANetworkFaultOnTheFooterAndTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpenedWithProfile(t, harness.Opts{}, harness.ShimProfile{
		OpeningNetworkUnreachable: "cannot reach api.anthropic.com: no route to host",
	})

	// Act
	footer := f.d.WatchFooter(f.ws)

	// Assert
	view := awaitFooter(t, f, footer, "the footer's network fault", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetNetworkFault().GetOffline() != nil
	})
	if got := view.GetStrip().GetStatus().GetNetworkFault().GetActivity().GetSalient().GetOffline().GetText(); got != "cannot reach api.anthropic.com: no route to host" {
		t.Fatalf("offline line = %q, want the shim's own observation", got)
	}
	awaitRoster(t, f.d, f.d.WatchRoster(), "the roster's network fault", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()).GetNetworkFault() != nil
	})
}
