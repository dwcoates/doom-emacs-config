package deploy

import (
	"errors"
	"os"
	"slices"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/buildid"
)

// installedWebapp is the harness's installed webapp build.
func installedWebapp(t *testing.T, h *harness) string {
	t.Helper()
	build, err := buildid.Webapp(h.live.WebappDist())
	if err != nil {
		t.Fatalf("read the installed webapp: %v", err)
	}
	return build
}

func TestReconcileWebhost(t *testing.T) {
	tests := []struct {
		name      string
		hosts     func(installed string) []*agentreplv1.WebhostPage
		reach     int
		wantPush  bool
		wantReach int
	}{
		{name: "no host stands", hosts: func(string) []*agentreplv1.WebhostPage { return nil }, reach: 1},
		{name: "a host on the installed webapp is left alone", hosts: func(installed string) []*agentreplv1.WebhostPage { return hostPages(installed) }, reach: 1},
		{name: "a host on another webapp is reloaded onto the installed one", hosts: func(string) []*agentreplv1.WebhostPage { return hostPages("Old") }, reach: 1, wantPush: true, wantReach: 1},
		{name: "a reload no Emacs stands to take is pushed but not spent", hosts: func(string) []*agentreplv1.WebhostPage { return hostPages("Old") }, reach: 0, wantPush: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			installed := installedWebapp(t, h)
			h.clients.webhosts = tt.hosts(installed)
			h.clients.webhostReach = tt.reach

			// Act
			got := h.d.ReconcileWebhost(WebhostEdgeHostAttached)

			// Assert
			if got != tt.wantReach {
				t.Fatalf("reached = %d, want %d", got, tt.wantReach)
			}
			var want []string
			if tt.wantPush {
				want = []string{installed}
			}
			if !slices.Equal(h.clients.webhostPush, want) {
				t.Fatalf("host reloads = %v, want %v", h.clients.webhostPush, want)
			}
		})
	}
}

// TestAReloadNoEmacsTookIsPushedWhenOneAttaches pins the two edges: a host
// standing on another webapp before any Emacs stream stood has its reload
// pushed when an Emacs attaches.
func TestAReloadNoEmacsTookIsPushedWhenOneAttaches(t *testing.T) {
	// Arrange
	h := newHarness(t)
	installed := installedWebapp(t, h)
	h.clients.webhosts = hostPages("Old")
	h.d.ReconcileWebhost(WebhostEdgeHostAttached)
	h.clients.webhostReach = 1

	// Act
	got := h.d.ReconcileWebhost(WebhostEdgeEmacsAttached)

	// Assert
	if got != 1 || !slices.Equal(h.clients.webhostPush, []string{installed, installed}) {
		t.Fatalf("reached = %d, reloads = %v; want the reload pushed to the Emacs that attached", got, h.clients.webhostPush)
	}
}

// The two records a host still stale after its reload was pushed can draw.
const (
	ignoredReload = "a web view host reloaded after its reload onto the installed webapp was pushed and still runs another webapp"
	notYetReload  = "a web view host has not reloaded onto the installed webapp yet; its reload is already pushed"
)

// TestAHostReloadIsPushedOncePerInstalledBuild pins the bound and the
// judgment: once pushed, a later edge pushes nothing more. A host still on
// another webapp is judged by its reload generation against the one the
// reload was pushed for: at or below it the host has not reloaded yet (its
// two documents attach together, 2026-11-13 14:42:05.765), above it the host
// reloaded and ignored the push, and only that is an error.
func TestAHostReloadIsPushedOncePerInstalledBuild(t *testing.T) {
	tests := []struct {
		name string
		// pushedOn are the hosts standing when the reload is pushed, and
		// after the hosts standing on the edge under test.
		pushedOn, after []*agentreplv1.WebhostPage
		edge            string
		wantError       bool
		wantDebug       string
	}{
		{
			name:     "an Emacs attaching while the reload runs",
			pushedOn: hostPages("Old"), after: hostPages("Old"),
			edge: WebhostEdgeEmacsAttached, wantDebug: notYetReload,
		},
		{
			name:     "the host's second document attaching at the generation the reload was pushed for",
			pushedOn: []*agentreplv1.WebhostPage{hostPageAt("Old", 3)},
			after:    []*agentreplv1.WebhostPage{hostPageAt("Old", 3), hostPageAt("Old", 3)},
			edge:     WebhostEdgeHostAttached, wantDebug: notYetReload,
		},
		{
			name:     "a host that reloaded and still runs another webapp",
			pushedOn: []*agentreplv1.WebhostPage{hostPageAt("Old", 3)},
			after:    []*agentreplv1.WebhostPage{hostPageAt("Old", 4)},
			edge:     WebhostEdgeHostAttached, wantError: true,
		},
		{
			name:     "a host that ignored the reload, seen on an Emacs attaching",
			pushedOn: []*agentreplv1.WebhostPage{hostPageAt("Old", 3)},
			after:    []*agentreplv1.WebhostPage{hostPageAt("Old", 4)},
			edge:     WebhostEdgeEmacsAttached, wantDebug: "the web view host's reload onto the installed webapp is already pushed",
		},
		{
			name:     "a host that states no generation cannot be judged",
			pushedOn: hostPages("Old"), after: hostPages("Old"),
			edge: WebhostEdgeHostAttached, wantDebug: notYetReload,
		},
		{
			name:     "a generation against a push for hosts that stated none cannot be judged",
			pushedOn: hostPages("Old"),
			after:    []*agentreplv1.WebhostPage{hostPageAt("Old", 1)},
			edge:     WebhostEdgeHostAttached, wantDebug: notYetReload,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			installed := installedWebapp(t, h)
			h.clients.webhosts = tt.pushedOn
			h.clients.webhostReach = 1
			h.d.ReconcileWebhost(WebhostEdgeHostAttached)
			h.clients.webhosts = tt.after

			// Act
			got := h.d.ReconcileWebhost(tt.edge)

			// Assert
			if got != 0 || !slices.Equal(h.clients.webhostPush, []string{installed}) {
				t.Fatalf("reached = %d, reloads = %v; want the one reload alone", got, h.clients.webhostPush)
			}
			if failed := logged(h.log, "error", opWebhost, ignoredReload); failed != tt.wantError {
				t.Fatalf("error recorded = %v, want %v; records = %+v", failed, tt.wantError, h.log.Records())
			}
			if tt.wantDebug != "" && !logged(h.log, "debug", opWebhost, tt.wantDebug) {
				t.Fatalf("records = %+v, want %q at DEBUG", h.log.Records(), tt.wantDebug)
			}
		})
	}
}

func TestAPushRecordsTheHighestGenerationAStaleHostStoodAt(t *testing.T) {
	// Arrange: two stale documents at different generations, one on the
	// installed build.
	h := newHarness(t)
	installed := installedWebapp(t, h)
	h.clients.webhosts = []*agentreplv1.WebhostPage{hostPageAt("Old", 2), hostPageAt("Old", 5), hostPageAt(installed, 9)}
	h.clients.webhostReach = 1

	// Act
	h.d.ReconcileWebhost(WebhostEdgeHostAttached)

	// Assert
	h.d.webhost.mu.Lock()
	pushedFor := h.d.webhost.pushedFor
	h.d.webhost.mu.Unlock()
	if pushedFor == nil || *pushedFor != 5 {
		t.Fatalf("pushed for = %v, want the highest stale generation, 5", pushedFor)
	}
}

func TestAnUnreadableInstalledWebappReconcilesNothing(t *testing.T) {
	// Arrange: the installed webapp's entry is gone.
	h := newHarness(t)
	if err := os.RemoveAll(h.live.WebappDist()); err != nil && !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("remove the installed webapp: %v", err)
	}
	h.clients.webhosts = hostPages("Old")
	h.clients.webhostReach = 1

	// Act
	got := h.d.ReconcileWebhost(WebhostEdgeHostAttached)

	// Assert
	if got != 0 || len(h.clients.webhostPush) != 0 {
		t.Fatalf("reached = %d, reloads = %v; want nothing pushed", got, h.clients.webhostPush)
	}
	if !logged(h.log, "error", opWebhost, "could not read the installed webapp's build; the web view host is not reconciled") {
		t.Fatalf("records = %+v, want the unreadable build at ERROR", h.log.Records())
	}
}
