package deploy

import (
	"errors"
	"os"
	"slices"
	"testing"

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
		hosts     func(installed string) []string
		reach     int
		wantPush  bool
		wantReach int
	}{
		{name: "no host stands", hosts: func(string) []string { return nil }, reach: 1},
		{name: "a host on the installed webapp is left alone", hosts: func(installed string) []string { return []string{installed} }, reach: 1},
		{name: "a host on another webapp is reloaded onto the installed one", hosts: func(string) []string { return []string{"Old"} }, reach: 1, wantPush: true, wantReach: 1},
		{name: "a reload no Emacs stands to take is pushed but not spent", hosts: func(string) []string { return []string{"Old"} }, reach: 0, wantPush: true},
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
	h.clients.webhosts = []string{"Old"}
	h.d.ReconcileWebhost(WebhostEdgeHostAttached)
	h.clients.webhostReach = 1

	// Act
	got := h.d.ReconcileWebhost(WebhostEdgeEmacsAttached)

	// Assert
	if got != 1 || !slices.Equal(h.clients.webhostPush, []string{installed, installed}) {
		t.Fatalf("reached = %d, reloads = %v; want the reload pushed to the Emacs that attached", got, h.clients.webhostPush)
	}
}

// TestAHostReloadIsPushedOncePerInstalledBuild pins the bound: once pushed,
// a later edge pushes nothing more, and a host that ATTACHES still on another
// webapp did not reload, which is an error rather than another push.
func TestAHostReloadIsPushedOncePerInstalledBuild(t *testing.T) {
	tests := []struct {
		name      string
		edge      string
		wantError bool
	}{
		{name: "an Emacs attaching while the reload runs", edge: WebhostEdgeEmacsAttached},
		{name: "a host attaching still on another webapp", edge: WebhostEdgeHostAttached, wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			installed := installedWebapp(t, h)
			h.clients.webhosts = []string{"Old"}
			h.clients.webhostReach = 1
			h.d.ReconcileWebhost(WebhostEdgeHostAttached)

			// Act
			got := h.d.ReconcileWebhost(tt.edge)

			// Assert
			if got != 0 || !slices.Equal(h.clients.webhostPush, []string{installed}) {
				t.Fatalf("reached = %d, reloads = %v; want the one reload alone", got, h.clients.webhostPush)
			}
			failed := logged(h.log, "error", opWebhost, "a web view host attached on another webapp after its reload onto the installed one was pushed")
			if failed != tt.wantError {
				t.Fatalf("error recorded = %v, want %v; records = %+v", failed, tt.wantError, h.log.Records())
			}
		})
	}
}

func TestAnUnreadableInstalledWebappReconcilesNothing(t *testing.T) {
	// Arrange: the installed webapp's entry is gone.
	h := newHarness(t)
	if err := os.RemoveAll(h.live.WebappDist()); err != nil && !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("remove the installed webapp: %v", err)
	}
	h.clients.webhosts = []string{"Old"}
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
