package runtimemetrics

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/pageheld"
	"agent-repld/internal/sessionwhy"
)

// pagesCensus answers a census holding A working, K up for a keep-alive and
// I up in view.
func pagesCensus() sessionwhy.Census {
	table := sessionwhy.NewTable()
	table.SetOwner("k", sessionwhy.OwnerCycle)
	table.SetOwner("i", sessionwhy.OwnerInView)
	return sessionwhy.Census{
		Live: func() []ids.WorkspaceID { return []ids.WorkspaceID{"a", "k", "i"} },
		Work: func(ws ids.WorkspaceID) (sessionwhy.Work, bool) {
			return sessionwhy.Work{Turn: ws == "a"}, true
		},
		Table: table,
		Now:   func() time.Time { return time.Unix(1_700_000_000, 0) },
	}
}

func TestLoadedPagesAreServedByReasonAndSessionState(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAwake)
	pages.Held("k", pageheld.LoadSlot)
	pages.Held("i", pageheld.LoadShown)
	pages.Held("p", pageheld.LoadShown)
	pages.Held("n", pageheld.LoadSlot)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"a", "k", "i", "p", "n"}, false)
	recorded := func(_ context.Context, ws ids.WorkspaceID) (bool, error) { return ws == "p", nil }
	if err := s.RegisterPages(pages, pagesCensus(), recorded); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_pages_loaded{reason="awake",session="working"} 1`,
		`agentrepl_pages_loaded{reason="slot",session="keepalive"} 1`,
		`agentrepl_pages_loaded{reason="shown",session="idle"} 1`,
		`agentrepl_pages_loaded{reason="shown",session="parked"} 1`,
		`agentrepl_pages_loaded{reason="slot",session="no_session"} 1`,
	)
}

func TestTheLoadedPagesAreTheHostsFrames(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAwake)
	pages.Held("gone", pageheld.LoadShown)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"a", "k"}, false)
	none := func(context.Context, ids.WorkspaceID) (bool, error) { return false, nil }
	if err := s.RegisterPages(pages, pagesCensus(), none); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_pages_loaded{reason="awake",session="working"} 1`,
		`agentrepl_pages_loaded{reason="unreported",session="keepalive"} 1`,
	)
	if strings.Contains(body, PagesLoadedName+`{reason="shown"`) {
		t.Fatalf("a page Emacs reports but the host does not hold was served:\n%s", body)
	}
}

func TestNoPageIsServedBeforeTheHostsFirstCensus(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAwake)
	none := func(context.Context, ids.WorkspaceID) (bool, error) { return false, nil }
	if err := s.RegisterPages(pages, pagesCensus(), none); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, PagesLoadedName+"{") {
		t.Fatalf("a page was served before the host said what it holds:\n%s", body)
	}
}

func TestAPageWhoseSessionCannotBeReadIsOmittedAndSaidOnce(t *testing.T) {
	// Arrange
	s, logs := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("p", pageheld.LoadShown)
	pages.Held("a", pageheld.LoadAwake)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"p", "a"}, false)
	failing := func(context.Context, ids.WorkspaceID) (bool, error) { return false, errors.New("state closed") }
	if err := s.RegisterPages(pages, pagesCensus(), failing); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	scrape(t, s)
	body := scrape(t, s)

	// Assert
	mustContain(t, body, `agentrepl_pages_loaded{reason="awake",session="working"} 1`)
	if strings.Contains(body, `session="parked"`) {
		t.Fatalf("an unread page was served:\n%s", body)
	}
	var n int
	for _, rec := range logs.Records() {
		if rec.Level == "error" && strings.HasPrefix(rec.Message, "a loaded page's session could not be read") {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("errors recorded = %d, want one for the one cause", n)
	}
}

func TestPageLoadsAndUnloadsAreCountedFromZero(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)

	// Act
	pages.Loaded("a", pageheld.LoadShown)
	pages.Unloaded("a", pageheld.UnloadDormant)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_page_loads_total{reason="shown"} 1`,
		`agentrepl_page_loads_total{reason="slot"} 0`,
		`agentrepl_page_unloads_total{reason="dormant"} 1`,
		`agentrepl_page_unloads_total{reason="released"} 0`,
	)
}

func TestAnUndeclaredUnloadReasonPanics(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("an undeclared unload reason did not panic")
		}
	}()

	// Act
	s.CountPageUnload("whim")
}
