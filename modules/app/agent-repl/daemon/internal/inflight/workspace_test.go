package inflight

import (
	"context"
	"errors"
	"strings"
	"testing"
)

// cancelled is a context already ended, for a wait that must not block.
func cancelled() context.Context {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	return ctx
}

func TestAwaitIdleFor(t *testing.T) {
	cases := []struct {
		name     string
		begin    func(tr *Tracker) func()
		wantIdle bool
	}{
		{name: "nothing in flight", begin: func(*Tracker) func() { return func() {} }, wantIdle: true},
		{name: "another workspace's work is not waited for", begin: func(tr *Tracker) func() { return tr.Workspace("other").Begin() }, wantIdle: true},
		{name: "the workspace's own work is waited for", begin: func(tr *Tracker) func() { return tr.Workspace("mine").Begin() }, wantIdle: false},
		{name: "work no workspace owns is waited for", begin: func(tr *Tracker) func() { return tr.Begin() }, wantIdle: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tr := &Tracker{}
			end := tc.begin(tr)
			defer end()

			// Act: a wait that cannot block answers whether it was idle.
			err := tr.AwaitIdleFor(cancelled(), []string{"mine"})

			// Assert
			if gotIdle := err == nil; gotIdle != tc.wantIdle {
				t.Fatalf("AwaitIdleFor = %v, want idle %v", err, tc.wantIdle)
			}
			if err != nil && !errors.Is(err, context.Canceled) {
				t.Fatalf("AwaitIdleFor = %v, want the context's end named", err)
			}
		})
	}
}

func TestAwaitIdleForWakesWhenTheWorkspacesWorkEnds(t *testing.T) {
	// Arrange: the workspace's work is in flight, and a wait is on it.
	tr := &Tracker{}
	end := tr.Workspace("mine").Begin()
	waiting := make(chan struct{})
	done := make(chan error, 1)
	go func() {
		close(waiting)
		done <- tr.AwaitIdleFor(context.Background(), []string{"mine"})
	}()
	<-waiting

	// Act
	end()

	// Assert: the wait answers idle, woken by the end.
	if err := <-done; err != nil {
		t.Fatalf("AwaitIdleFor = %v, want idle once the work ended", err)
	}
}

func TestAWorkspacesWorkIsCountedOnItsParent(t *testing.T) {
	// Arrange
	tr := &Tracker{}

	// Act
	end := tr.Workspace("mine").Begin()

	// Assert: the daemon-wide count and wait see it.
	if tr.InFlight() != 1 {
		t.Fatalf("parent InFlight = %d, want the workspace's work counted", tr.InFlight())
	}
	if err := tr.AwaitIdle(cancelled()); err == nil {
		t.Fatal("the daemon-wide wait answered idle with a workspace's work in flight")
	}
	end()
	if tr.InFlight() != 0 {
		t.Fatalf("parent InFlight = %d after the end, want 0", tr.InFlight())
	}
}

func TestInFlightForNamesTheUnownedAndTheWorkspacesOwnWork(t *testing.T) {
	// Arrange: one piece of each kind in flight.
	tr := &Tracker{}
	endUnowned := tr.Begin()
	endMine := tr.Workspace("mine").Begin()
	endOther := tr.Workspace("other").Begin()
	defer endUnowned()
	defer endMine()
	defer endOther()

	// Act
	n, sites := tr.InFlightFor([]string{"mine"})

	// Assert: two pieces, both begun in this file; the other workspace's is
	// not counted.
	if n != 2 {
		t.Fatalf("InFlightFor = %d, want the unowned work and the workspace's own", n)
	}
	total := 0
	for _, s := range sites {
		if !strings.HasPrefix(s, "inflight/workspace_test.go:") {
			t.Fatalf("site %q, want one in this file", s)
		}
		total++
	}
	if total != 2 {
		t.Fatalf("sites = %v, want two", sites)
	}
}

func TestTheSameWorkspaceAnswersTheSameChild(t *testing.T) {
	tr := &Tracker{}
	if tr.Workspace("mine") != tr.Workspace("mine") {
		t.Fatal("two asks for one workspace answered two trackers")
	}
}

func TestAWorkspaceTrackerWithNoWorkspaceIsRefused(t *testing.T) {
	// Arrange
	tr := &Tracker{}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("a workspace tracker with no workspace id did not panic")
		}
	}()

	// Act
	tr.Workspace("")
}
