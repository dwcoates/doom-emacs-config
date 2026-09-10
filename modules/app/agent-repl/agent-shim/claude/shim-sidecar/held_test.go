package main

import (
	"io"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func TestUnownedSpoolIsHeldNotTailed(t *testing.T) {
	// Arrange: a spool nobody has claimed.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: tailing it would mean inventing an owner or reading the spool
	// path's runtime id as an identity.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("an unclaimed spool was tailed")
	}
}

func TestUnownedSpoolIsRetainedAcrossRescans(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the spawning call arrives on a later pass.
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
	h.sc.rescan()

	// Assert: it was held, never dropped, so it is tailed the moment it is
	// claimed.
	watched, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatal("a held spool was not picked up once its owner arrived")
	}
	if watched.target.WorkspaceDir != "/workspace" || watched.target.WorkspaceID != "workspace-id" || watched.target.ClaudeSessionID != "session-1" {
		t.Fatalf("spool attribution = %+v, want the spawning transcript's workspace and session", watched.target)
	}
}

func TestAgedUnownedSpoolIsIngestedAsResidue(t *testing.T) {
	// Arrange: a spool whose owner never arrives.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Assert: an aged unowned spool is never dropped; its bytes go to residue.
	watched, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatal("an aged unclaimed spool was dropped instead of ingested as residue")
	}
	if watched.target.Kind != tail.KindResidueSpool {
		t.Fatalf("kind = %s, want the residue spool kind", watched.target.Kind)
	}
}

func TestAgedUnownedSpoolKeepsBeingTailed(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Act: bytes appended after the demotion.
	h.sc.pollAll()
	before := h.sc.watchers[spool].tailer.Offset()
	h.write(t, spool, "hello\nmore\n")
	h.sc.pollAll()

	// Assert: nothing appended later is lost either.
	if got := h.sc.watchers[spool].tailer.Offset(); got <= before {
		t.Fatalf("offset = %d, want it past %d (a demoted spool keeps being tailed)", got, before)
	}
}

func TestAgedUnownedSpoolIsWarnedAboutOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)

	// Act.
	h.sc.rescan()
	h.sc.rescan()

	// Assert.
	if got := strings.Count(h.logText(), "spool unclaimed after"); got != 1 {
		t.Fatalf("the demotion was stated %d times, want once", got)
	}
}

func TestAResidueSpoolNeedsNoOwner(t *testing.T) {
	// Arrange: a spool whose task-id prefix already failed classification.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "q1", "hello\n")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: no owner would change what happens to it, so it is not held.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("an unclassifiable spool was held instead of ingested")
	}
}

func TestAConfigRootTargetNeedsNoOwner(t *testing.T) {
	// Arrange: the transcript IS its session's record.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-1", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatal("a session transcript was held for an owner it names itself")
	}
}

func TestAZeroHoldWindowKeepsTheDefault(t *testing.T) {
	// Arrange: zero is how the caller says "unset".
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "held-test"})

	// Act.
	held := newHeldSpools(0, log)

	// Assert.
	if held.window != UnownedSpoolWindow {
		t.Fatalf("hold window = %s, want the default %s", held.window, UnownedSpoolWindow)
	}
}

func TestAConfiguredHoldWindowReplacesTheDefault(t *testing.T) {
	// Arrange.
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "held-test"})

	// Act.
	held := newHeldSpools(15*time.Millisecond, log)

	// Assert.
	if held.window != 15*time.Millisecond {
		t.Fatalf("hold window = %s, want the configured 15ms", held.window)
	}
}
