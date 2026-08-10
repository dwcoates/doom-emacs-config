package frontend

import (
	"sync"
	"testing"
	"time"
)

func TestCanonicalWorkspaceKey(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want string
	}{
		{name: "trailing separator is stripped", in: "/path/ws/", want: "/path/ws"},
		{name: "clean key is unchanged", in: "/path/ws", want: "/path/ws"},
		{name: "redundant separators collapse", in: "/path//ws///", want: "/path/ws"},
		{name: "dot element collapses", in: "/path/./ws", want: "/path/ws"},
		{name: "absent workspace stays absent", in: "", want: ""},
		{name: "root survives cleaning", in: "/", want: "/"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := canonicalWorkspaceKey(tc.in)

			// Assert.
			if got != tc.want {
				t.Fatalf("canonicalWorkspaceKey(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestWorkspaceKeyError(t *testing.T) {
	tests := []struct {
		name    string
		in      string
		wantErr bool
	}{
		{name: "canonical path is accepted", in: "/path/ws", wantErr: false},
		{name: "absent workspace is accepted", in: "", wantErr: false},
		{name: "key that cleans to nothing is refused", in: ".", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			err := workspaceKeyError(tc.in)

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("workspaceKeyError(%q) = %v, wantErr %v", tc.in, err, tc.wantErr)
			}
		})
	}
}

func TestNormalizeCommandWorkspaceRewritesTheFieldInPlace(t *testing.T) {
	// Arrange.
	cmd := openCmd("r1", "/path/ws/")

	// Act.
	normalizeCommandWorkspace(cmd)

	// Assert.
	if got := cmd.GetWorkspace(); got != "/path/ws" {
		t.Fatalf("workspace after normalize = %q, want %q", got, "/path/ws")
	}
}

func TestNormalizeCommandWorkspaceToleratesNilCommand(t *testing.T) {
	// Arrange / Act / Assert: a nil command must not panic on the ingress path.
	normalizeCommandWorkspace(nil)
}

func TestLaneKeyIsIdenticalForSlashedAndCleanSpellings(t *testing.T) {
	// Arrange.
	slashed := openCmd("r1", "/ws/a/")
	clean := openCmd("r2", "/ws/a")

	// Act: ingress canonicalization is what the lane key is computed from.
	normalizeCommandWorkspace(slashed)
	normalizeCommandWorkspace(clean)

	// Assert.
	if laneKey(slashed) != laneKey(clean) {
		t.Fatalf("laneKey(slashed) = %q, laneKey(clean) = %q, want identical", laneKey(slashed), laneKey(clean))
	}
}

func TestReadLoopRunsSlashedAndCleanSpellingsOnOneLane(t *testing.T) {
	// Arrange: the first open blocks until released. If the slashed spelling
	// took a lane of its own it would enter concurrently, so the census — not
	// a clock — is what proves they share a lane.
	h := newLaneHandler()
	entered := make(chan string, 2)
	release := make(chan struct{})
	var hold sync.Once
	h.open = func(ws string) error {
		entered <- ws
		hold.Do(func() { <-release })
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(openCmd("r-clean", "/ws/a"))
	harness.send(openCmd("r-slashed", "/ws/a/"))

	// Assert: the second command cannot enter while the first is held.
	select {
	case ws := <-entered:
		if ws != "/ws/a" {
			t.Fatalf("first handler workspace = %q, want %q", ws, "/ws/a")
		}
	case <-time.After(laneTestDeadline):
		t.Fatal("the first open never entered the handler")
	}
	// A lane of its own would show up as a second entry while the first is
	// still held. The window is a DETECTION window, never a synchronization
	// device: the ordering below is established by the release channel.
	select {
	case ws := <-entered:
		t.Fatalf("the slashed spelling entered concurrently as %q; it took a lane of its own", ws)
	case <-time.After(100 * time.Millisecond):
	}
	close(release)
	select {
	case ws := <-entered:
		if ws != "/ws/a" {
			t.Fatalf("second handler workspace = %q, want the canonical %q", ws, "/ws/a")
		}
	case <-time.After(laneTestDeadline):
		t.Fatal("the slashed spelling never ran")
	}
	if got := h.peakConcurrency("/ws/a"); got != 1 {
		t.Fatalf("peak concurrency on /ws/a = %d, want 1", got)
	}
	for i := 0; i < 2; i++ {
		if ack := harness.nextAck(); !ack.GetOk() {
			t.Fatalf("ack %s = nack %q, want ok", ack.GetRequestId(), ack.GetError())
		}
	}
}

func TestReadLoopRefusesAWorkspaceKeyThatCleansToNothing(t *testing.T) {
	// Arrange.
	h := newLaneHandler()
	h.open = func(ws string) error {
		t.Errorf("handler ran for refused workspace %q", ws)
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(openCmd("r-bad", "./"))

	// Assert.
	ack := harness.nextAck()
	if ack.GetOk() {
		t.Fatalf("ack = ok, want a refusal for an unaddressable workspace key")
	}
	if ack.GetRequestId() != "r-bad" {
		t.Fatalf("ack request_id = %q, want %q", ack.GetRequestId(), "r-bad")
	}
}

func TestReadLoopStillAcceptsWorkspacelessCommands(t *testing.T) {
	// Arrange: an empty workspace is a daemon-global command, not an invalid
	// key, and canonicalization must not turn it into one.
	h := newLaneHandler()
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(clientLogCmd("r-global"))

	// Assert.
	ack := harness.nextAck()
	if !ack.GetOk() {
		t.Fatalf("ack = nack %q, want ok for a workspace-less command", ack.GetError())
	}
}

func TestDispatchSeesTheCanonicalWorkspace(t *testing.T) {
	// Arrange.
	h := newLaneHandler()
	seen := make(chan string, 1)
	h.submit = func(ws, _ string) error {
		seen <- ws
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(submitCmd("r1", "/ws/a//"))

	// Assert.
	select {
	case ws := <-seen:
		if ws != "/ws/a" {
			t.Fatalf("handler workspace = %q, want the canonical %q", ws, "/ws/a")
		}
	case <-time.After(laneTestDeadline):
		t.Fatal("the handler never ran")
	}
}
