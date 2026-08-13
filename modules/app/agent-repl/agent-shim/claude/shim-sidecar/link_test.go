// link_test.go pins the store-link state machine (link.go) as UNIT tests: a
// fake clock and a socket that is simply absent, with no store process anywhere.
//
// The contract under test is that the sidecar reads a watched file only while a
// store connection is established, and that a down link is a link that is BEING
// REDIALED for as long as it stays down. The regression it guards is the silent
// cold start — a store that was not listening yet made the sidecar re-read every
// watched file from offset 0, which re-ingested whole conversations.
//
// The redial ladder is driven by advancing the fake clock rather than by
// waiting, which is what makes a backoff ceiling measured in seconds testable at
// all.
package main

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
)

// downSidecar builds a sidecar whose store socket does not exist, so every dial
// fails for a real reason rather than a mocked one, and gives it a clock the
// test advances by hand.
func downSidecar(t *testing.T) (*sidecar, *time.Time, func() []string) {
	t.Helper()
	logf, read := capturingLog()
	s := newSidecar(filepath.Join(t.TempDir(), "absent.sock"), nil, t.TempDir(), logf)
	now := time.Date(2026, time.August, 5, 12, 0, 0, 0, time.UTC)
	s.now = func() time.Time { return now }
	// A fixed jitter keeps the ladder's arithmetic assertable; the spread itself
	// is covered separately.
	s.jitter = func(d time.Duration) time.Duration { return d }
	s.nextDialAt = now
	return s, &now, read
}

// While the link is down the sidecar produces NOTHING. A store that has not
// started yet is a down dependency, never a reason to read a file anyway.
func TestNoIngestionWorkRunsWhileTheLinkIsDown(t *testing.T) {
	// Arrange.
	s, _, _ := downSidecar(t)
	ran := false

	// Act.
	s.whenUp(func() { ran = true })

	// Assert.
	if ran {
		t.Fatal("ingestion work ran with the store link down, so a file was read with nowhere to put what it says")
	}
}

func TestIngestionWorkRunsOnceTheLinkIsUp(t *testing.T) {
	// Arrange.
	s, _, _ := downSidecar(t)
	s.link = linkUp
	ran := false

	// Act.
	s.whenUp(func() { ran = true })

	// Assert.
	if !ran {
		t.Fatal("ingestion work was skipped with the link up")
	}
}

// A tailer's read position may ONLY come from a cursor the store handed us on a
// live connection. Building one without that is the silent cold start, so it
// fails hard rather than quietly starting from zero.
func TestBuildingAReaderWithoutRecoveredCursorsFailsHard(t *testing.T) {
	tests := []struct {
		name    string
		prepare func(*sidecar)
	}{
		{name: "link down", prepare: func(s *sidecar) { s.link = linkDown }},
		{name: "link up but no cursors recovered", prepare: func(s *sidecar) { s.link = linkUp; s.cursors = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _, _ := downSidecar(t)
			tc.prepare(s)

			// Act / Assert.
			defer func() {
				if recover() == nil {
					t.Fatal("a reader was built without a cursor recovered on the live connection")
				}
			}()
			s.requireLinkUp("test")
		})
	}
}

// The ladder is a DEADLINE the loop compares against, not an event someone must
// remember to schedule. A tick before the deadline must not dial.
func TestNoDialBeforeTheArmedDeadline(t *testing.T) {
	// Arrange.
	s, now, _ := downSidecar(t)
	s.armDial(time.Second)

	// Act.
	s.dialDue()

	// Assert.
	if s.dialFailures != 0 {
		t.Fatalf("dial attempts = %d before the deadline, want 0", s.dialFailures)
	}
	*now = now.Add(2 * time.Second)
	s.dialDue()
	if s.dialFailures != 1 {
		t.Fatalf("dial attempts = %d after the deadline passed, want 1", s.dialFailures)
	}
}

// A link that is up is not redialed, however many ticks arrive.
func TestNoDialWhileTheLinkIsUp(t *testing.T) {
	// Arrange.
	s, _, _ := downSidecar(t)
	s.link = linkUp

	// Act.
	s.dialDue()

	// Assert.
	if s.dialFailures != 0 {
		t.Fatalf("dial attempts = %d with the link already up, want 0", s.dialFailures)
	}
}

// There is no attempt budget and no terminal state: a link that is down keeps
// being redialed, for as long as it stays down.
func TestRedialingNeverGivesUp(t *testing.T) {
	// Arrange.
	s, now, _ := downSidecar(t)

	// Act — far past any plausible budget.
	for i := 0; i < 50; i++ {
		s.dialDue()
		*now = now.Add(dialBackoffMax)
	}

	// Assert.
	if s.dialFailures != 50 {
		t.Fatalf("dial attempts = %d, want one per elapsed deadline", s.dialFailures)
	}
}

// A failed dial is loud, because "reading no files" is the whole file plane
// stopped: for the length of the backoff nothing on disk reaches the store.
func TestFailedDialIsReportedAsAnIngestionOutage(t *testing.T) {
	// Arrange.
	s, _, read := downSidecar(t)

	// Act.
	s.dialDue()

	// Assert.
	got := linesContaining(read(), "reading no files")
	if len(got) != 1 {
		t.Fatalf("outage lines = %v, want exactly 1", got)
	}
	if !strings.Contains(got[0], `"level":"warn"`) {
		t.Fatalf("outage line %q is not at warning level", got[0])
	}
}

// The backoff doubles from its floor and then HOLDS at the ceiling forever.
func TestBackoffDoublesToACeilingAndHolds(t *testing.T) {
	tests := []struct {
		name string
		in   time.Duration
		want time.Duration
	}{
		{name: "first failure arms the floor", in: 0, want: dialBackoffMin},
		{name: "each further failure doubles", in: dialBackoffMin, want: 2 * dialBackoffMin},
		{name: "doubling stops at the ceiling", in: dialBackoffMax, want: dialBackoffMax},
		{name: "a value past the ceiling is clamped", in: 2 * dialBackoffMax, want: dialBackoffMax},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := nextBackoff(tc.in)

			// Assert.
			if got != tc.want {
				t.Fatalf("nextBackoff(%s) = %s, want %s", tc.in, got, tc.want)
			}
		})
	}
}

// Jitter spreads each armed delay so a fleet of sidecars that lost the same
// store does not rediscover it in lockstep bursts. A zero delay — the immediate
// redial a fresh link loss arms — stays immediate.
func TestJitterSpreadsADelayButKeepsAnImmediateRedialImmediate(t *testing.T) {
	// Arrange.
	const base = time.Second
	low := time.Duration(float64(base) * (1 - dialJitterFraction))
	high := time.Duration(float64(base) * (1 + dialJitterFraction))

	// Act / Assert.
	if got := jitterBackoff(0); got != 0 {
		t.Fatalf("jitterBackoff(0) = %s, want an immediate redial", got)
	}
	for i := 0; i < 100; i++ {
		got := jitterBackoff(base)
		if got < low || got > high {
			t.Fatalf("jitterBackoff(%s) = %s, outside ±%.0f%%", base, got, dialJitterFraction*100)
		}
	}
}

// Losing the link drops the recovered cursors with it, so a tailer can never be
// built from a stale recovery, and arms an IMMEDIATE redial.
func TestLinkLossDropsTheRecoveredCursorsAndRedialsAtOnce(t *testing.T) {
	// Arrange.
	s, now, read := downSidecar(t)
	s.link = linkUp
	s.cursors = map[string]*agentshimv1.CursorState{}

	// Act.
	s.linkLost("poll")

	// Assert.
	if s.link != linkDown {
		t.Fatal("the link stayed up after it was lost")
	}
	if s.cursors != nil {
		t.Fatal("recovered cursors survived the link loss, so a tailer could be built from a stale recovery")
	}
	if s.nextDialAt.After(*now) {
		t.Fatalf("next dial armed at %s, want it immediate", s.nextDialAt)
	}
	if got := linesContaining(read(), "store link lost"); len(got) != 1 {
		t.Fatalf("link-loss lines = %v, want exactly 1", got)
	}
}

// A single poll pass can surface the same dead connection through several failed
// writes, so tearing the link down must be idempotent.
func TestRepeatedLinkLossIsReportedOnce(t *testing.T) {
	// Arrange.
	s, _, read := downSidecar(t)
	s.link = linkUp

	// Act.
	s.linkLost("poll")
	s.linkLost("poll")
	s.linkLost("heartbeat")

	// Assert.
	if got := linesContaining(read(), "store link lost"); len(got) != 1 {
		t.Fatalf("link-loss lines = %v, want exactly 1", got)
	}
}

// A store REJECTION arrives on a healthy connection and is NOT a link loss, so
// the connection's own liveness decides rather than the presence of an error.
func TestAStoreErrorOnALiveConnectionIsNotALinkLoss(t *testing.T) {
	// Arrange.
	s, _, _ := downSidecar(t)
	s.link = linkUp

	// Act — the client never connected, so Connected() is false and this DOES
	// tear the link down; a nil error must not.
	s.noteStoreErr("write", nil)

	// Assert.
	if s.link != linkUp {
		t.Fatal("a nil error tore the link down")
	}
	s.noteStoreErr("write", errors.New("transport gone"))
	if s.link != linkDown {
		t.Fatal("a transport error on a dead connection did not tear the link down")
	}
}

// The outage is reported once per session the sidecar watches, because the store
// fans a record out to that session's subscribers and nobody else.
func TestOutageIsReportedPerWatchedSession(t *testing.T) {
	// Arrange.
	sessions := []string{"s1", "s2"}

	// Act.
	entries := degradedWindowEvents(sessions, "store unreachable for 900ms")

	// Assert.
	if len(entries) != len(sessions) {
		t.Fatalf("outage records = %d, want one per session (%d)", len(entries), len(sessions))
	}
	for i, entry := range entries {
		external := entry.GetExternal()
		if external.GetSessionId() != sessions[i] {
			t.Fatalf("record %d session = %q, want %q", i, external.GetSessionId(), sessions[i])
		}
		diagnostic := external.GetBookkeeping().GetProducerDiagnostic()
		if diagnostic == nil {
			t.Fatalf("record %d is not a producer diagnostic", i)
		}
		if diagnostic.GetDetail() != "store unreachable for 900ms" {
			t.Fatalf("record %d detail = %q, want the outage reason verbatim", i, diagnostic.GetDetail())
		}
	}
}

// The write identity names the OUTAGE rather than the moment it was reported, so
// the same outage reported twice is one record at the store.
func TestOutageReportIdentityNamesTheOutage(t *testing.T) {
	// Arrange / Act.
	first := degradedWindowEvents([]string{"s1"}, "store unreachable for 900ms")
	second := degradedWindowEvents([]string{"s1"}, "store unreachable for 900ms")
	different := degradedWindowEvents([]string{"s1"}, "store unreachable for 5000ms")

	// Assert.
	if first[0].GetInternal().GetWriteId() != second[0].GetInternal().GetWriteId() {
		t.Fatal("one outage reported twice minted two write identities")
	}
	if first[0].GetInternal().GetWriteId() == different[0].GetInternal().GetWriteId() {
		t.Fatal("two different outages share one write identity, so the second would be dropped")
	}
}

// A link that came up on its first attempt spent no time down and reports
// nothing.
func TestNoOutageIsReportedWhenTheLinkNeverFailed(t *testing.T) {
	// Arrange.
	s, _, read := downSidecar(t)
	s.dialFailures = 0

	// Act.
	s.reportOutageClosed()

	// Assert.
	if got := linesContaining(read(), "store link recovered"); len(got) != 0 {
		t.Fatalf("recovery lines = %v, want none for a link that never failed", got)
	}
}

// A degraded report can only reach the sessions this sidecar watches, and each
// one exactly once.
func TestWatchedSessionsAreListedWithoutDuplicates(t *testing.T) {
	// Arrange.
	s, _, _ := downSidecar(t)
	s.watchers = map[string]*watched{
		"/a": {sessionID: "s1"},
		"/b": {sessionID: "s1"},
		"/c": {sessionID: "s2"},
		"/d": {sessionID: ""},
	}

	// Act.
	got := s.watchedSessions()

	// Assert.
	if len(got) != 2 {
		t.Fatalf("watched sessions = %v, want s1 and s2 once each", got)
	}
	seen := map[string]bool{}
	for _, id := range got {
		if id == "" {
			t.Fatal("an unattributed watcher was listed as a reachable session")
		}
		if seen[id] {
			t.Fatalf("session %q listed twice", id)
		}
		seen[id] = true
	}
}

// A write with the link down never dials implicitly: reopening a connection
// under a write would skip the cursor recovery every connection must perform.
func TestStoreWriteWithNoConnectionFailsRatherThanDialing(t *testing.T) {
	// Arrange.
	s, _, _ := downSidecar(t)

	// Act.
	err := s.storeWrite("test batch", nil)

	// Assert.
	if err == nil {
		t.Fatal("a write with no producer connection reported success")
	}
	if s.store.Connected() {
		t.Fatal("a write dialed the store implicitly, bypassing cursor recovery")
	}
}
