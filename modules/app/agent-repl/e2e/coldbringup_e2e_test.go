// coldbringup_e2e_test.go — the ORDERING between an agent's registration in
// the store and the first reading session opened on it.
//
// A book comes into existence when the first write names its agent
// (agent-shim/shim-store/AGENTS.md, and internal/db/read.go's OpenPage: "AN
// AGENT ID THE STORE HOLDS NO `agent` ROW FOR NAMES NO BOOK AND IS REFUSED").
// The endpoint contract has the daemon open the main agent's watch as soon as a
// session is up and BEFORE any turn (daemon/internal/sessionwatcher's
// openMainLocked), so on a cold bring-up that watch names a book that provably
// does not exist yet.
//
// The store is right to refuse such an open and right to log the refusal at
// warn. What must not happen is the ASK: the shim minted the AgentId moments
// earlier and has written nothing under it, so it already holds the answer. A
// warning that fires on every healthy bring-up is a defect in the caller's
// ordering, and these tests are what keep it fixed — they assert on the REAL
// store's own log, the same file the defect was read out of.
//
// The bring-up is the first prompt, because that is what brings a session up:
// SelectWorkspace records a selection and starts nothing. The window the defect
// lived in is still entered, and deterministically — the session watcher is
// started INSIDE the bring-up (daemon/internal/workspace/sessions.go, which
// starts it before recording the session's facts), so the main agent's
// WatchAgent open strictly precedes the StartTurn that writes the prompt.
package e2e

import (
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// cbuStoreOpenRefusals is every `unknown_agent` refusal the REAL store logged
// for OpenAgentSession, read out of the store's own log file.
//
// The refusal's own vocabulary is what it matches on — the operation plus
// `refusal_site` — never the prose detail, which is a sentence a store
// maintainer may reword at any time.
func cbuStoreOpenRefusals(t *testing.T, w *World) []harness.LogRecord {
	t.Helper()
	var out []harness.LogRecord
	for _, rec := range harness.ReadLog(t, w.Store.LogPath) {
		if rec.Operation != "store.rpc.open-agent-session" {
			continue
		}
		if site, _ := rec.Context["refusal_site"].(string); site == "unknown_agent" {
			out = append(out, rec)
		}
	}
	return out
}

// cbuAwaitServedWithoutAsking waits for the shim's own statement, in the
// workspace's SHIM log sink, that it answered the main agent's watch WITHOUT
// asking the store for a book. It is the synchronization point the refusal
// assertion needs: without it, a store log read before the open had happened
// would pass for the wrong reason.
func cbuAwaitServedWithoutAsking(t *testing.T, w *World, workspaceDir string) harness.LogRecord {
	t.Helper()
	return w.Daemon.AwaitLogRecord(
		harness.WorkspaceLogPath(workspaceDir, "shim"),
		"the main agent's book served without asking the store",
		func(rec harness.LogRecord) bool { return strings.Contains(rec.Message, "no book was asked for") },
	)
}

// cbuBringUp registers a fresh workspace and brings its session up cold,
// through the first prompt, waiting until that turn has ended. It returns the
// workspace's directory.
func cbuBringUp(t *testing.T, w *World) string {
	t.Helper()
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	// Plain prose, which the fake registry falls through to its PROSE scenario
	// for: this suite is about the bring-up BEFORE the turn, and a scenario with
	// its own machinery would only add writes to reason about.
	turn := SubmitPrompt(t, w, ws, "say something")
	AwaitTurnEnded(t, w, ws, turn)
	return repo.Dir
}

// TestColdBringUpEarnsNoStoreRefusal is the defect itself: a cold bring-up must
// leave the store's log with no `unknown_agent` refusal in it at all.
func TestColdBringUpEarnsNoStoreRefusal(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})

	// Act.
	workspaceDir := cbuBringUp(t, w)
	// The record plane's own statement that it reached the decision — without
	// it the assertion below could pass merely by running before the open.
	cbuAwaitServedWithoutAsking(t, w, workspaceDir)

	// Assert.
	if refusals := cbuStoreOpenRefusals(t, w); len(refusals) != 0 {
		t.Fatalf("the store logged %d unknown_agent refusals on a cold bring-up, want 0; first: %s",
			len(refusals), refusals[0].Raw)
	}
}

// TestColdBringUpServesTheMainBookWithoutAsking is the MECHANISM the test above
// depends on: the shim states that it answered the main agent's watch from what
// it already knew, rather than by asking a store that would have to refuse.
func TestColdBringUpServesTheMainBookWithoutAsking(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})

	// Act.
	workspaceDir := cbuBringUp(t, w)

	// Assert.
	rec := cbuAwaitServedWithoutAsking(t, w, workspaceDir)
	if rec.Level == "warn" || rec.Level == "error" {
		t.Fatalf("the ordering the contract asks for was logged at %q, want an ordinary record: %s", rec.Level, rec.Raw)
	}
}
