// The HIBERNATION + CACHE KEEP-ALIVE harness: the shared vocabulary every
// acceptance test of that feature is written in.
//
// # Why the STORE is where a ping is observed
//
// A cache keep-alive ping is a prompt the DAEMON submits on the user's behalf,
// and the frozen contract says every frontend consumer excludes it
// unconditionally (core.proto PROMPT_ORIGIN_CACHE_KEEP_ALIVE: "conversation
// plumbing, not conversation"). So the frontend surface is precisely the place
// a ping must NOT be visible, and asserting on its absence there could never
// distinguish "the daemon excluded it" from "the daemon never sent it".
//
// The durable record is where it IS visible, and by contract must be: the shim
// writes a TurnStarted carrying turn_id and a durable copy of the accepted
// prompt_origin (core.proto TurnStarted.prompt_origin) into the shim-store for
// every prompt it accepts, keep-alive included — which is also the fact behind
// "keep-alive turn artifacts persist in the store, withheld rather than
// deleted". storeTail below therefore subscribes to the real shim-store the
// same way the shim's own client does, and every ping assertion in this feature
// is a statement about events in that seq space.
//
// # Why time is MOVED rather than waited out
//
// The keep-alive policy is time-since-based off a durable last-turn-end, with a
// one-hour default TTL. Nothing here sleeps: the daemon's wall clock is
// server.Config.Now (h.clock, e2e_test.go withTestClock) and the policy is
// evaluated by the idle sweeper, whose tick is injected (h.sweepIdle). A test
// therefore says "the session has now been idle for 5 minutes" and "check now"
// as two ordinary statements, and both are events it causes.
//
// # What is INTERPRETED here, and must be read as such
//
//   - THE POLICY IS EVALUATED ON THE IDLE SWEEP. The design fixes the trigger
//     as "an idle sweeper" and the daemon has exactly one (server.runIdleSweeper,
//     already injectable). These tests drive that one. A keep-alive loop that
//     ran on a second, separate ticker would need its own injection point and
//     these tests would fail with nothing arriving.
//   - THE CUTOFF IS BOTH ENV AND server.Config.IdleTimeout. The design names
//     AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS; the daemon already has an
//     -idle-timeout flag reaching the same gate. keepAlivePolicy.apply sets
//     BOTH to the same value so the two cannot disagree about this harness.
//
// Reuses e2e_test.go's building blocks READ-ONLY (newUDSHarness, dial,
// readFrame, writeCmd, frameTimeout), clearcompact_e2e_test.go's liveSession /
// storeSocket / awaitItem / isResult, and interrupt_e2e_test.go's awaitAll /
// ackFor.
package e2e

import (
	"encoding/json"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/types/known/anypb"

	"claude-repld/internal/session"

	"agentrepl/wire"
)

// --- the frozen configuration ------------------------------------------------

// The daemon's keep-alive knobs, by the names the design fixes them under.
const (
	envKeepAliveCacheTTL     = "AGENT_REPL_KEEPALIVE_CACHE_TTL_MS"
	envKeepAliveLeeway       = "AGENT_REPL_KEEPALIVE_LEEWAY_MS"
	envHibernateIdleCutoff   = "AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS"
	envUncachedCostAlertToks = "AGENT_REPL_UNCACHED_COST_ALERT_TOKENS"
)

// The defaults the design fixes, asserted by their own test so a silent change
// to any of them is a failure rather than a surprise in production.
const (
	defaultKeepAliveCacheTTL     = time.Hour
	defaultKeepAliveLeeway       = 2 * time.Minute
	defaultHibernateIdleCutoff   = 6 * time.Hour
	defaultUncachedCostAlertToks = 20_000
)

// keepAlivePingText is the ping, verbatim. The exact wording is contract, not
// decoration: it is what makes the vendor answer with a single token, and it is
// what a human reading a retained transcript recognizes as plumbing.
const keepAlivePingText = "respond to this message with only a '.'. make no tool calls or any other changes"

// keepAliveRetryFloor is how close to the TTL a retry may still be attempted
// ("retries allowed until TTL-60s"). Past it, pinging is pointless: the answer
// could not come back before the cache the ping exists to refresh has expired.
const keepAliveRetryFloor = time.Minute

// keepAlivePolicy is one test's keep-alive configuration.
//
// The numbers are minutes rather than the production hours purely for
// legibility in failure messages — the clock is injected, so a one-hour TTL
// would cost exactly the same as a ten-minute one and read worse.
type keepAlivePolicy struct {
	ttl        time.Duration
	leeway     time.Duration
	idleCutoff time.Duration
	costAlert  int64
}

// testKeepAlivePolicy is the shape every timing test in this feature uses.
//
// The relationships between the numbers are the ones the contract requires and
// are what make the edges distinguishable: the ping window opens at
// ttl-leeway (5m), the retry window closes at ttl-60s (9m), the cache is
// presumed cold at ttl (10m), and the idle cutoff (60m) is far enough beyond
// all of them that no timing test can trip it by accident.
func testKeepAlivePolicy() keepAlivePolicy {
	return keepAlivePolicy{
		ttl:        10 * time.Minute,
		leeway:     5 * time.Minute,
		idleCutoff: time.Hour,
		costAlert:  defaultUncachedCostAlertToks,
	}
}

// pingAt is the elapsed idleness at which a ping becomes due.
func (p keepAlivePolicy) pingAt() time.Duration { return p.ttl - p.leeway }

// retryUntil is an elapsed idleness at which a ping is still permitted, near
// the window's far end. The floor boundary itself (ttl - retry floor) is
// EXCLUSIVE — the daemon's predicate is elapsed >= ttl-floor — and the
// harness clock is an offset over live time that keeps flowing between the
// advance and the sweep, so a boundary-exact target inevitably lands a few
// milliseconds past the floor and is correctly declined. Aiming one second
// inside keeps the assertion about the window, not about scheduler latency.
func (p keepAlivePolicy) retryUntil() time.Duration {
	return p.ttl - keepAliveRetryFloor - time.Second
}

// apply installs the policy in the daemon's environment for one test.
//
// It must be called BEFORE the harness is built: the daemon reads its
// configuration once, and a test that moved a knob afterwards would be
// asserting against a value the daemon never saw.
func (p keepAlivePolicy) apply(t *testing.T) keepAlivePolicy {
	t.Helper()
	t.Setenv(envKeepAliveCacheTTL, fmt.Sprintf("%d", p.ttl.Milliseconds()))
	t.Setenv(envKeepAliveLeeway, fmt.Sprintf("%d", p.leeway.Milliseconds()))
	t.Setenv(envHibernateIdleCutoff, fmt.Sprintf("%d", p.idleCutoff.Milliseconds()))
	t.Setenv(envUncachedCostAlertToks, fmt.Sprintf("%d", p.costAlert))
	return p
}

// --- a live session with the keep-alive policy in force -----------------------

// keepAliveSession is one workspace brought up under a keep-alive policy, with
// everything an assertion about a ping needs already open.
type keepAliveSession struct {
	h         *e2eHarness
	policy    keepAlivePolicy
	sessionID string
	cwd       string
	// conn is the session's scoped /stream socket.
	conn *websocket.Conn
	// vendorID is the conversation uuid the store files this session under, as
	// of bring-up. A rewind MOVES it — tests that rewind re-read it.
	vendorID string
	// store tails that vendor seq space.
	store *storeTail
}

// newKeepAliveSession brings a workspace up with the keep-alive policy in
// force, runs ONE ordinary turn to completion, and returns with the store
// tailed.
//
// THE WARM-UP TURN IS THE PREMISE, not scaffolding. The whole policy is
// measured from the durable end of the last real turn, so a session that has
// never had one has no origin to measure from; the tests would then be
// asserting against whatever the daemon fell back to.
func newKeepAliveSession(t *testing.T, policy keepAlivePolicy, options ...harnessOption) *keepAliveSession {
	t.Helper()
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this tears the harness (and its shim processes) down before
	// the directory is removed.
	cwd := t.TempDir()
	return adoptKeepAliveSession(t, keepAliveHarness(t, policy, options...), policy, cwd)
}

// keepAliveHarness builds a daemon with the keep-alive policy in force, an
// injected sweep, a movable clock, and the policy's own idle cutoff.
func keepAliveHarness(t *testing.T, policy keepAlivePolicy, options ...harnessOption) *e2eHarness {
	t.Helper()
	policy.apply(t)
	return newUDSHarness(t, append([]harnessOption{
		withIdleSweeper(), withTestClock(), withIdleCutoff(policy.idleCutoff),
	}, options...)...)
}

// adoptKeepAliveSession brings cwd up on an existing keep-alive harness, runs
// one real turn, and tails the store.
func adoptKeepAliveSession(t *testing.T, h *e2eHarness, policy keepAlivePolicy, cwd string) *keepAliveSession {
	t.Helper()
	id, conn, vendorID, _ := liveSession(t, h, cwd)
	s := &keepAliveSession{h: h, policy: policy, sessionID: id, cwd: cwd, conn: conn, vendorID: vendorID}
	s.runRealTurn(t, "r-warmup", "warmup")
	s.store = tailStore(t, vendorID)
	return s
}

// unsetEnv removes key for the duration of the test and restores it after, so
// a test can assert what the daemon does with NO configuration — which is the
// only way the documented defaults are covered at all.
func unsetEnv(t *testing.T, key string) {
	t.Helper()
	previous, had := os.LookupEnv(key)
	if err := os.Unsetenv(key); err != nil {
		t.Fatalf("unset %s: %v", key, err)
	}
	t.Cleanup(func() {
		if had {
			_ = os.Setenv(key, previous)
			return
		}
		_ = os.Unsetenv(key)
	})
}

// runRealTurn submits an ordinary user prompt and returns once the turn has
// ENDED — the fake's reply plus its result item, the last item any turn
// produces. Returning on the end is what makes the caller's next statement
// ("the session has been idle for N") true.
func (s *keepAliveSession) runRealTurn(t *testing.T, requestID, text string) {
	t.Helper()
	writeCmd(t, s.conn, fmt.Sprintf(
		`{"requestId":%q,"submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`,
		requestID, text))
	awaitItem(t, s.conn, s.cwd, "the "+requestID+" turn's result item", isResult)
}

// idleFor claims the session has been idle for d, then runs ONE keep-alive
// check. Both halves are events this test causes: the daemon's clock moves, and
// the sweep it is measured by is fired.
func (s *keepAliveSession) idleFor(t *testing.T, d time.Duration) {
	t.Helper()
	s.checkAfterAdvancing(t, d)
}

// checkAfterAdvancing moves the daemon's clock forward by d and fires one
// sweep. The advance HAPPENS BEFORE the send, so the sweep it triggers cannot
// read the earlier time.
func (s *keepAliveSession) checkAfterAdvancing(t *testing.T, d time.Duration) {
	t.Helper()
	if s.h.clock == nil {
		t.Fatal("this session's harness has no test clock: build it through newKeepAliveSession")
	}
	s.h.clock.advance(d)
	select {
	case s.h.sweepIdle <- time.Now():
	case <-time.After(frameTimeout):
		t.Fatal("the daemon's idle sweeper never accepted a tick: nothing is evaluating the keep-alive policy")
	}
}

// hibernate forces this session asleep and returns the typed cause, failing if
// the daemon refuses. It is SETUP for the revival tests, so a refusal here is a
// broken premise rather than a finding.
func (s *keepAliveSession) hibernate(t *testing.T, requestID string) *frontendv1.HibernationDetail {
	t.Helper()
	sendHibernate(t, s.conn, requestID)
	// ONE combined await: the hibernated SessionView is pushed BEFORE the
	// CommandAck, so sequential awaitAck-then-awaitHibernationDetail loops
	// would consume and discard the view while hunting for the ack.
	var detail *frontendv1.HibernationDetail
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the forced hibernate's ack": func(frame *frontendv1.FrontendFrame) bool {
			ack := ackFor(frame, requestID)
			if ack == nil {
				return false
			}
			if !ack.GetOk() {
				t.Fatalf("hibernateWorkspace nacked while setting up a revival: %s", ack.GetError())
			}
			return true
		},
		"a SessionView reporting the session hibernated": func(frame *frontendv1.FrontendFrame) bool {
			view := sessionViewFor(frame, s.sessionID)
			if view == nil || !view.GetHibernated() {
				return false
			}
			detail = view.GetHibernation()
			return true
		},
	})
	return detail
}

// awaitAwake blocks until the session reports a live, attached shim again —
// the ordinary bring-up a revival performs.
func (s *keepAliveSession) awaitAwake(t *testing.T) {
	t.Helper()
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a SessionView reporting the revived session attached and awake": func(frame *frontendv1.FrontendFrame) bool {
			view := sessionViewFor(frame, s.sessionID)
			return view != nil && view.GetShimAttached() && !view.GetHibernated()
		},
	})
}

// --- tailing the durable record ----------------------------------------------

// storeTail is a live subscription to one vendor session's seq space in the
// real shim-store, opened the way the shim's own store client opens one: a
// Subscribe from 0, then replay-then-live-tail forever.
//
// It exists because the ping is deliberately invisible on the frontend, so the
// store is the only surface on which its presence OR absence is a statement
// about the daemon rather than about the frontend's exclusion rules.
type storeTail struct {
	conn   net.Conn
	events chan *corev1.Event

	mu     sync.Mutex
	failed error
}

// tailStore subscribes to vendorSessionID's seq space. The subscription is
// closed at test end.
func tailStore(t *testing.T, vendorSessionID string) *storeTail {
	t.Helper()
	conn, err := net.Dial("unix", storeSocket(t))
	if err != nil {
		t.Fatalf("dial the store to tail %s: %v", vendorSessionID, err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	if err := wire.WriteAny(conn, &corev1.Subscribe{SessionId: vendorSessionID, FromSeq: 0}); err != nil {
		t.Fatalf("subscribe to %s: %v", vendorSessionID, err)
	}
	// Generously buffered: a tail that blocked would apply backpressure to the
	// store and change the very timing the tests measure.
	s := &storeTail{conn: conn, events: make(chan *corev1.Event, 1024)}
	go s.read()
	return s
}

// read pumps the subscription. It records the terminating error rather than
// logging it: this goroutine outlives the test function, and t.Logf after the
// test has returned panics.
func (s *storeTail) read() {
	for {
		msg, err := wire.ReadAny(s.conn)
		if err != nil {
			s.mu.Lock()
			s.failed = err
			s.mu.Unlock()
			close(s.events)
			return
		}
		ev, ok := msg.(*corev1.Event)
		if !ok {
			// The store writes a Heartbeat as its end-of-replay marker; anything
			// else on this connection is not an observation.
			continue
		}
		s.events <- ev
	}
}

// await reads the subscription until an event satisfies match, and returns it.
func (s *storeTail) await(t *testing.T, what string, match func(*corev1.Event) bool) *corev1.Event {
	t.Helper()
	deadline := time.After(frameTimeout)
	for {
		select {
		case ev, ok := <-s.events:
			if !ok {
				s.mu.Lock()
				err := s.failed
				s.mu.Unlock()
				t.Fatalf("the store subscription ended before %s arrived: %v", what, err)
				return nil
			}
			if match(ev) {
				return ev
			}
		case <-deadline:
			t.Fatalf("no %s arrived in the durable record before the deadline", what)
			return nil
		}
	}
}

// awaitSentinel reads until sentinel is satisfied, failing the moment forbidden
// names an event that should not have been there.
//
// THE SENTINEL IS WHAT MAKES A NEGATIVE MEAN ANYTHING. The store assigns seq in
// arrival order and preserves it on every hop, so an event that WOULD have
// been written necessarily precedes one written after it. Reading up to a later
// event is therefore proof that a missing earlier one is absent rather than
// merely late — which no amount of waiting could establish.
func (s *storeTail) awaitSentinel(t *testing.T, what string, forbidden func(*corev1.Event) string, sentinel func(*corev1.Event) bool) {
	t.Helper()
	s.await(t, what, func(ev *corev1.Event) bool {
		if why := forbidden(ev); why != "" {
			t.Fatalf("forbidden durable event: %s", why)
		}
		return sentinel(ev)
	})
}

// --- durable-event predicates -------------------------------------------------

// turnStartOf returns the TurnStarted an event carries with the given prompt
// origin, or nil.
func turnStartOf(ev *corev1.Event, origin corev1.PromptOrigin) *corev1.TurnStarted {
	started := ev.GetTurnStarted()
	if started == nil || started.GetPromptOrigin() != origin {
		return nil
	}
	return started
}

// keepAlivePing returns the TurnStarted of a cache keep-alive ping, or nil.
func keepAlivePing(ev *corev1.Event) *corev1.TurnStarted {
	return turnStartOf(ev, corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE)
}

// userTurnStart returns the TurnStarted of an ordinary user prompt, or nil.
func userTurnStart(ev *corev1.Event) *corev1.TurnStarted {
	return turnStartOf(ev, corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
}

// turnEndedOf reports whether the event ends the named turn.
func turnEndedOf(ev *corev1.Event, turnID string) bool {
	return ev.GetTurnEnded() != nil && ev.GetTurnEnded().GetTurnId() == turnID
}

// noKeepAlivePing is the forbidden-event predicate every "must not ping"
// assertion is written with.
func noKeepAlivePing(why string) func(*corev1.Event) string {
	return func(ev *corev1.Event) string {
		if ping := keepAlivePing(ev); ping != nil {
			return fmt.Sprintf("a cache keep-alive ping (turn_id=%q, preview=%q) was submitted, but %s",
				ping.GetTurnId(), ping.GetPromptPreview(), why)
		}
		return ""
	}
}

// --- frontend accessors --------------------------------------------------------

// sessionViewFor returns the SessionView a frame carries for sessionID, or nil.
//
// It reads BOTH carriages: the live SessionView arm and a StateSnapshot's
// sessions list. Pushes are edge-triggered and a single-conn await loop that
// ran earlier may have consumed the edge while hunting for a different frame;
// the periodic snapshot redelivery makes every await ordering-robust instead
// of dependent on which helper read the conn first.
func sessionViewFor(frame *frontendv1.FrontendFrame, sessionID string) *frontendv1.SessionView {
	if sv, ok := frame.GetFrame().(*frontendv1.FrontendFrame_SessionView); ok {
		if sv.SessionView.GetSessionId() == sessionID {
			return sv.SessionView
		}
		return nil
	}
	if snap, ok := frame.GetFrame().(*frontendv1.FrontendFrame_Snapshot); ok {
		for _, sv := range snap.Snapshot.GetSessions() {
			if sv.GetSessionId() == sessionID {
				return sv
			}
		}
	}
	return nil
}

// snapshotSessionView returns the SessionView a StateSnapshot carries for
// sessionID, or nil.
func snapshotSessionView(snap *frontendv1.StateSnapshot, sessionID string) *frontendv1.SessionView {
	for _, view := range snap.GetSessions() {
		if view.GetSessionId() == sessionID {
			return view
		}
	}
	return nil
}

// awaitHibernationDetail reads conn until a SessionView reports the session
// hibernated, and returns its typed account.
//
// It requires the TYPED account rather than the `hibernated` bool: the bool is
// the compatibility projection (frontend.proto SessionView.hibernation), and a
// hibernation with no detail is a session the revival gate cannot explain.
func awaitHibernationDetail(t *testing.T, conn *websocket.Conn, sessionID string) *frontendv1.HibernationDetail {
	t.Helper()
	var detail *frontendv1.HibernationDetail
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a SessionView reporting the session hibernated": func(frame *frontendv1.FrontendFrame) bool {
			view := sessionViewFor(frame, sessionID)
			if view == nil || !view.GetHibernated() {
				return false
			}
			detail = view.GetHibernation()
			return true
		},
	})
	if detail == nil {
		t.Fatalf("session %s reports hibernated=true with no HibernationDetail: the bool is the projection of the typed account, so a hibernation with no cause is one the revival gate cannot explain to the user", sessionID)
	}
	return detail
}

// --- commands -------------------------------------------------------------------

// sendHibernate writes a HibernateWorkspaceCmd (FrontendCommand arm 28) on the
// workspace's own scoped socket.
func sendHibernate(t *testing.T, conn *websocket.Conn, requestID string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"hibernateWorkspace":{}}`, requestID))
}

// sendReviveDirect writes a ReviveSessionCmd (arm 29) choosing `direct`:
// resume the conversation as-is.
func sendReviveDirect(t *testing.T, conn *websocket.Conn, requestID string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"reviveSession":{"direct":{}}}`, requestID))
}

// sendReviveCompactFirst writes a ReviveSessionCmd choosing `compact_first`:
// compaction is the first order of business and prompts wait for it.
func sendReviveCompactFirst(t *testing.T, conn *websocket.Conn, requestID string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"reviveSession":{"compactFirst":{}}}`, requestID))
}

// awaitAck reads conn until the CommandAck for requestID arrives.
func awaitAck(t *testing.T, conn *websocket.Conn, requestID, what string) *frontendv1.CommandAck {
	t.Helper()
	var ack *frontendv1.CommandAck
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for " + what: func(frame *frontendv1.FrontendFrame) bool {
			ack = ackFor(frame, requestID)
			return ack != nil
		},
	})
	return ack
}

// --- the daemon's durable record of the conversation identity -------------------

// sessionRow is the registry's persisted account of one session, read straight
// out of the daemon's state database.
//
// The three fields together are what a rewind must move AS ONE: the vendor uuid
// the conversation continues under, and the two seq marks that are only
// meaningful within the seq space that uuid names. A flip that moved the uuid
// and left either mark behind would have the daemon resuming a brand-new seq
// space from a high-water mark belonging to a retired one — i.e. skipping the
// rewound conversation's entire history.
type sessionRow struct {
	claudeSessionID         string
	lastSeq                 uint64
	newestClearOrCompactSeq uint64
}

// readSessionRow reads sessionID's registry row.
func readSessionRow(t *testing.T, h *e2eHarness, sessionID string) sessionRow {
	t.Helper()
	var row sessionRow
	err := h.stateDB.QueryRow(
		`SELECT claude_session_id, last_seq, newest_clear_or_compact_seq FROM session_record WHERE session_id = ?`,
		sessionID,
	).Scan(&row.claudeSessionID, &row.lastSeq, &row.newestClearOrCompactSeq)
	if err != nil {
		t.Fatalf("read the registry row for session %s: %v", sessionID, err)
	}
	return row
}

// --- fixture transcripts ---------------------------------------------------------

// writeFixtureTranscript writes lines as the vendor transcript for
// (cwd, vendorSessionID), at the path the CLI itself would use.
//
// It resolves through session.TranscriptPath — the daemon's OWN encoding of the
// project directory — rather than assembling the path here, so a fixture can
// never be filed somewhere the code under test does not look. $HOME is the
// harness's isolated directory for the duration of the test (newUDSHarness), so
// nothing here can touch the operator's real transcripts.
func writeFixtureTranscript(t *testing.T, cwd, vendorSessionID string, lines []string) string {
	t.Helper()
	requireIsolatedHome(t, os.Getenv("HOME"))
	path := session.TranscriptPath(session.DefaultClaudeConfigDir(), cwd, vendorSessionID)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatalf("make the fixture transcript's project dir: %v", err)
	}
	if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
		t.Fatalf("write the fixture transcript %s: %v", path, err)
	}
	return path
}

// ingestTranscriptAsSidecar replays the truncated transcript copy the daemon
// wrote for vendorSessionID into the store as file-plane events — the backfill
// the production shim-sidecar performs by tailing the new file, which the
// harness (running no sidecar) must perform itself. Only the fixture shapes
// this feature writes are understood: a plain-string user line and a
// text-block assistant line; anything else is a loud failure rather than a
// silently dropped record.
func ingestTranscriptAsSidecar(t *testing.T, s *keepAliveSession, vendorSessionID string) {
	t.Helper()
	lines, ok := readFixtureTranscript(t, s.cwd, vendorSessionID)
	if !ok {
		t.Fatalf("no truncated transcript on disk for %s: the rewind should have written it", vendorSessionID)
	}
	store := dialStoreProducer(t)
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var rec struct {
			Type    string `json:"type"`
			UUID    string `json:"uuid"`
			Message struct {
				Content any `json:"content"`
			} `json:"message"`
		}
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("unparseable transcript line in the truncated copy: %v\n%s", err, line)
		}
		switch rec.Type {
		case "user":
			// Tool results are also `user` lines; the backfill's job is only
			// to make the retained conversation replayable, so their content
			// is carried as the placeholder text the assertions never match.
			text, ok := rec.Message.Content.(string)
			if !ok {
				text = "[tool result]"
			}
			store.write(sidecarUserLineEvent(t, vendorSessionID, rec.UUID, text))
		case "assistant":
			store.write(sidecarAssistantLineEvent(t, vendorSessionID, rec.UUID, assistantFixtureText(rec.Message.Content)))
		default:
			t.Fatalf("truncated copy carries a %q line the harness backfill does not model: %s", rec.Type, line)
		}
	}
}

// assistantFixtureText extracts the text of a fixture assistant line's first
// text block, or a placeholder for tool-use blocks — the backfill only makes
// the retained conversation replayable, and no assertion matches tool text.
func assistantFixtureText(content any) string {
	if blocks, ok := content.([]any); ok {
		for _, raw := range blocks {
			if block, ok := raw.(map[string]any); ok {
				if text, ok := block["text"].(string); ok {
					return text
				}
			}
		}
	}
	return "[tool use]"
}

// sidecarAssistantLineEvent is what handler.vendorEvent builds for a
// transcript `assistant` line, mirroring sidecarUserLineEvent
// (machinery_e2e_test.go): vendor Any carrying datav1.TranscriptLine, FILE
// plane, PERSISTENT, dedup key left for the store to derive.
func sidecarAssistantLineEvent(t *testing.T, vendorSessionID, lineUUID, text string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: lineUUID},
			Message: &datav1.ApiAssistantMessage{
				Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}}},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// readFixtureTranscript returns the lines of the transcript for
// (cwd, vendorSessionID), and whether it exists at all.
func readFixtureTranscript(t *testing.T, cwd, vendorSessionID string) ([]string, bool) {
	t.Helper()
	path := session.TranscriptPath(session.DefaultClaudeConfigDir(), cwd, vendorSessionID)
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, false
	}
	return strings.Split(strings.TrimRight(string(data), "\n"), "\n"), true
}
