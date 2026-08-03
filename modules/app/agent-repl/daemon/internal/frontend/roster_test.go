package frontend

import (
	"bufio"
	"context"
	"fmt"
	"net"
	"strconv"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/frontend/rostertest"
)

// testBootID is the publisher epoch every single-epoch test publishes under.
// Tests that care about the epoch boundary name their own ids instead.
const testBootID = "boot-a"

// validRoster builds the shared contract-legal fixture at the given revision,
// under the default publisher epoch.
//
// It is a one-line binding of the default epoch onto rostertest.ValidRoster,
// NOT a second fixture: this suite owns no roster shape of its own, so a
// contract change lands in rostertest and reaches every suite at once. The
// illegal shapes come from rostertest's named options, so every deliberate
// contract breach is visible as a helper name at the call site.
func validRoster(revision int64, opts ...rostertest.Option) *frontendv1.WorkspaceRoster {
	return rostertest.ValidRoster(testBootID, revision, opts...)
}

// validRosterInEpoch builds the same fixture under a named publisher epoch.
func validRosterInEpoch(bootID string, revision int64, opts ...rostertest.Option) *frontendv1.WorkspaceRoster {
	return rostertest.ValidRoster(bootID, revision, opts...)
}

// publishRosterCmd wraps a roster in the frozen command arm.
func publishRosterCmd(requestID string, roster *frontendv1.WorkspaceRoster) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID,
		Command: &frontendv1.FrontendCommand_PublishWorkspaceRoster{
			PublishWorkspaceRoster: &frontendv1.PublishWorkspaceRosterCmd{Roster: roster},
		},
	}
}

// --- the drift guard --------------------------------------------------------

// TestTheSharedRosterFixtureSatisfiesTheValidator is the guard that makes
// fixture-vs-contract drift impossible rather than merely unlikely.
//
// Every suite in the daemon — this one and the e2e roster suite — publishes
// rosters built by rostertest.ValidRoster. This test checks that fixture
// against validateRoster itself, in the package where validateRoster lives. So
// a future commit that tightens the validator (as the boot_id requirement did)
// fails HERE, in the daemon's own unit suite, at the amendment commit — instead
// of surfacing later as a wave of red e2e tests whose fixtures were left behind.
//
// The bootID cases are both epochs the suites use, because bootID is a
// parameter of the fixture and a fixture legal in one epoch must be legal in
// any.
func TestTheSharedRosterFixtureSatisfiesTheValidator(t *testing.T) {
	tests := []struct {
		name     string
		bootID   string
		revision int64
	}{
		{name: "the default epoch at the first revision", bootID: testBootID, revision: 1},
		{name: "a named epoch at a later revision", bootID: "boot-new", revision: 500},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			roster := rostertest.ValidRoster(tt.bootID, tt.revision)

			// Act.
			err := validateRoster(roster)

			// Assert.
			if err != nil {
				t.Fatalf("the shared roster fixture no longer satisfies validateRoster: %v\n"+
					"the validator was tightened without amending rostertest.ValidRoster; amend the fixture in one place and every suite follows", err)
			}
		})
	}
}

// --- publish: the accepted path --------------------------------------------

// TestDispatchAcksRosterPublish covers the happy path: a well-formed first
// roster is accepted and acknowledged, and never reaches the CommandHandler
// (the transport, not the handler, is the roster's retainer).
func TestDispatchAcksRosterPublish(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)

	// Act.
	ack := Dispatch(context.Background(), testLogf(t), h, s, publishRosterCmd("r-1", validRoster(1)))

	// Assert.
	if !ack.GetOk() {
		t.Fatalf("publish ack = %+v, want ok", ack)
	}
	if ack.GetRequestId() != "r-1" {
		t.Errorf("ack request_id = %q, want r-1", ack.GetRequestId())
	}
	if h.called != "" {
		t.Errorf("roster publish reached CommandHandler %q, want the retainer instead", h.called)
	}
}

// TestPublishWorkspaceRosterRetainsTheRoster covers retention: the accepted
// roster becomes the one a later connect will be handed.
func TestPublishWorkspaceRosterRetainsTheRoster(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)

	// Act.
	if err := s.PublishWorkspaceRoster(validRoster(7)); err != nil {
		t.Fatalf("PublishWorkspaceRoster: %v", err)
	}

	// Assert.
	s.mu.Lock()
	held := s.retainedRosterLocked()
	s.mu.Unlock()
	if held.GetRevision() != 7 {
		t.Errorf("retained revision = %d, want 7", held.GetRevision())
	}
}

// TestPublishWorkspaceRosterAdvancesTheRetainedRevision covers the ordinary
// republish: a strictly newer roster replaces the held one wholesale.
func TestPublishWorkspaceRosterAdvancesTheRetainedRevision(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	if err := s.PublishWorkspaceRoster(validRoster(1)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}

	// Act.
	if err := s.PublishWorkspaceRoster(validRoster(2)); err != nil {
		t.Fatalf("PublishWorkspaceRoster: %v", err)
	}

	// Assert.
	s.mu.Lock()
	held := s.retainedRosterLocked()
	s.mu.Unlock()
	if held.GetRevision() != 2 {
		t.Errorf("retained revision = %d, want 2", held.GetRevision())
	}
}

// --- publish: the refusals --------------------------------------------------

// TestPublishWorkspaceRosterRefusesAStaleRevision covers the monotonicity rule
// WITHIN one publisher epoch (both publishes carry the same boot_id). The
// refusal must name BOTH revisions: a publisher that cannot see which revision
// beat it cannot tell a lost race from a bug.
func TestPublishWorkspaceRosterRefusesAStaleRevision(t *testing.T) {
	tests := []struct {
		name     string
		retained int64
		offered  int64
	}{
		{name: "older revision", retained: 5, offered: 4},
		{name: "equal revision", retained: 5, offered: 5},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, _ := newTestServer(t, 0)
			if err := s.PublishWorkspaceRoster(validRoster(tt.retained)); err != nil {
				t.Fatalf("seed publish: %v", err)
			}

			// Act.
			err := s.PublishWorkspaceRoster(validRoster(tt.offered))

			// Assert.
			if err == nil {
				t.Fatal("PublishWorkspaceRoster = nil, want a refusal")
			}
			for _, want := range []string{"revision=" + strconv.FormatInt(tt.offered, 10), "revision=" + strconv.FormatInt(tt.retained, 10)} {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("refusal %q does not name %q", err, want)
				}
			}
			s.mu.Lock()
			held := s.retainedRosterLocked()
			s.mu.Unlock()
			if held.GetRevision() != tt.retained {
				t.Errorf("retained revision = %d, want the retained %d untouched", held.GetRevision(), tt.retained)
			}
		})
	}
}

// TestDispatchNacksAStaleRosterRevision covers the stale publication reaching
// a client: it is a loud Nack on the wire, never a silent drop.
func TestDispatchNacksAStaleRosterRevision(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)
	if err := s.PublishWorkspaceRoster(validRoster(9)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}

	// Act.
	ack := Dispatch(context.Background(), testLogf(t), h, s, publishRosterCmd("r-2", validRoster(8)))

	// Assert.
	if ack.GetOk() {
		t.Fatalf("stale publish ack = %+v, want a Nack", ack)
	}
	if !strings.Contains(ack.GetError(), "revision=8") || !strings.Contains(ack.GetError(), "revision=9") {
		t.Errorf("Nack %q does not name both the offered and retained revisions", ack.GetError())
	}
}

// --- publish: the publisher epoch -------------------------------------------

// TestPublishWorkspaceRosterAcceptsALowerRevisionFromANewEpoch covers the whole
// point of boot_id: a fresh Emacs boot publishing revision=1 against a daemon
// still retaining revision=500 from the previous boot is ACCEPTED, because the
// two counters were minted by different publishers and are not comparable.
// Without this the new boot would be refused forever, with no honest resync
// available (a nack carries no retained revision to resync against).
func TestPublishWorkspaceRosterAcceptsALowerRevisionFromANewEpoch(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	if err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-old", 500)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}

	// Act.
	err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-new", 1))

	// Assert.
	if err != nil {
		t.Fatalf("PublishWorkspaceRoster from a new epoch = %v, want acceptance", err)
	}
	s.mu.Lock()
	held := s.retainedRosterLocked()
	s.mu.Unlock()
	if held.GetBootId() != "boot-new" || held.GetRevision() != 1 {
		t.Errorf("retained boot_id=%q revision=%d, want boot-new/1", held.GetBootId(), held.GetRevision())
	}
}

// TestPublishWorkspaceRosterResetsTheFloorToTheNewEpoch covers the floor reset:
// after an epoch change the monotonicity rule applies against the NEW epoch's
// revision, so the old epoch's high-water mark no longer gates anything.
func TestPublishWorkspaceRosterResetsTheFloorToTheNewEpoch(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	if err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-old", 500)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}
	if err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-new", 3)); err != nil {
		t.Fatalf("epoch-change publish: %v", err)
	}

	// Act.
	err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-new", 2))

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal against the new epoch's floor")
	}
	if !strings.Contains(err.Error(), "revision=2") || !strings.Contains(err.Error(), "revision=3") {
		t.Errorf("refusal %q does not name both the offered and the new epoch's retained revision", err)
	}
}

// TestPublishWorkspaceRosterLogsTheEpochChange covers the observability of a
// floor reset: an epoch change discards a monotonicity guarantee, so it is
// recorded loudly through the canonical log rather than passing silently.
func TestPublishWorkspaceRosterLogsTheEpochChange(t *testing.T) {
	// Arrange.
	var logged []string
	s := New(Config{
		Logf:        func(format string, args ...any) { logged = append(logged, fmt.Sprintf(format, args...)) },
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()},
		Handler:     &mockHandler{},
	})
	if err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-old", 500)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}

	// Act.
	if err := s.PublishWorkspaceRoster(validRosterInEpoch("boot-new", 1)); err != nil {
		t.Fatalf("epoch-change publish: %v", err)
	}

	// Assert.
	joined := strings.Join(logged, "\n")
	for _, want := range []string{"epoch changed", `boot_id="boot-old"`, "revision=500", `boot_id="boot-new"`, "revision=1"} {
		if !strings.Contains(joined, want) {
			t.Errorf("epoch-change log %q does not name %q", joined, want)
		}
	}
}

// TestPublishWorkspaceRosterRefusesAnEmptyBootId covers the epoch-identity
// contract: a publisher that will not name its epoch cannot be told apart from
// one whose epoch ended, so it is refused rather than folded into the held one.
func TestPublishWorkspaceRosterRefusesAnEmptyBootId(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	roster := validRosterInEpoch("", 1)

	// Act.
	err := s.PublishWorkspaceRoster(roster)

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal for an empty boot_id")
	}
	if !strings.Contains(err.Error(), "empty boot_id") {
		t.Errorf("refusal %q does not name the missing epoch identity", err)
	}
}

// TestDispatchNacksARosterWithNoBootId covers the empty-epoch publication
// reaching a client: a loud Nack on the wire, never a silent drop.
func TestDispatchNacksARosterWithNoBootId(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)

	// Act.
	ack := Dispatch(context.Background(), testLogf(t), h, s, publishRosterCmd("r-3", validRosterInEpoch("", 1)))

	// Assert.
	if ack.GetOk() {
		t.Fatalf("boot_id-less publish ack = %+v, want a Nack", ack)
	}
	if !strings.Contains(ack.GetError(), "empty boot_id") {
		t.Errorf("Nack %q does not name the missing epoch identity", ack.GetError())
	}
}

// TestPublishWorkspaceRosterRefusesARowWithNoStatus covers the row contract:
// the SET ARM IS THE STATUS, so an unset oneof is a row with no lifecycle and
// is refused rather than defaulted to one.
func TestPublishWorkspaceRosterRefusesARowWithNoStatus(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	roster := validRoster(1, rostertest.WithStatuslessRow())

	// Act.
	err := s.PublishWorkspaceRoster(roster)

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal for a statusless row")
	}
	if !strings.Contains(err.Error(), rostertest.MergingRowDir) || !strings.Contains(err.Error(), "no status set") {
		t.Errorf("refusal %q does not name the statusless row", err)
	}
}

// TestPublishWorkspaceRosterRefusesAChildRowWithNoStatus covers the recursive
// case: a child row is a row and is held to the same standard.
func TestPublishWorkspaceRosterRefusesAChildRowWithNoStatus(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)

	// Act.
	err := s.PublishWorkspaceRoster(validRoster(1, rostertest.WithStatuslessChildRow()))

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal for a statusless child row")
	}
	if !strings.Contains(err.Error(), rostertest.ChildRowDir) {
		t.Errorf("refusal %q does not name the statusless child", err)
	}
}

// TestPublishWorkspaceRosterRefusesARecentlyMergedRowWithNoStatus covers the
// hoisted section, which is reachable under both views and so is validated
// independently of whichever view arm is set.
func TestPublishWorkspaceRosterRefusesARecentlyMergedRowWithNoStatus(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	roster := validRoster(1, rostertest.WithStatuslessRecentlyMergedRow())

	// Act.
	err := s.PublishWorkspaceRoster(roster)

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal")
	}
	if !strings.Contains(err.Error(), "recently_merged") {
		t.Errorf("refusal %q does not name the recently_merged section", err)
	}
}

// TestPublishWorkspaceRosterRefusesAnUnsetView covers the view contract: with
// no arm set there is no grouping, which is not the same thing as an empty one.
func TestPublishWorkspaceRosterRefusesAnUnsetView(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)

	// Act.
	err := s.PublishWorkspaceRoster(validRoster(1, rostertest.WithoutView()))

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal for an unset view")
	}
	if !strings.Contains(err.Error(), "no view set") {
		t.Errorf("refusal %q does not name the unset view", err)
	}
}

// TestPublishWorkspaceRosterRefusesANonPositiveRevision covers the revision
// floor: revisions start at 1, so 0 is an unset field rather than a first
// publication.
func TestPublishWorkspaceRosterRefusesANonPositiveRevision(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)

	// Act.
	err := s.PublishWorkspaceRoster(validRoster(0))

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster = nil, want a refusal for revision 0")
	}
	if !strings.Contains(err.Error(), "must be positive") {
		t.Errorf("refusal %q does not name the revision floor", err)
	}
}

// TestPublishWorkspaceRosterRefusesANilRoster covers the empty command payload:
// a publish with no roster in it is a refusal, not a nil-deref.
func TestPublishWorkspaceRosterRefusesANilRoster(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)

	// Act.
	err := s.PublishWorkspaceRoster(nil)

	// Assert.
	if err == nil {
		t.Fatal("PublishWorkspaceRoster(nil) = nil, want a refusal")
	}
}

// --- fan-out ----------------------------------------------------------------

// TestPublishWorkspaceRosterBroadcastsToAConnectedClient covers fan-out: an
// accepted roster reaches an already-connected client as a workspace_roster
// frame.
func TestPublishWorkspaceRosterBroadcastsToAConnectedClient(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	l, err := net.Listen("unix", shortSock(t, "rb.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	defer s.Close()
	conn, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)
	if frame := readFrame(t, r); frame.GetSnapshot() == nil {
		t.Fatalf("first frame was not a snapshot: %v", frame)
	}

	// Act: publish over the wire, so the ack proves the client is registered
	// before the broadcast is asserted (no sleep-based synchronization).
	writeCmd(t, conn, publishRosterCmd("r-3", validRoster(4)))
	frames := []*frontendv1.FrontendFrame{readFrame(t, r), readFrame(t, r)}

	// Assert: the client sees both the broadcast roster and its ack.
	var sawRoster, sawAck bool
	for _, frame := range frames {
		if roster := frame.GetWorkspaceRoster(); roster != nil {
			sawRoster = true
			if roster.GetRevision() != 4 {
				t.Errorf("broadcast roster revision = %d, want 4", roster.GetRevision())
			}
		}
		if ack := frame.GetCommandAck(); ack != nil && ack.GetRequestId() == "r-3" {
			sawAck = true
			if !ack.GetOk() {
				t.Errorf("publish ack = %+v, want ok", ack)
			}
		}
	}
	if !sawRoster {
		t.Errorf("no workspace_roster frame reached the client, got %v", frames)
	}
	if !sawAck {
		t.Errorf("no ack for r-3 reached the client, got %v", frames)
	}
}

// --- connect snapshot -------------------------------------------------------

// TestConnectCarriesTheRetainedRoster covers snapshot inclusion: a client that
// connects after a publication is handed the retained roster without having to
// ask for it.
func TestConnectCarriesTheRetainedRoster(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	if err := s.PublishWorkspaceRoster(validRoster(11)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}
	l, err := net.Listen("unix", shortSock(t, "rc.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	defer s.Close()

	// Act.
	conn, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)
	first := readFrame(t, r)
	second := readFrame(t, r)

	// Assert: the StateSnapshot still leads, the roster follows it.
	if first.GetSnapshot() == nil {
		t.Fatalf("first frame was not a snapshot: %v", first)
	}
	if second.GetWorkspaceRoster().GetRevision() != 11 {
		t.Fatalf("second frame was not the retained roster: %v", second)
	}
}

// TestConnectOmitsTheRosterFrameWhenNoneIsRetained covers the absent case,
// which is also the post-restart case: with nothing published, no roster frame
// is sent at all, so "no roster yet" never masquerades as an empty roster.
func TestConnectOmitsTheRosterFrameWhenNoneIsRetained(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	l, err := net.Listen("unix", shortSock(t, "ro.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	defer s.Close()
	conn, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)
	if frame := readFrame(t, r); frame.GetSnapshot() == nil {
		t.Fatalf("first frame was not a snapshot: %v", frame)
	}

	// Act: a command ack is the next frame IF no roster was interposed.
	writeCmd(t, conn, &frontendv1.FrontendCommand{
		RequestId: "probe", Workspace: "w1",
		Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"}},
	})
	next := readFrame(t, r)

	// Assert.
	if next.GetWorkspaceRoster() != nil {
		t.Fatalf("connect sent a roster frame with nothing retained: %v", next)
	}
	if next.GetCommandAck().GetRequestId() != "probe" {
		t.Fatalf("frame after the snapshot = %v, want the probe ack", next)
	}
}

// TestConnectCarriesTheRetainedRosterToAScopedClient covers the editor-global
// rule at the connect boundary: a session-scoped webview renders the whole
// sidebar, so scoping must not strip its roster.
func TestConnectCarriesTheRetainedRosterToAScopedClient(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	if err := s.PublishWorkspaceRoster(validRoster(3)); err != nil {
		t.Fatalf("seed publish: %v", err)
	}
	cl := &client{
		id: 1, out: newOutbox(4), done: make(chan struct{}),
		scope: &Scope{Workspace: "/other", SessionID: "s-other"}, kind: ClientKindGUIStream,
	}

	// Act: replay the delivery step a connect performs for a scoped client.
	s.mu.Lock()
	held := s.retainedRosterLocked()
	s.mu.Unlock()
	frame, keep := scopeFrame(WorkspaceRosterFrame(held), *cl.scope)

	// Assert.
	if !keep {
		t.Fatal("scopeFrame dropped the roster for a scoped client")
	}
	if frame.GetWorkspaceRoster().GetRevision() != 3 {
		t.Errorf("scoped roster revision = %d, want 3", frame.GetWorkspaceRoster().GetRevision())
	}
}

// --- protojson round trip ---------------------------------------------------

// TestWorkspaceRosterFrameRoundTripsTheStatusOneof covers the wire encoding of
// the status arm, whose messages are EMPTY: an empty message must still survive
// protojson as a set arm rather than decoding back to an unset oneof.
func TestWorkspaceRosterFrameRoundTripsTheStatusOneof(t *testing.T) {
	// Arrange.
	frame := WorkspaceRosterFrame(validRoster(2))

	// Act.
	data, err := protojson.Marshal(frame)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	decoded := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(data, decoded); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}

	// Assert.
	rows := decoded.GetWorkspaceRoster().GetRepository().GetSections()[0].GetRows()
	if len(rows) != 2 {
		t.Fatalf("decoded rows = %d, want the fixture's 2", len(rows))
	}
	if rows[0].GetStatus() == nil {
		t.Fatalf("decoded row status oneof is unset after a round trip: %s", data)
	}
	if rows[0].GetReady() == nil {
		t.Errorf("decoded row status = %T, want the ready arm", rows[0].GetStatus())
	}
	// The NESTED row travels the same wire and is checked to the same standard:
	// a child whose empty status message decoded back to an unset oneof would
	// be just as unrenderable as a top-level one.
	child := rows[0].GetChildren()[0]
	if child.GetStatus() == nil {
		t.Fatalf("decoded child row status oneof is unset after a round trip: %s", data)
	}
	if child.GetThinking() == nil {
		t.Errorf("decoded child row status = %T, want the thinking arm", child.GetStatus())
	}
}
