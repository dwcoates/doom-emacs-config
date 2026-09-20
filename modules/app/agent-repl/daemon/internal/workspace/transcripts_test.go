package workspace

import (
	"context"
	"errors"
	"testing"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// transcript builds one shim-side transcript with the fields a case cares
// about, so each test arranges only what it is about.
type transcriptSpec struct {
	id       string
	bound    bool
	active   bool
	opening  string
	prompts  uint32
	cleared  bool
	tokens   *int64
	lastAtMs *int64
}

func shimTranscript(spec transcriptSpec) *shimv1.Transcript {
	out := &shimv1.Transcript{
		VendorSessionId: spec.id,
		Opening:         &spec.opening,
		Prompts:         spec.prompts,
		ContextTokens:   spec.tokens,
		LastRequestAtMs: spec.lastAtMs,
	}
	if spec.bound {
		out.Bound = &shimv1.TranscriptBound{}
	}
	if spec.active {
		out.Active = &shimv1.TranscriptActive{AtMs: 1_700_000_000_000, QuietAfterMs: 120_000}
	}
	if spec.cleared {
		out.Cleared = &shimv1.TranscriptCleared{AtMs: 1_699_000_000_000}
	}
	return out
}

// shimAnswers makes the fixture's shim answer with these transcripts.
func shimAnswers(f *fixture, specs ...transcriptSpec) {
	transcripts := make([]*shimv1.Transcript, 0, len(specs))
	for _, spec := range specs {
		transcripts = append(transcripts, shimTranscript(spec))
	}
	f.shim.transcripts = &shimv1.ReadTranscriptsResponse{
		Result: &shimv1.ReadTranscriptsResponse_Success{
			Success: &shimv1.ReadTranscriptsSuccess{Transcripts: transcripts},
		},
	}
}

// bindTo records a workspace's session binding, which is what the daemon's own
// `current` and `held` facts are read from.
func bindTo(f *fixture, ws ids.WorkspaceID, vendorSessionID string) {
	f.db.sessions[ws] = wsm.Session{
		Workspace: ws, HostSessionID: "host-" + string(ws), VendorSessionID: vendorSessionID,
	}
}

// refusedArm is the arm a verb refused under, or "" when it did not refuse.
func refusedArm(err error) string {
	if r, ok := AsRefusal(err); ok {
		return r.Arm
	}
	return ""
}

// ---------------------------------------------------------------------------
// ListTranscripts
// ---------------------------------------------------------------------------

func TestListTranscriptsRelaysEveryTranscriptTheShimRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert.
	if len(got) != 2 || got[0].GetVendorSessionId() != "a" || got[1].GetVendorSessionId() != "b" {
		t.Fatalf("transcripts = %+v, want a then b", got)
	}
}

func TestListTranscriptsRelaysTheTranscriptsOwnFigures(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	tokens, at := int64(4242), int64(1_699_999_000_000)
	shimAnswers(f, transcriptSpec{id: "a", opening: "explain hash tables", prompts: 3, tokens: &tokens, lastAtMs: &at})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert: the daemon adds facts; it never restates the transcript's own.
	if got[0].GetOpening() != "explain hash tables" || got[0].GetPrompts() != 3 ||
		got[0].GetContextTokens() != 4242 || got[0].GetLastRequestAtMs() != 1_699_999_000_000 {
		t.Fatalf("relayed figures = %+v", got[0])
	}
}

func TestListTranscriptsLeavesAnUnstatedFigureUNSET(t *testing.T) {
	// Arrange: a conversation that never reached the model states no usage.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a"})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert: a zero that cannot be told from an absence ranks it cheapest.
	if got[0].ContextTokens != nil {
		t.Fatalf("context tokens = %v, want unset", *got[0].ContextTokens)
	}
}

func TestListTranscriptsMarksTheWorkspacesOwnBindingCurrent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert.
	if got[0].GetCurrent() == nil || got[1].GetCurrent() != nil {
		t.Fatalf("current markers = %+v", got)
	}
}

func TestListTranscriptsFallsBackToTheShimsOwnBoundFlagWhenNoRecordNamesOne(t *testing.T) {
	// Arrange: an ADOPTED conversation, which the record does not name yet.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a", bound: true}, transcriptSpec{id: "b"})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert: the workspace's own conversation is never the one thing it cannot see.
	if got[0].GetCurrent() == nil {
		t.Fatalf("current markers = %+v, want the shim's bound conversation marked", got)
	}
}

func TestListTranscriptsMarksAConversationANOTHERWorkspaceHoldsHeld(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	bindTo(f, "w2", "b")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert: the one fact the daemon holds that a transcript cannot state.
	if got[1].GetHeld() == nil || got[1].GetHeld().GetWorkspace().GetId() != "w2" {
		t.Fatalf("held marker = %+v, want w2", got[1].GetHeld())
	}
}

func TestListTranscriptsDoesNotCallTheWorkspacesOwnBindingHeld(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert.
	if got[0].GetHeld() != nil {
		t.Fatalf("held marker = %+v, want none for the workspace's own binding", got[0].GetHeld())
	}
}

func TestListTranscriptsRelaysTheActiveMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a", active: true})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert.
	if got[0].GetActive().GetAtMs() != 1_700_000_000_000 {
		t.Fatalf("active marker = %+v", got[0].GetActive())
	}
}

func TestListTranscriptsRelaysTheClearedMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a", cleared: true})

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("ListTranscripts: %v", err)
	}

	// Assert.
	if got[0].GetCleared().GetAtMs() != 1_699_000_000_000 {
		t.Fatalf("cleared marker = %+v", got[0].GetCleared())
	}
}

func TestListTranscriptsRefusesNoSessionWhenNoShimIsLive(t *testing.T) {
	// Arrange: the shim is what reads the transcripts.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	_, err := f.verbs.ListTranscripts(context.Background(), "w1")

	// Assert.
	if got := refusedArm(err); got != ArmNoSession {
		t.Fatalf("arm = %q, want %q", got, ArmNoSession)
	}
}

func TestListTranscriptsRefusesUnreadableWithTheReadsOwnAccount(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.transcripts = &shimv1.ReadTranscriptsResponse{
		Result: &shimv1.ReadTranscriptsResponse_Failure{
			Failure: &shimv1.ReadTranscriptsFailure{
				Cause: &shimv1.ReadTranscriptsFailure_Unreadable{
					Unreadable: &shimv1.ReadTranscriptsUnreadable{SearchedPath: "/p", Detail: "EACCES"},
				},
			},
		},
	}

	// Act.
	_, err := f.verbs.ListTranscripts(context.Background(), "w1")

	// Assert.
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmUnreadable || r.Fields["searched_path"] != "/p" || r.Fields["detail"] != "EACCES" {
		t.Fatalf("refusal = %+v, want unreadable carrying the path and the detail", err)
	}
}

func TestListTranscriptsAnswersAnEmptyListWhenNothingHasEverRunThere(t *testing.T) {
	// Arrange: no project directory at all, for which the contract has no arm.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.transcripts = &shimv1.ReadTranscriptsResponse{
		Result: &shimv1.ReadTranscriptsResponse_Failure{
			Failure: &shimv1.ReadTranscriptsFailure{
				Cause: &shimv1.ReadTranscriptsFailure_NoProjectDir{
					NoProjectDir: &shimv1.ReadTranscriptsNoProjectDir{SearchedPath: "/p"},
				},
			},
		},
	}

	// Act.
	got, err := f.verbs.ListTranscripts(context.Background(), "w1")

	// Assert: an empty list is an answer, not a failure.
	if err != nil || len(got) != 0 {
		t.Fatalf("got = %+v, err = %v, want an empty list and no error", got, err)
	}
}

func TestListTranscriptsRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange: nothing registered.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.ListTranscripts(context.Background(), "nope")

	// Assert.
	if got := refusedArm(err); got != ArmUnknownWorkspace {
		t.Fatalf("arm = %q, want %q", got, ArmUnknownWorkspace)
	}
}

func TestListTranscriptsRefusesAWorkspaceHandedToASuccessor(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	_, err := f.verbs.ListTranscripts(context.Background(), "w1")

	// Assert.
	if got := refusedArm(err); got != ArmTransferringAway {
		t.Fatalf("arm = %q, want %q", got, ArmTransferringAway)
	}
}

// ---------------------------------------------------------------------------
// BindSession
// ---------------------------------------------------------------------------

func TestBindSessionStopsRecordsAndStartsInThatOrder(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", nil); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert: the swap ended the old session and started one on the choice.
	if len(f.fleet.stopped) != 1 || len(f.fleet.started) != 1 {
		t.Fatalf("stops = %+v, starts = %+v, want one of each", f.fleet.stopped, f.fleet.started)
	}
}

func TestBindSessionRecordsTheChosenConversationOnTheSessionRecord(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", nil); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert: the binding is the WORKSPACE's, so it survives a restart.
	if got := f.db.sessions["w1"].VendorSessionID; got != "b" {
		t.Fatalf("recorded vendor session = %q, want %q", got, "b")
	}
}

func TestBindSessionStartsTHROUGHTheOrdinaryResumePath(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", nil); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert: a plain Start, so a cold conversation parks at its cold gate
	// exactly as a revival does — binding never pays for a cold read silently.
	if len(f.fleet.resumes) != 0 {
		t.Fatalf("cold resumes = %+v, want none: a bind takes the ordinary path", f.fleet.resumes)
	}
}

func TestBindSessionRecordsTheBindingBEFOREItStarts(t *testing.T) {
	// Arrange: the source classifier reads the record, so the order is the
	// whole mechanism by which the new conversation is the one resumed.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})
	var atStart string
	f.fleet.startEntered = make(chan ids.WorkspaceID, 1)
	f.fleet.startHold = make(chan struct{})
	go func() {
		<-f.fleet.startEntered
		atStart = f.db.sessions["w1"].VendorSessionID
		close(f.fleet.startHold)
	}()

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", nil); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert.
	if atStart != "b" {
		t.Fatalf("record at start = %q, want %q", atStart, "b")
	}
}

func TestBindSessionMintsAHostIdentityForAWorkspaceThatRecordedNone(t *testing.T) {
	// Arrange: a row with no host identity is refused at the write, so the
	// binding would otherwise have nowhere to live.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "b"})

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", nil); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert.
	if f.db.sessions["w1"].HostSessionID == "" {
		t.Fatalf("host session id = %q, want a minted one", f.db.sessions["w1"].HostSessionID)
	}
}

func TestBindSessionValidatesAgainstAFRESHListing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "b"})

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", nil); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert: a transcript that went active between the listing and the choice
	// is exactly what the refusals exist for, so the read is redone.
	if f.shim.transcriptReads != 1 {
		t.Fatalf("transcript reads = %d, want 1", f.shim.transcriptReads)
	}
}

func TestBindSessionRefusesAnIdNoTranscriptCarries(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a"})

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "invented", nil)

	// Assert.
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmUnknownTranscript || r.Fields["vendor_session_id"] != "invented" {
		t.Fatalf("refusal = %+v, want unknown_transcript naming the id", err)
	}
}

func TestBindSessionRefusesTheConversationItAlreadyRuns(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"})

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "a", nil)

	// Assert.
	if got := refusedArm(err); got != ArmAlreadyBound {
		t.Fatalf("arm = %q, want %q", got, ArmAlreadyBound)
	}
}

func TestBindSessionChangesNothingWhenItIsAlreadyBound(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"})

	// Act.
	_ = f.verbs.BindSession(context.Background(), "w1", "a", nil)

	// Assert.
	if len(f.fleet.stopped) != 0 {
		t.Fatalf("stops = %+v, want none", f.fleet.stopped)
	}
}

func TestBindSessionRefusesATranscriptSomethingIsWritingTo(t *testing.T) {
	// Arrange: two writers on one conversation is data loss.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "b", active: true})

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmTranscriptActive || r.Fields["at_ms"] != int64(1_700_000_000_000) {
		t.Fatalf("refusal = %+v, want transcript_active carrying the write instant", err)
	}
}

func TestBindSessionRefusesAConversationAnotherWorkspaceHolds(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	bindTo(f, "w2", "b")
	shimAnswers(f, transcriptSpec{id: "b"})

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert: the arm NAMES the holder rather than saying only that something does.
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmTranscriptHeld {
		t.Fatalf("refusal = %+v, want transcript_held", err)
	}
	if r.Fields["workspace"] == nil {
		t.Fatalf("held arm fields = %+v, want the holding workspace's ref", r.Fields)
	}
}

func TestBindSessionRefusesWhileATurnIsInFlight(t *testing.T) {
	// Arrange: a bind is not an interrupt.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	shimAnswers(f, transcriptSpec{id: "b"})

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	if got := refusedArm(err); got != ArmTurnInFlight {
		t.Fatalf("arm = %q, want %q", got, ArmTurnInFlight)
	}
}

func TestBindSessionStopsNothingWhileATurnIsInFlight(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	shimAnswers(f, transcriptSpec{id: "b"})

	// Act.
	_ = f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	if len(f.fleet.stopped) != 0 {
		t.Fatalf("stops = %+v, want none", f.fleet.stopped)
	}
}

func TestBindSessionRefusesStopFailedWithTheStopsOwnAccount(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "b"})
	f.fleet.stopErr = errors.New("the shim would not die")

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmStopFailed || r.Fields["detail"] != "the shim would not die" {
		t.Fatalf("refusal = %+v, want stop_failed carrying the stop's account", err)
	}
}

func TestBindSessionRecordsNothingWhenTheStopFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})
	f.fleet.stopErr = errors.New("the shim would not die")

	// Act.
	_ = f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert: the old session is still standing, so the old binding is too.
	if got := f.db.sessions["w1"].VendorSessionID; got != "a" {
		t.Fatalf("recorded vendor session = %q, want the old binding %q", got, "a")
	}
}

func TestBindSessionRefusesStartFailedWithTheStartsOwnAccount(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "b"})
	f.fleet.startErr = errors.New("the vendor would not start")

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmStartFailed || r.Fields["detail"] != "the vendor would not start" {
		t.Fatalf("refusal = %+v, want start_failed carrying the start's account", err)
	}
}

func TestBindSessionLeavesTheNEWBindingStandingWhenTheStartFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "a")
	shimAnswers(f, transcriptSpec{id: "a"}, transcriptSpec{id: "b"})
	f.fleet.startErr = errors.New("the vendor would not start")

	// Act.
	_ = f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert: a bind that silently reverted would leave the user looking at a
	// workspace they did not ask for with nothing saying why.
	if got := f.db.sessions["w1"].VendorSessionID; got != "b" {
		t.Fatalf("recorded vendor session = %q, want the chosen %q to stand", got, "b")
	}
}

func TestBindSessionRefusesNoSessionWhenNoShimIsLiveToList(t *testing.T) {
	// Arrange: the choice is validated against a listing only a shim can give.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	if got := refusedArm(err); got != ArmNoSession {
		t.Fatalf("arm = %q, want %q", got, ArmNoSession)
	}
}

func TestBindSessionRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.BindSession(context.Background(), "nope", "b", nil)

	// Assert.
	if got := refusedArm(err); got != ArmUnknownWorkspace {
		t.Fatalf("arm = %q, want %q", got, ArmUnknownWorkspace)
	}
}

func TestBindSessionRefusesAWorkspaceHandedToASuccessor(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	err := f.verbs.BindSession(context.Background(), "w1", "b", nil)

	// Assert.
	if got := refusedArm(err); got != ArmTransferringAway {
		t.Fatalf("arm = %q, want %q", got, ArmTransferringAway)
	}
}

// recordingBindProgress collects the stages a bind reported, in order.
type recordingBindProgress struct{ stages []BindStage }

func (r *recordingBindProgress) Stage(stage BindStage) { r.stages = append(r.stages, stage) }

func TestBindSessionReportsItsStagesInOrder(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "b"})
	progress := &recordingBindProgress{}

	// Act.
	if err := f.verbs.BindSession(context.Background(), "w1", "b", progress); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Assert.
	want := []BindStage{
		BindStageReadingTranscripts, BindStageStoppingSession,
		BindStageRecordingBinding, BindStageStartingSession,
	}
	if len(progress.stages) != len(want) {
		t.Fatalf("stages = %+v, want %+v", progress.stages, want)
	}
	for i, stage := range want {
		if progress.stages[i] != stage {
			t.Fatalf("stages = %+v, want %+v", progress.stages, want)
		}
	}
}

func TestBindSessionReportsNoStageAfterARefusal(t *testing.T) {
	// Arrange: a stage announcing work that is not happening is worse than none.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	shimAnswers(f, transcriptSpec{id: "a"})
	progress := &recordingBindProgress{}

	// Act.
	_ = f.verbs.BindSession(context.Background(), "w1", "invented", progress)

	// Assert.
	if len(progress.stages) != 1 || progress.stages[0] != BindStageReadingTranscripts {
		t.Fatalf("stages = %+v, want only the read", progress.stages)
	}
}
