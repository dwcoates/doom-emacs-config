package workspace

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

func TestSwitchConversationBindsTheConversationTheSwitchCallsFor(t *testing.T) {
	cases := []struct {
		name        string
		current     string
		parked      string
		taking      bool
		wantBound   string
		wantForkOf  string
		wantRebound bool
	}{
		{name: "the first switch into the mode forks the queue conversation", current: "vendor-queue", taking: true, wantForkOf: "vendor-queue", wantRebound: true},
		{name: "a later switch into the mode resumes the parked bypass conversation", current: "vendor-queue", parked: "vendor-bypass", taking: true, wantBound: "vendor-bypass", wantRebound: true},
		{name: "a release resumes the parked queue conversation", current: "vendor-bypass", parked: "vendor-queue", wantBound: "vendor-queue", wantRebound: true},
		{name: "a release with nothing parked starts fresh", current: "vendor-bypass"},
		{name: "the first switch from a workspace with no conversation starts fresh", taking: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			bindTo(f, "w1", tc.current)

			// Act
			err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", tc.parked, tc.taking, QueueBypassContextDefault)

			// Assert
			if err != nil {
				t.Fatalf("switchConversation: %v", err)
			}
			if got := f.db.sessions["w1"].VendorSessionID; got != tc.wantBound {
				t.Fatalf("bound conversation = %q, want %q", got, tc.wantBound)
			}
			fork, owed := f.db.pendingForks["w1"]
			if tc.wantForkOf == "" && owed {
				t.Fatalf("pending fork = %+v, want none", fork)
			}
			if want := (wsm.PendingFork{Workspace: "w1", Parent: "w1", ParentVendorSessionID: tc.wantForkOf}); tc.wantForkOf != "" && (!owed || fork != want) {
				t.Fatalf("pending fork = (%+v, %v), want %+v", fork, owed, want)
			}
			if got := slices.Contains(f.fleet.rebound, ids.WorkspaceID("w1")); got != tc.wantRebound {
				t.Fatalf("started as a rebind = %v, want %v", got, tc.wantRebound)
			}
		})
	}
}

func TestSwitchConversationParksTheConversationSteppedOutOf(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")

	// Act
	err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", "", true, QueueBypassContextDefault)

	// Assert
	if err != nil {
		t.Fatalf("switchConversation: %v", err)
	}
	if got := f.db.parkedConversations["w1"]; got != "vendor-queue" {
		t.Fatalf("parked = %q, want the queue conversation kept for the release", got)
	}
}

func TestSwitchConversationTellsTheKeeperBothClocksMoved(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")

	// Act
	err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", "vendor-bypass", true, QueueBypassContextDefault)

	// Assert
	if err != nil {
		t.Fatalf("switchConversation: %v", err)
	}
	if !slices.Equal(f.switchedEdges, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("switched edges = %v, want the keeper told once", f.switchedEdges)
	}
}

func TestSwitchConversationCarriesOutTheNamedContext(t *testing.T) {
	cases := []struct {
		name              string
		parked            string
		context           QueueBypassContext
		wantBound         string
		wantForkOf        string
		wantOwed          *wsm.OwedStartWork
		wantResumedParked bool
		wantDropLogged    bool
	}{
		{name: "full main context forks the queue conversation", context: QueueBypassContextFullMain, wantForkOf: "vendor-queue", wantResumedParked: true},
		{name: "full main context replaces the parked bypass conversation", parked: "vendor-bypass", context: QueueBypassContextFullMain, wantForkOf: "vendor-queue", wantDropLogged: true},
		{name: "compacted main context forks the queue conversation owing a compaction", context: QueueBypassContextCompactedMain, wantForkOf: "vendor-queue", wantOwed: &wsm.OwedStartWork{Compact: true}, wantResumedParked: true},
		{name: "compacted main context replaces the parked bypass conversation", parked: "vendor-bypass", context: QueueBypassContextCompactedMain, wantForkOf: "vendor-queue", wantOwed: &wsm.OwedStartWork{Compact: true}, wantDropLogged: true},
		{name: "existing context resumes the parked bypass conversation", parked: "vendor-bypass", context: QueueBypassContextExisting, wantBound: "vendor-bypass", wantResumedParked: true},
		{name: "existing plus compacted main context resumes it owing a summary of the queue conversation", parked: "vendor-bypass", context: QueueBypassContextExistingPlusCompactedMain, wantBound: "vendor-bypass", wantOwed: &wsm.OwedStartWork{SummaryOf: "vendor-queue"}, wantResumedParked: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			bindTo(f, "w1", "vendor-queue")
			log := dlog.NewTestLogger()

			// Act
			err := f.verbs.(*verbs).switchConversation(context.Background(), log, "UpdateQueueBypassMode", "w1", tc.parked, true, tc.context)

			// Assert
			if err != nil {
				t.Fatalf("switchConversation: %v", err)
			}
			if got := f.db.sessions["w1"].VendorSessionID; got != tc.wantBound {
				t.Fatalf("bound conversation = %q, want %q", got, tc.wantBound)
			}
			if got := f.db.pendingForks["w1"].ParentVendorSessionID; got != tc.wantForkOf {
				t.Fatalf("own fork of = %q, want %q", got, tc.wantForkOf)
			}
			owed, owes := f.db.owedWork["w1"]
			if (tc.wantOwed == nil) == owes || (owes && owed != *tc.wantOwed) {
				t.Fatalf("owed work = (%+v, %v), want %+v", owed, owes, tc.wantOwed)
			}
			if got := f.db.swapResumedParked["w1"]; got != tc.wantResumedParked {
				t.Fatalf("swap resumes the parked conversation = %v, want %v", got, tc.wantResumedParked)
			}
			if got := f.db.parkedConversations["w1"]; got != "vendor-queue" {
				t.Fatalf("parked = %q, want the queue conversation", got)
			}
			_, logged := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opQueueBypass,
				Message: "the switch drops the parked bypass conversation's binding; its transcript stays on disk"})
			if logged != tc.wantDropLogged {
				t.Fatalf("drop logged = %v, want %v (records %+v)", logged, tc.wantDropLogged, log.Records())
			}
		})
	}
}

func TestSwitchConversationFromNoConversationOwesNothing(t *testing.T) {
	cases := []struct {
		name      string
		parked    string
		context   QueueBypassContext
		wantBound string
	}{
		{name: "compacted main context with no queue conversation starts fresh", context: QueueBypassContextCompactedMain},
		{name: "existing plus compacted main context with no queue conversation resumes alone", parked: "vendor-bypass", context: QueueBypassContextExistingPlusCompactedMain, wantBound: "vendor-bypass"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act
			err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", tc.parked, true, tc.context)

			// Assert
			if err != nil {
				t.Fatalf("switchConversation: %v", err)
			}
			if got := f.db.sessions["w1"].VendorSessionID; got != tc.wantBound {
				t.Fatalf("bound conversation = %q, want %q", got, tc.wantBound)
			}
			if owed, owes := f.db.owedWork["w1"]; owes {
				t.Fatalf("owed work = %+v, want none", owed)
			}
		})
	}
}

func TestSwitchConversationAnswersAFailedCompaction(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")
	f.fleet.startErr = markCompaction(labeled(errors.New("refused"), false, true, "the summarizer failed"), true)
	log := dlog.NewTestLogger()

	// Act
	err := f.verbs.(*verbs).switchConversation(context.Background(), log, "UpdateQueueBypassMode", "w1", "", true, QueueBypassContextCompactedMain)

	// Assert
	r, ok := AsRefusal(err)
	if !ok || r.Arm != ArmCompactionFailed || r.Fields["detail"] != "the summarizer failed" {
		t.Fatalf("err = %v, want compaction_failed carrying the shim's account", err)
	}
	if owed := f.db.owedWork["w1"]; owed != (wsm.OwedStartWork{Compact: true}) {
		t.Fatalf("owed work = %+v, want the compaction still owed for the next start", owed)
	}
	if _, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: opRefusal, Message: r.Error()}); !found {
		t.Fatalf("records = %+v, want the refusal recorded", log.Records())
	}
}

func TestSwitchConversationAnswersAnyOtherFailedStartAsSwitchFailed(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")
	f.fleet.startErr = labeled(errors.New("refused"), false, true, "the vendor binary is missing")

	// Act
	err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", "", true, QueueBypassContextCompactedMain)

	// Assert
	if arm := refusedArm(err); arm != ArmSwitchFailed {
		t.Fatalf("arm = %q, want %q", arm, ArmSwitchFailed)
	}
}

func TestBypassBindingRefusesAnExistingContextWithNothingParked(t *testing.T) {
	for _, bypassContext := range []QueueBypassContext{QueueBypassContextExisting, QueueBypassContextExistingPlusCompactedMain} {
		t.Run(bypassContext.String(), func(t *testing.T) {
			// Act
			_, err := bypassBinding("vendor-queue", "", bypassContext)

			// Assert
			if err == nil {
				t.Fatal("bypassBinding = nil error, want a refusal")
			}
		})
	}
}

func TestUpdateQueueBypassModeRefusesBeforeChangingAnything(t *testing.T) {
	cases := []struct {
		name     string
		change   QueueBypassChange
		context  QueueBypassContext
		wantArm  string
		wantText string
	}{
		{name: "an existing context with nothing parked", change: QueueBypassOn, context: QueueBypassContextExisting, wantArm: ArmNoBypassConversation},
		{name: "an existing plus compacted context with nothing parked", change: QueueBypassOn, context: QueueBypassContextExistingPlusCompactedMain, wantArm: ArmNoBypassConversation},
		{name: "a context named with a toggle", change: QueueBypassToggle, context: QueueBypassContextFullMain,
			wantText: "a context was named with a change that does not take the mode"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			bindTo(f, "w1", "vendor-queue")

			// Act
			on, err := f.verbs.UpdateQueueBypassMode(context.Background(), "w1", tc.change, tc.context)

			// Assert
			if on || err == nil {
				t.Fatalf("UpdateQueueBypassMode = (%v, %v), want a refusal", on, err)
			}
			if arm := refusedArm(err); arm != tc.wantArm {
				t.Fatalf("arm = %q, want %q", arm, tc.wantArm)
			}
			if tc.wantText != "" {
				if _, found := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opQueueBypass, Message: tc.wantText}); !found {
					t.Fatalf("want %q at ERROR", tc.wantText)
				}
			}
			if len(f.fleet.stopped) != 0 || len(f.db.bindSessionCalls) != 0 || len(f.db.parkedConversations) != 0 {
				t.Fatalf("stopped %v, bound %v, parked %v: want nothing changed", f.fleet.stopped, f.db.bindSessionCalls, f.db.parkedConversations)
			}
		})
	}
}

func TestQueueBypassContextString(t *testing.T) {
	cases := []struct {
		context QueueBypassContext
		want    string
	}{
		{QueueBypassContextDefault, "default"},
		{QueueBypassContextFullMain, "full_main"},
		{QueueBypassContextCompactedMain, "compacted_main"},
		{QueueBypassContextExisting, "existing"},
		{QueueBypassContextExistingPlusCompactedMain, "existing_plus_compacted_main"},
		{QueueBypassContext(99), "QueueBypassContext(99)"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act
			got := tc.context.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestQueueBypassChipOffersTheContextsTheStandingAllows(t *testing.T) {
	cases := []struct {
		name   string
		bypass wsm.QueueBypass
		want   []QueueBypassContext
	}{
		{name: "nothing parked offers the two main-context options", bypass: wsm.QueueBypass{},
			want: []QueueBypassContext{QueueBypassContextFullMain, QueueBypassContextCompactedMain}},
		{name: "the bypass conversation parked while the mode is free offers all four", bypass: wsm.QueueBypass{ParkedVendorSessionID: "vendor-bypass"},
			want: []QueueBypassContext{QueueBypassContextFullMain, QueueBypassContextCompactedMain, QueueBypassContextExisting, QueueBypassContextExistingPlusCompactedMain}},
		{name: "the mode held offers nothing", bypass: wsm.QueueBypass{Holds: true, ParkedVendorSessionID: "vendor-queue"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := f.workspace("w1", t.TempDir())
			f.db.bypass = map[ids.WorkspaceID]wsm.QueueBypass{"w1": tc.bypass}

			// Act
			chip, err := f.verbs.(*verbs).queueBypassChip(context.Background(), record)

			// Assert
			if err != nil {
				t.Fatalf("queueBypassChip: %v", err)
			}
			var got []QueueBypassContext
			for _, offer := range chip.Offers {
				named, err := QueueBypassContextFrom(offer)
				if err != nil {
					t.Fatalf("offer %v: %v", offer, err)
				}
				got = append(got, named)
			}
			if !chip.Offered || !slices.Equal(got, tc.want) {
				t.Fatalf("chip offers %v (offered %v), want %v", got, chip.Offered, tc.want)
			}
		})
	}
}

func TestQueueBypassContextProtoRoundTrips(t *testing.T) {
	for _, named := range []QueueBypassContext{QueueBypassContextFullMain, QueueBypassContextCompactedMain, QueueBypassContextExisting, QueueBypassContextExistingPlusCompactedMain} {
		t.Run(named.String(), func(t *testing.T) {
			// Act
			got, err := QueueBypassContextFrom(named.Proto())

			// Assert
			if err != nil || got != named {
				t.Fatalf("QueueBypassContextFrom(%s.Proto()) = (%s, %v), want %s", named, got, err, named)
			}
		})
	}
}

func TestQueueBypassContextFrom(t *testing.T) {
	cases := []struct {
		name    string
		token   *frontendv1.TopbarQueueBypassContext
		want    QueueBypassContext
		wantErr bool
	}{
		{name: "unset is the default", token: nil, want: QueueBypassContextDefault},
		{name: "a token naming no arm is refused", token: &frontendv1.TopbarQueueBypassContext{}, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := QueueBypassContextFrom(tc.token)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("QueueBypassContextFrom = (%s, %v), want (%s, error %v)", got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestTheDefaultContextHasNoEchoToken(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("QueueBypassContextDefault.Proto() did not refuse")
		}
	}()

	// Act
	QueueBypassContextDefault.Proto()
}

// TestTheContextArmsAreBuiltOnlyByProto fails on a hand-rolled echo token
// anywhere in the daemon's production code: every one comes from
// QueueBypassContext.Proto.
func TestTheContextArmsAreBuiltOnlyByProto(t *testing.T) {
	// Arrange
	const spelling = "TopbarQueueBypassContext_FullMain{FullMain:"
	var offenders []string

	// Act
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") ||
			path == filepath.Join("..", "..", "internal", "workspace", "queuebypass.go") {
			return err
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), spelling) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("files building the context's echo token by hand = %v, want every one through QueueBypassContext.Proto", offenders)
	}
}

func TestLetQueueThroughAfterBypassTurn(t *testing.T) {
	cases := []struct {
		name        string
		standing    map[ids.WorkspaceID]wsm.QueueBypass
		readErr     error
		resumeErr   error
		wantResumes []ownResume
		wantErr     string
	}{
		{name: "the holder's turn end resumes its own pause",
			standing:    map[ids.WorkspaceID]wsm.QueueBypass{"w1": {Holds: true, RepoID: "r1"}},
			wantResumes: []ownResume{{scope: merge.RepositoryScope{ID: "r1"}, ws: "w1"}}},
		{name: "a workspace that does not hold the mode resumes nothing",
			standing: map[ids.WorkspaceID]wsm.QueueBypass{"w1": {HeldBy: "w2", RepoID: "r1"}}},
		{name: "a workspace no longer registered resumes nothing", readErr: wsm.ErrNotFound},
		{name: "an unreadable standing is answered and resumes nothing", readErr: errors.New("db gone"), wantErr: "db gone"},
		{name: "a resume that fails is answered",
			standing:    map[ids.WorkspaceID]wsm.QueueBypass{"w1": {Holds: true, RepoID: "r1"}},
			resumeErr:   errors.New("queue gone"),
			wantResumes: []ownResume{{scope: merge.RepositoryScope{ID: "r1"}, ws: "w1"}},
			wantErr:     "queue gone"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.bypass = tc.standing
			f.db.bypassErr = tc.readErr
			f.merge.ownResumeErr = tc.resumeErr

			// Act
			err := f.verbs.LetQueueThroughAfterBypassTurn(context.Background(), "w1")

			// Assert
			if tc.wantErr == "" && err != nil {
				t.Fatalf("LetQueueThroughAfterBypassTurn: %v", err)
			}
			if tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("LetQueueThroughAfterBypassTurn = %v, want an error naming %q", err, tc.wantErr)
			}
			if !slices.Equal(f.merge.ownResumes, tc.wantResumes) {
				t.Fatalf("resumes = %+v, want %+v", f.merge.ownResumes, tc.wantResumes)
			}
		})
	}
}

func TestLetQueueThroughAfterBypassTurnRecordsAWorkspaceNoLongerRegistered(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.bypassErr = wsm.ErrNotFound

	// Act
	err := f.verbs.LetQueueThroughAfterBypassTurn(context.Background(), "w1")

	// Assert
	if err != nil {
		t.Fatalf("LetQueueThroughAfterBypassTurn: %v", err)
	}
	if rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opBypassTurnEnd}); !ok || rec.Context["workspace"] != "w1" {
		t.Fatalf("no INFO record naming the workspace: %+v", f.log.logger.Records())
	}
}

func TestBypassTurnEndsMovesOnlyAtATurnsEnd(t *testing.T) {
	cases := []struct {
		name string
		edge func(*TurnEdgeRunner)
		want []ids.WorkspaceID
	}{
		{name: "a turn ending", edge: func(r *TurnEdgeRunner) { r.TurnEnded("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending unobserved", edge: func(r *TurnEdgeRunner) { r.TurnEndedUnobserved("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn starting", edge: func(r *TurnEdgeRunner) { r.TurnStarted("w1") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var carried []ids.WorkspaceID
			r := NewBypassTurnEnds(func(_ context.Context, ws ids.WorkspaceID) error { carried = append(carried, ws); return nil }, dlog.NewTestLogger())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tc.edge(r)
			r.Run(ctx)

			// Assert
			if !slices.Equal(carried, tc.want) {
				t.Fatalf("carried = %v, want %v", carried, tc.want)
			}
		})
	}
}

func TestBypassTurnEndsRecordsAResumeItCannotCarryOut(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	r := NewBypassTurnEnds(func(context.Context, ids.WorkspaceID) error { return errors.New("queue gone") }, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	r.TurnEnded("w1")
	r.Run(ctx)

	// Assert
	if rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opBypassTurnEnd,
		Message: "could not let the merge queue through at the end of a turn"}); !ok || rec.Context["workspace"] != "w1" {
		t.Fatalf("no ERROR record naming the workspace: %+v", log.Records())
	}
}

func TestTakingTheModeLeavesTheQueueRunning(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")
	f.db.bypass = map[ids.WorkspaceID]wsm.QueueBypass{"w1": {RepoID: "r1"}}

	// Act
	on, err := f.verbs.UpdateQueueBypassMode(context.Background(), "w1", QueueBypassOn, QueueBypassContextDefault)

	// Assert: taken, and the queue neither stopped nor paused.
	if err != nil || !on {
		t.Fatalf("UpdateQueueBypassMode = (%v, %v), want the mode taken", on, err)
	}
	if len(f.merge.hardStops) != 0 {
		t.Fatalf("hard stops = %+v, want none", f.merge.hardStops)
	}
}

func TestReleasingTheMode(t *testing.T) {
	cases := []struct {
		name      string
		none      bool
		resumeErr error
		wantErr   bool
		wantLevel string
		wantText  string
	}{
		{name: "the holder's own pause is resumed", wantLevel: "info", wantText: "the workspace switched its queue bypass mode"},
		{name: "no pause of the holder's own has nothing to resume", none: true,
			wantLevel: "info", wantText: "the mode was released with no pause of the holder's own standing, so there was nothing to resume"},
		{name: "a resume that fails is refused as a failed switch", resumeErr: errors.New("queue gone"), wantErr: true,
			wantLevel: "error", wantText: "the repository's merge queue could not be resumed after the mode was released"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			bindTo(f, "w1", "vendor-bypass")
			f.db.bypass = map[ids.WorkspaceID]wsm.QueueBypass{"w1": {Holds: true, RepoID: "r1", ParkedVendorSessionID: "vendor-queue"}}
			f.merge.ownResumeNone, f.merge.ownResumeErr = tc.none, tc.resumeErr

			// Act
			on, err := f.verbs.UpdateQueueBypassMode(context.Background(), "w1", QueueBypassOff, QueueBypassContextDefault)

			// Assert: only the holder's own pause, never any pause.
			if on || (err != nil) != tc.wantErr {
				t.Fatalf("UpdateQueueBypassMode = (%v, %v), want released with error %v", on, err, tc.wantErr)
			}
			if tc.wantErr && refusedArm(err) != ArmSwitchFailed {
				t.Fatalf("arm = %q, want %q", refusedArm(err), ArmSwitchFailed)
			}
			if f.merge.unpaused != 0 || len(f.merge.ownResumes) != 1 || f.merge.ownResumes[0].ws != "w1" {
				t.Fatalf("unpaused %d, own resumes %+v; want only w1's own pause resumed", f.merge.unpaused, f.merge.ownResumes)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: tc.wantLevel, Operation: opQueueBypass, Message: tc.wantText}); !ok {
				t.Fatalf("want %q at %s: %+v", tc.wantText, tc.wantLevel, f.log.logger.Records())
			}
		})
	}
}

// bypassFixture is the verb fixture with workspace w1 bound to its queue
// conversation, standing in the queue bypass mode when HOLDS, and recording
// the holds written and the fleet's restates in one order.
func bypassFixture(t *testing.T, holds bool) (*fixture, *[]string) {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")
	f.db.setBypass("w1", wsm.QueueBypass{Holds: holds, RepoID: "r1"})
	steps := &[]string{}
	f.db.bypassSteps, f.fleet.restateSteps = steps, steps
	return f, steps
}

func TestUpdateQueueBypassModeRestatesRemoteControlRightAfterEachHold(t *testing.T) {
	cases := []struct {
		name   string
		holds  bool
		change QueueBypassChange
		want   []string
	}{
		{name: "entering restates remote control after the take", change: QueueBypassOn, want: []string{"take", "restate entered"}},
		{name: "leaving restates remote control after the release", holds: true, change: QueueBypassOff, want: []string{"release", "restate left"}},
		{name: "a change that changes nothing restates nothing", holds: true, change: QueueBypassOn, want: []string{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, steps := bypassFixture(t, tc.holds)

			// Act
			_, err := f.verbs.UpdateQueueBypassMode(context.Background(), "w1", tc.change, contextFor(tc.change))

			// Assert
			if err != nil {
				t.Fatalf("UpdateQueueBypassMode: %v", err)
			}
			if !slices.Equal(*steps, tc.want) {
				t.Fatalf("steps = %v, want %v", *steps, tc.want)
			}
		})
	}
}

func TestUpdateQueueBypassModeRestatesNothingWhenTheHoldCannotBeWritten(t *testing.T) {
	cases := []struct {
		name   string
		holds  bool
		change QueueBypassChange
		break_ func(*fakeDB)
	}{
		{name: "a failed take", change: QueueBypassOn, break_: func(d *fakeDB) { d.takeBypassErr = errors.New("disk full") }},
		{name: "a failed release", holds: true, change: QueueBypassOff, break_: func(d *fakeDB) { d.releaseBypassErr = errors.New("disk full") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, _ := bypassFixture(t, tc.holds)
			tc.break_(f.db)

			// Act
			holds, err := f.verbs.UpdateQueueBypassMode(context.Background(), "w1", tc.change, contextFor(tc.change))

			// Assert
			if err == nil || holds != tc.holds {
				t.Fatalf("UpdateQueueBypassMode = (%v, %v), want the failure with the standing unchanged", holds, err)
			}
			if len(f.fleet.restates) != 0 {
				t.Fatalf("restates = %v, want none: the mode the store derives did not move", f.fleet.restates)
			}
			if !loggedAtError(f, opQueueBypass) {
				t.Fatalf("the failed hold was not recorded at ERROR under %s", opQueueBypass)
			}
		})
	}
}

func TestAFailedRemoteControlRestateLeavesTheTransitionStanding(t *testing.T) {
	// Arrange
	f, _ := bypassFixture(t, false)
	f.fleet.restateErr = errors.New("the vendor refused the bridge")

	// Act
	holds, err := f.verbs.UpdateQueueBypassMode(context.Background(), "w1", QueueBypassOn, QueueBypassContextFullMain)

	// Assert
	if err != nil || !holds {
		t.Fatalf("UpdateQueueBypassMode = (%v, %v), want the mode taken", holds, err)
	}
	if got := f.db.sessions["w1"].VendorSessionID; got == "vendor-queue" {
		t.Fatalf("bound conversation = %q, want the switch to the bypass conversation done", got)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opQueueBypass, Message: "the queue bypass transition stands; the fleet surfaced its remote-control failure"})
	if !ok || record.Context["transition"] != queueBypassEntered || record.Context["cause"] != "the vendor refused the bridge" {
		t.Fatalf("record = %+v (%v), want the transition and its cause", record, ok)
	}
}

// contextFor is the context a change names: one only when the mode is taken.
func contextFor(change QueueBypassChange) QueueBypassContext {
	if change == QueueBypassOn {
		return QueueBypassContextFullMain
	}
	return QueueBypassContextDefault
}
