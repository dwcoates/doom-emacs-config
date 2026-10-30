package workspace

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/netstate"
	"agent-repld/internal/prompts"
	"agent-repld/internal/wsm"
)

// The records the pending-create set's edges are stated by.
const (
	saidHeld     = "the create waits for the network; it resumes from the naming call when the network is restored"
	saidHeldMade = "the create held for the network was created"
	saidHeldLost = "the create held for the network ended without a workspace"
	saidJoined   = "the request is identical to a pending create; it joined that create"
	saidNoHold   = "the create held for the network could not be recorded; it fails instead"
	saidNamingNo = "the workspace naming call could not reach the network; the create waits for it"
)

var namingTimeout = &headless.Error{Cause: headless.CauseTimeout, Detail: "signal: killed"}

// heldCreateFixture arranges a create whose naming call times out while the
// network is down, in a repository whose main worktree a failure would be
// reported to, with the dequeued notice's brief shipped. It answers the spec.
func heldCreateFixture(t *testing.T) (*fixture, CreateSpec) {
	t.Helper()
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	spec := failingCreate(t, f, false)
	f.git.createErr = nil
	f.fleet.live[mainWorkspaceID] = true
	f.network.down(netstate.EvidenceFaultOpen)
	script(f, headlessAnswer{err: namingTimeout}, headlessAnswer{text: FixtureMintedName})
	f.briefs[BriefCreateDequeued] = prompts.Prompt{
		Name: BriefCreateDequeued, Body: "asked {{request}} then {{outcome}}",
		Placeholders: []string{"request", "outcome"},
	}
	return f, spec
}

// holdCreate runs SPEC with a reporter of OP until it is held, failing the
// test unless it is.
func holdCreate(t *testing.T, f *fixture, spec CreateSpec, op string) (*NetworkWaitError, *recordingProgress) {
	t.Helper()
	rec := &recordingProgress{op: op, ended: make(chan struct{})}
	spec.Progress = rec
	_, err := f.verbs.Create(context.Background(), spec)
	var wait *NetworkWaitError
	if !errors.As(err, &wait) {
		t.Fatalf("Create = %v, want it held for the network", err)
	}
	return wait, rec
}

func reportsMade(f *fixture) int {
	f.verbs.(*verbs).createFailureReports.Wait()
	n := 0
	for _, sub := range f.queue.submissions {
		if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATE_FAILED {
			n++
		}
	}
	return n
}

func TestANamingCallTheNetworkBlockedHoldsTheCreate(t *testing.T) {
	// Arrange
	f, spec := heldCreateFixture(t)

	// Act
	wait, rec := holdCreate(t, f, spec, "op-1")

	// Assert: held with the evidence, told on the op, recorded durably.
	if wait.Evidence != netstate.EvidenceFaultOpen || wait.ID == "" {
		t.Fatalf("wait = %+v, want a pending ref and the open fault as its evidence", wait)
	}
	if waiting, _, _ := rec.snapshot(); !slices.Equal(waiting, []PendingCreateID{wait.ID}) {
		t.Fatalf("op told waiting %v, want %v", waiting, wait.ID)
	}
	held := f.db.heldCreates()
	if len(held) != 1 || held[0].ID != string(wait.ID) || !slices.Equal(held[0].OpIDs, []string{"op-1"}) {
		t.Fatalf("held rows = %+v, want one for %s answering op-1", held, wait.ID)
	}
	// The outage is not retried into: one naming call.
	if len(f.headless.calls) != 1 {
		t.Fatalf("naming calls = %d, want 1", len(f.headless.calls))
	}
	if got := f.network.classifications(); len(got) != 1 || !strings.Contains(got[0], "signal: killed") {
		t.Fatalf("classified %v, want the timed-out call's own account", got)
	}
	if reportsMade(f) != 0 {
		t.Fatalf("a held create was reported to the main worktree: %+v", f.queue.submissions)
	}
	records := f.log.logger.Records()
	naming, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opNaming, Message: saidNamingNo})
	if !ok || naming.Context["cause"] != NamingCauseNetwork || naming.Context["headless_cause"] != headless.CauseTimeout {
		t.Fatalf("naming record = %+v, want cause network over a timeout: %+v", naming, records)
	}
	if _, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPendingCreate, Message: saidHeld}); !ok {
		t.Fatalf("no held record: %+v", records)
	}
	if _, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: dlog.LevelError, Operation: opNaming}); ok {
		t.Fatalf("a held create's naming call was recorded as failing: %+v", records)
	}
}

func TestANamingFailureTheNetworkDidNotCauseStillRefuses(t *testing.T) {
	tests := []struct {
		name           string
		answer         error
		wantClassified int
	}{
		{name: "a timeout with the network up", answer: namingTimeout, wantClassified: 2},
		{name: "a CLI failure with the network up", answer: &headless.Error{Cause: headless.CauseExitStatus, Detail: "exit status 3"}, wantClassified: 2},
		{name: "a guard refusal is never the network's", answer: &headless.Error{Cause: headless.CauseGuardRefused, Detail: "forbidden"}, wantClassified: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			script(f, headlessAnswer{err: tc.answer}, headlessAnswer{err: tc.answer})

			// Act
			_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

			// Assert
			asRefusal(t, err, ArmNamingFailed)
			if got := len(f.network.classifications()); got != tc.wantClassified {
				t.Fatalf("classifications = %d, want %d", got, tc.wantClassified)
			}
			if held := f.db.heldCreates(); len(held) != 0 {
				t.Fatalf("a refused create was held: %+v", held)
			}
		})
	}
}

func TestAHeldCreateResumesWhenTheNetworkIsRestored(t *testing.T) {
	// Arrange: held, with a requesting workspace to tell.
	f, spec := heldCreateFixture(t)
	spec.Requester = ptr(mainWorkspaceID)
	wait, rec := holdCreate(t, f, spec, "op-1")

	// Act
	f.network.restore()
	<-rec.ended

	// Assert: created once, on the op, the row released, the requester told.
	_, created, failed := rec.snapshot()
	if len(created) != 1 || len(failed) != 0 {
		t.Fatalf("op outcome created=%+v failed=%v, want one workspace", created, failed)
	}
	if !strings.HasSuffix(created[0].Name, FixtureMintedName) {
		t.Fatalf("created %q, want the name the resumed naming call minted", created[0].Name)
	}
	if held := f.db.heldCreates(); len(held) != 0 {
		t.Fatalf("held rows after the create = %+v, want none", held)
	}
	if len(f.headless.calls) != 2 {
		t.Fatalf("naming calls = %d, want the held one and the resumed one", len(f.headless.calls))
	}
	var told []string
	for _, sub := range f.queue.submissions {
		if sub.WS == mainWorkspaceID && sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE {
			told = append(told, submittedText(sub.Said))
		}
	}
	if len(told) != 1 || !strings.Contains(told[0], "It has now been created") {
		t.Fatalf("requester told %q, want the creation", told)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPendingCreate, Message: saidHeldMade}); !ok {
		t.Fatalf("no record of %s being created: %+v", wait.ID, f.log.logger.Records())
	}
}

func TestAHeldCreateThatFailsForAnotherCauseIsReported(t *testing.T) {
	// Arrange: held, then the worktree add fails once it resumes.
	f, spec := heldCreateFixture(t)
	_, rec := holdCreate(t, f, spec, "op-1")
	f.git.createErr = errors.New("fatal: a branch named 'ABC/fix' already exists")

	// Act
	f.network.restore()
	<-rec.ended

	// Assert
	if _, _, failed := rec.snapshot(); len(failed) != 1 {
		t.Fatalf("op failures = %v, want one", failed)
	}
	if reportsMade(f) != 1 {
		t.Fatalf("reports = %+v, want the resumed failure reported", f.queue.submissions)
	}
	if held := f.db.heldCreates(); len(held) != 0 {
		t.Fatalf("held rows = %+v, want the failed create released", held)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPendingCreate, Message: saidHeldLost}); !ok {
		t.Fatalf("no record of the held create ending without a workspace: %+v", f.log.logger.Records())
	}
}

func TestAHeldCreateBlockedAgainStaysHeld(t *testing.T) {
	// Arrange: held, and the resumed naming call is blocked again.
	f, spec := heldCreateFixture(t)
	script(f, headlessAnswer{err: namingTimeout}, headlessAnswer{err: namingTimeout}, headlessAnswer{text: FixtureMintedName})
	rec := &recordingProgress{op: "op-1", ended: make(chan struct{}), held: make(chan PendingCreateID, 4)}
	spec.Progress = rec
	_, err := f.verbs.Create(context.Background(), spec)
	if !IsNetworkWait(err) {
		t.Fatalf("Create = %v, want it held", err)
	}
	first := <-rec.held

	// Act: a restoration that does not last, then one that does.
	f.network.signalRestored()
	again := <-rec.held
	heldAgain := f.db.heldCreates()
	f.network.restore()
	<-rec.ended

	// Assert: the same create held twice, then one workspace.
	if again != first || len(heldAgain) != 1 || heldAgain[0].ID != string(first) {
		t.Fatalf("held again as %s with rows %+v, want %s kept", again, heldAgain, first)
	}
	if _, created, _ := rec.snapshot(); len(created) != 1 {
		t.Fatalf("created %+v, want one workspace", created)
	}
	if reportsMade(f) != 0 {
		t.Fatalf("a create held twice was reported: %+v", f.queue.submissions)
	}
}

func TestAHeldCreateThatCannotBeRecordedFails(t *testing.T) {
	// Arrange
	f, spec := heldCreateFixture(t)
	f.db.queuedPutErr = errors.New("database is locked")

	// Act
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert: failed, reported, recorded, nothing pending.
	if err == nil || IsNetworkWait(err) || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Create = %v, want the hold's failure", err)
	}
	if reportsMade(f) != 1 {
		t.Fatalf("reports = %+v, want the failure reported", f.queue.submissions)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opPendingCreate, Message: saidNoHold})
	if !ok || !strings.Contains(record.Context["cause"].(string), "database is locked") {
		t.Fatalf("hold failure record = %+v, want its cause", record)
	}
	if len(f.verbs.(*verbs).pending.byKey) != 0 {
		t.Fatal("a create that could not be held is still pending")
	}
}

func TestAnIdenticalRequestJoinsTheHeldCreate(t *testing.T) {
	// Arrange
	f, spec := heldCreateFixture(t)
	first, _ := holdCreate(t, f, spec, "op-1")
	again := spec
	again.InitialPrompt = "  " + spec.InitialPrompt + "\n"
	rec := &recordingProgress{op: "op-2"}
	again.Progress = rec

	// Act
	ticket := f.verbs.EnterCreate(again)
	_, err := f.verbs.RunCreate(context.Background(), ticket)

	// Assert: joined, answered with the same create, no second naming call.
	var wait *NetworkWaitError
	if !ticket.Joined() || ticket.ID() != first.ID || !errors.As(err, &wait) || wait.ID != first.ID {
		t.Fatalf("joined=%v id=%s err=%v, want the held create %s", ticket.Joined(), ticket.ID(), err, first.ID)
	}
	if waiting, _, _ := rec.snapshot(); !slices.Equal(waiting, []PendingCreateID{first.ID}) {
		t.Fatalf("joined op told waiting %v, want %s", waiting, first.ID)
	}
	if len(f.headless.calls) != 1 {
		t.Fatalf("naming calls = %d, want 1", len(f.headless.calls))
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPendingCreate, Message: saidJoined}); !ok {
		t.Fatalf("no joined record: %+v", f.log.logger.Records())
	}
}

func TestBothOpsOfAHeldCreateAreToldItWasCreated(t *testing.T) {
	// Arrange: held for op-1, joined by op-2.
	f, spec := heldCreateFixture(t)
	_, first := holdCreate(t, f, spec, "op-1")
	second := &recordingProgress{op: "op-2", ended: make(chan struct{})}
	again := spec
	again.Progress = second
	f.verbs.RunCreate(context.Background(), f.verbs.EnterCreate(again))

	// Act
	f.network.restore()
	<-first.ended
	<-second.ended

	// Assert
	_, a, _ := first.snapshot()
	_, b, _ := second.snapshot()
	if len(a) != 1 || len(b) != 1 || a[0].ID != b[0].ID {
		t.Fatalf("op-1 created %+v, op-2 created %+v, want the one workspace", a, b)
	}
}

func TestAnIdenticalRequestJoinsTheCreateInFlight(t *testing.T) {
	// Arrange: the first create is held inside its naming call.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	spec := standardSpec(t, f)
	f.headless.entered, f.headless.release = make(chan struct{}), make(chan struct{})
	type outcome struct {
		created wsm.Workspace
		err     error
	}
	firstDone := make(chan outcome, 1)
	go func() {
		created, err := f.verbs.Create(context.Background(), spec)
		firstDone <- outcome{created, err}
	}()
	<-f.headless.entered

	// Act: the identical request, while the first runs.
	ticket := f.verbs.EnterCreate(spec)
	secondDone := make(chan outcome, 1)
	go func() {
		created, err := f.verbs.RunCreate(context.Background(), ticket)
		secondDone <- outcome{created, err}
	}()
	close(f.headless.release)
	first, second := <-firstDone, <-secondDone

	// Assert: one create, both answered with it.
	if !ticket.Joined() {
		t.Fatal("the identical request did not join the create in flight")
	}
	if first.err != nil || second.err != nil || first.created.ID != second.created.ID {
		t.Fatalf("first=%+v second=%+v, want both the one workspace", first, second)
	}
	if len(f.headless.calls) != 1 {
		t.Fatalf("naming calls = %d, want 1", len(f.headless.calls))
	}
	if len(f.verbs.(*verbs).pending.byKey) != 0 {
		t.Fatal("the finished create is still pending")
	}
}

func TestDistinctRequestsAreDistinctCreates(t *testing.T) {
	base := CreateSpec{RepoDir: "/r", InitialPrompt: "fix the login bug"}
	p1 := wsm.PriorityP1
	tests := []struct {
		name       string
		change     func(*CreateSpec)
		wantJoined bool
	}{
		{name: "the same prompt after whitespace trimming joins", change: func(s *CreateSpec) { s.InitialPrompt = "\tfix the login bug  " }, wantJoined: true},
		{name: "another requester joins", change: func(s *CreateSpec) { s.Requester = ptr(ids.WorkspaceID("asker")) }, wantJoined: true},
		{name: "a distinct prompt does not", change: func(s *CreateSpec) { s.InitialPrompt = "fix the logout bug" }},
		{name: "another repository does not", change: func(s *CreateSpec) { s.RepoDir = "/other" }},
		{name: "an owner does not", change: func(s *CreateSpec) { s.Parent = ptr(ids.WorkspaceID("owner")) }},
		{name: "a fork source does not", change: func(s *CreateSpec) { s.ForkFrom = ptr(ids.WorkspaceID("source")) }},
		{name: "a context workspace does not", change: func(s *CreateSpec) { s.ContextWorkspace = ptr(ids.WorkspaceID("ctx")) }},
		{name: "one-shot does not", change: func(s *CreateSpec) { s.OneShot = true }},
		{name: "cut from origin does not", change: func(s *CreateSpec) { s.FromOrigin, s.Landing = true, &wsm.LandingChoice{} }},
		{name: "an explicit landing on the primary branch does not", change: func(s *CreateSpec) { s.Landing = &wsm.LandingChoice{} }},
		{name: "a landing into a workspace does not", change: func(s *CreateSpec) { s.Landing = &wsm.LandingChoice{Into: ptr(ids.WorkspaceID("into"))} }},
		{name: "a name does not", change: func(s *CreateSpec) { s.Name = "chosen" }},
		{name: "a model does not", change: func(s *CreateSpec) { s.Model = "opus" }},
		{name: "a priority does not", change: func(s *CreateSpec) { s.Priority = &p1 }},
		{name: "deploy hooks do not", change: func(s *CreateSpec) {
			s.DeployHooks.PreDeploy = []*repoconfigv1.DeployHook{{}}
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			first := f.verbs.EnterCreate(base)
			other := base
			tc.change(&other)

			// Act
			second := f.verbs.EnterCreate(other)

			// Assert
			if second.Joined() != tc.wantJoined || (second.ID() == first.ID()) != tc.wantJoined {
				t.Fatalf("joined=%v (ids %s, %s), want joined %v", second.Joined(), first.ID(), second.ID(), tc.wantJoined)
			}
		})
	}
}

func TestAHeldRequestSurvivesItsEncoding(t *testing.T) {
	// Arrange
	p2 := wsm.PriorityP2
	spec := CreateSpec{
		RepoDir: "/r", InitialPrompt: "fix it", OneShot: true, FromOrigin: true, Landing: &wsm.LandingChoice{},
		ContextWorkspace: ptr(ids.WorkspaceID("ctx")), Parent: ptr(ids.WorkspaceID("owner")),
		ForkFrom: ptr(ids.WorkspaceID("source")), Requester: ptr(ids.WorkspaceID("asker")),
		OwnerNotice: "land it", ConsentedUngatedMode: "bypassPermissions", Name: "n", Model: "opus",
		PermissionMode: "bypassPermissions", Priority: &p2,
		DeployHooks: wsm.DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{{}}},
	}

	// Act
	encoded, err := encodeHeldSpec(spec)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	decoded, err := decodeHeldSpec(encoded)

	// Assert: the same request, under the same key.
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if createKey(decoded) != createKey(spec) || *decoded.Requester != *spec.Requester || *decoded.Priority != p2 || decoded.Landing == nil || decoded.Landing.Into != nil {
		t.Fatalf("decoded %+v, want the request encoded", decoded)
	}
}

func TestAHeldRequestKeepsItsLandingTarget(t *testing.T) {
	into := ids.WorkspaceID("into")
	tests := []struct {
		name    string
		landing *wsm.LandingChoice
	}{
		{name: "the relationship's default", landing: nil},
		{name: "the primary branch", landing: &wsm.LandingChoice{}},
		{name: "a workspace", landing: &wsm.LandingChoice{Into: &into}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			spec := CreateSpec{RepoDir: "/r", InitialPrompt: "fix it", Landing: tt.landing}

			// Act
			encoded, err := encodeHeldSpec(spec)
			if err != nil {
				t.Fatalf("encode: %v", err)
			}
			decoded, err := decodeHeldSpec(encoded)

			// Assert
			if err != nil {
				t.Fatalf("decode: %v", err)
			}
			if !reflect.DeepEqual(decoded.Landing, tt.landing) {
				t.Fatalf("landing = %+v, want %+v", decoded.Landing, tt.landing)
			}
		})
	}
}

func TestRecoverQueuedCreates(t *testing.T) {
	t.Run("a held create resumes on the restored signal and answers its op", func(t *testing.T) {
		// Arrange: a row a previous daemon wrote.
		f, spec := heldCreateFixture(t)
		script(f, headlessAnswer{text: FixtureMintedName})
		encoded, _ := encodeHeldSpec(spec)
		f.db.PutQueuedCreate(context.Background(), wsm.QueuedCreate{Key: createKey(spec), ID: "create-old", Spec: encoded, OpIDs: []string{"op-old"}})

		// Act
		if err := f.verbs.RecoverQueuedCreates(context.Background()); err != nil {
			t.Fatalf("RecoverQueuedCreates: %v", err)
		}
		joined := f.verbs.EnterCreate(spec)
		f.network.restore()
		f.recoveredMu.Lock()
		rec := f.recovered["op-old"]
		f.recoveredMu.Unlock()
		<-rec.ended

		// Assert: an identical request joined it, and it was created.
		if !joined.Joined() || joined.ID() != "create-old" {
			t.Fatalf("joined=%v id=%s, want the recovered create", joined.Joined(), joined.ID())
		}
		if _, created, _ := rec.snapshot(); len(created) != 1 {
			t.Fatalf("recovered op created %+v, want one workspace", created)
		}
		if held := f.db.heldCreates(); len(held) != 0 {
			t.Fatalf("held rows = %+v, want none", held)
		}
	})
	t.Run("an unreadable held request is recorded and left held", func(t *testing.T) {
		// Arrange
		f := newFixture(t)
		f.db.PutQueuedCreate(context.Background(), wsm.QueuedCreate{Key: "k", ID: "create-bad", Spec: "{not json"})

		// Act
		err := f.verbs.RecoverQueuedCreates(context.Background())

		// Assert
		if err != nil {
			t.Fatalf("RecoverQueuedCreates = %v, want the others taken up", err)
		}
		if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opPendingCreate,
			Message: "a held create's request is unreadable; it is left held and not resumed"}); !ok {
			t.Fatalf("no error record: %+v", f.log.logger.Records())
		}
		if held := f.db.heldCreates(); len(held) != 1 {
			t.Fatalf("held rows = %+v, want it left", held)
		}
	})
	t.Run("an unreadable store fails the take-up", func(t *testing.T) {
		// Arrange
		f := newFixture(t)
		f.db.queuedReadErr = errors.New("database is locked")

		// Act
		err := f.verbs.RecoverQueuedCreates(context.Background())

		// Assert
		if err == nil || !strings.Contains(err.Error(), "database is locked") {
			t.Fatalf("RecoverQueuedCreates = %v, want the read's failure", err)
		}
		if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opPendingCreate,
			Message: "the creates held for the network could not be read"}); !ok {
			t.Fatalf("no error record: %+v", f.log.logger.Records())
		}
	})
}

func TestTheShippedDequeuedBriefSplices(t *testing.T) {
	// Arrange
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), BriefCreateDequeued)
	if err != nil {
		t.Fatalf("load: %v", err)
	}
	// Act
	_, err = brief.Splice(map[string]string{"request": "r", "outcome": "o"})
	// Assert
	if err != nil {
		t.Fatalf("splice: %v", err)
	}
}

// ptr answers a pointer to V.
func ptr[T any](v T) *T { return &v }
