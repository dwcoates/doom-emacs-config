package gather

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/encoding/protojson"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

var instant = time.Date(2026, 11, 10, 9, 0, 0, 0, time.UTC)

// ---- the harness ----

// store is a real in-memory registry whose named reads and writes a test can
// fail.
type store struct {
	wsm.DB
	fail map[string]error
}

func (s *store) err(name string) error { return s.fail[name] }

func (s *store) ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error) {
	if err := s.err("ListWorkspaces"); err != nil {
		return nil, err
	}
	return s.DB.ListWorkspaces(ctx)
}

func (s *store) PutGather(ctx context.Context, g wsm.Gather) error {
	if err := s.err("PutGather"); err != nil {
		return err
	}
	return s.DB.PutGather(ctx, g)
}

func (s *store) GathersOutstandingOn(ctx context.Context, ws wsm.WorkspaceID) ([]wsm.Gather, error) {
	if err := s.err("GathersOutstandingOn"); err != nil {
		return nil, err
	}
	return s.DB.GathersOutstandingOn(ctx, ws)
}

func (s *store) RecordTurnSuccession(ctx context.Context, m wsm.TurnSuccession) error {
	if err := s.err("RecordTurnSuccession"); err != nil {
		return err
	}
	return s.DB.RecordTurnSuccession(ctx, m)
}

func (s *store) UnsettledGathers(ctx context.Context) ([]wsm.Gather, error) {
	if err := s.err("UnsettledGathers"); err != nil {
		return nil, err
	}
	return s.DB.UnsettledGathers(ctx)
}

// submitted is one notice the fake prompt handler took.
type submitted struct {
	ws     ids.WorkspaceID
	text   string
	key    string
	origin conversationv1.PromptOrigin
}

// prompts is the prompt handler: it records each notice, and answers err.
type prompts struct {
	mu   sync.Mutex
	got  []submitted
	err  error
	took chan submitted
}

func (p *prompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, _ wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.err != nil {
		return prompthandler.Outcome{}, p.err
	}
	s := submitted{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), key: key, origin: origin}
	p.got = append(p.got, s)
	if p.took != nil {
		p.took <- s
	}
	return prompthandler.Outcome{Turn: "notice-turn"}, nil
}

func (p *prompts) all() []submitted {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]submitted(nil), p.got...)
}

type harness struct {
	t         *testing.T
	db        *store
	clock     *clocktest.Scripted
	log       *dlog.TestSurfaces
	prompts   *prompts
	statuses  map[ids.WorkspaceID]*agentreplv1.WorkspaceStatusActivity
	statusErr error
	briefErr  error
	svc       *Service
	run       context.Context
	caller    wsm.Workspace
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	db, err := wsm.OpenInMemory(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = db.Close() })
	run, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	h := &harness{t: t, db: &store{DB: db, fail: map[string]error{}}, clock: clocktest.New(instant), log: dlog.NewTestSurfaces(),
		prompts: &prompts{}, statuses: map[ids.WorkspaceID]*agentreplv1.WorkspaceStatusActivity{}, run: run}
	h.svc = New(Deps{
		DB: h.db,
		Status: func(_ context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error) {
			if h.statusErr != nil {
				return nil, h.statusErr
			}
			activity := h.statuses[ws]
			if activity == nil {
				activity = &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_TurnRunning{TurnRunning: &agentreplv1.WorkspaceStatusActivityTurnRunning{}}}
			}
			return &agentreplv1.WorkspaceStatus{Standing: &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Open{
				Open: &agentreplv1.WorkspaceStatusOpen{Activity: activity}}}}, nil
		},
		Briefs: func(_ string, values map[string]string) (string, error) {
			if h.briefErr != nil {
				return "", h.briefErr
			}
			return values["label"] + "\n" + values["notice"], nil
		},
		Clock: h.clock,
		Log:   h.log,
	})
	h.caller = h.register("caller")
	return h
}

// register registers an open workspace named NAME.
func (h *harness) register(name string) wsm.Workspace {
	h.t.Helper()
	dir := "/repos/" + name + "-" + string(wsm.NewTurnID())
	w, _, err := h.db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{Name: name, Branch: "feature", ParentBranch: "master", RepoDir: dir})
	if err != nil {
		h.t.Fatal(err)
	}
	return w
}

func (h *harness) close(w wsm.Workspace) {
	h.t.Helper()
	if err := h.db.MarkClosed(context.Background(), w.ID, wsm.Closure{At: instant, By: wsm.CloseByUser}); err != nil {
		h.t.Fatal(err)
	}
}

// ctx is a handler's context, carrying the run the timers live on.
func (h *harness) ctx() context.Context { return withRun(context.Background(), h.run) }

// handle runs one event as the loop would.
func (h *harness) handle(e event) { e.handle(h.ctx(), h.svc, h.prompts) }

// accepting is a Submit that mints turn "<name>-turn" for each target, and
// records the keys it was handed.
type accepting struct {
	keys   []string
	refuse map[string]*agentreplv1.SubmitPromptError
	fail   error
	calls  int
}

func (a *accepting) submit(_ context.Context, target wsm.Workspace, key string) (ids.TurnID, *agentreplv1.SubmitPromptError, error) {
	a.calls++
	a.keys = append(a.keys, key)
	if a.fail != nil {
		return "", nil, a.fail
	}
	if r, ok := a.refuse[target.Name]; ok {
		return "", r, nil
	}
	return ids.TurnID(target.Name + "-turn"), nil, nil
}

// create makes a gather on NAMES, as the loop would.
func (h *harness) create(sub *accepting, names ...string) (*agentreplv1.GatherWorkspacesResponse, error) {
	return h.svc.create(h.ctx(), Request{Caller: h.caller.ID, Names: names, PromptText: "status?", Key: "key-1", Submit: sub.submit})
}

// made makes a gather on NAMES that must succeed, answering its record.
func (h *harness) made(names ...string) wsm.Gather {
	h.t.Helper()
	resp, err := h.create(&accepting{}, names...)
	if err != nil || resp.GetSuccess() == nil {
		h.t.Fatalf("create = %v, %v", resp, err)
	}
	g, err := h.db.Gather(context.Background(), wsm.GatherID(resp.GetSuccess().GetGather().GetId()))
	if err != nil {
		h.t.Fatal(err)
	}
	return g
}

func (h *harness) gather(id wsm.GatherID) wsm.Gather {
	h.t.Helper()
	g, err := h.db.Gather(context.Background(), id)
	if err != nil {
		h.t.Fatal(err)
	}
	return g
}

// notice decodes the GatherNotice a submitted notice carries.
func notice(t *testing.T, s submitted) *agentreplv1.GatherNotice {
	t.Helper()
	body := s.text[strings.Index(s.text, "\n")+1:]
	var n agentreplv1.GatherNotice
	if err := protojson.Unmarshal([]byte(body), &n); err != nil {
		t.Fatalf("the notice carries no GatherNotice: %v\n%s", err, s.text)
	}
	return &n
}

func logged(records []dlog.Record, level, message string) bool {
	for _, r := range records {
		if r.Level == level && r.Operation == op && r.Message == message {
			return true
		}
	}
	return false
}

func activity(a any) *agentreplv1.WorkspaceStatusActivity {
	switch v := a.(type) {
	case *agentreplv1.WorkspaceStatusActivity_NeedsInput:
		return &agentreplv1.WorkspaceStatusActivity{Activity: v}
	case *agentreplv1.WorkspaceStatusActivity_Merging:
		return &agentreplv1.WorkspaceStatusActivity{Activity: v}
	case *agentreplv1.WorkspaceStatusActivity_ColdGate:
		return &agentreplv1.WorkspaceStatusActivity{Activity: v}
	case *agentreplv1.WorkspaceStatusActivity_Fault:
		return &agentreplv1.WorkspaceStatusActivity{Activity: v}
	case *agentreplv1.WorkspaceStatusActivity_Idle:
		return &agentreplv1.WorkspaceStatusActivity{Activity: v}
	}
	panic("activity: undeclared arm")
}

var (
	needsInput = &agentreplv1.WorkspaceStatusActivity_NeedsInput{NeedsInput: &agentreplv1.WorkspaceStatusActivityNeedsInput{}}
	merging    = &agentreplv1.WorkspaceStatusActivity_Merging{Merging: &agentreplv1.WorkspaceStatusActivityMerging{}}
	coldGate   = &agentreplv1.WorkspaceStatusActivity_ColdGate{ColdGate: &agentreplv1.WorkspaceStatusActivityColdGate{}}
	faulted    = &agentreplv1.WorkspaceStatusActivity_Fault{Fault: &agentreplv1.WorkspaceStatusActivityFault{Description: "overloaded"}}
	idle       = &agentreplv1.WorkspaceStatusActivity_Idle{Idle: &agentreplv1.WorkspaceStatusActivityIdle{}}
)

// ---- making a gather ----

func TestCreateRefusesACallerThatCannotBeTold(t *testing.T) {
	tests := []struct {
		name   string
		caller func(h *harness) ids.WorkspaceID
		want   string
	}{
		{"an unknown caller", func(*harness) ids.WorkspaceID { return "nobody" }, "unknown_caller"},
		{"a closed caller", func(h *harness) ids.WorkspaceID { h.close(h.caller); return h.caller.ID }, "caller_closed"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("a")
			sub := &accepting{}

			// Act
			resp, err := h.svc.create(h.ctx(), Request{Caller: tc.caller(h), Names: []string{"a"}, Key: "k", Submit: sub.submit})

			// Assert
			if err != nil || resp.GetError() == nil {
				t.Fatalf("create = %v, %v; want the error arm", resp, err)
			}
			if arm := resp.GetError().ProtoReflect().WhichOneof(resp.GetError().ProtoReflect().Descriptor().Oneofs().ByName("cause")); arm == nil || string(arm.Name()) != tc.want {
				t.Fatalf("arm = %v, want %s", arm, tc.want)
			}
			if sub.calls != 0 {
				t.Fatal("a target was prompted for a gather that was never made")
			}
		})
	}
}

func TestCreateRefusesEachTargetWithItsCause(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness, sub *accepting) string
		want    string
	}{
		{"a name no workspace has", func(*harness, *accepting) string { return "ghost" }, "unknown_workspace"},
		{"a name only a closed workspace has", func(h *harness, _ *accepting) string { h.close(h.register("old")); return "old" }, "closed"},
		{"a name several open workspaces have", func(h *harness, _ *accepting) string { h.register("twin"); h.register("twin"); return "twin" }, "ambiguous_name"},
		{"the caller's own name", func(*harness, *accepting) string { return "caller" }, "is_caller"},
		{"a question waiting on the user", func(h *harness, _ *accepting) string {
			h.statuses[h.register("asking").ID] = activity(needsInput)
			return "asking"
		}, "needs_input"},
		{"a merge in progress", func(h *harness, _ *accepting) string {
			h.statuses[h.register("landing").ID] = activity(merging)
			return "landing"
		}, "merging"},
		{"the submission's own refusal", func(h *harness, sub *accepting) string {
			h.register("gated")
			sub.refuse = map[string]*agentreplv1.SubmitPromptError{"gated": {Reason: &agentreplv1.SubmitPromptError_ColdGate{ColdGate: &agentreplv1.SubmitPromptColdGate{Detail: "cold"}}}}
			return "gated"
		}, "prompt_refused"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sub := &accepting{}
			name := tc.arrange(h, sub)

			// Act
			resp, err := h.create(sub, name)

			// Assert
			refused := resp.GetError().GetNoneAccepted().GetRefused()
			if err != nil || len(refused) != 1 || refused[0].GetName() != name {
				t.Fatalf("create = %v, %v; want the one target refused", resp, err)
			}
			cause := refused[0].GetCause().ProtoReflect()
			if arm := cause.WhichOneof(cause.Descriptor().Oneofs().ByName("cause")); arm == nil || string(arm.Name()) != tc.want {
				t.Fatalf("cause = %v, want %s", refused[0].GetCause(), tc.want)
			}
			if n, _ := h.db.UnsettledGathers(context.Background()); len(n) != 0 {
				t.Fatal("a gather was recorded with no accepted target")
			}
		})
	}
}

func TestCreateMakesTheGatherOfTheAcceptedTargetsAndReportsTheRest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	h.register("b")
	sub := &accepting{refuse: map[string]*agentreplv1.SubmitPromptError{"b": {Reason: &agentreplv1.SubmitPromptError_NoSession{NoSession: &agentreplv1.SubmitPromptNoSession{}}}}}

	// Act
	resp, err := h.create(sub, "a", "b", "a")

	// Assert
	success := resp.GetSuccess()
	if err != nil || success == nil || len(success.GetAccepted()) != 1 || success.GetAccepted()[0].GetName() != "a" || len(success.GetRefused()) != 1 || success.GetRefused()[0].GetName() != "b" {
		t.Fatalf("create = %v, %v; want a accepted once and b refused", resp, err)
	}
	if success.GetDeadlineAtUnixMs() != instant.Add(Timeout).UnixMilli() {
		t.Fatalf("deadline = %d, want five minutes after creation", success.GetDeadlineAtUnixMs())
	}
	g := h.gather(wsm.GatherID(success.GetGather().GetId()))
	if len(g.Targets) != 1 || g.Targets[0].Workspace != a.ID || g.Targets[0].Turn != "a-turn" {
		t.Fatalf("targets = %+v, want a and the turn its prompt was minted", g.Targets)
	}
	if len(sub.keys) != 2 || sub.keys[0] != "key-1/"+string(a.ID) {
		t.Fatalf("keys = %v, want each target's key derived from the request's, a repeated name prompted once", sub.keys)
	}
}

func TestCreateRepeatingAKeyAnswersTheGatherItMadeAndPromptsNobody(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.register("a")
	first := h.made("a")
	again := &accepting{}

	// Act
	resp, err := h.create(again, "a")

	// Assert
	if err != nil || resp.GetSuccess().GetGather().GetId() != string(first.ID) {
		t.Fatalf("create = %v, %v; want the first gather answered", resp, err)
	}
	if again.calls != 0 {
		t.Fatal("a repeated request prompted a target again")
	}
}

func TestCreateFailsLoudlyWhenItCannotProceed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness, sub *accepting)
		message string
	}{
		{"the registry cannot be read", func(h *harness, _ *accepting) { h.db.fail["ListWorkspaces"] = errors.New("io") },
			"could not read the registry to resolve the targets' names; no gather is made"},
		{"a target's status cannot be read", func(h *harness, _ *accepting) { h.statusErr = errors.New("io") },
			"could not read a target's status; the gather is not made"},
		{"a submission fails", func(_ *harness, sub *accepting) { sub.fail = errors.New("io") },
			"could not submit the gather's prompt to a target; the gather is not made, and the targets prompted before it answer nobody"},
		{"the gather cannot be recorded", func(h *harness, _ *accepting) { h.db.fail["PutGather"] = errors.New("io") },
			"could not record the gather; its targets were prompted but their answers reach nobody"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("a")
			sub := &accepting{}
			tc.arrange(h, sub)

			// Act
			resp, err := h.create(sub, "a")

			// Assert
			if err == nil || resp != nil {
				t.Fatalf("create = %v, %v; want it to fail", resp, err)
			}
			if !logged(h.log.Records(), "error", tc.message) {
				t.Fatalf("no ERROR record %q: %v", tc.message, h.log.Records())
			}
			if n, _ := h.db.UnsettledGathers(context.Background()); len(n) != 0 {
				t.Fatal("a failed gather left a row")
			}
		})
	}
}

// ---- resolving targets ----

func TestAnAnswerResolvesOnlyTheTargetWhosePromptTheTurnCarries(t *testing.T) {
	tests := []struct {
		name   string
		folds  bool
		turn   ids.TurnID
		wanted bool
	}{
		{"the prompt's own turn", false, "a-turn", true},
		{"another turn of the target", false, "other", false},
		{"the turn the prompt was folded into", true, "running", true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			a := h.register("a")
			h.register("b")
			g := h.made("a", "b")
			if tc.folds {
				h.handle(foldEvent{ws: a.ID, turn: "a-turn", into: "running"})
			}

			// Act
			h.handle(answerEvent{ws: a.ID, turn: tc.turn, text: "all green"})

			// Assert
			got := h.gather(g.ID).Targets[0]
			if resolved := got.Resolution != nil; resolved != tc.wanted {
				t.Fatalf("resolved = %v, want %v", resolved, tc.wanted)
			}
			if tc.wanted && (got.Resolution.Kind != wsm.GatherAnswered || got.Resolution.Text != "all green") {
				t.Fatalf("resolution = %+v, want the turn's answer", got.Resolution)
			}
		})
	}
}

func TestAFoldOnAWorkspaceNoGatherWaitsOnIsNotRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")

	// Act
	h.handle(foldEvent{ws: a.ID, turn: "t", into: "r"})

	// Assert
	if got, _ := h.db.SucceedingTurn(context.Background(), "t"); got != "t" {
		t.Fatalf("SucceedingTurn = %q, want no fold recorded", got)
	}
}

func TestAFoldThatCannotBeRecordedIsLoud(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	h.made("a")
	h.db.fail["RecordTurnSuccession"] = errors.New("io")

	// Act
	h.handle(foldEvent{ws: a.ID, turn: "a-turn", into: "running"})

	// Assert
	if !logged(h.log.Records(), "error", "could not record a fold; a gather waiting on the folded prompt does not see the joined turn's answer") {
		t.Fatalf("no ERROR record: %v", h.log.Records())
	}
}

func TestAnAnswerWhoseGathersCannotBeReadIsLoud(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	h.db.fail["GathersOutstandingOn"] = errors.New("io")

	// Act
	h.handle(answerEvent{ws: a.ID, turn: "a-turn", text: "x"})

	// Assert
	if !logged(h.log.Records(), "error", "could not read the gathers waiting on the workspace; nothing is resolved") {
		t.Fatalf("no ERROR record: %v", h.log.Records())
	}
}

func TestACloseResolvesTheTargetClosed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	h.register("b")
	g := h.made("a", "b")

	// Act
	h.handle(closeEvent{ws: a.ID})

	// Assert
	if r := h.gather(g.ID).Targets[0].Resolution; r == nil || r.Kind != wsm.GatherClosed {
		t.Fatalf("resolution = %+v, want closed", r)
	}
}

// ---- the first update and later answers ----

func TestTheLastAnswerTellsTheFirstUpdateAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a, b := h.register("a"), h.register("b")
	g := h.made("a", "b")
	h.handle(answerEvent{ws: a.ID, turn: "a-turn", text: "a done"})
	if len(h.prompts.all()) != 0 {
		t.Fatal("the first update was told before every target answered")
	}

	// Act
	h.handle(answerEvent{ws: b.ID, turn: "b-turn", text: "b done"})

	// Assert
	got := h.prompts.all()
	if len(got) != 1 || got[0].ws != h.caller.ID || got[0].origin != Origin || got[0].key != "gather-"+string(g.ID)+"-first" {
		t.Fatalf("notices = %+v, want one first update to the caller on the workspace notice path", got)
	}
	first := notice(t, got[0]).GetFirstUpdate()
	if first.GetTrigger().GetAllResolved() == nil || len(first.GetResolved()) != 2 || len(first.GetOutstanding()) != 0 ||
		first.GetResolved()[0].GetTarget().GetName() != "a" || first.GetResolved()[0].GetAnswered().GetText() != "a done" {
		t.Fatalf("first update = %v, want both answers under their names and nothing outstanding", first)
	}
	if told := h.gather(g.ID); told.FirstTold == nil || told.FirstTold.Trigger != wsm.GatherAllResolved || !told.Settled() {
		t.Fatalf("gather = %+v, want the first update recorded told and the gather settled", told)
	}
}

func TestTheDeadlineTellsTheFirstUpdateWithWhatIsOutstanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	h.register("b")
	g := h.made("a", "b")
	h.handle(answerEvent{ws: a.ID, turn: "a-turn", text: "a done"})

	// Act
	h.handle(deadlineEvent{gather: g.ID})

	// Assert
	got := h.prompts.all()
	if len(got) != 1 {
		t.Fatalf("notices = %+v, want the first update", got)
	}
	first := notice(t, got[0]).GetFirstUpdate()
	if first.GetTrigger().GetTimedOut() == nil || len(first.GetResolved()) != 1 || len(first.GetOutstanding()) != 1 ||
		first.GetOutstanding()[0].GetTarget().GetName() != "b" || first.GetOutstanding()[0].GetWorking() == nil {
		t.Fatalf("first update = %v, want a's answer and b outstanding, working", first)
	}
}

func TestADeadlineAfterTheFirstUpdateTellsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	g := h.made("a")
	h.handle(answerEvent{ws: a.ID, turn: "a-turn", text: "done"})

	// Act
	h.handle(deadlineEvent{gather: g.ID})

	// Assert
	if got := h.prompts.all(); len(got) != 1 {
		t.Fatalf("notices = %d, want only the latch's first update", len(got))
	}
}

func TestAnAnswerAfterTheFirstUpdateIsToldOnItsOwn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.register("a")
	b, c := h.register("b"), h.register("c")
	g := h.made("a", "b", "c")
	h.handle(deadlineEvent{gather: g.ID})

	// Act
	h.handle(answerEvent{ws: b.ID, turn: "b-turn", text: "b late"})

	// Assert
	got := h.prompts.all()
	if len(got) != 2 || got[1].key != "gather-"+string(g.ID)+"-"+string(b.ID) {
		t.Fatalf("notices = %+v, want the first update then b's own", got)
	}
	later := notice(t, got[1]).GetLaterAnswer()
	if later.GetResolved().GetTarget().GetName() != "b" || later.GetResolved().GetAnswered().GetText() != "b late" || later.GetOutstandingCount() != 2 {
		t.Fatalf("later answer = %v, want b's answer with 2 still outstanding", later)
	}
	_ = c
	if told := h.gather(g.ID).Targets[1].ToldAt; told == nil {
		t.Fatal("b's later answer was not recorded told")
	}
}

func TestATargetsOutstandingReasonIsReadAsItStands(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness, b wsm.Workspace)
		want    string
	}{
		{"its turn runs", func(*harness, wsm.Workspace) {}, "working"},
		{"its prompt is held", func(h *harness, b wsm.Workspace) {
			if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{Workspace: b.ID, Turn: "b-turn", Said: &conversationv1.UserSaid{}, Origin: "PROMPT_ORIGIN_USER_SENT", QueuedAt: instant}); err != nil {
				h.t.Fatal(err)
			}
		}, "held"},
		{"its cold gate", func(h *harness, b wsm.Workspace) { h.statuses[b.ID] = activity(coldGate) }, "cold_gate"},
		{"a question waiting on the user", func(h *harness, b wsm.Workspace) { h.statuses[b.ID] = activity(needsInput) }, "needs_input"},
		{"a fault", func(h *harness, b wsm.Workspace) { h.statuses[b.ID] = activity(faulted) }, "faulted"},
		{"its turn ended without a response", func(h *harness, b wsm.Workspace) {
			h.statuses[b.ID] = activity(idle)
			ctx := context.Background()
			if err := h.db.PutTurn(ctx, wsm.Turn{ID: "b-turn", Workspace: b.ID, Text: "status?", Origin: "PROMPT_ORIGIN_AGENT_SENT", SentBy: &h.caller.ID, StartedAt: instant}); err != nil {
				h.t.Fatal(err)
			}
			if err := h.db.CloseTurn(ctx, "b-turn", instant, wsm.CloseFailed); err != nil {
				h.t.Fatal(err)
			}
		}, "ended_without_response"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			b := h.register("b")
			g := h.made("b")
			tc.arrange(h, b)

			// Act
			h.handle(deadlineEvent{gather: g.ID})

			// Assert
			got := h.prompts.all()
			if len(got) != 1 {
				t.Fatalf("notices = %+v, want the first update", got)
			}
			outstanding := notice(t, got[0]).GetFirstUpdate().GetOutstanding()
			if len(outstanding) != 1 {
				t.Fatalf("outstanding = %v, want b", outstanding)
			}
			reason := outstanding[0].ProtoReflect()
			if arm := reason.WhichOneof(reason.Descriptor().Oneofs().ByName("reason")); arm == nil || string(arm.Name()) != tc.want {
				t.Fatalf("reason = %v, want %s", outstanding[0], tc.want)
			}
		})
	}
}

func TestTellingANotice(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		wantSent  int
		wantTold  bool
		wantLevel string
		message   string
	}{
		{"a notice already submitted before a restart stands told", func(h *harness) { h.prompts.err = prompthandler.ErrDuplicateSubmission },
			0, true, "info", "the notice was already submitted before a restart; it stands told"},
		{"a notice that cannot be submitted stays owed", func(h *harness) { h.prompts.err = errors.New("io") },
			0, false, "error", "could not submit a gather notice to its caller; it is told at the next event or boot"},
		{"a closed caller is told nothing and stands told", func(h *harness) { h.close(h.caller) },
			0, true, "info", "the gather's caller is closed; the notice is not told, and `workspace gather-status` still reads it"},
		{"an open caller is told", func(*harness) {}, 1, true, "info", "told the caller a gather notice"},
		{"a notice whose brief cannot be composed stays owed", func(h *harness) { h.briefErr = errors.New("missing brief") },
			0, false, "error", "could not compose a gather notice; it is told at the next event or boot"},
		{"a first update whose targets cannot be read stays owed", func(h *harness) { h.statusErr = errors.New("io") },
			0, false, "error", "could not read where the gather's targets stand; its first update is told at the next event or boot"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("a")
			g := h.made("a")
			tc.arrange(h)

			// Act
			h.handle(deadlineEvent{gather: g.ID})

			// Assert
			if got := len(h.prompts.all()); got != tc.wantSent {
				t.Fatalf("submitted %d notices, want %d", got, tc.wantSent)
			}
			if told := h.gather(g.ID).FirstTold != nil; told != tc.wantTold {
				t.Fatalf("first update told = %v, want %v", told, tc.wantTold)
			}
			if !logged(h.log.Records(), tc.wantLevel, tc.message) {
				t.Fatalf("no %s record %q: %v", tc.wantLevel, tc.message, h.log.Records())
			}
		})
	}
}

// ---- the boot ----

func TestTheBootReArmsEveryUnsettledGather(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(h *harness, a wsm.Workspace, g wsm.Gather)
		wantSent int
		wantKind string
		wantArm  bool
	}{
		{"a target that closed while no daemon served resolves closed, closing the latch", func(h *harness, a wsm.Workspace, _ wsm.Gather) { h.close(a) },
			1, "all_resolved", false},
		{"a deadline that passed while no daemon served tells the first update", func(h *harness, _ wsm.Workspace, _ wsm.Gather) { h.clock.Set(instant.Add(Timeout)) },
			1, "timed_out", false},
		{"a deadline still ahead is armed", func(*harness, wsm.Workspace, wsm.Gather) {}, 0, "", true},
		{"a later answer never told is told", func(h *harness, _ wsm.Workspace, g wsm.Gather) {
			ctx := context.Background()
			if err := h.db.MarkGatherFirstTold(ctx, g.ID, wsm.GatherFirstTold{At: instant, Trigger: wsm.GatherTimedOut}); err != nil {
				h.t.Fatal(err)
			}
			if _, err := h.db.ResolveGatherTarget(ctx, g.ID, g.Targets[0].Workspace, wsm.GatherResolution{Kind: wsm.GatherAnswered, At: instant, Text: "late"}); err != nil {
				h.t.Fatal(err)
			}
		}, 1, "later", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			a := h.register("a")
			g := h.made("a")
			h.svc.armed = map[wsm.GatherID]bool{}
			for len(h.clock.Asked) > 0 {
				<-h.clock.Asked
			}
			tc.arrange(h, a, g)

			// Act
			h.handle(bootEvent{})

			// Assert
			got := h.prompts.all()
			if len(got) != tc.wantSent {
				t.Fatalf("notices = %+v, want %d", got, tc.wantSent)
			}
			switch tc.wantKind {
			case "all_resolved":
				if notice(t, got[0]).GetFirstUpdate().GetTrigger().GetAllResolved() == nil {
					t.Fatalf("notice = %v, want the latch's first update", notice(t, got[0]))
				}
			case "timed_out":
				if notice(t, got[0]).GetFirstUpdate().GetTrigger().GetTimedOut() == nil {
					t.Fatalf("notice = %v, want the deadline's first update", notice(t, got[0]))
				}
			case "later":
				if notice(t, got[0]).GetLaterAnswer() == nil {
					t.Fatalf("notice = %v, want the later answer", notice(t, got[0]))
				}
			}
			if armed := h.svc.armed[g.ID]; armed != tc.wantArm {
				t.Fatalf("armed = %v, want %v", armed, tc.wantArm)
			}
		})
	}
}

func TestTheBootThatCannotReadTheGathersIsLoud(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.fail["UnsettledGathers"] = errors.New("io")

	// Act
	h.handle(bootEvent{})

	// Assert
	if !logged(h.log.Records(), "error", "could not read the unsettled gathers; none is re-armed until the next boot") {
		t.Fatalf("no ERROR record: %v", h.log.Records())
	}
}

// ---- the loop, end to end ----

func TestTheDeadlineTimerTellsTheFirstUpdateThroughTheLoop(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.register("a")
	h.prompts.took = make(chan submitted, 1)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() { h.svc.Run(ctx, h.prompts); close(done) }()
	t.Cleanup(func() { cancel(); <-done })
	sub := &accepting{}
	if _, err := h.svc.Create(context.Background(), Request{Caller: h.caller.ID, Names: []string{"a"}, PromptText: "status?", Key: "k", Submit: sub.submit}); err != nil {
		t.Fatal(err)
	}
	if wait := <-h.clock.Asked; wait != Timeout {
		t.Fatalf("armed for %v, want the owner's %v", wait, Timeout)
	}

	// Act
	h.clock.Fire <- instant.Add(Timeout)

	// Assert
	if n := notice(t, <-h.prompts.took); n.GetFirstUpdate().GetTrigger().GetTimedOut() == nil {
		t.Fatalf("notice = %v, want the deadline's first update", n)
	}
}

func TestACreateAfterTheServiceStoppedIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	h.svc.Run(ctx, h.prompts)

	// Act
	_, err := h.svc.Create(context.Background(), Request{Caller: h.caller.ID, Names: []string{"a"}, Key: "k", Submit: (&accepting{}).submit})

	// Assert
	if !errors.Is(err, ErrStopped) {
		t.Fatalf("Create = %v, want ErrStopped", err)
	}
}

// ---- the status read ----

func TestStatusOfAnUnknownGatherIsItsErrorArm(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.svc.Status(context.Background(), "nope")

	// Assert
	if err != nil || resp.GetError().GetUnknownGather() == nil {
		t.Fatalf("Status = %v, %v; want unknown_gather", resp, err)
	}
}

func TestStatusAnswersTheGatherAsItStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	a := h.register("a")
	h.register("b")
	g := h.made("a", "b")
	h.handle(answerEvent{ws: a.ID, turn: "a-turn", text: "done"})

	// Act
	resp, err := h.svc.Status(h.ctx(), g.ID)

	// Assert
	status := resp.GetSuccess().GetStatus()
	if err != nil || status.GetPromptText() != "status?" || status.GetCaller().GetName() != "caller" ||
		status.GetFirstUpdate().GetPending().GetDeadlineAtUnixMs() != g.DeadlineAt.UnixMilli() ||
		len(status.GetResolved()) != 1 || len(status.GetOutstanding()) != 1 || status.GetOutstanding()[0].GetTarget().GetName() != "b" {
		t.Fatalf("Status = %v, %v; want a resolved, b outstanding, the first update pending", status, err)
	}
}

// ---- the brief ----

func TestTheNoticeBriefSplicesEveryValue(t *testing.T) {
	// Arrange
	briefs := merge.BriefsFrom("../../../prompts")

	// Act
	text, err := briefs(BriefNotice, map[string]string{"label": "gather `g`: 1 of 1 answered", "gather": "g", "notice": `{"gather":{"id":"g"}}`})

	// Assert
	if err != nil || !strings.Contains(text, `{"gather":{"id":"g"}}`) || !strings.Contains(text, "workspace gather-status g") {
		t.Fatalf("brief = %q, %v; want the notice and the status command spliced in", text, err)
	}
}
