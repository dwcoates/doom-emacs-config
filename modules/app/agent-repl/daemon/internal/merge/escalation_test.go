package merge

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// bannerRaisedFor reports whether a banner of KIND was raised whose click
// opens workspace WS.
func (h *harness) bannerRaisedFor(kind string, ws ids.WorkspaceID) bool {
	for _, b := range h.banners.seen() {
		if b.kind == kind && b.ws == ws {
			return true
		}
	}
	return false
}

// escalationBriefs is every brief the harness submitted to the escalation
// workspace.
func escalationBriefs(h *harness) []promptqueue.Submission {
	return submissionsTo(h, "ws-escalate", "")
}

func TestEveryEscalationPutsTheDeployBeforeTheUserInANewWorkspace(t *testing.T) {
	tests := []struct {
		name         string
		act          func(t *testing.T, h *harness)
		wantSummary  string
		wantProblems string
	}{
		{name: "the intake's escalation carries its findings",
			act: func(t *testing.T, h *harness) {
				if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}); err != nil {
					t.Fatalf("DecideMediation: %v", err)
				}
			}, wantSummary: "pages regressed", wantProblems: "1. the reaper errors on every pass"},
		{name: "a turn that decided nothing",
			act:         func(t *testing.T, h *harness) { h.o.Mediate(context.Background()) },
			wantSummary: "the deploy intake ended its turn without deciding the deploy", wantProblems: "states no separate findings"},
		{name: "a remediation closed before it landed",
			act: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-remedy"); err != nil {
					t.Fatal(err)
				}
				// A closed workspace's record stays readable, as the
				// store keeps it, so the escalation names it.
				h.db.mu.Lock()
				h.db.workspaces["ws-remedy"] = wsm.Workspace{ID: "ws-remedy", Name: "remedy-the-reaper"}
				h.db.mu.Unlock()
				h.o.OnWorkspaceClosed(context.Background(), "ws-remedy")
			}, wantSummary: "the deploy remediation remedy-the-reaper was closed before its fix landed", wantProblems: "states no separate findings"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if !strings.HasPrefix(tt.name, "a turn") {
				withIntake(t, h, deployStarted)
			}

			// Act
			tt.act(t, h)

			// Assert: the check stays owed, holding the queue for the user.
			if got := heldEscalation(t, h); got != "ws-escalate" {
				t.Fatalf("the check waits on escalation %q, want the escalation workspace", got)
			}
			if len(h.checks.made) == 0 || !strings.HasPrefix(h.checks.made[len(h.checks.made)-1], "deploy-escalation-") {
				t.Fatalf("made %v, want an escalation workspace made last", h.checks.made)
			}
			if !slices.Contains(h.checks.efforts, "escalation:ws-escalate:recorded=false") {
				t.Fatalf("efforts %v, want the escalation at the remediation's effort", h.checks.efforts)
			}
			if subs := escalationBriefs(h); len(subs) != 1 {
				t.Fatalf("briefed the escalation %d times, want once", len(subs))
			}
			values := h.briefValues[len(h.briefValues)-1]
			if !strings.Contains(values["summary"], tt.wantSummary) || !strings.Contains(values["problems"], tt.wantProblems) {
				t.Fatalf("brief values %v, want the summary %q and findings %q", values, tt.wantSummary, tt.wantProblems)
			}
			if !h.bannerRaisedFor(desktopnotify.KindDeployMediated, "ws-escalate") {
				t.Fatal("the user was not notified with a click opening the escalation workspace")
			}
		})
	}
}

func TestAnEscalationThatCannotReachTheUserIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "a workspace that cannot be made", message: "the escalation workspace could not be made; the user is not told of the escalated deploy",
			arrange: func(h *harness) { h.checks.escalationErr = errors.New("disk full") }},
		{name: "an effort that cannot be set", message: "the escalation workspace's effort could not be set; it is not briefed and the user is not told",
			arrange: func(h *harness) { h.checks.effortErr = errors.New("no catalog") }},
		{name: "a brief that cannot be composed", message: "the escalation workspace's brief could not be composed; it is not briefed and the user is not told",
			arrange: func(h *harness) { delete(h.briefs, BriefDeployEscalation) }},
		{name: "a brief that cannot be submitted", message: "the escalation workspace's brief could not be submitted; the user is not told",
			arrange: func(h *harness) { h.queue.err = errors.New("queue closed") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			tt.arrange(h)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "yours"})

			// Assert: the queue stays held for the user, escalated with no
			// workspace for the next pass to make, and the lost escalation is
			// the record's to tell.
			if err != nil || heldEscalation(t, h) != "" {
				t.Fatalf("DecideMediation = %v, want the check held, escalated with no workspace", err)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
			if h.bannerRaised(desktopnotify.KindDeployMediated) {
				t.Fatal("the user was notified of an escalation workspace that was never briefed")
			}
		})
	}
}

func TestAnEscalationWorkspaceIsRecordedAsLandingDirectly(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)

	// Act
	if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}); err != nil {
		t.Fatalf("DecideMediation: %v", err)
	}

	// Assert
	repo, found, err := h.db.DeployEscalationOf(context.Background(), "ws-escalate")
	if err != nil || !found || repo != mediationKey(h) {
		t.Fatalf("DeployEscalationOf = (%q, %v, %v), want the escalation recorded for its queue", repo, found, err)
	}
}

func TestAnEscalationThatCannotBeRecordedIsNotBriefed(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.db.mu.Lock()
	h.db.escalationRecordErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	_ = h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem})

	// Assert
	if subs := escalationBriefs(h); len(subs) != 0 {
		t.Fatalf("briefed the escalation %d times, want none", len(subs))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the escalation workspace could not be recorded as landing directly; it is not briefed and the user is not told"}); !ok {
		t.Fatalf("records = %+v, want the failed record at ERROR", h.logs.Records())
	}
}

// heldEscalation answers the escalation workspace the owed check waits on,
// empty while it is still being made, failing unless the check is owed and
// escalated: an escalation holds the queue for the user.
func heldEscalation(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	m := owedCheck(t, h)
	if m == nil || !m.Escalated() {
		t.Fatalf("check %+v, want it owed and escalated, holding the queue", m)
	}
	return m.Escalation
}

// oldEscalation is the escalation workspace a check's problems were put
// before the user in, before the check under test.
const oldEscalation ids.WorkspaceID = "ws-old-escalation"

// aProblemKey is aProblem's one group.
var aProblemKey = wsm.ProblemKey{Operation: "daemon.worktreereap.pass", Message: "boom"}

// withEscalationOwning records oldEscalation, closed or not, as an
// escalation of the harness's queue owning KEYS.
func withEscalationOwning(t *testing.T, h *harness, closed bool, keys ...wsm.ProblemKey) {
	t.Helper()
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaces[oldEscalation] = wsm.Workspace{ID: oldEscalation, Name: "deploy-escalation-old", Repo: theRepository, Closed: closed}
	h.db.mu.Unlock()
	if err := h.db.RecordDeployEscalation(ctx, oldEscalation, mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}
	if err := h.db.OwnDeployProblems(ctx, mediationKey(h), keys, oldEscalation, "the location grant", deployStarted); err != nil {
		t.Fatal(err)
	}
}

// updatesTo is every update the harness submitted to escalation WS.
func updatesTo(h *harness, ws ids.WorkspaceID) []string {
	var out []string
	for _, sub := range submissionsTo(h, ws, BriefDeployEscalationUpdate) {
		out = append(out, submittedText(sub))
	}
	return out
}

// problemOf reads the problem KEY of the harness's queue.
func problemOf(t *testing.T, h *harness, key wsm.ProblemKey) wsm.DeployProblem {
	t.Helper()
	problems, err := h.db.DeployProblems(context.Background(), mediationKey(h))
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range problems {
		if p.Key == key {
			return p
		}
	}
	t.Fatalf("problems %+v, want one for %+v", problems, key)
	return wsm.DeployProblem{}
}

// escalationMade reports whether a new escalation workspace was made.
func escalationMade(h *harness) bool {
	return slices.ContainsFunc(h.checks.made, func(n string) bool { return strings.HasPrefix(n, "deploy-escalation-") })
}

func TestAProblemAnOpenEscalationOwnsGoesBackToIt(t *testing.T) {
	fresh := DeployProblem{Statement: "the store stalls", Evidence: []string{`{"level":"error","operation":"store.write","message":"stalled"}`}}
	tests := []struct {
		name       string
		closed     bool
		fixed      bool
		groups     []wsm.WindowGroup
		problems   []DeployProblem
		wantUpdate string
		wantNew    bool
	}{
		{name: "an escalated finding the open escalation owns", problems: aProblem, wantUpdate: "the reaper errors on every pass"},
		{name: "a group recurring after the open escalation's fix deployed", fixed: true,
			groups: []wsm.WindowGroup{{Operation: aProblemKey.Operation, Message: aProblemKey.Message, Class: wsm.WindowRecurred}}, wantUpdate: "Recurring after this workspace's fix deployed"},
		{name: "a finding of the open escalation beside a fresh one", problems: append(slices.Clone(aProblem), fresh), wantUpdate: "the reaper errors on every pass", wantNew: true},
		{name: "a finding of an escalation the user closed", closed: true, problems: aProblem, wantNew: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			withEscalationOwning(t, h, tt.closed, aProblemKey)
			if tt.fixed {
				if _, err := h.db.FixDeployProblemsOf(context.Background(), oldEscalation, deployStarted); err != nil {
					t.Fatal(err)
				}
			}
			toldWith(t, h, tt.groups...)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "the grant is missing", Problems: tt.problems})

			// Assert: the check waits on the escalation that took it.
			holder := oldEscalation
			if tt.wantNew {
				holder = "ws-escalate"
			}
			if got := heldEscalation(t, h); err != nil || got != holder {
				t.Fatalf("DecideMediation = %v, check waits on %q; want it held by %q", err, got, holder)
			}
			updates := updatesTo(h, oldEscalation)
			if tt.wantUpdate == "" && len(updates) != 0 {
				t.Fatalf("updated the closed escalation: %v", updates)
			}
			if tt.wantUpdate != "" {
				if len(updates) != 1 || !strings.Contains(updates[0], tt.wantUpdate) {
					t.Fatalf("updates %v, want one saying %q", updates, tt.wantUpdate)
				}
				if !h.bannerRaisedFor(desktopnotify.KindDeployMediated, oldEscalation) {
					t.Fatal("the user was not told the open escalation was updated")
				}
				if p := problemOf(t, h, aProblemKey); p.Owner != oldEscalation || p.FixedAt != nil {
					t.Fatalf("problem %+v, want the open escalation owning it again, unfixed", p)
				}
			}
			if escalationMade(h) != tt.wantNew {
				t.Fatalf("made %v, want a new escalation %v", h.checks.made, tt.wantNew)
			}
		})
	}
}

func TestADecisionConfirmsToTheOpenEscalationThatItsProblemsPersist(t *testing.T) {
	tests := []struct {
		name     string
		decision Decision
	}{
		{name: "an approval", decision: Decision{Kind: MediationApprove, Summary: "nothing new"}},
		{name: "a dispatch", decision: Decision{Kind: MediationDispatch, Summary: "the store stalls",
			Problems: []DeployProblem{{Statement: "the store stalls", Evidence: []string{`{"level":"error","operation":"store.write","message":"stalled"}`}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			withEscalationOwning(t, h, false, aProblemKey)
			toldWith(t, h, wsm.WindowGroup{Operation: aProblemKey.Operation, Message: aProblemKey.Message, Class: wsm.WindowOwned})

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", tt.decision)

			// Assert
			if err != nil {
				t.Fatalf("DecideMediation: %v", err)
			}
			updates := updatesTo(h, oldEscalation)
			if len(updates) != 1 || !strings.Contains(updates[0], "Still recurring while this workspace settles them") || !strings.Contains(updates[0], aProblemKey.Operation) {
				t.Fatalf("updates %v, want one confirming the group persists", updates)
			}
			if h.bannerRaisedFor(desktopnotify.KindDeployMediated, oldEscalation) {
				t.Fatal("the user was notified of a confirmation that tells nothing new")
			}
			if p := problemOf(t, h, aProblemKey); p.Findings != "the location grant" {
				t.Fatalf("problem %+v, want its findings unchanged by a confirmation", p)
			}
			if escalationMade(h) {
				t.Fatalf("made %v, want no escalation", h.checks.made)
			}
		})
	}
}

func TestAnEscalationsUpdateThatCannotBeSentIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "an update that cannot be composed", message: "the escalation's update could not be composed; it is not told that its problems persist",
			arrange: func(h *harness) { delete(h.briefs, BriefDeployEscalationUpdate) }},
		{name: "an update that cannot be submitted", message: "the escalation's update could not be submitted; it is not told that its problems persist",
			arrange: func(h *harness) {
				h.queue.mu.Lock()
				h.queue.err = errors.New("queue closed")
				h.queue.mu.Unlock()
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			withEscalationOwning(t, h, false, aProblemKey)
			if _, err := h.db.FixDeployProblemsOf(context.Background(), oldEscalation, deployStarted); err != nil {
				t.Fatal(err)
			}
			tt.arrange(h)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "the grant is missing", Problems: aProblem})

			// Assert: the check waits on the escalation it went back to.
			if got := heldEscalation(t, h); err != nil || got != oldEscalation {
				t.Fatalf("DecideMediation = %v, check waits on %q; want it held by the open escalation", err, got)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
			if h.bannerRaisedFor(desktopnotify.KindDeployMediated, oldEscalation) {
				t.Fatal("the user was told of an update that was never sent")
			}
			if p := problemOf(t, h, aProblemKey); p.FixedAt == nil {
				t.Fatalf("problem %+v, want it left as it stood, untold", p)
			}
		})
	}
}

func TestOwnersThatCannotBeReadEscalateNothing(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	withEscalationOwning(t, h, false, aProblemKey)
	h.db.mu.Lock()
	h.db.workspaceErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "the grant is missing", Problems: aProblem})

	// Assert: the check stays held, escalated with no workspace.
	if err != nil || heldEscalation(t, h) != "" {
		t.Fatalf("DecideMediation = %v, want the check held, escalated with no workspace", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "could not read which open escalations own the deploy's problems; none is told, and nothing is escalated anew"}); !ok {
		t.Fatalf("records = %v, want the failed read at ERROR", h.logs.Records())
	}
	if escalationMade(h) || len(updatesTo(h, oldEscalation)) != 0 {
		t.Fatalf("made %v, updated %v; want nothing escalated", h.checks.made, updatesTo(h, oldEscalation))
	}
}

func TestRouteEscalationSortsEachGroupAndFinding(t *testing.T) {
	owners := map[wsm.ProblemKey]ids.WorkspaceID{aProblemKey: oldEscalation}
	group := func(k wsm.ProblemKey, class wsm.WindowClass) wsm.WindowGroup {
		return wsm.WindowGroup{Operation: k.Operation, Message: k.Message, Class: class}
	}
	other := wsm.ProblemKey{Operation: "store.write", Message: "stalled"}
	otherProblem := DeployProblem{Statement: "the store stalls", Evidence: []string{`{"operation":"store.write","message":"stalled"}`}}
	tests := []struct {
		name       string
		groups     []wsm.WindowGroup
		problems   []DeployProblem
		escalating bool
		want       escalationRoutes
	}{
		{name: "a group an open escalation owns and is at work on persists",
			groups: []wsm.WindowGroup{group(aProblemKey, wsm.WindowOwned)},
			want:   escalationRoutes{order: []ids.WorkspaceID{oldEscalation}, updates: map[ids.WorkspaceID]*escalationUpdate{oldEscalation: {persisting: []wsm.ProblemKey{aProblemKey}}}}},
		{name: "a group another workspace owns goes nowhere",
			groups: []wsm.WindowGroup{group(other, wsm.WindowOwned)}, escalating: true,
			want: escalationRoutes{updates: map[ids.WorkspaceID]*escalationUpdate{}}},
		{name: "a recurred group goes nowhere unless the check escalates",
			groups: []wsm.WindowGroup{group(aProblemKey, wsm.WindowRecurred)},
			want:   escalationRoutes{updates: map[ids.WorkspaceID]*escalationUpdate{}}},
		{name: "an escalated recurred group goes back to its open escalation",
			groups: []wsm.WindowGroup{group(aProblemKey, wsm.WindowRecurred)}, escalating: true,
			want: escalationRoutes{order: []ids.WorkspaceID{oldEscalation}, updates: map[ids.WorkspaceID]*escalationUpdate{oldEscalation: {recurred: []wsm.ProblemKey{aProblemKey}}}}},
		{name: "an escalated group no open escalation owns is a new escalation's",
			groups: []wsm.WindowGroup{group(other, wsm.WindowNew)}, escalating: true,
			want: escalationRoutes{updates: map[ids.WorkspaceID]*escalationUpdate{}, freshKeys: []wsm.ProblemKey{other}}},
		{name: "findings go to the escalation owning their group, or a new one",
			problems: append(slices.Clone(aProblem), otherProblem), escalating: true,
			want: escalationRoutes{order: []ids.WorkspaceID{oldEscalation}, updates: map[ids.WorkspaceID]*escalationUpdate{oldEscalation: {problems: aProblem}}, fresh: []DeployProblem{otherProblem}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := routeEscalation(owners, wsm.CheckWindow{Groups: tt.groups}, tt.problems, tt.escalating)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("routeEscalation = %+v, want %+v", got, tt.want)
			}
		})
	}
}

// decidedAtStop arms H's intake stop to read whether the check owed at
// deployStarted is decided when the stop ends the intake's turn, the moment
// the round awaiting that turn reads it.
func decidedAtStop(t *testing.T, h *harness) *bool {
	t.Helper()
	decided := new(bool)
	h.checks.onStopNow = func() {
		m, err := h.db.MediationOf(context.Background(), mediationKey(h))
		if err != nil {
			t.Errorf("MediationOf at the stop = %v", err)
			return
		}
		*decided = !owes(m, deployStarted) || m.Dispatched || m.Escalated()
	}
	return decided
}

func TestEveryDecisionIsRecordedBeforeItStopsItsIntake(t *testing.T) {
	tests := []struct {
		name     string
		decision Decision
	}{
		{name: "an escalation", decision: Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}},
		{name: "a dispatch", decision: Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem}},
		{name: "an approval", decision: Decision{Kind: MediationApprove, Summary: "nothing new"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			decided := decidedAtStop(t, h)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", tt.decision)

			// Assert
			if err != nil || !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}) || !*decided {
				t.Fatalf("DecideMediation = %v, stopped %v, decided at the stop %v; want the decision recorded before the stop", err, h.checks.stopped, *decided)
			}
		})
	}
}

func TestADecisionThatStopsItsIntakeRunsOnALiveContext(t *testing.T) {
	tests := []struct {
		name     string
		decision Decision
	}{
		{name: "an escalation", decision: Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}},
		{name: "an approval", decision: Decision{Kind: MediationApprove, Summary: "nothing new"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: stopping the intake ends the session that sent the
			// decision, cancelling its request.
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			h.checks.onStopNow = cancel

			// Act
			err := h.o.DecideMediation(ctx, "ws-intake", tt.decision)

			// Assert
			if err != nil || !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}) {
				t.Fatalf("DecideMediation = %v, stopped %v; want the intake stopped", err, h.checks.stopped)
			}
			if ctx.Err() == nil {
				t.Fatal("the stop did not cancel the request, so the test proves nothing")
			}
			if !slices.Equal(h.checks.createCtxErrs, map[bool][]error{true: {nil}, false: nil}[tt.decision.Kind == MediationEscalate]) {
				t.Fatalf("escalation creation context errors %v, want one on a live context for an escalation", h.checks.createCtxErrs)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error"}); ok {
				t.Fatalf("records = %v, want no error", h.logs.Records())
			}
		})
	}
}

func TestADecisionWhileTheDaemonStopsLeavesTheCheckOwed(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	if idle := h.o.stopMediation(); idle != nil {
		<-idle
	}

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "nothing new"})

	// Assert
	if !errors.Is(err, errMediationStopped) || owedCheck(t, h) == nil {
		t.Fatalf("DecideMediation = %v, want it refused with the check still owed", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: mediationNotRun}); !ok {
		t.Fatalf("records = %v, want the refused decision recorded", h.logs.Records())
	}
}

// escalatedBy escalates the owed check through its intake and answers the
// escalation that holds it.
func escalatedBy(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	withIntake(t, h, deployStarted)
	if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "yours", Problems: aProblem}); err != nil {
		t.Fatalf("DecideMediation: %v", err)
	}
	return heldEscalation(t, h)
}

func TestClosingTheEscalationIsWhatReleasesItsHold(t *testing.T) {
	tests := []struct {
		name         string
		closed       ids.WorkspaceID
		wantReleased bool
	}{
		{name: "the user closes the escalation the check waits on", closed: "ws-escalate", wantReleased: true},
		{name: "another workspace closing leaves the hold", closed: theWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			escalatedBy(t, h)

			// Act
			h.o.OnWorkspaceClosed(context.Background(), tt.closed)

			// Assert
			if released := owedCheck(t, h) == nil; released != tt.wantReleased {
				t.Fatalf("released %v, want %v", released, tt.wantReleased)
			}
			_, recorded := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
				Message: "the user closed the escalation; its check is released and the queue advances"})
			if recorded != tt.wantReleased {
				t.Fatalf("records = %v, want the release recorded %v", h.logs.Records(), tt.wantReleased)
			}
		})
	}
}

func TestAnEscalationsOwnFixDeployingHandsItsCheckToAFreshIntake(t *testing.T) {
	// Arrange: the check is escalated, and the escalation's finish lands.
	h := checked(t)
	owe(t, h)
	esc := escalatedBy(t, h)
	ctx := context.Background()
	landed := deployStarted.Add(30 * time.Second)
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: esc, Repo: mediationKey(h), StartedAt: landed}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, esc, wsm.RemediationGating, wsm.RemediationLanding, landed); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, esc, tipAfter, nil, landed); err != nil {
		t.Fatal(err)
	}

	// Act: the escalation's fix deploys, and an unrelated deploy before it.
	oweAt(t, h, deployStarted.Add(time.Minute))

	// Assert
	m := owedCheck(t, h)
	if m == nil || m.Escalated() || m.Intake != "" {
		t.Fatalf("check %+v, want the fix's deploy owed afresh, unescalated, for a fresh intake", m)
	}
}

func TestAnUnrelatedDeployLeavesTheEscalationsHold(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	esc := escalatedBy(t, h)

	// Act
	oweAt(t, h, deployStarted.Add(time.Minute))
	h.o.Mediate(context.Background())

	// Assert: still held by the same escalation, and no intake told of it.
	if got := heldEscalation(t, h); got != esc || len(intakeTellings(h)) != 0 {
		t.Fatalf("check waits on %q, intake told %d times; want the hold kept and no intake told", got, len(intakeTellings(h)))
	}
}

func TestAnEscalationThatCouldNotBeMadeIsMadeByTheNextPass(t *testing.T) {
	// Arrange: the first escalation workspace could not be made.
	h := checked(t)
	owe(t, h)
	h.checks.escalationErr = errors.New("disk full")
	if escalatedBy(t, h) != "" {
		t.Fatal("the check names an escalation that was never made")
	}
	h.checks.mu.Lock()
	h.checks.escalationErr = nil
	h.checks.mu.Unlock()

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if got := heldEscalation(t, h); got != "ws-escalate" {
		t.Fatalf("check waits on %q, want the escalation made by the pass", got)
	}
	if subs := escalationBriefs(h); len(subs) != 1 || !strings.Contains(submittedText(subs[0]), "the reaper errors on every pass") {
		t.Fatalf("briefed %v, want the escalation briefed once with the kept findings", subs)
	}
}

func TestAnIntakeThatCannotBeClosedAfterItsDecisionIsRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.checks.stopErr = errors.New("kill refused")

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "yours"})

	// Assert: the decision stands, and the failed close is the record's.
	if err != nil || heldEscalation(t, h) != "ws-escalate" {
		t.Fatalf("DecideMediation = %v, want the escalation held", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the intake decided but could not be closed"}); !ok {
		t.Fatalf("records = %v, want the failed close at ERROR", h.logs.Records())
	}
}
