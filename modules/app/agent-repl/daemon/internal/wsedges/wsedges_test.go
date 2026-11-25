package wsedges

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

const ws = ids.WorkspaceID("ws-1")

// recorder is an observer that records every edge as "<edge>:<workspace>".
type recorder struct{ got []string }

func (r *recorder) TurnStarted(ws ids.WorkspaceID) { r.got = append(r.got, "turn_started:"+string(ws)) }
func (r *recorder) TurnEnded(ws ids.WorkspaceID)   { r.got = append(r.got, "turn_ended:"+string(ws)) }
func (r *recorder) TurnEndedUnobserved(ws ids.WorkspaceID) {
	r.got = append(r.got, "turn_ended_unobserved:"+string(ws))
}
func (r *recorder) NeedsInput(ws ids.WorkspaceID) { r.got = append(r.got, "needs_input:"+string(ws)) }
func (r *recorder) Landed(ws ids.WorkspaceID)     { r.got = append(r.got, "landed:"+string(ws)) }
func (r *recorder) Closed(before wsm.Workspace, how wsm.CloseHow) {
	r.got = append(r.got, "closed:"+string(before.ID)+":"+how.String())
}
func (r *recorder) Deployed(ws ids.WorkspaceID, failure string) {
	r.got = append(r.got, "deployed:"+string(ws)+":"+failure)
}
func (r *recorder) Evicted(ws ids.WorkspaceID, reason string) {
	r.got = append(r.got, "evicted:"+string(ws)+":"+reason)
}
func (r *recorder) ColdGated(ws ids.WorkspaceID) { r.got = append(r.got, "cold_gated:"+string(ws)) }
func (r *recorder) Retargeted(change wsm.ParentChange) {
	r.got = append(r.got, "retargeted:"+string(change.Workspace)+":"+parentName(change.Before)+"->"+parentName(change.After))
}

// parentName spells a target's parent for a recorded edge, "top" for none.
func parentName(target wsm.MergeTarget) string {
	if target.Parent == nil {
		return "top"
	}
	return string(*target.Parent)
}

func (r *recorder) PullRequestOwned(ws ids.WorkspaceID, pr wsm.PullRequestKey) {
	r.got = append(r.got, "pull_request:"+string(ws)+":"+pr.String())
}

var pr = wsm.PullRequestKey{Owner: "acme", Repo: "definitions", Number: 7}

func TestTheHubTellsEveryObserverEveryEdge(t *testing.T) {
	tests := []struct {
		name string
		edge func(h *Hub)
		want string
	}{
		{name: "a turn starting", edge: func(h *Hub) { h.TurnStarted(ws) }, want: "turn_started:ws-1"},
		{name: "a turn ending", edge: func(h *Hub) { h.TurnEnded(ws) }, want: "turn_ended:ws-1"},
		{name: "a turn ending unobserved", edge: func(h *Hub) { h.TurnEndedUnobserved(ws) }, want: "turn_ended_unobserved:ws-1"},
		{name: "an ask opening", edge: func(h *Hub) { h.NeedsInput(ws) }, want: "needs_input:ws-1"},
		{name: "a landing", edge: func(h *Hub) { h.Landed(ws) }, want: "landed:ws-1"},
		{name: "a close", edge: func(h *Hub) { h.Closed(wsm.Workspace{ID: ws}, wsm.HowClosed) }, want: "closed:ws-1:closed"},
		{name: "a kill", edge: func(h *Hub) { h.Closed(wsm.Workspace{ID: ws}, wsm.HowKilled) }, want: "closed:ws-1:killed"},
		{name: "a nuke", edge: func(h *Hub) { h.Closed(wsm.Workspace{ID: ws}, wsm.HowNuked) }, want: "closed:ws-1:nuked"},
		{name: "a deploy that ran", edge: func(h *Hub) { h.Deployed(ws, "") }, want: "deployed:ws-1:"},
		{name: "a deploy that failed", edge: func(h *Hub) { h.Deployed(ws, "the build failed") }, want: "deployed:ws-1:the build failed"},
		{name: "an eviction", edge: func(h *Hub) { h.Evicted(ws, "dequeued") }, want: "evicted:ws-1:dequeued"},
		{name: "a cold gate", edge: func(h *Hub) { h.ColdGated(ws) }, want: "cold_gated:ws-1"},
		{name: "a pull request taken as the workspace's own", edge: func(h *Hub) { h.PullRequestOwned(ws, pr) }, want: "pull_request:ws-1:acme/definitions#7"},
		{name: "a retarget", edge: func(h *Hub) { h.Retargeted(retarget) }, want: "retargeted:ws-1:parent-a->top"},
		{name: "a merge queue landing", edge: func(h *Hub) {
			h.QueueChanged(merge.QueueChange{Kind: merge.QueueChangeLanded, Subject: merge.QueueMember{Workspace: ws}})
		}, want: "landed:ws-1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			a, b := &recorder{}, &recorder{}
			h := New(a, b)

			// Act
			tt.edge(h)

			// Assert
			for _, r := range []*recorder{a, b} {
				if !slices.Equal(r.got, []string{tt.want}) {
					t.Errorf("observer told %v, want [%s]", r.got, tt.want)
				}
			}
		})
	}
}

func TestOnlyALandedOrUnlandedLeavingQueueChangeIsAnEdge(t *testing.T) {
	for _, kind := range []merge.QueueChangeKind{merge.QueueChangeEnqueued, merge.QueueChangeRequeued, merge.QueueChangeStarted, merge.QueueChangeStepped,
		merge.QueueChangeReordered, merge.QueueChangePaused, merge.QueueChangeResumed} {
		t.Run(kind.String(), func(t *testing.T) {
			// Arrange
			r := &recorder{}

			// Act
			New(r).QueueChanged(merge.QueueChange{Kind: kind, Subject: merge.QueueMember{Workspace: ws}})

			// Assert
			if len(r.got) != 0 {
				t.Errorf("observer told %v, want nothing", r.got)
			}
		})
	}
}

func TestAMergeLeavingTheQueueUnlandedIsItsSubjectsEviction(t *testing.T) {
	tests := []struct {
		name   string
		change merge.QueueChange
		want   string
	}{
		{name: "a failed merge that recorded its cause", change: merge.QueueChange{Kind: merge.QueueChangeFailed, Cause: "pick_failed"}, want: "evicted:ws-1:pick failed"},
		{name: "a failed merge that recorded none", change: merge.QueueChange{Kind: merge.QueueChangeFailed}, want: "evicted:ws-1:failed"},
		{name: "a merge taken off the queue", change: merge.QueueChange{Kind: merge.QueueChangeDequeued, Cause: "dequeued"}, want: "evicted:ws-1:dequeued"},
		{name: "a merge abandoned by a restart", change: merge.QueueChange{Kind: merge.QueueChangeDequeued, Cause: "restart_failed"}, want: "evicted:ws-1:restart failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			tt.change.Subject = merge.QueueMember{Workspace: ws}

			// Act
			New(r).QueueChanged(tt.change)

			// Assert
			if !slices.Equal(r.got, []string{tt.want}) {
				t.Errorf("observer told %v, want [%s]", r.got, tt.want)
			}
		})
	}
}

func TestADeployEndTellsEachWorkspaceItsLandingCarries(t *testing.T) {
	tests := []struct {
		name string
		end  merge.DeployEnd
		want []string
	}{
		{name: "a build that runs", end: merge.DeployEnd{Landed: []ids.WorkspaceID{"ws-1", "ws-2"}}, want: []string{"deployed:ws-1:", "deployed:ws-2:"}},
		{name: "a deploy that failed", end: merge.DeployEnd{Failure: "the build failed", Landed: []ids.WorkspaceID{"ws-1"}}, want: []string{"deployed:ws-1:the build failed"}},
		{name: "a deploy carrying no landing tells nobody", end: merge.DeployEnd{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}

			// Act
			New(r).DeployEnded(tt.end)

			// Assert
			if !slices.Equal(r.got, tt.want) {
				t.Errorf("observer told %v, want %v", r.got, tt.want)
			}
		})
	}
}

// closesDB is a registry whose MarkClosed is recorded and may fail.
type closesDB struct {
	wsm.DB
	ws       wsm.Workspace
	readErr  error
	closeErr error
	closed   int
}

func (d *closesDB) Workspace(context.Context, wsm.WorkspaceID) (wsm.Workspace, error) {
	return d.ws, d.readErr
}

func (d *closesDB) MarkClosed(context.Context, wsm.WorkspaceID, wsm.Closure) error {
	if d.closeErr != nil {
		return d.closeErr
	}
	d.closed++
	return nil
}

func TestReportingEdgesTellsTheCloseOfAnOpenWorkspace(t *testing.T) {
	tests := []struct {
		name string
		ws   wsm.Workspace
		how  wsm.CloseHow
		want []string
	}{
		{name: "an open workspace closing is told", ws: wsm.Workspace{ID: ws}, want: []string{"closed:ws-1:closed"}},
		{name: "a kill is told as a kill", ws: wsm.Workspace{ID: ws}, how: wsm.HowKilled, want: []string{"closed:ws-1:killed"}},
		{name: "a nuke is told as a nuke", ws: wsm.Workspace{ID: ws}, how: wsm.HowNuked, want: []string{"closed:ws-1:nuked"}},
		{name: "re-closing a closed workspace tells nothing", ws: wsm.Workspace{ID: ws, Closed: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			inner := &closesDB{ws: tt.ws}
			db := ReportingEdges(inner, New(r), dlog.NewTestLogger())

			// Act
			err := db.MarkClosed(context.Background(), ws, wsm.Closure{By: wsm.CloseByUser, How: tt.how})

			// Assert
			if err != nil || inner.closed != 1 {
				t.Fatalf("MarkClosed = %v, inner closes = %d, want the close made", err, inner.closed)
			}
			if !slices.Equal(r.got, tt.want) {
				t.Errorf("told %v, want %v", r.got, tt.want)
			}
		})
	}
}

func TestReportingEdgesCloseFails(t *testing.T) {
	tests := []struct {
		name    string
		inner   *closesDB
		closes  int
		message string
	}{
		{name: "a workspace that cannot be read is not closed", inner: &closesDB{readErr: errors.New("disk gone")},
			message: "could not read a workspace before closing it, so the close is refused rather than left untold"},
		{name: "a close that fails is told to nobody", inner: &closesDB{ws: wsm.Workspace{ID: ws}, closeErr: errors.New("disk gone")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			log := dlog.NewTestLogger()
			db := ReportingEdges(tt.inner, New(r), log)

			// Act
			err := db.MarkClosed(context.Background(), ws, wsm.Closure{By: wsm.CloseByUser})

			// Assert
			if err == nil || tt.inner.closed != tt.closes || len(r.got) != 0 {
				t.Fatalf("MarkClosed = %v, closes %d, told %v; want a failure that closes and tells nothing", err, tt.inner.closed, r.got)
			}
			if tt.message != "" {
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tt.message}); !ok {
					t.Errorf("no error record: %+v", log.Records())
				}
			}
		})
	}
}

// linksDB is a registry whose SetWorkspaceLink is recorded and may fail.
type linksDB struct {
	wsm.DB
	err  error
	sets []wsm.LinkSet
}

func (d *linksDB) SetWorkspaceLink(_ context.Context, _ wsm.WorkspaceID, set wsm.LinkSet) ([]wsm.WorkspaceLink, error) {
	if d.err != nil {
		return nil, d.err
	}
	d.sets = append(d.sets, set)
	return []wsm.WorkspaceLink{{URL: set.URL, Label: set.Label}}, nil
}

func TestReportingEdgesTellsAPullRequestTakenAsTheWorkspacesOwn(t *testing.T) {
	const url = "https://github.com/acme/definitions/pull/7"
	tests := []struct {
		name string
		set  wsm.LinkSet
		want []string
	}{
		{name: "an owned pull request taking a new subscription is told",
			set:  wsm.LinkSet{URL: url, Label: "the definitions change", Owned: true, PullRequest: &pr, Subscribe: &wsm.PullRequestSubscription{Subscriber: ws, PullRequest: pr}},
			want: []string{"pull_request:ws-1:acme/definitions#7"}},
		{name: "an owned pull request already followed tells nothing",
			set: wsm.LinkSet{URL: url, Label: "the definitions change", Owned: true, PullRequest: &pr}},
		{name: "a referenced pull request tells nothing",
			set: wsm.LinkSet{URL: url, Label: "the definitions change", PullRequest: &pr}},
		{name: "an owned link that is no pull request tells nothing",
			set: wsm.LinkSet{URL: "https://example.com/doc", Label: "the design doc", Owned: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			inner := &linksDB{}
			db := ReportingEdges(inner, New(r), dlog.NewTestLogger())

			// Act
			links, err := db.SetWorkspaceLink(context.Background(), ws, tt.set)

			// Assert
			if err != nil || len(inner.sets) != 1 || len(links) != 1 || links[0].URL != tt.set.URL {
				t.Fatalf("SetWorkspaceLink = %v, %v; inner sets %d; want the link stored and answered", links, err, len(inner.sets))
			}
			if !slices.Equal(r.got, tt.want) {
				t.Errorf("told %v, want %v", r.got, tt.want)
			}
		})
	}
}

func TestReportingEdgesTellsNothingOfALinkThatFailsToStore(t *testing.T) {
	// Arrange
	r := &recorder{}
	cause := errors.New("disk gone")
	db := ReportingEdges(&linksDB{err: cause}, New(r), dlog.NewTestLogger())
	set := wsm.LinkSet{URL: "https://github.com/acme/definitions/pull/7", Label: "the definitions change", Owned: true,
		PullRequest: &pr, Subscribe: &wsm.PullRequestSubscription{Subscriber: ws, PullRequest: pr}}

	// Act
	_, err := db.SetWorkspaceLink(context.Background(), ws, set)

	// Assert
	if !errors.Is(err, cause) || len(r.got) != 0 {
		t.Fatalf("SetWorkspaceLink = %v, told %v; want the store's failure and nothing told", err, r.got)
	}
}

func TestReportingEdgesPanicsOnASubscriptionNamingNoPullRequest(t *testing.T) {
	// Arrange
	db := ReportingEdges(&linksDB{}, New(&recorder{}), dlog.NewTestLogger())
	set := wsm.LinkSet{URL: "https://example.com/doc", Label: "the design doc", Owned: true, Subscribe: &wsm.PullRequestSubscription{Subscriber: ws}}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("SetWorkspaceLink did not panic on a subscription naming no pull request")
		}
	}()

	// Act
	_, _ = db.SetWorkspaceLink(context.Background(), ws, set)
}

// parentA is the parent a retarget takes ws-1 away from.
var parentA = wsm.WorkspaceID("parent-a")

// retarget moves ws-1 from under parent-a to the top level.
var retarget = wsm.ParentChange{Workspace: ws, Before: wsm.MergeTarget{Parent: &parentA}}

// parentsDB is a registry whose retargets answer CHANGE, STALE or ERR.
type parentsDB struct {
	wsm.DB
	change wsm.ParentChange
	stale  bool
	err    error
}

func (d *parentsDB) SetParent(context.Context, wsm.WorkspaceID, wsm.Retarget) (wsm.ParentChange, error) {
	return d.change, d.err
}

func (d *parentsDB) RestoreParent(context.Context, wsm.ParentChange) (bool, error) {
	return d.stale, d.err
}

func TestReportingEdgesTellsEveryRetarget(t *testing.T) {
	tests := []struct {
		name  string
		inner *parentsDB
		act   func(db wsm.DB) error
		want  []string
	}{
		{name: "a retarget is told as it changed",
			inner: &parentsDB{change: retarget},
			act: func(db wsm.DB) error {
				_, err := db.SetParent(context.Background(), ws, wsm.Retarget{})
				return err
			},
			want: []string{"retargeted:ws-1:parent-a->top"}},
		{name: "a restore is told as the reverse of the retarget it undid",
			inner: &parentsDB{},
			act: func(db wsm.DB) error {
				_, err := db.RestoreParent(context.Background(), retarget)
				return err
			},
			want: []string{"retargeted:ws-1:top->parent-a"}},
		{name: "a stale restore changed nothing and tells nothing",
			inner: &parentsDB{stale: true},
			act: func(db wsm.DB) error {
				_, err := db.RestoreParent(context.Background(), retarget)
				return err
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			db := ReportingEdges(tt.inner, New(r), dlog.NewTestLogger())

			// Act
			err := tt.act(db)

			// Assert
			if err != nil || !slices.Equal(r.got, tt.want) {
				t.Fatalf("err = %v, told %v; want %v", err, r.got, tt.want)
			}
		})
	}
}

func TestReportingEdgesTellsNothingOfARetargetThatFailed(t *testing.T) {
	tests := []struct {
		name string
		act  func(db wsm.DB) error
	}{
		{name: "a refused retarget", act: func(db wsm.DB) error {
			_, err := db.SetParent(context.Background(), ws, wsm.Retarget{})
			return err
		}},
		{name: "a failed restore", act: func(db wsm.DB) error {
			_, err := db.RestoreParent(context.Background(), retarget)
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			cause := errors.New("disk gone")
			db := ReportingEdges(&parentsDB{err: cause}, New(r), dlog.NewTestLogger())

			// Act
			err := tt.act(db)

			// Assert
			if !errors.Is(err, cause) || len(r.got) != 0 {
				t.Fatalf("err = %v, told %v; want the failure and nothing told", err, r.got)
			}
		})
	}
}
