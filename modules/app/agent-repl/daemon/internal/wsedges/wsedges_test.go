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
func (r *recorder) Closed(before wsm.Workspace)   { r.got = append(r.got, "closed:"+string(before.ID)) }
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
		{name: "a close", edge: func(h *Hub) { h.Closed(wsm.Workspace{ID: ws}) }, want: "closed:ws-1"},
		{name: "a pull request taken as the workspace's own", edge: func(h *Hub) { h.PullRequestOwned(ws, pr) }, want: "pull_request:ws-1:acme/definitions#7"},
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

func TestOnlyALandedQueueChangeIsAnEdge(t *testing.T) {
	for _, kind := range []merge.QueueChangeKind{merge.QueueChangeEnqueued, merge.QueueChangeStarted, merge.QueueChangeFailed, merge.QueueChangeDequeued} {
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

// closesDB is a registry whose MarkClosed is recorded and may fail.
type closesDB struct {
	wsm.DB
	ws       wsm.Workspace
	readErr  error
	closeErr error
	closed   int
	// exits counts the queue exits made.
	exits int
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

func (d *closesDB) RemoveMergeQueueEntry(_ context.Context, _ wsm.RepoKey, _ wsm.WorkspaceID, _ string, closes *wsm.Closure) error {
	return d.exit(closes)
}

func (d *closesDB) DropAdmittedMerge(_ context.Context, _ wsm.RepoKey, _ wsm.WorkspaceID, _ wsm.LeaseID, _ string, closes *wsm.Closure) error {
	return d.exit(closes)
}

// exit stands for a queue exit that closes its workspace when CLOSES is set.
func (d *closesDB) exit(closes *wsm.Closure) error {
	if d.closeErr != nil {
		return d.closeErr
	}
	d.exits++
	if closes != nil {
		d.closed++
	}
	return nil
}

func TestReportingEdgesTellsTheCloseAQueueExitMakes(t *testing.T) {
	closes := &wsm.Closure{By: wsm.CloseByDaemon}
	tests := []struct {
		name   string
		ws     wsm.Workspace
		exit   func(wsm.DB) error
		closed int
		want   []string
	}{
		{name: "a removal closing an open merge workspace is told", ws: wsm.Workspace{ID: ws}, exit: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", ws, "evicted", closes)
		}, closed: 1, want: []string{"closed:ws-1"}},
		{name: "a drop closing an open merge workspace is told", ws: wsm.Workspace{ID: ws}, exit: func(db wsm.DB) error {
			return db.DropAdmittedMerge(context.Background(), "/repo/.git", ws, "lease", "evicted", closes)
		}, closed: 1, want: []string{"closed:ws-1"}},
		{name: "an exit that closes nothing tells nothing", ws: wsm.Workspace{ID: ws}, exit: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", ws, "merged", nil)
		}},
		{name: "an exit closing a closed workspace tells nothing", ws: wsm.Workspace{ID: ws, Closed: true}, exit: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", ws, "evicted", closes)
		}, closed: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			inner := &closesDB{ws: tt.ws}
			db := ReportingEdges(inner, New(r), dlog.NewTestLogger())

			// Act
			err := tt.exit(db)

			// Assert
			if err != nil || inner.exits != 1 || inner.closed != tt.closed {
				t.Fatalf("exit = %v, exits %d, closes %d; want the exit made with %d close(s)", err, inner.exits, inner.closed, tt.closed)
			}
			if !slices.Equal(r.got, tt.want) {
				t.Errorf("told %v, want %v", r.got, tt.want)
			}
		})
	}
}

func TestReportingEdgesRefusesAClosingExitWhoseWorkspaceCannotBeRead(t *testing.T) {
	// Arrange
	r := &recorder{}
	log := dlog.NewTestLogger()
	inner := &closesDB{readErr: errors.New("disk gone")}
	db := ReportingEdges(inner, New(r), log)

	// Act
	err := db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", ws, "evicted", &wsm.Closure{By: wsm.CloseByDaemon})

	// Assert
	if err == nil || inner.exits != 0 || len(r.got) != 0 {
		t.Fatalf("exit = %v, exits %d, told %v; want a refusal that writes and tells nothing", err, inner.exits, r.got)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op,
		Message: "could not read a workspace before closing it, so the close is refused rather than left untold"}); !ok {
		t.Errorf("no error record: %+v", log.Records())
	}
}

func TestReportingEdgesTellsTheCloseOfAnOpenWorkspace(t *testing.T) {
	tests := []struct {
		name string
		ws   wsm.Workspace
		want []string
	}{
		{name: "an open workspace closing is told", ws: wsm.Workspace{ID: ws}, want: []string{"closed:ws-1"}},
		{name: "re-closing a closed workspace tells nothing", ws: wsm.Workspace{ID: ws, Closed: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			inner := &closesDB{ws: tt.ws}
			db := ReportingEdges(inner, New(r), dlog.NewTestLogger())

			// Act
			err := db.MarkClosed(context.Background(), ws, wsm.Closure{By: wsm.CloseByUser})

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
