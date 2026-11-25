package rostermirror

import (
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"sync"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/rostermirror/rostermirrortest"
)

// awaitBound bounds every wait on the mirror's goroutine. Each wait is for an
// in-process channel send a few function calls away (microseconds alone,
// under 10ms in a loaded gate run); the bound is the fallback for a hang, and
// every wait fails on the mirror's own exit first.
const awaitBound = 2 * time.Second

// fixture is one mirror under test with its incumbent and its sink.
type fixture struct {
	m       *Mirror
	log     *dlog.TestLogger
	dialed  chan *rostermirrortest.Stream
	dialErr error
	sunk    chan map[ids.WorkspaceID]string
	ran     chan struct{}

	mu      sync.Mutex
	adopted map[ids.WorkspaceID]bool
	dials   int
	// notices are the callbacks the mirror asked the next roster
	// publication to tell, in order; events are the notices asked and the
	// arm maps sunk, in order ("notice", "sink").
	notices []func(revision uint64)
	events  []string
}

func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{
		log:     dlog.NewTestLogger(),
		dialed:  make(chan *rostermirrortest.Stream, 1),
		sunk:    make(chan map[ids.WorkspaceID]string, 64),
		ran:     make(chan struct{}),
		adopted: map[ids.WorkspaceID]bool{},
	}
	m, err := New(Deps{
		Log:       f.log,
		Incumbent: "127.0.0.1:7777",
		Dial: func(ctx context.Context, address string) (Stream, error) {
			f.mu.Lock()
			f.dials++
			err := f.dialErr
			f.mu.Unlock()
			if err != nil {
				return nil, err
			}
			s := rostermirrortest.New(ctx)
			f.dialed <- s
			return s, nil
		},
		NotYetAdopted: func(ws ids.WorkspaceID) bool {
			f.mu.Lock()
			defer f.mu.Unlock()
			return !f.adopted[ws]
		},
		Sink: func(arms map[ids.WorkspaceID]string) {
			f.mu.Lock()
			f.events = append(f.events, "sink")
			f.mu.Unlock()
			f.sunk <- maps.Clone(arms)
		},
		NextPublication: func(fn func(revision uint64)) {
			f.mu.Lock()
			defer f.mu.Unlock()
			f.notices = append(f.notices, fn)
			f.events = append(f.events, "notice")
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.m = m
	return f
}

// run starts the mirror and answers its stream.
func (f *fixture) run(t *testing.T, ctx context.Context) *rostermirrortest.Stream {
	t.Helper()
	go func() {
		defer close(f.ran)
		f.m.Run(ctx)
	}()
	select {
	case s := <-f.dialed:
		return s
	case <-f.ran:
		t.Fatal("the mirror ended before it dialed")
	case <-time.After(awaitBound):
		t.Fatal("the mirror never dialed the incumbent")
	}
	return nil
}

// awaitSunk answers the next map handed to the sink.
func (f *fixture) awaitSunk(t *testing.T) map[ids.WorkspaceID]string {
	t.Helper()
	select {
	case arms := <-f.sunk:
		return arms
	case <-time.After(awaitBound):
		t.Fatal("the sink was never told")
	}
	return nil
}

// awaitEnded waits for Run to return.
func (f *fixture) awaitEnded(t *testing.T) {
	t.Helper()
	select {
	case <-f.ran:
	case <-time.After(awaitBound):
		t.Fatal("the mirror never ended")
	}
}

// published plays the next roster publication at REVISION: every notice the
// mirror asked for is told it, as the resolver tells them.
func (f *fixture) published(revision uint64) {
	f.mu.Lock()
	notices := f.notices
	f.notices = nil
	f.mu.Unlock()
	for _, fn := range notices {
		fn(revision)
	}
}

// noticesAsked answers how many notices the mirror has asked for so far.
func (f *fixture) noticesAsked() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.notices)
}

func (f *fixture) adopt(ws ids.WorkspaceID) {
	f.mu.Lock()
	f.adopted[ws] = true
	f.mu.Unlock()
}

// row is a roster row of WS drawing STATUS (nil: no arm this build decodes).
func row(ws string, status func(*frontendv1.RosterRow)) *frontendv1.RosterClosedRow {
	r := &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: ws}}}
	if status != nil {
		status(r)
	}
	return &frontendv1.RosterClosedRow{Row: r}
}

func thinking(r *frontendv1.RosterRow) {
	r.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
}

func done(r *frontendv1.RosterRow) {
	r.Status = &frontendv1.RosterRow_Done{Done: &frontendv1.RosterRowStatusDone{}}
}

// rosterFrame is one roster push carrying ROWS on its closed stream alone.
func rosterFrame(rows ...*frontendv1.RosterClosedRow) *agentreplv1.WatchWorkspaceRosterResponse {
	return streamsFrame(&frontendv1.RosterPush{Closed: &frontendv1.RosterClosedRows{Rows: rows}})
}

// streamsFrame is one roster push carrying PUSH's streams.
func streamsFrame(push *frontendv1.RosterPush) *agentreplv1.WatchWorkspaceRosterResponse {
	return &agentreplv1.WatchWorkspaceRosterResponse{Push: &agentreplv1.WatchWorkspaceRosterResponse_Streams{Streams: push}}
}

// workspacesStream is a workspaces stream drawing OPEN in the open section
// and BACKLOGGED in the backlog.
func workspacesStream(open, backlog []*frontendv1.RosterClosedRow) *frontendv1.WorkspaceRoster {
	var entries []*frontendv1.RosterRepoEntry
	for _, r := range open {
		entries = append(entries, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: &frontendv1.RosterOpenRow{Row: r.GetRow()}}})
	}
	var backlogRows []*frontendv1.RosterBacklogRow
	for _, r := range backlog {
		backlogRows = append(backlogRows, &frontendv1.RosterBacklogRow{Row: r.GetRow()})
	}
	return &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{Entries: entries}}},
		Backlog:    &frontendv1.RosterBacklogSection{BacklogRows: &frontendv1.RosterBacklogRows{Rows: backlogRows}},
	}
}

func endingFrame() *agentreplv1.WatchWorkspaceRosterResponse {
	return &agentreplv1.WatchWorkspaceRosterResponse{Push: &agentreplv1.WatchWorkspaceRosterResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}}}
}

func TestNewRefusesAMissingDependency(t *testing.T) {
	full := func() Deps {
		return Deps{
			Log: dlog.NewTestLogger(), Incumbent: "127.0.0.1:1",
			Dial:            func(context.Context, string) (Stream, error) { return nil, nil },
			NotYetAdopted:   func(ids.WorkspaceID) bool { return true },
			Sink:            func(map[ids.WorkspaceID]string) {},
			NextPublication: func(func(uint64)) {},
		}
	}
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"no logger", func(d *Deps) { d.Log = nil }},
		{"no incumbent address", func(d *Deps) { d.Incumbent = "" }},
		{"no dial", func(d *Deps) { d.Dial = nil }},
		{"no adoption standing", func(d *Deps) { d.NotYetAdopted = nil }},
		{"no sink", func(d *Deps) { d.Sink = nil }},
		{"no next publication notice", func(d *Deps) { d.NextPublication = nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.strip(&deps)

			// Act
			m, err := New(deps)

			// Assert
			if err == nil || m != nil {
				t.Fatalf("New = %v, %v; want a refusal", m, err)
			}
		})
	}
}

func TestARosterPushMirrorsTheRowsNotYetAdopted(t *testing.T) {
	cases := []struct {
		name    string
		adopted []ids.WorkspaceID
		rows    []*frontendv1.RosterClosedRow
		want    map[ids.WorkspaceID]string
	}{
		{
			name: "a row not yet adopted draws the incumbent's arm",
			rows: []*frontendv1.RosterClosedRow{row("ws-a", thinking)},
			want: map[ids.WorkspaceID]string{"ws-a": "thinking"},
		},
		{
			name:    "a row already adopted is this daemon's own",
			adopted: []ids.WorkspaceID{"ws-a"},
			rows:    []*frontendv1.RosterClosedRow{row("ws-a", thinking), row("ws-b", done)},
			want:    map[ids.WorkspaceID]string{"ws-b": "done"},
		},
		{
			name:    "a roster of adopted rows mirrors nothing",
			adopted: []ids.WorkspaceID{"ws-a"},
			rows:    []*frontendv1.RosterClosedRow{row("ws-a", thinking)},
			want:    nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			for _, ws := range tc.adopted {
				f.adopt(ws)
			}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			s := f.run(t, ctx)

			// Act
			s.Frames <- rosterFrame(tc.rows...)

			// Assert
			if got := f.awaitSunk(t); !maps.Equal(got, tc.want) {
				t.Fatalf("mirrored = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAPartialPushKeepsTheStreamsItLeavesOut(t *testing.T) {
	cases := []struct {
		name  string
		first *frontendv1.RosterPush
		next  *frontendv1.RosterPush
		want  map[ids.WorkspaceID]string
	}{
		{
			name: "the first push mirrors the open, backlogged and closed rows",
			first: &frontendv1.RosterPush{
				Workspaces: workspacesStream([]*frontendv1.RosterClosedRow{row("ws-o", thinking)}, []*frontendv1.RosterClosedRow{row("ws-h", done)}),
				Closed:     &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{row("ws-c", done)}},
			},
			want: map[ids.WorkspaceID]string{"ws-o": "thinking", "ws-h": "done", "ws-c": "done"},
		},
		{
			name: "a push of the workspaces stream alone keeps the closed rows",
			first: &frontendv1.RosterPush{
				Workspaces: workspacesStream([]*frontendv1.RosterClosedRow{row("ws-o", thinking)}, nil),
				Closed:     &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{row("ws-c", done)}},
			},
			next: &frontendv1.RosterPush{Workspaces: workspacesStream([]*frontendv1.RosterClosedRow{row("ws-o", done)}, nil)},
			want: map[ids.WorkspaceID]string{"ws-o": "done", "ws-c": "done"},
		},
		{
			name: "a push of the closed stream alone keeps the open rows",
			first: &frontendv1.RosterPush{
				Workspaces: workspacesStream([]*frontendv1.RosterClosedRow{row("ws-o", thinking)}, nil),
				Closed:     &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{row("ws-c", done)}},
			},
			next: &frontendv1.RosterPush{Closed: &frontendv1.RosterClosedRows{}},
			want: map[ids.WorkspaceID]string{"ws-o": "thinking"},
		},
		{
			name: "a push of the other streams alone changes nothing",
			first: &frontendv1.RosterPush{
				Workspaces: workspacesStream([]*frontendv1.RosterClosedRow{row("ws-o", thinking)}, nil),
			},
			next: &frontendv1.RosterPush{MergeQueue: &frontendv1.RosterMergeQueueSection{}, RecentlyMerged: &frontendv1.RosterMergedSection{}},
			want: map[ids.WorkspaceID]string{"ws-o": "thinking"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			s := f.run(t, ctx)
			s.Frames <- streamsFrame(tc.first)
			got := f.awaitSunk(t)

			// Act
			if tc.next != nil {
				s.Frames <- streamsFrame(tc.next)
				got = f.awaitSunk(t)
			}

			// Assert
			if !maps.Equal(got, tc.want) {
				t.Fatalf("mirrored = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestARowFollowsTheIncumbentThenIsItsOwnOnceAdopted(t *testing.T) {
	// Arrange: the incumbent's row moves while the workspace waits.
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)
	s.Frames <- rosterFrame(row("ws-a", done))
	first := f.awaitSunk(t)
	s.Frames <- rosterFrame(row("ws-a", thinking))
	followed := f.awaitSunk(t)

	// Act: the successor adopts it.
	f.adopt("ws-a")
	f.m.OwnershipChanged()
	adopted := f.awaitSunk(t)

	// Assert
	if first["ws-a"] != "done" || followed["ws-a"] != "thinking" {
		t.Fatalf("mirrored %v then %v, want done then thinking", first, followed)
	}
	if adopted != nil {
		t.Fatalf("after adoption mirrored = %v, want nothing: the row is this daemon's own", adopted)
	}
}

func TestTheIncumbentsPlannedEndingWithdrawsTheMirror(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)
	s.Frames <- rosterFrame(row("ws-a", thinking))
	f.awaitSunk(t)

	// Act
	s.Frames <- endingFrame()

	// Assert
	if got := f.awaitSunk(t); got != nil {
		t.Fatalf("after the ending mirrored = %v, want nothing", got)
	}
	f.awaitEnded(t)
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: Op,
		Message: "the incumbent ended its roster stream; every row is this daemon's own"}); !ok {
		t.Fatalf("no INFO record of the ending: %v", f.log.Records())
	}
	if errs := dlog.FilterRecords(f.log.Records(), dlog.RecordQuery{Level: "error"}); len(errs) != 0 {
		t.Fatalf("errors = %v, want none for a planned ending", errs)
	}
	select {
	case <-s.Closed:
	default:
		t.Fatal("the stream was left open")
	}
}

func TestAFailedSubscriptionWithdrawsTheMirrorAtError(t *testing.T) {
	cases := []struct {
		name    string
		dialErr error
		fail    func(*rostermirrortest.Stream)
		message string
	}{
		{
			name:    "the dial fails",
			dialErr: errors.New("connection refused"),
			message: "the incumbent's roster stream could not be opened; every row is this daemon's own",
		},
		{
			name:    "the stream fails mid-subscription",
			fail:    func(s *rostermirrortest.Stream) { s.End(errors.New("stream reset")) },
			message: "the incumbent's roster stream failed; every row is this daemon's own",
		},
		{
			name:    "the stream closes without the planned ending",
			fail:    func(s *rostermirrortest.Stream) { s.End(nil) },
			message: "the incumbent's roster stream failed; every row is this daemon's own",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.dialErr = tc.dialErr
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			// Act
			if tc.dialErr != nil {
				go func() {
					defer close(f.ran)
					f.m.Run(ctx)
				}()
			} else {
				s := f.run(t, ctx)
				s.Frames <- rosterFrame(row("ws-a", thinking))
				f.awaitSunk(t)
				tc.fail(s)
			}

			// Assert
			if got := f.awaitSunk(t); got != nil {
				t.Fatalf("after the failure mirrored = %v, want nothing", got)
			}
			f.awaitEnded(t)
			rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: Op, Message: tc.message})
			if !ok {
				t.Fatalf("no ERROR %q: %v", tc.message, f.log.Records())
			}
			if rec.Context["incumbent"] != "127.0.0.1:7777" || rec.Context["cause"] == nil {
				t.Fatalf("error context = %v, want the incumbent and the cause", rec.Context)
			}
		})
	}
}

func TestAnIncumbentThatIsGoneEndsTheMirrorAtInfo(t *testing.T) {
	tests := []struct {
		name string
		cut  error
	}{
		{name: "its connection reset", cut: connect.NewError(connect.CodeUnavailable, errors.New("read: connection reset by peer"))},
		{name: "its stream cut partway through a frame",
			cut: connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("protocol error: incomplete envelope: %w", io.ErrUnexpectedEOF))},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			s := f.run(t, ctx)
			s.Frames <- rosterFrame(row("ws-a", thinking))
			f.awaitSunk(t)

			// Act: the incumbent's link breaks as it dies.
			s.End(tt.cut)

			// Assert
			if got := f.awaitSunk(t); got != nil {
				t.Fatalf("after the incumbent went, mirrored = %v, want nothing", got)
			}
			f.awaitEnded(t)
			rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: Op,
				Message: "the incumbent's roster stream ended with the incumbent gone; every row is this daemon's own"})
			if !ok || rec.Context["incumbent"] != "127.0.0.1:7777" || rec.Context["cause"] == nil {
				t.Fatalf("records = %v, want the incumbent's departure at INFO with the incumbent and the cause", f.log.Records())
			}
			if _, bad := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: Op}); bad {
				t.Fatalf("records = %v, want no ERROR for an incumbent that is gone", f.log.Records())
			}
		})
	}
}

func TestStopEndsTheSubscriptionAndWithdrawsTheMirror(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)
	s.Frames <- rosterFrame(row("ws-a", thinking))
	f.awaitSunk(t)

	// Act
	f.m.Stop("the incumbent exited")

	// Assert
	if got := f.awaitSunk(t); got != nil {
		t.Fatalf("after Stop mirrored = %v, want nothing", got)
	}
	f.awaitEnded(t)
	if errs := dlog.FilterRecords(f.log.Records(), dlog.RecordQuery{Level: "error"}); len(errs) != 0 {
		t.Fatalf("errors = %v, want none: a stop is the handover ending", errs)
	}
	rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: Op,
		Message: "the mirror stopped; every row is this daemon's own"})
	if !ok || rec.Context["stopped_because"] != "the incumbent exited" {
		t.Fatalf("stop record = %v (%v), want one naming why", rec, ok)
	}
}

func TestAMirrorStoppedBeforeItRunsDialsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.m.Stop("the handover was abandoned")
	f.awaitSunk(t)

	// Act
	f.m.Run(context.Background())

	// Assert
	f.mu.Lock()
	dials := f.dials
	f.mu.Unlock()
	if dials != 0 {
		t.Fatalf("dials = %d, want none", dials)
	}
}

func TestAPushAfterTheEndMirrorsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.m.Stop("the incumbent exited")
	f.awaitSunk(t)

	// Act
	f.m.take(rosterFrame(row("ws-a", thinking)).GetStreams(), dlog.Context{})

	// Assert
	select {
	case got := <-f.sunk:
		t.Fatalf("mirrored %v after the end, want nothing published", got)
	default:
	}
}

func TestARowWhoseStatusThisBuildCannotReadKeepsItsOwnState(t *testing.T) {
	// Arrange: an incumbent whose row sets no arm this build decodes, as an
	// arm only another build names decodes.
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)

	// Act: two pushes, so the record is shown to be written once.
	s.Frames <- rosterFrame(row("ws-a", nil), row("ws-b", thinking))
	first := f.awaitSunk(t)
	s.Frames <- rosterFrame(row("ws-a", nil), row("ws-b", done))
	second := f.awaitSunk(t)

	// Assert
	want := map[ids.WorkspaceID]string{"ws-b": "thinking"}
	if !maps.Equal(first, want) || second["ws-b"] != "done" || len(second) != 1 {
		t.Fatalf("mirrored %v then %v, want ws-a left out of both", first, second)
	}
	errs := dlog.FilterRecords(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: Op,
		Message: "the incumbent's row has a status this build cannot read; the row keeps this daemon's own status"})
	if len(errs) != 1 {
		t.Fatalf("undecodable-row errors = %v, want exactly one", errs)
	}
	if errs[0].Context["workspace_id"] != "ws-a" || errs[0].Context["incumbent"] != "127.0.0.1:7777" {
		t.Fatalf("error context = %v, want the workspace and the incumbent", errs[0].Context)
	}
}

func TestStatusOfNamesTheArmOrWhyItCannotBeMirrored(t *testing.T) {
	cases := []struct {
		name        string
		status      func(*frontendv1.RosterRow)
		wantArm     string
		wantProblem bool
	}{
		{"a placed arm", thinking, "thinking", false},
		{"no arm set", nil, "", true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := row("ws", tc.status).GetRow()

			// Act
			arm, problem := statusOf(r)

			// Assert
			if arm != tc.wantArm || (problem != "") != tc.wantProblem {
				t.Fatalf("statusOf = %q, %q; want %q, problem %v", arm, problem, tc.wantArm, tc.wantProblem)
			}
		})
	}
}

// firstRosterAfter is the message of the mark of the first roster published
// after the mirror ended.
const firstRosterAfter = "the first roster this daemon published after the incumbent's roster stream ended"

func TestTheFirstRosterAfterTheMirrorEndsIsMarked(t *testing.T) {
	cases := []struct {
		name  string
		end   func(f *fixture, s *rostermirrortest.Stream)
		ended string
	}{
		{name: "the incumbent's planned ending", end: func(_ *fixture, s *rostermirrortest.Stream) { s.Frames <- endingFrame() }, ended: "ending"},
		{name: "a stop", end: func(f *fixture, _ *rostermirrortest.Stream) { f.m.Stop("the incumbent exited") }, ended: "stopped"},
		{name: "a failed stream", end: func(_ *fixture, s *rostermirrortest.Stream) { s.End(errors.New("stream reset")) }, ended: "failed"},
		{name: "an incumbent that is gone", end: func(_ *fixture, s *rostermirrortest.Stream) {
			s.End(connect.NewError(connect.CodeUnavailable, errors.New("read: connection reset by peer")))
		}, ended: "incumbent_gone"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			s := f.run(t, ctx)
			s.Frames <- rosterFrame(row("ws-a", thinking))
			f.awaitSunk(t)
			tc.end(f, s)
			f.awaitSunk(t)
			f.awaitEnded(t)

			// Act
			f.published(7)

			// Assert
			rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: Op, Message: firstRosterAfter})
			if !ok {
				t.Fatalf("no INFO mark of the first roster after the mirror: %v", f.log.Records())
			}
			if rec.Context["revision"] != uint64(7) || rec.Context["ended"] != tc.ended || rec.Context["incumbent"] != "127.0.0.1:7777" {
				t.Fatalf("mark context = %v, want revision 7, ended %q and the incumbent", rec.Context, tc.ended)
			}
		})
	}
}

func TestNoRosterIsMarkedWhileTheMirrorRuns(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)

	// Act
	s.Frames <- rosterFrame(row("ws-a", thinking))
	f.awaitSunk(t)

	// Assert
	if n := f.noticesAsked(); n != 0 {
		t.Fatalf("notices asked = %d, want none while the mirror runs", n)
	}
}

func TestOnlyTheMirrorsFirstEndAsksForTheMark(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)
	s.Frames <- rosterFrame(row("ws-a", thinking))
	f.awaitSunk(t)

	// Act: a stop, then the subscription's own end it causes.
	f.m.Stop("the incumbent exited")
	f.awaitSunk(t)
	f.awaitEnded(t)

	// Assert
	if n := f.noticesAsked(); n != 1 {
		t.Fatalf("notices asked = %d, want exactly one for the first end", n)
	}
}

func TestTheMarkIsAskedForBeforeTheWithdrawalPublishes(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := f.run(t, ctx)
	s.Frames <- rosterFrame(row("ws-a", thinking))
	f.awaitSunk(t)

	// Act
	s.Frames <- endingFrame()
	f.awaitSunk(t)
	f.awaitEnded(t)

	// Assert: the withdrawal's own publication is the one marked.
	f.mu.Lock()
	events := append([]string(nil), f.events...)
	f.mu.Unlock()
	want := []string{"sink", "notice", "sink"}
	if fmt.Sprint(events) != fmt.Sprint(want) {
		t.Fatalf("events = %v, want %v", events, want)
	}
}
