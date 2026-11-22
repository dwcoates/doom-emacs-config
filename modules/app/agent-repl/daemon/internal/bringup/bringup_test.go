package bringup

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

const testOp = "daemon.test.bring_up"

// fakeSessions answers a session record per workspace, or an arranged error.
type fakeSessions struct {
	records map[ids.WorkspaceID]wsm.Session
	errs    map[ids.WorkspaceID]error
}

func (f *fakeSessions) Session(_ context.Context, ws ids.WorkspaceID) (wsm.Session, bool, error) {
	if err := f.errs[ws]; err != nil {
		return wsm.Session{}, false, err
	}
	s, ok := f.records[ws]
	return s, ok, nil
}

// fixture records every start and every marker edge, in order.
type fixture struct {
	mu       sync.Mutex
	starts   []ids.WorkspaceID
	marker   []string
	startErr map[ids.WorkspaceID]error
	sessions *fakeSessions
	log      *dlog.TestLogger
	clock    *clocktest.Scripted
	// startTakes is how far each start moves the clock.
	startTakes time.Duration
	restarts   *restarttimingtest.Recorder
}

var epoch = time.Date(2026, 10, 19, 12, 0, 0, 0, time.UTC)

func newFixture() *fixture {
	return &fixture{
		startErr: map[ids.WorkspaceID]error{},
		sessions: &fakeSessions{records: map[ids.WorkspaceID]wsm.Session{}, errs: map[ids.WorkspaceID]error{}},
		log:      dlog.NewTestLogger(),
		clock:    clocktest.New(epoch),
		restarts: &restarttimingtest.Recorder{},
	}
}

func (f *fixture) deps() Deps {
	return Deps{
		DB: f.sessions,
		StartSession: func(_ context.Context, ws ids.WorkspaceID) error {
			f.mu.Lock()
			defer f.mu.Unlock()
			f.starts = append(f.starts, ws)
			f.marker = append(f.marker, "start:"+string(ws))
			f.clock.Set(f.clock.Now().Add(f.startTakes))
			return f.startErr[ws]
		},
		BringingUp: func(ws ids.WorkspaceID, up bool) {
			f.mu.Lock()
			defer f.mu.Unlock()
			f.marker = append(f.marker, fmt.Sprintf("%s:%v", ws, up))
		},
		Log:       f.log,
		Operation: testOp,
		Via:       restarttiming.StartBoot,
		Restarts:  f.restarts,
		Now:       f.clock.Now,
	}
}

func (f *fixture) markerFor(ws ids.WorkspaceID) []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, m := range f.marker {
		if strings.HasPrefix(m, string(ws)+":") || m == "start:"+string(ws) {
			out = append(out, m)
		}
	}
	return out
}

func (f *fixture) recordsAt(level string) []dlog.Record {
	var out []dlog.Record
	for _, r := range f.log.Records() {
		if r.Operation == testOp && r.Level == level {
			out = append(out, r)
		}
	}
	return out
}

func workspaces(names ...string) []wsm.Workspace {
	out := make([]wsm.Workspace, len(names))
	for i, n := range names {
		out[i] = wsm.Workspace{ID: ids.WorkspaceID(n), Dir: "/tmp/" + n}
	}
	return out
}

func TestRunClassifiesEachWorkspacesOutcome(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		want    func(r Report) []ids.WorkspaceID
	}{
		{
			name:    "an ordinary start is started",
			arrange: func(*fixture) {},
			want:    func(r Report) []ids.WorkspaceID { return r.Started },
		},
		{
			name: "a hibernated workspace's start is a wake",
			arrange: func(f *fixture) {
				f.sessions.records["a"] = wsm.Session{Workspace: "a", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
			},
			want: func(r Report) []ids.WorkspaceID { return r.Woken },
		},
		{
			name: "a start this daemon stood down is stood down",
			arrange: func(f *fixture) {
				f.startErr["a"] = fmt.Errorf("start: %w", shimclient.ErrStandDownOrdered)
			},
			want: func(r Report) []ids.WorkspaceID { return r.StoodDown },
		},
		{
			name: "a start lost to a handover is stood down",
			arrange: func(f *fixture) {
				f.startErr["a"] = fmt.Errorf("%w: already started", ErrNotServed)
			},
			want: func(r Report) []ids.WorkspaceID { return r.StoodDown },
		},
		{
			name: "a start that fails is failed",
			arrange: func(f *fixture) {
				f.startErr["a"] = errors.New("arranged: the shim never answered")
			},
			want: func(r Report) []ids.WorkspaceID { return r.Failed },
		},
		{
			name: "an unreadable session record is failed",
			arrange: func(f *fixture) {
				f.sessions.errs["a"] = errors.New("arranged: the record could not be read")
			},
			want: func(r Report) []ids.WorkspaceID { return r.Failed },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			tc.arrange(f)

			// Act
			report := Run(context.Background(), f.deps(), workspaces("a"))

			// Assert
			if got := tc.want(report); !slices.Equal(got, []ids.WorkspaceID{"a"}) {
				t.Fatalf("report = %+v, want the workspace in the expected outcome", report)
			}
		})
	}
}

func TestRunDoesNotStartAWorkspaceWhoseRecordCannotBeRead(t *testing.T) {
	// Arrange
	f := newFixture()
	f.sessions.errs["a"] = errors.New("arranged: the record could not be read")

	// Act
	Run(context.Background(), f.deps(), workspaces("a"))

	// Assert
	if len(f.starts) != 0 {
		t.Fatalf("starts = %v, want none for an unreadable record", f.starts)
	}
	errs := f.recordsAt(dlog.LevelError)
	if len(errs) != 1 || errs[0].Context[dlog.KeyWorkspaceID] != "a" || errs[0].Context["error"] == nil {
		t.Fatalf("error records = %+v, want one naming the workspace and its cause", errs)
	}
}

func TestRunRecordsAFailedStartAtError(t *testing.T) {
	// Arrange
	f := newFixture()
	f.startErr["a"] = errors.New("arranged: the shim never answered")

	// Act
	Run(context.Background(), f.deps(), workspaces("a"))

	// Assert
	errs := f.recordsAt(dlog.LevelError)
	if len(errs) != 1 || errs[0].Context["error"] != "arranged: the shim never answered" {
		t.Fatalf("error records = %+v, want one carrying the start's cause", errs)
	}
}

func TestRunDoesNotRecordAStandDownAsAnError(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{name: "a stand-down this daemon ordered", err: shimclient.ErrStandDownOrdered},
		{name: "a spawn refused because the daemon is standing down", err: shimclient.ErrStandingDown},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			f.startErr["a"] = fmt.Errorf("start: %w", tt.err)

			// Act
			Run(context.Background(), f.deps(), workspaces("a"))

			// Assert
			if errs := f.recordsAt(dlog.LevelError); len(errs) != 0 {
				t.Fatalf("error records = %+v, want none", errs)
			}
		})
	}
}

func TestRunBeginsNothingOnceTheDaemonIsLeaving(t *testing.T) {
	// Arrange
	f := newFixture()
	leaving, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	report := Run(leaving, f.deps(), workspaces("a"))

	// Assert
	if len(f.starts) != 0 {
		t.Fatalf("starts = %v, want none once the daemon is leaving", f.starts)
	}
	if len(report.Started)+len(report.Woken)+len(report.StoodDown)+len(report.Failed) != 0 {
		t.Fatalf("report = %+v, want a start never begun counted nowhere", report)
	}
}

func TestRunLowersTheMarkerAfterTheStart(t *testing.T) {
	// Arrange
	f := newFixture()

	// Act
	Run(context.Background(), f.deps(), workspaces("a"))

	// Assert
	want := []string{"start:a", "a:false"}
	if got := f.markerFor("a"); !slices.Equal(got, want) {
		t.Fatalf("marker edges = %v, want %v", got, want)
	}
}

func TestRunLowersTheMarkerOfAStartNeverBegun(t *testing.T) {
	// Arrange
	f := newFixture()
	leaving, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	Run(leaving, f.deps(), workspaces("a"))

	// Assert
	want := []string{"a:false"}
	if got := f.markerFor("a"); !slices.Equal(got, want) {
		t.Fatalf("marker edges = %v, want the marker lowered", got)
	}
}

func TestRunReportsWorkspacesInPendingOrder(t *testing.T) {
	// Arrange
	f := newFixture()
	pending := workspaces("c", "a", "b")

	// Act
	report := Run(context.Background(), f.deps(), pending)

	// Assert
	if want := []ids.WorkspaceID{"c", "a", "b"}; !slices.Equal(report.Started, want) {
		t.Fatalf("Started = %v, want the pending order %v", report.Started, want)
	}
}

func TestRunSummarizesAtInfo(t *testing.T) {
	// Arrange
	f := newFixture()
	f.startErr["b"] = errors.New("arranged: the shim never answered")

	// Act
	Run(context.Background(), f.deps(), workspaces("a", "b"))

	// Assert
	infos := f.recordsAt(dlog.LevelInfo)
	if len(infos) != 1 {
		t.Fatalf("info records = %+v, want exactly the one summary", infos)
	}
	if infos[0].Context["pending"] != 2 || infos[0].Context["started"] != 1 || infos[0].Context["failed"] != 1 {
		t.Fatalf("summary context = %v, want pending 2, started 1, failed 1", infos[0].Context)
	}
}

func TestDepsValidateRefusesAMissingCollaborator(t *testing.T) {
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"no session reader", func(d *Deps) { d.DB = nil }},
		{"no starter", func(d *Deps) { d.StartSession = nil }},
		{"no marker", func(d *Deps) { d.BringingUp = nil }},
		{"no logger", func(d *Deps) { d.Log = nil }},
		{"no operation", func(d *Deps) { d.Operation = "" }},
		{"no bring-up kind", func(d *Deps) { d.Via = "" }},
		{"no restart recorder", func(d *Deps) { d.Restarts = nil }},
		{"no clock", func(d *Deps) { d.Now = nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := newFixture().deps()
			tc.strip(&deps)

			// Act
			err := deps.Validate()

			// Assert
			if err == nil {
				t.Fatalf("Validate() = nil, want a refusal")
			}
		})
	}
}

func TestDepsValidateAcceptsACompleteDeps(t *testing.T) {
	// Arrange
	deps := newFixture().deps()

	// Act
	err := deps.Validate()

	// Assert
	if err != nil {
		t.Fatalf("Validate() = %v, want nil", err)
	}
}

func TestRunTellsDoneHowEachWorkspacesStartEnded(t *testing.T) {
	// Arrange
	f := newFixture()
	f.startErr["bad"] = errors.New("spawn refused")
	f.sessions.errs["unread"] = errors.New("disk gone")
	deps := f.deps()
	var mu sync.Mutex
	got := map[ids.WorkspaceID]error{}
	deps.Done = func(ws ids.WorkspaceID, err error) {
		mu.Lock()
		defer mu.Unlock()
		got[ws] = err
	}

	// Act
	Run(context.Background(), deps, []wsm.Workspace{{ID: "good"}, {ID: "bad"}, {ID: "unread"}})

	// Assert
	if len(got) != 3 {
		t.Fatalf("Done told %d workspaces, want 3: %v", len(got), got)
	}
	if got["good"] != nil {
		t.Fatalf("good ended %v, want nil", got["good"])
	}
	if got["bad"] == nil || !strings.Contains(got["bad"].Error(), "spawn refused") {
		t.Fatalf("bad ended %v, want the start's error", got["bad"])
	}
	if got["unread"] == nil || !strings.Contains(got["unread"].Error(), "disk gone") {
		t.Fatalf("unread ended %v, want the record's read error", got["unread"])
	}
}

func TestRunTellsDoneOfAStartNeverBegun(t *testing.T) {
	// Arrange
	f := newFixture()
	deps := f.deps()
	var told error
	deps.Done = func(_ ids.WorkspaceID, err error) { told = err }
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	Run(ctx, deps, []wsm.Workspace{{ID: "w1"}})

	// Assert
	if told == nil || !errors.Is(told, context.Canceled) {
		t.Fatalf("Done = %v, want the daemon leaving", told)
	}
}

func TestRunTimesEachStartedWorkspace(t *testing.T) {
	cases := []struct {
		name        string
		arrange     func(f *fixture)
		wantPhase   string
		wantOutcome restarttiming.Outcome
	}{
		{"a start is timed as a start", func(*fixture) {}, "start", restarttiming.OK},
		{
			"a wake is timed as a wake",
			func(f *fixture) {
				f.sessions.records["a"] = wsm.Session{Workspace: "a", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
			},
			"wake", restarttiming.OK,
		},
		{
			"a failed start is timed as failed",
			func(f *fixture) { f.startErr["a"] = errors.New("arranged: the shim never answered") },
			"start", restarttiming.Failed,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			f.startTakes = 7 * time.Second
			tc.arrange(f)

			// Act
			Run(context.Background(), f.deps(), workspaces("a"))

			// Assert
			got := f.restarts.Of(restarttiming.WorkspaceStart)
			if len(got) != 1 {
				t.Fatalf("observations = %+v, want one workspace start", got)
			}
			o := got[0]
			if o.Workspace() != "a" || o.Phase() != tc.wantPhase || o.Cause() != "boot" || o.Outcome() != tc.wantOutcome || o.Seconds() != 7 {
				t.Fatalf("observation = %+v, want a %s of a via boot, %s, 7s", o, tc.wantPhase, tc.wantOutcome)
			}
		})
	}
}

func TestRunDoesNotTimeAStartItNeverRanOrCutShort(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture) context.Context
	}{
		{
			"a start this daemon stood down",
			func(f *fixture) context.Context {
				f.startErr["a"] = fmt.Errorf("start: %w", shimclient.ErrStandDownOrdered)
				return context.Background()
			},
		},
		{
			"a start never begun because the daemon is leaving",
			func(*fixture) context.Context {
				ctx, cancel := context.WithCancel(context.Background())
				cancel()
				return ctx
			},
		},
		{
			"a workspace whose session record could not be read",
			func(f *fixture) context.Context {
				f.sessions.errs["a"] = errors.New("arranged: database is locked")
				return context.Background()
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			ctx := tc.arrange(f)

			// Act
			Run(ctx, f.deps(), workspaces("a"))

			// Assert
			if got := f.restarts.Observations(); len(got) != 0 {
				t.Fatalf("observations = %+v, want none", got)
			}
		})
	}
}

func TestRunStartsNoMoreThanItsWidthAtOnce(t *testing.T) {
	// Arrange: five workspaces, two starts at once; each start holds until
	// the test releases it.
	f := newFixture()
	deps := f.deps()
	deps.Width = 2
	entered := make(chan ids.WorkspaceID, 5)
	release := make(chan struct{})
	var mu sync.Mutex
	running, most := 0, 0
	deps.StartSession = func(_ context.Context, ws ids.WorkspaceID) error {
		mu.Lock()
		running++
		most = max(most, running)
		mu.Unlock()
		entered <- ws
		<-release
		mu.Lock()
		running--
		mu.Unlock()
		return nil
	}
	done := make(chan Report, 1)

	// Act
	go func() { done <- Run(context.Background(), deps, workspaces("a", "b", "c", "d", "e")) }()
	first := []ids.WorkspaceID{<-entered, <-entered}
	select {
	case ws := <-entered:
		t.Fatalf("a third start (%s) began while two held the width", ws)
	default:
	}
	close(release)
	report := <-done

	// Assert
	if most != 2 || len(report.Started) != 5 {
		t.Fatalf("at most %d starts ran at once and %d started (first %v), want 2 and 5", most, len(report.Started), first)
	}
}

func TestDepsWidth(t *testing.T) {
	cases := []struct {
		name    string
		width   int
		pending int
		want    int
	}{
		{name: "a set width", width: 3, pending: 10, want: 3},
		{name: "no more than the workspaces", width: 8, pending: 2, want: 2},
		{name: "unset is the processor count", width: 0, pending: 1000, want: runtime.NumCPU()},
		{name: "nothing pending starts no worker", width: 4, pending: 0, want: 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Deps{Width: tc.width}.width(tc.pending)

			// Assert
			if got != tc.want {
				t.Fatalf("width(%d) = %d, want %d", tc.pending, got, tc.want)
			}
		})
	}
}

func TestIsStandDownNamesEveryStopThisDaemonOrdered(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "a stand-down the supervisor ordered", err: fmt.Errorf("start: %w", shimclient.ErrStandDownOrdered), want: true},
		{name: "a spawn refused while standing down", err: fmt.Errorf("start: %w", shimclient.ErrStandingDown), want: true},
		{name: "a workspace no longer served here", err: fmt.Errorf("start: %w", ErrNotServed), want: true},
		{name: "a shim that would not spawn", err: errors.New("the shim would not spawn"), want: false},
		{name: "no error", err: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act, Assert
			if got := IsStandDown(tt.err); got != tt.want {
				t.Fatalf("IsStandDown(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}

func TestTheStandDownClassificationHasOneHome(t *testing.T) {
	// Assert: the three stand-down errors are tested together only inside
	// IsStandDown, so no start site in this package classifies its own way.
	if got := sourcescan.Sites(t, "errors.Is(err, ErrNotServed)"); !slices.Equal(got, []string{"bringup.go"}) {
		t.Fatalf("ErrNotServed classified at %v, want only IsStandDown in bringup.go", got)
	}
	if got := sourcescan.Count(t, "IsStandDown(err)"); got < 1 {
		t.Fatalf("IsStandDown(err) called %d times in bringup, want the bring-up's own start to use it", got)
	}
}

func TestRunRecordsHowManyShimsARestartStarted(t *testing.T) {
	cases := []struct {
		name string
		via  restarttiming.StartVia
		want []string
	}{
		{"a boot is cold", restarttiming.StartBoot, []string{"shim cold 2"}},
		{"a takeover is a successor", restarttiming.StartTakeover, []string{"shim successor 2"}},
		{"an editor's opening is no restart", restarttiming.StartVia("editor"), nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a started workspace, a woken one and a failed one.
			f := newFixture()
			f.sessions.records["b"] = wsm.Session{Workspace: "b", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
			f.startErr["c"] = errors.New("arranged: the shim never answered")
			deps := f.deps()
			deps.Via = tc.via

			// Act
			Run(context.Background(), deps, workspaces("a", "b", "c"))

			// Assert: started and woken count, a failed start does not.
			if got := f.restarts.Affecteds(); !slices.Equal(got, tc.want) {
				t.Fatalf("affected = %v, want %v", got, tc.want)
			}
		})
	}
}
