package bringup

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
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
}

func newFixture() *fixture {
	return &fixture{
		startErr: map[ids.WorkspaceID]error{},
		sessions: &fakeSessions{records: map[ids.WorkspaceID]wsm.Session{}, errs: map[ids.WorkspaceID]error{}},
		log:      dlog.NewTestLogger(),
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
			return f.startErr[ws]
		},
		BringingUp: func(ws ids.WorkspaceID, up bool) {
			f.mu.Lock()
			defer f.mu.Unlock()
			f.marker = append(f.marker, fmt.Sprintf("%s:%v", ws, up))
		},
		Log:       f.log,
		Operation: testOp,
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
				f.sessions.records["a"] = wsm.Session{Workspace: "a", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalHibernated}}
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
