package storagesweep

import (
	"errors"
	"path/filepath"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// discardCall is one DiscardWorkspace the sweep made.
type discardCall struct {
	dir, id   string
	judgeOnly bool
}

// fakeLogs is the scripted log surfaces: what each workspace's discard
// answers, and every call made.
type fakeLogs struct {
	calls   []discardCall
	reports map[string]dlog.DiscardReport
	errs    map[string]error
}

func (f *fakeLogs) DiscardWorkspace(dir, id string, judgeOnly bool) (dlog.DiscardReport, error) {
	f.calls = append(f.calls, discardCall{dir: dir, id: id, judgeOnly: judgeOnly})
	return f.reports[id], f.errs[id]
}

// logged registers a workspace last touched ago before now, with one log file
// of its own in the logs directory, and scripts what its discard answers.
func (w *world) logged(id string, ago time.Duration, report dlog.DiscardReport) wsm.Workspace {
	w.t.Helper()
	ws := wsm.Workspace{ID: ids.WorkspaceID(id), Name: "ws-" + id, Dir: "/ws/" + id, CreatedAt: now.Add(-ago)}
	w.reg.workspaces = append(w.reg.workspaces, ws)
	w.write(filepath.Join(w.logsDir(), "agent-repl-"+id+"-daemon-1.log"), "x", now)
	if w.logs.reports == nil {
		w.logs.reports = map[string]dlog.DiscardReport{}
	}
	w.logs.reports[id] = report
	return ws
}

func removedLog(path string, bytes int64) dlog.DiscardReport {
	return dlog.DiscardReport{Removed: []dlog.DiscardedLog{{Path: path, Bytes: bytes}}, Links: []string{"/ws/link"}}
}

func TestTheLogsRuleJudgesAWorkspaceByItsLastTouch(t *testing.T) {
	tests := []struct {
		name string
		ago  time.Duration
		// wantDiscarded is whether the workspace's logs are discarded.
		wantDiscarded bool
		wantKept      KeepReason
	}{
		{"a workspace touched inside the window keeps its logs", 4 * day, false, KeepRecentlyTouched},
		{"a workspace touched exactly at the window keeps its logs", 5 * day, false, KeepRecentlyTouched},
		{"a workspace untouched past the window loses its logs", 6 * day, true, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w := newWorld(t)
			w.logged("00000000000000aa", tt.ago, removedLog("/logs/a.log", 7))

			// Act.
			report := w.sweep(false)

			// Assert.
			if discarded := len(w.logs.calls) == 1; discarded != tt.wantDiscarded {
				t.Fatalf("discard calls %+v, want discarded %v", w.logs.calls, tt.wantDiscarded)
			}
			if tt.wantDiscarded {
				want := Removal{Path: "/logs/a.log", Reason: ReasonUntouchedLogs, Bytes: 7}
				if len(report.Removed) != 1 || report.Removed[0] != want {
					t.Fatalf("removed %+v, want %+v", report.Removed, want)
				}
				return
			}
			if report.Kept[tt.wantKept] != 1 || len(report.Removed) != 0 {
				t.Fatalf("report %+v, want one kept as %s", report, tt.wantKept)
			}
		})
	}
}

func TestTheLogsRuleCountsFromTheNewestStamp(t *testing.T) {
	// Arrange: created long ago, selected yesterday.
	w := newWorld(t)
	ws := w.logged("00000000000000aa", 40*day, removedLog("/logs/a.log", 7))
	selected := now.Add(-day)
	w.reg.workspaces[0] = wsm.Workspace{ID: ws.ID, Dir: ws.Dir, CreatedAt: ws.CreatedAt, LastSelectedAt: &selected}

	// Act.
	report := w.sweep(false)

	// Assert.
	if len(w.logs.calls) != 0 || report.Kept[KeepRecentlyTouched] != 1 {
		t.Fatalf("calls %+v report %+v, want the selected workspace's logs kept", w.logs.calls, report)
	}
}

func TestTheLogsRuleAsksOnlyAboutWorkspacesWithLogFiles(t *testing.T) {
	// Arrange: an untouched workspace with no file in the logs directory, and
	// a file of a workspace the registry does not know.
	w := newWorld(t)
	w.reg.workspaces = append(w.reg.workspaces, wsm.Workspace{ID: "00000000000000bb", Dir: "/ws/b", CreatedAt: now.Add(-40 * day)})
	w.write(filepath.Join(w.logsDir(), "agent-repl-00000000000000cc-daemon-1.log"), "x", now)
	w.write(filepath.Join(w.logsDir(), "daemon.run.log"), "x", now)

	// Act.
	report := w.sweep(false)

	// Assert.
	if len(w.logs.calls) != 0 || len(report.Removed) != 0 || report.Failures != 0 {
		t.Fatalf("calls %+v report %+v, want nothing asked and nothing removed", w.logs.calls, report)
	}
}

func TestTheLogsRuleReadsTheIdOfAGeneration(t *testing.T) {
	// Arrange: the workspace's only file is a rolled generation.
	w := newWorld(t)
	w.reg.workspaces = append(w.reg.workspaces, wsm.Workspace{ID: "00000000000000aa", Dir: "/ws/a", CreatedAt: now.Add(-40 * day)})
	w.write(filepath.Join(w.logsDir(), "agent-repl-00000000000000aa-webapp-9Zx.log.2"), "x", now)

	// Act.
	w.sweep(false)

	// Assert.
	if len(w.logs.calls) != 1 || w.logs.calls[0].id != "00000000000000aa" || w.logs.calls[0].dir != "/ws/a" {
		t.Fatalf("calls %+v, want the generation's workspace asked about", w.logs.calls)
	}
}

func TestTheLogsRuleCountsHeldFilesAsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.logged("00000000000000aa", 40*day, dlog.DiscardReport{Held: []string{"/logs/a.log", "/logs/b.log"}})

	// Act.
	report := w.sweep(false)

	// Assert.
	if report.Kept[KeepHeldLogs] != 2 || len(report.Removed) != 0 || report.Failures != 0 {
		t.Fatalf("report %+v, want two files kept as held", report)
	}
}

func TestADryRunJudgesTheLogsAndReportsWhatWouldGo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.logged("00000000000000aa", 40*day, removedLog("/logs/a.log", 7))

	// Act.
	report := w.sweep(true)

	// Assert.
	if len(w.logs.calls) != 1 || !w.logs.calls[0].judgeOnly || len(report.Removed) != 1 {
		t.Fatalf("calls %+v report %+v, want one judgement reported", w.logs.calls, report)
	}
}

func TestEveryLogsDeletionIsRecorded(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	ws := w.logged("00000000000000aa", 40*day, removedLog("/logs/a.log", 7))

	// Act.
	w.sweep(false)

	// Assert.
	var found bool
	for _, r := range w.log.Records() {
		if r.Operation == opLogs && r.Level == dlog.LevelInfo && r.Context["workspace"] == string(ws.ID) &&
			r.Context["workspace_name"] == ws.Name && r.Context["workspace_dir"] == ws.Dir &&
			r.Context["files"] == 1 && r.Context["bytes"] == int64(7) && r.Context["links"] == 1 &&
			r.Context["last_touched_signal"] == "workspace_created" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records %+v, want an INFO naming the workspace, its files, bytes and links", w.log.Records())
	}
}

func TestALogsDiscardThatFailsIsAnErrorAndTheSweepGoesOn(t *testing.T) {
	// Arrange: the first workspace's discard fails after removing one file.
	w := newWorld(t)
	stuck := w.logged("00000000000000aa", 40*day, removedLog("/logs/a.log", 7))
	w.logged("00000000000000bb", 40*day, removedLog("/logs/b.log", 3))
	w.logs.errs = map[string]error{string(stuck.ID): errors.New("a link could not be removed")}

	// Act.
	report := w.sweep(false)

	// Assert.
	if report.Failures != 1 || len(report.Removed) != 2 {
		t.Fatalf("report %+v, want the failure counted and both removals reported", report)
	}
	errs := w.errors()
	if len(errs) != 1 || errs[0].Operation != opLogs || errs[0].Context["workspace"] != string(stuck.ID) || errs[0].Context["cause"] != "a link could not be removed" {
		t.Fatalf("errors %+v, want one naming the stuck workspace and its cause", errs)
	}
}

func TestAnUnreadableRegistryLeavesTheLogsAlone(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.logged("00000000000000aa", 40*day, removedLog("/logs/a.log", 7))
	w.reg.err = errors.New("the registry is locked")

	// Act.
	report, _ := w.sweeper().Sweep(t.Context(), false)

	// Assert.
	if len(w.logs.calls) != 0 || len(report.Removed) != 0 {
		t.Fatalf("calls %+v report %+v, want no logs discarded", w.logs.calls, report)
	}
	var logged bool
	for _, r := range w.errors() {
		logged = logged || r.Operation == opLogs
	}
	if !logged {
		t.Fatalf("errors %+v, want the logs rule's own ERROR", w.errors())
	}
}

func TestAMissingLogsDirectoryIsNothingToSweep(t *testing.T) {
	// Arrange: no logs directory at all.
	w := newWorld(t)
	w.reg.workspaces = append(w.reg.workspaces, wsm.Workspace{ID: "00000000000000aa", Dir: "/ws/a", CreatedAt: now.Add(-40 * day)})

	// Act.
	report := w.sweep(false)

	// Assert.
	if len(w.logs.calls) != 0 || report.Failures != 0 || len(w.errors()) != 0 {
		t.Fatalf("calls %+v report %+v errors %v, want a quiet sweep", w.logs.calls, report, w.errors())
	}
}
