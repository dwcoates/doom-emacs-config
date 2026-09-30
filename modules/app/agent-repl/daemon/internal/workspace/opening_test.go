package workspace

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// openingPointer is the main-watch pointer the retired watcher in these
// scenarios was served.
var openingPointer = &conversationv1.HistoryPointer{Value: "ptr-main-9"}

// TestStartWatcherOpeningFollowsTheReplayRule covers the owner's rule at the
// place the fleet decides it: a watcher REPLAYS the first page only when the
// workspace is opened in this process or a transcript was selected, and every
// other watcher resumes from the pointers of the watcher it replaces.
func TestStartWatcherOpeningFollowsTheReplayRule(t *testing.T) {
	tests := []struct {
		name string
		// before brings the workspace to the state the case is about; the
		// retired watcher it leaves behind was served openingPointer.
		before func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID)
		// act starts the watcher under test.
		act         func(f *fleetFixture, ws ids.WorkspaceID) error
		wantOpening string
		wantMain    *conversationv1.HistoryPointer
	}{
		{
			name:        "a workspace's first open replays its first page",
			before:      func(*testing.T, *fleetFixture, ids.WorkspaceID) {},
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "workspace_opened",
		},
		{
			name:        "a restart resumes from the retired watcher's pointers",
			before:      startedThenStopped,
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "resumed",
			wantMain:    openingPointer,
		},
		{
			name:        "a transcript select replays the selected transcript's first page",
			before:      startedThenStopped,
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.StartRebound(context.Background(), ws) },
			wantOpening: "transcript_selected",
		},
		{
			name: "a fresh conversation forgets the previous book's pointers",
			before: func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				startedThenStopped(t, f, ws)
				delete(f.db.sessions, ws)
			},
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "workspace_opened",
		},
		{
			name: "a relaunch's install resumes from the watcher it retires",
			before: func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				t.Helper()
				if err := f.fleet.Start(context.Background(), ws); err != nil {
					t.Fatalf("Start: %v", err)
				}
				f.watcher.pointers = sessionwatcher.Pointers{Main: openingPointer}
			},
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				return f.fleet.Install(context.Background(), ws, &fakeClient{response: startedResponse("vendor-1"), pid: 5151})
			},
			wantOpening: "resumed",
			wantMain:    openingPointer,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			tt.before(t, f, ws.ID)
			before := len(f.openings)

			// Act.
			err := tt.act(f, ws.ID)

			// Assert.
			if err != nil {
				t.Fatalf("act: %v", err)
			}
			if len(f.openings) != before+1 {
				t.Fatalf("watchers started = %d, want exactly one", len(f.openings)-before)
			}
			got := f.openings[len(f.openings)-1]
			if got.String() != tt.wantOpening {
				t.Fatalf("opening = %q, want %q", got.String(), tt.wantOpening)
			}
			if main := got.From().Main; main.GetValue() != tt.wantMain.GetValue() {
				t.Fatalf("resumed main pointer = %q, want %q", main.GetValue(), tt.wantMain.GetValue())
			}
		})
	}
}

// TestStartWatcherLogsTheOpeningAtInfo covers the record that says, per
// bring-up, whether history was replayed: without it a replay is invisible in
// the logs, which is how every-turn replays went unnoticed.
func TestStartWatcherLogsTheOpeningAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	for _, rec := range f.log.logger.Records() {
		if rec.Level == "info" && rec.Operation == opBringUp && rec.Context["opening"] == "workspace_opened" && rec.Context["replays_history"] == true {
			return
		}
	}
	t.Fatalf("no info %s record stated the opening; records: %v", opBringUp, f.log.logger.Records())
}

// TestFailedWatcherStartKeepsTheSelectionOwed covers the error path of a
// select: a watcher that never started has not replayed the selected
// transcript, so the next watcher still owes it the first page.
func TestFailedWatcherStartKeepsTheSelectionOwed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	startedThenStopped(t, f, ws.ID)
	f.watchErr = errors.New("the watch fleet could not open")
	if err := f.fleet.StartRebound(context.Background(), ws.ID); err == nil {
		t.Fatal("StartRebound = nil error, want the watcher's failure surfaced")
	}
	f.watchErr = nil
	if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if got := f.openings[len(f.openings)-1].String(); got != "transcript_selected" {
		t.Fatalf("opening after a failed select = %q, want transcript_selected", got)
	}
}

// TestFirstPageRequestsHaveNamedSitesOnly is the structural half of the rule:
// it fails the moment any other path could request a first page without a
// pointer.
//
//   - The two replay openings each have ONE production caller, the fleet's
//     openingFor.
//   - A WatchAgentRequest is built in ONE place, the watcher's
//     openAgentStreamLocked, which states the watch's known pointer whenever
//     the watcher holds one.
//   - A StartTurnRequest is built in ONE place, the sender's startTurn (behind
//     both StartTurn and JoinRunningTurn), whose page is bounded to the turn's
//     own row.
//   - Nothing in production asks ReadHistory for its newest page.
func TestFirstPageRequestsHaveNamedSitesOnly(t *testing.T) {
	// Arrange.
	want := map[string][]string{
		"sessionwatcher.WorkspaceOpened":    {"internal/workspace/opening.go:openingFor"},
		"sessionwatcher.TranscriptSelected": {"internal/workspace/opening.go:openingFor"},
		"shimv1.WatchAgentRequest":          {"internal/sessionwatcher/watcher.go:openAgentStreamLocked"},
		"shimv1.StartTurnRequest":           {"internal/workspace/sender.go:startTurn"},
		"shimv1.ReadHistoryFirst":           nil,
		"shimv1.ReadHistoryRequest_First":   nil,
	}

	// Act.
	got := firstPageSites(t, daemonRoot(t), want)

	// Assert.
	for name, sites := range want {
		if strings.Join(got[name], ",") != strings.Join(sites, ",") {
			t.Errorf("%s is referenced from %v, want only %v", name, got[name], sites)
		}
	}
}

// startedThenStopped brings a workspace up, has its watcher served
// openingPointer, and stops it: the retired watcher a restart replaces.
func startedThenStopped(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
	t.Helper()
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.watcher.pointers = sessionwatcher.Pointers{Main: openingPointer}
	if err := f.fleet.Stop(context.Background(), ws, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}
}

// daemonRoot is the daemon module's root: the directory holding go.mod.
func daemonRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatal("no go.mod above the test's directory")
		}
		dir = parent
	}
}

// firstPageSites answers, for every watched name, the production sites (the
// integration harness excluded) that
// reference it as "<file relative to root>:<enclosing func>", sorted.
func firstPageSites(t *testing.T, root string, watched map[string][]string) map[string][]string {
	t.Helper()
	got := map[string][]string{}
	fset := token.NewFileSet()
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			// PRODUCTION ONLY: the integration suite's harness is test support
			// that scripts a shim's requests, not a daemon path that sends one.
			if name := d.Name(); name == "integration" || name == "testdata" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			ast.Inspect(fn, func(n ast.Node) bool {
				// A SITE IS A CALL OR A LITERAL — what builds an opening or a
				// request. A type named in a signature builds nothing.
				var expr ast.Expr
				switch node := n.(type) {
				case *ast.CallExpr:
					expr = node.Fun
				case *ast.CompositeLit:
					expr = node.Type
				default:
					return true
				}
				sel, ok := expr.(*ast.SelectorExpr)
				if !ok {
					return true
				}
				pkg, ok := sel.X.(*ast.Ident)
				if !ok {
					return true
				}
				name := pkg.Name + "." + sel.Sel.Name
				if _, watch := watched[name]; watch {
					got[name] = append(got[name], filepath.ToSlash(rel)+":"+fn.Name.Name)
				}
				return true
			})
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}
	for name := range got {
		sort.Strings(got[name])
	}
	return got
}
