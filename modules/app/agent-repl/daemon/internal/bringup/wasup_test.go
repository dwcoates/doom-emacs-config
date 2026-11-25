package bringup

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

type fakeSessionList struct {
	sessions []wsm.Session
	gated    []wsm.WorkspaceID
	err      error
	gateErr  error
}

func (f fakeSessionList) Sessions(context.Context) ([]wsm.Session, error) { return f.sessions, f.err }

func (f fakeSessionList) ColdGateWorkspaces(context.Context) ([]wsm.WorkspaceID, error) {
	return f.gated, f.gateErr
}

func TestOnlyWasUp(t *testing.T) {
	up := wsm.Session{Workspace: "ws-up", VendorSessionID: "vendor-up"}
	created := wsm.Session{Workspace: "ws-created"}
	parked := wsm.Session{Workspace: "ws-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
	hibernated := wsm.Session{Workspace: "ws-hibernated", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
	killed := wsm.Session{Workspace: "ws-killed", Terminal: &wsm.SessionTerminal{Kind: "killed"}}
	pending := []wsm.Workspace{{ID: "ws-up"}, {ID: "ws-parked"}, {ID: "ws-hibernated"}, {ID: "ws-killed"}, {ID: "ws-created"}, {ID: "ws-never-started"}}

	// Act
	gotUp, gotLeft, _, err := OnlyWasUp(context.Background(), fakeSessionList{sessions: []wsm.Session{up, parked, hibernated, killed, created}}, pending)

	// Assert
	if err != nil {
		t.Fatalf("OnlyWasUp: %v", err)
	}
	if ids := IDs(gotUp); !slices.Equal(ids, []string{"ws-up"}) {
		t.Fatalf("up = %v, want only the session with no terminal", ids)
	}
	if ids := IDs(gotLeft); !slices.Equal(ids, []string{"ws-parked", "ws-hibernated", "ws-killed", "ws-created", "ws-never-started"}) {
		t.Fatalf("left = %v, want the parked, hibernated, killed, created-unstarted and never-started workspaces", ids)
	}
}

func TestOnlyWasUpSplitsNothingWhenTheSessionsCannotBeRead(t *testing.T) {
	// Act
	up, left, _, err := OnlyWasUp(context.Background(), fakeSessionList{err: errors.New("locked")}, []wsm.Workspace{{ID: "ws-1"}})

	// Assert
	if err == nil || up != nil || left != nil {
		t.Fatalf("OnlyWasUp = %v, %v, %v; want the failure and nothing split", up, left, err)
	}
}

func TestOnlyWasUpCountsAWorkspaceAtItsRecordedColdGateAsUp(t *testing.T) {
	// Arrange: a gate stands its shim down, so its session record reads parked or has none at all.
	parkedAtGate := wsm.Session{Workspace: "ws-gated-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
	pending := []wsm.Workspace{{ID: "ws-gated-parked"}, {ID: "ws-gated-no-session"}, {ID: "ws-parked"}}
	lister := fakeSessionList{
		sessions: []wsm.Session{parkedAtGate, {Workspace: "ws-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}},
		gated:    []wsm.WorkspaceID{"ws-gated-parked", "ws-gated-no-session"},
	}

	// Act
	gotUp, gotLeft, _, err := OnlyWasUp(context.Background(), lister, pending)

	// Assert
	if err != nil {
		t.Fatalf("OnlyWasUp: %v", err)
	}
	if ids := IDs(gotUp); !slices.Equal(ids, []string{"ws-gated-parked", "ws-gated-no-session"}) {
		t.Fatalf("up = %v, want both workspaces at a recorded gate", ids)
	}
	if ids := IDs(gotLeft); !slices.Equal(ids, []string{"ws-parked"}) {
		t.Fatalf("left = %v, want only the parked workspace with no gate", ids)
	}
}

func TestOnlyWasUpSplitsNothingWhenTheColdGatesCannotBeRead(t *testing.T) {
	// Act
	up, left, _, err := OnlyWasUp(context.Background(), fakeSessionList{gateErr: errors.New("locked")}, []wsm.Workspace{{ID: "ws-1"}})

	// Assert
	if err == nil || up != nil || left != nil {
		t.Fatalf("OnlyWasUp = %v, %v, %v; want the failure and nothing split", up, left, err)
	}
}

func TestWasUpFactsStatesWhatTheRecordsSay(t *testing.T) {
	at := time.Date(2026, 11, 23, 15, 27, 51, 0, time.UTC)
	parked := &wsm.SessionTerminal{Kind: wsm.TerminalParked, Detail: "keepalive_cycle", At: at}
	cases := []struct {
		name   string
		lister fakeSessionList
		want   WasUp
		wantUp bool
	}{
		{name: "a live session", lister: fakeSessionList{sessions: []wsm.Session{{Workspace: "ws", VendorSessionID: "v"}}},
			want: WasUp{Recorded: true, Conversation: true, Live: true}, wantUp: true},
		{name: "a parked session", lister: fakeSessionList{sessions: []wsm.Session{{Workspace: "ws", VendorSessionID: "v", Terminal: parked}}},
			want: WasUp{Recorded: true, Conversation: true, Terminal: parked}},
		{name: "a session created and never started", lister: fakeSessionList{sessions: []wsm.Session{{Workspace: "ws"}}},
			want: WasUp{Recorded: true}},
		{name: "no session record", lister: fakeSessionList{}, want: WasUp{}},
		{name: "a recorded cold gate", lister: fakeSessionList{gated: []wsm.WorkspaceID{"ws"}},
			want: WasUp{ColdGate: true}, wantUp: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			facts, err := WasUpFacts(context.Background(), tc.lister, []wsm.Workspace{{ID: "ws"}})

			// Assert
			if err != nil {
				t.Fatalf("WasUpFacts: %v", err)
			}
			if got := facts["ws"]; !reflect.DeepEqual(got, tc.want) || got.Up() != tc.wantUp {
				t.Fatalf("facts = %+v (up %v), want %+v (up %v)", got, got.Up(), tc.want, tc.wantUp)
			}
		})
	}
}

func TestWasUpFactsAnswersOnlyThePendingWorkspaces(t *testing.T) {
	// Arrange
	lister := fakeSessionList{sessions: []wsm.Session{{Workspace: "other", VendorSessionID: "v"}}, gated: []wsm.WorkspaceID{"other"}}

	// Act
	facts, err := WasUpFacts(context.Background(), lister, []wsm.Workspace{{ID: "ws"}})

	// Assert
	if err != nil || len(facts) != 1 || facts["ws"] != (WasUp{}) {
		t.Fatalf("facts = %v, %v; want only the pending workspace, with nothing recorded", facts, err)
	}
}

func TestWasUpFactsFailsWhenARecordCannotBeRead(t *testing.T) {
	cases := []struct {
		name   string
		lister fakeSessionList
	}{
		{name: "the sessions", lister: fakeSessionList{err: errors.New("locked")}},
		{name: "the cold gates", lister: fakeSessionList{gateErr: errors.New("locked")}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			facts, err := WasUpFacts(context.Background(), tc.lister, []wsm.Workspace{{ID: "ws"}})

			// Assert
			if err == nil || facts != nil {
				t.Fatalf("WasUpFacts = %v, %v; want the failure and no facts", facts, err)
			}
		})
	}
}

func TestWasUpContextStatesEveryFact(t *testing.T) {
	cases := []struct {
		name  string
		facts WasUp
		want  dlog.Context
	}{
		{name: "a live session", facts: WasUp{Recorded: true, Conversation: true, Live: true},
			want: dlog.Context{"was_up": true, "cold_gate": false, "recorded": true, "conversation": true, "live": true}},
		{name: "a parked session names its terminal", facts: WasUp{Recorded: true, Conversation: true,
			Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked, Detail: "keepalive_cycle", At: time.Date(2026, 11, 23, 15, 27, 51, 0, time.UTC)}},
			want: dlog.Context{"was_up": false, "cold_gate": false, "recorded": true, "conversation": true, "live": false,
				"terminal": wsm.TerminalParked, "terminal_detail": "keepalive_cycle", "terminal_at": "2026-11-23T15:27:51Z"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.facts.Context()

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("context = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestWasUpStartsShim(t *testing.T) {
	cases := []struct {
		name  string
		facts WasUp
		want  bool
	}{
		{"a live session runs its shim", WasUp{Live: true}, true},
		{"a recorded cold gate runs nothing", WasUp{ColdGate: true}, false},
		{"a live session at a recorded gate runs nothing", WasUp{Live: true, ColdGate: true}, false},
		{"a parked session runs nothing", WasUp{Recorded: true, Conversation: true}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.facts.StartsShim()

			// Assert
			if got != tc.want {
				t.Fatalf("StartsShim(%+v) = %v, want %v", tc.facts, got, tc.want)
			}
		})
	}
}

func TestRaiseMarkers(t *testing.T) {
	cases := []struct {
		name  string
		facts map[wsm.WorkspaceID]WasUp
		want  []string
	}{
		{"a live session is marked", map[wsm.WorkspaceID]WasUp{"ws-1": {Live: true}}, []string{"ws-1"}},
		{"a recorded cold gate is not marked", map[wsm.WorkspaceID]WasUp{"ws-1": {ColdGate: true}}, nil},
		{"only the starts that run a shim are marked", map[wsm.WorkspaceID]WasUp{"ws-1": {ColdGate: true}, "ws-2": {Live: true}}, []string{"ws-2"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var up []wsm.Workspace
			for _, id := range []wsm.WorkspaceID{"ws-1", "ws-2"} {
				if _, ok := tc.facts[id]; ok {
					up = append(up, wsm.Workspace{ID: id})
				}
			}
			var raised []string
			raise := func(ws wsm.WorkspaceID, underWay bool) {
				if !underWay {
					t.Fatalf("RaiseMarkers lowered %s; it only raises", ws)
				}
				raised = append(raised, string(ws))
			}

			// Act
			RaiseMarkers(raise, up, tc.facts)

			// Assert
			if !slices.Equal(raised, tc.want) {
				t.Fatalf("raised = %v, want %v", raised, tc.want)
			}
		})
	}
}

func TestOnlyWasUpAnswersTheFactsItSplitBy(t *testing.T) {
	// Arrange
	lister := fakeSessionList{
		sessions: []wsm.Session{{Workspace: "ws-up", VendorSessionID: "vendor-up"}},
		gated:    []wsm.WorkspaceID{"ws-gated"},
	}
	pending := []wsm.Workspace{{ID: "ws-up"}, {ID: "ws-gated"}}

	// Act
	_, _, facts, err := OnlyWasUp(context.Background(), lister, pending)

	// Assert
	if err != nil {
		t.Fatalf("OnlyWasUp: %v", err)
	}
	if !facts["ws-up"].StartsShim() || facts["ws-gated"].StartsShim() || !facts["ws-gated"].ColdGate {
		t.Fatalf("facts = %+v, want ws-up live and ws-gated at its gate", facts)
	}
}

// TestEveryBringUpMarkerRaiseGoesThroughRaiseMarkers pins that no boot or
// takeover raises the bring-up marker by hand: each raise goes through
// RaiseMarkers, so a start that runs no shim is never marked.
func TestEveryBringUpMarkerRaiseGoesThroughRaiseMarkers(t *testing.T) {
	// Arrange
	handRaise := regexp.MustCompile(`BringingUp\([^)]*,\s*true\)`)
	var offenders []string
	for _, dir := range []string{"../boot", "../rollout"} {
		files, err := filepath.Glob(filepath.Join(dir, "*.go"))
		if err != nil {
			t.Fatalf("Glob(%s): %v", dir, err)
		}
		if len(files) == 0 {
			t.Fatalf("no Go files under %s: the scan read nothing", dir)
		}
		for _, file := range files {
			if strings.HasSuffix(file, "_test.go") {
				continue
			}
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("ReadFile(%s): %v", file, err)
			}
			// Act
			if handRaise.Match(src) {
				offenders = append(offenders, file)
			}
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("files raising the bring-up marker by hand = %v, want every raise through bringup.RaiseMarkers", offenders)
	}
}
