package sessionwatcher

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"reflect"
	"sort"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/sourcescan"
)

// TestLiveWorkSetEmpty covers the freeness half the watcher answers with: an
// empty live set. Every kind of live work must defeat it, monitors included —
// a monitor opens no stream, and forgetting it there would report a session
// with a live watcher as free.
func TestLiveWorkSetEmpty(t *testing.T) {
	tests := []struct {
		name string
		live LiveWorkSet
		want bool
	}{
		{
			name: "nothing live is empty",
			live: LiveWorkSet{},
			want: true,
		},
		{
			name: "a live subagent is not empty",
			live: LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}},
			want: false,
		},
		{
			name: "a live shell is not empty",
			live: LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("shell-1")}},
			want: false,
		},
		{
			name: "a live monitor is not empty",
			live: LiveWorkSet{Monitors: []*conversationv1.DetachedWorkId{workID("mon-1")}},
			want: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act.
			got := tt.live.Empty()

			// Assert.
			if got != tt.want {
				t.Fatalf("Empty() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestStartRefusesAnIncompleteFleet covers the constructor's refusals: a
// watcher with a missing collaborator would fail later, on a frame, where the
// cause is no longer visible.
// TestStartAcceptsAnUnsetSessionStartedAsAPureAttach is the landing-7 contract
// change: an ADOPTING daemon (crash boot, handover) opens the watch fleet with
// NO session facts and takes them from the shim's re-announcement, so an unset
// Session.Started is legal rather than the refusal it used to be.
func TestStartAcceptsAnUnsetSessionStartedAsAPureAttach(t *testing.T) {
	// Arrange.
	rec := newRecorder()
	sinks := Sinks{
		Feed:       &feedSink{rec: rec},
		Footer:     &footerSink{rec: rec},
		Topbar:     &topbarSink{rec: rec},
		Sidebar:    &sidebarSink{rec: rec},
		Lifecycle:  &lifecycleSink{rec: rec},
		Commands:   &commandSink{rec: rec},
		CacheLapse: &cacheLapseSink{rec: rec},
		Clock:      newManualClock(),
	}

	// Act.
	w, err := Start(t.Context(), "ws-1", newFakeClient(), Session{Opening: WorkspaceOpened()}, sinks, newTestLogger())

	// Assert.
	if err != nil {
		t.Fatalf("Start with no session facts = %v, want a pure attach", err)
	}
	t.Cleanup(func() { _ = w.Close() })
}

func TestStartRefusesAnIncompleteFleet(t *testing.T) {
	tests := []struct {
		name    string
		mutate  func(*Session, *Sinks)
		wantErr string
	}{
		{
			name:    "a missing feed sink is refused",
			mutate:  func(_ *Session, sinks *Sinks) { sinks.Feed = nil },
			wantErr: "every sink but Holds is required",
		},
		{
			name:    "a missing lifecycle sink is refused",
			mutate:  func(_ *Session, sinks *Sinks) { sinks.Lifecycle = nil },
			wantErr: "every sink but Holds is required",
		},
		{
			name:    "a missing command catalog sink is refused",
			mutate:  func(_ *Session, sinks *Sinks) { sinks.Commands = nil },
			wantErr: "every sink but Holds is required",
		},
		{
			name:    "a missing cache lapse sink is refused",
			mutate:  func(_ *Session, sinks *Sinks) { sinks.CacheLapse = nil },
			wantErr: "every sink but Holds is required",
		},
		{
			name:    "a missing clock is refused",
			mutate:  func(_ *Session, sinks *Sinks) { sinks.Clock = nil },
			wantErr: "a clock is required",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			rec := newRecorder()
			session := Session{Started: sessionStarted(""), Opening: WorkspaceOpened()}
			sinks := Sinks{
				Feed:       &feedSink{rec: rec},
				Footer:     &footerSink{rec: rec},
				Topbar:     &topbarSink{rec: rec},
				Sidebar:    &sidebarSink{rec: rec},
				Lifecycle:  &lifecycleSink{rec: rec},
				Commands:   &commandSink{rec: rec},
				CacheLapse: &cacheLapseSink{rec: rec},
				Clock:      newManualClock(),
			}
			tt.mutate(&session, &sinks)

			// Act.
			_, err := Start(t.Context(), "ws-1", newFakeClient(), session, sinks, newTestLogger())

			// Assert.
			if err == nil {
				t.Fatal("Start accepted an incomplete fleet")
			}
			if !contains(err.Error(), tt.wantErr) {
				t.Fatalf("Start error = %q, want it to mention %q", err, tt.wantErr)
			}
		})
	}
}

func TestPlaceOf(t *testing.T) {
	// Arrange.
	recorded := &conversationv1.ConversationPlace{AtMs: 10, Ordinal: 1}
	received := &conversationv1.ConversationPlace{AtMs: 20}
	tests := []struct {
		name string
		at   *conversationv1.HistoryEntryAt
		want *conversationv1.ConversationPlace
	}{
		{
			name: "a recorded place is the entry's place",
			at:   &conversationv1.HistoryEntryAt{Place: &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: recorded}},
			want: recorded,
		},
		{
			name: "a received stand-in is the entry's place",
			at:   &conversationv1.HistoryEntryAt{Place: &conversationv1.HistoryEntryAt_ReceivedPlace{ReceivedPlace: received}},
			want: received,
		},
		{
			name: "no stated place is none",
			at:   &conversationv1.HistoryEntryAt{},
			want: nil,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := PlaceOf(tc.at)

			// Assert.
			if got != tc.want {
				t.Fatalf("PlaceOf = %v, want %v", got, tc.want)
			}
		})
	}
}

// sinkMethods answers, for every Sinks field whose type is an interface this
// package declares, the field's name mapped to its interface's method names.
func sinkMethods(t *testing.T, files []*ast.File) map[string][]string {
	t.Helper()
	interfaces := map[string][]string{}
	var fields map[string]string
	for _, file := range files {
		ast.Inspect(file, func(n ast.Node) bool {
			spec, ok := n.(*ast.TypeSpec)
			if !ok {
				return true
			}
			switch typ := spec.Type.(type) {
			case *ast.InterfaceType:
				methods := []string{}
				for _, m := range typ.Methods.List {
					for _, name := range m.Names {
						methods = append(methods, name.Name)
					}
				}
				interfaces[spec.Name.Name] = methods
			case *ast.StructType:
				if spec.Name.Name != "Sinks" {
					return true
				}
				fields = map[string]string{}
				for _, f := range typ.Fields.List {
					ident, ok := f.Type.(*ast.Ident)
					if !ok {
						continue
					}
					for _, name := range f.Names {
						fields[name.Name] = ident.Name
					}
				}
			}
			return true
		})
	}
	if fields == nil {
		t.Fatal("the package declares no Sinks struct")
	}
	out := map[string][]string{}
	for field, typ := range fields {
		if methods, ok := interfaces[typ]; ok {
			out[field] = methods
		}
	}
	return out
}

// sinkCalls answers every `<x>.sinks.<Field>.<Method>(...)` and
// `sinks.<Field>.<Method>(...)` call in files, as "Field.Method".
func sinkCalls(files []*ast.File) map[string]bool {
	out := map[string]bool{}
	for _, file := range files {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			method, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			field, ok := method.X.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			switch holder := field.X.(type) {
			case *ast.Ident:
				if holder.Name != "sinks" {
					return true
				}
			case *ast.SelectorExpr:
				if holder.Sel.Name != "sinks" {
					return true
				}
			default:
				return true
			}
			out[field.Sel.Name+"."+method.Sel.Name] = true
			return true
		})
	}
	return out
}

// TestEverySinkMethodHasAWatcherCaller holds the watcher's fan-out to the
// sinks it declares: a method a sink asks for and the watcher never calls is a
// fact that resolver can never learn. The roster's OnActivity was such a
// method, and its `api_retrying` outlived every vendor reconnect.
func TestEverySinkMethodHasAWatcherCaller(t *testing.T) {
	// Arrange.
	fset := token.NewFileSet()
	var files []*ast.File
	for _, src := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(fset, src.Name, src.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", src.Name, err)
		}
		files = append(files, parsed)
	}
	sinks := sinkMethods(t, files)
	var wanted []string
	for field, methods := range sinks {
		for _, method := range methods {
			wanted = append(wanted, field+"."+method)
		}
	}
	sort.Strings(wanted)
	if len(wanted) == 0 {
		t.Fatal("found no sink methods to hold; the scan is broken")
	}

	// Act.
	called := sinkCalls(files)

	// Assert.
	for _, name := range wanted {
		t.Run(name, func(t *testing.T) {
			if !called[name] {
				t.Fatalf("the watcher never calls sinks.%s; the resolver behind it never learns the fact it asks for", name)
			}
		})
	}
}

func TestLiveWorkSetNames(t *testing.T) {
	tests := []struct {
		name string
		live LiveWorkSet
		want []string
	}{
		{name: "nothing live names nothing", live: LiveWorkSet{}, want: []string{}},
		{
			name: "every kind is named by its prefix, sorted",
			live: LiveWorkSet{
				Shells:   []*conversationv1.DetachedWorkId{workID("s-1")},
				Agents:   []*conversationv1.AgentId{agentID("a-1")},
				Monitors: []*conversationv1.DetachedWorkId{workID("m-1")},
				Pending:  []*conversationv1.DetachedWorkId{workID("p-1")},
			},
			want: []string{"agent:a-1", "monitor:m-1", "pending:p-1", "shell:s-1"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act
			got := tt.live.Names()
			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Names() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAgentMembersAreTheLiveAgentsAndThePendingHandles(t *testing.T) {
	// Arrange.
	live := LiveWorkSet{
		Agents:   []*conversationv1.AgentId{{Value: "a-1"}},
		Pending:  []*conversationv1.DetachedWorkId{{Value: "w-2"}},
		Shells:   []*conversationv1.DetachedWorkId{{Value: "sh-3"}},
		Monitors: []*conversationv1.DetachedWorkId{{Value: "m-4"}},
	}

	// Act.
	members := live.AgentMembers()

	// Assert.
	if len(members) != 2 {
		t.Fatalf("members = %v, want a-1 and w-2 only", members)
	}
	for _, id := range []string{"a-1", "w-2"} {
		if _, ok := members[id]; !ok {
			t.Fatalf("members = %v, want %s", members, id)
		}
	}
}

func TestAgentsNewSinceAreTheMembersThePreviousSetDidNotHold(t *testing.T) {
	// Arrange.
	live := LiveWorkSet{Agents: []*conversationv1.AgentId{{Value: "b"}, {Value: "a"}, {Value: "c"}}}
	prev := map[string]struct{}{"b": {}}

	// Act.
	fresh := live.AgentsNewSince(prev)

	// Assert.
	if len(fresh) != 2 || fresh[0] != "a" || fresh[1] != "c" {
		t.Fatalf("fresh = %v, want [a c], sorted", fresh)
	}
}

func TestAgentsNewSinceAnEmptyPreviousSetIsEveryMember(t *testing.T) {
	// Arrange.
	live := LiveWorkSet{Pending: []*conversationv1.DetachedWorkId{{Value: "w-1"}}}

	// Act.
	fresh := live.AgentsNewSince(nil)

	// Assert.
	if len(fresh) != 1 || fresh[0] != "w-1" {
		t.Fatalf("fresh = %v, want [w-1]", fresh)
	}
}

func TestEveryViewFindsASubagentNewToTheSetThroughAgentsNewSince(t *testing.T) {
	// Arrange: the views that take a run's end as final, and must draw the
	// subagent live again when the set lists it anew.
	want := []string{"internal/resolve/feed/subagent.go", "internal/resolve/footer/livework.go"}

	// Act.
	root := sourcescan.DaemonRoot(t)
	var sites []string
	sourcescan.EachDaemonSource(t, root, nil, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if sel, ok := n.(*ast.SelectorExpr); ok && sel.Sel.Name == "AgentsNewSince" {
				rel, err := filepath.Rel(root, fset.Position(sel.Pos()).Filename)
				if err != nil {
					t.Fatalf("relative path: %v", err)
				}
				sites = append(sites, rel)
			}
			return true
		})
	})

	// Assert.
	sort.Strings(sites)
	if !reflect.DeepEqual(sites, want) {
		t.Fatalf("AgentsNewSince is called from %v, want %v", sites, want)
	}
}

// A WATCHER'S WORK IS LABELED BY ITS WORKSPACE (internal/proflabel): the
// session watch it opens runs on its goroutines, under the label Start set.
func TestStartLabelsTheWatchersWorkByItsWorkspace(t *testing.T) {
	// Arrange / Act
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Assert
	h.client.mu.Lock()
	label, labeled := h.client.sessionLabel, h.client.sessionLabeled
	h.client.mu.Unlock()
	if !labeled || label != "ws-1" {
		t.Fatalf("the session watch ran labeled %q (%v), want the watcher's workspace ws-1", label, labeled)
	}
}
