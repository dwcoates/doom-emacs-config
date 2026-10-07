package sessionwatcher

import (
	"go/ast"
	"go/parser"
	"go/token"
	"sort"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/sourcescan"
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
		Feed:      &feedSink{rec: rec},
		Footer:    &footerSink{rec: rec},
		Topbar:    &topbarSink{rec: rec},
		Sidebar:   &sidebarSink{rec: rec},
		Lifecycle: &lifecycleSink{rec: rec},
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
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			rec := newRecorder()
			session := Session{Started: sessionStarted(""), Opening: WorkspaceOpened()}
			sinks := Sinks{
				Feed:      &feedSink{rec: rec},
				Footer:    &footerSink{rec: rec},
				Topbar:    &topbarSink{rec: rec},
				Sidebar:   &sidebarSink{rec: rec},
				Lifecycle: &lifecycleSink{rec: rec},
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
