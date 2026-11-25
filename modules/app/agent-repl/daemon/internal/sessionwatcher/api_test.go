package sessionwatcher

import (
	"agent-repld/internal/sessionwatcher/watchtest"
	"reflect"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
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
	w, err := Start(t.Context(), "ws-1", watchtest.NewClient(), Session{Opening: WorkspaceOpened()}, sinks, newTestLogger())

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
			_, err := Start(t.Context(), "ws-1", watchtest.NewClient(), session, sinks, newTestLogger())

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

// A WATCHER'S WORK IS LABELED BY ITS WORKSPACE (internal/proflabel): the
// session watch it opens runs on its goroutines, under the label Start set.
func TestStartLabelsTheWatchersWorkByItsWorkspace(t *testing.T) {
	// Arrange / Act
	h := startHarness(t, Session{}, nil)
	h.session = h.client.NextSessionOpen(t)

	// Assert
	h.client.Mu.Lock()
	label, labeled := h.client.SessionLabel, h.client.SessionLabeled
	h.client.Mu.Unlock()
	if !labeled || label != "ws-1" {
		t.Fatalf("the session watch ran labeled %q (%v), want the watcher's workspace ws-1", label, labeled)
	}
}
