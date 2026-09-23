package sessionwatcher

import (
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
