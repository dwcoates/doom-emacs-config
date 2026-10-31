package server

import (
	"fmt"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func fanOutWorkspace(id string) *wsm.Workspace {
	return &wsm.Workspace{ID: ids.WorkspaceID(id)}
}

// sendToStrangers has sender prompt n unrelated workspaces named stranger-0..
func sendToStrangers(t *testing.T, f *fanOut, sender *wsm.Workspace, n int, at time.Time) {
	t.Helper()
	for i := range n {
		if !f.admit(sender, fanOutWorkspace(fmt.Sprintf("stranger-%d", i)), false, at) {
			t.Fatalf("send %d of %d refused, want admitted under the cap", i+1, n)
		}
	}
}

func TestFanOutAdmit(t *testing.T) {
	sender := fanOutWorkspace("sender")
	parentID := ids.WorkspaceID("parent")
	child := &wsm.Workspace{ID: "child", Parent: &sender.ID}
	start := time.Unix(1_700_000_000, 0)
	tests := []struct {
		name      string
		recipient *wsm.Workspace
		asUser    bool
		arrange   func(t *testing.T, f *fanOut)
		at        time.Time
		want      bool
	}{
		{name: "the first unrelated recipients are admitted", recipient: fanOutWorkspace("stranger-2"),
			arrange: func(t *testing.T, f *fanOut) { sendToStrangers(t, f, sender, 2, start) }, at: start, want: true},
		{name: "one more unrelated recipient past the cap is refused", recipient: fanOutWorkspace("one-too-many"),
			arrange: func(t *testing.T, f *fanOut) { sendToStrangers(t, f, sender, fanOutCap, start) }, at: start, want: false},
		{name: "a recipient already reached in the window widens nothing", recipient: fanOutWorkspace("stranger-0"),
			arrange: func(t *testing.T, f *fanOut) { sendToStrangers(t, f, sender, fanOutCap, start) }, at: start, want: true},
		{name: "the user lifts the cap", recipient: fanOutWorkspace("one-too-many"), asUser: true,
			arrange: func(t *testing.T, f *fanOut) { sendToStrangers(t, f, sender, fanOutCap, start) }, at: start, want: true},
		{name: "a child never counts", recipient: child,
			arrange: func(t *testing.T, f *fanOut) { sendToStrangers(t, f, sender, fanOutCap, start) }, at: start, want: true},
		{name: "a parent never counts", recipient: fanOutWorkspace(string(parentID)),
			arrange: func(t *testing.T, f *fanOut) {
				sender.Parent = &parentID
				t.Cleanup(func() { sender.Parent = nil })
				sendToStrangers(t, f, sender, fanOutCap, start)
			}, at: start, want: true},
		{name: "a merge's requester never counts", recipient: fanOutWorkspace("requester"),
			arrange: func(t *testing.T, f *fanOut) {
				sender.MergeOf = &wsm.MergeOrigin{Requester: "requester"}
				t.Cleanup(func() { sender.MergeOf = nil })
				sendToStrangers(t, f, sender, fanOutCap, start)
			}, at: start, want: true},
		{name: "a reply to a correspondent never counts", recipient: fanOutWorkspace("asker"),
			arrange: func(t *testing.T, f *fanOut) {
				if !f.admit(fanOutWorkspace("asker"), sender, false, start) {
					t.Fatal("the asker's first send refused")
				}
				sendToStrangers(t, f, sender, fanOutCap, start)
			}, at: start, want: true},
		{name: "the window frees the cap once it passes", recipient: fanOutWorkspace("later"),
			arrange: func(t *testing.T, f *fanOut) { sendToStrangers(t, f, sender, fanOutCap, start) },
			at: start.Add(fanOutWindow), want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFanOut()
			tc.arrange(t, f)

			// Act
			got := f.admit(sender, tc.recipient, tc.asUser, tc.at)

			// Assert
			if got != tc.want {
				t.Fatalf("admit = %v, want %v", got, tc.want)
			}
		})
	}
}
