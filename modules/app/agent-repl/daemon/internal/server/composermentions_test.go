package server

import (
	"context"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// receiveHostMentions reads the host stream up to its next `mentions` push.
func receiveHostMentions(t *testing.T, h *harness, ctx context.Context) func() *agentreplv1.HostWorkspaceMentions {
	t.Helper()
	stream := dialHostStream(t, h, ctx)
	return func() *agentreplv1.HostWorkspaceMentions {
		t.Helper()
		return receivePush(t, stream, "a mentions push", func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostWorkspaceMentions {
			return r.GetMentions()
		})
	}
}

// TestTheHostStreamServesTheComposerMentions pins the host stream's
// `mentions` arm: a fresh subscription is handed the record in force first,
// which is what hydrates Emacs after a restart, and then each change.
func TestTheHostStreamServesTheComposerMentions(t *testing.T) {
	tests := []struct {
		name    string
		before  *agentreplv1.HostWorkspaceMentions
		after   *agentreplv1.HostWorkspaceMentions
		wantAll []*agentreplv1.HostWorkspaceMentions
	}{
		{
			name:    "the record in force is the first push",
			before:  &agentreplv1.HostWorkspaceMentions{Commands: []string{"merge-queue"}, Workspaces: []string{"alpha"}},
			wantAll: []*agentreplv1.HostWorkspaceMentions{{Commands: []string{"merge-queue"}, Workspaces: []string{"alpha"}}},
		},
		{
			name:   "a change after the open is pushed",
			before: &agentreplv1.HostWorkspaceMentions{Commands: []string{"a"}},
			after:  &agentreplv1.HostWorkspaceMentions{Commands: []string{"b", "a"}},
			wantAll: []*agentreplv1.HostWorkspaceMentions{
				{Commands: []string{"a"}},
				{Commands: []string{"b", "a"}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			topic := h.Mentions.Topic(testWorkspaceID)
			topic.Publish(tt.before)
			next := receiveHostMentions(t, h, t.Context())

			// Act.
			got := []*agentreplv1.HostWorkspaceMentions{next()}
			if tt.after != nil {
				topic.Publish(tt.after)
				got = append(got, next())
			}

			// Assert.
			for i := range tt.wantAll {
				if !proto.Equal(got[i], tt.wantAll[i]) {
					t.Fatalf("push %d = %v, want %v", i, got[i], tt.wantAll[i])
				}
			}
		})
	}
}
