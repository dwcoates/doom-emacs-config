package server

import (
	"context"
	"errors"
	"slices"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

func commandSelection(name string) *agentreplv1.ComposerSelection {
	return &agentreplv1.ComposerSelection{Kind: &agentreplv1.ComposerSelection_Command{Command: &agentreplv1.ComposerSelectionCommand{Name: name}}}
}

// TestReportComposerSelectionRecordsEachKind pins that every kind of selection
// reaches the record as its kind and name, and is answered success.
func TestReportComposerSelectionRecordsEachKind(t *testing.T) {
	tests := []struct {
		name      string
		selection *agentreplv1.ComposerSelection
		want      fakeSelection
	}{
		{"a command", commandSelection("compact"), fakeSelection{Kind: wsm.ComposerSelectionCommand, Name: "compact"}},
		{"a workspace", &agentreplv1.ComposerSelection{Kind: &agentreplv1.ComposerSelection_Workspace{Workspace: &agentreplv1.ComposerSelectionWorkspace{Name: "alpha"}}},
			fakeSelection{Kind: wsm.ComposerSelectionWorkspace, Name: "alpha"}},
		{"a channel", &agentreplv1.ComposerSelection{Kind: &agentreplv1.ComposerSelection_Channel{Channel: &agentreplv1.ComposerSelectionChannel{Name: "eng"}}},
			fakeSelection{Kind: wsm.ComposerSelectionChannel, Name: "eng"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.ReportComposerSelection(context.Background(),
				connect.NewRequest(&agentreplv1.ReportComposerSelectionRequest{Selection: tt.selection}))

			// Assert.
			if err != nil {
				t.Fatalf("ReportComposerSelection: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("response = %v, want success", resp.Msg)
			}
			if !slices.Equal(h.Selections.selected, []fakeSelection{tt.want}) {
				t.Fatalf("selected = %+v, want %+v", h.Selections.selected, tt.want)
			}
		})
	}
}

// TestReportComposerSelectionAnswersAFailedWriteAsNotRecorded pins the error
// arm: a write the record refused is answered `not_recorded` with its cause.
func TestReportComposerSelectionAnswersAFailedWriteAsNotRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Selections.err = errors.New("disk full")

	// Act.
	resp, err := h.Client.ReportComposerSelection(context.Background(),
		connect.NewRequest(&agentreplv1.ReportComposerSelectionRequest{Selection: commandSelection("compact")}))

	// Assert.
	if err != nil {
		t.Fatalf("ReportComposerSelection: %v", err)
	}
	if got := resp.Msg.GetError().GetNotRecorded().GetDetail(); got != "disk full" {
		t.Fatalf("response = %v, want not_recorded carrying the cause", resp.Msg)
	}
}

// TestReportComposerSelectionRefusesAMalformedRequest pins the validation:
// nothing reaches the record from a request that names no candidate.
func TestReportComposerSelectionRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ReportComposerSelectionRequest
	}{
		{"no selection", &agentreplv1.ReportComposerSelectionRequest{}},
		{"no kind", &agentreplv1.ReportComposerSelectionRequest{Selection: &agentreplv1.ComposerSelection{}}},
		{"an empty name", &agentreplv1.ReportComposerSelectionRequest{Selection: commandSelection("")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.ReportComposerSelection(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Selections.selected) != 0 {
				t.Fatalf("err = %v, selected = %+v; want InvalidArgument and nothing recorded", err, h.Selections.selected)
			}
		})
	}
}

// TestTheHostStreamServesTheComposerSelections pins the host stream's
// `composer_selections` arm: a fresh subscription is handed the record in
// force first, which hydrates Emacs after a restart, and then each change.
func TestTheHostStreamServesTheComposerSelections(t *testing.T) {
	tests := []struct {
		name    string
		before  *agentreplv1.HostComposerSelections
		after   *agentreplv1.HostComposerSelections
		wantAll []*agentreplv1.HostComposerSelections
	}{
		{
			name:    "the record in force is the first push",
			before:  &agentreplv1.HostComposerSelections{Commands: []string{"compact"}, Channels: []string{"eng"}},
			wantAll: []*agentreplv1.HostComposerSelections{{Commands: []string{"compact"}, Channels: []string{"eng"}}},
		},
		{
			name:   "a change after the open is pushed",
			before: &agentreplv1.HostComposerSelections{Commands: []string{"a"}},
			after:  &agentreplv1.HostComposerSelections{Commands: []string{"b", "a"}},
			wantAll: []*agentreplv1.HostComposerSelections{
				{Commands: []string{"a"}},
				{Commands: []string{"b", "a"}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			topic := h.Selections.Topic()
			topic.Publish(tt.before)
			stream := dialHostStream(t, h, t.Context())
			next := func() *agentreplv1.HostComposerSelections {
				return receivePush(t, stream, "a composer selections push", func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostComposerSelections {
					return r.GetComposerSelections()
				})
			}

			// Act.
			got := []*agentreplv1.HostComposerSelections{next()}
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
