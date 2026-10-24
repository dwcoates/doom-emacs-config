package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// THE SESSION PROFILE: every StartSession names the session it is, an agent's
// or a zettel chat thread's.

func TestStartNamesTheSessionProfile(t *testing.T) {
	cases := []struct {
		name   string
		kind   wsm.WorkspaceKind
		agent  bool
		prompt string
	}{
		{name: "a repository workspace starts an agent", kind: wsm.WorkspaceKindRepository, agent: true},
		{name: "a thread starts a chat under the chat system prompt", kind: wsm.WorkspaceKindThread, prompt: "Answer questions."},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			ws.Kind = tc.kind
			f.db.with(ws)
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID}
			f.chatPrompt = "Answer questions."

			// Act
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert
			profile := f.client.requests[0].GetProfile()
			if got := profile.GetAgent() != nil; got != tc.agent {
				t.Fatalf("profile = %v, want agent %v", profile, tc.agent)
			}
			if got := profile.GetChat().GetSystemPrompt().GetText(); got != tc.prompt {
				t.Fatalf("system prompt = %q, want %q", got, tc.prompt)
			}
		})
	}
}

func TestAThreadWhoseChatPromptCannotBeReadIsNotStarted(t *testing.T) {
	cases := []struct {
		name   string
		prompt string
		err    error
	}{
		{name: "the prompt is unreadable", err: errors.New("no such file")},
		{name: "the prompt is empty", prompt: "  \n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			ws.Kind = wsm.WorkspaceKindThread
			f.db.with(ws)
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID}
			f.chatPrompt, f.chatPromptErr = tc.prompt, tc.err

			// Act
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert
			if err == nil {
				t.Fatal("Start answered no error")
			}
			if len(f.client.requests) != 0 {
				t.Fatalf("a StartSession was sent: %v", f.client.requests)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Message: "the chat system prompt could not be read; the thread's session was not started"}); !ok {
				t.Fatalf("records = %+v, want the chat prompt failure", f.log.logger.Records())
			}
		})
	}
}
