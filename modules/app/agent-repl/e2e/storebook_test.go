package e2e

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"

	"connectrpc.com/connect"
)

// readAgentBook answers AGENT's newest page straight from the store, the
// suite's one read-only door to a book for a fact no daemon surface carries
// (a file-plane-only record, a prompt an Emacs-layer world cannot ask the
// daemon about). A transport failure and a failure answer are both errors,
// so a caller that polls can retry and one that must not can fail.
func readAgentBook(ctx context.Context, client storev1connect.ShimStoreClient, agent *conversationv1.AgentId) (*storev1.AgentSessionPage, error) {
	resp, err := client.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{Agent: agent}))
	if err != nil {
		return nil, fmt.Errorf("OpenAgentSession(%s): %w", agent.GetValue(), err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		return nil, fmt.Errorf("OpenAgentSession(%s) = %v, want success", agent.GetValue(), resp.Msg)
	}
	return success.GetPage(), nil
}

// fakeBookStore answers OpenAgentSession with a scripted response or error;
// every other method is the embedded nil interface and panics if reached.
type fakeBookStore struct {
	storev1connect.ShimStoreClient
	resp *storev1.OpenAgentSessionResponse
	err  error
}

func (f fakeBookStore) OpenAgentSession(context.Context, *connect.Request[storev1.OpenAgentSessionRequest]) (*connect.Response[storev1.OpenAgentSessionResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.resp), nil
}

func TestReadAgentBook(t *testing.T) {
	page := &storev1.AgentSessionPage{}
	tests := []struct {
		name     string
		store    fakeBookStore
		wantPage bool
		wantErr  string
	}{
		{
			name:     "a success answers its page",
			store:    fakeBookStore{resp: &storev1.OpenAgentSessionResponse{Result: &storev1.OpenAgentSessionResponse_Success{Success: &storev1.OpenAgentSessionSuccess{Page: page}}}},
			wantPage: true,
		},
		{
			name:    "a transport failure is an error naming the book",
			store:   fakeBookStore{err: errors.New("dial unix: no such file")},
			wantErr: "OpenAgentSession(agent-1): dial unix: no such file",
		},
		{
			name:    "a failure answer is an error",
			store:   fakeBookStore{resp: &storev1.OpenAgentSessionResponse{Result: &storev1.OpenAgentSessionResponse_Failure{Failure: &storev1.OpenAgentSessionFailure{}}}},
			wantErr: "want success",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := readAgentBook(context.Background(), tt.store, &conversationv1.AgentId{Value: "agent-1"})

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("readAgentBook error = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != page {
				t.Fatalf("readAgentBook = (%v, %v), want the success's page", got, err)
			}
		})
	}
}

// TestEveryBookReadGoesThroughReadAgentBook pins that no test opens a book by
// hand: the store's success arm and failure arm are read in one place.
func TestEveryBookReadGoesThroughReadAgentBook(t *testing.T) {
	for _, name := range []string{"remainder_e2e_test.go", "emacs_interrupt_e2e_test.go", "subagents_e2e_test.go"} {
		// Act.
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}

		// Assert.
		if strings.Contains(string(raw), ".OpenAgentSession(") {
			t.Fatalf("%s opens a store book by hand; use readAgentBook", name)
		}
	}
}
