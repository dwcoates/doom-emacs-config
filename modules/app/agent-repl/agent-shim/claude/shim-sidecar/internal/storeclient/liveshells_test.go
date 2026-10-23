package storeclient

import (
	"os"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

func TestLiveShellRunsAnnouncedBeforeReturnsTheRunsAnswered(t *testing.T) {
	// Arrange.
	run := &storev1.GetLiveShellRunsAnnouncedBeforeRun{
		Detached:      &storev1.GetLiveWorkDetached{Work: &conversationv1.DetachedWorkId{Value: "call-1"}, Owner: &conversationv1.AgentId{Value: "agent-a"}},
		Run:           &conversationv1.AgentActivityId{Value: "call-1"},
		AnnouncedAtMs: 7,
	}
	store := &fakeStore{liveShells: &storev1.GetLiveShellRunsAnnouncedBeforeResponse{
		Result: &storev1.GetLiveShellRunsAnnouncedBeforeResponse_Success{Success: &storev1.GetLiveShellRunsAnnouncedBeforeSuccess{
			Runs: []*storev1.GetLiveShellRunsAnnouncedBeforeRun{run},
		}},
	}}
	client := serve(t, store)

	// Act.
	got, err := client.LiveShellRunsAnnouncedBefore(ctx(), 1000)

	// Assert.
	if err != nil || len(got) != 1 || got[0].GetRun().GetValue() != "call-1" || got[0].GetDetached().GetOwner().GetValue() != "agent-a" {
		t.Fatalf("LiveShellRunsAnnouncedBefore = %v, %v; want call-1 owned by agent-a", got, err)
	}
	if store.lastLiveShellsReq.GetAnnouncedBeforeMs() != 1000 {
		t.Fatalf("asked before %d, want 1000", store.lastLiveShellsReq.GetAnnouncedBeforeMs())
	}
}

func TestLiveShellRunsAnnouncedBeforeFailureArmIsARefusalNamingItsKind(t *testing.T) {
	tests := []struct {
		name      string
		failure   *storev1.GetLiveShellRunsAnnouncedBeforeFailure
		wantKind  RefusalKind
		wantField string
	}{
		{
			name: "storage failure",
			failure: &storev1.GetLiveShellRunsAnnouncedBeforeFailure{Detail: "database is locked",
				Kind: &storev1.GetLiveShellRunsAnnouncedBeforeFailure_StorageFailure{StorageFailure: &storev1.GetLiveShellRunsAnnouncedBeforeStorageFailure{}}},
			wantKind: RefusalStorageFailure,
		},
		{
			name: "invalid request",
			failure: &storev1.GetLiveShellRunsAnnouncedBeforeFailure{Detail: "not an instant",
				Kind: &storev1.GetLiveShellRunsAnnouncedBeforeFailure_InvalidRequest{InvalidRequest: &storev1.GetLiveShellRunsAnnouncedBeforeInvalidRequest{Field: "announced_before_ms"}}},
			wantKind:  RefusalInvalidRequest,
			wantField: "announced_before_ms",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			client, lines := serveLogged(t, &fakeStore{liveShells: &storev1.GetLiveShellRunsAnnouncedBeforeResponse{
				Result: &storev1.GetLiveShellRunsAnnouncedBeforeResponse_Failure{Failure: test.failure},
			}})

			// Act.
			_, err := client.LiveShellRunsAnnouncedBefore(ctx(), 1000)

			// Assert.
			refusal, ok := err.(*RefusalError)
			if !ok || refusal.Kind != test.wantKind || refusal.Field != test.wantField {
				t.Fatalf("error = %v, want a %v refusal naming %q", err, test.wantKind, test.wantField)
			}
			requireOnceIn(t, parseLogLines(t, *lines), "storeclient-live-shell-runs", "error")
		})
	}
}

func TestLiveShellRunsAnnouncedBeforeUnsetResultIsAnError(t *testing.T) {
	// Arrange.
	client, lines := serveLogged(t, &fakeStore{liveShells: &storev1.GetLiveShellRunsAnnouncedBeforeResponse{}})

	// Act.
	_, err := client.LiveShellRunsAnnouncedBefore(ctx(), 1000)

	// Assert.
	if err == nil || IsRefusal(err) {
		t.Fatalf("error = %v, want a non-refusal error for an unset result", err)
	}
	requireOnceIn(t, parseLogLines(t, *lines), "storeclient-live-shell-runs", "error")
}

func TestLiveShellRunsAnnouncedBeforeTransportErrorIsNotARefusal(t *testing.T) {
	// Arrange: a socket path nothing is listening on.
	client := clientTo(t, filepath.Join(os.TempDir(), "ar-absent.sock"))

	// Act.
	_, err := client.LiveShellRunsAnnouncedBefore(ctx(), 1000)

	// Assert.
	if err == nil || IsRefusal(err) {
		t.Fatalf("error = %v, want a transport error, not a refusal", err)
	}
}
