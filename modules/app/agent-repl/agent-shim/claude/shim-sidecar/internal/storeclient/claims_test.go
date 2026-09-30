package storeclient

import (
	"os"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

func TestShellRunClaimsReturnsTheClaimsAnswered(t *testing.T) {
	// Arrange.
	want := &storev1.ShellRunClaimed{
		Claim: &storev1.ShellRunClaim{VendorTaskId: "b1", Run: &conversationv1.AgentActivityId{Value: "call-1"}},
		Owner: &conversationv1.AgentId{Value: "agent-1"},
	}
	store := &fakeStore{claims: &storev1.GetShellRunClaimsResponse{
		Result: &storev1.GetShellRunClaimsResponse_Success{Success: &storev1.GetShellRunClaimsSuccess{
			Claims: []*storev1.ShellRunClaimed{want},
		}},
	}}
	client := serve(t, store)

	// Act.
	got, err := client.ShellRunClaims(ctx(), []string{"b1", "b2"})

	// Assert.
	if err != nil {
		t.Fatalf("ShellRunClaims returned %v, want success", err)
	}
	if len(got) != 1 || got[0].GetClaim().GetRun().GetValue() != "call-1" || got[0].GetOwner().GetValue() != "agent-1" {
		t.Fatalf("ShellRunClaims = %v, want b1 -> call-1 owned by agent-1", got)
	}
	if ids := store.lastClaimsReq.GetVendorTaskIds(); len(ids) != 2 || ids[0] != "b1" || ids[1] != "b2" {
		t.Fatalf("asked %v, want [b1 b2]", ids)
	}
}

func TestShellRunClaimsFailureArmIsARefusalNamingItsKind(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{claims: &storev1.GetShellRunClaimsResponse{
		Result: &storev1.GetShellRunClaimsResponse_Failure{Failure: &storev1.GetShellRunClaimsFailure{
			Detail: "database is locked",
			Kind:   &storev1.GetShellRunClaimsFailure_StorageFailure{StorageFailure: &storev1.GetShellRunClaimsStorageFailure{}},
		}},
	}})

	// Act.
	_, err := client.ShellRunClaims(ctx(), []string{"b1"})

	// Assert.
	refusal, ok := err.(*RefusalError)
	if !ok || refusal.Kind != RefusalStorageFailure {
		t.Fatalf("ShellRunClaims error = %v, want a storage_failure refusal", err)
	}
}

func TestShellRunClaimsUnsetResultIsAnError(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{claims: &storev1.GetShellRunClaimsResponse{}})

	// Act.
	_, err := client.ShellRunClaims(ctx(), []string{"b1"})

	// Assert.
	if err == nil || IsRefusal(err) {
		t.Fatalf("ShellRunClaims error = %v, want a non-refusal error for an unset result", err)
	}
}

func TestShellRunClaimsTransportErrorIsNotARefusal(t *testing.T) {
	// Arrange: a socket path nothing is listening on.
	client := clientTo(t, filepath.Join(os.TempDir(), "ar-absent.sock"))

	// Act.
	_, err := client.ShellRunClaims(ctx(), []string{"b1"})

	// Assert.
	if err == nil {
		t.Fatal("an unreachable store answered successfully")
	}
	if IsRefusal(err) {
		t.Fatalf("a transport failure was reported as a store refusal: %v", err)
	}
}
