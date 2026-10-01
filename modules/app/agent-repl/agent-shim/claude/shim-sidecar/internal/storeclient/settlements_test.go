package storeclient

import (
	"os"
	"path/filepath"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestRunSettlementsReturnsTheSettledRunsAnswered(t *testing.T) {
	// Arrange.
	store := &fakeStore{settlements: &storev1.GetRunSettlementsResponse{
		Result: &storev1.GetRunSettlementsResponse_Success{Success: &storev1.GetRunSettlementsSuccess{
			Settled: []*storev1.RunSettlement{{RunId: "call-1", EndedAtMs: 42}},
		}},
	}}
	client := serve(t, store)

	// Act.
	got, err := client.RunSettlements(ctx(), []string{"call-1", "call-2"})

	// Assert.
	if err != nil {
		t.Fatalf("RunSettlements returned %v, want success", err)
	}
	if len(got) != 1 || got[0].GetRunId() != "call-1" || got[0].GetEndedAtMs() != 42 {
		t.Fatalf("RunSettlements = %v, want call-1 ended at 42", got)
	}
	if ids := store.lastSettleReq.GetRunIds(); len(ids) != 2 || ids[0] != "call-1" || ids[1] != "call-2" {
		t.Fatalf("asked %v, want [call-1 call-2]", ids)
	}
}

func TestRunSettlementsFailureArmIsARefusalNamingItsKind(t *testing.T) {
	tests := []struct {
		name      string
		failure   *storev1.GetRunSettlementsFailure
		wantKind  RefusalKind
		wantField string
	}{
		{
			name: "storage failure",
			failure: &storev1.GetRunSettlementsFailure{Detail: "database is locked",
				Kind: &storev1.GetRunSettlementsFailure_StorageFailure{StorageFailure: &storev1.GetRunSettlementsStorageFailure{}}},
			wantKind: RefusalStorageFailure,
		},
		{
			name: "invalid request",
			failure: &storev1.GetRunSettlementsFailure{Detail: "an asked run id is empty",
				Kind: &storev1.GetRunSettlementsFailure_InvalidRequest{InvalidRequest: &storev1.GetRunSettlementsInvalidRequest{Field: "run_ids[0]"}}},
			wantKind:  RefusalInvalidRequest,
			wantField: "run_ids[0]",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			client, lines := serveLogged(t, &fakeStore{settlements: &storev1.GetRunSettlementsResponse{
				Result: &storev1.GetRunSettlementsResponse_Failure{Failure: test.failure},
			}})

			// Act.
			_, err := client.RunSettlements(ctx(), []string{"call-1"})

			// Assert.
			refusal, ok := err.(*RefusalError)
			if !ok || refusal.Kind != test.wantKind || refusal.Field != test.wantField {
				t.Fatalf("RunSettlements error = %v, want a %v refusal naming %q", err, test.wantKind, test.wantField)
			}
			requireOnceIn(t, parseLogLines(t, *lines), "storeclient-run-settlements", "error")
		})
	}
}

func TestRunSettlementsUnsetResultIsAnError(t *testing.T) {
	// Arrange.
	client, lines := serveLogged(t, &fakeStore{settlements: &storev1.GetRunSettlementsResponse{}})

	// Act.
	_, err := client.RunSettlements(ctx(), []string{"call-1"})

	// Assert.
	if err == nil || IsRefusal(err) {
		t.Fatalf("RunSettlements error = %v, want a non-refusal error for an unset result", err)
	}
	requireOnceIn(t, parseLogLines(t, *lines), "storeclient-run-settlements", "error")
}

func TestRunSettlementsTransportErrorIsNotARefusal(t *testing.T) {
	// Arrange: a socket path nothing is listening on.
	client := clientTo(t, filepath.Join(os.TempDir(), "ar-absent.sock"))

	// Act.
	_, err := client.RunSettlements(ctx(), []string{"call-1"})

	// Assert.
	if err == nil {
		t.Fatal("an unreachable store answered successfully")
	}
	if IsRefusal(err) {
		t.Fatalf("a transport failure was reported as a store refusal: %v", err)
	}
}
