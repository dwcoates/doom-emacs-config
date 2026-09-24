package workspace

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestDescribeLockHolderFailureWordsEachArm(t *testing.T) {
	// Arrange.
	tests := []struct {
		name    string
		failure *conversationv1.LockHolderFailure
		want    string
		wantOK  bool
	}{
		{
			name:    "a spawn failure",
			failure: &conversationv1.LockHolderFailure{How: &conversationv1.LockHolderFailure_SpawnFailed{SpawnFailed: &conversationv1.LockHolderSpawnFailed{OsError: "spawn ENOENT"}}},
			want:    "could not be spawned: spawn ENOENT",
			wantOK:  true,
		},
		{
			name:    "an exit with stderr",
			failure: &conversationv1.LockHolderFailure{How: &conversationv1.LockHolderFailure_Exited{Exited: &conversationv1.LockHolderExited{Code: 1, Stderr: "EACCES"}}},
			want:    "exited with code 1 before taking the lock (EACCES)",
			wantOK:  true,
		},
		{
			name:    "an exit with no stderr",
			failure: &conversationv1.LockHolderFailure{How: &conversationv1.LockHolderFailure_Exited{Exited: &conversationv1.LockHolderExited{Code: 2}}},
			want:    "exited with code 2 before taking the lock",
			wantOK:  true,
		},
		{
			name:    "a signal",
			failure: &conversationv1.LockHolderFailure{How: &conversationv1.LockHolderFailure_Signaled{Signaled: &conversationv1.LockHolderSignaled{Signal: "SIGSEGV"}}},
			want:    "was killed by SIGSEGV before taking the lock",
			wantOK:  true,
		},
		{
			name:    "a wrong line",
			failure: &conversationv1.LockHolderFailure{How: &conversationv1.LockHolderFailure_Misanswered{Misanswered: &conversationv1.LockHolderMisanswered{Line: "ok"}}},
			want:    `answered "ok" instead of "locked" and was killed`,
			wantOK:  true,
		},
		{
			name:    "no answer",
			failure: &conversationv1.LockHolderFailure{How: &conversationv1.LockHolderFailure_Silent{Silent: &conversationv1.LockHolderSilent{TimeoutMs: 5000}}},
			want:    `gave no "locked" answer within 5000 ms and was killed`,
			wantOK:  true,
		},
		{
			name:    "no how at all",
			failure: &conversationv1.LockHolderFailure{},
			want:    "failed without saying how",
			wantOK:  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, ok := describeLockHolderFailure(tt.failure)

			// Assert.
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("describeLockHolderFailure = (%q, %t), want (%q, %t)", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}
