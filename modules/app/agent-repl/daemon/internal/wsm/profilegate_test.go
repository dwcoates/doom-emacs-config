package wsm

import (
	"context"
	"testing"
	"time"
)

func TestARequestKeepsItsProfileGateInTheQueue(t *testing.T) {
	for _, profile := range []bool{true, false} {
		t.Run(map[bool]string{true: "a profiled gate", false: "an ordinary gate"}[profile], func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, ProfileGate: profile}, time.Unix(100, 0))
			queue, readErr := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || readErr != nil || len(queue) != 1 {
				t.Fatalf("RequestMerge = %v, MergeQueue = %+v, %v; want one request", err, queue, readErr)
			}
			if queue[0].Source.ProfileGate != profile {
				t.Fatalf("ProfileGate = %v, want %v", queue[0].Source.ProfileGate, profile)
			}
		})
	}
}
