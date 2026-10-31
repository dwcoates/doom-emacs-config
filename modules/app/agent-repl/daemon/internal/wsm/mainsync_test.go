package wsm

import (
	"context"
	"testing"
	"time"
)

func TestMainSyncToldRoundTripsAndIsReplaced(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := testWorkspace(t, s).Repo
	ctx := context.Background()
	_, foundBefore, errBefore := s.MainSyncTold(ctx, repo)
	first := MainSyncTold{UpstreamSHA: "aaa", Cause: "diverged", At: instant}
	second := MainSyncTold{UpstreamSHA: "bbb", Cause: "local_changes", At: instant.Add(time.Minute)}

	// Act
	errFirst := s.RecordMainSyncTold(ctx, repo, first)
	errSecond := s.RecordMainSyncTold(ctx, repo, second)
	got, found, err := s.MainSyncTold(ctx, repo)

	// Assert
	if errBefore != nil || foundBefore {
		t.Fatalf("before = %v, %v; want none told", foundBefore, errBefore)
	}
	if errFirst != nil || errSecond != nil || err != nil || !found || got != second {
		t.Fatalf("MainSyncTold = %+v, %v, %v; want the second", got, found, err)
	}
}
