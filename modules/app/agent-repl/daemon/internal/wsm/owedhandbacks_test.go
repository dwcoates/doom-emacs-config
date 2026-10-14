package wsm

import (
	"context"
	"testing"
	"time"
)

// owedHandBack is one owed report for merge workspace mergeWS, owed at second at.
func owedHandBack(mergeWS WorkspaceID, at int64) OwedHandBack {
	return OwedHandBack{
		MergeWorkspace: mergeWS, Requester: "ws-requester", MergeName: string(mergeWS) + "-name",
		Branch: "feature", Summary: "it landed as commit abc", CompactAfter: true,
		IdempotencyKey: "key-" + string(mergeWS), OwedAt: time.Unix(at, 0).UTC(),
	}
}

func TestAnOwedHandBackReadsBackWhole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	want := owedHandBack("mws-a", 100)
	if err := s.OweHandBack(ctx, want); err != nil {
		t.Fatalf("OweHandBack: %v", err)
	}

	// Act
	got, err := s.OwedHandBacks(ctx, false)

	// Assert
	if err != nil || len(got) != 1 || got[0] != want {
		t.Fatalf("OwedHandBacks = %+v, %v; want exactly %+v", got, err, want)
	}
}

func TestOwingTheSameMergeTwiceKeepsTheFirstReport(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	first := owedHandBack("mws-a", 100)
	second := first
	second.Summary, second.IdempotencyKey = "another summary", "another-key"
	if err := s.OweHandBack(ctx, first); err != nil {
		t.Fatalf("first OweHandBack: %v", err)
	}

	// Act
	err := s.OweHandBack(ctx, second)

	// Assert
	got, readErr := s.OwedHandBacks(ctx, false)
	if err != nil || readErr != nil || len(got) != 1 || got[0] != first {
		t.Fatalf("after owing twice: err %v, read %+v, %v; want only the first", err, got, readErr)
	}
}

func TestADeployCoversEveryReportOwedBeforeIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	for _, owed := range []OwedHandBack{owedHandBack("mws-a", 100), owedHandBack("mws-b", 200)} {
		if err := s.OweHandBack(ctx, owed); err != nil {
			t.Fatalf("OweHandBack(%s): %v", owed.MergeWorkspace, err)
		}
	}

	// Act
	covered, err := s.CoverOwedHandBacks(ctx)

	// Assert
	got, readErr := s.OwedHandBacks(ctx, true)
	if err != nil || covered != 2 || readErr != nil || len(got) != 2 {
		t.Fatalf("CoverOwedHandBacks = %d, %v; covered rows %+v, %v; want both covered", covered, err, got, readErr)
	}
}

func TestAReportOwedAfterTheDeployCommittedStaysUncovered(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	if err := s.OweHandBack(ctx, owedHandBack("mws-a", 100)); err != nil {
		t.Fatalf("OweHandBack: %v", err)
	}
	if _, err := s.CoverOwedHandBacks(ctx); err != nil {
		t.Fatalf("CoverOwedHandBacks: %v", err)
	}

	// Act
	if err := s.OweHandBack(ctx, owedHandBack("mws-b", 200)); err != nil {
		t.Fatalf("OweHandBack after the cover: %v", err)
	}

	// Assert
	covered, err := s.OwedHandBacks(ctx, true)
	if err != nil || len(covered) != 1 || covered[0].MergeWorkspace != "mws-a" {
		t.Fatalf("covered = %+v, %v; want only the report owed before the deploy", covered, err)
	}
	all, err := s.OwedHandBacks(ctx, false)
	if err != nil || len(all) != 2 || all[1].Covered {
		t.Fatalf("every owed report = %+v, %v; want the later one uncovered", all, err)
	}
}

func TestACoverWithNothingOwedCoversNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	covered, err := s.CoverOwedHandBacks(context.Background())

	// Assert
	if err != nil || covered != 0 {
		t.Fatalf("CoverOwedHandBacks = %d, %v; want 0", covered, err)
	}
}

func TestOwedHandBacksAreOldestFirst(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	for _, owed := range []OwedHandBack{owedHandBack("mws-late", 300), owedHandBack("mws-early", 100)} {
		if err := s.OweHandBack(ctx, owed); err != nil {
			t.Fatalf("OweHandBack(%s): %v", owed.MergeWorkspace, err)
		}
	}

	// Act
	got, err := s.OwedHandBacks(ctx, false)

	// Assert
	if err != nil || len(got) != 2 || got[0].MergeWorkspace != "mws-early" || got[1].MergeWorkspace != "mws-late" {
		t.Fatalf("OwedHandBacks = %+v, %v; want the earlier landing first", got, err)
	}
}

func TestPayingAHandBackDeletesItOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	if err := s.OweHandBack(ctx, owedHandBack("mws-a", 100)); err != nil {
		t.Fatalf("OweHandBack: %v", err)
	}

	// Act
	first, err1 := s.PayHandBack(ctx, "mws-a")
	second, err2 := s.PayHandBack(ctx, "mws-a")

	// Assert
	left, readErr := s.OwedHandBacks(ctx, false)
	if err1 != nil || err2 != nil || !first || second || readErr != nil || len(left) != 0 {
		t.Fatalf("pays = (%v, %v), (%v, %v); left %+v, %v; want the first to pay and nothing left", first, err1, second, err2, left, readErr)
	}
}
