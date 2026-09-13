package classifier

import (
	"context"
	"testing"

	"claude-repld/internal/headless"
)

// TestNewBuildsTheVendorJudge pins that the production constructor hands back
// the headless vendor run — the judge whose seams are the real loader,
// splicer and exec site — rather than the scripted one.
func TestNewBuildsTheVendorJudge(t *testing.T) {
	// Arrange.
	guard := forbiddingGuard(t)

	// Act.
	got, err := New(guard, headless.New(guard, "fake-claude"), "/prompts")

	// Assert.
	if err != nil {
		t.Fatalf("New() error = %v, want nil", err)
	}
	judge, ok := got.(*vendorJudge)
	if !ok {
		t.Fatalf("New() = %T, want *vendorJudge", got)
	}
	if judge.headless.Bin() != "fake-claude" || judge.promptsDir != "/prompts" {
		t.Fatalf("judge = {bin:%q promptsDir:%q}, want the constructor's arguments", judge.headless.Bin(), judge.promptsDir)
	}
}

// TestNewWiresTheProductionRunSeam pins that the judge New returns actually
// carries the exec site: a judge whose run seam were nil would panic on the
// first classification rather than refuse.
func TestNewWiresTheProductionRunSeam(t *testing.T) {
	// Arrange.
	got, err := New(forbiddingGuard(t), headless.New(forbiddingGuard(t), "fake-claude"), "/prompts")
	if err != nil {
		t.Fatalf("New() error = %v, want nil", err)
	}

	// Act.
	judge := got.(*vendorJudge)

	// Assert.
	if judge.load == nil || judge.splice == nil || judge.run == nil {
		t.Fatalf("judge seams = {load:%v splice:%v run:%v}, want all three wired",
			judge.load != nil, judge.splice != nil, judge.run != nil)
	}
	// And the guard is the one it was built with: a forbidden site refuses.
	if _, err := judge.Judge(context.Background(), "running", "an ordinary follow-up"); err == nil {
		t.Fatal("Judge() = nil error, want the guard's refusal")
	}
}
