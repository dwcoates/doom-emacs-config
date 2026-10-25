package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/worktreereap"
)

func TestReapWorktreesAnswersTheSweepsReport(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Worktrees.report = worktreereap.Report{
		DryRun:          true,
		Repositories:    2,
		Worktrees:       9,
		Removed:         []string{"/w/landed"},
		Expired:         []string{"/w/old"},
		Forced:          []string{"/w/closed"},
		Leftovers:       []string{"/w/leftover"},
		BranchesDeleted: 1,
		Pruned:          1,
		Kept:            map[string]int{"recently_active": 3, "open_workspace": 2},
		Failures:        1,
	}
	reason := func(r agentreplv1.ReapWorktreesReason, path string) *agentreplv1.ReapWorktreesRemoval {
		return &agentreplv1.ReapWorktreesRemoval{Path: path, Reason: r}
	}
	want := &agentreplv1.ReapWorktreesSuccess{
		DryRun:       true,
		Repositories: 2,
		Worktrees:    9,
		Removed: []*agentreplv1.ReapWorktreesRemoval{
			reason(agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_LANDED, "/w/landed"),
			reason(agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_EXPIRED, "/w/old"),
			reason(agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_FORCED, "/w/closed"),
			reason(agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_LEFTOVER, "/w/leftover"),
		},
		BranchesDeleted: 1,
		Pruned:          1,
		Kept: []*agentreplv1.ReapWorktreesKept{
			{Reason: agentreplv1.ReapWorktreesKeepReason_REAP_WORKTREES_KEEP_REASON_OPEN_WORKSPACE, Count: 2},
			{Reason: agentreplv1.ReapWorktreesKeepReason_REAP_WORKTREES_KEEP_REASON_RECENTLY_ACTIVE, Count: 3},
		},
		Failures: 1,
	}

	// Act.
	resp, err := h.Client.ReapWorktrees(context.Background(), connect.NewRequest(&agentreplv1.ReapWorktreesRequest{DryRun: true, Force: true}))

	// Assert.
	if err != nil || !proto.Equal(resp.Msg.GetSuccess(), want) {
		t.Fatalf("ReapWorktrees = (%v, %v), want %v", resp, err, want)
	}
	if len(h.Worktrees.asked) != 1 || h.Worktrees.asked[0] != (worktreereap.Options{DryRun: true, Force: true}) {
		t.Fatalf("the reaper was asked %+v, want one forced dry run", h.Worktrees.asked)
	}
}

func TestReapWorktreesAnswersEachRefusal(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want *agentreplv1.ReapWorktreesError
	}{
		{"a sweep already running", worktreereap.ErrSweepRunning, &agentreplv1.ReapWorktreesError{Cause: &agentreplv1.ReapWorktreesError_AlreadyRunning{AlreadyRunning: &agentreplv1.ReapWorktreesAlreadyRunning{}}}},
		{"another daemon sweeping", worktreereap.ErrHeldElsewhere, &agentreplv1.ReapWorktreesError{Cause: &agentreplv1.ReapWorktreesError_HeldElsewhere{HeldElsewhere: &agentreplv1.ReapWorktreesHeldElsewhere{}}}},
		{"a sweep that ended early", errors.New("db gone"), &agentreplv1.ReapWorktreesError{Cause: &agentreplv1.ReapWorktreesError_Failed{Failed: &agentreplv1.ReapWorktreesFailed{Detail: "db gone"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Worktrees.err = tt.err

			// Act.
			resp, err := h.Client.ReapWorktrees(context.Background(), connect.NewRequest(&agentreplv1.ReapWorktreesRequest{}))

			// Assert.
			if err != nil || !proto.Equal(resp.Msg.GetError(), tt.want) {
				t.Fatalf("ReapWorktrees = (%v, %v), want %v", resp, err, tt.want)
			}
		})
	}
}

func TestEveryReaperKeepReasonIsDrawnByTheContract(t *testing.T) {
	for _, reason := range worktreereap.KeepReasons {
		t.Run(reason, func(t *testing.T) {
			// Act.
			wire := reapWorktreesKeepReason(reason)

			// Assert.
			if wire == agentreplv1.ReapWorktreesKeepReason_REAP_WORKTREES_KEEP_REASON_UNSPECIFIED {
				t.Fatalf("keep reason %q drew UNSPECIFIED", reason)
			}
		})
	}
}

func TestTheContractDrawsNoKeepReasonTheReaperLacks(t *testing.T) {
	// Assert: the enum's values past UNSPECIFIED are exactly the reaper's.
	if got, want := len(agentreplv1.ReapWorktreesKeepReason_name)-1, len(worktreereap.KeepReasons); got != want {
		t.Fatalf("the contract draws %d keep reasons, the reaper has %d", got, want)
	}
}

func TestAKeepReasonTheContractDoesNotDrawPanics(t *testing.T) {
	// Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("an undrawn keep reason did not panic")
		}
	}()

	// Act.
	reapWorktreesKeepReason("never_a_reason")
}
