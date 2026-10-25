package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/storagesweep"
)

func TestPruneStorageAnswersTheSweepsReport(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Storage.report = storagesweep.Report{
		DryRun: true,
		Removed: []storagesweep.Removal{
			{Path: "/s/shim/aaaa", Reason: storagesweep.ReasonNoWorkspace, Bytes: 10},
			{Path: "/s/shim/bbbb/backups", Reason: storagesweep.ReasonClosedBackups, Bytes: 5},
			{Path: "/c/profiles/x", Reason: storagesweep.ReasonAged, Bytes: 1},
			{Path: "/s/shim/cccc", Reason: storagesweep.ReasonLanded, Bytes: 2},
		},
		Kept:     map[storagesweep.KeepReason]int{storagesweep.KeepYoung: 3, storagesweep.KeepOpenWorkspace: 2},
		Failures: 1,
	}
	want := &agentreplv1.PruneStorageSuccess{
		DryRun: true,
		Removed: []*agentreplv1.PruneStorageRemoval{
			{Path: "/s/shim/aaaa", Reason: agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_NO_WORKSPACE, Bytes: 10},
			{Path: "/s/shim/bbbb/backups", Reason: agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_CLOSED_BACKUPS, Bytes: 5},
			{Path: "/c/profiles/x", Reason: agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_AGED, Bytes: 1},
			{Path: "/s/shim/cccc", Reason: agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_LANDED, Bytes: 2},
		},
		Bytes: 18,
		Kept: []*agentreplv1.PruneStorageKept{
			{Reason: agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_OPEN_WORKSPACE, Count: 2},
			{Reason: agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_YOUNG, Count: 3},
		},
		Failures: 1,
	}

	// Act.
	resp, err := h.Client.PruneStorage(context.Background(), connect.NewRequest(&agentreplv1.PruneStorageRequest{DryRun: true}))

	// Assert.
	if err != nil || !proto.Equal(resp.Msg.GetSuccess(), want) {
		t.Fatalf("PruneStorage = (%v, %v), want %v", resp, err, want)
	}
	if len(h.Storage.asked) != 1 || !h.Storage.asked[0] {
		t.Fatalf("the sweep was asked %v, want one dry run", h.Storage.asked)
	}
}

func TestPruneStorageAnswersEachRefusal(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want *agentreplv1.PruneStorageError
	}{
		{"a sweep already running", storagesweep.ErrSweepRunning, &agentreplv1.PruneStorageError{Cause: &agentreplv1.PruneStorageError_AlreadyRunning{AlreadyRunning: &agentreplv1.PruneStorageAlreadyRunning{}}}},
		{"another daemon sweeping", storagesweep.ErrHeldElsewhere, &agentreplv1.PruneStorageError{Cause: &agentreplv1.PruneStorageError_HeldElsewhere{HeldElsewhere: &agentreplv1.PruneStorageHeldElsewhere{}}}},
		{"a sweep that ended early", errors.New("db gone"), &agentreplv1.PruneStorageError{Cause: &agentreplv1.PruneStorageError_Failed{Failed: &agentreplv1.PruneStorageFailed{Detail: "db gone"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Storage.err = tt.err

			// Act.
			resp, err := h.Client.PruneStorage(context.Background(), connect.NewRequest(&agentreplv1.PruneStorageRequest{}))

			// Assert.
			if err != nil || !proto.Equal(resp.Msg.GetError(), tt.want) {
				t.Fatalf("PruneStorage = (%v, %v), want %v", resp, err, tt.want)
			}
		})
	}
}

func TestEverySweepReasonHasAWireValue(t *testing.T) {
	// A reason the contract does not draw panics; every one the sweep can
	// give is drawn, each as its own value.
	reasons := []storagesweep.Reason{storagesweep.ReasonNoWorkspace, storagesweep.ReasonLanded, storagesweep.ReasonClosedBackups, storagesweep.ReasonAged}
	keeps := []storagesweep.KeepReason{
		storagesweep.KeepOpenWorkspace, storagesweep.KeepRecentlyClosed, storagesweep.KeepNotADirectory,
		storagesweep.KeepNoBackups, storagesweep.KeepYoung, storagesweep.KeepKeptByDirectory,
	}
	seen := map[int32]bool{}
	for _, r := range reasons {
		seen[int32(pruneStorageReason(r))] = true
	}
	seenKeeps := map[int32]bool{}
	for _, k := range keeps {
		seenKeeps[int32(pruneStorageKeepReason(k))] = true
	}
	if len(seen) != len(reasons) || len(seenKeeps) != len(keeps) || seen[0] || seenKeeps[0] {
		t.Fatalf("reasons %v keeps %v, want each its own specified value", seen, seenKeeps)
	}
}

func TestAnUndrawnSweepReasonPanics(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("pruneStorageReason accepted a reason the contract does not draw")
		}
	}()
	pruneStorageReason("invented")
}
