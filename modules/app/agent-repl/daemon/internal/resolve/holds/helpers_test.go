package holds_test

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/wsm"
)

// testWS is the workspace every test in this package addresses.
const testWS = ids.WorkspaceID("ws-holds")

// queuedAt is the instant held prompts are stamped with unless a test needs an
// order, so a conversion assertion never depends on wall-clock.
var queuedAt = time.UnixMilli(1_700_000_000_000)

// newResolver builds a bound resolver plus the surfaces its records land in.
func newResolver(t *testing.T) (holds.Resolver, *dlog.TestSurfaces) {
	t.Helper()
	surfaces := dlog.NewTestSurfaces()
	r, err := holds.New(surfaces)
	if err != nil {
		t.Fatalf("holds.New: %v", err)
	}
	if err := r.SetWorkspaceDir(testWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	return r, surfaces
}

// latest reads the tray the resolver last published, failing when it published
// none.
func latest(t *testing.T, r holds.Resolver) *frontendv1.DaemonHoldTray {
	t.Helper()
	tray, ok := r.Topic(testWS).Latest()
	if !ok {
		t.Fatal("the tray published nothing")
	}
	return tray
}

// subscribe opens a subscription that is torn down with the test.
func subscribe(t *testing.T, r holds.Resolver) <-chan *frontendv1.DaemonHoldTray {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	return r.Topic(testWS).Subscribe(ctx)
}

// said builds a user submission carrying one line of text.
func said(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: text}},
			}},
		},
	}
}

// hold builds a standing hold with the given turn and text.
func hold(turn, text string) wsm.HeldPrompt {
	return wsm.HeldPrompt{
		Workspace: testWS,
		Turn:      ids.TurnID(turn),
		Said:      said(text),
		QueuedAt:  queuedAt,
	}
}

// verdict stamps a classification on a hold.
func verdict(h wsm.HeldPrompt, arm wsm.ClassificationArm, reason string) wsm.HeldPrompt {
	h.Classification = &wsm.Classification{Arm: arm, Reason: reason, At: queuedAt}
	return h
}

// prompts reads the tray's prompt entries, failing on any item that is not one.
func prompts(t *testing.T, tray *frontendv1.DaemonHoldTray) []*frontendv1.HeldPrompt {
	t.Helper()
	var out []*frontendv1.HeldPrompt
	for i, item := range tray.GetItems() {
		p, ok := item.GetItem().(*frontendv1.DaemonHoldItem_Prompt)
		if !ok {
			continue
		}
		if p.Prompt == nil {
			t.Fatalf("item %d carried a nil prompt", i)
		}
		out = append(out, p.Prompt)
	}
	return out
}

// onlyPrompt reads the tray's single prompt entry.
func onlyPrompt(t *testing.T, tray *frontendv1.DaemonHoldTray) *frontendv1.HeldPrompt {
	t.Helper()
	got := prompts(t, tray)
	if len(got) != 1 {
		t.Fatalf("tray carried %d prompts, want exactly 1", len(got))
	}
	return got[0]
}

// hasError reports whether any captured record was an error under operation.
func hasError(records []dlog.Record, operation string) bool {
	for _, rec := range records {
		if rec.Level == "error" && rec.Operation == operation {
			return true
		}
	}
	return false
}
