// THE REBASED CLIENT'S RESYNC, over the real processes: after a vendor session
// rotation the webapp discards the seq state it accumulated in the retired
// space and asks the daemon for everything FROM ZERO (the client-side rebase).
//
// That request is the one the production failure card came out of. The daemon
// had reset its durable cursors and left the RETAINED RING standing, so the
// resync's store re-pull was ceilinged at a retired-space seq — `stop_at=1122`
// against a space that had reached 12 — and the re-pull itself was cut short by
// the rotation's OWN deliberate shim-link bounce. The two together produced the
// exact pair the user saw: an empty feed, and a red card about it.
//
// So these tests assert BOTH halves at once on the real stack: the clear
// renders as ConversationItem arm 32 on a from-zero resync, and no failure card
// is pushed while it happens.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, readFrame, writeCmd, frameTimeout), clearcompact's
// liveSession / deltaItems / isClear / sidecarClearEvent / clearDedupKey,
// interrupt_e2e_test.go's awaitAll / assistantText / echoOf,
// rotation_e2e_test.go's rotateSession, and rotationresync_e2e_test.go's
// replayItemsFrom.
package e2e

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// rebasedMark is what a client that rebased across a rotation asks with. Zero
// is not a placeholder: the webapp throws its retired-space seq state away and
// asks for the new conversation from its beginning (045a79d8).
const rebasedMark = 0

// failureCard names a pushed system-failure item for the workspace, or "" when
// the frame carries none. It is awaitAll's rejection predicate: the whole point
// of the rotation-safe re-pull is that a frontend crossing a rotation is never
// handed an alarm instead of its history.
func failureCard(frame *frontendv1.FrontendFrame, workspace string) string {
	for _, item := range deltaItems(frame, workspace) {
		if f := item.GetSystemFailure(); f != nil {
			return "a system-failure card was pushed across the rotation: type=" +
				f.GetErrorType() + " detail=" + f.GetSourceDetail()
		}
	}
	return ""
}

// TestE2ERebasedResyncAfterARotationRendersTheClear covers THE DEFECT ITSELF.
// The sidecar files its ContextCleared under the ROTATED uuid, so the clear
// lives at a low seq in the new space. A client asking from zero must be served
// it — before this the request was answered by a re-pull bounded in the retired
// space, which delivered nothing at all.
func TestE2ERebasedResyncAfterARotationRendersTheClear(t *testing.T) {
	// Arrange — see rotation_e2e_test.go for why the tempdir precedes the
	// harness, and for why the clear is injected rather than produced.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, store := liveSession(t, h, cwd)
	rot := rotateSession(t, h, conn, id, cwd)
	const lineUUID = "e2e-rebased-clear-1"
	store.write(sidecarClearEvent(rot.next, lineUUID))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the clear's live push": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
					return true
				}
			}
			return false
		},
	})

	// Act — the rebased client asks for the whole new conversation.
	items := replayItemsFrom(t, conn, cwd, "r-rebased", rebasedMark)

	// Assert — arm 32, replayed, exactly once.
	var clears int
	for _, item := range items {
		if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
			clears++
		}
	}
	if clears != 1 {
		t.Fatalf("replayed %d ContextCleared items for %s, want 1 — a from-zero resync after a rotation must be served the clear", clears, clearDedupKey(lineUUID))
	}
}

// TestE2ERebasedResyncReplaysNoRetiredSpaceItem covers THE RING THE ROTATION
// USED TO LEAVE STANDING, on the real stack.
//
// The warmup turn ran under the PREVIOUS uuid, so its reply belongs to the seq
// space the rotation retired: the resubscribe asks the new space only, and
// nothing in the new space refers to it. While the ring went unpurged the
// daemon still held it, and a from-zero resync replayed it back to a client
// that had just discarded exactly that conversation — with its retired seqs
// still setting the ceilings every later replay was bounded by.
func TestE2ERebasedResyncReplaysNoRetiredSpaceItem(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, _ := liveSession(t, h, cwd)
	rotateSession(t, h, conn, id, cwd)

	// Act
	items := replayItemsFrom(t, conn, cwd, "r-rebased-purged", rebasedMark)

	// Assert
	for _, item := range items {
		if strings.Contains(assistantText(item), echoOf("warmup")) {
			t.Fatalf("the from-zero resync replayed the warmup turn's reply (uuid %s) — that item belongs to the RETIRED seq space and the rotation must have dropped it", item.GetUuid())
		}
	}
}

// TestE2ERebasedResyncPushesNoFailureCard covers THE ALARM. The rotation
// bounces the shim link deliberately, and a re-pull crossing that bounce used
// to come back as a truncation — a red card in a feed the rotation had just
// emptied. The re-pull is re-armed across the reattach instead, so the client
// sees history or nothing, never an alarm about the daemon's own re-handshake.
func TestE2ERebasedResyncPushesNoFailureCard(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, store := liveSession(t, h, cwd)
	rot := rotateSession(t, h, conn, id, cwd)
	const lineUUID = "e2e-rebased-clear-2"
	store.write(sidecarClearEvent(rot.next, lineUUID))

	// Act — the from-zero resync, and a turn after it so the connection is
	// still being watched for frames once the replay has settled.
	replayItemsFrom(t, conn, cwd, "r-rebased-nocard", rebasedMark)
	writeCmd(t, conn, `{"requestId":"r-after-rebase","submitPrompt":{"text":"after-the-rebase"}}`)

	// Assert
	awaitAll(t, conn,
		func(frame *frontendv1.FrontendFrame) string { return failureCard(frame, cwd) },
		map[string]func(*frontendv1.FrontendFrame) bool{
			"the post-rebase turn's reply": func(frame *frontendv1.FrontendFrame) bool {
				for _, item := range deltaItems(frame, cwd) {
					if strings.Contains(assistantText(item), echoOf("after-the-rebase")) {
						return true
					}
				}
				return false
			},
		})
}
