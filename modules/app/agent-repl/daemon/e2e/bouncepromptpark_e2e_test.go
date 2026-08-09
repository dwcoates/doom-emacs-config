// PROMPTS SUBMITTED WHILE A SHIM IS BOUNCING.
//
// A shim bounce — the stale-build refresh, or any automatic replacement — has a
// window in which the workspace has a session, a registry record and a
// frontend, and no process to deliver a prompt to. What the user types into
// that window is the sharpest possible test of the queue: they cannot see the
// bounce, so as far as they are concerned they typed into a working session.
//
// TODAY a submit that lands during bring-up is honestly NACKED — "no queue by
// design" (e2e_test.go's first test says so in as many words). That is a
// defensible answer for a session that is coming up for the first time. It is
// not a defensible answer for a session the daemon is DELIBERATELY BOUNCING
// under the user's feet: the daemon caused the window, so the daemon owns what
// falls into it.
//
// WHAT MUST BE TRUE. Prompts submitted while a shim is at or awaiting its
// boundary restart PARK DURABLY and are delivered IN ORDER once the replacement
// reports ShimReady. No loss, no reorder, no duplication — which is why this
// test uses TWO prompts: a single-prompt version cannot distinguish "kept" from
// "kept in order", and cannot see a duplicate at all.
//
// THE BOUNCE IS REAL, not simulated: the harness bakes one build identity into
// a genuinely built bundle and tells the daemon the current one is different,
// so the shim is replaced by the production stale-refresh path (staleshim_e2e
// covers that the replacement happens at all).
package e2e

import (
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EPromptsTypedDuringAShimBounceAreDeliveredInOrder covers THE PARK.
func TestE2EPromptsTypedDuringAShimBounceAreDeliveredInOrder(t *testing.T) {
	// Arrange — a session whose shim is being replaced the moment it comes up.
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t, withStaleShimBuild("sha-old", "sha-new"))
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// Act — two prompts typed into the bounce window, in a known order.
	const (
		firstPrompt  = "typed-into-the-bounce-one"
		secondPrompt = "typed-into-the-bounce-two"
	)
	submitPrompt(t, conn, "r-park-1", firstPrompt)
	submitPrompt(t, conn, "r-park-2", secondPrompt)

	// Assert — both acks are acceptances, and both replies arrive, in order and
	// exactly once each.
	//
	// The acks are read on the SAME loop as the replies rather than awaited
	// first: an ack burst and a conversation delta are different producers, and
	// consuming one to look for the other is how a frame gets thrown away.
	want := []string{firstPrompt, secondPrompt}
	var order []string
	seen := map[string]int{}
	acked := map[string]bool{}
	deadline := time.Now().Add(frameTimeout)
	for len(order) < len(want) && time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		for _, requestID := range []string{"r-park-1", "r-park-2"} {
			ack := ackFor(frame, requestID)
			if ack == nil {
				continue
			}
			acked[requestID] = true
			if !ack.GetOk() {
				t.Fatalf("the prompt submitted as %s was NACKED (%q) because its shim was mid-bounce: the daemon caused that window, so a prompt that falls into it is PARKED until the replacement is ready, never refused back at the user",
					requestID, ack.GetError())
			}
		}
		for _, item := range deltaItems(frame, cwd) {
			text := assistantText(item)
			for _, prompt := range want {
				if text == "" || !strings.Contains(text, echoOf(prompt)) {
					continue
				}
				seen[prompt]++
				if seen[prompt] == 1 {
					order = append(order, prompt)
				}
			}
		}
	}
	for _, requestID := range []string{"r-park-1", "r-park-2"} {
		if !acked[requestID] {
			t.Errorf("no CommandAck ever arrived for %s: a parked prompt is still an accepted prompt and must be acknowledged as one", requestID)
		}
	}
	if len(order) != len(want) {
		t.Fatalf("replies arrived for %v before the deadline, want both of %v: prompts submitted while the shim was at its boundary restart must PARK and be delivered once it is ready, never dropped with the process they were typed at",
			order, want)
	}
	for i := range want {
		if order[i] != want[i] {
			t.Fatalf("parked prompts ran in the order %v, want %v: a park preserves submission order", order, want)
		}
	}
	for _, prompt := range want {
		if seen[prompt] != 1 {
			t.Errorf("the parked prompt %q was answered %d times, want exactly 1: a park that re-delivers on the replacement's ShimReady must not also deliver the copy it was holding", prompt, seen[prompt])
		}
	}
}

// TestE2EAPromptParkedAcrossAShimBounceIsVisibleWhileItWaits covers THE
// ACCOUNTING half: a prompt that is being held is DELAYED, and the user is
// told so. A park that is invisible is indistinguishable from a prompt that
// vanished, which is the failure the park exists to prevent.
func TestE2EAPromptParkedAcrossAShimBounceIsVisibleWhileItWaits(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	h := newUDSHarness(t, withStaleShimBuild("sha-old", "sha-new"))
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// Act
	submitPrompt(t, conn, "r-parked-visible", "waiting on the replacement shim")

	// Assert — the workspace's queue accounts for it while it waits.
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueView carrying the prompt parked behind the shim bounce — a held prompt the user cannot see is indistinguishable from one that was dropped": func(frame *frontendv1.FrontendFrame) bool {
			return len(queueViewFor(frame, cwd).GetEntries()) > 0
		},
	})
}
