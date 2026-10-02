// restart_test.go — SUBJECT 12: what survives a restart, and what must not.
//
// The database survives; the process's memory does not, and the token registry
// lives in memory ON PURPOSE. That split is the contract: a caller that comes
// back after a store restart re-opens, and gets a page it can trust.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"syscall"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// TestPagesAndCursorsSurviveASigtermRestart.
func TestPagesAndCursorsSurviveASigtermRestart(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	shim := streamProducer(cli)
	cursor := cursorState("16777232:424242", "/transcripts/live.jsonl", 65536, []byte("tail"))
	sidecar.writeWithCursor(ctx, t, cursor,
		sidecar.agentEntry("w-restart-1", "u-restart-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))),
	)
	shim.write(ctx, t,
		shim.agentEntry("w-restart-2", "u-restart-2", frameLine(agentID("main"), responseFrame("main", "act-2", "L2"))),
	)

	// Act: a real SIGTERM mid-session, then a new process on the same database.
	store.signal(syscall.SIGTERM)
	if err := store.awaitExit(); err != nil {
		t.Fatalf("SIGTERM was not an orderly exit: %v", err)
	}
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	revived := store.client()

	page := openSession(after, t, revived, "main", nil)
	assertTexts(t, "the book after a restart", pageTexts(page.GetPage()), []string{"L2", "L1"})

	got := sidecarCursors(after, t, revived, nil)
	if len(got) != 1 || got[0].GetOffset() != cursor.GetOffset() {
		t.Fatalf("the cursor did not survive the restart: %v", got)
	}
	store.assertNoErrorRecords()
}

// TestWatchTokensDoNotSurviveARestart: the registry is in memory by design, so
// a token minted by the dead process buys nothing from the live one.
func TestWatchTokensDoNotSurviveARestart(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-token-1", "u-token-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))),
	)
	opened := openSession(ctx, t, store.client(), "main", nil)
	staleToken := opened.GetWatch()

	// Act.
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	stream := watchStream(after, t, store.client(), staleToken)
	defer testclose.OrFail(t, stream)
	assertWatchRefused(t, stream)
}

// TestReopeningAfterARestartRecoversTheTail: the whole point of tokens dying
// is that the recovery is the ordinary re-open, and it works.
func TestReopeningAfterARestartRecoversTheTail(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-reopen-1", "u-reopen-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))),
	)
	before := openSession(ctx, t, store.client(), "main", nil)
	highWater := &storev1.StoreItemPointer{Value: pagePointers(before.GetPage())[0]}

	// Act.
	store.restart()
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	revived := store.client()
	reopened := openSession(after, t, revived, "main", highWater)
	stream := watchStream(after, t, revived, reopened.GetWatch())
	defer testclose.OrFail(t, stream)

	revivedShim := streamProducer(revived)
	revivedShim.write(after, t,
		revivedShim.agentEntry("w-reopen-2", "u-reopen-2", frameLine(agentID("main"), responseFrame("main", "act-2", "L2"))),
	)

	// Assert: nothing already held is repainted, and the tail resumes.
	assertTexts(t, "the catch-up page after a restart", pageTexts(reopened.GetPage()), nil)
	// AN EMPTY CATCH-UP PAGE IS `floor`, NOT `more`. The caller asked for what
	// it does not have yet and there is nothing, so the boundary says the walk
	// is over: the caller is current, and nothing older is owed below its mark.
	// `more` would hand it a continuation pointer to a page that can only ever
	// come back empty, which is a repaint loop dressed as pagination.
	assertPageFloor(t, reopened.GetPage())
	assertTexts(t, "the tail after a restart", receivedTexts(receiveLines(t, stream, 1)), []string{"L2"})
	store.assertNoErrorRecords()
}
