// heal_test.go — SUBJECT: the file plane's re-derivation retires a line the
// current conversion no longer produces, through the real store.
//
// A sidecar whose conversion advanced re-reads a transcript and names, for each
// record, the rows that record no longer converts to. The store takes each such
// line out of its book: no page serves it again, and a standing watch is told
// on the `retired` arm so a reader that drew it withdraws it.
package integration

import (
	"context"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// healedVersion is the conversion version the re-read runs under, one past
// the version the fixtures were written at.
const healedVersion = testConversionVersion + 1

// writeHealBatch writes one file-plane batch at `offset` under healedVersion,
// retiring the named rows.
func writeHealBatch(ctx context.Context, t *testing.T, sidecar *producer, offset int64, retire ...string) {
	t.Helper()
	retirements := make([]*storev1.StoreRetirement, 0, len(retire))
	for _, key := range retire {
		retirements = append(retirements, &storev1.StoreRetirement{UpsertKey: key, ConversionVersion: healedVersion})
	}
	cursor := cursorState("12:34", "/t/a.jsonl", offset, nil)
	cursor.Conversion = &storev1.CursorConversion{
		Version: healedVersion,
		State:   &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}},
	}
	resp, err := sidecar.attempt(ctx, &storev1.EntryBatch{CursorAdvance: cursor, Retirements: retirements})
	if err != nil {
		t.Fatalf("WriteBatch transport error: %v", err)
	}
	if resp.GetSuccess() == nil {
		t.Fatalf("WriteBatch refused the heal batch: %v", resp)
	}
}

func TestAStandingWatchIsToldOfARetiredLine(t *testing.T) {
	// Arrange: the old conversion minted a task notification as a prompt.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	sidecar.writeWithCursor(ctx, t, cursorState("12:34", "/t/a.jsonl", 100, nil),
		sidecar.agentEntry("w-notify", "prompt:u-notify",
			promptLine(agentID("main"), promptFact("u-notify", "main", "<task-notification>"))))
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())

	// Act
	writeHealBatch(ctx, t, sidecar, 200, "prompt:u-notify")

	// Assert
	got := receiveLines(t, stream, 1)
	if !got[0].retired || got[0].text != "prompt:<task-notification>" {
		t.Fatalf("watch frame = %+v, want the retirement of the notification prompt", got[0])
	}
	store.assertNoErrorRecords()
}

func TestARetiredLineIsServedByNoPage(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	sidecar.writeWithCursor(ctx, t, cursorState("12:34", "/t/a.jsonl", 100, nil),
		sidecar.agentEntry("w-notify", "prompt:u-notify",
			promptLine(agentID("main"), promptFact("u-notify", "main", "<task-notification>"))),
		sidecar.agentEntry("w-typed", "prompt:u-typed",
			promptLine(agentID("main"), promptFact("u-typed", "main", "a real prompt"))))

	// Act
	writeHealBatch(ctx, t, sidecar, 200, "prompt:u-notify")

	// Assert
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "the healed book", pageTexts(page.GetPage()), []string{"prompt:a real prompt"})
	store.assertNoErrorRecords()
}
