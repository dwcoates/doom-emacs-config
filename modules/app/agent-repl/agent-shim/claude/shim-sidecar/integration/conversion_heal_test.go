package integration

// conversion_heal_test.go — SUBJECT: a conversion change heals the store,
// through the REAL store.
//
// The live store holds rows a pre-versioning sidecar wrote: frames with no
// conversion_version, write ids digested without one, cursors with no
// conversion bookkeeping, and among them `prompt:<uuid>` rows the old
// conversion minted for records no person typed (task notifications above
// all). The current sidecar re-reads such a transcript from its start and the
// store retires every row a record no longer converts to — out of the book,
// and withdrawn from every standing watch — while a row the record still
// converts to stays exactly where it was.
//
// THE PRE-VERSIONING STATE IS FABRICATED IN THE DATABASE ITSELF. The store now
// refuses to write it (a file-plane entry must carry its version), so the
// subject lets the current sidecar and store write a transcript, adds the
// prompt the OLD conversion minted for the notification, and then — with the
// store stopped — rewrites every file-plane frame and write id and drops the
// cursor's conversion bookkeeping: exactly the bytes a pre-versioning store
// holds.

import (
	"context"
	"database/sql"
	"io"
	"path/filepath"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
	_ "modernc.org/sqlite"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

const (
	healCwd          = "/Users/dodgecoates/conversion-heal-probe"
	healSession      = "c1c1c1c1-c1c1-4c1c-8c1c-c1c1c1c1c1c1"
	healTypedUUID    = "c1c1c1c1-0000-4000-8000-00000000c0p1"
	healNoticeUUID   = "c1c1c1c1-0000-4000-8000-00000000c0n1"
	healSentinelUUID = "c1c1c1c1-0000-4000-8000-00000000c0s1"
	// healLegacyPrefix marks a write id as digested without a conversion
	// version, which no current write id can collide with.
	healLegacyPrefix = "legacy-"
)

// healTypedPrompt is a user record a person typed, parentless so it is its
// own prompt row.
func healTypedPrompt(uuid, text string) string {
	return `{"type":"user","uuid":"` + uuid + `","sessionId":"` + healSession + `","cwd":"` + healCwd + `","isSidechain":false,"entrypoint":"cli","timestamp":"2026-09-27T12:00:00.000Z",` +
		`"message":{"role":"user","content":[{"type":"text","text":"` + text + `"}]}}`
}

// healNotice is a task notification: the old conversion minted a prompt for
// it, and the current one never does.
func healNotice() string {
	return `{"type":"user","uuid":"` + healNoticeUUID + `","sessionId":"` + healSession + `","cwd":"` + healCwd + `","isSidechain":false,"entrypoint":"cli","origin":{"kind":"task-notification"},` +
		`"timestamp":"2026-09-27T12:00:02.000Z","message":{"role":"user","content":"<task-notification>\n<task-id>bsh1</task-id>\n<status>completed</status>\n</task-notification>"}}`
}

// healFixture is a real store holding one transcript as a pre-versioning
// sidecar left it, and the transcript itself.
type healFixture struct {
	store  *realStore
	file   *growingFile
	tree   *vendorTree
	socket string
	dbPath string
}

// seedPreVersioningStore writes the transcript through the current sidecar,
// adds the notification's legacy prompt row, and rewrites the database as a
// pre-versioning store holds it. The store is running again when it returns;
// no sidecar is.
func seedPreVersioningStore(ctx context.Context, t *testing.T) *healFixture {
	t.Helper()
	fx := &healFixture{
		tree:   newVendorTree(t),
		socket: shortSocketPath(t, "store"),
		dbPath: filepath.Join(t.TempDir(), "store.db"),
	}
	fx.store = startRealStoreAt(t, fx.socket, fx.dbPath)
	slug := cwdSlug(healCwd)
	fx.file = newGrowingFile(t, fx.tree.sessionPath(slug, healSession))
	fx.file.AppendLine(healTypedPrompt(healTypedUUID, "fix the build"))
	noticeOffset := fx.file.AppendLine(healNotice())

	first := startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitCursorAtLeast(ctx, t, fx.store.Client, fx.file.Path(), fx.file.Offset())
	first.Stop()

	writeLegacyNoticePrompt(ctx, t, fx.store.Client, fx.file.Path(), noticeOffset)
	fx.store.Stop()
	rewriteAsPreVersioning(t, fx.dbPath)
	fx.store = startRealStoreAt(t, fx.socket, fx.dbPath)
	return fx
}

// writeLegacyNoticePrompt stores the prompt row the old conversion minted for
// the notification: the current conversion's prompt for a TYPED record with
// the notification's uuid and position, which is what the old one made of it.
func writeLegacyNoticePrompt(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, path string, offset int64) {
	t.Helper()
	conv := convert.New(logging.New(io.Discard, io.Discard).With(logging.Context{Component: "legacy-fixture"}))
	record := decodeRecord(t, healTypedPrompt(healNoticeUUID, "task notification"))
	entries := conv.Line(record, convert.Attribution{
		VendorSessionID: healSession, MainAgentID: healSession, AgentID: healSession,
		Path: path, FileID: fileID(t, path), Offset: offset,
	}, nil)
	var legacy []*storev1.StoreEntry
	for _, entry := range entries {
		if entry.GetUpsertKey() == convert.PromptKey(healNoticeUUID) {
			entry.ConversionVersion = proto.Uint32(convert.ConversionVersion)
			legacy = append(legacy, entry)
		}
	}
	if len(legacy) != 1 {
		t.Fatalf("the legacy fixture minted %d prompt rows for the notification, want one", len(legacy))
	}
	res, err := c.WriteBatch(ctx, connect.NewRequest(&storev1.WriteBatchRequest{
		Producer:   "shim-claude-sidecar",
		WriteClass: &storev1.WriteClass{WriteClass: &storev1.WriteClass_Bulk{Bulk: &storev1.WriteClassBulk{}}},
		Batch:      &storev1.EntryBatch{Entries: legacy},
	}))
	if err != nil {
		t.Fatalf("WriteBatch(legacy prompt): %v", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("the store refused the legacy prompt: %v", res.Msg)
	}
}

// rewriteAsPreVersioning makes a stopped store's database what a
// pre-versioning sidecar left: every file-plane frame without its
// conversion_version, every file-plane write id digested without one, and no
// cursor stating a conversion.
func rewriteAsPreVersioning(t *testing.T, dbPath string) {
	t.Helper()
	db, err := sql.Open("sqlite", dbPath+"?_pragma=synchronous(OFF)")
	if err != nil {
		t.Fatalf("open %s: %v", dbPath, err)
	}
	defer func() {
		if err := db.Close(); err != nil {
			t.Errorf("close %s: %v", dbPath, err)
		}
	}()
	rows, err := db.Query(`SELECT position, frame FROM entry WHERE plane = 2`)
	if err != nil {
		t.Fatalf("reading file-plane rows: %v", err)
	}
	frames := map[int64][]byte{}
	for rows.Next() {
		var position int64
		var frame []byte
		if err := rows.Scan(&position, &frame); err != nil {
			t.Fatalf("scan: %v", err)
		}
		frames[position] = frame
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("rows: %v", err)
	}
	if err := rows.Close(); err != nil {
		t.Fatalf("close rows: %v", err)
	}
	for position, frame := range frames {
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			t.Fatalf("decode row %d: %v", position, err)
		}
		entry.ConversionVersion = nil
		entry.WriteId = healLegacyPrefix + entry.GetWriteId()
		legacy, err := proto.Marshal(entry)
		if err != nil {
			t.Fatalf("encode row %d: %v", position, err)
		}
		if _, err := db.Exec(`UPDATE entry SET frame = ?, write_id = ? WHERE position = ?`, legacy, entry.GetWriteId(), position); err != nil {
			t.Fatalf("rewrite row %d: %v", position, err)
		}
	}
	for _, statement := range []string{
		`UPDATE write_ledger SET write_id = '` + healLegacyPrefix + `' || write_id WHERE write_id IN (SELECT substr(write_id, ` +
			`length('` + healLegacyPrefix + `') + 1) FROM entry WHERE plane = 2)`,
		`DELETE FROM cursor_conversion`,
	} {
		if _, err := db.Exec(statement); err != nil {
			t.Fatalf("%s: %v", statement, err)
		}
	}
}

// promptPointers maps each prompt line's turn to its pointer.
func promptPointers(lines []*storev1.StoreLineAt) map[string]*storev1.StoreItemPointer {
	out := map[string]*storev1.StoreItemPointer{}
	for _, at := range lines {
		if prompt := at.GetLine().GetAgentItem().GetAgentPrompt(); prompt != nil {
			out[prompt.GetId().GetValue()] = at.GetAt()
		}
	}
	return out
}

// awaitHealed waits for the transcript's cursor to reach its end under the
// current conversion, with nothing left to re-derive. The pre-versioning
// cursor already stands at the end, so the offset alone would be satisfied
// before the heal read a byte: the conversion it states is the signal.
func awaitHealed(ctx context.Context, t *testing.T, fx *healFixture) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		cs := cursorByPath(ctx, t, fx.store.Client, fx.file.Path())
		conv := cs.GetConversion()
		if cs.GetOffset() >= fx.file.Offset() && conv.GetCurrent() != nil && conv.GetVersion() == convert.ConversionVersion {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the transcript's cursor never stood at its end under conversion %d; last %v", convert.ConversionVersion, cs)
		case <-tick.C:
		}
	}
}

// watchFrames follows a book and hands back every frame, retired arm
// included.
func watchFrames(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) <-chan *storev1.WatchAgentSessionResponse {
	t.Helper()
	res, err := c.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{Agent: agentID(agent)}))
	if err != nil {
		t.Fatalf("OpenAgentSession(%s): %v", agent, err)
	}
	opened := res.Msg.GetSuccess()
	if opened == nil {
		t.Fatalf("OpenAgentSession(%s) refused: %v", agent, res.Msg)
	}
	stream, err := c.WatchAgentSession(ctx, connect.NewRequest(&storev1.WatchAgentSessionRequest{Watch: opened.GetWatch()}))
	if err != nil {
		t.Fatalf("WatchAgentSession(%s): %v", agent, err)
	}
	out := make(chan *storev1.WatchAgentSessionResponse, 256)
	go func() {
		defer close(out)
		defer func() { _ = stream.Close() }()
		for stream.Receive() {
			select {
			case out <- stream.Msg():
			case <-ctx.Done():
				return
			}
		}
	}()
	return out
}

// framesThroughSentinel collects watch frames until the line of the sentinel
// prompt arrives. The store publishes in write order, so every frame the heal
// published has arrived by then.
func framesThroughSentinel(ctx context.Context, t *testing.T, frames <-chan *storev1.WatchAgentSessionResponse) []*storev1.WatchAgentSessionResponse {
	t.Helper()
	var seen []*storev1.WatchAgentSessionResponse
	for {
		select {
		case frame, open := <-frames:
			if !open {
				t.Fatalf("the watch ended before the sentinel arrived; frames: %v", seen)
			}
			seen = append(seen, frame)
			if frame.GetLine().GetLine().GetAgentItem().GetAgentPrompt().GetId().GetValue() == healSentinelUUID {
				return seen
			}
		case <-ctx.Done():
			t.Fatalf("the sentinel prompt never reached the watch; frames: %v", seen)
		}
	}
}

func TestAHealThroughTheRealStoreTakesTheNotificationsPromptOutOfItsBook(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(ctx, t)
	if _, legacy := promptPointers(bookLines(ctx, t, fx.store.Client, healSession))[healNoticeUUID]; !legacy {
		t.Fatal("the fixture's legacy notification prompt is not in the book to begin with")
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)

	// Assert.
	if _, still := promptPointers(bookLines(ctx, t, fx.store.Client, healSession))[healNoticeUUID]; still {
		t.Fatal("the book still serves the prompt the old conversion minted for a task notification")
	}
}

func TestAHealThroughTheRealStoreLeavesAStillValidPromptWhereItWas(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(ctx, t)
	before := promptPointers(bookLines(ctx, t, fx.store.Client, healSession))[healTypedUUID]

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)

	// Assert.
	after := promptPointers(bookLines(ctx, t, fx.store.Client, healSession))[healTypedUUID]
	if before == nil || !proto.Equal(before, after) {
		t.Fatalf("the typed prompt's pointer moved from %v to %v, want it untouched", before, after)
	}
}

func TestAHealThroughTheRealStoreTellsAStandingWatchOfTheRetirement(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(ctx, t)
	frames := watchFrames(ctx, t, fx.store.Client, healSession)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)
	fx.file.AppendLine(healTypedPrompt(healSentinelUUID, "the sentinel"))
	seen := framesThroughSentinel(ctx, t, frames)

	// Assert.
	var retired bool
	for _, frame := range seen {
		retired = retired || frame.GetRetired().GetLine().GetAgentItem().GetAgentPrompt().GetId().GetValue() == healNoticeUUID
	}
	if !retired {
		t.Fatalf("the watch was never told the notification's prompt retired; frames: %v", seen)
	}
}

func TestAHealThroughTheRealStorePublishesNothingForAnUnchangedRow(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(ctx, t)
	frames := watchFrames(ctx, t, fx.store.Client, healSession)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)
	fx.file.AppendLine(healTypedPrompt(healSentinelUUID, "the sentinel"))
	seen := framesThroughSentinel(ctx, t, frames)

	// Assert: the re-derived typed prompt is a restamp, which no reader sees.
	for _, frame := range seen {
		if frame.GetLine().GetLine().GetAgentItem().GetAgentPrompt().GetId().GetValue() == healTypedUUID {
			t.Fatalf("the heal re-published the unchanged typed prompt; frames: %v", seen)
		}
	}
}

func TestASecondRunAfterAHealReDerivesNothing(t *testing.T) {
	t.Parallel()
	// Arrange: the heal ran to completion and the sidecar stopped.
	ctx, cancel := testContext(t)
	defer cancel()
	fx := seedPreVersioningStore(ctx, t)
	healer := startSidecar(t, defaultSidecarOptions(t, fx.socket, fx.tree))
	awaitHealed(ctx, t, fx)
	healer.Stop()
	opts := defaultSidecarOptions(t, fx.socket, fx.tree)

	// Act.
	startSidecar(t, opts)
	fx.file.AppendLine(healTypedPrompt(healSentinelUUID, "the sentinel"))
	awaitCursorAtLeast(ctx, t, fx.store.Client, fx.file.Path(), fx.file.Offset())

	// Assert.
	for _, rec := range readLog(t, opts.LogPath) {
		if rec.Operation == "conversion-heal" {
			t.Fatalf("the second run re-derived again: %v", rec)
		}
	}
}
