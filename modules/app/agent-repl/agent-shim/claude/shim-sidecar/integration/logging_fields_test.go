package integration

import (
	"testing"
)

// SUBJECT 22 — THE LOG'S FIELD SET, ITS EXACTLY-ONCE RULE, AND CONVERGENCE.
//
// The sidecar's log is read by the integration loop, which joins its records
// against the store's and the shim's BY KEY. So three things are contract, and
// each has its own subject class below:
//
//  1. FIELD SET. Every production site class carries the correlation keys its
//     class owes (logging-contract.md's vocabulary: producer, agent_id,
//     vendor_session_id, book_agent_id, write_id, upsert_key, position,
//     write_seq, watch_token_hash, rpc, file_id, path, offset, task_id,
//     activity_id, turn_id, reason, attempt, backoff_ms — plus component and
//     store_socket). The keys are DEDICATED, never prose in the message.
//  2. EXACTLY ONCE. An error is logged once by its owning layer. Twice is a
//     reader double-counting one failure; zero is a silent one.
//  3. CONVERGENCE. A FAILED operation settles on ONE record however many
//     cycles run afterwards. A failure restated every poll is the loop the
//     parking and suspension rulings exist to prevent.
//
// Two keys in that vocabulary have no sidecar site and are asserted nowhere:
// `position` and `write_seq` are store-minted and only ever ride records the
// STORE writes, and `watch_token_hash` belongs to a watcher the sidecar is not.
// `request_id` is top-level rather than a context key and is carried only by a
// record answering an inbound rpc; the sidecar serves none, so it emits none —
// and `omitempty` keeps the key absent rather than present-and-empty.

// siteClass is one production logging site and the keys it owes.
type siteClass struct {
	name      string
	operation string
	keys      []string
}

// greenPathSiteClasses enumerates the site classes ONE CLEAN INGEST of the real
// captured session reaches. Every row is grounded in a record that ingest
// actually writes: a row for a branch the green path never takes would assert
// nothing, and this table is checked for PRESENCE as well as for keys, so a site
// that stops logging fails here rather than passing silently.
var greenPathSiteClasses = []siteClass{
	// The store seam. `rpc` is what joins a sidecar record to the store's own
	// record of the same call; `producer` is what says whose rows those were.
	{"cursor recovery rpc", "storeclient-cursors", []string{"component", "store_socket", "rpc"}},
	{"write rpc", "storeclient-write-batch", []string{"component", "store_socket", "rpc", "producer", "path", "file_id", "offset"}},

	// The file plane. `tail-pickup` is the one that reports a READ, so it owes
	// the full (file_id, offset) address. The tailer's own poll and commit
	// records bracket that read and are written before the file has been opened
	// as well as after, so they owe the file and — where there is one — the
	// position, but a file identity they do not have yet is ABSENT rather than
	// invented.
	{"tail poll", "tailer-poll", []string{"component", "path", "offset"}},
	{"tail commit", "tailer-commit", []string{"component", "path"}},
	{"batch pickup", "tail-pickup", []string{"component", "path", "file_id", "offset"}},

	// Per-ENTRY conversion. These sites produced a store row, so they owe the
	// row's key on top of the file position it was read at.
	{"tool call entry", "tool-call", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id", "activity_id", "upsert_key"}},
	{"tool return entry", "tool-return", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id", "activity_id", "upsert_key"}},
	{"thinking entry", "thinking", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id", "activity_id", "upsert_key"}},
	// The hook site narrates what the hook did AND names the row it stored it
	// as. It owes no activity_id: the 2026-09-04 plane-ownership ruling gives
	// the STREAM plane the served hook row, so this plane mints no unit of work
	// here and the record it writes is an unserved item keyed by the vendor
	// record's own uuid.
	{"hook entry", "hook", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id", "upsert_key"}},
	{"injected context entry", "context-injected", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id", "activity_id", "upsert_key"}},

	// Per-LINE conversion. A line is not yet a row, so it owes the book it was
	// read for and the position it was read at, and no key.
	{"converted line", "convert-line", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id"}},
	{"assistant line", "assistant-line", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id"}},
	{"user prompt line", "user-prompt", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id"}},
	{"withheld line", "withhold", []string{"component", "path", "file_id", "offset", "producer", "agent_id", "vendor_session_id"}},

	// Residue is the one conversion site that owes a write_id in the log: it is
	// the row nobody could model, and its write_id is the only way to find the
	// bytes again.
	{"residue row", "residue", []string{"component", "path", "file_id", "offset", "upsert_key", "write_id"}},

	// The spawn spine. A launch names the task, the call that spawned it and
	// the book the subagent is announced under.
	{"launch", "launch", []string{"component", "path", "task_id", "activity_id", "agent_id", "book_agent_id"}},
	{"spawn recorded", "record-spawn", []string{"component", "task_id", "activity_id"}},

	// Lifecycle. These name the dependency and the file, and nothing else is
	// theirs to name.
	{"file watched", "watch", []string{"component", "path"}},
	{"file classified", "discover-classify", []string{"component", "path"}},
	{"cursor recovery", "recover-cursors", []string{"component", "store_socket"}},
	{"process start", "start", []string{"component", "store_socket"}},
}

// TestEverySiteClassCarriesTheKeysItOwes drives one clean ingest and checks the
// whole table against it.
func TestEverySiteClassCarriesTheKeysItOwes(t *testing.T) {
	t.Parallel()
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	for _, class := range greenPathSiteClasses {
		class := class
		t.Run(class.name, func(t *testing.T) {
			got := recordsFor(records, class.operation)
			if len(got) == 0 {
				t.Fatalf("a clean ingest wrote no %q record; the site class is unreachable or was renamed. The log held %v",
					class.operation, operationLevels(records))
			}
			for _, r := range got {
				requireContextKeys(t, r, class.keys...)
			}
		})
	}
}

// TestEveryProducerKeyNamesTheSidecar asserts the producer spelling is the one
// the store's rows are written under, on every record that names one. A record
// naming a different producer would join a reader onto the wrong system's rows.
func TestEveryProducerKeyNamesTheSidecar(t *testing.T) {
	t.Parallel()
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	var seen int
	for _, r := range records {
		producer, ok := r.Context["producer"]
		if !ok {
			continue
		}
		seen++
		if producer != "shim-claude-sidecar" {
			t.Errorf("record %q names producer %v, want the sidecar's own producer string", r.Operation, producer)
		}
	}
	if seen == 0 {
		t.Fatal("no record named a producer at all; the store rows the sidecar writes would be unattributable")
	}
}

// TestNoRecordCarriesARetiredOrEmptyKey asserts PRESENCE, NEVER SENTINELS: a
// key the site does not own is absent, not present as an empty string.
func TestNoRecordCarriesAnEmptyCorrelationKey(t *testing.T) {
	t.Parallel()
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	for _, r := range records {
		for key, value := range r.Context {
			if text, ok := value.(string); ok && text == "" {
				t.Errorf("record %q carries %q as an empty string; an unowned fact is an ABSENT key", r.Operation, key)
			}
		}
	}
}

// TestEveryRowAnnouncementNamesThePositionItWasReadAt asserts the stronger half
// of the field-set contract: a record that names an upsert_key announced a STORE
// ROW, and a row nobody can trace back to the byte it was read from is a row
// nobody can debug. So the key never travels without the file and the offset.
func TestEveryRowAnnouncementNamesThePositionItWasReadAt(t *testing.T) {
	t.Parallel()
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	announcements := logsWithContextKey(records, "upsert_key")
	if len(announcements) == 0 {
		t.Fatal("no record named an upsert_key; the rows the sidecar wrote would be untraceable")
	}
	for _, r := range announcements {
		requireContextKeys(t, r, "path", "file_id", "offset")
	}
}

// ---------------------------------------------------------------------------
// Exactly once, and convergence.
// ---------------------------------------------------------------------------

// TestAnInvalidRequestRefusalIsStatedExactlyOnce asserts the producer-defect
// record is written ONCE for the batch that was refused.
//
// The refusal parks the file, so the refused records are never re-offered — and
// a defect restated every poll would bury every other reader's records in a loop
// that says the same thing forever.
func TestAnInvalidRequestRefusalIsStatedExactlyOnce(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	fake.FailWritesInvalid(1, "batch.entries[0].upsert_key", "entry 0 carries no upsert_key")

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	defects := awaitOperationCount(ctx, t, opts.LogPath, "producer-defect", "error", 1)

	// Assert: the record carries the whole refusal vocabulary.
	requireContextKeys(t, defects[0], "refusal_kind", "refusal_site", "field", "write_ids", "path", "file_id", "offset")
	if got := defects[0].Context["refusal_kind"]; got != "invalid_request" {
		t.Errorf("refusal_kind = %v, want the arm that says a retry cannot help", got)
	}
	// The SITE rides beside the KIND and answers a different question: the kind
	// says whether a retry can help, the site says which call was refused, which
	// is what joins this record to the store's own refusal record.
	if got := defects[0].Context["refusal_site"]; got != "/store.v1.ShimStore/WriteBatch" {
		t.Errorf("refusal_site = %v, want the write call the store refused", got)
	}
	if got := defects[0].Context["field"]; got != "batch.entries[0].upsert_key" {
		t.Errorf("field = %v, want the offending field the store named", got)
	}

	// Assert: it CONVERGES. A SECOND file appearing and reaching the store is the
	// observable proof that further cycles ran; the defect is still stated once.
	other := "71717171-7171-4171-8171-717171717171"
	cwd := "/work/producer-defect-convergence-probe"
	second := newGrowingFile(t, tree.sessionPath(cwdSlug(cwd), other))
	second.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), other, cwd)))
	awaitCursorInBatches(ctx, t, fake, second.Path(), second.Offset())

	after := recordsAt(readLog(t, opts.LogPath), "producer-defect", "error")
	if len(after) != 1 {
		t.Fatalf("the producer defect was stated %d times across later cycles, want exactly once; the log held %v",
			len(after), operationLevels(readLog(t, opts.LogPath)))
	}
}

// TestAStoreOutageStatesOneSuspensionHoweverManyAttemptsFail asserts an outage
// converges on ONE warning.
//
// The recovery ladder retries forever; a warning per attempt would make the
// duration of the outage, rather than the fact of it, the thing that dominates
// the log. Each retry is recorded at VERBOSE instead, carrying the attempt
// ordinal and the delay it armed, so the progress is still filterable.
func TestAStoreOutageStatesOneSuspensionHoweverManyAttemptsFail(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	opts.ExtraEnv = []string{"AGENT_REPL_LOG_LEVEL=debug"}
	fake.FailCursors("the store cannot read its cursors")

	// Act: wait until the ladder has failed SEVERAL times, which is the event
	// that makes "however many attempts" true rather than assumed.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitOperationCount(ctx, t, opts.LogPath, "recover-cursors", "warn", 2)

	// Assert: one suspension, naming the store the file plane stopped for.
	records := readLog(t, opts.LogPath)
	attempts := recordsFor(records, "recover-cursors")
	suspensions := recordsAt(records, "production-suspended", "warn")
	if len(suspensions) != 1 {
		t.Fatalf("the outage was stated %d times across %d failed attempts, want exactly once",
			len(suspensions), len(attempts))
	}
	requireContextKeys(t, suspensions[0], "store_socket", "attempt")

	// Assert: the ladder's own levels. The FIRST refusal is the error an
	// operator must not miss; every attempt after it is the same known outage
	// still running, and is a warning.
	if got := recordsAt(records, "recover-cursors", "error"); len(got) != 1 {
		t.Errorf("the ladder wrote %d error records, want only the outage's first refusal", len(got))
	}
	if got := recordsAt(records, "recover-cursors", "warn"); len(got) != len(attempts)-1 {
		t.Errorf("the ladder wrote %d warning records for %d attempts, want one for every attempt after the first",
			len(got), len(attempts))
	}

	// Assert: every attempt stays filterable by ordinal and by the delay it
	// armed, and none of them is verbose — an outage that only shows up with
	// verbose emission on is an outage nobody sees.
	for _, r := range attempts {
		requireContextKeys(t, r, "attempt", "backoff_ms")
		if r.Verbosity != "normal" {
			t.Errorf("a ladder record was written at verbosity %q: %q", r.Verbosity, r.Message)
		}
	}
}

// TestARefusedCursorReadConvergesOnOneRecordPerLayer asserts a failed operation
// converges: each LAYER that owns a fact about it states that fact ONCE, and no
// layer restates it.
//
// The store client owns "this rpc was refused" and the cycle owns "production is
// now suspended". Those are two different facts, not one failure logged twice:
// the contract's exactly-once rule is PER OWNING LAYER.
//
// The two layers converge differently, and both are asserted here. The CYCLE's
// fact is about the OUTAGE, which happens once however long the ladder runs, so
// it is stated once. The CLIENT's fact is about ONE RPC, and every retry is a
// genuinely new rpc that was genuinely refused — a leaf that suppressed its
// second refusal would be hiding a failure from every caller that does NOT
// retry. So the client owes exactly one record PER ATTEMPT, no more: what the
// rule forbids is the same attempt narrated twice.
func TestARefusedCursorReadConvergesOnOneRecordPerLayer(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	opts.ExtraEnv = []string{"AGENT_REPL_LOG_LEVEL=debug"}
	fake.FailCursors("the store cannot read its cursors")

	// Act: several attempts must have failed, or "it is not restated" is a claim
	// about a ladder that never ran.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitOperationCount(ctx, t, opts.LogPath, "recover-cursors", "warn", 2)

	// Assert. Both counts come from ONE snapshot of the log: reading it twice
	// would compare a retry ladder against itself at two different instants.
	records := readLog(t, opts.LogPath)
	attempts := recordsFor(records, "recover-cursors")
	if got := recordsAt(records, "production-suspended", "warn"); len(got) != 1 {
		t.Errorf("the cycle stated the suspension %d times across %d attempts, want once", len(got), len(attempts))
	}
	// One record per attempt, never two. The snapshot may catch an attempt whose
	// client record has landed and whose cycle record has not, so the client's
	// count is allowed to lead the cycle's by exactly that one in-flight attempt
	// — and by nothing else.
	refusals := recordsAt(records, "storeclient-cursors", "error")
	if len(refusals) < len(attempts) || len(refusals) > len(attempts)+1 {
		t.Errorf("the store client wrote %d refusal records for %d refused attempts, want exactly one each",
			len(refusals), len(attempts))
	}
	// And no OTHER layer joined in. Exactly two layers own a fact about this
	// failure: the client (this rpc was refused) and the cycle's ladder (the
	// outage's first refusal). A third would be the double-counting the
	// exactly-once rule exists to prevent.
	for _, r := range logsAtLevel(records, "error") {
		switch r.Operation {
		case "storeclient-cursors", "recover-cursors":
		default:
			t.Errorf("a third layer restated the refused cursor read: op=%q message=%q", r.Operation, r.Message)
		}
	}
}
