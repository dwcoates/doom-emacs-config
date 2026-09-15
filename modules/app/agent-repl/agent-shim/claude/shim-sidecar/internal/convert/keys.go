package convert

// keys.go — THE UPSERT KEY SPACE, in one file.
//
// The store never interprets an upsert key: it holds one row per key and a
// write supersedes that row WHOLE. So the key space is the producer's whole
// statement of "what is one thing", and the shim must mint the IDENTICAL key
// for the same unit observed on the stream plane. That is why every spelling
// lives here rather than at its call site — two producers cannot agree on a
// convention that is written down in twenty places.
//
// WHERE THE TWO PRODUCERS CANNOT AGREE, ONE OF THEM OWNS THE ROW. A hook is the
// grounded case (ruling 2026-09-04): the vendor hands the planes DISJOINT
// identity material — a `hook_id` on the stream, a `toolUseID` in the transcript
// attachment, and differing record uuids — so no key in this space can name one
// firing on both planes. The STREAM plane therefore owns the served hook row
// under `activity:<hook_id>`, exactly as R15 gives it the one served prompt row,
// and this reader keys its hook attachments into the RESIDUE space by the
// record's own uuid (ResidueKey) as unserved items. There is deliberately no
// HookKey here: minting one would re-create the second, unjoinable row.
//
// AN ASK IS THE SECOND SUCH ROW, for the opposite reason: both planes CAN name
// it (`question:<tool_use_id>`), and that is exactly why only one may write it.
// The stream plane gates the ask and receives the answer as a repeated `chosen`;
// the transcript carries only the vendor's comma-joined rendering of it, which
// question.proto's retired tag 4 says cannot be split back. So the ask is
// STREAM-OWNED (streamowned.go) and there is deliberately no QuestionKey here:
// minting one would let a lossy file-plane frame supersede the true one.

// ActivityKey names one unit of work for its whole life: a tool call by the
// vendor's tool_use_id, a text or thinking block by message id + block index.
func ActivityKey(id string) string { return "activity:" + id }

// ContextInjectedUnitID names an injected-context unit — a memory file or a
// skills injection — SCOPED TO THE BOOK it was injected into, so the upsert key
// deterministically implies its book and a re-ingest never moves the row.
//
// WHY THE BOOK IS IN THE KEY. The vendor pulls context in with no tool call and
// copies the SAME attachment record — uuid and all — into every sidechain
// transcript that inherits it, so one record uuid recurs under several books
// (the main agent's and each subagent's). Keyed by the uuid alone the identical
// key named different books across two files, and re-converting the second file
// tried to MOVE the row's book, which the store rightly refuses
// (upsert_changes_identity) — dropping the skills-context write. Scoping the
// unit id by its book gives each book its own row, which is the truth: the
// injection happened in each of those agents' contexts. Both `agent` and the
// record uuid are properties of the transcript, so the key is stable across
// re-conversions.
func ContextInjectedUnitID(kind, agent, recordUUID string) string {
	return "context:" + kind + ":" + agent + ":" + recordUUID
}

// TerminalKey names one agent's stream terminal, which is per RECORD rather
// than per agent: a transcript can carry several terminals for one agent over a
// session's life and none of them supersedes another.
func TerminalKey(agent, recordUUID string) string { return "terminal:" + agent + ":" + recordUUID }

// Bash row keys — ONE ROW PER SPOOL-DERIVED WRITE, never one row superseded.
//
// WHY NOT ONE KEY FOR THE RUN. The store holds one row per upsert key and a
// write supersedes that row WHOLE, so a single `bash:<run>` key would leave the
// run holding only its most recent delta: every earlier chunk of output would be
// erased by the next one. store.v1 WatchBashRun exists to REPLAY a run's rows in
// write order — the start, each delta, then the terminal — which is only
// possible if each of those writes is its own row.
//
// THE DELTA'S KEY IS ITS from_offset, which is the delta's identity: the same
// bytes re-read after a restart mint the same key and supersede their own row
// rather than appending a second copy of themselves. The terminal has a fixed
// key because a run has exactly one, however many times it is restated (an EXIT
// marker re-read, a LOST sweep re-concluding).

// BashStartKey names a run's opening row. The sidecar does not normally mint one
// — a spool exists only after the launch the STREAM plane announced — but the
// key belongs to this space so a producer that does mint one agrees with the
// replay order.
func BashStartKey(run string) string { return "bash:" + run + ":start" }

// BashDeltaKey names one delta of a run by the file offset its bytes start at.
func BashDeltaKey(run string, fromOffset int64) string {
	return "bash:" + run + ":" + itoa(int(fromOffset))
}

// BashTerminalKey names a run's single terminal row.
func BashTerminalKey(run string) string { return "bash:" + run + ":terminal" }

// ResidueKey names one unconvertible record: BY THE VENDOR'S OWN UUID where the
// record has one, and by its file coordinates where it does not.
//
// THE UUID IS WHAT MAKES THE TWO PLANES COLLAPSE. The shim and the sidecar see
// the same vendor record and either may store it as residue; keyed by the
// vendor's uuid both writes land on ONE row, and the second supersedes the first
// instead of standing beside it as a duplicate nobody can reconcile. A digest of
// the file coordinates could never do that — the stream plane has no file
// position to digest.
//
// A record with NO uuid (an unparsed line, a spool's raw bytes) has nothing the
// other plane could agree on, so it is keyed by where it lives: `residue:file:`
// keeps that space visibly separate from the uuid space, so no path can ever
// collide with a uuid.
func ResidueKey(at Attribution) string {
	if at.RecordUUID != "" {
		return "residue:" + at.RecordUUID
	}
	return "residue:file:" + at.Path + ":" + itoa(int(at.Offset))
}

// SessionKey names a session-scoped fact by its arm and the record that carried
// it — a context cut, a mid-turn api error.
func SessionKey(arm, recordUUID string) string { return "session:" + arm + ":" + recordUUID }

// PromptKey names one served prompt row by its turn id, IN THE SAME SPELLING THE
// STREAM PLANE MINTS.
//
// THE SHIM OWNS THIS KEY SPACE. `agent-shim/claude/shim/src/store/keys.ts`
// spells a stream-plane prompt `prompt:<turn.value>`; a file-plane prompt this
// reader emits (an ADOPTED external transcript's, whose turn id is derived from
// the record uuid) uses the identical spelling so the two planes would collapse
// onto ONE row if they ever named the same turn. They do not for an adopted
// prompt — agent-repl's own prompts are still withheld here and drawn live —
// but the key stays in the one space so a producer that does mint both agrees.
func PromptKey(turn string) string { return "prompt:" + turn }

// BlockActivityID is the unit identity of a content block that has no vendor
// call id of its own: the response's message id and the block's 0-based index
// within message.content.
//
// THE INDEX IS LOAD-BEARING. An assistant message holding three tool calls plus
// prose becomes four units, and collapsing them onto the message id would make
// the last one written the only one stored.
func BlockActivityID(messageID string, blockIndex int) string {
	return messageID + ":" + itoa(blockIndex)
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	negative := n < 0
	if negative {
		n = -n
	}
	var digits [20]byte
	i := len(digits)
	for n > 0 {
		i--
		digits[i] = byte('0' + n%10)
		n /= 10
	}
	if negative {
		i--
		digits[i] = '-'
	}
	return string(digits[i:])
}
