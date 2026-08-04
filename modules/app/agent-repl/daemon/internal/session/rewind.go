package session

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// rewind.go — TRUNCATING A VENDOR TRANSCRIPT, as pure filesystem-and-JSON
// arithmetic.
//
// The CLI has no "resume at an earlier point" control, so a conversation is
// rewound by producing a TRUNCATED COPY of its transcript under a new vendor
// session id and resuming that. The retained prefix is byte-identical in
// message content to what the vendor already received, which is the whole
// point: the next prompt lands on the prompt cache the dropped turns kept warm
// rather than re-ingesting the conversation.
//
// NOTHING IS EVER SYNTHESIZED. Every retained record is the original record,
// with exactly two fields rewritten — the per-record `sessionId`, which must
// name the new transcript, and the tail `last-prompt` marker's `leafUuid`,
// which must point at a record that still exists. `message` is carried as raw
// JSON and never re-encoded, so no marshaling difference can perturb the bytes
// the cache is keyed on.
//
// THE COPY IS NON-DESTRUCTIVE. The source transcript is read and left exactly
// as it was, so a crash anywhere in this file costs nothing: the registry still
// names the old uuid, and the next real prompt that needs the keep-alive turns
// gone runs the rewind again from the beginning. Nothing in this file is the
// point of no return — the caller's registry flip is (sessioncontroller/
// rewind.go), and that flip carries the lineage the respawn replays from, so a
// crash after it is recovered by the next bring-up rather than by this file.

// nonChainTypes are the record types that carry no conversation node — no
// uuid/parentUuid participation — and are therefore skipped by the chain
// verifier and by the leaf search.
var nonChainTypes = map[string]bool{
	"queue-operation": true,
	"last-prompt":     true,
	"mode":            true,
	"summary":         true,
}

// ErrRewindUnsafe reports a transcript the cut cannot be taken on safely. It is
// a REFUSAL, never a partial rewind: the caller submits the prompt WITHOUT
// rewinding, which leaves keep-alive turns in the conversation. That is the
// correct trade — a visible ping in the history is a cosmetic problem, and a
// mis-cut transcript is a corrupted conversation.
var ErrRewindUnsafe = errors.New("session: the transcript cannot be rewound safely")

// ErrNoRewindNeeded reports a transcript whose tail holds no droppable turns.
var ErrNoRewindNeeded = errors.New("session: the transcript tail holds no turns to drop")

// Record is one transcript line, decoded only as far as the rewind needs. The
// unrecognized remainder is carried as raw JSON so a re-encode is lossless.
type Record struct {
	fields map[string]json.RawMessage
}

// str reads a string-valued field, or "" when it is absent or not a string.
func (r Record) str(key string) string {
	raw, ok := r.fields[key]
	if !ok {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return ""
	}
	return s
}

// boolField reads a bool-valued field, or false when absent or not a bool.
func (r Record) boolField(key string) bool {
	raw, ok := r.fields[key]
	if !ok {
		return false
	}
	var b bool
	if err := json.Unmarshal(raw, &b); err != nil {
		return false
	}
	return b
}

// Type is the record's `type` discriminator.
func (r Record) Type() string { return r.str("type") }

// UUID is the record's own conversation-node id, empty for a non-chain record.
func (r Record) UUID() string { return r.str("uuid") }

// IsSidechain reports the `isSidechain` flag a subagent's records carry.
func (r Record) IsSidechain() bool { return r.boolField("isSidechain") }

// UserText reports the plain-string user message this record carries, and
// whether it carries one at all.
//
// THE STRING CHECK IS THE WHOLE POINT, and it is the known trap in this format:
// TOOL RESULTS ARE ALSO type:"user". The CLI writes every tool result back into
// the transcript as a user record whose `message.content` is an ARRAY of
// tool_result blocks. A turn-boundary detector that accepted any type:"user"
// record would therefore find a "turn boundary" in the middle of a tool-heavy
// assistant turn and cut the conversation in half.
//
// A real typed prompt is the only user record whose content is a plain string,
// so requiring that is what makes a boundary a boundary.
func (r Record) UserText() (string, bool) {
	if r.Type() != "user" || r.IsSidechain() {
		return "", false
	}
	raw, ok := r.fields["message"]
	if !ok {
		return "", false
	}
	var msg struct {
		Content json.RawMessage `json:"content"`
	}
	if err := json.Unmarshal(raw, &msg); err != nil {
		return "", false
	}
	var text string
	if err := json.Unmarshal(msg.Content, &text); err != nil {
		// An array (tool results) or an object — not a typed prompt.
		return "", false
	}
	return text, true
}

// LoadTranscript parses a transcript JSONL file, preserving record order.
//
// A malformed line is an ERROR rather than a skipped record. The cut is decided
// by counting turn boundaries, so a line silently dropped could move the
// boundary and truncate real conversation.
func LoadTranscript(path string) ([]Record, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("session: open transcript %s: %w", path, err)
	}
	defer f.Close()

	var records []Record
	scanner := bufio.NewScanner(f)
	// Transcript lines carry whole assistant messages and can be very large;
	// the default 64KiB token limit would fail on ordinary content.
	scanner.Buffer(make([]byte, 0, 1<<20), 64<<20)
	for line := 1; scanner.Scan(); line++ {
		raw := bytes.TrimSpace(scanner.Bytes())
		if len(raw) == 0 {
			continue
		}
		fields := map[string]json.RawMessage{}
		if err := json.Unmarshal(raw, &fields); err != nil {
			return nil, fmt.Errorf("session: %s:%d: malformed transcript line: %w", path, line, err)
		}
		records = append(records, Record{fields: fields})
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("session: read transcript %s: %w", path, err)
	}
	if len(records) == 0 {
		return nil, fmt.Errorf("session: transcript %s is empty", path)
	}
	return records, nil
}

// RewindPlan is a decided cut, before anything is written.
type RewindPlan struct {
	// KeepThrough is the index of the LAST record retained, inclusive.
	KeepThrough int
	// RetainedLeafUUID is the uuid of the deepest retained conversation node —
	// the record the rewound conversation continues from.
	RetainedLeafUUID string
	// DroppedTexts are the user texts of the dropped turns, in order. The
	// caller matches them back to the turn ids it submitted.
	DroppedTexts []string
}

// PlanRewind decides where to cut a transcript so that every TRAILING turn
// whose prompt text is in dropText is discarded.
//
// dropText is a set rather than a predicate because the daemon knows exactly
// one droppable text — the keep-alive ping's — and matching on the literal is
// what keeps the cut decidable from the transcript alone, with no daemon state
// the file does not carry.
//
// THE CUT IS ALWAYS AT A TURN BOUNDARY, never mid-turn: it lands immediately
// before the first droppable user record of the trailing run, so the last
// retained turn keeps its assistant response and every attachment that
// followed it.
//
// A TRAILING PARTIAL TURN NEEDS NO SPECIAL CASE. An interrupted ping — a user
// record with no closing assistant response — is dropped by exactly the same
// cut, because the cut is decided from the boundary in front of it rather than
// from what follows it.
func PlanRewind(records []Record, dropText map[string]bool) (RewindPlan, error) {
	// Index every real user turn: type "user", not a sidechain, and carrying a
	// plain-string message (the tool_result trap, see Record.UserText).
	type userTurn struct {
		index int
		text  string
	}
	var turns []userTurn
	for i, rec := range records {
		if text, ok := rec.UserText(); ok {
			turns = append(turns, userTurn{index: i, text: text})
		}
	}
	if len(turns) == 0 {
		return RewindPlan{}, fmt.Errorf("%w: no real user turns found", ErrRewindUnsafe)
	}

	// Walk back over the TRAILING run of droppable turns.
	firstDropped := len(turns)
	for firstDropped > 0 && dropText[strings.TrimSpace(turns[firstDropped-1].text)] {
		firstDropped--
	}
	if firstDropped == len(turns) {
		return RewindPlan{}, ErrNoRewindNeeded
	}
	if firstDropped == 0 {
		// EVERY turn is droppable, so there is no real turn to continue from.
		// Refused rather than cut to nothing: a transcript with no conversation
		// in it is not a rewind, it is a deletion.
		return RewindPlan{}, fmt.Errorf("%w: every turn in the transcript is droppable, so there is no real turn to resume from", ErrRewindUnsafe)
	}

	cutBefore := turns[firstDropped].index
	keepThrough := cutBefore - 1
	// Trailing queue-operation records belong to the dropped turn's submission,
	// not to the retained one, so they go with it.
	for keepThrough > turns[firstDropped-1].index && records[keepThrough].Type() == "queue-operation" {
		keepThrough--
	}
	if keepThrough <= turns[firstDropped-1].index {
		return RewindPlan{}, fmt.Errorf("%w: the last real turn has no assistant response to retain", ErrRewindUnsafe)
	}

	// SIDECHAIN AND SUMMARY RECORDS IN THE DROPPED REGION REFUSE THE REWIND.
	// A sidechain is a subagent's own conversation branch and a summary is a
	// compaction's product; either interleaved with the turns being dropped
	// means the region is not the simple trailing run of pings this cut assumes,
	// and cutting anyway could orphan a branch the retained prefix still points
	// into. Correctness over cleanliness: the caller submits without rewinding.
	for i := keepThrough + 1; i < len(records); i++ {
		if records[i].IsSidechain() {
			return RewindPlan{}, fmt.Errorf("%w: a sidechain record at index %d interleaves the region being dropped", ErrRewindUnsafe, i)
		}
		if records[i].Type() == "summary" {
			return RewindPlan{}, fmt.Errorf("%w: a summary record at index %d interleaves the region being dropped", ErrRewindUnsafe, i)
		}
	}

	leaf, err := leafUUID(records[:keepThrough+1])
	if err != nil {
		return RewindPlan{}, err
	}
	plan := RewindPlan{KeepThrough: keepThrough, RetainedLeafUUID: leaf}
	for _, t := range turns[firstDropped:] {
		plan.DroppedTexts = append(plan.DroppedTexts, t.text)
	}
	return plan, nil
}

// leafUUID is the uuid of the deepest chain-bearing retained record — the node
// the resumed conversation hangs its next turn from.
func leafUUID(kept []Record) (string, error) {
	for i := len(kept) - 1; i >= 0; i-- {
		if nonChainTypes[kept[i].Type()] {
			continue
		}
		if uuid := kept[i].UUID(); uuid != "" {
			return uuid, nil
		}
	}
	return "", fmt.Errorf("%w: the retained prefix has no chain-bearing record", ErrRewindUnsafe)
}

// VerifyChain checks that every retained record's parentUuid resolves to an
// EARLIER retained record, or is null.
//
// This is the cut's own proof, and it runs before anything is written. A
// dangling parent means the truncation removed a record something retained
// still points at, which the CLI would resume into and fail on — so the rewind
// refuses rather than producing a transcript that is wrong in a way only the
// vendor will discover.
func VerifyChain(kept []Record) error {
	seen := map[string]bool{}
	for i, rec := range kept {
		if nonChainTypes[rec.Type()] {
			continue
		}
		if raw, ok := rec.fields["parentUuid"]; ok && !bytes.Equal(bytes.TrimSpace(raw), []byte("null")) {
			var parent string
			if err := json.Unmarshal(raw, &parent); err == nil && parent != "" && !seen[parent] {
				return fmt.Errorf("%w: record %d (uuid %s) has a dangling parentUuid %s",
					ErrRewindUnsafe, i, rec.UUID(), parent)
			}
		}
		if uuid := rec.UUID(); uuid != "" {
			seen[uuid] = true
		}
	}
	return nil
}

// WriteRewound writes the truncated copy to destDir as <newSessionID>.jsonl and
// returns its path.
//
// It writes to a temporary file and RENAMES it into place, so a crash mid-write
// cannot leave a partial transcript under a name the CLI would try to resume.
func WriteRewound(records []Record, plan RewindPlan, newSessionID, destDir string) (string, error) {
	kept := make([]Record, 0, plan.KeepThrough+1)
	for _, rec := range records[:plan.KeepThrough+1] {
		clone := Record{fields: make(map[string]json.RawMessage, len(rec.fields))}
		for k, v := range rec.fields {
			clone.fields[k] = v
		}
		// The per-record session id must name the NEW transcript: the CLI
		// validates it against the file it loaded.
		if _, ok := clone.fields["sessionId"]; ok {
			encoded, err := json.Marshal(newSessionID)
			if err != nil {
				return "", fmt.Errorf("session: encode session id: %w", err)
			}
			clone.fields["sessionId"] = encoded
		}
		kept = append(kept, clone)
	}
	// The tail `last-prompt` marker is what the CLI's resume reads to find the
	// conversation's head. Left pointing at a dropped node it would resume into
	// nothing, so it is repointed at the retained leaf.
	for i := len(kept) - 1; i >= 0; i-- {
		if kept[i].Type() != "last-prompt" {
			continue
		}
		encoded, err := json.Marshal(plan.RetainedLeafUUID)
		if err != nil {
			return "", fmt.Errorf("session: encode retained leaf uuid: %w", err)
		}
		kept[i].fields["leafUuid"] = encoded
		break
	}
	if err := VerifyChain(kept); err != nil {
		return "", err
	}

	if err := os.MkdirAll(destDir, 0o755); err != nil {
		return "", fmt.Errorf("session: create transcript dir %s: %w", destDir, err)
	}
	dest := filepath.Join(destDir, newSessionID+".jsonl")
	tmp, err := os.CreateTemp(destDir, "."+newSessionID+".*.jsonl")
	if err != nil {
		return "", fmt.Errorf("session: create temporary rewound transcript in %s: %w", destDir, err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName)

	w := bufio.NewWriter(tmp)
	for i, rec := range kept {
		encoded, err := json.Marshal(rec.fields)
		if err != nil {
			tmp.Close()
			return "", fmt.Errorf("session: encode rewound record %d: %w", i, err)
		}
		if _, err := w.Write(append(encoded, '\n')); err != nil {
			tmp.Close()
			return "", fmt.Errorf("session: write rewound record %d: %w", i, err)
		}
	}
	if err := w.Flush(); err != nil {
		tmp.Close()
		return "", fmt.Errorf("session: flush rewound transcript: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return "", fmt.Errorf("session: close rewound transcript: %w", err)
	}
	if err := os.Rename(tmpName, dest); err != nil {
		return "", fmt.Errorf("session: install rewound transcript at %s: %w", dest, err)
	}
	return dest, nil
}
