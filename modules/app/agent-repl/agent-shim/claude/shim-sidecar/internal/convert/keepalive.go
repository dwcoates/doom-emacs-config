package convert

// keepalive.go — WHICH TRANSCRIPT RECORDS A KEEP-ALIVE TURN PRODUCED, so none
// of them is ever stored.
//
// NOTHING OF A KEEP-ALIVE IS STORED, ON EITHER PLANE (2026-09-23). The shim's
// keep-alive exists to keep the vendor's prompt cache warm; its prompt, its
// answer and everything between have no reader anywhere. Nothing needs them to
// work — the send, the answer and the rewind anchor are all the shim's
// in-memory state, and a resume reads the vendor's own file — so the shim
// writes nothing for one and this file makes the sidecar write nothing either.
// Every record is still read and converted in full (the discovery mandate);
// only its entries are withheld, one DEBUG `keepalive-skip` record each.
//
// THE CLASSIFICATION IS THE TRANSCRIPT'S OWN STRUCTURE, NOT ARRIVAL ORDER. The
// vendor's file is a TREE, not a list: every record names its parent in
// `parentUuid`, and concurrent writers interleave. A real prompt delivered after
// a rewind parents onto the rewind anchor while the keep-alive's late records
// still parent onto the keep-alive; a compaction's summary query runs beside a
// keep-alive; a task notification's own turn starts while one is pending. The
// old rule — one remembered bool, set by the marker and cleared by the next
// prompt IN FILE ORDER — therefore served the keep-alive's "." whenever
// another prompt landed first, and lost the bit entirely across a restart.
// The rule is now:
//
//   - A USER record carrying a `promptId` belongs to that prompt's turn. It is
//     the keep-alive's when that promptId is a keep-alive prompt's, and a
//     record that OPENS a prompt is one when its first text block begins with
//     KeepaliveMarker (a meta record or a compaction summary never
//     does). A tool result, a folded meta record and the keep-alive prompt
//     itself all carry the prompt's own promptId; a DIFFERENT promptId is a
//     different turn, whatever its parent — which is exactly how a task
//     notification or a peer message chained onto a keep-alive's last record
//     stays served.
//   - A USER record with no `promptId` (an older CLI) is a prompt by its shape:
//     prose that is not meta and not a summary is classified by the marker,
//     and anything else follows its parent.
//   - EVERY OTHER RECORD FOLLOWS ITS PARENT: an assistant block, a system
//     record or an attachment is the keep-alive's exactly when the record it
//     names in `parentUuid` is. A record with no parent (a compaction boundary,
//     the CLI's unchained bookkeeping) belongs to no keep-alive.
//
// NEVER FROM THE TEXT OF A REPLY. The model's "." is evidence of nothing; only
// the prompt's marker and the vendor's own links decide.
//
// A RESTART LOSES NOTHING. The sets are rebuilt from the file itself: a reader
// that resumes past byte 0 is handed the file's prefix first (tail.Primer and
// SeedKeepalive), classified by this same rule, so a keep-alive whose prompt
// predates the cursor still owns the records written after it.
//
// WHAT IT HOLDS. The uuids of keep-alive records and the promptIds of
// keep-alive prompts this file has shown — a handful per keep-alive, and a
// keep-alive runs about once an hour. Nothing else about the file is kept.

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
)

// KeepaliveMarker opens a prompt whose turn is never stored.
//
// The literal must be at the very start of the prompt's first text block: the
// same bytes anywhere else are ordinary prose, and a person quoting the marker
// must not have their own turn withheld.
const KeepaliveMarker = "<!--agent-repl:keepalive-->"

// keepaliveProbeNeedle is what a line must contain to be a keep-alive prompt at
// all. It is the marker WITHOUT its angle brackets, so a writer that escapes
// `<` and `>` in JSON strings still matches: the prefilter may only ever admit
// more lines than the rule does, never fewer.
var keepaliveProbeNeedle = []byte("agent-repl:keepalive")

// keepaliveFacts is everything the rule reads from one record.
type keepaliveFacts struct {
	kind     string
	uuid     string
	parent   string
	promptID string
	// prompt is true for a user record that OPENS a turn by its shape: prose
	// that is neither harness bookkeeping (isMeta) nor a compaction summary.
	prompt bool
	// marked is true when that prompt's first text block begins with the marker.
	marked bool
}

// keepaliveFactsOf reads the rule's inputs off a decoded record. It is the ONE
// extractor: the live path and the prefix seed both call it, so the two can
// never classify the same line differently.
func keepaliveFactsOf(record map[string]any) keepaliveFacts {
	facts := keepaliveFacts{
		kind:     str(record["type"]),
		uuid:     str(record["uuid"]),
		parent:   str(record["parentUuid"]),
		promptID: str(record["promptId"]),
	}
	if facts.kind != "user" || boolean(record["isMeta"]) || boolean(record["isCompactSummary"]) {
		return facts
	}
	text, prose := promptProse(obj(record["message"]))
	facts.prompt = prose
	facts.marked = prose && strings.HasPrefix(text, KeepaliveMarker)
	return facts
}

// promptProse returns a user message's first text and whether it carries prose
// at all: a bare string, or an array holding at least one text block. A tool
// result carrier holds none.
func promptProse(message map[string]any) (string, bool) {
	switch content := message["content"].(type) {
	case string:
		return content, content != ""
	case []any:
		for _, raw := range content {
			block := obj(raw)
			if block != nil && str(block["type"]) == "text" {
				return str(block["text"]), true
			}
		}
	}
	return "", false
}

// keepaliveScope is the per-file memory the rule needs.
type keepaliveScope struct {
	records map[string]struct{}
	prompts map[string]struct{}
}

func newKeepaliveScope() keepaliveScope {
	return keepaliveScope{records: map[string]struct{}{}, prompts: map[string]struct{}{}}
}

// empty reports that no keep-alive has been seen, so only a line naming the
// marker can be one.
func (s keepaliveScope) empty() bool { return len(s.records) == 0 && len(s.prompts) == 0 }

// classify answers whether one record is the keep-alive's, remembering it when
// it is, so its own children are recognized in turn. Records arrive in file
// order, and a parent is always written before its child.
func (s keepaliveScope) classify(f keepaliveFacts) bool {
	var keepalive bool
	switch {
	case f.kind == "user" && f.promptID != "":
		if _, known := s.prompts[f.promptID]; known {
			keepalive = true
		} else if f.marked {
			keepalive = true
			s.prompts[f.promptID] = struct{}{}
		}
	case f.kind == "user" && f.prompt:
		keepalive = f.marked
	case f.parent != "":
		_, keepalive = s.records[f.parent]
	}
	if keepalive && f.uuid != "" {
		s.records[f.uuid] = struct{}{}
	}
	return keepalive
}

// seedProbe is the part of a prefix line the rule reads. `message` is decoded
// only for a user record, because an assistant message is the bulk of a
// transcript and the rule never looks inside one.
type seedProbe struct {
	Type             any             `json:"type"`
	UUID             any             `json:"uuid"`
	ParentUUID       any             `json:"parentUuid"`
	PromptID         any             `json:"promptId"`
	IsMeta           any             `json:"isMeta"`
	IsCompactSummary any             `json:"isCompactSummary"`
	Message          json.RawMessage `json:"message"`
}

// SeedKeepalive classifies the part of the file this converter will never be
// handed — the bytes before the offset it joins at — so a keep-alive whose
// prompt predates a restart still owns the records written after it.
//
// It must run BEFORE the first Line, and only an I/O failure is an error: a
// line that does not decode is unparsed residue on the live path and never
// reaches the rule there either, so it is skipped here too. The caller states
// the returned tally, because only the caller knows which file it read.
func (c *Converter) SeedKeepalive(prefix io.Reader) (KeepaliveSeed, error) {
	var seed KeepaliveSeed
	if c.joined {
		return seed, fmt.Errorf("convert: SeedKeepalive after the converter joined the file at offset %d; the prefix would be classified after records that depend on it", c.joinedOffset)
	}
	reader := bufio.NewReaderSize(prefix, 1<<20)
	for {
		line, err := reader.ReadBytes('\n')
		if len(line) > 0 {
			seed.Lines++
			if c.seedLine(line) {
				seed.KeepaliveRecords++
			}
		}
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return seed, fmt.Errorf("convert: reading the prefix for the keep-alive seed after %d line(s): %w", seed.Lines, err)
		}
	}
	seed.KeepalivePrompts = len(c.keepaliveScope.prompts)
	return seed, nil
}

// KeepaliveSeed is what one prefix seed read.
type KeepaliveSeed struct {
	// Lines is how many lines the prefix held.
	Lines int
	// KeepaliveRecords is how many of them a keep-alive turn produced.
	KeepaliveRecords int
	// KeepalivePrompts is how many keep-alive prompts the file has shown.
	KeepalivePrompts int
}

// seedLine classifies one prefix line, reporting whether it was a keep-alive's.
func (c *Converter) seedLine(line []byte) bool {
	trimmed := bytes.TrimSpace(line)
	if len(trimmed) == 0 {
		return false
	}
	// Until a keep-alive has been seen, only its own prompt can be one.
	if c.keepaliveScope.empty() && !bytes.Contains(trimmed, keepaliveProbeNeedle) {
		return false
	}
	facts, ok := seedFacts(trimmed)
	if !ok {
		return false
	}
	return c.keepaliveScope.classify(facts)
}

// seedFacts reads the rule's inputs off one raw line, or reports that the line
// does not decode. It goes through keepaliveFactsOf, the live path's own
// extractor, so the only thing it adds is the cheaper decode.
func seedFacts(line []byte) (keepaliveFacts, bool) {
	var probe seedProbe
	if err := json.Unmarshal(line, &probe); err != nil {
		return keepaliveFacts{}, false
	}
	record := map[string]any{
		"type":             probe.Type,
		"uuid":             probe.UUID,
		"parentUuid":       probe.ParentUUID,
		"promptId":         probe.PromptID,
		"isMeta":           probe.IsMeta,
		"isCompactSummary": probe.IsCompactSummary,
	}
	if str(probe.Type) == "user" && len(probe.Message) > 0 {
		// A message that is not an object is left out, which is exactly what
		// the live path's obj() makes of it: a record with no prose.
		var message map[string]any
		if json.Unmarshal(probe.Message, &message) == nil {
			record["message"] = message
		}
	}
	return keepaliveFactsOf(record), true
}
