package tail

import (
	"bytes"
	"encoding/json"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

// DefaultRewindWindow bounds the boot rewind's backward scan. One turn of a
// transcript is far smaller than this; the bound exists so the scan costs a
// constant, known amount however large the file has grown.
const DefaultRewindWindow = 4 << 20

// RewindToTurnStart moves the RESTORED cursor back to the first record of the
// in-progress turn, and is called exactly ONCE per file per boot.
//
// WHY A RESTART REWINDS AT ALL. The cursor is exactly-once for BYTES, but a
// converter's joins are in-memory: a tool result whose call was converted
// before the restart has no open call to settle, and the turn it belongs to is
// half-converted. Re-reading the in-progress turn re-warms those joins. It is
// free to do so because every record mints a DETERMINISTIC write_id from its
// source coordinates, so the re-emitted records are absorbed by the store as
// the same success arm rather than duplicated.
//
// IT IS ONE BOUNDED BACKWARD SCAN, NEVER A PER-RECORD SEARCH: at most
// `window` bytes ending at the committed offset are read once, and the LAST
// record satisfying isTurnStart within them is the rewind target. When the
// window holds no turn start the cursor is left exactly where the store put it
// — reading from an arbitrary older position would be worse than not rewinding.
//
// THE STORE'S CURSOR ROW IS NOT REWRITTEN. Only this reader's in-memory
// position moves; the next successful batch advances the durable cursor
// normally.
func (t *Tailer) RewindToTurnStart(window int64, isTurnStart func(map[string]any) bool) bool {
	bound := t.log.With(logging.Context{Operation: "boot-rewind", Path: t.path, FileID: t.fileID, Offset: logging.Off(t.offset)})
	if isTurnStart == nil {
		panic("tail: RewindToTurnStart requires a turn-start predicate")
	}
	if t.offset <= 0 {
		bound.LogVerbose("no rewind: the file is already read from its start")
		return false
	}
	if window <= 0 {
		window = DefaultRewindWindow
	}
	start := t.offset - window
	if start < 0 {
		start = 0
	}
	buf := make([]byte, t.offset-start)
	if err := readAt(t.path, buf, start); err != nil {
		// A file that cannot be read here is not a reason to guess a position:
		// the cursor stays where the store put it and the ordinary poll path
		// reports the read failure with its own context.
		bound.With(logging.Context{Level: "warn"}).Log("no rewind: reading the rewind window failed: %v", err)
		return false
	}
	target, ok := lastTurnStart(buf, start, start > 0, isTurnStart)
	if !ok {
		// BENIGN NORMAL RESUME — debug, not warn, matching the sibling no-rewind
		// outcome above (already read from start). No turn start in the window is
		// the deliberate, correct path: the store's cursor stands because reading
		// from an arbitrary older position would be worse. A rewind that actually
		// moves the cursor stays info below (it is a state change). This fires
		// once per file at boot and flooded a cold re-scan's strict harvest at
		// warn for a decision that was correct.
		bound.LogVerbose(
			"no rewind: no turn start within the %d-byte window ending at offset %d; resuming at the store's cursor", len(buf), t.offset)
		return false
	}
	t.log.With(logging.Context{Operation: "boot-rewind", Path: t.path, FileID: t.fileID, Offset: logging.Off(target)}).Log(
		"rewound the restored cursor from offset %d to the in-progress turn's first record at offset %d; its records re-emit with identical write_ids",
		t.offset, target)
	t.offset = target
	// Every carried byte belongs to a line at or after the old offset, so it is
	// re-read whole by the rewound position.
	t.carry = nil
	t.records = 0
	return true
}

// lastTurnStart finds the offset of the LAST line in buf satisfying isTurnStart.
// skipFirst drops the leading line when the window began mid-file, because that
// line is a fragment rather than a record.
func lastTurnStart(buf []byte, startOffset int64, skipFirst bool, isTurnStart func(map[string]any) bool) (int64, bool) {
	offset := startOffset
	var target int64
	found := false
	first := true
	for len(buf) > 0 {
		nl := bytes.IndexByte(buf, '\n')
		var line []byte
		consumed := len(buf)
		if nl >= 0 {
			line, consumed = buf[:nl], nl+1
		} else {
			line = buf
		}
		lineOffset := offset
		offset += int64(consumed)
		buf = buf[consumed:]
		if first {
			first = false
			if skipFirst {
				continue
			}
		}
		trimmed := bytes.TrimSpace(line)
		if len(trimmed) == 0 {
			continue
		}
		var obj map[string]any
		if err := json.Unmarshal(trimmed, &obj); err != nil {
			continue
		}
		if isTurnStart(obj) {
			target, found = lineOffset, true
		}
	}
	return target, found
}

// IsUserPromptRecord reports whether a transcript record is the first record of
// a turn: a REAL user prompt (ruling R-S3), meaning a `type: "user"` line
// carrying PROSE that is neither a tool_result carrier nor a compaction summary.
//
// The distinctions are all structural rather than semantic:
//
//   - a tool_result carrier's content is an array whose blocks are typed
//     `tool_result`, while a real prompt is either a bare string or an array
//     carrying at least one text block;
//   - `isMeta` records are the harness's own bookkeeping;
//   - a COMPACTION SUMMARY is spelled `isCompactSummary: true`, and it is a
//     `user` record carrying prose in exactly the shape a prompt does. It is
//     the machine's own writing-down of the conversation so far, not a person
//     asking for something, and treating it as a turn start rewound the reader
//     to the summary line instead of to the prompt whose turn is genuinely
//     in progress — re-warming none of the joins the rewind exists for.
//
// A KEEP-ALIVE PROMPT DOES COUNT. It is a real user prompt with a marker in its
// first text block; the marker decides whether the turn's records are STORED,
// not whether a turn began.
func IsUserPromptRecord(obj map[string]any) bool {
	if kind, _ := obj["type"].(string); kind != "user" {
		return false
	}
	if meta, ok := obj["isMeta"].(bool); ok && meta {
		return false
	}
	if summary, ok := obj["isCompactSummary"].(bool); ok && summary {
		return false
	}
	message, ok := obj["message"].(map[string]any)
	if !ok {
		return false
	}
	switch content := message["content"].(type) {
	case string:
		return content != ""
	case []any:
		for _, raw := range content {
			block, ok := raw.(map[string]any)
			if !ok {
				continue
			}
			if kind, _ := block["type"].(string); kind == "text" {
				return true
			}
		}
		return false
	default:
		return false
	}
}
