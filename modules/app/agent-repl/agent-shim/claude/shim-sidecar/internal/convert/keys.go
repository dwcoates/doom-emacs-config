package convert

// keys.go — THE UPSERT KEY SPACE, in one file.
//
// The store never interprets an upsert key: it holds one row per key and a
// write supersedes that row WHOLE. So the key space is the producer's whole
// statement of "what is one thing", and the shim must mint the IDENTICAL key
// for the same unit observed on the stream plane. That is why every spelling
// lives here rather than at its call site — two producers cannot agree on a
// convention that is written down in twenty places.

// ActivityKey names one unit of work for its whole life: a tool call by the
// vendor's tool_use_id, a text or thinking block by message id + block index.
func ActivityKey(id string) string { return "activity:" + id }

// QuestionKey names an ask. Its OWN identity space, keyed by the AskUserQuestion
// call that posed it — a question joins to no unit of work.
func QuestionKey(toolUseID string) string { return "question:" + toolUseID }

// TerminalKey names one agent's stream terminal, which is per RECORD rather
// than per agent: a transcript can carry several terminals for one agent over a
// session's life and none of them supersedes another.
func TerminalKey(agent, recordUUID string) string { return "terminal:" + agent + ":" + recordUUID }

// BashKey names a detached shell run by the spawning call's unit id, so the
// spool's deltas and the announcement land on ONE row.
func BashKey(run string) string { return "bash:" + run }

// SessionKey names a session-scoped fact by its arm and the record that carried
// it — a context cut, a mid-turn api error.
func SessionKey(arm, recordUUID string) string { return "session:" + arm + ":" + recordUUID }

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
