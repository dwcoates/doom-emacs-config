package convert

// journal.go — a workflow's own record of its steps.
//
// A workflow journal is the OUTPUT of detached work, not a conversation of its
// own: the run has a card in the feed and its journal is what accumulates into
// that card. So each record becomes a DetachedWorkProgressed delta on the run's
// message, and the terminal record ends it.

import (
	"strings"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// JournalRecord converts one workflow-journal object into the progress it adds
// to the run's card.
//
// A record whose type this reader does not know is stored unconverted rather
// than rendered as a blank step: a step that shows nothing is worse than a step
// a later schema can still recover, because only one of the two is reversible.
func (c *Converter) JournalRecord(record map[string]any, at Attribution, taskID string) []*agentshimv1.Entry {
	if taskID == "" {
		// Without the run's identity there is no card to append to, and there is
		// no arm for a progress record that names no work.
		c.log.With(logging.Context{Operation: "journal-record", Path: at.Path, Session: at.SessionID, Level: "warn"}).
			Log("journal record at offset=%d has no run identity; stored unconverted", at.Offset)
		return []*agentshimv1.Entry{UnknownEntry(at, str(record["type"]), "type", record)}
	}
	kind := str(record["type"])
	switch kind {
	case "started", "result":
		return []*agentshimv1.Entry{DetachedProgress(at, taskID, journalLine(kind, record))}
	default:
		c.log.With(logging.Context{Operation: "journal-record", Path: at.Path, Session: at.SessionID, Level: "warn"}).
			Log("journal record type=%q at offset=%d is not modeled; stored unconverted", kind, at.Offset)
		return []*agentshimv1.Entry{UnknownEntry(at, kind, "type", record)}
	}
}

// journalLine renders one journal record as the line it contributes to the run's
// output.
//
// THE RENDERING IS LOSSY AND THAT IS A KNOWN COST. DetachedWorkProgressed
// carries a string, because output is what a card shows — so a journal record's
// structure does not survive into the store. See the gap note on partially
// convertible records: an Entry can carry an external half OR an unconverted
// half, so there is nowhere to put the verbatim record alongside its rendering.
func journalLine(kind string, record map[string]any) string {
	var b strings.Builder
	b.WriteString(kind)
	if key := str(record["key"]); key != "" {
		b.WriteString(" ")
		b.WriteString(key)
	}
	switch result := record["result"].(type) {
	case string:
		if result != "" {
			b.WriteString(": ")
			b.WriteString(result)
		}
	case map[string]any:
		if status := str(result["status"]); status != "" {
			b.WriteString(": ")
			b.WriteString(status)
		}
	}
	b.WriteString("\n")
	return b.String()
}
