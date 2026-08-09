// asyncjournal.go is the daemon's ONE reading of a Workflow run's
// journal.jsonl.
//
// The parse used to live in the webapp (async-stream.ts parseJournal), which
// made every frontend a second interpreter of the same bytes and left the
// status verdict — running / done / failed — decided independently in each one.
// The contract moved that verdict to the daemon: journal rows arrive as
// AsyncWorkflowJournalRow with a status ARM already chosen, so a state added
// later is an arm a reader must handle rather than an integer it silently
// renders as something else.
package frontend

import (
	"encoding/json"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ParseJournalRows curates a Workflow journal's JSONL text into rows, in order.
//
// UNPARSEABLE LINES ARE SKIPPED, NOT GUESSED AT. A journal is read as it grows,
// so the last line of any read is routinely a partial write; a line that is not
// a JSON object is not a row and there is nothing honest to render from it. The
// count of what was skipped is returned rather than swallowed, so the caller can
// record it.
//
// A record with no label is not a step. The label is the row's whole identity —
// it is what a frontend collapses rows by — and inventing one would put a step
// on screen that the run never logged.
func ParseJournalRows(text string) (rows []*frontendv1.AsyncWorkflowJournalRow, skipped int) {
	for _, line := range strings.Split(text, "\n") {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		var rec map[string]any
		if err := json.Unmarshal([]byte(trimmed), &rec); err != nil {
			skipped++
			continue
		}
		row := journalRow(rec)
		if row == nil {
			skipped++
			continue
		}
		rows = append(rows, row)
	}
	return rows, skipped
}

// journalRow curates one journal record, or nil for a record that names no
// step.
//
// THE STATUS IS RESOLVED FROM THE RECORD'S OWN EVIDENCE, in the order the
// evidence settles the question: an `error` key is a failure whatever else the
// record carries, a `result` key is a completed step, and a record with neither
// is a step still running. The detail line follows the same order, so the text
// on screen and the dot beside it are always drawn from the same reading.
func journalRow(rec map[string]any) *frontendv1.AsyncWorkflowJournalRow {
	label := firstString(rec, "label", "agent", "phase")
	if label == "" {
		return nil
	}
	row := &frontendv1.AsyncWorkflowJournalRow{Label: label}
	if failure := stringField(rec, "error"); failure != "" {
		row.Detail = failure
		row.Status = &frontendv1.AsyncWorkflowJournalRow_Failed{Failed: &frontendv1.AsyncWorkflowStepFailed{}}
		return row
	}
	if _, done := rec["result"]; done {
		row.Detail = stringField(rec, "result")
		row.Status = &frontendv1.AsyncWorkflowJournalRow_Done{Done: &frontendv1.AsyncWorkflowStepDone{}}
		return row
	}
	row.Detail = stringField(rec, "prompt")
	row.Status = &frontendv1.AsyncWorkflowJournalRow_Running{Running: &frontendv1.AsyncWorkflowStepRunning{}}
	return row
}

// firstString returns the first of keys carrying a non-empty string.
func firstString(rec map[string]any, keys ...string) string {
	for _, key := range keys {
		if v := stringField(rec, key); v != "" {
			return v
		}
	}
	return ""
}

// stringField reads a key as a string, and reads a non-string value as absent
// rather than rendering its Go formatting. A journal detail line is prose the
// run wrote; a `map[string]interface {}{...}` on screen is not that.
func stringField(rec map[string]any, key string) string {
	s, _ := rec[key].(string)
	return s
}
