package frontend

import "testing"

func TestParseJournalRowsReadsAStepInOrder(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a"}` + "\n" + `{"label":"b"}`)
	if len(rows) != 2 || rows[0].GetLabel() != "a" || rows[1].GetLabel() != "b" {
		t.Fatalf("want rows a then b, got %v", rows)
	}
}

func TestParseJournalRowsReadsARecordWithNeitherResultNorErrorAsRunning(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","prompt":"do it"}`)
	if rows[0].GetRunning() == nil {
		t.Fatalf("a step with no outcome yet is still running, got %T", rows[0].GetStatus())
	}
}

func TestParseJournalRowsShowsThePromptAsARunningStepsDetail(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","prompt":"do it"}`)
	if got := rows[0].GetDetail(); got != "do it" {
		t.Fatalf("want detail=%q, got %q", "do it", got)
	}
}

func TestParseJournalRowsReadsAResultAsDone(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","result":"ok"}`)
	if rows[0].GetDone() == nil {
		t.Fatalf("a record carrying a result is a completed step, got %T", rows[0].GetStatus())
	}
}

func TestParseJournalRowsReadsAnEmptyResultAsDoneRatherThanRunning(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","result":""}`)
	if rows[0].GetDone() == nil {
		t.Fatal("a step that completed with no text still completed; the key's presence is the fact")
	}
}

func TestParseJournalRowsReadsAnErrorAsFailed(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","error":"boom"}`)
	if rows[0].GetFailed() == nil {
		t.Fatalf("a record carrying an error is a failed step, got %T", rows[0].GetStatus())
	}
}

func TestParseJournalRowsPrefersTheErrorOverAResultOnTheSameRecord(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","result":"ok","error":"boom"}`)
	if rows[0].GetFailed() == nil {
		t.Fatal("an error settles the question whatever else the record carries")
	}
}

func TestParseJournalRowsCarriesTheFailureTextAsTheDetail(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","error":"boom"}`)
	if got := rows[0].GetDetail(); got != "boom" {
		t.Fatalf("the failure text is the row's detail and is not restated on the arm, got %q", got)
	}
}

func TestParseJournalRowsFallsBackToTheAgentKeyForALabel(t *testing.T) {
	rows, _ := ParseJournalRows(`{"agent":"reviewer"}`)
	if len(rows) != 1 || rows[0].GetLabel() != "reviewer" {
		t.Fatalf("want the agent name as the label, got %v", rows)
	}
}

func TestParseJournalRowsFallsBackToThePhaseKeyForALabel(t *testing.T) {
	rows, _ := ParseJournalRows(`{"phase":"build"}`)
	if len(rows) != 1 || rows[0].GetLabel() != "build" {
		t.Fatalf("want the phase as the label, got %v", rows)
	}
}

func TestParseJournalRowsSkipsARecordThatNamesNoStep(t *testing.T) {
	rows, skipped := ParseJournalRows(`{"result":"ok"}`)
	if len(rows) != 0 || skipped != 1 {
		t.Fatalf("a record with no label is not a step: want 0 rows and 1 skipped, got %d/%d", len(rows), skipped)
	}
}

func TestParseJournalRowsSkipsAPartiallyWrittenTrailingLine(t *testing.T) {
	rows, skipped := ParseJournalRows(`{"label":"a"}` + "\n" + `{"label":"b`)
	if len(rows) != 1 || skipped != 1 {
		t.Fatalf("a journal read mid-write ends on a partial line: want 1 row and 1 skipped, got %d/%d", len(rows), skipped)
	}
}

func TestParseJournalRowsIgnoresBlankLines(t *testing.T) {
	_, skipped := ParseJournalRows("\n\n" + `{"label":"a"}` + "\n")
	if skipped != 0 {
		t.Fatalf("a blank line between records is not a skipped record, got skipped=%d", skipped)
	}
}

func TestParseJournalRowsReadsANonStringDetailAsAbsent(t *testing.T) {
	rows, _ := ParseJournalRows(`{"label":"a","prompt":{"nested":1}}`)
	if got := rows[0].GetDetail(); got != "" {
		t.Fatalf("a detail line is prose the run wrote, not a formatted Go value, got %q", got)
	}
}
