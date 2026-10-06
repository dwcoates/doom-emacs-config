package merge

import (
	"context"
	"encoding/json"
	"testing"
)

// recordedDoc reads the harness workspace's progress record.
func recordedDoc(t *testing.T, h *harness) progressDoc {
	t.Helper()
	stored, found, err := h.db.MergeProgressOf(context.Background(), theWorkspace)
	if err != nil || !found {
		t.Fatalf("MergeProgressOf = (%v, %v), want the merge's record", found, err)
	}
	var doc progressDoc
	if err := json.Unmarshal(stored.Document, &doc); err != nil {
		t.Fatalf("decoding the record: %v", err)
	}
	return doc
}

// recordedStep is the step the harness workspace's progress record names.
func recordedStep(t *testing.T, h *harness) string {
	t.Helper()
	return recordedDoc(t, h).Step
}
