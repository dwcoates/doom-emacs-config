package runtimemetrics

import (
	"encoding/json"
	"os"
	"testing"

	"agent-repld/internal/vendorspend"
)

// The vendor spend vocabulary's tests live here, beside the vocabulary the
// other metrics tests read as data: the test roster grants this package
// proto/vocab/.
const spendVocabPath = "../../../proto/vocab/vendor-spend.json"

// spendVocab is proto/vocab/vendor-spend.json.
type spendVocab struct {
	ContextBuckets []uint64 `json:"context_buckets_tokens"`
	CacheOutcomes  []struct {
		CacheRead     uint64 `json:"cache_read"`
		CacheWrite    uint64 `json:"cache_write"`
		UncachedInput uint64 `json:"uncached_input"`
		Outcome       string `json:"outcome"`
	} `json:"cache_outcomes"`
}

func readSpendVocab(t *testing.T) spendVocab {
	t.Helper()
	raw, err := os.ReadFile(spendVocabPath)
	if err != nil {
		t.Fatalf("read the vocabulary: %v", err)
	}
	var v spendVocab
	if err := json.Unmarshal(raw, &v); err != nil {
		t.Fatalf("parse the vocabulary: %v", err)
	}
	return v
}

func TestContextBucketsAreTheVocabularys(t *testing.T) {
	// Arrange
	v := readSpendVocab(t)
	// Assert
	if len(v.ContextBuckets) != len(vendorspend.ContextBuckets) {
		t.Fatalf("the vocabulary has %d buckets, the code %d", len(v.ContextBuckets), len(vendorspend.ContextBuckets))
	}
	for i := range vendorspend.ContextBuckets {
		if vendorspend.ContextBuckets[i] != v.ContextBuckets[i] {
			t.Errorf("bucket %d: code %d, vocabulary %d", i, vendorspend.ContextBuckets[i], v.ContextBuckets[i])
		}
	}
}

func TestOutcomeOfAnswersTheVocabularysCases(t *testing.T) {
	for _, c := range readSpendVocab(t).CacheOutcomes {
		t.Run(c.Outcome, func(t *testing.T) {
			// Act
			got := vendorspend.OutcomeOf(c.CacheRead, c.CacheWrite, c.UncachedInput)
			// Assert
			if want := "VENDOR_CACHE_OUTCOME_" + c.Outcome; got.String() != want {
				t.Errorf("read %d write %d uncached %d: got %s, want %s", c.CacheRead, c.CacheWrite, c.UncachedInput, got, want)
			}
		})
	}
}
