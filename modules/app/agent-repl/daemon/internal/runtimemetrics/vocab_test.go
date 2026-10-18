package runtimemetrics

import (
	"encoding/json"
	"os"
	"slices"
	"testing"
)

const latencyVocabPath = "../../../proto/vocab/latencies.json"

type vocabLatency struct {
	Name       string    `json:"name"`
	ReportedAs *string   `json:"reported_as"`
	Buckets    []float64 `json:"buckets_seconds"`
}

// TestTheKindTablesMatchTheVocabulary pins the daemon's two kind tables to
// proto/vocab/latencies.json: every live-reported entry is in exactly one
// table under its wire name, with the vocabulary's name and buckets, and the
// tables hold nothing the vocabulary does not.
func TestTheKindTablesMatchTheVocabulary(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile(latencyVocabPath)
	if err != nil {
		t.Fatalf("read the vocabulary: %v", err)
	}
	var vocab struct {
		Latencies []vocabLatency `json:"latencies"`
	}
	if err := json.Unmarshal(raw, &vocab); err != nil {
		t.Fatalf("decode the vocabulary: %v", err)
	}
	tables := map[string]LatencyKind{}
	for k, v := range ReportedKinds {
		tables[k.String()] = v
	}
	for k, v := range ShimKinds {
		tables[k.String()] = v
	}

	// Act / Assert
	reported := 0
	for _, l := range vocab.Latencies {
		if l.ReportedAs == nil {
			continue
		}
		reported++
		kind, ok := tables[*l.ReportedAs]
		if !ok {
			t.Errorf("%s is reported as %s, which no daemon table holds", l.Name, *l.ReportedAs)
			continue
		}
		if kind.Name != l.Name || !slices.Equal(kind.Buckets, l.Buckets) {
			t.Errorf("%s: the daemon holds %+v, the vocabulary name %s buckets %v", *l.ReportedAs, kind, l.Name, l.Buckets)
		}
	}
	if reported != len(tables) {
		t.Errorf("the vocabulary reports %d latencies live, the daemon's tables hold %d", reported, len(tables))
	}
}
