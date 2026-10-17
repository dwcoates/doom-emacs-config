package prose

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"testing"

	"google.golang.org/protobuf/encoding/protojson"
)

// The webapp draws only what this package parses, so its tests draw trees from
// this package's own answers rather than hand-built graphs: the inputs file
// lists the texts its tests need, and the fixture file is Parse's answer for
// each, regenerated with `go test ./internal/prose -run WebappFixture -update`.
const (
	webappInputsPath  = "../../../webapp/test/fixtures/prose-inputs.json"
	webappFixturePath = "../../../webapp/test/fixtures/parsed-prose.json"
)

var update = flag.Bool("update", false, "rewrite the webapp's parsed-prose fixture from its inputs")

// webappInput is one text the webapp's tests draw, and the stage it is parsed at.
type webappInput struct {
	Text     string `json:"text"`
	Arriving bool   `json:"arriving,omitempty"`
}

// webappEntry is one fixture entry: an input and Parse's answer for it, as the
// protobuf JSON the webapp's fromJson reads.
type webappEntry struct {
	Text     string          `json:"text"`
	Arriving bool            `json:"arriving,omitempty"`
	Prose    json.RawMessage `json:"prose"`
}

func webappFixture(t *testing.T) []byte {
	t.Helper()
	raw, err := os.ReadFile(webappInputsPath)
	if err != nil {
		t.Fatalf("read %s: %v", webappInputsPath, err)
	}
	var inputs []webappInput
	if err := json.Unmarshal(raw, &inputs); err != nil {
		t.Fatalf("decode %s: %v", webappInputsPath, err)
	}
	entries := make([]webappEntry, 0, len(inputs))
	for _, in := range inputs {
		stage := Settled
		if in.Arriving {
			stage = Arriving
		}
		parsed, err := protojson.Marshal(parse(in.Text, stage))
		if err != nil {
			t.Fatalf("marshal the parse of %q: %v", in.Text, err)
		}
		// protojson's spacing is deliberately unstable; compacting it makes
		// the fixture byte-comparable.
		var compact bytes.Buffer
		if err := json.Compact(&compact, parsed); err != nil {
			t.Fatalf("compact the parse of %q: %v", in.Text, err)
		}
		entries = append(entries, webappEntry{Text: in.Text, Arriving: in.Arriving, Prose: compact.Bytes()})
	}
	out, err := json.MarshalIndent(entries, "", "  ")
	if err != nil {
		t.Fatalf("encode the fixture: %v", err)
	}
	return append(out, '\n')
}

func TestWebappFixtureIsParsesAnswerForEveryInput(t *testing.T) {
	// Arrange.
	want := webappFixture(t)

	// Act.
	if *update {
		if err := os.WriteFile(webappFixturePath, want, 0o644); err != nil {
			t.Fatalf("write %s: %v", webappFixturePath, err)
		}
	}
	got, err := os.ReadFile(webappFixturePath)

	// Assert.
	if err != nil {
		t.Fatalf("read %s: %v", webappFixturePath, err)
	}
	if !bytes.Equal(got, want) {
		t.Errorf("%s is stale: rerun with -update", webappFixturePath)
	}
}
