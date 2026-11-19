package codespans

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"testing"
)

// The webapp's renderer is the authority on where a link is, so its tests draw
// this package's answers for a shared corpus and assert every link comes out
// whole, with a clean href and no code in it: the inputs file lists each text,
// the inline code it is marked with, and the links and code the renderer must
// draw; the fixture file adds what the feed draws for it (Unmark of Markup),
// regenerated with `go test ./internal/codespans -run WebappFixture -update`.
const (
	webappInputsPath  = "../../../webapp/test/fixtures/link-code-inputs.json"
	webappFixturePath = "../../../webapp/test/fixtures/link-code.json"
)

var update = flag.Bool("update", false, "rewrite the webapp's link-code fixture from its inputs")

// webappEntry is one corpus text: the markdown, the spans it is marked with,
// the hrefs and code texts the renderer must draw, and (in the fixture only)
// what the feed draws for it.
type webappEntry struct {
	Markdown string   `json:"markdown"`
	Spans    []string `json:"spans,omitempty"`
	Hrefs    []string `json:"hrefs"`
	Codes    []string `json:"codes"`
	Drawn    string   `json:"drawn,omitempty"`
}

func webappFixture(t *testing.T) []byte {
	t.Helper()
	raw, err := os.ReadFile(webappInputsPath)
	if err != nil {
		t.Fatalf("read %s: %v", webappInputsPath, err)
	}
	var entries []webappEntry
	if err := json.Unmarshal(raw, &entries); err != nil {
		t.Fatalf("decode %s: %v", webappInputsPath, err)
	}
	for i := range entries {
		entries[i].Drawn = Unmark(Markup(entries[i].Markdown, entries[i].Spans))
	}
	// HTML escaping off keeps an autolink's angle brackets readable.
	var out bytes.Buffer
	enc := json.NewEncoder(&out)
	enc.SetEscapeHTML(false)
	enc.SetIndent("", "  ")
	if err := enc.Encode(entries); err != nil {
		t.Fatalf("encode the fixture: %v", err)
	}
	return out.Bytes()
}

func TestWebappFixtureIsTheFeedsDrawingOfEveryInput(t *testing.T) {
	// Arrange
	want := webappFixture(t)

	// Act
	if *update {
		if err := os.WriteFile(webappFixturePath, want, 0o644); err != nil {
			t.Fatalf("write %s: %v", webappFixturePath, err)
		}
	}
	got, err := os.ReadFile(webappFixturePath)

	// Assert
	if err != nil {
		t.Fatalf("read %s: %v", webappFixturePath, err)
	}
	if !bytes.Equal(got, want) {
		t.Errorf("%s is stale: rerun with -update", webappFixturePath)
	}
}
