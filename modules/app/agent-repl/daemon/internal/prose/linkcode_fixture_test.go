package prose

import (
	"bytes"
	"encoding/json"
	"os"
	"testing"

	"agent-repld/internal/codespans"
	"agent-repld/internal/goldenfile"
)

// The webapp's renderer is the authority on where a link is, so its tests draw
// this package's answers for a shared corpus and assert every link comes out
// whole, with a clean href and no code in it: the inputs file lists each text,
// the inline code it is marked with, and the links and code the renderer must
// draw; the fixture file adds what the feed draws for it (Unmark of Markup),
// regenerated with `go test ./internal/prose -run LinkCodeFixture -update`.
//
// IT LIVES IN THIS PACKAGE, NOT IN codespans, because this is the one daemon
// package whose tests read and write the webapp's fixtures (the test roster's
// declared inputs), as its own parsed-prose fixture does.
const (
	linkCodeInputsPath  = "../../../webapp/test/fixtures/link-code-inputs.json"
	linkCodeFixturePath = "../../../webapp/test/fixtures/link-code.json"
)

// linkCodeEntry is one corpus text: the markdown, the spans it is marked with,
// the hrefs and code texts the renderer must draw, and (in the fixture only)
// what the feed draws for it.
type linkCodeEntry struct {
	Markdown string   `json:"markdown"`
	Spans    []string `json:"spans,omitempty"`
	Hrefs    []string `json:"hrefs"`
	Codes    []string `json:"codes"`
	Drawn    string   `json:"drawn,omitempty"`
}

func linkCodeFixture(t *testing.T) []byte {
	t.Helper()
	raw, err := os.ReadFile(linkCodeInputsPath)
	if err != nil {
		t.Fatalf("read %s: %v", linkCodeInputsPath, err)
	}
	var entries []linkCodeEntry
	if err := json.Unmarshal(raw, &entries); err != nil {
		t.Fatalf("decode %s: %v", linkCodeInputsPath, err)
	}
	for i := range entries {
		entries[i].Drawn = codespans.Unmark(codespans.Markup(entries[i].Markdown, entries[i].Spans))
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

func TestLinkCodeFixtureIsTheFeedsDrawingOfEveryInput(t *testing.T) {
	goldenfile.Check(t, linkCodeFixturePath, linkCodeFixture(t), *update)
}
