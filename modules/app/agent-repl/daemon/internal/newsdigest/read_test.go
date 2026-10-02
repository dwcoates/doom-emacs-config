package newsdigest

import (
	"reflect"
	"strings"
	"testing"
	"time"
)

func TestParseAtomReadsEveryEntry(t *testing.T) {
	// Arrange
	updated := time.Date(2026, 10, 1, 8, 0, 0, 0, time.UTC)

	// Act
	r, err := parse(feedSource, []byte(atomWith(updated, "v1")))

	// Assert
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := []entry{{ID: "v1", Title: "Release v1", Link: "https://fixture.test/releases/v1", Body: "Notes for v1", At: updated}}
	if !reflect.DeepEqual(r.entries, want) {
		t.Fatalf("entries = %+v, want %+v", r.entries, want)
	}
}

func TestParseAtomLinksAnEntryWithNoLinkToTheSourceHome(t *testing.T) {
	// Arrange
	body := `<feed xmlns="http://www.w3.org/2005/Atom"><entry><id>a</id><updated>2026-10-01T00:00:00Z</updated></entry></feed>`

	// Act
	r, err := parse(feedSource, []byte(body))

	// Assert
	if err != nil || len(r.entries) != 1 || r.entries[0].Link != feedSource.Home {
		t.Fatalf("parse = (%+v, %v), want one entry linking %s", r.entries, err, feedSource.Home)
	}
}

func TestParseAtomRefusesBadFeeds(t *testing.T) {
	tests := []struct {
		name string
		body string
	}{
		{name: "not xml", body: "{"},
		{name: "an entry with no id", body: `<feed><entry><updated>2026-10-01T00:00:00Z</updated></entry></feed>`},
		{name: "an undated entry", body: `<feed><entry><id>a</id><updated>yesterday</updated></entry></feed>`},
		{name: "a link that does not parse", body: `<feed><entry><id>a</id><updated>2026-10-01T00:00:00Z</updated><link href="%zz"/></entry></feed>`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := parse(feedSource, []byte(tt.body))

			// Assert
			if err == nil {
				t.Fatal("parse = nil, want a refusal")
			}
		})
	}
}

func TestParseRSSReadsEveryItem(t *testing.T) {
	// Arrange
	src := Source{Key: "s", Name: "Status", URL: "https://status.test/history.rss", Home: "https://status.test/history", Format: FormatRSS}
	body := `<rss><channel><item><title>Outage</title><link>https://status.test/incidents/1</link>` +
		`<guid>inc-1</guid><pubDate>Wed, 01 Oct 2026 10:00:00 +0000</pubDate><description>&lt;p&gt;Resolved&lt;/p&gt;</description></item></channel></rss>`

	// Act
	r, err := parse(src, []byte(body))

	// Assert
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := []entry{{ID: "inc-1", Title: "Outage", Link: "https://status.test/incidents/1", Body: "Resolved",
		At: time.Date(2026, 10, 1, 10, 0, 0, 0, time.UTC)}}
	if !reflect.DeepEqual(r.entries, want) {
		t.Fatalf("entries = %+v, want %+v", r.entries, want)
	}
}

func TestParseRSSIdentifiesAnItemWithNoGUIDByItsLink(t *testing.T) {
	// Arrange
	src := Source{Key: "s", Name: "Status", URL: "https://status.test/history.rss", Home: "https://status.test/history", Format: FormatRSS}
	body := `<rss><channel><item><link>https://status.test/incidents/2</link><pubDate>Wed, 01 Oct 2026 10:00:00 GMT</pubDate></item></channel></rss>`

	// Act
	r, err := parse(src, []byte(body))

	// Assert
	if err != nil || len(r.entries) != 1 || r.entries[0].ID != "https://status.test/incidents/2" {
		t.Fatalf("parse = (%+v, %v), want the item identified by its link", r.entries, err)
	}
}

func TestParseRSSRefusesBadFeeds(t *testing.T) {
	src := Source{Key: "s", Name: "Status", URL: "https://status.test/history.rss", Home: "https://status.test/history", Format: FormatRSS}
	tests := []struct {
		name string
		body string
	}{
		{name: "not xml", body: "{"},
		{name: "an item with neither guid nor link", body: `<rss><channel><item><pubDate>Wed, 01 Oct 2026 10:00:00 GMT</pubDate></item></channel></rss>`},
		{name: "an undated item", body: `<rss><channel><item><guid>a</guid><pubDate>soon</pubDate></item></channel></rss>`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := parse(src, []byte(tt.body))

			// Assert
			if err == nil {
				t.Fatal("parse = nil, want a refusal")
			}
		})
	}
}

func TestParseNPMReadsEveryVersionNewestFirst(t *testing.T) {
	// Arrange
	src := Source{Key: "npm", Name: "npm", URL: "https://registry.test/sdk", Home: "https://npm.test/package/sdk", Format: FormatNPM}
	body := `{"name":"sdk","time":{"created":"2025-01-01T00:00:00Z","modified":"2026-10-01T00:00:00Z",` +
		`"0.1.0":"2026-09-01T00:00:00Z","0.2.0":"2026-10-01T00:00:00Z"}}`

	// Act
	r, err := parse(src, []byte(body))

	// Assert
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	var ids, links []string
	for _, e := range r.entries {
		ids = append(ids, e.ID)
		links = append(links, e.Link)
	}
	if !reflect.DeepEqual(ids, []string{"0.2.0", "0.1.0"}) {
		t.Fatalf("versions = %v, want 0.2.0 then 0.1.0 (created and modified are no versions)", ids)
	}
	if links[0] != "https://npm.test/package/sdk/v/0.2.0" {
		t.Fatalf("link = %q, want the version's npm page", links[0])
	}
}

func TestParseNPMRefusesBadDocuments(t *testing.T) {
	src := Source{Key: "npm", Name: "npm", URL: "https://registry.test/sdk", Home: "https://npm.test/package/sdk", Format: FormatNPM}
	tests := []struct {
		name string
		body string
	}{
		{name: "not json", body: "<"},
		{name: "no name", body: `{"time":{"0.1.0":"2026-09-01T00:00:00Z"}}`},
		{name: "no time map", body: `{"name":"sdk"}`},
		{name: "a stamp that does not parse", body: `{"name":"sdk","time":{"0.1.0":"last week"}}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := parse(src, []byte(tt.body))

			// Assert
			if err == nil {
				t.Fatal("parse = nil, want a refusal")
			}
		})
	}
}

func TestParseChangelogReadsEveryHeadingAsAnEntry(t *testing.T) {
	// Arrange
	src := Source{Key: "cl", Name: "Changelog", URL: "https://raw.test/CHANGELOG.md", Home: "https://git.test/CHANGELOG.md", Format: FormatChangelog}
	body := "# Changelog\n\n## 2.0.1\n\n- Fixed a thing\n\n## 2.0.0\n- Breaking\n"

	// Act
	r, err := parse(src, []byte(body))

	// Assert
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := []entry{
		{ID: "2.0.1", Title: "2.0.1", Link: src.Home, Body: "- Fixed a thing"},
		{ID: "2.0.0", Title: "2.0.0", Link: src.Home, Body: "- Breaking"},
	}
	if !reflect.DeepEqual(r.entries, want) {
		t.Fatalf("entries = %+v, want %+v", r.entries, want)
	}
}

func TestParsePageReadsItsTextBlocks(t *testing.T) {
	// Act
	r, err := parse(pageSource, []byte("<p>One</p><p>Two</p>"))

	// Assert
	if err != nil || !reflect.DeepEqual(r.blocks, []string{"One", "Two"}) {
		t.Fatalf("parse = (%v, %v), want the blocks One and Two", r.blocks, err)
	}
}

func TestParsePanicsOnASourceWithNoFormat(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("parse did not panic on a source with no format")
		}
	}()

	// Act
	_, _ = parse(Source{Key: "x"}, nil)
}

func TestTextBlocks(t *testing.T) {
	tests := []struct {
		name string
		html string
		want []string
	}{
		{name: "block elements separate blocks", html: "<div><h2>Title</h2><p>Body text</p></div>", want: []string{"Title", "Body text"}},
		{name: "inline elements join their block", html: "<p>A <b>bold</b> <a href='x'>link</a></p>", want: []string{"A bold link"}},
		{name: "whitespace collapses", html: "<p>  spread\n   out  </p>", want: []string{"spread out"}},
		{name: "scripts and styles are not text", html: "<script>var x = 1;</script><style>p{}</style><p>Kept</p>", want: []string{"Kept"}},
		{name: "the head is not text", html: "<html><head><title>T</title></head><body><p>Kept</p></body></html>", want: []string{"Kept"}},
		{name: "empty blocks are dropped", html: "<p> </p><p>Kept</p><br/>", want: []string{"Kept"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := textBlocks([]byte(tt.html))

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("textBlocks = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAbsoluteLinkResolvesAgainstTheSource(t *testing.T) {
	// Act
	got, err := absoluteLink("https://github.com/a/b/releases.atom", "/a/b/releases/tag/v1")

	// Assert
	if err != nil || got != "https://github.com/a/b/releases/tag/v1" {
		t.Fatalf("absoluteLink = (%q, %v), want the absolute release url", got, err)
	}
}

func TestAbsoluteLinkRefusesAnUnparsableBase(t *testing.T) {
	// Act
	_, err := absoluteLink("%zz", "/x")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "source url") {
		t.Fatalf("absoluteLink = %v, want a refusal naming the source url", err)
	}
}
