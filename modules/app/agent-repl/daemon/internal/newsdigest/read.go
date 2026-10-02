package newsdigest

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"errors"
	"fmt"
	"net/url"
	"sort"
	"strings"
	"time"

	"golang.org/x/net/html"
	"golang.org/x/net/html/atom"
)

// entry is one dated item of a feed.
type entry struct {
	// ID is the entry's stable identity within its source.
	ID string
	// Title is the entry's own title.
	Title string
	// Link is the entry's own page; the source's Home when it names none.
	Link string
	// Body is the entry's text, markup removed.
	Body string
	// At is when the source dates the entry; zero when it dates none.
	At time.Time
}

// reading is what one fetch of a source yielded: a feed's entries, or a
// page's text blocks.
type reading struct {
	entries []entry
	blocks  []string
}

// parse reads body as src's format.
func parse(src Source, body []byte) (reading, error) {
	switch src.Format {
	case FormatAtom:
		return parseAtom(src, body)
	case FormatRSS:
		return parseRSS(src, body)
	case FormatNPM:
		return parseNPM(src, body)
	case FormatChangelog:
		return parseChangelog(src, body), nil
	case FormatPage:
		return reading{blocks: textBlocks(body)}, nil
	default:
		panic(fmt.Sprintf("newsdigest: source %q has no format", src.Key))
	}
}

// atomFeed is the slice of an Atom document the digest reads.
type atomFeed struct {
	XMLName xml.Name `xml:"feed"`
	Entries []struct {
		ID      string `xml:"id"`
		Title   string `xml:"title"`
		Updated string `xml:"updated"`
		Links   []struct {
			Href string `xml:"href,attr"`
			Rel  string `xml:"rel,attr"`
		} `xml:"link"`
		Content string `xml:"content"`
	} `xml:"entry"`
}

// parseAtom reads an Atom feed. An entry with no id, or with an updated stamp
// that does not parse, fails the whole read: a feed the digest misreads is a
// failed source, never a partial one.
func parseAtom(src Source, body []byte) (reading, error) {
	var feed atomFeed
	if err := xml.Unmarshal(body, &feed); err != nil {
		return reading{}, fmt.Errorf("the atom feed did not parse: %w", err)
	}
	var out reading
	for i, e := range feed.Entries {
		if strings.TrimSpace(e.ID) == "" {
			return reading{}, fmt.Errorf("atom entry %d carries no id", i)
		}
		at, err := time.Parse(time.RFC3339, strings.TrimSpace(e.Updated))
		if err != nil {
			return reading{}, fmt.Errorf("atom entry %q: the updated stamp did not parse: %w", e.ID, err)
		}
		link := src.Home
		for _, l := range e.Links {
			if l.Rel == "" || l.Rel == "alternate" {
				if link, err = absoluteLink(src.URL, l.Href); err != nil {
					return reading{}, fmt.Errorf("atom entry %q: %w", e.ID, err)
				}
				break
			}
		}
		out.entries = append(out.entries, entry{
			ID:    strings.TrimSpace(e.ID),
			Title: strings.TrimSpace(e.Title),
			Link:  link,
			Body:  strings.Join(textBlocks([]byte(e.Content)), "\n"),
			At:    at.UTC(),
		})
	}
	return out, nil
}

// rssFeed is the slice of an RSS 2.0 document the digest reads.
type rssFeed struct {
	XMLName xml.Name `xml:"rss"`
	Items   []struct {
		Title       string `xml:"title"`
		Link        string `xml:"link"`
		GUID        string `xml:"guid"`
		PubDate     string `xml:"pubDate"`
		Description string `xml:"description"`
	} `xml:"channel>item"`
}

// rssDateLayouts are the RFC 822 spellings RSS dates arrive in.
var rssDateLayouts = []string{time.RFC1123Z, time.RFC1123, time.RFC822Z, time.RFC822}

// parseRSS reads an RSS feed. An item is identified by its guid, else its
// link; one with neither, or with a date that does not parse, fails the read.
func parseRSS(src Source, body []byte) (reading, error) {
	var feed rssFeed
	if err := xml.Unmarshal(body, &feed); err != nil {
		return reading{}, fmt.Errorf("the rss feed did not parse: %w", err)
	}
	var out reading
	for i, it := range feed.Items {
		id := strings.TrimSpace(it.GUID)
		if id == "" {
			id = strings.TrimSpace(it.Link)
		}
		if id == "" {
			return reading{}, fmt.Errorf("rss item %d carries neither a guid nor a link", i)
		}
		at, err := parseRSSDate(strings.TrimSpace(it.PubDate))
		if err != nil {
			return reading{}, fmt.Errorf("rss item %q: %w", id, err)
		}
		link := src.Home
		if strings.TrimSpace(it.Link) != "" {
			if link, err = absoluteLink(src.URL, strings.TrimSpace(it.Link)); err != nil {
				return reading{}, fmt.Errorf("rss item %q: %w", id, err)
			}
		}
		out.entries = append(out.entries, entry{
			ID:    id,
			Title: strings.TrimSpace(it.Title),
			Link:  link,
			Body:  strings.Join(textBlocks([]byte(it.Description)), "\n"),
			At:    at.UTC(),
		})
	}
	return out, nil
}

// parseRSSDate parses one RSS date in any of its RFC 822 spellings.
func parseRSSDate(s string) (time.Time, error) {
	for _, layout := range rssDateLayouts {
		if at, err := time.Parse(layout, s); err == nil {
			return at, nil
		}
	}
	return time.Time{}, fmt.Errorf("the date %q is no RFC 822 date", s)
}

// npmDocument is the slice of an npm registry document the digest reads.
type npmDocument struct {
	Name string            `json:"name"`
	Time map[string]string `json:"time"`
}

// npmTimeKeys are the `time` map's keys that are not versions.
var npmTimeKeys = map[string]bool{"created": true, "modified": true}

// parseNPM reads an npm registry document: one entry per published version,
// dated by the `time` map. A document with no name or no time map, or a stamp
// that does not parse, fails the read.
func parseNPM(src Source, body []byte) (reading, error) {
	var doc npmDocument
	if err := json.Unmarshal(body, &doc); err != nil {
		return reading{}, fmt.Errorf("the npm document did not parse: %w", err)
	}
	if doc.Name == "" || len(doc.Time) == 0 {
		return reading{}, errors.New("the npm document carries no name or no time map")
	}
	var out reading
	for version, stamp := range doc.Time {
		if npmTimeKeys[version] {
			continue
		}
		at, err := time.Parse(time.RFC3339, stamp)
		if err != nil {
			return reading{}, fmt.Errorf("npm version %q: the stamp did not parse: %w", version, err)
		}
		out.entries = append(out.entries, entry{
			ID:    version,
			Title: doc.Name + " " + version,
			Link:  src.Home + "/v/" + url.PathEscape(version),
			Body:  "Published to npm.",
			At:    at.UTC(),
		})
	}
	sort.Slice(out.entries, func(i, j int) bool { return out.entries[i].At.After(out.entries[j].At) })
	return out, nil
}

// parseChangelog reads a markdown changelog: every `## ` heading opens an
// entry identified by its heading text, and runs to the next one. A changelog
// dates no entry.
func parseChangelog(src Source, body []byte) reading {
	var out reading
	var current *entry
	var lines []string
	flush := func() {
		if current != nil {
			current.Body = strings.TrimSpace(strings.Join(lines, "\n"))
			out.entries = append(out.entries, *current)
		}
	}
	for _, line := range strings.Split(string(body), "\n") {
		if heading, ok := strings.CutPrefix(line, "## "); ok {
			flush()
			heading = strings.TrimSpace(heading)
			current = &entry{ID: heading, Title: heading, Link: src.Home}
			lines = nil
			continue
		}
		if current != nil {
			lines = append(lines, line)
		}
	}
	flush()
	return out
}

// absoluteLink resolves an entry's href against its source's URL. A link that
// does not parse fails the read rather than being drawn as something else.
func absoluteLink(base, href string) (string, error) {
	b, err := url.Parse(base)
	if err != nil {
		return "", fmt.Errorf("the source url %q did not parse: %w", base, err)
	}
	h, err := url.Parse(href)
	if err != nil {
		return "", fmt.Errorf("the link %q did not parse: %w", href, err)
	}
	return b.ResolveReference(h).String(), nil
}

// skippedElements are elements whose text is never page content.
var skippedElements = map[atom.Atom]bool{
	atom.Script: true, atom.Style: true, atom.Noscript: true, atom.Svg: true,
	atom.Template: true, atom.Head: true, atom.Iframe: true,
}

// blockElements are elements that end one text block and begin another.
var blockElements = map[atom.Atom]bool{
	atom.P: true, atom.Div: true, atom.Li: true, atom.Ul: true, atom.Ol: true,
	atom.H1: true, atom.H2: true, atom.H3: true, atom.H4: true, atom.H5: true, atom.H6: true,
	atom.Br: true, atom.Tr: true, atom.Td: true, atom.Th: true, atom.Table: true,
	atom.Section: true, atom.Article: true, atom.Header: true, atom.Footer: true,
	atom.Nav: true, atom.Main: true, atom.Aside: true, atom.Blockquote: true,
	atom.Pre: true, atom.Dt: true, atom.Dd: true, atom.Figcaption: true, atom.Summary: true,
	atom.Details: true, atom.Hr: true,
}

// textBlocks extracts an HTML fragment's or document's visible text as
// blocks: one per block-level element's run of text, whitespace collapsed,
// empties dropped.
func textBlocks(body []byte) []string {
	z := html.NewTokenizer(bytes.NewReader(body))
	var blocks []string
	var current strings.Builder
	skipping := 0
	end := func() {
		if text := strings.Join(strings.Fields(current.String()), " "); text != "" {
			blocks = append(blocks, text)
		}
		current.Reset()
	}
	for {
		tt := z.Next()
		switch tt {
		case html.ErrorToken:
			end()
			return blocks
		case html.TextToken:
			if skipping == 0 {
				current.Write(z.Text())
				current.WriteByte(' ')
			}
		case html.StartTagToken, html.SelfClosingTagToken, html.EndTagToken:
			name, _ := z.TagName()
			a := atom.Lookup(name)
			switch {
			case skippedElements[a] && tt == html.StartTagToken:
				skipping++
			case skippedElements[a] && tt == html.EndTagToken && skipping > 0:
				skipping--
			case skippedElements[a]:
			case blockElements[a]:
				end()
			}
		}
	}
}
