package newsdigest

import (
	"encoding/json"
	"fmt"
	"sort"
	"strings"
	"time"
)

// The bounds on what one source hands the model, so one source that changed
// wholesale cannot grow the condensing call without limit. A feed's newest
// entries come first, and a page's changed text is kept in page order.
const (
	// maxEntriesPerSource is how many new entries of one feed are condensed.
	maxEntriesPerSource = 30
	// maxEntryRunes caps one feed entry's body.
	maxEntryRunes = 3000
	// maxPageRunes caps one page's changed text.
	maxPageRunes = 12000
)

// news is what is new in one source since the previous recorded reading.
type news struct {
	// entries are what the model condenses: a feed's new entries, or one
	// entry holding a page's changed text. Empty when nothing is new.
	entries []entry
	// count is how many new entries (a page: changed text blocks) there
	// are, before any bound: the source row's count.
	count int
	// snapshot replaces the source's stored one when the run is recorded.
	snapshot string
}

// diff tells what in r is new against the source's stored snapshot.
//
// A FEED's snapshot is the set of entry ids already read. With one, an entry
// is new exactly when its id is not in it. On a source's FIRST reading there
// is no set to compare, so an entry is new when it is dated after since (the
// previous digest's end, or one cadence ago on the very first run); an
// undated entry is not.
//
// A PAGE's snapshot is its text blocks. With one, the blocks it did not hold
// are new. On a first reading nothing is: there is no earlier text to tell a
// change from, so the reading only becomes the baseline.
func diff(src Source, r reading, prior string, hadPrior bool, since time.Time) (news, error) {
	if src.Format == FormatPage {
		return diffPage(src, r, prior, hadPrior), nil
	}
	return diffFeed(r, prior, hadPrior, since)
}

// diffFeed is diff for a feed.
func diffFeed(r reading, prior string, hadPrior bool, since time.Time) (news, error) {
	seen := map[string]bool{}
	if hadPrior {
		var ids []string
		if err := json.Unmarshal([]byte(prior), &ids); err != nil {
			return news{}, fmt.Errorf("the stored snapshot of seen entries did not decode: %w", err)
		}
		for _, id := range ids {
			seen[id] = true
		}
	}
	var out news
	for _, e := range r.entries {
		var isNew bool
		if hadPrior {
			isNew = !seen[e.ID]
		} else {
			isNew = !e.At.IsZero() && e.At.After(since)
		}
		if isNew {
			out.count++
			out.entries = append(out.entries, e)
		}
		seen[e.ID] = true
	}
	sort.SliceStable(out.entries, func(i, j int) bool { return out.entries[i].At.After(out.entries[j].At) })
	if len(out.entries) > maxEntriesPerSource {
		out.entries = out.entries[:maxEntriesPerSource]
	}
	for i := range out.entries {
		out.entries[i].Body = truncateRunes(out.entries[i].Body, maxEntryRunes)
	}
	ids := make([]string, 0, len(seen))
	for id := range seen {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	encoded, err := json.Marshal(ids)
	if err != nil {
		return news{}, fmt.Errorf("encoding the seen entries: %w", err)
	}
	out.snapshot = string(encoded)
	return out, nil
}

// diffPage is diff for a page.
func diffPage(src Source, r reading, prior string, hadPrior bool) news {
	out := news{snapshot: strings.Join(r.blocks, "\n")}
	if !hadPrior {
		return out
	}
	held := map[string]bool{}
	for _, block := range strings.Split(prior, "\n") {
		held[block] = true
	}
	var changed []string
	for _, block := range r.blocks {
		if !held[block] {
			changed = append(changed, block)
			held[block] = true
		}
	}
	if len(changed) == 0 {
		return out
	}
	out.count = len(changed)
	out.entries = []entry{{
		ID:    src.Key,
		Title: "Changed text on " + src.Name,
		Link:  src.Home,
		Body:  truncateRunes(strings.Join(changed, "\n"), maxPageRunes),
	}}
	return out
}

// truncateRunes keeps the first n runes of s.
func truncateRunes(s string, n int) string {
	runes := []rune(s)
	if len(runes) <= n {
		return s
	}
	return string(runes[:n])
}
