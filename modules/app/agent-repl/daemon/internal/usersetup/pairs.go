package usersetup

import "strings"

// Pair is one `<key>=<value>` entry of a per-user environment variable.
type Pair struct {
	Key, Value string
	// Entry is the pair as written, untrimmed, for a caller's error to quote.
	Entry string
}

// Pairs splits raw, the value of the environment variable NAME, into its
// comma-separated `<key>=<value>` pairs, each side trimmed. A blank raw is no
// pairs. An entry with no `=` or an empty key is an Errorf naming the entry and
// SHAPE, prefixed with OWNER (the package reading the variable), never an
// entry skipped. The value is the caller's to check: what shape it must take
// is the caller's knowledge.
func Pairs(owner, name, shape, raw string) ([]Pair, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return nil, nil
	}
	var pairs []Pair
	for entry := range strings.SplitSeq(raw, ",") {
		key, value, ok := strings.Cut(strings.TrimSpace(entry), "=")
		key, value = strings.TrimSpace(key), strings.TrimSpace(value)
		if !ok || key == "" {
			return nil, Errorf("%s: %s pair %q is not `%s`", owner, name, entry, shape)
		}
		pairs = append(pairs, Pair{Key: key, Value: value, Entry: entry})
	}
	return pairs, nil
}
