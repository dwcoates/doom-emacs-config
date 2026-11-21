// Package scenariogolden is the daemon half of the mocked-vendor scenario
// goldens (testdata/scenario-goldens): reading each scenario's frames.json and
// feed.json, renaming its identities to the driver's, driving its frames
// through a Driver, and normalizing the root feed the frames drew.
//
// IT IS SHARED BY TWO DRIVERS of the same goldens: the in-process projection
// (inprocess_test.go), which runs the real session watcher and the real feed
// resolver with no daemon, shim process, repository or log file and carries
// every scenario, and the end-to-end check through a real daemon
// (daemon/integration/scenariogoldens_test.go), which carries one or two to
// prove the shim-to-watcher-to-feed routing seam. One drive loop serves both,
// so the two cannot drift.
package scenariogolden

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// Row is one row of a scenario's frames.json: the fold's output, in
// order, as the shim writes it to its store.
type Row struct {
	UpsertKey   string `json:"upsertKey"`
	Book        string `json:"book"`
	Turn        string `json:"turn"`
	RecordPlace *struct {
		AtMs int64 `json:"atMs"`
	} `json:"recordPlace"`
	Item struct {
		Kind   string          `json:"kind"`
		Prompt json.RawMessage `json:"prompt"`
		Frame  json.RawMessage `json:"frame"`
		Update json.RawMessage `json:"update"`
	} `json:"item"`
}

// Names lists every scenario directory, sorted.
func Names(t *testing.T, scenarioGoldensDir string) []string {
	t.Helper()
	entries, err := os.ReadDir(scenarioGoldensDir)
	if err != nil {
		t.Fatalf("read the scenario goldens at %s: %v", scenarioGoldensDir, err)
	}
	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("%s holds no scenario golden", scenarioGoldensDir)
	}
	return names
}

// ReadRows reads one scenario's frames.json.
func ReadRows(t *testing.T, scenarioGoldensDir, name string) []Row {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(scenarioGoldensDir, name, "frames.json"))
	if err != nil {
		t.Fatalf("read %s's frames.json: %v", name, err)
	}
	var rows []Row
	if err := json.Unmarshal(raw, &rows); err != nil {
		t.Fatalf("decode %s's frames.json: %v", name, err)
	}
	return rows
}

// WriteEnv rewrites every scenario's feed.json from a fresh
// projection, the same switch the shim's goldens are rewritten by.
const WriteEnv = "AGENT_REPL_SCENARIO_GOLDENS_WRITE"

// FeedGolden is a scenario's feed.json: the daemon's half of the scenario,
// what its root feed draws from the frames the shim's half produced, and the
// warning operations drawing it records.
type FeedGolden struct {
	Rows     json.RawMessage `json:"rows"`
	Warnings []string        `json:"warnings"`
}

// IDs maps a drive's identities to this fixture's.
type IDs struct {
	// mainBook is the drive's main agent: the book its turn's prompt is in.
	mainBook string
	// names maps each drive identity to the fixture's.
	names map[string]string
}

// Identities reads a drive's identities off its rows: the main agent
// is the book the turn's prompt was written to, renamed to MAINAGENT, and the turn is that prompt's.
// A drive with no prompt row is refused: every golden opens one turn.
func Identities(t *testing.T, name string, rows []Row, turn, mainAgent string) IDs {
	t.Helper()
	for _, r := range rows {
		if r.Item.Kind == "prompt" {
			return IDs{mainBook: r.Book, names: map[string]string{r.Book: mainAgent, r.Turn: turn}}
		}
	}
	t.Fatalf("%s's frames.json writes no prompt row, so it names no main agent and no turn", name)
	return IDs{}
}

// Rename renames the drive's identities to the fixture's in RAW, an item of
// frames.json: every string value that IS a drive identity (a frame's
// agent_id, an announcement's owner, a prompt's turn) names the fixture's
// instead. A value merely containing one is left alone; only an identity is
// renamed.
func (ids IDs) Rename(t *testing.T, raw json.RawMessage) []byte {
	t.Helper()
	var v any
	if err := json.Unmarshal(raw, &v); err != nil {
		t.Fatalf("decode a golden item: %v", err)
	}
	renamed := mapJSONLeaves(v, func(_ string, leaf any) any {
		if x, ok := leaf.(string); ok {
			if name, ok := ids.names[x]; ok {
				return name
			}
		}
		return leaf
	}, nil)
	out, err := json.Marshal(renamed)
	if err != nil {
		t.Fatalf("encode a golden item: %v", err)
	}
	return out
}

// ReadNowMs reads the instant a scenario's drive began, which its
// meta.json states (the shim's HARNESS_NOW_MS).
func ReadNowMs(t *testing.T, scenarioGoldensDir, name string) int64 {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(scenarioGoldensDir, name, "meta.json"))
	if err != nil {
		t.Fatalf("read %s's meta.json: %v", name, err)
	}
	var meta struct {
		NowMs int64 `json:"nowMs"`
	}
	if err := json.Unmarshal(raw, &meta); err != nil {
		t.Fatalf("decode %s's meta.json: %v", name, err)
	}
	if meta.NowMs <= 0 {
		t.Fatalf("%s's meta.json states no nowMs, the instant its drive began", name)
	}
	return meta.NowMs
}

// ReadFeedGolden reads a scenario's committed feed.json.
func ReadFeedGolden(t *testing.T, path string) FeedGolden {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the daemon half's golden %s (write it with %s=1): %v", path, WriteEnv, err)
	}
	var g FeedGolden
	if err := json.Unmarshal(raw, &g); err != nil {
		t.Fatalf("decode %s: %v", path, err)
	}
	canonical, err := canonicalJSON(g.Rows)
	if err != nil {
		t.Fatalf("decode %s's rows: %v", path, err)
	}
	g.Rows = canonical
	return g
}

// WriteFeedGolden writes a scenario's feed.json.
func WriteFeedGolden(t *testing.T, path string, g FeedGolden) {
	t.Helper()
	if g.Warnings == nil {
		g.Warnings = []string{}
	}
	raw, err := json.MarshalIndent(g, "", "  ")
	if err != nil {
		t.Fatalf("encode %s: %v", path, err)
	}
	if err := os.WriteFile(path, append(raw, '\n'), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// Instants is every instant the shim's half states (a recorded place,
// a frame's own times): a feed value equal to one of them came from the
// frames and is kept, while any other wall-clock value is the daemon's own
// clock and is normalized away.
func Instants(t *testing.T, rows []Row) map[string]bool {
	t.Helper()
	out := map[string]bool{}
	for _, r := range rows {
		raw, err := json.Marshal(r)
		if err != nil {
			t.Fatalf("encode a golden row: %v", err)
		}
		var v any
		if err := json.Unmarshal(raw, &v); err != nil {
			t.Fatalf("decode a golden row: %v", err)
		}
		mapJSONLeaves(v, func(_ string, leaf any) any {
			switch x := leaf.(type) {
			case string:
				if isInstant(x) {
					out[x] = true
				}
			case float64:
				out[strconv.FormatInt(int64(x), 10)] = true
			}
			return leaf
		}, nil)
	}
	return out
}

// isInstant reports whether S is a millisecond wall-clock instant as protojson
// spells an int64: thirteen or more digits.
func isInstant(s string) bool {
	if len(s) < 13 {
		return false
	}
	for _, c := range s {
		if c < '0' || c > '9' {
			return false
		}
	}
	return true
}

// Normalize renders ROWS as canonical JSON with every value that differs
// between two projections of the same frames replaced by a stable token: the
// minted turn, the workspace id, each row id and order key by its first
// appearance, and every instant of the daemon's own clock.
func Normalize(t *testing.T, rows []*frontendv1.FeedRow, turn, ws string, instants map[string]bool) json.RawMessage {
	t.Helper()
	var all []any
	for _, row := range rows {
		raw, err := protojson.Marshal(row)
		if err != nil {
			t.Fatalf("encode a feed row: %v", err)
		}
		var v any
		if err := json.Unmarshal(raw, &v); err != nil {
			t.Fatalf("decode a feed row: %v", err)
		}
		all = append(all, v)
	}
	ids, orders := map[string]string{}, map[string]string{}
	normalized := mapJSONLeaves(all, func(key string, leaf any) any {
		x, ok := leaf.(string)
		if !ok {
			return leaf
		}
		switch {
		case x == turn:
			return "${TURN}"
		case strings.HasPrefix(x, "f1."):
			if ids[x] == "" {
				ids[x] = "${ROW_" + strconv.Itoa(len(ids)+1) + "}"
			}
			return ids[x]
		case key == "key" && orders[x] == "" && isOrderKey(x):
			orders[x] = "${ORDER_" + strconv.Itoa(len(orders)+1) + "}"
			return orders[x]
		case orders[x] != "":
			return orders[x]
		case isInstant(x) && !instants[x]:
			return "${NOW}"
		}
		return strings.ReplaceAll(x, ws, "${WS}")
	}, func(key string) bool { return wallClockFields[key] })
	raw, err := json.Marshal(normalized)
	if err != nil {
		t.Fatalf("encode the normalized feed: %v", err)
	}
	canonical, err := canonicalJSON(raw)
	if err != nil {
		t.Fatalf("canonicalize the normalized feed: %v", err)
	}
	return canonical
}

// wallClockFields are the row fields the daemon derives from ITS OWN wall
// clock between arrivals, never from the frames: a response's reveal pace
// (frontend.v1 FeedResponse.reveal_pace, from the gaps between its streamed
// updates) differs between two projections of the same frames and is pinned
// on a fake clock by the resolver's own suite (resolve/feed/revealpace_test.go).
var wallClockFields = map[string]bool{"revealPace": true}

// isOrderKey reports whether S is a feed row's order key (FeedRowOrder.key):
// a class byte, then hex.
func isOrderKey(s string) bool {
	return len(s) > 17 && (s[0] == '1' || s[0] == '2' || s[0] == '~')
}

// mapJSONLeaves rewrites V, a decoded JSON value, in place: every leaf (a
// value that is no object and no array) becomes LEAF(key, leaf), where key is
// the object key it sits under (an array's elements carry their array's), and
// every object field DROP accepts is removed. Objects are walked in sorted key
// order, so a LEAF that mints tokens by first appearance mints the same ones
// for the same value every time.
func mapJSONLeaves(v any, leaf func(key string, v any) any, drop func(key string) bool) any {
	var walk func(key string, v any) any
	walk = func(key string, v any) any {
		switch x := v.(type) {
		case map[string]any:
			keys := make([]string, 0, len(x))
			for k := range x {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			for _, k := range keys {
				if drop != nil && drop(k) {
					delete(x, k)
					continue
				}
				x[k] = walk(k, x[k])
			}
			return x
		case []any:
			for i := range x {
				x[i] = walk(key, x[i])
			}
			return x
		default:
			return leaf(key, v)
		}
	}
	return walk("", v)
}

// canonicalJSON re-encodes RAW indented with sorted keys, so two encodings of
// one value compare equal byte for byte.
func canonicalJSON(raw []byte) (json.RawMessage, error) {
	var v any
	if err := json.Unmarshal(raw, &v); err != nil {
		return nil, err
	}
	return json.MarshalIndent(v, "", "  ")
}

// WarningOperations is the sorted, distinct operations of OPS, the warning
// operations a drawing recorded.
func WarningOperations(ops []string) []string {
	seen := map[string]bool{}
	var out []string
	for _, op := range ops {
		if !seen[op] {
			seen[op] = true
			out = append(out, op)
		}
	}
	sort.Strings(out)
	return out
}
