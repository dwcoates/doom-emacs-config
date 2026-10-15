//go:build integration

package integration

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"

	"agent-repld/integration/harness"
)

// scenarioGoldensDir is the shim's scenario goldens, at the module's shared
// testdata: what each mocked-vendor scenario's fold produced, one directory
// per scenario (agent-shim/claude/shim/test/fake/goldens.ts).
const scenarioGoldensDir = "../../testdata/scenario-goldens"

// goldenRow is one row of a scenario's frames.json: the fold's output, in
// order, as the shim writes it to its store.
type goldenRow struct {
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

// scenarioGoldens lists every scenario directory, sorted.
func scenarioGoldens(t *testing.T) []string {
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

// readGoldenRows reads one scenario's frames.json.
func readGoldenRows(t *testing.T, name string) []goldenRow {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(scenarioGoldensDir, name, "frames.json"))
	if err != nil {
		t.Fatalf("read %s's frames.json: %v", name, err)
	}
	var rows []goldenRow
	if err := json.Unmarshal(raw, &rows); err != nil {
		t.Fatalf("decode %s's frames.json: %v", name, err)
	}
	return rows
}

// scenarioGoldensWriteEnv rewrites every scenario's feed.json from a fresh
// projection, the same switch the shim's goldens are rewritten by.
const scenarioGoldensWriteEnv = "AGENT_REPL_SCENARIO_GOLDENS_WRITE"

// feedGolden is a scenario's feed.json: the daemon's half of the scenario,
// what its root feed draws from the frames the shim's half produced, and the
// warning operations drawing it records.
type feedGolden struct {
	Rows     json.RawMessage `json:"rows"`
	Warnings []string        `json:"warnings"`
}

// TestEveryScenarioGoldenProjectsToItsFeedGolden is the daemon half of every
// mocked-vendor scenario (SCENARIO-MATRIX.md, ruling (a) of 2026-10-14): the
// frames the shim's fold produced for the scenario (frames.json, the shim
// half's golden) are served by the fake shim exactly as the real shim serves
// its store -- one pointer per upsert key, stamped with the turn and its
// recorded place -- and the root feed the daemon draws from them must match
// the scenario's feed.json.
//
// Every entry and update is awaited on the watcher's own routing records
// (daemon.sessionwatcher.entry_routed, session_update_routed), never a quiet
// window. The file-plane items (residue, shell_run_claim, bash_run) are the
// sidecar's to deliver and are not pushed here; a scenario that needs them is
// a two-plane seam with its e2e wiring test.
func TestEveryScenarioGoldenProjectsToItsFeedGolden(t *testing.T) {
	t.Parallel()
	write := os.Getenv(scenarioGoldensWriteEnv) == "1"
	for _, name := range scenarioGoldens(t) {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			rows := readGoldenRows(t, name)
			path := filepath.Join(scenarioGoldensDir, name, "feed.json")
			f := newOpened(t, harness.Opts{})
			var want feedGolden
			if !write {
				want = readFeedGolden(t, path)
				f.d.RequireWarnings(want.Warnings...)
			}

			got := projectScenario(t, f, name, rows)

			if write {
				warnings := warningOperations(f.d.UnexpectedWarnings())
				f.d.ExpectWarnings(warnings...)
				writeFeedGolden(t, path, feedGolden{Rows: got, Warnings: warnings})
				return
			}
			if string(got) != string(want.Rows) {
				t.Fatalf("the daemon's feed for %s differs from its golden %s (rewrite with %s=1 if the change is intended):\ngot:\n%s\nwant:\n%s",
					name, path, scenarioGoldensWriteEnv, got, want.Rows)
			}
		})
	}
}

// projectScenario drives one scenario's frames through the daemon and answers
// its normalized root feed.
func projectScenario(t *testing.T, f *fixture, name string, rows []goldenRow) json.RawMessage {
	t.Helper()
	prompt := "!" + name
	turn := f.submit(prompt, "k-"+name, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).GetSuccess().GetTurn().GetTurn().GetValue()
	if turn == "" {
		t.Fatalf("SubmitPrompt(%q) minted no turn", prompt)
	}
	f.shim.ExpectStartTurn()
	// THE DRIVE'S IDENTITIES ARE THIS FIXTURE'S: its main agent's book (the
	// book its turn's prompt was written to) and its turn name this daemon's.
	ids := goldenIdentities(t, name, rows, turn)
	// Counted BEFORE the first push, so no routing it causes is in the base.
	routed := map[string]int{"": countRouted(f, entryRouted(""))}
	updates := countRouted(f, updateRouted)

	pointers := map[string]string{}
	terminal := false
	for _, r := range rows {
		// THE STORE'S POINTER RULE: minted at a key's first write and kept
		// across every later upsert of the key.
		key := r.Book + "\x00" + r.UpsertKey
		if pointers[key] == "" {
			pointers[key] = "g-" + strconv.Itoa(len(pointers)+1)
		}
		var place int64
		if r.RecordPlace != nil {
			place = r.RecordPlace.AtMs
		}
		switch r.Item.Kind {
		case "prompt":
			prompt := &conversationv1.AgentPrompt{}
			if err := protojson.Unmarshal(ids.rename(t, r.Item.Prompt), prompt); err != nil {
				t.Fatalf("decode the prompt of %s: %v", name, err)
			}
			if place == 0 {
				// THE STORE PLACES AN UNPLACED ROW AT ITS ARRIVAL, which for the
				// turn's prompt is the instant the drive began: the goldens' own
				// clock, never this daemon's, so every row sorts on one timeline.
				place = readGoldenNowMs(t, name)
			}
			f.shim.PushUserPromptStamped(mainAgent, harness.EntryStamp{Pointer: pointers[key], Turn: turn, PlaceMs: place}, prompt)
			routed[""]++
			awaitRouted(t, f, "the delivered prompt", entryRouted(""), routed[""])
		case "frame":
			frame := &conversationv1.AgentFrame{}
			if err := protojson.Unmarshal(ids.rename(t, r.Item.Frame), frame); err != nil {
				t.Fatalf("decode a frame of %s: %v", name, err)
			}
			agent, watch := r.Book, r.Book
			if agent == ids.mainBook {
				agent, watch = mainAgent, ""
				terminal = terminal || frame.GetSuccess() != nil || frame.GetFailure() != nil
			}
			if _, open := routed[watch]; !open {
				// A SUBAGENT'S FRAMES ARE PUSHED ONCE ITS WATCH IS OPEN, so
				// each is served live and routed as an entry, never folded
				// into the opening page at whatever moment the watch opens.
				// The page's record follows the fake's subscription.
				f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "the watch of "+watch+" to open",
					func(r harness.LogRecord) bool {
						return r.Operation == "daemon.sessionwatcher.history_page" && r.Context["agent_id"] == watch
					})
				routed[watch] = countRouted(f, entryRouted(watch))
			}
			f.shim.PushAgentFrameStamped(agent, harness.EntryStamp{Pointer: pointers[key], Turn: turn, PlaceMs: place}, frame)
			routed[watch]++
			awaitRouted(t, f, "a frame of "+agent, entryRouted(watch), routed[watch])
		case "session_update":
			u := &conversationv1.SessionUpdate{}
			if err := protojson.Unmarshal(ids.rename(t, r.Item.Update), u); err != nil {
				t.Fatalf("decode a session update of %s: %v", name, err)
			}
			f.shim.PushSessionUpdate(u)
			updates++
			awaitRouted(t, f, "a session update", updateRouted, updates)
		}
	}
	if terminal {
		// THE TURN'S END IS DRAWN ONCE THE LIFECYCLE CLOSES IT, after the
		// watcher's lock is released, so its row is awaited on its own.
		f.awaitRowInFeed(nil, "the scenario turn's ended row", func(r *frontendv1.FeedRow) bool {
			return r.GetTurnEnded() != nil && r.GetTurn().GetValue() == turn
		})
	}
	page, _ := f.openFeed(nil)
	return normalizeFeed(t, page.GetSuccess().GetRows(), turn, f.ws.GetId(), goldenInstants(t, rows))
}

// entryRouted is the routing record of an entry on AGENT's watch ("" for
// the main watch).
func entryRouted(agent string) func(harness.LogRecord) bool {
	return func(r harness.LogRecord) bool {
		if r.Operation != "daemon.sessionwatcher.entry_routed" {
			return false
		}
		if agent == "" {
			return r.Context["main"] == true
		}
		return r.Context["agent_id"] == agent
	}
}

// updateRouted is the routing record of a session update.
func updateRouted(r harness.LogRecord) bool {
	return r.Operation == "daemon.sessionwatcher.session_update_routed"
}

// countRouted counts the workspace log's records MATCH accepts.
func countRouted(f *fixture, match func(harness.LogRecord) bool) int {
	n := 0
	for _, r := range f.d.WorkspaceLog(f.repo.Dir, "daemon") {
		if match(r) {
			n++
		}
	}
	return n
}

// awaitRouted waits until the workspace log holds WANT records MATCH accepts:
// the watcher writes one after each entry or update it routed.
func awaitRouted(t *testing.T, f *fixture, what string, match func(harness.LogRecord) bool, want int) {
	t.Helper()
	ctx, cancel := f.d.WaitCtx()
	defer cancel()
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		if got := countRouted(f, match); got >= want {
			return
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("waiting for %d routing records of %s, the log holds %d: %v", want, what, countRouted(f, match), ctx.Err())
		}
	}
}

// goldenIDs maps a drive's identities to this fixture's.
type goldenIDs struct {
	// mainBook is the drive's main agent: the book its turn's prompt is in.
	mainBook string
	// names maps each drive identity to the fixture's.
	names map[string]string
}

// goldenIdentities reads a drive's identities off its rows: the main agent
// is the book the turn's prompt was written to, and the turn is that prompt's.
// A drive with no prompt row is refused: every golden opens one turn.
func goldenIdentities(t *testing.T, name string, rows []goldenRow, turn string) goldenIDs {
	t.Helper()
	for _, r := range rows {
		if r.Item.Kind == "prompt" {
			return goldenIDs{mainBook: r.Book, names: map[string]string{r.Book: mainAgent, r.Turn: turn}}
		}
	}
	t.Fatalf("%s's frames.json writes no prompt row, so it names no main agent and no turn", name)
	return goldenIDs{}
}

// rename renames the drive's identities to the fixture's in RAW, an item of
// frames.json: every string value that IS a drive identity (a frame's
// agent_id, an announcement's owner, a prompt's turn) names the fixture's
// instead. A value merely containing one is left alone; only an identity is
// renamed.
func (ids goldenIDs) rename(t *testing.T, raw json.RawMessage) []byte {
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

// readGoldenNowMs reads the instant a scenario's drive began, which its
// meta.json states (the shim's HARNESS_NOW_MS).
func readGoldenNowMs(t *testing.T, name string) int64 {
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

// readFeedGolden reads a scenario's committed feed.json.
func readFeedGolden(t *testing.T, path string) feedGolden {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the daemon half's golden %s (write it with %s=1): %v", path, scenarioGoldensWriteEnv, err)
	}
	var g feedGolden
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

// writeFeedGolden writes a scenario's feed.json.
func writeFeedGolden(t *testing.T, path string, g feedGolden) {
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

// warningOperations is the sorted, distinct operations of RECORDS.
func warningOperations(records []harness.LogRecord) []string {
	seen := map[string]bool{}
	var out []string
	for _, r := range records {
		if !seen[r.Operation] {
			seen[r.Operation] = true
			out = append(out, r.Operation)
		}
	}
	sort.Strings(out)
	return out
}

// goldenInstants is every instant the shim's half states (a recorded place,
// a frame's own times): a feed value equal to one of them came from the
// frames and is kept, while any other wall-clock value is the daemon's own
// clock and is normalized away.
func goldenInstants(t *testing.T, rows []goldenRow) map[string]bool {
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

// normalizeFeed renders ROWS as canonical JSON with every value that differs
// between two projections of the same frames replaced by a stable token: the
// minted turn, the workspace id, each row id and order key by its first
// appearance, and every instant of the daemon's own clock.
func normalizeFeed(t *testing.T, rows []*frontendv1.FeedRow, turn, ws string, instants map[string]bool) json.RawMessage {
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

func TestMapJSONLeaves(t *testing.T) {
	tests := []struct {
		name string
		in   string
		drop []string
		want string
	}{
		{name: "every leaf is mapped with the key it sits under", in: `{"a":"x","b":{"c":"y"}}`, want: `{"a":"a=x","b":{"c":"c=y"}}`},
		{name: "an array's elements carry their array's key", in: `{"list":["x","y"]}`, want: `{"list":["list=x","list=y"]}`},
		{name: "a dropped field is removed, wherever it sits", in: `{"a":"x","gone":"y","b":{"gone":"z"}}`, drop: []string{"gone"}, want: `{"a":"a=x","b":{}}`},
		{name: "non-string leaves reach the mapper too", in: `{"n":1,"t":true}`, want: `{"n":"n=1","t":"t=true"}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var v any
			if err := json.Unmarshal([]byte(tt.in), &v); err != nil {
				t.Fatal(err)
			}
			drop := func(key string) bool { return slices.Contains(tt.drop, key) }

			// Act
			got := mapJSONLeaves(v, func(key string, leaf any) any { return key + "=" + fmt.Sprint(leaf) }, drop)

			// Assert
			raw, err := json.Marshal(got)
			if err != nil {
				t.Fatal(err)
			}
			if string(raw) != tt.want {
				t.Fatalf("mapJSONLeaves = %s, want %s", raw, tt.want)
			}
		})
	}
}

func TestMapJSONLeavesMintsTokensInSortedKeyOrder(t *testing.T) {
	// Arrange: one object decoded twice, whose map iteration order Go randomizes.
	minted := func() string {
		var v any
		if err := json.Unmarshal([]byte(`{"z":"a","m":"b","a":"c"}`), &v); err != nil {
			t.Fatal(err)
		}
		n := 0
		out := mapJSONLeaves(v, func(_ string, _ any) any { n++; return n }, nil)
		raw, _ := json.Marshal(out)
		return string(raw)
	}

	// Act / Assert
	if got := minted(); got != `{"a":1,"m":2,"z":3}` {
		t.Fatalf("tokens = %s, want them minted in sorted key order", got)
	}
}

func TestTheScenarioGoldensWalkJSONOnlyThroughMapJSONLeaves(t *testing.T) {
	// A hand-rolled walk drifts from the shared one's sorted order and drops;
	// only mapJSONLeaves may recurse over a decoded value here.
	src, err := os.ReadFile("scenariogoldens_test.go")
	if err != nil {
		t.Fatal(err)
	}
	if n := strings.Count(string(src), "\n\tvar walk func("); n != 1 {
		t.Fatalf("scenariogoldens_test.go declares %d JSON walks, want one, inside mapJSONLeaves", n)
	}
}
