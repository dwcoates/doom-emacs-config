package convert

// keepalive_test.go — which transcript records a keep-alive turn produced, by
// the transcript's own prompt and parent links, and the prefix seed that keeps
// the answer across a restart.

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"
)

// ---- chained record builders: every record names its parent ----

// kaLine encodes one record, turning an empty parent into the vendor's null.
func kaLine(t *testing.T, fields map[string]any) string {
	t.Helper()
	if parent, ok := fields["parentUuid"].(string); ok && parent == "" {
		fields["parentUuid"] = nil
	}
	fields["isSidechain"] = false
	fields["timestamp"] = ts1
	out, err := json.Marshal(fields)
	if err != nil {
		t.Fatalf("encode %v: %v", fields, err)
	}
	return string(out)
}

// kaPrompt is a prompt agent-repl submitted, opening the turn `promptID`.
func kaPrompt(t *testing.T, uuid, parent, promptID, text string) string {
	t.Helper()
	fields := map[string]any{
		"type": "user", "uuid": uuid, "parentUuid": parent, "entrypoint": "sdk-cli",
		"message": map[string]any{"role": "user", "content": []any{map[string]any{"type": "text", "text": text}}},
	}
	if promptID != "" {
		fields["promptId"] = promptID
	}
	return kaLine(t, fields)
}

// kaMeta is a harness-injected user record of the turn `promptID`.
func kaMeta(t *testing.T, uuid, parent, promptID, text string) string {
	t.Helper()
	return kaLine(t, map[string]any{
		"type": "user", "uuid": uuid, "parentUuid": parent, "promptId": promptID, "isMeta": true,
		"message": map[string]any{"role": "user", "content": text},
	})
}

// kaReply is an assistant text block.
func kaReply(t *testing.T, uuid, parent, messageID, text string) string {
	t.Helper()
	return kaLine(t, map[string]any{
		"type": "assistant", "uuid": uuid, "parentUuid": parent,
		"message": map[string]any{"id": messageID, "role": "assistant", "content": []any{map[string]any{"type": "text", "text": text}}},
	})
}

// kaCall is an assistant tool call.
func kaCall(t *testing.T, uuid, parent, messageID, callID string) string {
	t.Helper()
	return kaLine(t, map[string]any{
		"type": "assistant", "uuid": uuid, "parentUuid": parent,
		"message": map[string]any{"id": messageID, "role": "assistant", "content": []any{
			map[string]any{"type": "tool_use", "id": callID, "name": "Read", "input": map[string]any{"file_path": "/f"}},
		}},
	})
}

// kaResult is the user record a tool result is filed under, in turn `promptID`.
func kaResult(t *testing.T, uuid, parent, promptID, callID string) string {
	t.Helper()
	fields := map[string]any{
		"type": "user", "uuid": uuid, "parentUuid": parent,
		"message": map[string]any{"role": "user", "content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": callID, "content": "c"},
		}},
		"toolUseResult": map[string]any{"type": "text", "file": map[string]any{"filePath": "/f", "content": "c", "numLines": 1, "totalLines": 1}},
	}
	if promptID != "" {
		fields["promptId"] = promptID
	}
	return kaLine(t, fields)
}

// kaAttachment is an attachment record.
func kaAttachment(t *testing.T, uuid, parent string) string {
	t.Helper()
	return kaLine(t, map[string]any{
		"type": "attachment", "uuid": uuid, "parentUuid": parent,
		"attachment": map[string]any{"type": "hook_success", "hookName": "Stop", "hookEvent": "Stop", "content": "", "stdout": "", "stderr": "", "exitCode": 0},
	})
}

// entriesPerLine converts lines through ONE converter in file order, joining
// at `from`, and returns what each line produced.
func entriesPerLine(t *testing.T, c *Converter, from int64, lines ...string) []int {
	t.Helper()
	counts := make([]int, len(lines))
	for i, line := range lines {
		counts[i] = len(c.Line(decode(t, line), testAttribution(from+int64(i*1000)), nil))
	}
	return counts
}

// ka is the keep-alive prompt's text.
const ka = KeepaliveMarker + "\nRespond with the single character \".\" and nothing else. (1)"

// TestKeepaliveRecordsAreClassifiedByTheirLinks drives one edge per case: the
// SUBJECT is the last line, and every line before it is its arrangement.
func TestKeepaliveRecordsAreClassifiedByTheirLinks(t *testing.T) {
	cases := []struct {
		name       string
		lines      func(t *testing.T) []string
		wantStored bool
	}{
		{
			name: "the keep-alive prompt itself",
			lines: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka)}
			},
			wantStored: false,
		},
		{
			name: "the keep-alive's reply, parented on its prompt",
			lines: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka), kaReply(t, "k2", "k1", "msg_k", ".")}
			},
			wantStored: false,
		},
		{
			name: "an attachment deeper in the keep-alive's chain",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "pk", ka),
					kaReply(t, "k2", "k1", "msg_k", "."),
					kaAttachment(t, "k3", "k2"),
				}
			},
			wantStored: false,
		},
		{
			name: "a tool result carrying the keep-alive's promptId",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "pk", ka),
					kaCall(t, "k2", "k1", "msg_k", "toolu_k"),
					kaResult(t, "k3", "k2", "pk", "toolu_k"),
				}
			},
			wantStored: false,
		},
		{
			name: "a meta record the keep-alive's turn folded in",
			lines: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka), kaMeta(t, "k2", "k1", "pk", "a reminder")}
			},
			wantStored: false,
		},
		{
			name: "the keep-alive's reply landing AFTER a real prompt interleaved with it",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "anchor", "pk", ka),
					kaPrompt(t, "r1", "anchor", "pr", "a real question"),
					kaReply(t, "k2", "k1", "msg_k", "."),
				}
			},
			wantStored: false,
		},
		{
			name: "the real prompt's reply beside an interleaved keep-alive",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "anchor", "pk", ka),
					kaPrompt(t, "r1", "anchor", "pr", "a real question"),
					kaReply(t, "r2", "r1", "msg_r", "an answer"),
				}
			},
			wantStored: true,
		},
		{
			name: "a task notification's turn chained onto the keep-alive's last record",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "pk", ka),
					kaReply(t, "k2", "k1", "msg_k", "."),
					kaPrompt(t, "n1", "k2", "pn", "<task-notification>a background task finished</task-notification>"),
					kaReply(t, "n2", "n1", "msg_n", "noted"),
				}
			},
			wantStored: true,
		},
		{
			name: "a peer's meta prompt of its own turn chained onto the keep-alive",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "pk", ka),
					kaMeta(t, "p1", "k1", "pp", "Another Claude session sent a message"),
					kaReply(t, "p2", "p1", "msg_p", "replying"),
				}
			},
			wantStored: true,
		},
		{
			name: "a real prompt carrying keep-alives on, parented on the keep-alive",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "pk", ka),
					kaReply(t, "k2", "k1", "msg_k", "."),
					kaPrompt(t, "r1", "k2", "pr", "a real question"),
					kaReply(t, "r2", "r1", "msg_r", "an answer"),
				}
			},
			wantStored: true,
		},
		{
			name: "a prompt merely quoting the marker",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "q1", "", "pq", "what does "+KeepaliveMarker+" do?"),
					kaReply(t, "q2", "q1", "msg_q", "it marks a keep-alive"),
				}
			},
			wantStored: true,
		},
		{
			name: "a keep-alive prompt from a CLI that writes no promptId, and its reply",
			lines: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "", ka), kaReply(t, "k2", "k1", "msg_k", ".")}
			},
			wantStored: false,
		},
		{
			name: "a tool result from a CLI that writes no promptId follows its parent",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "", ka),
					kaCall(t, "k2", "k1", "msg_k", "toolu_k"),
					kaResult(t, "k3", "k2", "", "toolu_k"),
				}
			},
			wantStored: false,
		},
		{
			name: "a record naming no parent after a keep-alive",
			lines: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka), kaReply(t, "x1", "", "msg_x", "unchained")}
			},
			wantStored: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			lines := tc.lines(t)

			// Act.
			counts := entriesPerLine(t, c, 0, lines...)

			// Assert.
			if stored := counts[len(counts)-1] > 0; stored != tc.wantStored {
				t.Fatalf("subject produced %d entrie(s) (stored=%v), want stored=%v", counts[len(counts)-1], stored, tc.wantStored)
			}
		})
	}
}

// TestAKeepaliveSkipIsLoggedAtDebugWithNoUpsertKey asserts the one record each
// skip leaves: DEBUG, and naming no row, because none was stored.
func TestAKeepaliveSkipIsLoggedAtDebugWithNoUpsertKey(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)

	// Act.
	convertLines(t, c, kaPrompt(t, "k1", "", "pk", ka), kaReply(t, "k2", "k1", "msg_k", "."))

	// Assert.
	var skips []map[string]any
	for _, raw := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		var rec map[string]any
		if json.Unmarshal([]byte(raw), &rec) != nil {
			continue
		}
		if rec["operation"] == "keepalive-skip" {
			skips = append(skips, rec)
		}
	}
	if len(skips) != 2 {
		t.Fatalf("keepalive-skip records = %d, want one per record (2); log:\n%s", len(skips), sink.String())
	}
	for _, rec := range skips {
		if rec["level"] != "debug" {
			t.Errorf("keepalive-skip level = %v, want debug", rec["level"])
		}
		fields, _ := rec["context"].(map[string]any)
		if fields == nil {
			t.Fatalf("keepalive-skip record carries no context: %v", rec)
		}
		if _, named := fields["upsert_key"]; named {
			t.Errorf("keepalive-skip names upsert_key %v, but no row was stored", fields["upsert_key"])
		}
	}
}

// TestSeedKeepaliveCarriesTheClassificationAcrossTheJoin drives the restart
// edges: the converter joins past byte 0 and is handed the prefix first. The
// subject is the first line after the join.
func TestSeedKeepaliveCarriesTheClassificationAcrossTheJoin(t *testing.T) {
	cases := []struct {
		name       string
		prefix     func(t *testing.T) []string
		after      func(t *testing.T) string
		wantStored bool
	}{
		{
			name:       "the keep-alive's prompt is before the join, its reply after",
			prefix:     func(t *testing.T) []string { return []string{kaPrompt(t, "k1", "", "pk", ka)} },
			after:      func(t *testing.T) string { return kaReply(t, "k2", "k1", "msg_k", ".") },
			wantStored: false,
		},
		{
			name: "a real prompt interleaved before the join, the keep-alive's reply after",
			prefix: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "a0", "pk", ka), kaPrompt(t, "r1", "a0", "pr", "a real question")}
			},
			after:      func(t *testing.T) string { return kaReply(t, "k2", "k1", "msg_k", ".") },
			wantStored: false,
		},
		{
			name: "a task notification before the join, the keep-alive's reply after",
			prefix: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "k1", "", "pk", ka),
					kaPrompt(t, "n1", "k1", "pn", "<task-notification>done</task-notification>"),
				}
			},
			after:      func(t *testing.T) string { return kaReply(t, "k2", "k1", "msg_k", ".") },
			wantStored: false,
		},
		{
			name: "a tool result of the keep-alive after the join",
			prefix: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka), kaCall(t, "k2", "k1", "msg_k", "toolu_k")}
			},
			after:      func(t *testing.T) string { return kaResult(t, "k3", "k2", "pk", "toolu_k") },
			wantStored: false,
		},
		{
			name: "a real turn before the join, its reply after",
			prefix: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka), kaPrompt(t, "r1", "k1", "pr", "a real question")}
			},
			after:      func(t *testing.T) string { return kaReply(t, "r2", "r1", "msg_r", "an answer") },
			wantStored: true,
		},
		{
			name: "an undecodable prefix line is skipped, not fatal",
			prefix: func(t *testing.T) []string {
				return []string{kaPrompt(t, "k1", "", "pk", ka), `{"type":"assistant",` + KeepaliveMarker}
			},
			after:      func(t *testing.T) string { return kaReply(t, "k2", "k1", "msg_k", ".") },
			wantStored: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			prefix := strings.Join(tc.prefix(t), "\n") + "\n"
			if _, err := c.SeedKeepalive(strings.NewReader(prefix)); err != nil {
				t.Fatalf("SeedKeepalive: %v", err)
			}

			// Act.
			counts := entriesPerLine(t, c, int64(len(prefix)), tc.after(t))

			// Assert.
			if stored := counts[0] > 0; stored != tc.wantStored {
				t.Fatalf("the first record after the join produced %d entrie(s) (stored=%v), want stored=%v", counts[0], stored, tc.wantStored)
			}
		})
	}
}

// TestSeedKeepaliveTalliesWhatItRead asserts the tally the caller states.
func TestSeedKeepaliveTalliesWhatItRead(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	prefix := strings.Join([]string{
		kaPrompt(t, "r0", "", "p0", "hello"),
		kaReply(t, "r1", "r0", "msg_0", "hi"),
		kaPrompt(t, "k1", "r1", "pk", ka),
		kaReply(t, "k2", "k1", "msg_k", "."),
	}, "\n")

	// Act.
	seed, err := c.SeedKeepalive(strings.NewReader(prefix))

	// Assert.
	if err != nil {
		t.Fatalf("SeedKeepalive: %v", err)
	}
	if want := (KeepaliveSeed{Lines: 4, KeepaliveRecords: 2, KeepalivePrompts: 1}); seed != want {
		t.Fatalf("seed = %+v, want %+v", seed, want)
	}
}

// errPrefixRead stands in for a failed read of the prefix.
var errPrefixRead = errors.New("input/output error")

// failingReader yields its bytes and then fails.
type failingReader struct{ served bool }

func (r *failingReader) Read(p []byte) (int, error) {
	if r.served {
		return 0, errPrefixRead
	}
	r.served = true
	return copy(p, "{}\n"), nil
}

// TestSeedKeepaliveSurfacesAReadFailure asserts an I/O failure is returned,
// never absorbed into a partial classification.
func TestSeedKeepaliveSurfacesAReadFailure(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	_, err := c.SeedKeepalive(&failingReader{})

	// Assert.
	if !errors.Is(err, errPrefixRead) {
		t.Fatalf("SeedKeepalive error = %v, want it to wrap the read failure", err)
	}
}

// TestSeedKeepaliveRefusesAConverterThatAlreadyJoined asserts a seed that
// would land after the records depending on it is refused.
func TestSeedKeepaliveRefusesAConverterThatAlreadyJoined(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	entriesPerLine(t, c, 5000, kaReply(t, "x1", "", "msg_x", "already reading"))

	// Act.
	_, err := c.SeedKeepalive(strings.NewReader(kaPrompt(t, "k1", "", "pk", ka)))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "joined the file at offset 5000") {
		t.Fatalf("SeedKeepalive error = %v, want a refusal naming the join offset", err)
	}
}

// TestTheSeedAndTheLivePathReadTheSameFacts asserts the two decoders feed the
// rule identical inputs, one line shape per case.
func TestTheSeedAndTheLivePathReadTheSameFacts(t *testing.T) {
	cases := []struct {
		name string
		line func(t *testing.T) string
	}{
		{name: "a marked prompt", line: func(t *testing.T) string { return kaPrompt(t, "k1", "a0", "pk", ka) }},
		{name: "a prompt with no promptId", line: func(t *testing.T) string { return kaPrompt(t, "k1", "", "", ka) }},
		{name: "a meta record", line: func(t *testing.T) string { return kaMeta(t, "m1", "k1", "pk", "reminder") }},
		{name: "a tool result", line: func(t *testing.T) string { return kaResult(t, "k3", "k2", "pk", "toolu_k") }},
		{name: "an assistant reply", line: func(t *testing.T) string { return kaReply(t, "k2", "k1", "msg_k", ".") }},
		{name: "an attachment", line: func(t *testing.T) string { return kaAttachment(t, "k3", "k2") }},
		{name: "a message that is not an object", line: func(t *testing.T) string {
			return `{"type":"user","uuid":"u1","promptId":"p1","message":"` + KeepaliveMarker + `"}`
		}},
		{name: "a field of the wrong type", line: func(t *testing.T) string {
			return `{"type":"user","uuid":7,"isMeta":"true","promptId":"p1","message":{"content":"` + KeepaliveMarker + `"}}`
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			line := tc.line(t)

			// Act.
			seeded, ok := seedFacts([]byte(line))
			live := keepaliveFactsOf(decode(t, line))

			// Assert.
			if !ok {
				t.Fatalf("seedFacts refused a line the live path decodes: %s", line)
			}
			if seeded != live {
				t.Fatalf("seed facts %+v != live facts %+v", seeded, live)
			}
		})
	}
}
