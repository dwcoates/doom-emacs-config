package convert

// contextcut_test.go — /clear detection through the expanded command envelope.

import "testing"

// clearEnvelope is what the harness ACTUALLY writes for `/clear`. The literal
// "/clear" never appears on disk, so anything matching raw prompt text against it
// misses every real session.
func clearEnvelope(uuid, inner string) string {
	return `{"type":"user","uuid":"` + uuid + `","isSidechain":false,"timestamp":"` + ts1 +
		`","message":{"role":"user","content":` + quote(inner) + `}}`
}

func TestClearIsDetectedByUnwrappingTheCommandEnvelope(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	inner := "<command-name>/clear</command-name><command-message>clear</command-message><command-args></command-args>"

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", inner))

	// Assert.
	entry := entryByKey(t, entries, SessionKey("context_cut", "session-uuid"))
	cut := frameOf(entry).GetUpdate().GetContextCut()
	if cut.GetCleared() == nil {
		t.Fatal("an unwrapped /clear must land on the cleared arm")
	}
}

func TestClearWithArgumentsIsNotAClear(t *testing.T) {
	// Arrange. An argument means the user asked for something else.
	c := newTestConverter(t)
	inner := "<command-name>/clear</command-name><command-args>everything</command-args>"

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", inner))

	// Assert.
	for _, e := range entries {
		if frameOf(e).GetUpdate().GetContextCut() != nil {
			t.Fatal("/clear with an argument must not be read as a context clear")
		}
	}
}

func TestQuotedCommandEnvelopeIsNotAClear(t *testing.T) {
	// Arrange. Prose around the envelope means the prompt merely QUOTES a command
	// — a pasted transcript, a tool result echoing one — rather than invoking it.
	c := newTestConverter(t)
	inner := "look at this: <command-name>/clear</command-name> is what I ran"

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", inner))

	// Assert.
	for _, e := range entries {
		if frameOf(e).GetUpdate().GetContextCut() != nil {
			t.Fatal("a quoted command must not be read as an invocation")
		}
	}
}

func TestABareSlashClearPromptIsStillRecognized(t *testing.T) {
	// Arrange. A rehydrated or replayed session can carry the bare literal.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", "/clear"))

	// Assert.
	entry := entryByKey(t, entries, SessionKey("context_cut", "session-uuid"))
	if frameOf(entry).GetUpdate().GetContextCut().GetCleared() == nil {
		t.Fatal("a bare /clear must be recognized too")
	}
}

func TestAnotherCommandIsNotAClear(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	inner := "<command-name>/compact</command-name><command-args></command-args>"

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", inner))

	// Assert.
	for _, e := range entries {
		if frameOf(e).GetUpdate().GetContextCut() != nil {
			t.Fatal("only /clear produces the cleared arm here")
		}
	}
}

func TestClearCarriesNoTokenDelta(t *testing.T) {
	// Arrange. The vendor's conversation-reset record carries NO token delta, so
	// none is claimed — and clearing does not empty the context anyway (the system
	// prompt, skills and memory files are reloaded), so a fabricated zero would be
	// a lie a reader could see.
	c := newTestConverter(t)
	inner := "<command-name>/clear</command-name><command-args></command-args>"

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", inner))

	// Assert: the cleared arm has no token field at all, which is the schema's
	// own statement of this. Asserting the arm is what pins the choice.
	cut := frameOf(entryByKey(t, entries, SessionKey("context_cut", "session-uuid"))).GetUpdate().GetContextCut()
	if cut.GetCompacted() != nil {
		t.Fatal("a clear must not be reported as a compaction, which is the only arm carrying a delta")
	}
}

func TestAClearIsKeyedByTheSessionItRotatedTo(t *testing.T) {
	// Arrange. ONE CLEAR IS ONE ROW, and the two planes see DIFFERENT records
	// for it: this reader's only evidence is the `/clear` envelope, the shim's
	// is the SDK `conversation_reset`, and their uuids are unrelated
	// (identity-rotation-clear: `04f97c00-…` here, `cc07c2a0-…` there). The one
	// identity both can mint is the session the clear rotated to — the vendor
	// writes the envelope into the NEW transcript, so it is this file's own
	// session uuid.
	c := newTestConverter(t)
	inner := "<command-name>/clear</command-name><command-args></command-args>"

	// Act.
	entries := convertLines(t, c, clearEnvelope("the-record-uuid", inner))

	// Assert.
	for _, e := range entries {
		if e.GetUpsertKey() == SessionKey("context_cut", "the-record-uuid") {
			t.Fatal("a clear keyed by its own record uuid can never collide with the stream plane's write, and the feed draws the divider twice")
		}
	}
	entryByKey(t, entries, SessionKey("context_cut", "session-uuid"))
}

func TestAClearInAFileThatNamesNoSessionFallsBackToItsRecordUUID(t *testing.T) {
	// Arrange. A cut is still recorded when the one shared identity is missing —
	// a reader must see WHERE the conversation was cut — and the converter says
	// out loud that this key cannot collapse with the other plane's.
	c := newTestConverter(t)
	inner := "<command-name>/clear</command-name><command-args></command-args>"
	record := decode(t, clearEnvelope("u1", inner))
	at := testAttribution(0)
	at.VendorSessionID = ""

	// Act.
	entries := c.Line(record, at, nil)

	// Assert.
	entryByKey(t, entries, SessionKey("context_cut", "u1"))
}

func TestAutomaticCompactionIsDistinguishedFromAManualOne(t *testing.T) {
	// Arrange. THE TWO ARE DIFFERENT EVENTS FOR THE USER: a manual compaction is
	// something they did; an automatic one HAPPENED to them. Drawing them
	// identically is the most misleading thing this message can do.
	cases := []struct {
		trigger     string
		wantAutomat bool
	}{
		{trigger: "manual", wantAutomat: false},
		{trigger: "auto", wantAutomat: true},
	}
	for _, tc := range cases {
		t.Run(tc.trigger, func(t *testing.T) {
			c := newTestConverter(t)
			line := `{"type":"system","subtype":"compact_boundary","uuid":"b1","isSidechain":false,` +
				`"timestamp":"` + ts1 + `","compactMetadata":{"trigger":"` + tc.trigger +
				`","preTokens":100,"postTokens":10,"durationMs":5}}`
			summaryLine := `{"type":"user","uuid":"s1","isCompactSummary":true,"isSidechain":false,` +
				`"timestamp":"` + ts1 + `","message":{"role":"user","content":"summary"}}`

			// Act.
			entries := convertLines(t, c, line, summaryLine)

			// Assert.
			compacted := frameOf(entryByKey(t, entries, SessionKey("context_cut", "b1"))).GetUpdate().GetContextCut().GetCompacted()
			if tc.wantAutomat && compacted.GetAutomatic() == nil {
				t.Fatal("an auto trigger must land on the automatic arm")
			}
			if !tc.wantAutomat && compacted.GetRequested() == nil {
				t.Fatal("a manual trigger must land on the requested arm")
			}
		})
	}
}

// TestCompactionWithNoSummaryStatesThePlaceholder pins what a reader is TOLD
// when the vendor wrote no summary. The cut is REAL and must be drawn, and an
// empty summary drew it as a hole — indistinguishable, from the feed, from a
// summary this reader lost. The placeholder is the reader's own sentence about
// the condition, never a summary of anything, and a real summary supersedes it.
func TestCompactionWithNoSummaryStatesThePlaceholder(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	line := `{"type":"system","subtype":"compact_boundary","uuid":"b1","isSidechain":false,` +
		`"timestamp":"` + ts1 + `","compactMetadata":{"trigger":"manual","preTokens":100,"postTokens":10}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	compacted := frameOf(entryByKey(t, entries, SessionKey("context_cut", "b1"))).GetUpdate().GetContextCut().GetCompacted()
	if compacted == nil {
		t.Fatal("a summary-less boundary must still produce the cut")
	}
	if got := compacted.GetSummary().GetMarkdown(); got != NoSummaryWritten {
		t.Fatalf("summary = %q, want the stated placeholder %q", got, NoSummaryWritten)
	}
}

// TestASummarylessCompactionIsStatedAtInfo follows the placeholder: the render
// is no longer degraded, so the condition is an account of what the vendor did
// rather than a defect of this reader's.
func TestASummarylessCompactionIsStatedAtInfo(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)
	line := `{"type":"system","subtype":"compact_boundary","uuid":"b1","isSidechain":false,` +
		`"timestamp":"` + ts1 + `","compactMetadata":{"trigger":"manual","preTokens":100,"postTokens":10}}`

	// Act.
	convertLines(t, c, line)

	// Assert.
	if got := levelForMessage(t, sink, "carries no summary on the following line"); got != "info" {
		t.Fatalf("the summary-less boundary was recorded at %q, want info", got)
	}
}

// TestASummaryThatArrivesLaterSupersedesThePlaceholder is the shape the owner's
// own transcript carries: a `system/scheduled_task_fire` between the boundary
// and its summary, and the summary naming the boundary as its parent. The
// boundary's one-record lookahead cannot see past the intervening line, so the
// summary would otherwise be dropped by the branch that folds it into a cut it
// never reached.
func TestASummaryThatArrivesLaterSupersedesThePlaceholder(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	boundary := `{"type":"system","subtype":"compact_boundary","uuid":"b1","isSidechain":false,` +
		`"timestamp":"` + ts1 + `","compactMetadata":{"trigger":"manual","preTokens":100,"postTokens":10}}`
	between := `{"type":"system","subtype":"scheduled_task_fire","uuid":"s1","isSidechain":false,"timestamp":"` + ts1 + `","content":"resuming"}`
	summary := `{"type":"user","uuid":"u1","parentUuid":"b1","isCompactSummary":true,"isSidechain":false,` +
		`"timestamp":"` + ts2 + `","message":{"role":"user","content":"the story so far"}}`

	// Act.
	entries := convertLines(t, c, boundary, between, summary)

	// Assert: ONE key, carrying the real summary — the later write supersedes
	// the placeholder rather than standing a second cut beside it.
	var cuts int
	var carried string
	for _, e := range entries {
		compacted := frameOf(e).GetUpdate().GetContextCut().GetCompacted()
		if compacted == nil {
			continue
		}
		cuts++
		carried = compacted.GetSummary().GetMarkdown()
		if got := e.GetUpsertKey(); got != SessionKey("context_cut", "b1") {
			t.Fatalf("the cut was written under %q, want the boundary's own key", got)
		}
	}
	if cuts != 2 {
		t.Fatalf("the file produced %d cut writes, want the placeholder and its supersession", cuts)
	}
	if carried != "the story so far" {
		t.Fatalf("the superseding cut carries %q, want the summary that named the boundary", carried)
	}
}

// TestASummaryNamingAnotherBoundaryDoesNotSupersedeThisCut keeps the late
// attachment honest: distance is only safe because the summary NAMES its
// boundary, so one that names something else must leave the placeholder alone.
func TestASummaryNamingAnotherBoundaryDoesNotSupersedeThisCut(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	boundary := `{"type":"system","subtype":"compact_boundary","uuid":"b1","isSidechain":false,` +
		`"timestamp":"` + ts1 + `","compactMetadata":{"trigger":"manual","preTokens":100,"postTokens":10}}`
	between := `{"type":"system","subtype":"scheduled_task_fire","uuid":"s1","isSidechain":false,"timestamp":"` + ts1 + `","content":"resuming"}`
	stray := `{"type":"user","uuid":"u1","parentUuid":"b-other","isCompactSummary":true,"isSidechain":false,` +
		`"timestamp":"` + ts2 + `","message":{"role":"user","content":"someone else's history"}}`

	// Act.
	entries := convertLines(t, c, boundary, between, stray)

	// Assert.
	compacted := frameOf(entryByKey(t, entries, SessionKey("context_cut", "b1"))).GetUpdate().GetContextCut().GetCompacted()
	if got := compacted.GetSummary().GetMarkdown(); got != NoSummaryWritten {
		t.Fatalf("summary = %q, want the placeholder left standing", got)
	}
}

// The harness writes the summary either as a bare string or as content blocks,
// and the cut has to carry the text in both spellings — the summary IS the
// discarded history, so losing it to a shape difference loses the history.
func TestACompactionSummaryWrittenAsContentBlocksIsStillCarried(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	boundary := `{"type":"system","subtype":"compact_boundary","uuid":"b1","isSidechain":false,` +
		`"timestamp":"` + ts1 + `","compactMetadata":{"trigger":"auto","preTokens":100,"postTokens":10}}`
	summary := `{"type":"user","uuid":"s1","isCompactSummary":true,"isSidechain":false,` +
		`"timestamp":"` + ts1 + `","message":{"role":"user","content":[` +
		`{"type":"text","text":"first half"},` +
		`{"type":"thinking","thinking":"ignored"},` +
		`{"type":"text","text":"second half"}]}}`

	// Act.
	entries := convertLines(t, c, boundary, summary)

	// Assert: both text blocks, joined, and nothing from the non-text block.
	got := frameOf(entryByKey(t, entries, SessionKey("context_cut", "b1"))).
		GetUpdate().GetContextCut().GetCompacted().GetSummary().GetMarkdown()
	if got != "first half\nsecond half" {
		t.Fatalf("summary = %q, want both text blocks joined", got)
	}
}

// An UNTERMINATED command tag is not an element, so the envelope never parses
// and the prompt stays an ordinary prompt rather than being read as a command
// whose name happens to be the rest of the line.
func TestAnUnterminatedCommandTagIsNotACommandEnvelope(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, clearEnvelope("u1", "<command-name>/clear"))

	// Assert.
	for _, entry := range entries {
		if frameOf(entry).GetUpdate().GetContextCut() != nil {
			t.Fatal("an unterminated command tag was read as a /clear")
		}
	}
}

// TestIsClearCommandIsTheRuleTheConverterDrawsACutBy pins the exported helper
// to the converter's own verdict, one edge case per row: the rotation hold in
// the reader and the converter here must never disagree about what a clear is.
func TestIsClearCommandIsTheRuleTheConverterDrawsACutBy(t *testing.T) {
	cases := []struct {
		name    string
		message map[string]any
		want    bool
	}{
		{"the expanded envelope", map[string]any{"content": "<command-name>/clear</command-name>\n<command-message>clear</command-message>\n<command-args></command-args>"}, true},
		{"a bare /clear prompt", map[string]any{"content": "/clear"}, true},
		{"an argument", map[string]any{"content": "<command-name>/clear</command-name><command-args>everything</command-args>"}, false},
		{"a quoted envelope", map[string]any{"content": "see <command-name>/clear</command-name> here"}, false},
		{"another command", map[string]any{"content": "<command-name>/compact</command-name>"}, false},
		{"a blocks-form message", map[string]any{"content": []any{map[string]any{"type": "text", "text": "/clear"}}}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)

			// Act.
			got := IsClearCommand(tc.message)

			// Assert.
			if got != tc.want {
				t.Errorf("IsClearCommand(%v) = %v, want %v", tc.message, got, tc.want)
			}
			if shared := c.isClearCommand(tc.message); shared != got {
				t.Errorf("the converter's isClearCommand = %v, the exported rule = %v; the two spellings diverged", shared, got)
			}
		})
	}
}
