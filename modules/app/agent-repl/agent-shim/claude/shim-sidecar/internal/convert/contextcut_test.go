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
	entry := entryByKey(t, entries, SessionKey("context_cut", "u1"))
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
	entry := entryByKey(t, entries, SessionKey("context_cut", "u1"))
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
	cut := frameOf(entryByKey(t, entries, SessionKey("context_cut", "u1"))).GetUpdate().GetContextCut()
	if cut.GetCompacted() != nil {
		t.Fatal("a clear must not be reported as a compaction, which is the only arm carrying a delta")
	}
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

func TestCompactionWithNoSummaryStillStatesTheCut(t *testing.T) {
	// Arrange. The cut is REAL and a reader must see WHERE, even when the summary
	// never arrived — but it is never silent (the branch logs at warning level).
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
	if got := compacted.GetSummary().GetMarkdown(); got != "" {
		t.Fatalf("summary = %q, want empty rather than invented", got)
	}
}
