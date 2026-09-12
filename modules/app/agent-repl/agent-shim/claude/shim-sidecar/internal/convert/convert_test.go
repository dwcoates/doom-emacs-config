package convert

// convert_test.go — the converter's own entry points and the vendor-spelling
// readers every conversion in the package leans on.

import "testing"

// A caller that meant to listen and passed nil would otherwise lose every
// spool attribution with no signal at all, so nil is refused rather than
// quietly treated as the no-op.
func TestSetObserverRefusesNilRatherThanLosingEveryAttribution(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	defer func() {
		if recover() == nil {
			t.Fatal("a nil observer was accepted")
		}
	}()

	// Act / Assert.
	c.SetObserver(nil)
}

// A line that PARSED but names no type is not unparsed — nothing can say what
// it is, which is exactly the unknown arm, and it is still stored.
func TestALineWithNoTypeIsStoredAsUnknownResidue(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, `{"uuid":"u1","timestamp":"`+ts1+`"}`)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("got %d entries, want the record stored exactly once", len(entries))
	}
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil {
		t.Fatalf("entry = %v, want the unknown residue arm", entries[0])
	}
	if unknown.GetDiscriminatorField() != "type" {
		t.Fatalf("discriminator field = %q, want %q", unknown.GetDiscriminatorField(), "type")
	}
	if unknown.GetRaw() == nil {
		t.Fatal("the unknown residue carries no raw record, so nothing was actually kept")
	}
}

// The disk carries both camelCase and snake_case spellings of one name, so a
// reader that only matched exactly would silently miss half the corpus.
func TestHasMatchesEitherVendorSpellingOfOneName(t *testing.T) {
	cases := []struct {
		name   string
		object map[string]any
		key    string
		want   bool
	}{
		{name: "the exact spelling", object: map[string]any{"toolUseID": "t"}, key: "toolUseID", want: true},
		{name: "the other spelling", object: map[string]any{"tool_use_id": "t"}, key: "toolUseID", want: true},
		{name: "a name that is simply absent", object: map[string]any{"other": "t"}, key: "toolUseID", want: false},
		{name: "an empty object", object: map[string]any{}, key: "toolUseID", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := has(tc.object, tc.key)

			// Assert.
			if got != tc.want {
				t.Fatalf("has(%v, %q) = %v, want %v", tc.object, tc.key, got, tc.want)
			}
		})
	}
}

func TestPickFallsBackToTheFoldedSpellingOfEveryCandidate(t *testing.T) {
	cases := []struct {
		name   string
		object map[string]any
		keys   []string
		want   any
	}{
		{name: "the first candidate, exactly", object: map[string]any{"command": "ls"}, keys: []string{"command", "line"}, want: "ls"},
		{name: "a later candidate, exactly", object: map[string]any{"line": "ls"}, keys: []string{"command", "line"}, want: "ls"},
		{name: "a candidate under the other spelling", object: map[string]any{"tool_use_id": "t"}, keys: []string{"toolUseID"}, want: "t"},
		{name: "no candidate at all", object: map[string]any{"other": "t"}, keys: []string{"command", "line"}, want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := pick(tc.object, tc.keys...)

			// Assert.
			if got != tc.want {
				t.Fatalf("pick(%v, %v) = %v, want %v", tc.object, tc.keys, got, tc.want)
			}
		})
	}
}

// A numeric optional reads only what JSON actually decodes as a number; a
// string standing where a count belongs is ABSENT, never coerced to a value
// the vendor never wrote.
func TestOptionalUint32ReadsOnlyAPresentNumber(t *testing.T) {
	cases := []struct {
		name   string
		object map[string]any
		want   *uint32
	}{
		{name: "absent", object: map[string]any{}, want: nil},
		{name: "explicitly null", object: map[string]any{"limit": nil}, want: nil},
		{name: "not a number", object: map[string]any{"limit": "25"}, want: nil},
		{name: "a number", object: map[string]any{"limit": float64(25)}, want: ptrUint32(25)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := optionalUint32(tc.object, "limit")

			// Assert.
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("optionalUint32 = %d, want an absent value", *got)
			case tc.want != nil && got == nil:
				t.Fatalf("optionalUint32 is absent, want %d", *tc.want)
			case tc.want != nil && *got != *tc.want:
				t.Fatalf("optionalUint32 = %d, want %d", *got, *tc.want)
			}
		})
	}
}

func TestOptionalInt64ReadsOnlyAPresentNumber(t *testing.T) {
	cases := []struct {
		name   string
		object map[string]any
		want   *int64
	}{
		{name: "absent", object: map[string]any{}, want: nil},
		{name: "explicitly null", object: map[string]any{"at": nil}, want: nil},
		{name: "not a number", object: map[string]any{"at": true}, want: nil},
		{name: "a number", object: map[string]any{"at": float64(-7)}, want: ptrInt64(-7)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := optionalInt64(tc.object, "at")

			// Assert.
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("optionalInt64 = %d, want an absent value", *got)
			case tc.want != nil && got == nil:
				t.Fatalf("optionalInt64 is absent, want %d", *tc.want)
			case tc.want != nil && *got != *tc.want:
				t.Fatalf("optionalInt64 = %d, want %d", *got, *tc.want)
			}
		})
	}
}

func ptrUint32(v uint32) *uint32 { return &v }
func ptrInt64(v int64) *int64    { return &v }

// TestAnUnmodeledLineTypeIsStoredAsResidueAtDebug pins the reclassified
// convert-line default arm. A line whose type is PRESENT but not yet modelled is
// stored whole as residue — that residue is the coverage and re-converts once
// the type is modelled — so a vendor adding a type is benign forward-compat, not
// a gap, and the trace is debug rather than a warn that floods a cold re-scan.
func TestAnUnmodeledLineTypeIsStoredAsResidueAtDebug(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)

	// Act.
	entries := convertLines(t, c, `{"type":"quantum_flux","uuid":"u1"}`)

	// Assert: the residue is kept (the coverage is unchanged)...
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1 (the unmodeled line stored whole as residue)", len(entries))
	}
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil || unknown.GetDiscriminator() != "quantum_flux" {
		t.Fatalf("residue = %v, want the unknown arm discriminated quantum_flux", entries[0])
	}
	// ...and only the severity drops.
	if got := levelForMessage(t, sink, "is not modeled"); got != "debug" {
		t.Fatalf("the unmodeled-line record was recorded at %q, want debug (benign forward-compat)", got)
	}
}
