package convert

// instants_test.go — the vendor's timestamps: what an unreadable one yields,
// and which instants are allowed to be absent.

import "testing"

// A record whose timestamp cannot be read yields 0, which callers express as
// "the producer observed no instant" — never as the epoch.
func TestParseInstantYieldsZeroForATimestampItCannotRead(t *testing.T) {
	cases := []struct {
		name  string
		value string
		want  int64
	}{
		{name: "no timestamp at all", value: "", want: 0},
		{name: "not a timestamp", value: "yesterday", want: 0},
		{name: "a truncated RFC3339", value: "2026-09-05T16", want: 0},
		{name: "a whole RFC3339 with millis", value: "2026-09-05T00:00:00.500Z", want: 1788566400500},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := parseInstant(tc.value)

			// Assert.
			if got != tc.want {
				t.Fatalf("parseInstant(%q) = %d, want %d", tc.value, got, tc.want)
			}
		})
	}
}

// The settle instant is OPTIONAL, so an unobserved one is left unset rather
// than written as a zero a reader would render as the epoch.
func TestSettledAtIsUnsetWhenNoInstantWasObserved(t *testing.T) {
	// Act.
	got := settledAt(0, 5)

	// Assert.
	if got != nil {
		t.Fatalf("settledAt(0, 5) = %v, want an unset instant", got)
	}
}

// A settle restates the start it closes, so the settled frame alone states a
// runtime; a settle whose start is unknown leaves the restated start unset.
func TestSettledAtRestatesTheStartItCloses(t *testing.T) {
	cases := []struct {
		name    string
		startMs int64
		want    *int64
	}{
		{name: "a known start is restated", startMs: 1_000, want: ptrInt64(1_000)},
		{name: "an unknown start stays unset", startMs: 0, want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := settledAt(4_000, tc.startMs).GetStartedAt()

			// Assert.
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("started_at = %v, want unset", got)
			case tc.want != nil && got.GetAtMs() != *tc.want:
				t.Fatalf("started_at = %v, want %d", got, *tc.want)
			}
		})
	}
}

// The START instant is NON-optional on every start arm, so even a zero is
// carried: it is what the vendor effectively gave us.
func TestStartedAtCarriesAZeroTheVendorEffectivelyGaveUs(t *testing.T) {
	// Act.
	got := startedAt(0)

	// Assert.
	if got == nil {
		t.Fatal("startedAt(0) dropped a non-optional instant")
	}
	if got.GetAtMs() != 0 {
		t.Fatalf("started at = %d, want 0", got.GetAtMs())
	}
}
