package vendortraffic

import (
	"strings"
	"testing"
)

// reading is one update fed to a ledger.
type reading struct {
	ref      uint64
	counts   Counts
	closing  bool
	baseline bool
}

func TestLedgerCountsEachByteOnce(t *testing.T) {
	cases := []struct {
		name     string
		readings []reading
		want     Counts
		live     int
	}{
		{
			name:     "a new source counts in full",
			readings: []reading{{ref: 1, counts: Counts{Received: 100, Sent: 10}}},
			want:     Counts{Received: 100, Sent: 10},
			live:     1,
		},
		{
			name: "a source's later update counts only what is new",
			readings: []reading{
				{ref: 1, counts: Counts{Received: 100, Sent: 10}},
				{ref: 1, counts: Counts{Received: 250, Sent: 15}},
			},
			want: Counts{Received: 250, Sent: 15},
			live: 1,
		},
		{
			name: "a closing update counts its tail and drops the source",
			readings: []reading{
				{ref: 1, counts: Counts{Received: 100, Sent: 10}},
				{ref: 1, counts: Counts{Received: 180, Sent: 12}, closing: true},
			},
			want: Counts{Received: 180, Sent: 12},
			live: 0,
		},
		{
			name:     "a source opened and closed between polls counts its final counts",
			readings: []reading{{ref: 7, counts: Counts{Received: 4_000_000, Sent: 4_000_000}, closing: true}},
			want:     Counts{Received: 4_000_000, Sent: 4_000_000},
			live:     0,
		},
		{
			name: "a baselined source counts nothing at first sighting",
			readings: []reading{
				{ref: 1, counts: Counts{Received: 900, Sent: 90}, baseline: true},
			},
			want: Counts{},
			live: 1,
		},
		{
			name: "a baselined source counts what it moves afterwards",
			readings: []reading{
				{ref: 1, counts: Counts{Received: 900, Sent: 90}, baseline: true},
				{ref: 1, counts: Counts{Received: 950, Sent: 99}},
			},
			want: Counts{Received: 50, Sent: 9},
			live: 1,
		},
		{
			name: "a replaced process's new sockets count from zero beside the old ones' finals",
			readings: []reading{
				{ref: 1, counts: Counts{Received: 300, Sent: 30}},
				{ref: 1, counts: Counts{Received: 320, Sent: 31}, closing: true},
				{ref: 2, counts: Counts{Received: 5, Sent: 1}},
			},
			want: Counts{Received: 325, Sent: 32},
			live: 1,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			l := newLedger()
			var got Counts

			// Act.
			for _, r := range tc.readings {
				delta, err := l.observe(r.ref, r.counts, r.closing, r.baseline)
				if err != nil {
					t.Fatalf("observe: %v", err)
				}
				got = got.Plus(delta)
			}

			// Assert.
			if got != tc.want {
				t.Fatalf("counted %+v, want %+v", got, tc.want)
			}
			if l.sources() != tc.live {
				t.Fatalf("live sources = %d, want %d", l.sources(), tc.live)
			}
		})
	}
}

func TestLedgerRefusesAShrinkingCounter(t *testing.T) {
	// Arrange.
	l := newLedger()
	if _, err := l.observe(1, Counts{Received: 100, Sent: 10}, false, false); err != nil {
		t.Fatalf("observe: %v", err)
	}

	// Act.
	delta, err := l.observe(1, Counts{Received: 40, Sent: 10}, false, false)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "never shrink") {
		t.Fatalf("observe error = %v, want the shrinking counter named", err)
	}
	if !delta.IsZero() {
		t.Fatalf("a shrinking counter counted %+v, want nothing", delta)
	}
}

func TestLedgerReanchorsAfterAShrinkingCounter(t *testing.T) {
	// Arrange.
	l := newLedger()
	_, _ = l.observe(1, Counts{Received: 100, Sent: 10}, false, false)
	_, _ = l.observe(1, Counts{Received: 40, Sent: 10}, false, false)

	// Act.
	delta, err := l.observe(1, Counts{Received: 60, Sent: 12}, false, false)

	// Assert.
	if err != nil {
		t.Fatalf("observe: %v", err)
	}
	if want := (Counts{Received: 20, Sent: 2}); delta != want {
		t.Fatalf("counted %+v after the re-anchor, want %+v", delta, want)
	}
}

func TestLedgerRemoveOfAnUnseenSourceIsOrdinary(t *testing.T) {
	// Arrange.
	l := newLedger()
	_, _ = l.observe(1, Counts{Received: 1}, false, false)

	// Act.
	l.remove(99)

	// Assert.
	if l.sources() != 1 {
		t.Fatalf("live sources = %d, want the one source untouched", l.sources())
	}
}

func TestLedgerRemoveDropsASeenSource(t *testing.T) {
	// Arrange.
	l := newLedger()
	_, _ = l.observe(1, Counts{Received: 1}, false, false)

	// Act.
	l.remove(1)

	// Assert.
	if l.sources() != 0 {
		t.Fatalf("live sources = %d, want none", l.sources())
	}
}
