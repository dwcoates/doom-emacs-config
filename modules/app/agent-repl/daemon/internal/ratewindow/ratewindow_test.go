package ratewindow

import (
	"testing"
	"time"
)

var instant = time.UnixMilli(1_700_000_000_000)

func TestAdmit(t *testing.T) {
	type call struct {
		at             time.Duration
		wantAdmitted   bool
		wantSuppressed int
	}
	tests := []struct {
		name  string
		calls []call
	}{
		{name: "the first record is admitted at once", calls: []call{{0, true, 0}}},
		{name: "a record inside the interval is turned away and counted", calls: []call{
			{0, true, 0}, {time.Second / 2, false, 1}, {time.Second * 3 / 4, false, 2},
		}},
		{name: "the record at the interval is admitted with the count it stood for", calls: []call{
			{0, true, 0}, {time.Second / 2, false, 1}, {time.Second, true, 1},
		}},
		{name: "an admitted record starts the count again", calls: []call{
			{0, true, 0}, {time.Second / 2, false, 1}, {time.Second, true, 1}, {time.Second * 3 / 2, false, 1},
		}},
		{name: "the interval runs from the last admitted record", calls: []call{
			{0, true, 0}, {time.Second, true, 0}, {time.Second * 3 / 2, false, 1}, {time.Second * 2, true, 1},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w := Window{Every: time.Second}

			for i, c := range tt.calls {
				// Act.
				admitted, suppressed := w.Admit(instant.Add(c.at))

				// Assert.
				if admitted != c.wantAdmitted || suppressed != c.wantSuppressed {
					t.Fatalf("call %d at %v = (%v, %d), want (%v, %d)", i, c.at, admitted, suppressed, c.wantAdmitted, c.wantSuppressed)
				}
			}
		})
	}
}
