package apiresponses

import "testing"

// frame is one activity frame the ledger observes.
type frame struct {
	unit         string
	carriesUsage bool
	settles      bool
}

func TestLedgerUnaccounted(t *testing.T) {
	tests := []struct {
		name   string
		frames []frame
		want   int
	}{
		{
			name:   "a response whose own unit carried the usage is accounted for",
			frames: []frame{{unit: "msg:0", carriesUsage: true, settles: true}},
			want:   0,
		},
		{
			name: "a response whose usage rode the thinking unit that opened it is accounted for",
			frames: []frame{
				{unit: "msg:0", carriesUsage: true},
				{unit: "msg:1", settles: true},
			},
			want: 0,
		},
		{
			name: "two response units of ONE api response share its single usage stamp",
			frames: []frame{
				{unit: "msg:0", carriesUsage: true, settles: true},
				{unit: "msg:1"},
				{unit: "msg:2", settles: true},
			},
			want: 0,
		},
		{
			name:   "a response in an api response that carried no usage at all is unaccounted",
			frames: []frame{{unit: "msg:0", settles: true}},
			want:   1,
		},
		{
			name: "a unit's usage arriving on a later frame accounts for the response it was filed under",
			frames: []frame{
				{unit: "msg:0"},
				{unit: "msg:0", carriesUsage: true, settles: true},
			},
			want: 0,
		},
		{
			name: "a settling frame does not re-open a response for a unit already filed",
			frames: []frame{
				{unit: "msg:0", carriesUsage: true},
				{unit: "msg:1"},
				{unit: "msg:1", settles: true},
			},
			want: 0,
		},
		{
			name: "a response settled before any usage was ever seen stays unaccounted once a later one carries some",
			frames: []frame{
				{unit: "a:0", settles: true},
				{unit: "b:0", carriesUsage: true, settles: true},
			},
			want: 1,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			ledger := New()

			// Act.
			for _, f := range test.frames {
				ledger.Observe(f.unit, f.carriesUsage)
				if f.settles {
					ledger.Settle(f.unit)
				}
			}

			// Assert.
			if got := ledger.Unaccounted(); got != test.want {
				t.Fatalf("Unaccounted() = %d, want %d", got, test.want)
			}
		})
	}
}
