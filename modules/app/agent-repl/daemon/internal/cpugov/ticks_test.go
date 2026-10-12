package cpugov

import "testing"

func TestFoldTicks(t *testing.T) {
	tests := []struct {
		name            string
		prev, now       [4]uint32
		primed          bool
		wantBusy, wantT uint64
	}{
		{name: "user, system and nice are busy", prev: [4]uint32{10, 20, 30, 40}, now: [4]uint32{15, 23, 40, 42}, primed: true, wantBusy: 10, wantT: 20},
		{name: "a counter that wrapped", prev: [4]uint32{0xFFFFFFF0, 0, 0, 0}, now: [4]uint32{0x10, 0, 0, 0}, primed: true, wantBusy: 0x20, wantT: 0x20},
		{name: "a first reading answers nothing", now: [4]uint32{5, 5, 5, 5}, wantBusy: 0, wantT: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			busy, total := foldTicks(tt.prev, tt.now, tt.primed)

			// Assert.
			if busy != tt.wantBusy || total != tt.wantT {
				t.Fatalf("foldTicks = %d/%d, want %d/%d", busy, total, tt.wantBusy, tt.wantT)
			}
		})
	}
}

func TestParseProcStat(t *testing.T) {
	tests := []struct {
		name            string
		text            string
		wantBusy, wantT uint64
		wantErr         bool
	}{
		{name: "idle and iowait are idle", text: "cpu  1 2 3 4 5 6 7 8 9 10\ncpu0 1 1 1 1 1 1 1 1\n", wantBusy: 1 + 2 + 3 + 6 + 7 + 8, wantT: 36},
		{name: "no aggregate line", text: "cpu0 1 2 3 4 5\n", wantErr: true},
		{name: "an unreadable count", text: "cpu 1 x 3 4 5\n", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			busy, total, err := parseProcStat(tt.text)

			// Assert.
			if (err != nil) != tt.wantErr {
				t.Fatalf("parseProcStat error = %v, want error %v", err, tt.wantErr)
			}
			if busy != tt.wantBusy || total != tt.wantT {
				t.Fatalf("parseProcStat = %d/%d, want %d/%d", busy, total, tt.wantBusy, tt.wantT)
			}
		})
	}
}
