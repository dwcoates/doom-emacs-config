package procgroup

import "testing"

// TestStateOf pins the classification of the process-table entries a live
// process cannot be put into on demand: the instant between the exit event
// and the zombie is too short to arrange, so the entry is built directly.
func TestStateOf(t *testing.T) {
	cases := []struct {
		name       string
		stat       int8
		flag       int32
		wantFrozen bool
		wantExited bool
	}{
		{
			name:       "a running process whose exit is under way has exited",
			stat:       pStatRun,
			flag:       pWExit,
			wantFrozen: true,
			wantExited: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := StateOf(tc.stat, tc.flag)

			// Assert
			if err != nil {
				t.Fatalf("StateOf(%d, %#x) = %v", tc.stat, tc.flag, err)
			}
			if got.Frozen != tc.wantFrozen || got.Exited != tc.wantExited {
				t.Fatalf("StateOf(%d, %#x) = %+v, want frozen %v, exited %v", tc.stat, tc.flag, got, tc.wantFrozen, tc.wantExited)
			}
		})
	}
}
