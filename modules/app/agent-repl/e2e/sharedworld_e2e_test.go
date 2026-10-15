package e2e

import "testing"

// TestTheSharedWorldGivesEachTestItsOwnViewOfOneDaemon drives a turn in two
// tests over the shared World: both reach the one daemon process, each
// through its own view and its own fresh workspace, and each turn ends.
func TestTheSharedWorldGivesEachTestItsOwnViewOfOneDaemon(t *testing.T) {
	t.Parallel()
	pids := make([]int, 2)
	dirs := make([]string, 2)
	for i := range pids {
		t.Run("a test", func(t *testing.T) {
			// Arrange
			w := SharedWorld(t)
			ws := w.NewWorkspace(t)

			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-max-turns")

			// Assert
			if row := AwaitTurnEnded(t, w, ws, turn); row.GetTurnEnded().GetErrored().GetMaxTurns() == nil {
				t.Errorf("turn %s ended %v, want Errored{MaxTurns}", turn.GetValue(), row)
			}
			if !w.IsView() {
				t.Errorf("SharedWorld's daemon is not a view")
			}
			pids[i], dirs[i] = w.PID(), ws.GetDir()
		})
	}
	if pids[0] != pids[1] {
		t.Errorf("the two tests reached daemons %d and %d, want the one shared process", pids[0], pids[1])
	}
	if dirs[0] == dirs[1] {
		t.Errorf("the two tests shared workspace %s, want one fresh workspace each", dirs[0])
	}
}
