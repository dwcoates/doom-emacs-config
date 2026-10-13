package wsm

import "testing"

// undoLayout42 strips layout 42's addition (signedInTurnsDDL) from a migrated
// file, leaving its stamp alone.
func undoLayout42(t *testing.T, path string) {
	t.Helper()
	execRaw(t, path, `DROP TABLE signed_in_turns`)
}
