package e2e

import (
	"slices"
	"testing"
)

func TestThePrewarmCoversEveryPrimitiveTheScenariosRedefine(t *testing.T) {
	// Arrange / Act
	prewarmed := prewarmedFunctions(t)

	// Assert: the two primitives a scenario's prompt answers redefine, and
	// the boot's own, are all prewarmed.
	for _, name := range []string{"completing-read", "read-string", "yes-or-no-p", "set-window-buffer"} {
		if !slices.Contains(prewarmed, name) {
			t.Errorf("prewarmedFunctions = %v, want it to carry %s", prewarmed, name)
		}
	}
}
