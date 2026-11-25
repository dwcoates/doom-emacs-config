package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalStreamslotDir is the package these rules hold, under the daemon module's root.
const internalStreamslotDir = "internal/streamslot"

// TestEveryStreamScopedHolderIsBuiltOnSlot pins the holders that keep a value
// for one standing stream to the shared Slot: a holder that counted its own
// generations would drift from the superseding rule.
func TestEveryStreamScopedHolderIsBuiltOnSlot(t *testing.T) {
	sourcescan.InPackage(t, internalStreamslotDir)
	for _, file := range []string{"../desktopnotify/focus.go", "../procmetrics/emacs.go"} {
		t.Run(file, func(t *testing.T) {
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(string(src), "streamslot.Slot[") || strings.Contains(string(src), "generation++") {
				t.Fatalf("%s holds a stream-scoped value without streamslot.Slot", file)
			}
		})
	}
}
