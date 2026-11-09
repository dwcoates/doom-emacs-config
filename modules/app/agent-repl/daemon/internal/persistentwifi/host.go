package persistentwifi

import (
	"context"
	"fmt"
	"path/filepath"

	"agent-repld/internal/kernelevent"
)

// THE HOST'S STANDING IS HEARD, NEVER RE-READ ON A BEAT (owner ruling,
// 2026-11-09). The two facts the standing reads change in two places, and the
// operating system says when each does:
//
//   - the Wi-Fi link: a network joined, lost or swapped moves an address, which
//     the daemon's one network listener hears (internal/netwatch);
//   - the mode: `pmset` writes the power settings into the power-management
//     preference files, which the kernel follows (PowerChanges).
//
// Each change reads the standing at once, so a change made outside agent-repl
// reaches the topbar as it happens rather than within a cadence, and a lost
// network is answered by the hotspot failover the moment it is lost.

// PowerPrefsDir is where macOS keeps the power settings `pmset` writes.
const PowerPrefsDir = "/Library/Preferences"

// powerPrefsPattern names the power-management preference files in
// PowerPrefsDir: the system's and each per-host one.
const powerPrefsPattern = "com.apple.PowerManagement*.plist"

// PowerChanges is the Source of the power settings changing: the preference
// directory (an atomic rewrite is an entry change) and every power-management
// file in it (a rewrite in place). A fake host (EnvLid set) has no real power
// settings to hear, so its source never fires: its mode changes only through
// Update, which publishes at once.
func (c Config) PowerChanges() kernelevent.Source {
	if c.FakeLid != "" {
		return quietSource
	}
	return func(ctx context.Context) (<-chan error, func()) {
		return kernelevent.Follow(ctx, powerPrefsPaths(PowerPrefsDir)...)
	}
}

// powerPrefsPaths answers dir and every power-management preference file in
// it.
func powerPrefsPaths(dir string) []string {
	files, err := filepath.Glob(filepath.Join(dir, powerPrefsPattern))
	if err != nil {
		// The pattern is this package's constant; a bad one is a defect.
		panic(fmt.Sprintf("persistentwifi: the power preference pattern %q is malformed: %v", powerPrefsPattern, err))
	}
	return append([]string{dir}, files...)
}

// quietSource is a Source that never fires and never fails.
func quietSource(context.Context) (<-chan error, func()) {
	return make(chan error), func() {}
}
