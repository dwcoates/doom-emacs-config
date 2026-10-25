package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/stateroot"
	"agent-repld/internal/storagesweep"
)

// storageSweepLockName is the storage sweep's cross-process lock, in the
// kernel-lock directory: an incumbent and its handover successor never sweep
// at once.
const storageSweepLockName = "storage-sweep.lock"

// The cache directories the storage sweep ages, each with the override its
// WRITER honors (bin/profile-agent-repl.sh; testrun/internal/reuse), so the
// sweep reads the same directory the writer filled.
const (
	envProfileRoot = "AGENT_REPL_PROFILE_ROOT"
	envTestPasses  = "AGENT_REPL_TEST_PASSES"
)

// The profiling session root's own files: the open session's id, and the
// arming marker. Neither is a session.
const (
	profileActiveFile = "active"
	profileArmedFile  = "armed"
)

// buildStorageSweep builds the storage sweep over the state root's shim
// directories and the host's aged cache directories.
func buildStorageSweep(registry storagesweep.Registry, layout stateroot.Layout, runDir string, getenv func(string) string, log dlog.Logger) (*storagesweep.Sweeper, error) {
	aged, err := storageAgedDirs(getenv)
	if err != nil {
		log.Error(graphOperation, "the storage sweep's cache directories could not be resolved", dlog.Context{"cause": err.Error()})
		return nil, err
	}
	sweeper, err := storagesweep.New(storagesweep.Deps{
		Registry:    registry,
		Clock:       clock.System{},
		LockPath:    filepath.Join(runDir, storageSweepLockName),
		ShimDir:     layout.ShimDir(),
		Aged:        aged,
		LandedAfter: storagesweep.DefaultLandedAfter,
		ClosedAfter: storagesweep.DefaultClosedAfter,
		StartDelay:  storagesweep.DefaultStartDelay,
		Every:       storagesweep.DefaultEvery,
		Log:         log,
	})
	if err != nil {
		return nil, fmt.Errorf("agent-repld: build the storage sweep: %w", err)
	}
	return sweeper, nil
}

// storageAgedDirs are the cache directories swept by age.
func storageAgedDirs(getenv func(string) string) ([]storagesweep.AgedDir, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return nil, fmt.Errorf("resolve the home directory the caches live under: %w", err)
	}
	cache := filepath.Join(home, ".cache", "agent-repl")
	profiles := firstNonEmpty(getenv(envProfileRoot), filepath.Join(cache, "profiles"))
	return []storagesweep.AgedDir{
		{Name: "profiles", Dir: profiles, After: storagesweep.DefaultAgedAfter, Keep: func() (map[string]bool, error) {
			return openProfileSession(profiles)
		}},
		{Name: "test-passes", Dir: firstNonEmpty(getenv(envTestPasses), filepath.Join(cache, "test-passes")), After: storagesweep.DefaultAgedAfter},
	}, nil
}

// openProfileSession answers the profiling root's entries kept whatever their
// age: its own two files, and the session its active file names.
func openProfileSession(root string) (map[string]bool, error) {
	keep := map[string]bool{profileActiveFile: true, profileArmedFile: true}
	body, err := os.ReadFile(filepath.Join(root, profileActiveFile))
	if os.IsNotExist(err) {
		return keep, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read the open profiling session: %w", err)
	}
	if id := strings.TrimSpace(string(body)); id != "" {
		keep[id] = true
	}
	return keep, nil
}
