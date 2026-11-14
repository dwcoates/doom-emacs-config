package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

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
// directories, the host's aged cache directories, and the logs the surfaces
// write under the state root.
func buildStorageSweep(registry storagesweep.Registry, logs storagesweep.Logs, layout stateroot.Layout, runDir string, getenv func(string) string, log dlog.Logger) (*storagesweep.Sweeper, error) {
	aged, err := storageAgedDirs(getenv)
	if err != nil {
		log.Error(graphOperation, "the storage sweep's cache directories could not be resolved", dlog.Context{"cause": err.Error()})
		return nil, err
	}
	buildCache, err := goBuildCacheDir(getenv, os.ReadFile, os.UserConfigDir, os.UserCacheDir)
	if err != nil {
		log.Error(graphOperation, "the storage sweep's Go build cache could not be resolved", dlog.Context{"cause": err.Error()})
		return nil, err
	}
	sweeper, err := storagesweep.New(storagesweep.Deps{
		Registry: registry,
		Clock:    clock.System{},
		LockPath: filepath.Join(runDir, storageSweepLockName),
		ShimDir:  layout.ShimDir(),
		Aged:     aged,
		Bounded:  storageBoundedDirs(buildCache),
		LogsDir:  layout.LogsDir(),
		Logs:     logs,

		BuildCacheDir:   buildCache,
		BuildCacheFS:    storagesweep.OSBuildCacheFS{},
		Zone:            time.Local,
		BuildCacheEvery: storagesweep.DefaultBuildCacheEvery,

		LandedAfter: storagesweep.DefaultLandedAfter,
		ClosedAfter: storagesweep.DefaultClosedAfter,
		LogsAfter:   storagesweep.DefaultLogsAfter,
		StartDelay:  storagesweep.DefaultStartDelay,
		Every:       storagesweep.DefaultEvery,
		Log:         log,
	})
	if err != nil {
		return nil, fmt.Errorf("agent-repld: build the storage sweep: %w", err)
	}
	return sweeper, nil
}

// goBuildCacheDir is the Go build cache the go command on this host uses,
// resolved as `go env GOCACHE` resolves it and without running the go command:
// the GOCACHE variable, else the GOCACHE line of the go command's environment
// file (the GOENV variable names it, `off` meaning none; by default
// <user config dir>/go/env), else <user cache dir>/go-build. A GOCACHE of
// `off` is a host with no build cache, answered as the empty string.
func goBuildCacheDir(getenv func(string) string, readFile func(string) ([]byte, error), userConfigDir, userCacheDir func() (string, error)) (string, error) {
	cache := getenv("GOCACHE")
	if cache == "" {
		fromFile, err := goEnvFileValue("GOCACHE", getenv, readFile, userConfigDir)
		if err != nil {
			return "", err
		}
		cache = fromFile
	}
	switch cache {
	case "off":
		return "", nil
	case "":
		dir, err := userCacheDir()
		if err != nil {
			return "", fmt.Errorf("resolve the user cache directory the Go build cache lives under: %w", err)
		}
		return filepath.Join(dir, "go-build"), nil
	}
	return cache, nil
}

// goEnvFileValue reads one variable out of the go command's environment
// file, the `KEY=value` lines `go env -w` writes. A file that is not there,
// or turned off, sets nothing.
func goEnvFileValue(key string, getenv func(string) string, readFile func(string) ([]byte, error), userConfigDir func() (string, error)) (string, error) {
	path := getenv("GOENV")
	switch path {
	case "off":
		return "", nil
	case "":
		dir, err := userConfigDir()
		if err != nil {
			return "", fmt.Errorf("resolve the user config directory the go command's environment file lives under: %w", err)
		}
		path = filepath.Join(dir, "go", "env")
	}
	body, err := readFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return "", nil
	}
	if err != nil {
		return "", fmt.Errorf("read the go command's environment file %s: %w", path, err)
	}
	for _, line := range strings.Split(string(body), "\n") {
		if name, value, found := strings.Cut(line, "="); found && name == key {
			return value, nil
		}
	}
	return "", nil
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

// storageBoundedDirs are the caches the sweep keeps under a size: the host's
// Go build cache at buildCache (goBuildCacheDir), or none on a host that has
// no build cache.
func storageBoundedDirs(buildCache string) []storagesweep.BoundedDir {
	if buildCache == "" {
		return nil
	}
	return []storagesweep.BoundedDir{{Name: "go-build", Dir: buildCache, Bound: storagesweep.DefaultGoBuildCacheBound}}
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
