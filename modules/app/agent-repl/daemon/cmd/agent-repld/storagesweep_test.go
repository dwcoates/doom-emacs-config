package main

import (
	"errors"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/stateroot"
	"agent-repld/internal/storagesweep"
)

func TestOpenProfileSession(t *testing.T) {
	tests := []struct {
		name   string
		active *string
		want   map[string]bool
	}{
		{"no open session keeps the root's own files", nil, map[string]bool{"active": true, "armed": true}},
		{"an open session is kept too", ptr("20261012T192845Z\n"), map[string]bool{"active": true, "armed": true, "20261012T192845Z": true}},
		{"an empty active file names no session", ptr("\n"), map[string]bool{"active": true, "armed": true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := t.TempDir()
			if tt.active != nil {
				if err := os.WriteFile(filepath.Join(root, "active"), []byte(*tt.active), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act.
			got, err := openProfileSession(root)

			// Assert.
			if err != nil || !maps.Equal(got, tt.want) {
				t.Fatalf("openProfileSession = (%v, %v), want %v", got, err, tt.want)
			}
		})
	}
}

func TestOpenProfileSessionRefusesAnUnreadableActiveFile(t *testing.T) {
	// Arrange: the active file is a directory.
	root := t.TempDir()
	if err := os.Mkdir(filepath.Join(root, "active"), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err := openProfileSession(root)

	// Assert.
	if err == nil {
		t.Fatal("openProfileSession = nil error, want the read failure")
	}
}

func TestStorageAgedDirsHonorTheirWritersOverrides(t *testing.T) {
	// Arrange.
	env := map[string]string{envProfileRoot: "/p", envTestPasses: "/t"}

	// Act.
	dirs, err := storageAgedDirs(func(k string) string { return env[k] })

	// Assert.
	if err != nil || len(dirs) != 2 || dirs[0].Dir != "/p" || dirs[1].Dir != "/t" {
		t.Fatalf("storageAgedDirs = (%+v, %v), want the overrides", dirs, err)
	}
}

func TestStorageAgedDirsDefaultUnderTheCache(t *testing.T) {
	// Arrange.
	home := t.TempDir()
	t.Setenv("HOME", home)

	// Act.
	dirs, err := storageAgedDirs(func(string) string { return "" })

	// Assert.
	cache := filepath.Join(home, ".cache", "agent-repl")
	if err != nil || dirs[0].Dir != filepath.Join(cache, "profiles") || dirs[1].Dir != filepath.Join(cache, "test-passes") {
		t.Fatalf("storageAgedDirs = (%+v, %v), want both under %s", dirs, err, cache)
	}
}

// goEnv is the scripted host goBuildCacheDir resolves the build cache on: the
// process environment, and the files that can be read.
type goEnv struct {
	env       map[string]string
	files     map[string]string
	readErr   error
	configErr error
	cacheErr  error
}

func (g goEnv) resolve() (string, error) {
	return goBuildCacheDir(
		func(k string) string { return g.env[k] },
		func(path string) ([]byte, error) {
			if g.readErr != nil {
				return nil, g.readErr
			}
			body, found := g.files[path]
			if !found {
				return nil, fs.ErrNotExist
			}
			return []byte(body), nil
		},
		func() (string, error) { return "/config", g.configErr },
		func() (string, error) { return "/caches", g.cacheErr },
	)
}

func TestGoBuildCacheDirResolvesAsTheGoCommandDoes(t *testing.T) {
	tests := []struct {
		name string
		host goEnv
		want string
	}{
		{"the variable wins", goEnv{
			env:   map[string]string{"GOCACHE": "/from-env"},
			files: map[string]string{"/config/go/env": "GOCACHE=/from-file\n"},
		}, "/from-env"},
		{"then the environment file's line", goEnv{
			files: map[string]string{"/config/go/env": "GOFLAGS=-mod=mod\nGOCACHE=/from-file\n"},
		}, "/from-file"},
		{"GOENV names the environment file", goEnv{
			env:   map[string]string{"GOENV": "/elsewhere/env"},
			files: map[string]string{"/elsewhere/env": "GOCACHE=/from-elsewhere\n", "/config/go/env": "GOCACHE=/from-file\n"},
		}, "/from-elsewhere"},
		{"GOENV off reads no file", goEnv{
			env:   map[string]string{"GOENV": "off"},
			files: map[string]string{"/config/go/env": "GOCACHE=/from-file\n"},
		}, "/caches/go-build"},
		{"no file is the user cache directory", goEnv{}, "/caches/go-build"},
		{"a file that sets other variables is the user cache directory", goEnv{
			files: map[string]string{"/config/go/env": "GOFLAGS=-mod=mod\n"},
		}, "/caches/go-build"},
		{"off is a host with no build cache", goEnv{env: map[string]string{"GOCACHE": "off"}}, ""},
		{"off in the environment file is a host with no build cache", goEnv{
			files: map[string]string{"/config/go/env": "GOCACHE=off\n"},
		}, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := tt.host.resolve()

			// Assert.
			if err != nil || got != tt.want {
				t.Fatalf("goBuildCacheDir = (%q, %v), want %q", got, err, tt.want)
			}
		})
	}
}

func TestGoBuildCacheDirFailsWhenItCannotTell(t *testing.T) {
	tests := []struct {
		name string
		host goEnv
	}{
		{"an unreadable environment file", goEnv{readErr: errors.New("permission denied")}},
		{"no user config directory", goEnv{configErr: errors.New("$HOME is not defined")}},
		{"no user cache directory", goEnv{cacheErr: errors.New("$HOME is not defined")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := tt.host.resolve()

			// Assert.
			if err == nil || got != "" {
				t.Fatalf("goBuildCacheDir = (%q, %v), want a failure and no directory", got, err)
			}
		})
	}
}

func TestAnUnresolvableBuildCacheFailsTheSweepsBuildAndIsRecorded(t *testing.T) {
	// Arrange: the go command's environment file is a directory, which cannot
	// be read as one.
	env := map[string]string{"GOENV": t.TempDir()}
	log := dlog.NewTestLogger()

	// Act.
	sweeper, err := buildStorageSweep(nil, nil, stateroot.Layout{}, t.TempDir(), func(k string) string { return env[k] }, log)

	// Assert.
	if err == nil || sweeper != nil {
		t.Fatalf("buildStorageSweep = (%v, %v), want a failure and no sweep", sweeper, err)
	}
	errs := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error", Operation: graphOperation, Message: "the storage sweep's Go build cache could not be resolved"})
	if len(errs) != 1 || errs[0].Context["cause"] != err.Error() {
		t.Fatalf("want one ERROR record carrying the cause %q, got %+v", err, log.Records())
	}
}

func ptr(s string) *string { return &s }

func TestStorageBoundedDirsFindTheGoBuildCache(t *testing.T) {
	tests := []struct {
		name     string
		gocache  string
		cacheErr error
		wantDir  string
		wantNone bool
		wantErr  string
	}{
		{name: "GOCACHE names it", gocache: "/gc", wantDir: "/gc"},
		{name: "go-build under the user cache directory by default", wantDir: "/caches/go-build"},
		{name: "GOCACHE=off keeps no cache", gocache: "off", wantNone: true},
		{name: "no user cache directory is an error", cacheErr: errors.New("no HOME"), wantErr: "no HOME"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			host := goEnv{env: map[string]string{"GOCACHE": tt.gocache}, cacheErr: tt.cacheErr}
			buildCache, err := host.resolve()
			dirs := storageBoundedDirs(buildCache)

			// Assert.
			switch {
			case tt.wantErr != "":
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("storageBoundedDirs = (%+v, %v), want %q", dirs, err, tt.wantErr)
				}
			case tt.wantNone:
				if err != nil || len(dirs) != 0 {
					t.Fatalf("storageBoundedDirs = (%+v, %v), want none", dirs, err)
				}
			default:
				if err != nil || len(dirs) != 1 || dirs[0] != (storagesweep.BoundedDir{Name: "go-build", Dir: tt.wantDir, Bound: storagesweep.DefaultGoBuildCacheBound}) {
					t.Fatalf("storageBoundedDirs = (%+v, %v), want go-build at %s", dirs, err, tt.wantDir)
				}
			}
		})
	}
}
