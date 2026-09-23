// Package buildreport is how a launchd SERVICE — the store, the sidecar —
// reports the build it is running, so the daemon's deploy can tell whether it
// is out of date.
//
// EVERY PROCESS REPORTS ITS BUILD WHEN IT CONNECTS (owner design, 2026-09-23).
// A shim reports on its diagnostics frame, Emacs and a webview on their watch
// requests. The two services have no connection to the daemon at all — the
// daemon never speaks store.v1 — so their report is a FILE they write the
// moment they boot: this process's pid and the content hash of the binary it
// was exec'd from. The daemon reads it and compares it against the build it
// just made. A report whose pid is not alive is no report: the service is not
// running that build, or anything.
//
// It lives in the logging module because that is the one Go module the
// daemon, the store and the sidecar already share, so the report's path and
// shape are owned once rather than restated by a writer and a reader that
// could drift.
package buildreport

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// The services that report through this package, by the name their report
// file carries.
const (
	// ServiceStore is agent-shim/shim-store.
	ServiceStore = "shim-store"
	// ServiceSidecar is agent-shim/claude/shim-sidecar.
	ServiceSidecar = "shim-claude-sidecar"
)

// Report is one service's statement of what it runs.
type Report struct {
	// PID is the reporting process.
	PID int `json:"pid"`
	// Build is the lowercase hex SHA-256 of the binary the process was exec'd
	// from.
	Build string `json:"build"`
}

// DirEnv redirects the run directory. It is the variable that already
// redirects ~/.cache/agent-repl/run for the kernel locks, because the reports
// live beside them: a harness that isolates one isolates the other, and a test
// service can never overwrite the report the owner's own store wrote.
const DirEnv = "AGENT_REPL_LOCK_DIR"

// ResolveDir is the run directory the reports live in: DirEnv when it is set,
// else ~/.cache/agent-repl/run. getenv is os.Getenv in production.
func ResolveDir(getenv func(string) string) (string, error) {
	if dir := getenv(DirEnv); dir != "" {
		return dir, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("buildreport: resolve the home directory: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "run"), nil
}

// Path is where SERVICE's report lives under DIR.
func Path(dir, service string) string {
	return filepath.Join(dir, service+".build.json")
}

// HashFile answers the lowercase hex SHA-256 of a file's bytes: the content
// hash every component's build is named by.
func HashFile(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", fmt.Errorf("buildreport: open %s to hash it: %w", path, err)
	}
	h := sha256.New()
	_, copyErr := io.Copy(h, f)
	closeErr := f.Close()
	if copyErr != nil {
		return "", fmt.Errorf("buildreport: read %s to hash it: %w", path, copyErr)
	}
	if closeErr != nil {
		return "", fmt.Errorf("buildreport: close %s after hashing it: %w", path, closeErr)
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

// Self answers this process's own report: its pid and the content hash of the
// executable it runs.
func Self() (Report, error) {
	exe, err := os.Executable()
	if err != nil {
		return Report{}, fmt.Errorf("buildreport: resolve this process's executable: %w", err)
	}
	build, err := HashFile(exe)
	if err != nil {
		return Report{}, err
	}
	return Report{PID: os.Getpid(), Build: build}, nil
}

// Write records a report ATOMICALLY — a temporary file in the same directory,
// then a rename — so a reader sees the previous report or this one, never a
// half-written one.
func Write(dir, service string, r Report) error {
	if err := validate(r); err != nil {
		return err
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("buildreport: create %s: %w", dir, err)
	}
	payload, err := json.Marshal(r)
	if err != nil {
		return fmt.Errorf("buildreport: encode the %s report: %w", service, err)
	}
	tmp, err := os.CreateTemp(dir, "."+service+".build.*")
	if err != nil {
		return fmt.Errorf("buildreport: create a temporary report beside %s: %w", Path(dir, service), err)
	}
	name := tmp.Name()
	if _, err := tmp.Write(payload); err != nil {
		_ = tmp.Close()
		return errors.Join(fmt.Errorf("buildreport: write %s: %w", name, err), os.Remove(name))
	}
	if err := tmp.Close(); err != nil {
		return errors.Join(fmt.Errorf("buildreport: close %s: %w", name, err), os.Remove(name))
	}
	if err := os.Rename(name, Path(dir, service)); err != nil {
		return errors.Join(fmt.Errorf("buildreport: install %s: %w", Path(dir, service), err), os.Remove(name))
	}
	return nil
}

// Read answers SERVICE's report. The bool is false when no report exists,
// which is not an error: a service that never booted has nothing to say. A
// report that exists and cannot be read or parsed IS an error — it is never
// read as absent.
func Read(dir, service string) (Report, bool, error) {
	raw, err := os.ReadFile(Path(dir, service))
	if errors.Is(err, os.ErrNotExist) {
		return Report{}, false, nil
	}
	if err != nil {
		return Report{}, false, fmt.Errorf("buildreport: read %s: %w", Path(dir, service), err)
	}
	var r Report
	if err := json.Unmarshal(raw, &r); err != nil {
		return Report{}, false, fmt.Errorf("buildreport: parse %s: %w", Path(dir, service), err)
	}
	if err := validate(r); err != nil {
		return Report{}, false, fmt.Errorf("buildreport: %s: %w", Path(dir, service), err)
	}
	return r, true, nil
}

// validate refuses a report missing either of its two facts.
func validate(r Report) error {
	if r.PID <= 0 {
		return fmt.Errorf("buildreport: a report needs the reporting pid, got %d", r.PID)
	}
	if strings.TrimSpace(r.Build) == "" {
		return errors.New("buildreport: a report needs the build it runs")
	}
	return nil
}
