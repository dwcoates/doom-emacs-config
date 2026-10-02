package main

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/newsdigest"
	"claude-repld/internal/wsm"
)

// noDigestStore is a news digest store nothing reads: these tests only build.
type noDigestStore struct{}

func (noDigestStore) NewsDigestState(context.Context) (wsm.NewsDigestState, error) {
	return wsm.NewsDigestState{}, nil
}
func (noDigestStore) RecordNewsDigestRun(context.Context, wsm.NewsDigestRun) error { return nil }
func (noDigestStore) DismissNewsDigest(context.Context, string) (bool, error)      { return false, nil }
func (noDigestStore) RestandNewsDigest(context.Context, string) (bool, error)      { return false, nil }

// noRunner is a headless runner nothing calls: these tests only build.
type noRunner struct{}

func (noRunner) Run(context.Context, headless.Request) (headless.Response, error) {
	return headless.Response{}, nil
}
func (noRunner) Bin() string { return "none" }

// allowAll is a vendor guard that forbids nothing.
type allowAll struct{}

func (allowAll) Check(string) error { return nil }

// digestInputs are buildNewsDigest's inputs over env.
func digestInputs(t *testing.T, env map[string]string) (newsDigestInputs, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	return newsDigestInputs{
		Guard: allowAll{}, Headless: noRunner{}, PromptsDir: t.TempDir(), Store: noDigestStore{},
		RunDir: t.TempDir(), Serves: func() bool { return true },
		Getenv: func(k string) string { return env[k] }, Log: log,
	}, log
}

func TestTheNewsDigestBuildsWithTheProductionWindows(t *testing.T) {
	// Arrange
	in, _ := digestInputs(t, nil)

	// Act
	digest, err := buildNewsDigest(in)

	// Assert
	if err != nil || digest == nil {
		t.Fatalf("buildNewsDigest = (%v, %v), want a digest", digest, err)
	}
}

func TestTheNewsDigestRefusesABadKnob(t *testing.T) {
	tests := []struct {
		name string
		env  map[string]string
	}{
		{name: "a malformed start delay", env: map[string]string{envNewsDigestStartDelay: "soon"}},
		{name: "a non-positive cadence", env: map[string]string{envNewsDigestEvery: "0s"}},
		{name: "a malformed recheck", env: map[string]string{envNewsDigestRecheck: "often"}},
		{name: "a missing sources file", env: map[string]string{newsdigest.EnvSources: "/nonexistent/sources.json"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			in, log := digestInputs(t, tt.env)

			// Act
			_, err := buildNewsDigest(in)

			// Assert
			if err == nil {
				t.Fatal("buildNewsDigest = nil, want a boot refusal")
			}
			recorded := false
			for _, r := range log.Records() {
				recorded = recorded || (r.Level == "error" && r.Operation == graphOperation)
			}
			if !recorded {
				t.Fatalf("records = %v, want the refusal at ERROR", log.Records())
			}
		})
	}
}

func TestTheNewsDigestReadsANamedSourcesFile(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "sources.json")
	content := `[{"key":"a","name":"A","url":"https://a.test/feed","home":"https://a.test","format":"atom"}]`
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	in, log := digestInputs(t, map[string]string{newsdigest.EnvSources: path})

	// Act
	_, err := buildNewsDigest(in)

	// Assert
	if err != nil {
		t.Fatalf("buildNewsDigest: %v", err)
	}
	for _, r := range log.Records() {
		if r.Message == "the news digest is built" && r.Context["sources"] == 1 && r.Context["sources_file"] == path {
			return
		}
	}
	t.Fatalf("records = %v, want the build naming the one source from the file", log.Records())
}
