package discover

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"testing"

	sharedlogging "agentrepl/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

var testWorkspaceNonAlphanumeric = regexp.MustCompile(`[^A-Za-z0-9]`)

func TestResolveWorkspaceReadsCWDWithoutDecodingTheProjectSlug(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	workspaceDir := filepath.Join(root, "work_tree")
	slug := testWorkspaceNonAlphanumeric.ReplaceAllString(workspaceDir, "-")
	projectDir := filepath.Join(root, "config", "projects", slug)
	if err := os.MkdirAll(projectDir, 0o755); err != nil {
		t.Fatalf("create project fixture: %v", err)
	}
	transcript := filepath.Join(projectDir, "session-1.jsonl")
	contents := "{\"type\":\"queue-operation\"}\n{\"cwd\":" + quotedJSON(t, workspaceDir) + "}\n"
	if err := os.WriteFile(transcript, []byte(contents), 0o600); err != nil {
		t.Fatalf("write transcript fixture: %v", err)
	}
	target := Target{Path: transcript, Kind: tail.KindSessionTranscript, SessionID: "session-1", ConfigRoot: filepath.Join(root, "config")}
	wantID, err := sharedlogging.WorkspaceID(workspaceDir)
	if err != nil {
		t.Fatalf("derive expected workspace id: %v", err)
	}

	// Act.
	got, err := ResolveWorkspace(target)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveWorkspace = %v", err)
	}
	if got != (Attribution{Dir: workspaceDir, ID: wantID}) {
		t.Fatalf("attribution = %+v, want (%q, %q) matched to its project folder", got, workspaceDir, wantID)
	}
}

func TestResolveWorkspaceNeverDecodesTheLossyProjectDirectory(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	projectDir := filepath.Join(root, "config", "projects", "-wrong-project")
	if err := os.MkdirAll(projectDir, 0o755); err != nil {
		t.Fatalf("create project fixture: %v", err)
	}
	transcript := filepath.Join(projectDir, "session-1.jsonl")
	if err := os.WriteFile(transcript, []byte("{\"cwd\":\"/actual/project\"}\n"), 0o600); err != nil {
		t.Fatalf("write transcript fixture: %v", err)
	}
	target := Target{Path: transcript, Kind: tail.KindSessionTranscript, SessionID: "session-1", ConfigRoot: filepath.Join(root, "config")}

	// Act.
	got, err := ResolveWorkspace(target)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveWorkspace = %v", err)
	}
	if got.Dir != "/actual/project" {
		t.Fatalf("workspace dir = %q, want the transcript cwd rather than a decoded slug", got.Dir)
	}
}

func TestTranscriptCWDReadsACompleteTokenFromAGrowingRecord(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(path, []byte(`{"cwd":"/work/project","message":`), 0o600); err != nil {
		t.Fatalf("write growing transcript fixture: %v", err)
	}

	// Act.
	got, _, err := transcriptCWD(path, "-work-project")

	// Assert.
	if err != nil {
		t.Fatalf("transcriptCWD = %v", err)
	}
	if got != "/work/project" {
		t.Fatalf("cwd = %q, want the complete token from the growing record", got)
	}
}

// sessionTranscript writes a main transcript into the project folder named
// slug, one record per cwd, and answers the target that names it.
func sessionTranscript(t *testing.T, slug string, cwds ...string) Target {
	t.Helper()
	configRoot := filepath.Join(t.TempDir(), "config")
	projectDir := filepath.Join(configRoot, "projects", slug)
	if err := os.MkdirAll(projectDir, 0o755); err != nil {
		t.Fatalf("create project fixture: %v", err)
	}
	var contents string
	for _, cwd := range cwds {
		contents += "{\"cwd\":" + quotedJSON(t, cwd) + ",\"type\":\"user\"}\n"
	}
	transcript := filepath.Join(projectDir, "session-1.jsonl")
	if err := os.WriteFile(transcript, []byte(contents), 0o600); err != nil {
		t.Fatalf("write transcript fixture: %v", err)
	}
	return Target{Path: transcript, Kind: tail.KindSessionTranscript, SessionID: "session-1", ConfigRoot: configRoot}
}

func TestResolveWorkspaceAttributesTheCWDThatEncodesToTheProjectFolder(t *testing.T) {
	tests := []struct {
		name         string
		slug         string
		cwds         []string
		wantDir      string
		wantFallback bool
	}{
		{
			name:    "a single cwd matching its folder is attributed as before",
			slug:    "-work-ship-gns",
			cwds:    []string{"/work/ship-gns"},
			wantDir: "/work/ship-gns",
		},
		{
			name:    "a later cwd matching the folder wins over the first",
			slug:    "-work-ship-gns",
			cwds:    []string{"/work/iterm-2", "/work/iterm-2", "/work/ship-gns", "/work/iterm-2"},
			wantDir: "/work/ship-gns",
		},
		{
			name:         "no cwd matching the folder falls back to the first",
			slug:         "-elsewhere",
			cwds:         []string{"/work/iterm-2", "/work/ship-gns"},
			wantDir:      "/work/iterm-2",
			wantFallback: true,
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			target := sessionTranscript(t, test.slug, test.cwds...)

			// Act.
			got, err := ResolveWorkspace(target)

			// Assert.
			if err != nil {
				t.Fatalf("ResolveWorkspace = %v", err)
			}
			if got.Dir != test.wantDir || got.FirstCWDFallback != test.wantFallback {
				t.Fatalf("attribution = %+v, want dir %q fallback %v", got, test.wantDir, test.wantFallback)
			}
		})
	}
}

func TestTranscriptCWDRefusesATranscriptWithNoCWDYet(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(path, []byte("{\"type\":\"queue-operation\"}\n"), 0o600); err != nil {
		t.Fatalf("write transcript fixture: %v", err)
	}

	// Act.
	_, _, err := transcriptCWD(path, "-work-project")

	// Assert.
	if !errors.Is(err, ErrNoCWDYet) {
		t.Fatalf("transcriptCWD error = %v, want ErrNoCWDYet", err)
	}
}

func quotedJSON(t *testing.T, value string) string {
	t.Helper()
	encoded, err := json.Marshal(value)
	if err != nil {
		t.Fatalf("marshal fixture string: %v", err)
	}
	return string(encoded)
}
