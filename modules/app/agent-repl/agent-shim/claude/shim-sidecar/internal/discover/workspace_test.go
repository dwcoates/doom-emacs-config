package discover

import (
	"encoding/json"
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
	gotDir, gotID, err := ResolveWorkspace(target)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveWorkspace = %v", err)
	}
	if gotDir != workspaceDir || gotID != wantID {
		t.Fatalf("workspace = (%q, %q), want (%q, %q)", gotDir, gotID, workspaceDir, wantID)
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
	gotDir, _, err := ResolveWorkspace(target)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveWorkspace = %v", err)
	}
	if gotDir != "/actual/project" {
		t.Fatalf("workspace dir = %q, want the transcript cwd rather than a decoded slug", gotDir)
	}
}

func TestTranscriptCWDReadsACompleteTokenFromAGrowingRecord(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(path, []byte(`{"cwd":"/work/project","message":`), 0o600); err != nil {
		t.Fatalf("write growing transcript fixture: %v", err)
	}

	// Act.
	got, err := transcriptCWD(path)

	// Assert.
	if err != nil {
		t.Fatalf("transcriptCWD = %v", err)
	}
	if got != "/work/project" {
		t.Fatalf("cwd = %q, want the complete token from the growing record", got)
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
