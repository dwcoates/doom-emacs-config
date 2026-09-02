package main

import (
	"bytes"
	"encoding/base64"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"
)

func TestLoadProfileWithoutADirectoryIsTheZeroProfile(t *testing.T) {
	// Arrange / Act
	got, err := LoadProfile("", "/w/one")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the zero profile", err)
	}
	if !reflect.DeepEqual(got, Profile{}) {
		t.Fatalf("LoadProfile = %+v, want the zero profile", got)
	}
}

func TestLoadProfilePrefersThePerWorkspaceFile(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	write(t, filepath.Join(dir, "default.json"), `{"build_sha":"shared"}`)
	write(t, filepath.Join(dir, ProfileFileName("/w/one")), `{"build_sha":"mine"}`)

	// Act
	got, err := LoadProfile(dir, "/w/one")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the workspace's own profile", err)
	}
	if got.BuildSHA != "mine" {
		t.Fatalf("LoadProfile BuildSHA = %q, want the per-workspace file to win", got.BuildSHA)
	}
}

func TestLoadProfileFallsBackToDefault(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	write(t, filepath.Join(dir, "default.json"), `{"delay_diagnostics":true}`)

	// Act
	got, err := LoadProfile(dir, "/w/unprofiled")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the default profile", err)
	}
	if !got.DelayDiagnostics {
		t.Fatalf("LoadProfile DelayDiagnostics = false, want the default profile applied")
	}
}

func TestLoadProfileRefusesAMalformedFile(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	write(t, filepath.Join(dir, "default.json"), `{not json`)

	// Act
	_, err := LoadProfile(dir, "/w/one")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "parse profile") {
		t.Fatalf("LoadProfile on a malformed file = %v, want a parse error and never a default", err)
	}
}

func TestProfileFileNameIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := ProfileFileName("/w/one")
	noisy := ProfileFileName("/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("ProfileFileName = %q and %q, want one name for one cleaned path", plain, noisy)
	}
}

func write(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func TestLoadProfileCarriesTheLiveWorkTheOpeningStates(t *testing.T) {
	// Arrange: one binary-encoded AgentDetachedWork, base64 as JSON renders
	// []byte.
	dir := t.TempDir()
	raw, err := proto.Marshal(&conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
	})
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	write(t, filepath.Join(dir, "default.json"),
		`{"live_work":["`+base64.StdEncoding.EncodeToString(raw)+`"]}`)

	// Act
	got, err := LoadProfile(dir, "/w/one")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the profile", err)
	}
	if len(got.LiveWork) != 1 || !bytes.Equal(got.LiveWork[0], raw) {
		t.Fatalf("LoadProfile.LiveWork = %v, want the one encoded item", got.LiveWork)
	}
}
