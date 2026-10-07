package recorded_test

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/recorded"
)

// homeTokenVectors are shared with the capture tooling's expandHome
// (anonymize.mjs), so the two expansions cannot drift apart.
type homeTokenVectors struct {
	Home    string `json:"home"`
	Vectors []struct {
		Name string `json:"name"`
		Text string `json:"text"`
		Want string `json:"want"`
	} `json:"vectors"`
}

func TestExpand(t *testing.T) {
	// Arrange.
	raw, err := os.ReadFile(filepath.Join("..", "..", "..", "shim", "scripts", "capture", "home-token-vectors.json"))
	if err != nil {
		t.Fatalf("read the shared home-token vectors: %v", err)
	}
	var shared homeTokenVectors
	if err := json.Unmarshal(raw, &shared); err != nil {
		t.Fatalf("parse the shared home-token vectors: %v", err)
	}
	if len(shared.Vectors) == 0 {
		t.Fatal("the shared home-token vectors are empty")
	}
	for _, tc := range shared.Vectors {
		t.Run(tc.Name, func(t *testing.T) {
			// Act.
			got := recorded.Expand(tc.Text, shared.Home)

			// Assert.
			if got != tc.Want {
				t.Fatalf("Expand(%q) = %q, want %q", tc.Text, got, tc.Want)
			}
		})
	}
}

func TestHomeTokenMatchesTheCaptureTooling(t *testing.T) {
	// Assert: the sidecar expands the very token the capture scrub writes.
	raw, err := os.ReadFile(filepath.Join("..", "..", "..", "shim", "scripts", "capture", "anonymize.mjs"))
	if err != nil {
		t.Fatalf("read anonymize.mjs: %v", err)
	}
	if want := `export const HOME_TOKEN = "` + recorded.HomeToken + `";`; !strings.Contains(string(raw), want) {
		t.Fatalf("anonymize.mjs does not declare %s", want)
	}
}
