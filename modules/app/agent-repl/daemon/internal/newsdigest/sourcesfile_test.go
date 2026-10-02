package newsdigest

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

// sourcesFile writes content as a sources file and answers its path.
func sourcesFile(t *testing.T, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "sources.json")
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	return path
}

func TestLoadSourcesReadsEverySource(t *testing.T) {
	// Arrange
	path := sourcesFile(t, `[{"key":"feed","name":"SDK releases","url":"https://fixture.test/feed.atom","home":"https://fixture.test/releases","format":"atom"}]`)

	// Act
	got, err := LoadSources(path)

	// Assert
	if err != nil || !reflect.DeepEqual(got, []Source{feedSource}) {
		t.Fatalf("LoadSources = (%+v, %v), want the feed source", got, err)
	}
}

func TestLoadSourcesRefusesBadFiles(t *testing.T) {
	tests := []struct {
		name    string
		content string
	}{
		{name: "not json", content: "["},
		{name: "an unknown field", content: `[{"key":"a","name":"A","url":"https://a.test","home":"https://a.test","format":"page","extra":1}]`},
		{name: "an unknown format", content: `[{"key":"a","name":"A","url":"https://a.test","home":"https://a.test","format":"podcast"}]`},
		{name: "no source", content: `[]`},
		{name: "a repeated key", content: `[{"key":"a","name":"A","url":"https://a.test","home":"https://a.test","format":"page"},{"key":"a","name":"B","url":"https://b.test","home":"https://b.test","format":"page"}]`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := sourcesFile(t, tt.content)

			// Act
			_, err := LoadSources(path)

			// Assert
			if err == nil {
				t.Fatal("LoadSources = nil, want a refusal")
			}
		})
	}
}

func TestLoadSourcesRefusesAMissingFile(t *testing.T) {
	// Act
	_, err := LoadSources(filepath.Join(t.TempDir(), "absent.json"))

	// Assert
	if err == nil {
		t.Fatal("LoadSources = nil, want a refusal")
	}
}
