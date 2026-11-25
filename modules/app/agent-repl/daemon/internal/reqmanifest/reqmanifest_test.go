package reqmanifest

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

func TestIncludesAnswersTheRootManifestsIncludes(t *testing.T) {
	tests := []struct {
		name    string
		body    *string
		want    []string
		wantErr bool
	}{
		{name: "the includes are answered in order", body: ptr(`{"includes": ["a/requirements.json", "b/requirements.json"]}`), want: []string{"a/requirements.json", "b/requirements.json"}},
		{name: "a manifest including nothing answers none", body: ptr(`{"requirements": []}`)},
		{name: "a manifest that is no JSON is an error", body: ptr(`{`), wantErr: true},
		{name: "no manifest is an error", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "requirements.json")
			if tt.body != nil {
				if err := os.WriteFile(path, []byte(*tt.body), 0o600); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := Includes(path)

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("Includes = (%v, %v), want %v (error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func ptr(s string) *string { return &s }
