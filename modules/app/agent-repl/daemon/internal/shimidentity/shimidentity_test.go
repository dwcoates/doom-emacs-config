package shimidentity

import (
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/sessionlock"
)

// writeRecord writes CONTENT as the identity record of the workspace at DIR
// under SHIMDIR.
func writeRecord(t *testing.T, shimDir, dir, content string) {
	t.Helper()
	key, err := sessionlock.WorkspaceKey(dir)
	if err != nil {
		t.Fatalf("WorkspaceKey: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(shimDir, key), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(filepath.Join(shimDir, key, fileName), []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
}

func TestBook(t *testing.T) {
	tests := []struct {
		name      string
		content   *string
		wantBook  string
		wantFound bool
		wantErr   bool
	}{
		{
			// PINNED: the record exactly as identity.ts's atomicWriteJson writes
			// it (JSON.stringify with two-space indent and a trailing newline).
			name:      "a record in the shim's own format names its book",
			content:   ptr("{\n  \"original_vendor_session_id\": \"vendor-1\",\n  \"workspace_key\": \"0123abcd\",\n  \"minted_at_ms\": 1793217311658\n}\n"),
			wantBook:  "vendor-1",
			wantFound: true,
		},
		{name: "no record is no book"},
		{name: "a torn record is an error", content: ptr("{\"original_vendor"), wantErr: true},
		{name: "a record naming no book is an error", content: ptr(`{"workspace_key": "0123abcd"}`), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			shimDir, dir := t.TempDir(), t.TempDir()
			if tt.content != nil {
				writeRecord(t, shimDir, dir, *tt.content)
			}

			// Act
			book, found, err := Book(shimDir, dir)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, want error %v", err, tt.wantErr)
			}
			if book != tt.wantBook || found != tt.wantFound {
				t.Fatalf("Book = %q, %v; want %q, %v", book, found, tt.wantBook, tt.wantFound)
			}
		})
	}
}

func ptr(s string) *string { return &s }
