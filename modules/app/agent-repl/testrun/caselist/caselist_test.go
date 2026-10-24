package caselist

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestSplitListDropsEmptyEntries(t *testing.T) {
	if got := SplitList(" a, ,b,"); !reflect.DeepEqual(got, []string{"a", "b"}) {
		t.Fatalf("SplitList = %v", got)
	}
}

func TestReadDeleteSet(t *testing.T) {
	tests := []struct {
		name, body string
		missing    bool
		want       []string
		wantErr    string
	}{
		{name: "comments and blank lines are skipped", body: "# rows\nA\n\n  B  \n", want: []string{"A", "B"}},
		{name: "a set naming nothing", body: "# none\n", wantErr: "names no case"},
		{name: "a missing file", missing: true, wantErr: "read -delete"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "d.txt")
			if !tt.missing {
				if err := os.WriteFile(path, []byte(tt.body), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := ReadDeleteSet(path)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ReadDeleteSet = %v, %v", got, err)
			}
		})
	}
}

// THE DAEMON'S COMMAND LINE READS THE LISTS THROUGH THIS PACKAGE, never a
// copy of its own, so a request and a hand-written flag read alike.
func TestTheDaemonsRedundancyCommandReadsItsListsHere(t *testing.T) {
	// Arrange
	src, err := os.ReadFile(filepath.Join("..", "..", "daemon", "cmd", "agent-repld", "testredundancycli.go"))
	if err != nil {
		t.Fatal(err)
	}
	body := string(src)

	// Act
	shared := strings.Contains(body, "caselist.ReadDeleteSet(") && strings.Contains(body, "caselist.SplitList(")
	copied := strings.Contains(body, "func readDeleteSet(") || strings.Contains(body, "func commaItems(")

	// Assert
	if !shared || copied {
		t.Fatalf("testredundancycli.go reads its case lists with a copy of its own; use caselist")
	}
}
