package procgroup

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestParseStat(t *testing.T) {
	cases := []struct {
		name       string
		stat       string
		wantComm   string
		wantFields []string
		wantErr    string
	}{
		{name: "an ordinary line", stat: "42 (node) S 1 42", wantComm: "node", wantFields: []string{"S", "1", "42"}},
		{name: "a command name holding spaces and parentheses", stat: "42 (a (b) c) R 1", wantComm: "a (b) c", wantFields: []string{"R", "1"}},
		{name: "a line with no command name", stat: "42 node R", wantErr: "no command name"},
		{name: "a close before the open", stat: "42 )node( R", wantErr: "no command name"},
		{name: "a line with nothing after its command name", stat: "42 (node) ", wantErr: "no state after its command name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := ParseStat(tc.stat)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ParseStat error = %v, want one containing %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got.Comm != tc.wantComm || !slices.Equal(got.Fields, tc.wantFields) {
				t.Fatalf("ParseStat = %+v, %v; want comm %q fields %v", got, err, tc.wantComm, tc.wantFields)
			}
		})
	}
}

// TestEveryProcStatLineIsSplitByParseStat pins that no other source of the
// daemon module splits a /proc stat line at its command name by hand.
func TestEveryProcStatLineIsSplitByParseStat(t *testing.T) {
	// Arrange
	var offenders []string

	// Act
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Base(path) == "stat.go" && filepath.Base(filepath.Dir(path)) == "procgroup" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), "LastIndexByte(stat, ')')") || strings.Contains(string(src), `LastIndexByte(stat, ')'`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("%v split a /proc stat line by hand instead of through procgroup.ParseStat", offenders)
	}
}
