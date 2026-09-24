package daemon_test

import (
	"bufio"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// retiredKeepaliveIdentifiers are the names of the daemon's retired keep-alive
// machinery: the StartTurn re-drive behind a keep-alive, its interrupt cancel,
// the keep-alive hold badge and the cold keep-alive alarm. The shim makes a
// prompt that arrives during its own keep-alive wait inside the shim, so the
// daemon never sees a keep-alive collision and never holds a prompt behind one.
// None of these names may come back in production code. The shim's
// `keepalive_failed` SESSION FAULT is a different arm and is not listed.
var retiredKeepaliveIdentifiers = []string{
	"isKeepaliveCollision",
	"KeepaliveTurnAlreadyOpen",
	"TransientKeepalive",
	"CancelKeepaliveRedrive",
	"redriveBehindKeepalive",
	"keepaliveRedrive",
	"HeldPrompt_KeepAlive",
	"HeldPromptKeepAliveHold",
	"coldKeepalive",
	"GetKeepalive(",
}

// TestNoProductionCodeNamesTheRetiredKeepaliveMachinery walks every non-test
// Go file under the daemon and fails naming the file and line of any retired
// keep-alive identifier.
func TestNoProductionCodeNamesTheRetiredKeepaliveMachinery(t *testing.T) {
	// Arrange.
	var hits []string

	// Act.
	err := filepath.WalkDir(".", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := os.Open(path)
		if err != nil {
			return err
		}
		defer file.Close()
		found, err := retiredKeepaliveHits(filepath.ToSlash(path), file)
		if err != nil {
			return err
		}
		hits = append(hits, found...)
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("scan production Go: %v", err)
	}
	if len(hits) > 0 {
		t.Errorf("production code names retired keep-alive machinery; the shim makes a prompt wait inside its own keep-alive, so the daemon has none:\n  %s", strings.Join(hits, "\n  "))
	}
}

// TestTheRetiredKeepaliveScanFlagsAPlantedIdentifier pins the scanner itself,
// so the guard above cannot pass by no longer seeing what it guards.
func TestTheRetiredKeepaliveScanFlagsAPlantedIdentifier(t *testing.T) {
	tests := []struct {
		name string
		body string
		want []string
	}{
		{
			name: "a planted re-drive call is flagged with its file and line",
			body: "package x\n\nfunc f() {\n\tq.redriveBehindKeepalive(ctx)\n}\n",
			want: []string{"internal/x/x.go:4: redriveBehindKeepalive"},
		},
		{
			name: "a prefix of the re-drive's bound is flagged",
			body: "package x\n\nconst n = keepaliveRedriveMaxAttempts\n",
			want: []string{"internal/x/x.go:3: keepaliveRedrive"},
		},
		{
			name: "a read of the retired keepalive flag is flagged",
			body: "package x\n\nvar b = tao.GetKeepalive()\n",
			want: []string{"internal/x/x.go:3: GetKeepalive("},
		},
		{
			name: "a doc comment naming the retired hold is flagged",
			body: "package x\n\n// HeldPromptKeepAliveHold is gone.\n",
			want: []string{"internal/x/x.go:3: HeldPromptKeepAliveHold"},
		},
		{
			name: "the shim's keepalive_failed session fault is not flagged",
			body: "package x\n\nvar k = f.GetKeepaliveFailed()\n",
		},
		{
			name: "a file without any retired name is not flagged",
			body: "package x\n\nfunc f() {}\n",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			source := strings.NewReader(tt.body)

			// Act.
			got, err := retiredKeepaliveHits("internal/x/x.go", source)

			// Assert.
			if err != nil {
				t.Fatalf("scan: %v", err)
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("hits = %q, want %q", got, tt.want)
			}
		})
	}
}

// retiredKeepaliveHits answers one "path:line: identifier" entry for each
// retired identifier on each line of source.
func retiredKeepaliveHits(path string, source io.Reader) ([]string, error) {
	var hits []string
	scanner := bufio.NewScanner(source)
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for line := 1; scanner.Scan(); line++ {
		text := scanner.Text()
		for _, name := range retiredKeepaliveIdentifiers {
			if strings.Contains(text, name) {
				hits = append(hits, fmt.Sprintf("%s:%d: %s", path, line, name))
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	return hits, nil
}
