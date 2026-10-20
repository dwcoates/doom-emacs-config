package reposcans

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestEveryRestatementOfTheLogRetentionMatchesTheLibrary pins the three days
// every log is kept (agentrepl/logging DefaultRetention) to the two places that
// cannot import it: the log reader is one standalone file built without the
// module, and Emacs is another language. A change to one must change all three.
func TestEveryRestatementOfTheLogRetentionMatchesTheLibrary(t *testing.T) {
	cases := []struct {
		file string
		want string
	}{
		{file: "agent-shim/logging/go/retention.go", want: "const DefaultRetention = 72 * time.Hour"},
		{file: "bin/logs-reader.go", want: "const sinkRetention = 72 * time.Hour"},
		{file: "lisp/core.el", want: "(defcustom agent-repl-log-retention-seconds (* 3 86400)"},
	}
	root := moduleRootDir(t)
	for _, tc := range cases {
		t.Run(tc.file, func(t *testing.T) {
			// Arrange.
			source, err := os.ReadFile(filepath.Join(root, tc.file))
			if err != nil {
				t.Fatalf("read %s: %v", tc.file, err)
			}

			// Act / Assert.
			if !strings.Contains(string(source), tc.want) {
				t.Fatalf("%s no longer states the three-day log retention as %q; update every restatement together", tc.file, tc.want)
			}
		})
	}
}
