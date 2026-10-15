package vitestmeasure

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Every setup file names a test through redundancy-names.mjs, so the traced
// runs and Stryker's runs key each test alike: none hand-rolls its own.
func TestTheSetupFilesShareOneTestName(t *testing.T) {
	entries, err := os.ReadDir(filepath.Join("..", "..", "..", "vitest"))
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		name := e.Name()
		if !strings.HasPrefix(name, "redundancy-") || name == "redundancy-names.mjs" {
			continue
		}
		b, err := os.ReadFile(filepath.Join("..", "..", "..", "vitest", name))
		if err != nil {
			t.Fatal(err)
		}
		src := string(b)
		if strings.Contains(src, "function fullName") || !strings.Contains(src, `import { fullName } from "./redundancy-names.mjs"`) {
			t.Errorf("%s does not name tests through redundancy-names.mjs", name)
		}
	}
}
