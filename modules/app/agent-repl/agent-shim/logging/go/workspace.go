package logging

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"path/filepath"
)

// WorkspaceIDLength is the width shared by workspace log records and locks.
const WorkspaceIDLength = 8

// WorkspaceID derives md5hex(filepath.Clean(absolute dir))[:8], the canonical
// workspace correlation key used across agent-repl runtimes.
func WorkspaceID(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve workspace dir %q for its log id: %w", dir, err)
	}
	sum := md5.Sum([]byte(filepath.Clean(abs)))
	return hex.EncodeToString(sum[:])[:WorkspaceIDLength], nil
}
