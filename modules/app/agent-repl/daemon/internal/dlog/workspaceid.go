package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"path/filepath"
)

// LogWorkspaceIDLength is the width of the log-attribution workspace id.
const LogWorkspaceIDLength = 8

// LogWorkspaceID derives a record's workspace_id from a workspace directory:
// md5hex(filepath.Clean(absolute dir))[:8].
//
// This is deliberately the SAME derivation the shim-held kernel lock file uses
// (ARCHITECTURE.md "Shim-held kernel locks"), so a log record and a lock file
// name the same workspace with the same eight characters and an operator can
// grep one against the other. It is NOT the daemon-minted ids.WorkspaceID,
// which is opaque and never derived from a path; the two never substitute for
// each other.
func LogWorkspaceID(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve workspace dir %q for its log id: %w", dir, err)
	}
	sum := md5.Sum([]byte(filepath.Clean(abs)))
	return hex.EncodeToString(sum[:])[:LogWorkspaceIDLength], nil
}
