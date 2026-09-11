package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"path/filepath"
)

// WorkspaceDirHashLength is the width of the workspace directory hash.
const WorkspaceDirHashLength = 8

// WorkspaceIDLookup resolves a workspace directory to that workspace's
// DAEMON-MINTED ids.WorkspaceID. It is bound once at boot, after the state
// client is open, and it is the ONLY source of a record's workspace_id.
//
// It answers an error (or an empty id) rather than anything derived from the
// path: a log record and a sink name must carry the id every other runtime
// carries, and a path-derived stand-in would make a workspace look like two.
type WorkspaceIDLookup func(dir string) (string, error)

// WorkspaceDirHash derives the workspace directory hash:
// md5hex(filepath.Clean(absolute dir))[:8].
//
// This is the derivation the shim-held kernel lock file uses (ARCHITECTURE.md
// "Shim-held kernel locks"), and it is recorded under `workspace_dir_hash` so
// an operator can grep a record against a lock file name. IT IS NOT THE
// RECORD'S workspace_id: that is the daemon-minted ids.WorkspaceID, resolved
// through a WorkspaceIDLookup, and the two never substitute for each other.
func WorkspaceDirHash(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve workspace dir %q for its directory hash: %w", dir, err)
	}
	sum := md5.Sum([]byte(filepath.Clean(abs)))
	return hex.EncodeToString(sum[:])[:WorkspaceDirHashLength], nil
}
