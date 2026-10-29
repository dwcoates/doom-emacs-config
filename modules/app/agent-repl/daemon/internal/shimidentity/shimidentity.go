// Package shimidentity reads the main agent's identity a workspace's shim
// persisted: `<state root>/shim/<workspace key>/agent-id.json`, which the shim
// writes before its first query (agent-shim/claude/shim/src/engine/identity.ts)
// and which names the original vendor session id its conversation's book is
// filed under in the store.
//
// THE DAEMON READS IT ONLY FOR A WORKSPACE WITH NO RECORDED BOOK
// (wsm.workspace_book): one whose shim has not answered a start since the book
// was first recorded, or whose record could not be written. The file is the
// shim's on-disk contract; its field names are pinned by this package's test.
package shimidentity

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"agent-repld/internal/sessionlock"
)

// fileName is the identity record's name inside the workspace's shim state
// directory.
const fileName = "agent-id.json"

// record is the persisted identity, as the shim writes it. Only the field the
// daemon reads is named.
type record struct {
	OriginalVendorSessionID string `json:"original_vendor_session_id"`
}

// Book answers the book the shim persisted for the workspace at WORKSPACEDIR,
// its state directories under SHIMDIR (stateroot.Layout.ShimDir). FOUND is
// false when the shim never persisted one: the workspace has no conversation
// yet. A record that exists and cannot be read or names no book is an error.
func Book(shimDir, workspaceDir string) (book string, found bool, err error) {
	key, err := sessionlock.WorkspaceKey(workspaceDir)
	if err != nil {
		return "", false, fmt.Errorf("shimidentity: the shim state key of %s: %w", workspaceDir, err)
	}
	path := filepath.Join(shimDir, key, fileName)
	raw, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("shimidentity: read %s: %w", path, err)
	}
	var rec record
	if err := json.Unmarshal(raw, &rec); err != nil {
		return "", false, fmt.Errorf("shimidentity: parse %s: %w", path, err)
	}
	if rec.OriginalVendorSessionID == "" {
		return "", false, fmt.Errorf("shimidentity: %s names no original_vendor_session_id", path)
	}
	return rec.OriginalVendorSessionID, true, nil
}
