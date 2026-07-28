package registry

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"time"
)

// metaLegacyImport is the registry_meta key stamped once the pre-SQLite JSON
// registry has been imported. The stamp — not the emptiness of the tables — is
// what makes the import ONCE: a registry legitimately emptied by compaction
// must not re-inherit a stale roster on the next boot.
const metaLegacyImport = "legacy_json_import"

// legacyFileShape is the pre-SQLite registry document.
type legacyFileShape struct {
	Version     int                      `json:"version"`
	Sessions    []Record                 `json:"sessions"`
	Checkpoints []ConversationCheckpoint `json:"conversation_checkpoints,omitempty"`
}

// legacyCheckpointFileShape is the pre-SQLite checkpoint sidecar, which was
// written before the roster so a crash between them could leave a checkpoint
// ahead but never lose replay progress.
type legacyCheckpointFileShape struct {
	Version     int                      `json:"version"`
	Checkpoints []ConversationCheckpoint `json:"conversation_checkpoints"`
}

// importLegacyJSON performs the ONE-TIME import of the pre-SQLite registry
// file (and its checkpoint sidecar) into the tables.
//
// It runs only when a path is given, the stamp is absent, and both tables are
// empty. Afterwards the tables are the SOLE authority: there is no dual read
// and no fallback to the file, so a table that later reads empty means the
// registry IS empty. The JSON is neither deleted nor rewritten — it stays on
// disk as inert history.
//
// A file that cannot be read or parsed FAILS the open. It is the daemon's
// entire session roster; treating garbage as "nothing to import" would silently
// discard every live session's identity.
func (r *Registry) importLegacyJSON(path string) error {
	if path == "" {
		return nil
	}
	stamped, err := r.legacyImportStamp()
	if err != nil {
		return err
	}
	if stamped != "" {
		return nil
	}
	populated, err := r.tablesPopulated()
	if err != nil {
		return err
	}
	if populated {
		// Records exist without a stamp: this database was written by a build
		// that already owned the tables. The file is history; leave it.
		return nil
	}

	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		// No legacy file: a genuinely fresh install, or a daemon whose state
		// root never held one. Nothing to import and nothing to stamp.
		return nil
	}
	if err != nil {
		r.logf("registry: legacy import READ FAILED for %s — refusing to serve an empty roster: %v", path, err)
		return fmt.Errorf("registry: read legacy registry %s: %w", path, err)
	}
	var doc legacyFileShape
	if err := json.Unmarshal(data, &doc); err != nil {
		r.logf("registry: legacy registry at %s is CORRUPT — refusing to serve an empty roster: %v", path, err)
		return fmt.Errorf("registry: parse legacy registry %s: %w", path, err)
	}
	if doc.Version != 1 && doc.Version != 2 {
		r.logf("registry: legacy registry at %s has UNSUPPORTED version %d — refusing to serve an empty roster", path, doc.Version)
		return fmt.Errorf("registry: unsupported legacy registry version %d in %s", doc.Version, path)
	}

	records := map[string]Record{}
	for _, rec := range doc.Sessions {
		if rec.SessionID == "" {
			r.logf("registry: legacy registry at %s carries an INVALID record with empty session_id — refusing to import a partial roster", path)
			return fmt.Errorf("registry: legacy record with empty session_id in %s", path)
		}
		records[rec.SessionID] = rec
	}
	checkpoints := map[ConversationIdentity]ConversationCheckpoint{}
	for _, cp := range doc.Checkpoints {
		if err := mergeCheckpoint(checkpoints, cp); err != nil {
			r.logf("registry: legacy registry at %s carries an INVALID conversation checkpoint: %v", path, err)
			return err
		}
	}
	sidecarPath := path + ".checkpoints"
	sidecar, err := readLegacySidecar(sidecarPath)
	if err != nil {
		r.logf("registry: legacy checkpoint sidecar at %s is unusable — refusing to import a partial roster: %v", sidecarPath, err)
		return err
	}
	for _, cp := range sidecar {
		if err := mergeCheckpoint(checkpoints, cp); err != nil {
			r.logf("registry: legacy checkpoint sidecar at %s carries an INVALID checkpoint: %v", sidecarPath, err)
			return err
		}
	}

	if err := r.writeImport(path, records, checkpoints); err != nil {
		return err
	}
	r.logf("registry: ONE-TIME IMPORT of the legacy JSON registry at %s complete — records=%d checkpoints=%d (sidecar checkpoints=%d); the tables are the sole authority from here, and the file is left in place as inert history",
		path, len(records), len(checkpoints), len(sidecar))
	return nil
}

// readLegacySidecar reads the pre-SQLite checkpoint sidecar. A missing sidecar
// is normal (only v2 wrote one).
func readLegacySidecar(path string) ([]ConversationCheckpoint, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("registry: read legacy checkpoint sidecar %s: %w", path, err)
	}
	var doc legacyCheckpointFileShape
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("registry: parse legacy checkpoint sidecar %s: %w", path, err)
	}
	if doc.Version != 1 {
		return nil, fmt.Errorf("registry: unsupported legacy checkpoint sidecar version %d in %s", doc.Version, path)
	}
	return doc.Checkpoints, nil
}

// writeImport lands the imported state and the stamp in ONE transaction, so a
// crash mid-import can never leave a half-imported roster that the next boot
// would then decline to complete.
func (r *Registry) writeImport(path string, records map[string]Record, checkpoints map[ConversationIdentity]ConversationCheckpoint) error {
	tx, err := r.db.Begin()
	if err != nil {
		return fmt.Errorf("registry: begin legacy import transaction: %w", err)
	}
	committed := false
	defer func() {
		if committed {
			return
		}
		if err := tx.Rollback(); err != nil && !errors.Is(err, sql.ErrTxDone) {
			r.logf("registry: legacy import rollback FAILED: %v", err)
		}
	}()
	if err := saveState(tx, &registryState{records: records, checkpoints: checkpoints}); err != nil {
		r.logf("registry: legacy import WRITE FAILED: %v", err)
		return err
	}
	if _, err := tx.Exec(`INSERT INTO registry_meta(key, value) VALUES (?, ?)`,
		metaLegacyImport, fmt.Sprintf("%s@%s", path, time.Now().UTC().Format(time.RFC3339))); err != nil {
		r.logf("registry: legacy import STAMP FAILED: %v", err)
		return fmt.Errorf("registry: stamp legacy import: %w", err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("registry: commit legacy import: %w", err)
	}
	committed = true
	return nil
}

// legacyImportStamp returns the recorded import stamp, or "" when none.
func (r *Registry) legacyImportStamp() (string, error) {
	var stamp string
	err := r.db.QueryRow(`SELECT value FROM registry_meta WHERE key = ?`, metaLegacyImport).Scan(&stamp)
	if errors.Is(err, sql.ErrNoRows) {
		return "", nil
	}
	if err != nil {
		return "", fmt.Errorf("registry: read legacy import stamp: %w", err)
	}
	return stamp, nil
}

// tablesPopulated reports whether either registry table already holds rows.
func (r *Registry) tablesPopulated() (bool, error) {
	var n int
	if err := r.db.QueryRow(
		`SELECT (SELECT COUNT(*) FROM session_record) + (SELECT COUNT(*) FROM conversation_checkpoint)`,
	).Scan(&n); err != nil {
		return false, fmt.Errorf("registry: count existing rows: %w", err)
	}
	return n > 0, nil
}
