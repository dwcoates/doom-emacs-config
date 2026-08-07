package registry

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
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
	// nil `before`: the import ESTABLISHES the tables' contents rather than
	// advancing them, so there is no prior state to write the difference
	// against and both tables are cleared first (saveState).
	if err := saveState(tx, nil, &registryState{records: records, checkpoints: checkpoints}); err != nil {
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

// retiredMarkerSuffix names the deprecation record written beside the retired
// JSON registry. Uppercase on purpose: the file exists to be noticed by a human
// listing the state directory.
const retiredMarkerSuffix = ".RETIRED"

// retirementNote is the deprecation record's human-facing explanation. It is
// the answer to the only question the retired file provokes: "why is this
// stale?".
const retirementNote = "The JSON session registry is RETIRED and is NOT written by any daemon. " +
	"Its contents are frozen at the moment of the one-time import named by import_stamp; " +
	"they do not describe any session created afterwards. " +
	"The successor authority is the SQLite state store named by successor_authority " +
	"(tables session_record and conversation_checkpoint). " +
	"Query that store, never this file. This file and its checkpoint sidecar are kept only as pre-migration history."

// legacyRetirement is the on-disk deprecation record. It is written as JSON so
// an investigation that reaches for the dead file finds a machine-readable
// pointer at the live one instead of a stale roster.
type legacyRetirement struct {
	RetiredAt string `json:"retired_at"`
	// SuccessorAuthority is the state store that actually holds the registry.
	SuccessorAuthority string `json:"successor_authority"`
	// ImportStamp is the registry_meta stamp recording when this file's
	// contents were imported, or "" when the tables were already populated by
	// a post-migration binary and no import was ever needed.
	ImportStamp   string `json:"import_stamp,omitempty"`
	SchemaVersion int    `json:"schema_version"`
	Note          string `json:"note"`
	// ResidueRemoved lists the retired writer's orphaned temp and lock files
	// swept on this pass. They are partial writes and a lock from the atomic
	// write-rename path that no longer exists; leaving them on disk reads as a
	// broken live writer rather than as the dead one it is.
	ResidueRemoved []string `json:"residue_removed,omitempty"`
}

// retireLegacyJSON records the JSON registry's retirement beside it and sweeps
// the retired writer's orphaned residue.
//
// NOTHING WRITES THE JSON REGISTRY ANY MORE — the tables are the sole
// authority (see importLegacyJSON). That is deliberate, but it is invisible on
// disk: a frozen file surrounded by orphaned `.tmp-<n>` partials and a stale
// `.lock` is indistinguishable from a live writer whose atomic rename keeps
// failing, and reading it answers identity questions with a snapshot taken at
// the migration. This makes the retirement explicit and self-describing.
//
// Best-effort by design, LOUD by requirement: the marker is a signpost, so
// failing to plant it must not take down a daemon whose registry is otherwise
// healthy — but every failure is reported through the caller's logger rather
// than skipped. Idempotent: a pass with the marker already present and no
// residue left does nothing and says nothing.
func (r *Registry) retireLegacyJSON(path, storePath string) {
	if path == "" {
		return
	}
	if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
		// No retired file: a fresh install. Nothing to mark.
		return
	} else if err != nil {
		r.logf("registry: legacy retirement STAT FAILED for %s — the retired file's deprecation record was not written: %v", path, err)
		return
	}

	residue, err := legacyResidue(path)
	if err != nil {
		r.logf("registry: legacy retirement RESIDUE SCAN FAILED for %s — orphaned temp files were left in place: %v", path, err)
		return
	}
	marker := path + retiredMarkerSuffix
	if _, err := os.Stat(marker); err == nil && len(residue) == 0 {
		return
	} else if err != nil && !errors.Is(err, os.ErrNotExist) {
		r.logf("registry: legacy retirement MARKER STAT FAILED for %s: %v", marker, err)
		return
	}

	removed := []string{}
	for _, name := range residue {
		if err := os.Remove(name); err != nil && !errors.Is(err, os.ErrNotExist) {
			r.logf("registry: legacy retirement could not remove orphaned residue %s — it will keep reading as a broken live writer: %v", name, err)
			continue
		}
		removed = append(removed, filepath.Base(name))
	}

	stamp, err := r.legacyImportStamp()
	if err != nil {
		r.logf("registry: legacy retirement could not read the import stamp — the deprecation record was not written: %v", err)
		return
	}
	record := legacyRetirement{
		RetiredAt:          time.Now().UTC().Format(time.RFC3339),
		SuccessorAuthority: storePath,
		ImportStamp:        stamp,
		SchemaVersion:      schemaVersion,
		Note:               retirementNote,
		ResidueRemoved:     removed,
	}
	if err := writeRetirementMarker(marker, record); err != nil {
		r.logf("registry: legacy retirement MARKER WRITE FAILED for %s — the retired file stays unlabelled and will keep being mistaken for live state: %v", marker, err)
		return
	}
	r.logf("registry: the JSON session registry at %s is RETIRED — successor authority is the state store at %s (tables session_record, conversation_checkpoint); deprecation record written to %s; orphaned residue swept=%d %v",
		path, storePath, marker, len(removed), removed)
}

// legacyResidue lists the retired atomic-write path's leftovers: partial
// `.tmp-<n>` writes of the roster and its checkpoint sidecar, and the lock file
// that used to guard the rename.
func legacyResidue(path string) ([]string, error) {
	var out []string
	for _, pattern := range []string{path + ".tmp-*", path + ".checkpoints.tmp-*"} {
		matches, err := filepath.Glob(pattern)
		if err != nil {
			return nil, fmt.Errorf("registry: scan legacy residue %s: %w", pattern, err)
		}
		out = append(out, matches...)
	}
	lock := path + ".lock"
	if _, err := os.Stat(lock); err == nil {
		out = append(out, lock)
	} else if !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("registry: stat legacy lock %s: %w", lock, err)
	}
	sort.Strings(out)
	return out, nil
}

// writeRetirementMarker lands the deprecation record atomically. Its temp file
// deliberately does NOT use the retired writer's `.tmp-<n>` spelling, so a
// crashed marker write can never be mistaken for — or swept as — roster residue.
func writeRetirementMarker(marker string, record legacyRetirement) error {
	data, err := json.MarshalIndent(record, "", "  ")
	if err != nil {
		return fmt.Errorf("registry: encode retirement record: %w", err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(marker), filepath.Base(marker)+".writing-")
	if err != nil {
		return fmt.Errorf("registry: create retirement record temp: %w", err)
	}
	name := tmp.Name()
	defer func() { _ = os.Remove(name) }()
	if _, err := tmp.Write(append(data, '\n')); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("registry: write retirement record: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("registry: close retirement record: %w", err)
	}
	if err := os.Chmod(name, 0o600); err != nil {
		return fmt.Errorf("registry: chmod retirement record: %w", err)
	}
	if err := os.Rename(name, marker); err != nil {
		return fmt.Errorf("registry: install retirement record %s: %w", marker, err)
	}
	return nil
}
