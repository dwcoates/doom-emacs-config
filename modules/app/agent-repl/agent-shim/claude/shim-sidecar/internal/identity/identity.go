// Package identity resolves a vendor session id to the conversation's ORIGINAL
// vendor session id — the shim-minted main AgentId a transcript's records must
// be booked under.
//
// WHY THIS EXISTS. A `/clear` (and a `forkSession`) mints a NEW vendor session
// id and a NEW transcript file, and NOTHING IN EITHER FILE LINKS THEM: across
// the shim's 1,107-transcript survey no `forkedFrom`, `parentSessionId` or
// `resumedFrom` key exists, and the SDK's own ForkSessionResult is
// `{ sessionId }` alone. A file-plane reader that books the rotated
// transcript's records under the id its FILENAME carries opens a second book
// for one conversation — and the store rightly refuses the batch ("would move
// the row from book A to book B — an upsert supersedes a row's content, never
// its identity"), because the same rows already exist under the original.
//
// SO THE SHIM WRITES THE LINK THE FILES LACK, and this package is its file-only
// reader. Two shapes under `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/`, both
// written by agent-shim/claude/shim/src/engine/identity.ts and both read here
// under the field names that file declares as the on-disk contract:
//
//	agent-id.json            {original_vendor_session_id, workspace_key, minted_at_ms}
//	vendor-id/<vendor>.json  {vendor_session_id, original_vendor_session_id, linked_at_ms}
//
// THE WORKSPACE KEY IS NEVER DERIVED HERE, only read. The shim keys the
// directory by md5(cwd)[:8] of the workspace it was spawned in; the sidecar
// knows a transcript by a config-root path whose project segment is the
// vendor's LOSSY, non-invertible cwd slug, so anything this package computed
// from it would be a guess. The directories are ENUMERATED instead — one glob
// over `<state>/shim/*` — and the key is whatever the records themselves say.
//
// NO STATE DIR MEANS NO RESOLUTION, NOT A GUESS. An Index with an empty state
// dir answers every id with itself, which is the reader's behavior before this
// package existed: a transcript with no identity record on disk is booked by
// its own id, exactly as R9's resume rule derives it.
package identity

import (
	"encoding/json"
	"os"
	"path/filepath"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

// Source names WHERE a resolution came from, so a record can say whether an
// answer was read off the shim's files or is the reader's own default.
type Source string

const (
	// SourceVendorLink: a `vendor-id/<id>.json` pointer named the original.
	// This is the rotation case — the id on the transcript is NOT the book.
	SourceVendorLink Source = "vendor_link"
	// SourceAgentIDRecord: the id IS an `agent-id.json`'s
	// original_vendor_session_id, so it is its own book, stated by the shim
	// rather than assumed.
	SourceAgentIDRecord Source = "agent_id_record"
	// SourceUnrecorded: no identity record names this id. It is its own book by
	// the R9 resume rule, which is a derivation and is logged as one.
	SourceUnrecorded Source = "unrecorded"
)

// Resolution is one answer: the book, where it came from, and the workspace
// whose shim wrote the record (empty when nothing did).
type Resolution struct {
	Original     string
	Source       Source
	WorkspaceKey string
}

// Rotated reports that the transcript's own id is NOT its book.
func (r Resolution) Rotated(vendorSessionID string) bool {
	return r.Original != "" && r.Original != vendorSessionID
}

// agentIDRecord is engine/identity.ts's AgentIdentityRecord. The json tags are
// that file's declared on-disk contract and are the reason this struct exists
// rather than a map.
type agentIDRecord struct {
	OriginalVendorSessionID string `json:"original_vendor_session_id"`
	WorkspaceKey            string `json:"workspace_key"`
	MintedAtMs              int64  `json:"minted_at_ms"`
}

// vendorLinkRecord is engine/identity.ts's VendorSessionLink.
type vendorLinkRecord struct {
	VendorSessionID         string `json:"vendor_session_id"`
	OriginalVendorSessionID string `json:"original_vendor_session_id"`
	LinkedAtMs              int64  `json:"linked_at_ms"`
}

// Index is the in-memory view of the shim's identity files.
//
// IT IS NOT SAFE FOR CONCURRENT USE, and does not need to be: every caller runs
// on the sidecar's single cycle goroutine, exactly like the owner index beside
// it.
type Index struct {
	stateDir string
	log      *logging.Bound

	// links maps a ROTATED vendor session id to its original.
	links map[string]Resolution
	// originals maps an original vendor session id to the workspace key whose
	// agent-id.json states it.
	originals map[string]string
	// warned remembers the record files whose defect has already been stated,
	// so a refresh every few seconds does not repeat one line forever while a
	// file stays broken. It SURVIVES a refresh, exactly as discover's warnedMeta
	// does: the defect is a property of the file, not of the pass that saw it.
	warned map[string]bool

	// THERE IS NO NEGATIVE CACHE, deliberately. "No record names this id" is the
	// answer that goes stale the instant a rotation writes one, and remembering
	// it is exactly how a mid-tail rotation stays mis-booked until a restart.
	// A miss costs one readdir of `<state>/shim` per rescan, which is the price
	// of never being wrong about a book.
}

// New builds an index over one state root. An empty stateDir builds an index
// that resolves nothing, which is the honest answer when no state root was
// configured.
func New(stateDir string, log *logging.Bound) *Index {
	return &Index{
		stateDir:  stateDir,
		log:       log,
		links:     map[string]Resolution{},
		originals: map[string]string{},
		warned:    map[string]bool{},
	}
}

// Refresh re-reads every identity record under the state root.
//
// It runs on the rescan interval, beside discovery, because a rotation's link
// file appears without warning and nothing notifies this process of it. A
// missing state root is NOT an error: the sidecar may be running beside a
// daemon that has not started a shim yet.
func (i *Index) Refresh() {
	if i.stateDir == "" {
		return
	}
	// Cleared together, because a record that has been REMOVED must stop
	// answering: an index that only ever grew would go on booking rows against
	// a link nobody stands behind any more.
	i.links = map[string]Resolution{}
	i.originals = map[string]string{}

	for _, path := range i.glob(filepath.Join(i.stateDir, "shim", "*", "agent-id.json")) {
		var record agentIDRecord
		if !i.readJSON(path, &record) {
			continue
		}
		if record.OriginalVendorSessionID == "" {
			i.malformed(path, "agent-id.json names no original_vendor_session_id, so it cannot say which book its conversation writes to")
			continue
		}
		i.originals[record.OriginalVendorSessionID] = record.WorkspaceKey
	}

	for _, path := range i.glob(filepath.Join(i.stateDir, "shim", "*", "vendor-id", "*.json")) {
		record, ok := i.readLink(path)
		if !ok {
			continue
		}
		i.links[record.VendorSessionID] = Resolution{
			Original:     record.OriginalVendorSessionID,
			Source:       SourceVendorLink,
			WorkspaceKey: workspaceKeyOfLink(path),
		}
	}

	i.log.With(logging.Context{Operation: "identity-refresh"}).LogVerbose(
		"read the shim's identity records under %s: %d minted identit(ies), %d vendor-session link(s)",
		i.stateDir, len(i.originals), len(i.links))
}

// Resolve answers which book a transcript carrying vendorSessionID writes to.
//
// A MISS IS RE-CHECKED ON DISK BEFORE IT IS BELIEVED. The index is refreshed on
// the rescan interval, and a rotation's link file can land between two
// refreshes — in the exact window where the rotated transcript is first
// discovered. A lookup of the known path (the shim's own "resolve any transcript
// to its book with one stat instead of a scan") closes that window. A HIT is
// remembered; a MISS is not, because "nothing links this id" is precisely the
// answer a rotation invalidates.
func (i *Index) Resolve(vendorSessionID string) Resolution {
	if vendorSessionID == "" || i.stateDir == "" {
		return Resolution{Original: vendorSessionID, Source: SourceUnrecorded}
	}
	if linked, ok := i.links[vendorSessionID]; ok {
		return linked
	}
	if key, ok := i.originals[vendorSessionID]; ok {
		return Resolution{Original: vendorSessionID, Source: SourceAgentIDRecord, WorkspaceKey: key}
	}
	for _, path := range i.glob(filepath.Join(i.stateDir, "shim", "*", "vendor-id", vendorSessionID+".json")) {
		record, ok := i.readLink(path)
		if !ok {
			continue
		}
		resolution := Resolution{
			Original:     record.OriginalVendorSessionID,
			Source:       SourceVendorLink,
			WorkspaceKey: workspaceKeyOfLink(path),
		}
		i.links[record.VendorSessionID] = resolution
		return resolution
	}
	return Resolution{Original: vendorSessionID, Source: SourceUnrecorded}
}

// readLink reads and validates one vendor-session pointer file.
func (i *Index) readLink(path string) (vendorLinkRecord, bool) {
	var record vendorLinkRecord
	if !i.readJSON(path, &record) {
		return record, false
	}
	if record.OriginalVendorSessionID == "" {
		i.malformed(path, "a vendor-session link names no original_vendor_session_id; booking a transcript under the empty id is worse than booking it under its own")
		return record, false
	}
	if record.VendorSessionID == "" {
		// The file name IS the rotated id (vendorLinkPath), so a record that
		// omits the field is still usable — but it is a defect in the writer and
		// is stated as one rather than silently repaired.
		record.VendorSessionID = vendorIDOfLink(path)
		i.malformed(path, "a vendor-session link names no vendor_session_id; its file name is read as the rotated id instead")
	}
	return record, true
}

// glob enumerates one shape under the state root. A glob error is a malformed
// PATTERN, which is this package's own bug, so it is stated at error rather
// than passed off as "no records".
func (i *Index) glob(pattern string) []string {
	matches, err := filepath.Glob(pattern)
	if err != nil {
		i.log.With(logging.Context{Operation: "identity-refresh", Level: "error", Path: pattern}).Log(
			"the identity-record pattern is malformed, so no record matching it can be read: %v", err)
		return nil
	}
	return matches
}

// malformed states a record's defect ONCE for the life of the process. The same
// broken file is read by every refresh and by every direct lookup that globs
// over it, and a line per read would drown the log without adding a fact.
func (i *Index) malformed(path, what string) {
	if i.warned[path] {
		return
	}
	i.warned[path] = true
	i.log.With(logging.Context{Operation: "identity-record", Level: "warn", Path: path}).Log(
		"ignoring an identity record: %s", what)
}

// readJSON reads one identity record. An ABSENT file is silent — the shim may
// be about to write it, and a glob's match can vanish between the readdir and
// the read — while an unreadable or unparseable one is stated, because a record
// that exists and cannot be read is the shape that silently splits a book.
func (i *Index) readJSON(path string, into any) bool {
	raw, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return false
		}
		i.malformed(path, "it could not be read, so the conversation it names resolves to no book through it: "+err.Error())
		return false
	}
	if err := json.Unmarshal(raw, into); err != nil {
		i.malformed(path, "it could not be parsed, so the conversation it names resolves to no book through it: "+err.Error())
		return false
	}
	return true
}

// workspaceKeyOfLink reads the workspace key out of a link file's path:
// <state>/shim/<workspace-key>/vendor-id/<vendor-id>.json.
func workspaceKeyOfLink(path string) string {
	return filepath.Base(filepath.Dir(filepath.Dir(path)))
}

// vendorIDOfLink reads the rotated vendor session id out of a link file's name.
func vendorIDOfLink(path string) string {
	return filepath.Base(path[:len(path)-len(filepath.Ext(path))])
}
