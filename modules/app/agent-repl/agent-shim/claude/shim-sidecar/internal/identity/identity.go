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
	"sort"
	"strconv"
	"strings"

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

	// unlinked remembers the ids the fallback glob looked for and did not find
	// SINCE THE LAST REFRESH, so a miss costs one glob per refresh rather than
	// one per poll tick.
	//
	// THE MISS USED TO BE RE-CHECKED EVERY TIME, and that was a defensible
	// reading of a cost that turned out to be wrong. "No record names this id"
	// is indeed the answer a rotation invalidates, but the re-check is not
	// per-rescan: `rekeyRotations` runs it FOR EVERY WATCHER ON EVERY POLL TICK.
	// On the owner's machine that is ~2900 globs a second, ~1100 of them for
	// cold runs whose ids will never resolve — a 10s `sample` of the live
	// sidecar (pid 96084) found it holding 76-101% of a core in steady state,
	// inside filepath.Glob under Resolve, with poll ticks running seconds long.
	//
	// THE CACHE IS BOUNDED BY THE ONE EVENT THAT CAN FALSIFY IT: a link file
	// appearing. `RecheckLinks` asks the LINK DIRECTORIES whether that has
	// happened — one readdir of `<state>/shim` and one stat per workspace, a
	// constant per tick rather than one glob per watcher — and clears the map
	// when it has; `Refresh` clears it outright alongside the other two. A
	// positive answer the fallback glob finds is still cached exactly as before,
	// and a miss is still re-globbed the moment anything writes a link.
	unlinked map[string]bool
	// linkStamp fingerprints the link directories as of the last RecheckLinks:
	// each `<state>/shim/*/vendor-id` and its mtime, which moves when a file is
	// created in it. An empty stamp means no check has run yet.
	linkStamp string
	// recordStamp fingerprints EVERY identity record directory as of the last
	// refresh: each `<state>/shim/<key>` (agent-id.json is written by
	// tmp-and-rename there) and its `vendor-id`, with their mtimes. It is what
	// lets RefreshIfMoved answer "has any record changed" with one readdir and
	// two stats per workspace instead of re-reading every record.
	recordStamp string
	// globs counts filepath.Glob calls, for the suite that pins the poll
	// path's cost.
	globs int
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
		unlinked:  map[string]bool{},
		warned:    map[string]bool{},
	}
}

// Refresh re-reads every identity record under the state root.
//
// It runs on the rescan interval, beside discovery, because a rotation's link
// file appears without warning and nothing notifies this process of it. A
// missing state root is NOT an error: the sidecar may be running beside a
// daemon that has not started a shim yet.
func (i *Index) Refresh() { i.refresh(true) }

// RefreshKeepingMisses is Refresh for the POLL PATH: it re-reads every record
// but keeps the remembered misses unless a link directory moved (see refresh).
// The rescan's Refresh stays unconditional, which is the net under a link
// written inside the same mtime tick as the fingerprint.
func (i *Index) RefreshKeepingMisses() { i.refresh(false) }

func (i *Index) refresh(clearMisses bool) {
	if i.stateDir == "" {
		return
	}
	// Cleared together, because a record that has been REMOVED must stop
	// answering: an index that only ever grew would go on booking rows against
	// a link nobody stands behind any more.
	i.links = map[string]Resolution{}
	i.originals = map[string]string{}
	// THE NEGATIVE CACHE GOES ONLY IF A LINK CAN HAVE APPEARED. A miss is
	// falsified by exactly one event, a link file landing, and that moves its
	// `vendor-id` directory's mtime, which the fingerprint reads. Refresh runs
	// on every poll tick whose change probe found anything, and an active
	// session creates files nearly every tick; clearing the misses on each one
	// sent every watcher back to a per-id glob over every shim directory,
	// which held 74% of the sidecar's CPU (2026-09-24 profile, 2085 watchers,
	// 132 shim directories).
	stamp := i.linkFingerprint()
	if clearMisses || stamp != i.linkStamp {
		i.unlinked = map[string]bool{}
	}
	i.linkStamp = stamp
	i.recordStamp = i.recordFingerprint()

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

// RefreshIfMoved re-reads every record, keeping the remembered misses as
// RefreshKeepingMisses does, but ONLY when a record directory changed since the
// last refresh, and reports whether it did.
//
// IT IS AN OPTIMIZATION FOR THE ACTIVE-WORKSPACE PROBE, which runs on every poll
// tick. Re-reading all 132 agent-id.json files and every link each second would
// be ~150 file reads a second for records that change a handful of times a day;
// the fingerprint is one readdir of `<state>/shim` plus two stats per workspace
// (~265 stats on the owner's machine, 2026-09-24). Every record the shim writes
// lands by tmp-and-rename or removal inside one of those directories, which
// moves that directory's mtime, so the fingerprint cannot miss a record the
// shim wrote. The rescan's unconditional Refresh stays the backstop for a write
// inside the same mtime tick as the fingerprint.
func (i *Index) RefreshIfMoved() bool {
	if i.stateDir == "" {
		return false
	}
	if i.recordStamp != "" && i.recordFingerprint() == i.recordStamp {
		return false
	}
	i.refresh(false)
	return true
}

// Workspaces answers each workspace key an agent-id.json names, mapped to the
// conversation's ORIGINAL vendor session id — the book that workspace's shim
// writes, as of the last refresh.
func (i *Index) Workspaces() map[string]string {
	out := make(map[string]string, len(i.originals))
	for original, key := range i.originals {
		if key == "" {
			continue
		}
		out[key] = original
	}
	return out
}

// Lookup is Resolve WITHOUT the disk fallback: it answers from the records the
// last refresh read and never globs.
//
// IT IS AN OPTIMIZATION FOR THE ACTIVE-WORKSPACE GATE, which asks about every
// discovered file on every rescan (~2100 on the owner's machine). Resolve's
// miss path is one glob over every workspace's link directory per unrecorded
// id per refresh — the very per-id glob that held most of a core before the
// negative cache existed. The gate does not need it: the active-workspace probe
// refreshes the index on the tick a record directory moves and then re-offers
// every file it gated out, so an answer that is one tick behind the disk is
// corrected on the next tick.
func (i *Index) Lookup(vendorSessionID string) Resolution {
	if linked, ok := i.links[vendorSessionID]; ok {
		return linked
	}
	if key, ok := i.originals[vendorSessionID]; ok {
		return Resolution{Original: vendorSessionID, Source: SourceAgentIDRecord, WorkspaceKey: key}
	}
	return Resolution{Original: vendorSessionID, Source: SourceUnrecorded}
}

// Resolve answers which book a transcript carrying vendorSessionID writes to.
//
// A MISS IS RE-CHECKED ON DISK ONCE PER REFRESH. The index is refreshed at every
// rescan and on every poll tick where the change probe found something, and a
// rotation's link file can land between two refreshes — in the exact window
// where the rotated transcript is first discovered. A lookup of the known path
// (the shim's own "resolve any transcript to its book with one stat instead of a
// scan") closes that window.
//
// IT IS AN IN-MEMORY LOOKUP ON THE POLL PATH, which is what `unlinked` buys.
// `rekeyRotations` calls this for EVERY WATCHER ON EVERY TICK; re-globbing each
// miss made a steady-state sidecar spend a whole core on filepath.Glob for ids
// that were never going to resolve. A miss now costs one glob per refresh, and
// the refresh is the only event that can make yesterday's miss wrong.
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
	if i.unlinked[vendorSessionID] {
		return Resolution{Original: vendorSessionID, Source: SourceUnrecorded}
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
	i.unlinked[vendorSessionID] = true
	return Resolution{Original: vendorSessionID, Source: SourceUnrecorded}
}

// RecheckLinks drops the remembered misses IF the shim's link directories have
// changed since the last check, and is the poll path's whole answer to "has a
// rotation happened since I last looked".
//
// IT ASKS THE DIRECTORY, NOT THE FILES — the same shape the discovery change
// probe already uses, and for the same reason. A link file lands INSIDE
// `<state>/shim/<key>/vendor-id/`, so creating one moves that directory's mtime;
// one readdir of `<state>/shim` plus one stat per workspace therefore answers
// for every id at once. That is a constant per poll tick, against the ~2900
// per-watcher globs a second that re-checking each miss cost.
//
// IT IS WHY THE NEGATIVE CACHE DOES NOT COST A GUARANTEE. "The link that appears
// mid-tail moves the book on the very next poll" is a rule about a rotation
// while a transcript is being read, and this is the event that rule turns on —
// not the rescan cadence, which would have delayed it by a whole rescan.
//
// It is ALLOWED TO MISS, and Refresh is why that is safe: a write inside the
// microseconds between the stat and the readdir, or a filesystem whose mtime
// granularity swallowed one, is found by the next full refresh, exactly as the
// discovery probe's misses are found by the next full scan.
func (i *Index) RecheckLinks() {
	if i.stateDir == "" {
		return
	}
	stamp := i.linkFingerprint()
	if stamp == i.linkStamp {
		return
	}
	i.linkStamp = stamp
	if len(i.unlinked) == 0 {
		return
	}
	i.log.With(logging.Context{Operation: "identity-recheck", Repeat: logging.Repeat(len(i.unlinked))}).LogVerbose(
		"the shim's link directories changed since the last check; %d remembered miss(es) are dropped so the next lookup asks the disk again", len(i.unlinked))
	i.unlinked = map[string]bool{}
}

// linkFingerprint renders the link directories and their mtimes, in a stable
// order, so two checks over an unchanged tree read identically.
//
// A DIRECTORY THAT CANNOT BE STAT'D IS RENDERED AS SUCH rather than skipped: a
// workspace whose records became unreadable is a CHANGE, and skipping it would
// make the tree look untouched.
func (i *Index) linkFingerprint() string {
	dirs := i.glob(filepath.Join(i.stateDir, "shim", "*"))
	sort.Strings(dirs)
	var out strings.Builder
	for _, dir := range dirs {
		linkDir := filepath.Join(dir, "vendor-id")
		out.WriteString(linkDir)
		if info, err := os.Stat(linkDir); err == nil {
			out.WriteString("|" + strconv.FormatInt(info.ModTime().UnixNano(), 10))
		} else {
			out.WriteString("|absent")
		}
		out.WriteByte('\n')
	}
	return out.String()
}

// recordFingerprint renders every identity record directory — each
// `<state>/shim/<key>` and its `vendor-id` — with its mtime, in a stable order.
// A directory that cannot be stat'd is rendered as such, so a workspace whose
// records became unreadable reads as a change rather than as untouched.
func (i *Index) recordFingerprint() string {
	dirs := i.glob(filepath.Join(i.stateDir, "shim", "*"))
	sort.Strings(dirs)
	var out strings.Builder
	for _, dir := range dirs {
		for _, path := range []string{dir, filepath.Join(dir, "vendor-id")} {
			out.WriteString(path)
			if info, err := os.Stat(path); err == nil {
				out.WriteString("|" + strconv.FormatInt(info.ModTime().UnixNano(), 10))
			} else {
				out.WriteString("|absent")
			}
			out.WriteByte('\n')
		}
	}
	return out.String()
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
	i.globs++
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
