package registry

import "path/filepath"

// ---------------------------------------------------------------------------
// Phantom workspace-key detection at boot
// ---------------------------------------------------------------------------
//
// A record's CWD is its workspace key, and the daemon used to accept whatever
// spelling a command frame carried. A trailing separator therefore minted a
// PHANTOM record — a second workspace, with its own session, for a directory
// that already had one. Ingress canonicalizes the key now, so no new phantom
// can appear, but rows written before that still sit in the store.
//
// This reports them ONCE at boot and does nothing else. Rewriting a record's
// key would move a session between workspaces and could collide with the
// canonical row that already exists, so the repair is deliberately a human's
// decision, not a startup side effect.

// noncanonicalRecords returns the records whose workspace key is not its own
// canonical spelling, sorted by session id via All.
func noncanonicalRecords(records []Record) []Record {
	var out []Record
	for _, rec := range records {
		if rec.CWD == "" {
			continue
		}
		if filepath.Clean(rec.CWD) != rec.CWD {
			out = append(out, rec)
		}
	}
	return out
}

// reportPhantomWorkspaceKeys logs every record carrying a noncanonical
// workspace key. Detection only — no record is touched.
func (r *Registry) reportPhantomWorkspaceKeys() {
	phantoms := noncanonicalRecords(r.All())
	if len(phantoms) == 0 {
		return
	}
	r.logf("registry: %d record(s) carry a NONCANONICAL workspace key; each is a phantom workspace beside the clean spelling and is reported, never rewritten", len(phantoms))
	for _, rec := range phantoms {
		r.logf("registry: phantom workspace key session_id=%s cwd=%q canonical=%q", rec.SessionID, rec.CWD, filepath.Clean(rec.CWD))
	}
}
