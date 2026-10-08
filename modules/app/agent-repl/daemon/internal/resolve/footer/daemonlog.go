package footer

import (
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE DAEMON'S OWN WARNINGS AND ERRORS about a workspace reach its strip as the
// `daemon_warning` and `daemon_error` transients (owner ruling, 2026-09-28:
// every non-blocking error or warning about the turn in flight is transient).
//
// THEY ARRIVE THROUGH ONE TEE, never a hook beside a call site: dlog hands every
// Warn and Error a WORKSPACE logger emits to the bound RecordTee, which is this
// resolver (bound at boot, dlog.Surfaces.BindRecordTee). The same rule is how
// faults reach the strip — through the one place they are opened.
//
// THE FOOTER'S OWN RECORDS ARE EXCLUDED, or the tee would feed back into
// itself: a record this resolver writes while raising a line would raise
// another. Every record the footer resolver writes is named under
// footerOperations (the logging contract's `daemon.<package>.<verb>`), and the
// exclusion is decided before any lock is taken, because a footer record can be
// emitted while this resolver's own lock is held.

// footerOperations is the operation prefix every record of this resolver
// carries.
const footerOperations = "daemon.footer."

// OnWorkspaceRecord implements dlog.RecordTee.
//
// A record for a workspace this resolver has not bound (SetWorkspaceDir) is
// not drawn: the strip does not exist yet, and drawing it would publish a
// footer for a workspace the daemon has not registered with this resolver. It
// is noted at DEBUG on the run log, which the tee never reads.
func (r *resolver) OnWorkspaceRecord(rec dlog.WorkspaceRecord) {
	if strings.HasPrefix(rec.Operation, footerOperations) {
		return
	}
	ws := ids.WorkspaceID(rec.WorkspaceID)
	r.mu.Lock()
	s, known := r.states[ws]
	bound := known && s.log != nil
	r.mu.Unlock()
	if !bound {
		r.log.Global().Debug("daemon.footer.daemon_record_unbound",
			"a workspace warning or error arrived for a workspace the footer has not bound; it is not drawn",
			dlog.Context{"workspace_id": rec.WorkspaceID, "level": rec.Level, "operation": rec.Operation})
		return
	}
	r.mutateLine(ws, "daemon.footer.daemon_record", "the footer took a daemon warning or error about the workspace",
		dlog.Context{"level": rec.Level, "operation": rec.Operation}, func(s *wsState) {
			t := &frontendv1.FooterActivityTransient{}
			if rec.Level == dlog.LevelError {
				t.Kind = &frontendv1.FooterActivityTransient_DaemonError{DaemonError: &frontendv1.FooterActivityTransientDaemonError{
					Operation: rec.Operation, Message: rec.Message}}
			} else {
				t.Kind = &frontendv1.FooterActivityTransient_DaemonWarning{DaemonWarning: &frontendv1.FooterActivityTransientDaemonWarning{
					Operation: rec.Operation, Message: rec.Message}}
			}
			r.raiseTransient(ws, s, "", t)
		})
}
