package workspace

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// raiseDaemonFault opens FAULT as a DAEMON-SCOPED fault unless one of its kind
// that SAME holds to be the same fault already stands: a second identical row
// would tell the operator nothing the first did not, and faults stay open
// until closed. Every record is written under OP with FIELDS and the fault's
// kind. A read of what stands that fails does not excuse leaving the fault
// unrecorded: it is recorded at ERROR, and the fault is opened anyway. An
// open that fails is recorded at ERROR.
func (v *verbs) raiseDaemonFault(ctx context.Context, log dlog.Logger, op string, fault wsm.Fault, same func(wsm.Fault) bool, fields dlog.Context) {
	fields = dlog.Merge(fields, dlog.Context{"kind": fault.Kind})
	standing, err := v.deps.Health.OpenFaults(ctx, wsm.FaultScope{Kind: fault.Kind})
	if err != nil {
		log.Error(op, "could not check whether the daemon fault already stands; it is recorded anyway",
			dlog.Merge(fields, dlog.Context{"cause": err.Error()}))
	}
	for _, f := range standing {
		if f.Workspace == nil && same(f) {
			log.Debug(op, "the daemon fault already stands", dlog.Merge(fields, dlog.Context{"fault": string(f.ID)}))
			return
		}
	}
	fault.Workspace = nil
	if _, err := v.deps.Health.OpenFault(ctx, fault); err != nil {
		log.Error(op, "could not record the daemon fault", dlog.Merge(fields, dlog.Context{"cause": err.Error()}))
	}
}
