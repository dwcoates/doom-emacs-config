package sessioncontroller

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// resumeIdentityTracker proves that each resumed query first reports the
// exact vendor conversation named by QueryCreated. A later SDK rotation is a
// different lifecycle fact: it occurs only after this first identity has been
// confirmed and therefore cannot disguise a failed resume as a fresh query.
//
// A `/clear` DISCHARGES that commitment, and this is where the discharge is
// held. The command discards the conversation, the vendor answers by rotating
// the session uuid, and from then on a query reporting an identity other than
// the one it was asked to resume is the command WORKING rather than a failed
// resume. Before this, a clear submitted before the resumed query had reported
// its first identity — which is exactly what reviveSession{clear} does, since
// the gate keeps every other prompt out until the cut lands — made the
// rotation the FIRST observation, and the tracker read the vendor obeying the
// daemon as a resume that had silently landed on the wrong conversation. The
// controller was killed for it, and the revival that owned it wedged with it.
type resumeIdentityTracker struct {
	queries map[string]*resumeQueryIdentity
	// clearDispatched records that this session has been handed a `/clear`
	// whose rotation has not been observed yet. It is session-scoped rather
	// than query-scoped because the daemon dispatches the command to a SESSION
	// and one shim owns exactly one query, so there is no second query the
	// consent could be misapplied to.
	clearDispatched bool
}

// noteContextClearDispatched records the daemon's own commitment to rotate this
// session's vendor conversation. Called from the dispatch of a `/clear`
// (promptdispatch.go), on the same classification the clearing axis is opened
// on, so the two cannot disagree about whether a clear is running.
func (t *resumeIdentityTracker) noteContextClearDispatched() {
	t.clearDispatched = true
}

type resumeQueryIdentity struct {
	requestedVendorSessionID string
	observed                 bool
	mismatch                 *resumeIdentityMismatch
}

type resumeIdentityMismatch struct {
	queryInstanceID          string
	requestedVendorSessionID string
	observedVendorSessionID  string
}

// resumeIdentityAdoption is a rotation the daemon ASKED FOR: the identity a
// resumed query reported instead of the one it was asked to resume, after a
// `/clear` this daemon dispatched discharged the resume commitment.
//
// It is a distinct value from resumeIdentityMismatch rather than a nil one,
// because it is EVIDENCE and must be logged as loudly as the fatal case is. The
// two describe the same observation and differ only in whether the daemon had
// consented to it, which is precisely the distinction a reader of the log needs.
type resumeIdentityAdoption struct {
	queryInstanceID          string
	requestedVendorSessionID string
	adoptedVendorSessionID   string
}

func newResumeIdentityTracker() *resumeIdentityTracker {
	return &resumeIdentityTracker{queries: make(map[string]*resumeQueryIdentity)}
}

// observe returns a mismatch on every observation after it is proven. Query lifecycle records from an
// older producer that predate QueryCreated are accepted for replay
// compatibility; they cannot establish or disprove a resume commitment.
//
// A ROTATION THE DAEMON ASKED FOR IS ADOPTED, NOT REFUSED. When a `/clear` has
// been dispatched for this session, the discharged commitment is retired and
// the observed identity becomes this query's identity — reported as an
// adoption so the substitution is still on the record. Every OTHER changed
// identity is as fatal as it ever was: the discharge is consumed by the one
// rotation it consented to, so a second unexplained change is refused.
func (t *resumeIdentityTracker) observe(ev *corev1.Event) (*resumeIdentityMismatch, *resumeIdentityAdoption, error) {
	lifecycle := ev.GetQueryLifecycle()
	if lifecycle == nil {
		return nil, nil, nil
	}
	queryID := lifecycle.GetQueryInstanceId()
	if queryID == "" {
		return nil, nil, fmt.Errorf("query lifecycle omitted query_instance_id")
	}
	if created := lifecycle.GetCreated(); created != nil {
		if _, exists := t.queries[queryID]; exists {
			return nil, nil, fmt.Errorf("query %q emitted QueryCreated more than once", queryID)
		}
		identity := &resumeQueryIdentity{}
		if resumed := created.GetResumed(); resumed != nil {
			if resumed.GetRequestedVendorSessionId() == "" {
				return nil, nil, fmt.Errorf("resumed query %q omitted requested_vendor_session_id", queryID)
			}
			identity.requestedVendorSessionID = resumed.GetRequestedVendorSessionId()
		}
		t.queries[queryID] = identity
		return nil, nil, nil
	}
	runtime := lifecycle.GetRuntimeObserved()
	if runtime == nil {
		return nil, nil, nil
	}
	identity, exists := t.queries[queryID]
	if !exists {
		return nil, nil, nil
	}
	observed := runtime.GetIdentity().GetVendorSessionId()
	if observed == "" {
		return nil, nil, fmt.Errorf("query %q runtime observation omitted vendor_session_id", queryID)
	}
	if identity.observed {
		return identity.mismatch, nil, nil
	}
	identity.observed = true
	if identity.requestedVendorSessionID == "" || identity.requestedVendorSessionID == observed {
		return nil, nil, nil
	}
	if t.clearDispatched {
		// THE DISCHARGE IS SPENT HERE, on the one rotation it consented to. A
		// later unexplained identity change on this session meets the ordinary
		// refusal again, so consenting to a clear's rotation is not consenting
		// to every rotation afterwards.
		t.clearDispatched = false
		adopted := &resumeIdentityAdoption{
			queryInstanceID:          queryID,
			requestedVendorSessionID: identity.requestedVendorSessionID,
			adoptedVendorSessionID:   observed,
		}
		identity.requestedVendorSessionID = observed
		return nil, adopted, nil
	}
	identity.mismatch = &resumeIdentityMismatch{
		queryInstanceID:          queryID,
		requestedVendorSessionID: identity.requestedVendorSessionID,
		observedVendorSessionID:  observed,
	}
	return identity.mismatch, nil, nil
}

type resumeIdentityMismatchError struct {
	detail *frontendv1.SessionResumeFailure
}

func (e *resumeIdentityMismatchError) Error() string {
	return fmt.Sprintf("resumed query reported vendor session %q instead of requested session %q",
		e.detail.GetIdentityMismatch().GetReplacementClaudeSessionId(), e.detail.GetClaudeSessionId())
}

func (e *resumeIdentityMismatchError) SessionResumeFailureDetail() *frontendv1.SessionResumeFailure {
	return e.detail
}

func newResumeIdentityMismatchError(sessionID string, mismatch *resumeIdentityMismatch) *resumeIdentityMismatchError {
	// sessionID still names the record in the wrapped error's own text; it no
	// longer rides the evidence, because a rendering frontend has no session
	// vocabulary to read it with. What the card shows is the VENDOR
	// conversation, which is content, and that is carried unchanged.
	detail := &frontendv1.SessionResumeFailure{
		ClaudeSessionId: mismatch.requestedVendorSessionID,
		Attempt: &frontendv1.SessionResumeFailure_AutomaticRestore{
			AutomaticRestore: &frontendv1.SessionResumeFailureAutomaticRestore{},
		},
		Cause: &frontendv1.SessionResumeFailure_IdentityMismatch{
			IdentityMismatch: &frontendv1.SessionResumeFailureIdentityMismatch{
				ReplacementClaudeSessionId: mismatch.observedVendorSessionID,
			},
		},
	}
	return &resumeIdentityMismatchError{detail: detail}
}
