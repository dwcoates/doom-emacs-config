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
type resumeIdentityTracker struct {
	queries map[string]*resumeQueryIdentity
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

func newResumeIdentityTracker() *resumeIdentityTracker {
	return &resumeIdentityTracker{queries: make(map[string]*resumeQueryIdentity)}
}

// observe returns a mismatch on every observation after it is proven. Query lifecycle records from an
// older producer that predate QueryCreated are accepted for replay
// compatibility; they cannot establish or disprove a resume commitment.
func (t *resumeIdentityTracker) observe(ev *corev1.Event) (*resumeIdentityMismatch, error) {
	lifecycle := ev.GetQueryLifecycle()
	if lifecycle == nil {
		return nil, nil
	}
	queryID := lifecycle.GetQueryInstanceId()
	if queryID == "" {
		return nil, fmt.Errorf("query lifecycle omitted query_instance_id")
	}
	if created := lifecycle.GetCreated(); created != nil {
		if _, exists := t.queries[queryID]; exists {
			return nil, fmt.Errorf("query %q emitted QueryCreated more than once", queryID)
		}
		identity := &resumeQueryIdentity{}
		if resumed := created.GetResumed(); resumed != nil {
			if resumed.GetRequestedVendorSessionId() == "" {
				return nil, fmt.Errorf("resumed query %q omitted requested_vendor_session_id", queryID)
			}
			identity.requestedVendorSessionID = resumed.GetRequestedVendorSessionId()
		}
		t.queries[queryID] = identity
		return nil, nil
	}
	runtime := lifecycle.GetRuntimeObserved()
	if runtime == nil {
		return nil, nil
	}
	identity, exists := t.queries[queryID]
	if !exists {
		return nil, nil
	}
	observed := runtime.GetIdentity().GetVendorSessionId()
	if observed == "" {
		return nil, fmt.Errorf("query %q runtime observation omitted vendor_session_id", queryID)
	}
	if identity.observed {
		return identity.mismatch, nil
	}
	identity.observed = true
	if identity.requestedVendorSessionID == "" || identity.requestedVendorSessionID == observed {
		return nil, nil
	}
	identity.mismatch = &resumeIdentityMismatch{
		queryInstanceID:          queryID,
		requestedVendorSessionID: identity.requestedVendorSessionID,
		observedVendorSessionID:  observed,
	}
	return identity.mismatch, nil
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
	detail := &frontendv1.SessionResumeFailure{
		AgentReplSessionId: sessionID,
		ClaudeSessionId:    mismatch.requestedVendorSessionID,
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
