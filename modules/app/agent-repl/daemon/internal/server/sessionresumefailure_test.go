package server

import (
	"errors"
	"fmt"
	"testing"

	agentshimcorev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

type queryTerminationTestError struct {
	err    error
	detail *frontendv1.QueryTerminationFailure
}

type exactResumeTestError struct {
	detail *frontendv1.SessionResumeFailure
}

func (e *exactResumeTestError) Error() string { return "exact resume identity mismatch" }
func (e *exactResumeTestError) SessionResumeFailureDetail() *frontendv1.SessionResumeFailure {
	return e.detail
}

func (e *queryTerminationTestError) Error() string { return e.err.Error() }
func (e *queryTerminationTestError) Unwrap() error { return e.err }
func (e *queryTerminationTestError) QueryTerminationFailureDetail() *frontendv1.QueryTerminationFailure {
	return e.detail
}

func TestResumeEstablishmentAuthorityClassifiesCreateAndAutomaticRestore(t *testing.T) {
	missing := &ResumeTranscriptMissingError{
		ResumeID: "claude-uuid", CWD: "/work/ws", ConfigDir: "/cfg",
		ResolvedConfigDir: "/cfg", SearchedPaths: []string{"/cfg/projects/work/claude-uuid.jsonl"},
	}
	tests := []struct {
		name      string
		sessionID string
		automatic bool
	}{
		{name: "create", automatic: false},
		{name: "automatic restore", sessionID: "agent-session", automatic: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			authority := createResumeEstablishment(CreateOpts{}, tc.sessionID)
			if tc.automatic {
				authority = automaticResumeEstablishment(registry.Record{SessionID: tc.sessionID})
			}
			err := authority.classify(missing)
			if !errors.Is(err, missing) {
				t.Fatalf("wrapped error = %v, want ResumeTranscriptMissingError preserved", err)
			}
			failure := errclass.Command(nil, err)
			detail := failure.GetSessionResume()
			if failure.GetErrorType() != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetTranscriptUnavailable() == nil {
				t.Fatalf("classified failure = %v", failure)
			}
			if got := detail.GetAgentReplSessionId(); got != tc.sessionID {
				t.Fatalf("agent_repl_session_id = %q, want %q", got, tc.sessionID)
			}
			if tc.automatic != (detail.GetAutomaticRestore() != nil) || (!tc.automatic) != (detail.GetCreate() != nil) {
				t.Fatalf("attempt = %T, automatic=%t", detail.GetAttempt(), tc.automatic)
			}
		})
	}
}

func TestAutomaticResumeEstablishmentPreservesTypedTerminationAndGenericCause(t *testing.T) {
	rec := registry.Record{
		SessionID: "agent-session", ClaudeSessionID: "claude-session", CWD: "/work/ws", ConfigDir: "/cfg",
	}
	tests := []struct {
		name  string
		err   error
		check func(*testing.T, *frontendv1.SessionResumeFailure)
	}{
		{
			name: "typed query startup failure",
			err: &queryTerminationTestError{
				err: errors.New("query startup failed"),
				detail: &frontendv1.QueryTerminationFailure{
					AgentReplSessionId: "agent-session", QueryInstanceId: "query-1", VendorIdentity: &frontendv1.QueryTerminationFailure_VendorSessionId{VendorSessionId: "claude-session"},
					Reason: &frontendv1.QueryTerminationFailure_StartupFailure{StartupFailure: &agentshimcorev1.QueryStartupFailure{Cause: "resume rejected"}},
				},
			},
			check: func(t *testing.T, detail *frontendv1.SessionResumeFailure) {
				t.Helper()
				if got := detail.GetQueryTermination(); got == nil || got.GetQueryInstanceId() != "query-1" || got.GetVendorSessionId() != "claude-session" || got.GetStartupFailure().GetCause() != "resume rejected" {
					t.Fatalf("query termination = %v", got)
				}
			},
		},
		{
			name: "generic driveability failure",
			err:  errors.New("shim connection died before readiness"),
			check: func(t *testing.T, detail *frontendv1.SessionResumeFailure) {
				t.Helper()
				if got := detail.GetBringUpFailure(); got == nil || got.GetCause() != "shim connection died before readiness" {
					t.Fatalf("bring-up failure = %v", got)
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := automaticResumeEstablishment(rec).classify(tc.err)
			if !errors.Is(got, tc.err) {
				t.Fatalf("wrapped error = %v, want original chain", got)
			}
			failure := errclass.Command(nil, got)
			detail := failure.GetSessionResume()
			if failure.GetErrorType() != string(errclass.TypeSessionResumeFailed) || detail == nil || detail.GetAutomaticRestore() == nil || detail.GetAgentReplSessionId() != rec.SessionID || detail.GetClaudeSessionId() != rec.ClaudeSessionID || detail.GetCwd() != rec.CWD || detail.GetConfigDir() != rec.ConfigDir || detail.GetResolvedConfigDir() == "" {
				t.Fatalf("classified resume failure = %v", failure)
			}
			tc.check(t, detail)
		})
	}
}

func TestResumeEstablishmentAuthorityLeavesFreshErrorsUnchanged(t *testing.T) {
	cause := errors.New("fresh session startup failed")
	got := automaticResumeEstablishment(registry.Record{SessionID: "agent-session", CWD: "/work/ws"}).classify(cause)
	if got != cause {
		t.Fatalf("fresh error = %v, want original error without session.resume wrapping", got)
	}
}

func TestResumeEstablishmentAuthorityPreservesOwnerTypedIdentityMismatch(t *testing.T) {
	cause := &exactResumeTestError{detail: &frontendv1.SessionResumeFailure{
		AgentReplSessionId: "agent-session",
		ClaudeSessionId:    "requested-vendor-session",
		Attempt: &frontendv1.SessionResumeFailure_AutomaticRestore{
			AutomaticRestore: &frontendv1.SessionResumeFailureAutomaticRestore{},
		},
		Cause: &frontendv1.SessionResumeFailure_IdentityMismatch{
			IdentityMismatch: &frontendv1.SessionResumeFailureIdentityMismatch{
				ReplacementClaudeSessionId: "replacement-vendor-session",
			},
		},
	}}
	got := automaticResumeEstablishment(registry.Record{
		SessionID: "agent-session", ClaudeSessionID: "requested-vendor-session", CWD: "/work/ws",
	}).classify(fmt.Errorf("readiness failed: %w", cause))
	if !errors.Is(got, cause) {
		t.Fatalf("classified error = %v, want exact mismatch in chain", got)
	}
	detail := errclass.Command(nil, got).GetSessionResume()
	if detail.GetIdentityMismatch().GetReplacementClaudeSessionId() != "replacement-vendor-session" || detail.GetClaudeSessionId() != "requested-vendor-session" {
		t.Fatalf("classified detail = %v", detail)
	}
}
