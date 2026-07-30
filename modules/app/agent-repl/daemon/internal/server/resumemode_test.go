package server

import (
	"context"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// ResumeMode dispatch — the create path turning a caller's INTENT into the
// conversation it lands on.
//
// The frontend used to send a remembered vendor uuid. It now sends what it
// WANTS, and the daemon decides what that means; these tests pin the seam.
// ---------------------------------------------------------------------------

// createWith runs a create carrying cmd against a handler wired to resumes,
// returning what the session layer was asked to create.
func createWith(t *testing.T, resumes ConversationResumeResolver, cmd *frontendv1.CreateSessionCmd) (*fakeSessionCmds, error) {
	t.Helper()
	sessions := &fakeSessionCmds{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sessions, nil, nil, t.Logf,
		CommandHandlerConfig{Health: HealthConfig{Router: &probeHealthRouter{healthy: true}}, Resumes: resumes})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	_, err = h.CreateSession(context.Background(), "/w", "r1", cmd)
	return sessions, err
}

func TestContinueResolvesTheResumeTargetFromTheDaemon(t *testing.T) {
	// Arrange — the caller names no conversation at all.
	resumes := &fakeResumes{uuid: "uuid-resolved"}

	// Act
	sessions, err := createWith(t, resumes, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg", ResumeMode: frontendv1.ResumeMode_RESUME_MODE_CONTINUE,
	})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession: %v", err)
	}
	if len(sessions.created) != 1 || sessions.created[0].Resume != "uuid-resolved" {
		t.Fatalf("created = %+v, want the daemon-resolved conversation", sessions.created)
	}
}

func TestContinueAsksTheResolverForTheCommandsOwnLocation(t *testing.T) {
	// Arrange — resolution is keyed on (config_dir, cwd), and passing either
	// one wrong would reattach a workspace to somebody else's conversation.
	resumes := &fakeResumes{uuid: "uuid-resolved"}

	// Act
	if _, err := createWith(t, resumes, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg", ResumeMode: frontendv1.ResumeMode_RESUME_MODE_CONTINUE,
	}); err != nil {
		t.Fatalf("CreateSession: %v", err)
	}

	// Assert
	if len(resumes.asked) != 1 || resumes.asked[0] != [2]string{"/cfg", "/w"} {
		t.Fatalf("resolver asked %v, want one lookup for (/cfg, /w)", resumes.asked)
	}
}

func TestAnUnspecifiedModeIsTreatedAsContinue(t *testing.T) {
	// Arrange — the zero value must be the SAFE default. Continuing an
	// existing conversation is recoverable; stranding one is not.
	resumes := &fakeResumes{uuid: "uuid-resolved"}

	// Act
	sessions, err := createWith(t, resumes, &frontendv1.CreateSessionCmd{Cwd: "/w", ConfigDir: "/cfg"})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession: %v", err)
	}
	if len(sessions.created) != 1 || sessions.created[0].Resume != "uuid-resolved" {
		t.Fatalf("created = %+v, want an unspecified mode to continue", sessions.created)
	}
}

func TestContinueStartsFreshWhenNothingResolves(t *testing.T) {
	// Arrange — a brand-new workspace has no conversation to continue.
	resumes := &fakeResumes{}

	// Act
	sessions, err := createWith(t, resumes, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg", ResumeMode: frontendv1.ResumeMode_RESUME_MODE_CONTINUE,
	})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession: %v", err)
	}
	if len(sessions.created) != 1 || sessions.created[0].Resume != "" {
		t.Fatalf("created = %+v, want a fresh conversation", sessions.created)
	}
}

func TestFreshNeverConsultsTheResolver(t *testing.T) {
	// Arrange — the user asked for a blank slate over a workspace that HAS a
	// resumable conversation. Helpfully reattaching would ignore them.
	resumes := &fakeResumes{uuid: "uuid-resolved"}

	// Act
	sessions, err := createWith(t, resumes, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg", ResumeMode: frontendv1.ResumeMode_RESUME_MODE_FRESH,
	})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession: %v", err)
	}
	if len(sessions.created) != 1 || sessions.created[0].Resume != "" {
		t.Fatalf("created = %+v, want no resume for an explicit FRESH", sessions.created)
	}
	if len(resumes.asked) != 0 {
		t.Fatalf("resolver was consulted %v times for a FRESH create, want none", len(resumes.asked))
	}
}

func TestExplicitLandsOnTheNamedConversation(t *testing.T) {
	// Arrange — a human picked this conversation (a picker, a fork).
	resumes := &fakeResumes{uuid: "uuid-resolved"}

	// Act
	sessions, err := createWith(t, resumes, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg",
		ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_EXPLICIT,
		ExplicitClaudeSessionId: "uuid-chosen",
	})

	// Assert — the human's choice outranks the resolver's.
	if err != nil {
		t.Fatalf("CreateSession: %v", err)
	}
	if len(sessions.created) != 1 || sessions.created[0].Resume != "uuid-chosen" {
		t.Fatalf("created = %+v, want the named conversation", sessions.created)
	}
}

func TestExplicitWithoutAUUIDIsRefused(t *testing.T) {
	// Arrange — EXPLICIT with nothing named is a caller bug, and falling back
	// to CONTINUE would land it somewhere it did not ask for.
	_, err := createWith(t, &fakeResumes{}, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg", ResumeMode: frontendv1.ResumeMode_RESUME_MODE_EXPLICIT,
	})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "requires explicit_claude_session_id") {
		t.Fatalf("err = %v, want a refusal naming the missing conversation id", err)
	}
}

func TestAUUIDUnderContinueIsRefused(t *testing.T) {
	// Arrange — a caller that filled in a uuid believes it is steering. This
	// is the old pointer shape trying to come back; ignoring it quietly would
	// land the session somewhere the caller did not choose and say nothing.
	_, err := createWith(t, &fakeResumes{uuid: "uuid-resolved"}, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg",
		ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_CONTINUE,
		ExplicitClaudeSessionId: "uuid-smuggled",
	})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "RESUME_MODE_EXPLICIT") {
		t.Fatalf("err = %v, want a refusal naming the only mode that may carry a uuid", err)
	}
}

func TestAUUIDUnderFreshIsRefused(t *testing.T) {
	// Arrange — FRESH plus a named conversation is self-contradictory.
	_, err := createWith(t, &fakeResumes{}, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg",
		ResumeMode:              frontendv1.ResumeMode_RESUME_MODE_FRESH,
		ExplicitClaudeSessionId: "uuid-smuggled",
	})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "RESUME_MODE_EXPLICIT") {
		t.Fatalf("err = %v, want the contradiction refused", err)
	}
}

func TestAnUnwiredResolverRefusesRatherThanStartingFresh(t *testing.T) {
	// Arrange — quietly starting a NEW conversation on top of an intact one is
	// the data loss this whole mechanism exists to prevent. It must not be
	// reachable by forgetting to wire a dependency.
	_, err := createWith(t, nil, &frontendv1.CreateSessionCmd{
		Cwd: "/w", ConfigDir: "/cfg", ResumeMode: frontendv1.ResumeMode_RESUME_MODE_CONTINUE,
	})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "needs a conversation resolver") {
		t.Fatalf("err = %v, want a loud unwired-resolver refusal", err)
	}
}

func TestTheAckReportsTheConversationTheSessionLandedOn(t *testing.T) {
	// Arrange — the client needs this to attribute its logs before the first
	// pushed SessionView. An empty value there is accepted by the client-log
	// validator; a WRONG one is what nacks.
	resumes := &fakeResumes{observed: "uuid-landed"}
	sessions := &fakeSessionCmds{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sessions, nil, nil, t.Logf,
		CommandHandlerConfig{Health: HealthConfig{Router: &probeHealthRouter{healthy: true}}, Resumes: resumes})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act
	observed, err := h.CreateSession(context.Background(), "/w", "r1",
		&frontendv1.CreateSessionCmd{Cwd: "/w", ConfigDir: "/cfg"})

	// Assert
	if err != nil {
		t.Fatalf("CreateSession: %v", err)
	}
	if observed != "uuid-landed" {
		t.Fatalf("observed = %q, want uuid-landed", observed)
	}
}
