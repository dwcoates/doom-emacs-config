package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// bannerRaisedFor reports whether a banner of KIND was raised whose click
// opens workspace WS.
func (h *harness) bannerRaisedFor(kind string, ws ids.WorkspaceID) bool {
	for _, b := range h.banners.seen() {
		if b.kind == kind && b.ws == ws {
			return true
		}
	}
	return false
}

// escalationBriefs is every brief the harness submitted to the escalation
// workspace.
func escalationBriefs(h *harness) []promptqueue.Submission {
	var out []promptqueue.Submission
	for _, sub := range h.queue.submissions {
		if sub.WS == "ws-escalate" {
			out = append(out, sub)
		}
	}
	return out
}

func TestEveryEscalationPutsTheDeployBeforeTheUserInANewWorkspace(t *testing.T) {
	tests := []struct {
		name         string
		act          func(t *testing.T, h *harness)
		wantSummary  string
		wantProblems string
	}{
		{name: "the intake's escalation carries its findings",
			act: func(t *testing.T, h *harness) {
				if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}); err != nil {
					t.Fatalf("DecideMediation: %v", err)
				}
			}, wantSummary: "pages regressed", wantProblems: "1. the reaper errors on every pass"},
		{name: "a turn that decided nothing",
			act:         func(t *testing.T, h *harness) { h.o.Mediate(context.Background()) },
			wantSummary: "the deploy intake ended its turn without deciding the deploy", wantProblems: "states no separate findings"},
		{name: "a remediation closed before it landed",
			act: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-remedy"); err != nil {
					t.Fatal(err)
				}
				// A closed workspace's record stays readable, as the
				// store keeps it, so the escalation names it.
				h.db.mu.Lock()
				h.db.workspaces["ws-remedy"] = wsm.Workspace{ID: "ws-remedy", Name: "remedy-the-reaper"}
				h.db.mu.Unlock()
				h.o.OnWorkspaceClosed(context.Background(), "ws-remedy")
			}, wantSummary: "the deploy remediation remedy-the-reaper was closed before its fix landed", wantProblems: "states no separate findings"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if !strings.HasPrefix(tt.name, "a turn") {
				withIntake(t, h, deployStarted)
			}

			// Act
			tt.act(t, h)

			// Assert
			if owedCheck(t, h) != nil {
				t.Fatal("the escalated check still holds the queue")
			}
			if len(h.checks.made) == 0 || !strings.HasPrefix(h.checks.made[len(h.checks.made)-1], "deploy-escalation-") {
				t.Fatalf("made %v, want an escalation workspace made last", h.checks.made)
			}
			if !slices.Contains(h.checks.efforts, "remediation:ws-escalate:recorded=false") {
				t.Fatalf("efforts %v, want the escalation at the remediation's effort", h.checks.efforts)
			}
			if subs := escalationBriefs(h); len(subs) != 1 {
				t.Fatalf("briefed the escalation %d times, want once", len(subs))
			}
			values := h.briefValues[len(h.briefValues)-1]
			if !strings.Contains(values["summary"], tt.wantSummary) || !strings.Contains(values["problems"], tt.wantProblems) {
				t.Fatalf("brief values %v, want the summary %q and findings %q", values, tt.wantSummary, tt.wantProblems)
			}
			if !h.bannerRaisedFor(desktopnotify.KindDeployMediated, "ws-escalate") {
				t.Fatal("the user was not notified with a click opening the escalation workspace")
			}
		})
	}
}

func TestAnEscalationThatCannotReachTheUserIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "a workspace that cannot be made", message: "the escalation workspace could not be made; the user is not told of the escalated deploy",
			arrange: func(h *harness) { h.checks.escalationErr = errors.New("disk full") }},
		{name: "an effort that cannot be set", message: "the escalation workspace's effort could not be set; it is not briefed and the user is not told",
			arrange: func(h *harness) { h.checks.effortErr = errors.New("no catalog") }},
		{name: "a brief that cannot be composed", message: "the escalation workspace's brief could not be composed; it is not briefed and the user is not told",
			arrange: func(h *harness) { delete(h.briefs, BriefDeployEscalation) }},
		{name: "a brief that cannot be submitted", message: "the escalation workspace's brief could not be submitted; the user is not told",
			arrange: func(h *harness) { h.queue.err = errors.New("queue closed") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			tt.arrange(h)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "yours"})

			// Assert: the queue moves on regardless, and the lost escalation
			// is the record's to tell.
			if err != nil || owedCheck(t, h) != nil {
				t.Fatalf("DecideMediation = %v, want the check released", err)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
			if h.bannerRaised(desktopnotify.KindDeployMediated) {
				t.Fatal("the user was notified of an escalation workspace that was never briefed")
			}
		})
	}
}

func TestAnEscalationWorkspaceIsRecordedAsLandingDirectly(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)

	// Act
	if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}); err != nil {
		t.Fatalf("DecideMediation: %v", err)
	}

	// Assert
	repo, found, err := h.db.DeployEscalationOf(context.Background(), "ws-escalate")
	if err != nil || !found || repo != mediationKey(h) {
		t.Fatalf("DeployEscalationOf = (%q, %v, %v), want the escalation recorded for its queue", repo, found, err)
	}
}

func TestAnEscalationThatCannotBeRecordedIsNotBriefed(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.db.mu.Lock()
	h.db.escalationRecordErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	_ = h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem})

	// Assert
	if subs := escalationBriefs(h); len(subs) != 0 {
		t.Fatalf("briefed the escalation %d times, want none", len(subs))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the escalation workspace could not be recorded as landing directly; it is not briefed and the user is not told"}); !ok {
		t.Fatalf("records = %+v, want the failed record at ERROR", h.logs.Records())
	}
}
