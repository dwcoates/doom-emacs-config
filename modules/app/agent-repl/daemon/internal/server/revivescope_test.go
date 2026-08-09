package server

import (
	"context"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/sessioncontroller"
)

// THE SCOPE IS THE USER'S DECISION, CARRIED. A compact-first revival now says
// how much of the conversation its compaction may summarize away, and this
// decode is the only place the wire's answer becomes the controller's mode — so
// a scope that decoded to the wrong mode would silently compact something the
// user asked to keep.

// fakeHibernator records the revival mode the handler resolved, which is the
// one fact this decode is responsible for.
type fakeHibernator struct {
	modes []sessioncontroller.ReviveMode
	err   error
}

func (f *fakeHibernator) HibernateWorkspace(string) error { return nil }

func (f *fakeHibernator) ReviveSession(_ context.Context, _ string, mode sessioncontroller.ReviveMode) error {
	f.modes = append(f.modes, mode)
	return f.err
}

// newRevivalHandler is a command handler wired to nothing but the hibernator,
// which is all the revival command touches.
func newRevivalHandler(t *testing.T) (*commandHandler, *fakeHibernator) {
	t.Helper()
	hib := &fakeHibernator{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Hibernations: hib})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h, hib
}

// compactFirstCmd is a ReviveSessionCmd choosing compact_first at scope.
func compactFirstCmd(scope frontendv1.CompactionScope) *frontendv1.ReviveSessionCmd {
	return &frontendv1.ReviveSessionCmd{
		Mode: &frontendv1.ReviveSessionCmd_CompactFirst{
			CompactFirst: &frontendv1.ReviveCompactFirst{Scope: scope},
		},
	}
}

// EVERY SCOPE REACHES THE CONTROLLER AS ITS OWN MODE.
func TestReviveSessionCarriesEachCompactionScopeToItsMode(t *testing.T) {
	tests := []struct {
		name  string
		scope frontendv1.CompactionScope
		want  sessioncontroller.ReviveMode
	}{
		{"all", frontendv1.CompactionScope_COMPACTION_SCOPE_ALL, sessioncontroller.ReviveModeCompactAll},
		{"responses", frontendv1.CompactionScope_COMPACTION_SCOPE_RESPONSES, sessioncontroller.ReviveModeCompactResponses},
		{"prompts", frontendv1.CompactionScope_COMPACTION_SCOPE_PROMPTS, sessioncontroller.ReviveModeCompactPrompts},
		{
			"prompts and responses",
			frontendv1.CompactionScope_COMPACTION_SCOPE_PROMPTS_AND_RESPONSES,
			sessioncontroller.ReviveModeCompactPromptsAndResponses,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, hib := newRevivalHandler(t)

			// Act
			err := h.ReviveSession(context.Background(), "/ws", "r-1", compactFirstCmd(tc.scope))

			// Assert
			if err != nil {
				t.Fatalf("ReviveSession(%s) = %v, want it accepted", tc.scope, err)
			}
			if len(hib.modes) != 1 || hib.modes[0] != tc.want {
				t.Fatalf("revival modes = %v, want exactly [%v]", hib.modes, tc.want)
			}
		})
	}
}

// DIRECT IS UNTOUCHED BY THE SCOPE'S ARRIVAL: it has no compaction to aim.
func TestReviveSessionStillCarriesDirect(t *testing.T) {
	// Arrange
	h, hib := newRevivalHandler(t)
	cmd := &frontendv1.ReviveSessionCmd{
		Mode: &frontendv1.ReviveSessionCmd_Direct{Direct: &frontendv1.ReviveDirect{}},
	}

	// Act
	if err := h.ReviveSession(context.Background(), "/ws", "r-1", cmd); err != nil {
		t.Fatalf("ReviveSession(direct) = %v, want it accepted", err)
	}

	// Assert
	if len(hib.modes) != 1 || hib.modes[0] != sessioncontroller.ReviveModeDirect {
		t.Fatalf("revival modes = %v, want exactly [direct]", hib.modes)
	}
}

// CLEAR CARRIES AS ITS OWN MODE, and takes no scope: a scope says what a
// summary keeps, and a clear keeps nothing. An arm that decoded to a compaction
// here would summarize a conversation the user asked to discard.
func TestReviveSessionCarriesClear(t *testing.T) {
	// Arrange
	h, hib := newRevivalHandler(t)
	cmd := &frontendv1.ReviveSessionCmd{
		Mode: &frontendv1.ReviveSessionCmd_Clear{Clear: &frontendv1.ReviveClear{}},
	}

	// Act
	if err := h.ReviveSession(context.Background(), "/ws", "r-1", cmd); err != nil {
		t.Fatalf("ReviveSession(clear) = %v, want it accepted", err)
	}

	// Assert
	if len(hib.modes) != 1 || hib.modes[0] != sessioncontroller.ReviveModeClear {
		t.Fatalf("revival modes = %v, want exactly [clear]", hib.modes)
	}
}

// A COMMAND WITH NO ARM AT ALL IS A NACK. The wire makes "no decision"
// unrepresentable precisely so the daemon never invents one, and a mode that
// fell through the decode would reach the controller as unset.
func TestReviveSessionRefusesACommandWithNoMode(t *testing.T) {
	// Arrange
	h, hib := newRevivalHandler(t)

	// Act
	err := h.ReviveSession(context.Background(), "/ws", "r-1", &frontendv1.ReviveSessionCmd{})

	// Assert
	if err == nil {
		t.Fatal("ReviveSession with no mode arm = nil, want a nack")
	}
	if len(hib.modes) != 0 {
		t.Fatalf("revival modes = %v, want the revival never to have been attempted", hib.modes)
	}
}

// AN UNSPECIFIED SCOPE IS A NACK, NOT A WHOLE-CONVERSATION COMPACTION. The
// daemon has no default for what it may discard, and inventing one would answer
// an unstated request by throwing away the most it possibly could.
func TestReviveSessionRefusesACompactFirstWithNoScope(t *testing.T) {
	// Arrange
	h, hib := newRevivalHandler(t)

	// Act
	err := h.ReviveSession(context.Background(), "/ws", "r-1",
		compactFirstCmd(frontendv1.CompactionScope_COMPACTION_SCOPE_UNSPECIFIED))

	// Assert
	if err == nil {
		t.Fatal("ReviveSession(compact_first, unspecified scope) = nil, want a refusal")
	}
	if !strings.Contains(err.Error(), "scope") {
		t.Fatalf("refusal = %q, want it to name the missing scope", err)
	}
	if len(hib.modes) != 0 {
		t.Fatalf("revival modes = %v, want nothing forwarded on a refused command", hib.modes)
	}
}

// AN UNKNOWN SCOPE — a newer frontend's arm this daemon has never heard of — is
// refused for the same reason, rather than being rounded down to the mode that
// discards the most.
func TestReviveSessionRefusesAnUnrecognizedScope(t *testing.T) {
	// Arrange
	h, hib := newRevivalHandler(t)

	// Act
	err := h.ReviveSession(context.Background(), "/ws", "r-1", compactFirstCmd(frontendv1.CompactionScope(9001)))

	// Assert
	if err == nil {
		t.Fatal("ReviveSession(compact_first, unknown scope) = nil, want a refusal")
	}
	if len(hib.modes) != 0 {
		t.Fatalf("revival modes = %v, want nothing forwarded on a refused command", hib.modes)
	}
}
