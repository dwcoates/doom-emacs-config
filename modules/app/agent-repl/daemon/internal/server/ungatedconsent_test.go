package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/protocol"
)

// ungatedconsent_test.go — the consent gate is enforced ONCE, and both routes
// to a new session reach it.
//
// A mode that shadows canUseTool in the fail-OPEN direction leaves a session
// with no permission gate at all: the SDK auto-approves every tool before the
// daemon's round-trip engages, so no permission card can ever appear. Creating
// such a session takes a caller who said so. There are two ways to reach
// creation — the createSession command and an open of a workspace that has no
// session — and a refusal that held on only one of them would make the other a
// quieter door to the same thing.

const ungatedMode = string(protocol.PermissionModeBypassPermissions)

// creationHarness is a real *Server (the one creation entry point) plus an
// opener wired to it, so both callers below exercise the SAME gate rather than
// a test double of it.
func creationHarness(t *testing.T) (*harness, *WorkspaceOpener) {
	t.Helper()
	h := newHarness(t)
	o := &WorkspaceOpener{
		Reg:        h.reg,
		Ensurer:    &fakeEnsurer{},
		Creator:    h.srv,
		ConfigDirs: func() []string { return nil },
		Failures:   newFakeOpenFailures(),
		Logf:       func(string, ...any) {},
	}
	return h, o
}

func TestUngatedSessionIsRefusedWithoutConsentOnEveryCreationRoute(t *testing.T) {
	// Arrange — the two callers, each asking for an ungated session without
	// setting the consent flag.
	cases := []struct {
		name   string
		create func(*harness, *WorkspaceOpener) error
	}{
		{"createSession command", func(h *harness, _ *WorkspaceOpener) error {
			_, err := h.srv.CreateSession(context.Background(), CreateOpts{
				CWD: "/w", PermissionMode: ungatedMode, Fake: true,
			})
			return err
		}},
		{"open of a workspace with no session", func(_ *harness, o *WorkspaceOpener) error {
			return o.Open(context.Background(), "/w", WorkspaceOpenOpts{
				PermissionMode: ungatedMode, Fake: true,
			})
		}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			h, o := creationHarness(t)

			// Act.
			err := c.create(h, o)

			// Assert — refused, named, and nothing registered. A session that
			// exists in that posture is the hole the refusal closes, so its
			// absence is the assertion that matters.
			if err == nil {
				t.Fatal("an ungated session was created without consent")
			}
			if !strings.Contains(err.Error(), "NO permission gate") {
				t.Fatalf("err = %v, want it to name the missing gate", err)
			}
			for _, rec := range h.reg.All() {
				if rec.CWD == "/w" {
					t.Fatalf("a refused ungated create still registered %+v", rec)
				}
			}
		})
	}
}

func TestUngatedSessionIsAdmittedWithConsentOnEveryCreationRoute(t *testing.T) {
	// Arrange — the same two callers, this time consenting. The refusal must
	// be a consent gate, not a ban: a caller that said so still gets through.
	cases := []struct {
		name   string
		create func(*harness, *WorkspaceOpener) error
	}{
		{"createSession command", func(h *harness, _ *WorkspaceOpener) error {
			_, err := h.srv.CreateSession(context.Background(), CreateOpts{
				CWD: "/w", PermissionMode: ungatedMode, AllowUngated: true, Fake: true,
			})
			return err
		}},
		{"open of a workspace with no session", func(_ *harness, o *WorkspaceOpener) error {
			return o.Open(context.Background(), "/w", WorkspaceOpenOpts{
				PermissionMode: ungatedMode, AllowUngated: true, Fake: true,
			})
		}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			h, o := creationHarness(t)

			// Act.
			err := c.create(h, o)

			// Assert — admitted past the consent gate. A bring-up failure is a
			// different question and does not bear on it; what must not happen
			// is the consent refusal.
			if err != nil && strings.Contains(err.Error(), "NO permission gate") {
				t.Fatalf("a consented ungated create was still refused: %v", err)
			}
			var found bool
			for _, rec := range h.reg.All() {
				if rec.CWD == "/w" && rec.PermissionMode == ungatedMode {
					found = true
				}
			}
			if !found {
				t.Fatal("a consented ungated create registered no session in that mode")
			}
		})
	}
}

func TestOpenRejectsAnInvalidPermissionModeThroughTheSameValidation(t *testing.T) {
	// Arrange — the open path must not accept a posture the create path
	// rejects, or the two would disagree about what a valid mode is.
	_, o := creationHarness(t)

	// Act.
	err := o.Open(context.Background(), "/w", WorkspaceOpenOpts{PermissionMode: "notAMode"})

	// Assert.
	var invalid *InvalidCreateError
	if !errors.As(err, &invalid) {
		t.Fatalf("err = %v (%T), want an *InvalidCreateError", err, err)
	}
}
