package persistentwifi

import (
	"context"
	"strings"
	"testing"

	"agent-repld/internal/usersetup"
)

func TestPmsetAllRunsTheGrantedInvocation(t *testing.T) {
	cases := []struct {
		name    string
		answer  answer
		wantErr string
	}{
		{name: "a granted write succeeds", answer: answer{}},
		{name: "a refused write names the invocation", answer: answer{out: "sudo: a password is required", code: 1},
			wantErr: "pmset -a womp 1: "},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.r.on(argPower("womp", "1"), tc.answer)

			// Act.
			err := h.c.pmsetAll(context.Background(), "womp", "1")

			// Assert.
			if got := h.r.ran(); len(got) != 1 || got[0] != argPower("womp", "1") {
				t.Fatalf("calls = %v, want exactly %q", got, argPower("womp", "1"))
			}
			switch {
			case tc.wantErr == "" && err != nil:
				t.Fatalf("pmsetAll() error = %v, want nil", err)
			case tc.wantErr != "" && (err == nil || !strings.HasPrefix(err.Error(), tc.wantErr)):
				t.Fatalf("pmsetAll() error = %v, want one starting %q", err, tc.wantErr)
			}
		})
	}
}

// TestApplyPowerWritesEverySettingThroughPmsetAll pins that the power step's
// writes are each the one granted shape, so a hand-rolled write that drifts
// from the sudoers grant fails here.
func TestApplyPowerWritesEverySettingThroughPmsetAll(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	scriptPower(h.r, "1")

	// Act.
	err := h.c.applyPower(context.Background(), true)

	// Assert.
	if err != nil {
		t.Fatalf("applyPower() error = %v", err)
	}
	var want []string
	for _, s := range powerSettings {
		want = append(want, argPower(s, "1"))
	}
	if got := h.r.ran(); strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Fatalf("calls = %v, want %v", got, want)
	}
}

func TestErrNoHotspotIsAUserSetupErrorNamingTheVariable(t *testing.T) {
	// Act.
	err := errNoHotspot()

	// Assert.
	if !strings.Contains(err.Error(), EnvHotspot) || !strings.Contains(err.Error(), usersetup.Doc) {
		t.Fatalf("errNoHotspot() = %v, want a user-setup error naming %s", err, EnvHotspot)
	}
}
