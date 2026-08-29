package envc_test

import (
	"reflect"
	"testing"

	"claude-repld/internal/envc"
)

func TestLoadFake(t *testing.T) {
	tests := []struct {
		name string
		set  string
		want bool
	}{
		{name: "unset", set: "", want: false},
		{name: "one", set: "1", want: true},
		{name: "true", set: "true", want: true},
		{name: "mixed case yes", set: "YeS", want: true},
		{name: "padded on", set: "  on  ", want: true},
		{name: "zero", set: "0", want: false},
		{name: "garbage", set: "maybe", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			t.Setenv(envc.EnvFake, tc.set)

			// Act.
			got := envc.Load().Fake()

			// Assert.
			if got != tc.want {
				t.Fatalf("Fake() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestLoadForbidVendorCalls(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvForbidVendorCalls, "1")

	// Act.
	got := envc.Load().ForbidVendorCalls()

	// Assert.
	if !got {
		t.Fatal("ForbidVendorCalls() = false, want true")
	}
}

func TestLoadStateDir(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvStateDir, "/tmp/state-root")

	// Act.
	got := envc.Load().StateDir()

	// Assert.
	if got != "/tmp/state-root" {
		t.Fatalf("StateDir() = %q, want %q", got, "/tmp/state-root")
	}
}

func TestLoadOwned(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvOwned, "1")

	// Act.
	got := envc.Load().Owned()

	// Assert.
	if !got {
		t.Fatal("Owned() = false, want true")
	}
}

func TestWithFakeOverridesEnvironment(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvFake, "")
	c := envc.Load()

	// Act.
	got := c.WithFake(true).Fake()

	// Assert.
	if !got {
		t.Fatal("WithFake(true).Fake() = false, want true")
	}
}

func TestWithStateDirEmptyKeepsEnvironment(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvStateDir, "/from/env")
	c := envc.Load()

	// Act.
	got := c.WithStateDir("").StateDir()

	// Assert.
	if got != "/from/env" {
		t.Fatalf("StateDir() = %q, want %q", got, "/from/env")
	}
}

func TestWithStateDirOverridesEnvironment(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvStateDir, "/from/env")
	c := envc.Load()

	// Act.
	got := c.WithStateDir("/from/flag").StateDir()

	// Assert.
	if got != "/from/flag" {
		t.Fatalf("StateDir() = %q, want %q", got, "/from/flag")
	}
}

func TestWithFakeDoesNotMutateReceiver(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvFake, "")
	c := envc.Load()

	// Act.
	_ = c.WithFake(true)

	// Assert.
	if c.Fake() {
		t.Fatal("receiver mutated by WithFake")
	}
}

func TestChildEnv(t *testing.T) {
	tests := []struct {
		name              string
		fake              bool
		forbidVendorCalls bool
		stateDir          string
		want              []string
	}{
		{
			name: "bare",
			want: []string{"AGENT_REPL_OWNED=1"},
		},
		{
			name:     "state dir only",
			stateDir: "/s",
			want:     []string{"AGENT_REPL_OWNED=1", "AGENT_REPL_STATE_DIR=/s"},
		},
		{
			name: "fake only",
			fake: true,
			want: []string{"AGENT_REPL_OWNED=1", "AGENT_REPL_FAKE=1"},
		},
		{
			name:              "forbid only",
			forbidVendorCalls: true,
			want:              []string{"AGENT_REPL_OWNED=1", "AGENT_REPL_FORBID_VENDOR_CALLS=1"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			t.Setenv(envc.EnvFake, boolEnv(tc.fake))
			t.Setenv(envc.EnvForbidVendorCalls, boolEnv(tc.forbidVendorCalls))
			t.Setenv(envc.EnvStateDir, tc.stateDir)
			t.Setenv(envc.EnvOwned, "")

			// Act.
			got := envc.Load().ChildEnv()

			// Assert.
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("ChildEnv() = %v, want %v", got, tc.want)
			}
		})
	}
}

func boolEnv(b bool) string {
	if b {
		return "1"
	}
	return ""
}
