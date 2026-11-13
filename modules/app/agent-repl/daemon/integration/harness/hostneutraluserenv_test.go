package harness

import (
	"testing"

	"agent-repld/internal/userenv"
)

func TestHostNeutralUserEnvEnvClearsWhatEmacsStates(t *testing.T) {
	tests := []struct {
		name, key string
	}{
		{name: "the env file", key: userenv.EnvFile},
		{name: "the settings Emacs states itself", key: userenv.EmacsStated},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			env := HostNeutralUserEnvEnv()

			// Assert
			if !envHas(env, tt.key+"=") {
				t.Fatalf("HostNeutralUserEnvEnv() = %v, want %s stated empty", env, tt.key)
			}
		})
	}
}

func TestStartDaemonAppliesHostNeutralUserEnvEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the cleared env file after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralUserEnvEnv")
}
