package reuse

import (
	"maps"
	"slices"
	"strings"
	"testing"
)

func TestRedactedEnv(t *testing.T) {
	tests := []struct {
		name, value string
		redacted    bool
	}{
		{"GOFLAGS", "-race", false},
		{"GOPRIVATE", "example.com/*", false},
		{"PATH", "/bin", false},
		{"GITHUB_TOKEN", "ghp_x", true},
		{"AWS_SECRET_ACCESS_KEY", "s", true},
		{"API_KEY", "k", true},
		{"GOAUTH", "netrc", true},
		{"DB_PASSWORD", "p", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RedactedEnv(map[string]string{tt.name: tt.value})[tt.name]

			// Assert
			if tt.redacted && (strings.Contains(got, tt.value) || !strings.HasPrefix(got, "<redacted ")) {
				t.Fatalf("RedactedEnv[%s] = %q, want its value redacted", tt.name, got)
			}
			if !tt.redacted && got != tt.value {
				t.Fatalf("RedactedEnv[%s] = %q, want %q", tt.name, got, tt.value)
			}
		})
	}
}

func TestARedactedSecretStillShowsAChange(t *testing.T) {
	// Act
	a := RedactedEnv(map[string]string{"GITHUB_TOKEN": "one"})
	b := RedactedEnv(map[string]string{"GITHUB_TOKEN": "two"})

	// Assert
	if maps.Equal(a, b) {
		t.Fatalf("two secrets redact alike: %v", a)
	}
}

func TestEnvChanges(t *testing.T) {
	tests := []struct {
		name    string
		rec     Record
		now     map[string]string
		changed []string
		want    []EnvChange
	}{
		{"nothing differs", Record{Env: map[string]string{"PATH": "/bin"}}, map[string]string{"PATH": "/bin"}, nil, nil},
		{"a value changed", Record{Env: map[string]string{"GOFLAGS": "-p=1"}}, map[string]string{"GOFLAGS": "-race"}, nil,
			[]EnvChange{{Name: "GOFLAGS", Old: "-p=1", OldSet: true, OldKnown: true, New: "-race", NewSet: true}}},
		{"a variable added", Record{Env: map[string]string{}}, map[string]string{"GOFLAGS": "-race"}, nil,
			[]EnvChange{{Name: "GOFLAGS", OldKnown: true, New: "-race", NewSet: true}}},
		{"a variable removed", Record{Env: map[string]string{"GOFLAGS": "-race"}}, map[string]string{}, nil,
			[]EnvChange{{Name: "GOFLAGS", Old: "-race", OldSet: true, OldKnown: true}}},
		{"several, sorted", Record{Env: map[string]string{"PATH": "/a", "GOFLAGS": "x"}}, map[string]string{"PATH": "/b", "GOFLAGS": "y"}, nil,
			[]EnvChange{
				{Name: "GOFLAGS", Old: "x", OldSet: true, OldKnown: true, New: "y", NewSet: true},
				{Name: "PATH", Old: "/a", OldSet: true, OldKnown: true, New: "/b", NewSet: true},
			}},
		{"a record that predates stored environments", Record{}, map[string]string{"GOENV": "off"},
			[]string{"@env GOENV", "@env GOPRIVATE", "prompts/p.md"},
			[]EnvChange{{Name: "GOENV", New: "off", NewSet: true}, {Name: "GOPRIVATE"}}},
		{"a secret, compared redacted", Record{Env: RedactedEnv(map[string]string{"GITHUB_TOKEN": "one"})}, map[string]string{"GITHUB_TOKEN": "one"}, nil, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := EnvChanges(tt.rec, tt.now, tt.changed)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("EnvChanges = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestEnvChangeString(t *testing.T) {
	tests := []struct {
		name string
		c    EnvChange
		want string
	}{
		{"changed", EnvChange{Name: "GOFLAGS", Old: "-p=1", OldSet: true, OldKnown: true, New: "-race", NewSet: true}, `GOFLAGS was "-p=1", is now "-race"`},
		{"added", EnvChange{Name: "GOFLAGS", OldKnown: true, New: "-race", NewSet: true}, `GOFLAGS was unset, is now "-race"`},
		{"removed", EnvChange{Name: "LANG", Old: "en_US.UTF-8", OldSet: true, OldKnown: true}, `LANG was "en_US.UTF-8", is now unset`},
		{"unknown", EnvChange{Name: "GOPRIVATE"}, `GOPRIVATE was unknown (its record predates stored environments), is now unset`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.c.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String = %q, want %q", got, tt.want)
			}
		})
	}
}
