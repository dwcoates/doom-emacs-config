package appfreeze

import (
	"reflect"
	"testing"
)

func TestAppsFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		value   string
		want    []string
		wantErr bool
	}{
		{name: "unset is the default list", value: "", want: DefaultApps},
		{name: "none freezes nothing", value: " none ", want: nil},
		{name: "a list is trimmed in order", value: "Slack, Google Chrome ,Discord", want: []string{"Slack", "Google Chrome", "Discord"}},
		{name: "an empty entry is refused", value: "Slack,,Discord", wantErr: true},
		{name: "a duplicate is refused", value: "Slack,slack", wantErr: true},
		{name: "a path is refused", value: "/Applications/Slack.app", wantErr: true},
		{name: "the editor is refused", value: "Emacs", wantErr: true},
		{name: "a terminal is refused", value: "Slack,iTerm2", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			getenv := func(k string) string {
				if k == EnvApps {
					return tt.value
				}
				return ""
			}

			// Act
			got, err := AppsFromEnv(getenv)

			// Assert
			if tt.wantErr {
				if err == nil {
					t.Fatalf("AppsFromEnv() = %v, want a refusal", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("AppsFromEnv() error = %v", err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("AppsFromEnv() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheDefaultListLeavesDockerAlone(t *testing.T) {
	// Act and assert: the e2e-emacs gate suite needs Docker with the lid closed.
	for _, app := range DefaultApps {
		if app == "Docker" {
			t.Fatal("DefaultApps names Docker")
		}
	}
}
