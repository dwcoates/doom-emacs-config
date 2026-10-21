package runtimemetrics

import (
	"strings"
	"testing"

	"agent-repld/internal/deployedversion"
	"agent-repld/internal/dlog"
	"agent-repld/internal/semver"
)

func TestTheSurfaceServesTheDeployedVersion(t *testing.T) {
	release := semver.Version{Minor: 129}
	tests := []struct {
		name    string
		version *deployedversion.Version
		want    string
	}{
		{name: "a landing is served bare", version: &deployedversion.Version{Release: release}, want: `agentrepl_deployed_version_info{version="0.129.0"} 1`},
		{name: "a build past the landing carries its commit", version: &deployedversion.Version{Release: release, Past: "0123456789ab"}, want: `agentrepl_deployed_version_info{version="0.129.0+0123456789ab"} 1`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, err := Open(dlog.NewTestLogger(), t.TempDir())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			t.Cleanup(s.Close)

			// Act
			s.DeployedVersion(tt.version)

			// Assert
			if body := scrape(t, s); !strings.Contains(body, tt.want) {
				t.Fatalf("scrape lacks %s:\n%s", tt.want, body)
			}
		})
	}
}

func TestAWithdrawnVersionLeavesNoSeries(t *testing.T) {
	// Arrange
	s, err := Open(dlog.NewTestLogger(), t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	s.DeployedVersion(&deployedversion.Version{Release: semver.Version{Minor: 129}})

	// Act
	s.DeployedVersion(nil)

	// Assert
	if body := scrape(t, s); strings.Contains(body, DeployedVersionName+"{") {
		t.Fatalf("a withdrawn version is still served:\n%s", body)
	}
}

func TestTheOffSurfaceStatesNoVersion(t *testing.T) {
	// Arrange
	var s *Surface

	// Act / Assert: the off surface takes the version without a surface.
	s.DeployedVersion(&deployedversion.Version{Release: semver.Version{Minor: 129}})
	s.DeployedVersion(nil)
}
