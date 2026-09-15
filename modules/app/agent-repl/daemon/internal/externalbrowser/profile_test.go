package externalbrowser_test

import (
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/externalbrowser"
)

// twoProfiles is a Chrome Local State document with a personal and a work
// profile on different on-disk directories, the shape the routing reads.
const twoProfiles = `{
  "profile": {
    "info_cache": {
      "Default":   {"user_name": "dodge.w.coates@gmail.com", "gaia_name": "Dodge Personal"},
      "Profile 6": {"user_name": "dodge@chess.com",          "gaia_name": "Dodge Work"}
    }
  }
}`

func TestProfileForEmail(t *testing.T) {
	tests := []struct {
		name        string
		data        string
		email       string
		wantProfile string
		wantMatched bool
	}{
		{
			name:        "personal email routes to its profile",
			data:        twoProfiles,
			email:       "dodge.w.coates@gmail.com",
			wantProfile: "Default",
			wantMatched: true,
		},
		{
			name:        "work email routes to a different profile",
			data:        twoProfiles,
			email:       "dodge@chess.com",
			wantProfile: "Profile 6",
			wantMatched: true,
		},
		{
			name:        "the match is case-insensitive",
			data:        twoProfiles,
			email:       "DODGE@CHESS.COM",
			wantProfile: "Profile 6",
			wantMatched: true,
		},
		{
			name:        "an email-only profile still matches",
			data:        `{"profile":{"info_cache":{"Profile 3":{"email":"only@email.field"}}}}`,
			email:       "only@email.field",
			wantProfile: "Profile 3",
			wantMatched: true,
		},
		{
			name:        "gaia_name is a display name and is not matched on",
			data:        `{"profile":{"info_cache":{"Profile 3":{"gaia_name":"dodge@chess.com"}}}}`,
			email:       "dodge@chess.com",
			wantMatched: false,
		},
		{
			name:        "an unknown email matches nothing",
			data:        twoProfiles,
			email:       "stranger@example.com",
			wantMatched: false,
		},
		{
			name:        "a blank email matches nothing",
			data:        twoProfiles,
			email:       "",
			wantMatched: false,
		},
		{
			name:        "a malformed document matches nothing",
			data:        `{not json`,
			email:       "dodge@chess.com",
			wantMatched: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			profile, matched := externalbrowser.ProfileForEmail([]byte(tc.data), tc.email)

			// Assert.
			if matched != tc.wantMatched {
				t.Fatalf("ProfileForEmail(%q) matched = %v, want %v", tc.email, matched, tc.wantMatched)
			}
			if matched && profile != tc.wantProfile {
				t.Fatalf("ProfileForEmail(%q) = %q, want %q", tc.email, profile, tc.wantProfile)
			}
		})
	}
}

func TestDefaultLocalStatePathNamesChromeUserData(t *testing.T) {
	// Arrange, Act.
	got := externalbrowser.DefaultLocalStatePath()

	// Assert: on a host with a home dir it points at Chrome's own Local State.
	if got == "" {
		t.Skip("no home directory on this host; the empty answer is the fallback case")
	}
	if want := filepath.Join("Google", "Chrome", "Local State"); !strings.Contains(got, want) {
		t.Fatalf("DefaultLocalStatePath() = %q, want it under %q", got, want)
	}
}
