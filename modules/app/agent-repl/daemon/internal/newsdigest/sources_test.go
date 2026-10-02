package newsdigest

import (
	"net/url"
	"testing"
)

func TestTheDefaultSourcesAreValid(t *testing.T) {
	// Act
	err := validateSources(DefaultSources)

	// Assert
	if err != nil {
		t.Fatalf("validateSources(DefaultSources) = %v", err)
	}
}

func TestEveryDefaultSourceIsAnAbsoluteHTTPSURL(t *testing.T) {
	for _, src := range DefaultSources {
		t.Run(src.Key, func(t *testing.T) {
			for _, raw := range []string{src.URL, src.Home} {
				// Act
				u, err := url.Parse(raw)

				// Assert
				if err != nil || u.Scheme != "https" || u.Host == "" {
					t.Fatalf("%q is not an absolute https url (%v)", raw, err)
				}
			}
		})
	}
}

func TestTheDefaultSourcesAreTheDesignDocsEleven(t *testing.T) {
	// Act
	n := len(DefaultSources)

	// Assert
	if n != 11 {
		t.Fatalf("len(DefaultSources) = %d, want the 11 sources docs/protobuf-design/news-digest.md names", n)
	}
}

func TestFormatString(t *testing.T) {
	tests := []struct {
		format Format
		want   string
	}{
		{FormatAtom, "atom"},
		{FormatRSS, "rss"},
		{FormatNPM, "npm"},
		{FormatChangelog, "changelog"},
		{FormatPage, "page"},
		{formatUnset, "unset"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.format.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}
