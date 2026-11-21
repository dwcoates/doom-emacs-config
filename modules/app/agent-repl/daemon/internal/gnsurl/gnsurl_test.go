package gnsurl

import "testing"

func TestIsHost(t *testing.T) {
	tests := []struct {
		name string
		host string
		want bool
	}{
		{name: "the deployed GNS host", host: "gns.int.sgns.chess-platform.com", want: true},
		{name: "a host in upper case", host: "GNS.EXAMPLE.COM", want: true},
		{name: "a host naming gns past its first label", host: "knowledge.gns.example.com", want: true},
		{name: "a host with no gns in it", host: "github.com", want: false},
		{name: "an empty host", host: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got := IsHost(tt.host)

			// Assert
			if got != tt.want {
				t.Errorf("IsHost(%q) = %v, want %v", tt.host, got, tt.want)
			}
		})
	}
}

func TestParsePage(t *testing.T) {
	tests := []struct {
		name   string
		raw    string
		want   Page
		wantOK bool
	}{
		{name: "a key page", raw: "https://gns.int.sgns.chess-platform.com/key/teams.core.docs",
			want: Page{Host: "gns.int.sgns.chess-platform.com", Key: "teams.core.docs"}, wantOK: true},
		{name: "a prefix browse", raw: "https://gns.example.com/keys/teams.core", want: Page{Host: "gns.example.com", Key: "teams.core"}, wantOK: true},
		{name: "an app with its view state", raw: "https://gns.example.com/app/teams.core.review.app?s=v%3Dreview#top",
			want: Page{Host: "gns.example.com", Key: "teams.core.review.app"}, wantOK: true},
		{name: "a host in upper case is lower-cased", raw: "http://GNS.Example.com/key/user.dodge", want: Page{Host: "gns.example.com", Key: "user.dodge"}, wantOK: true},
		{name: "a one-segment key", raw: "https://gns.example.com/key/teams", want: Page{Host: "gns.example.com", Key: "teams"}, wantOK: true},
		{name: "a host that is not GNS's", raw: "https://example.com/key/teams.core"},
		{name: "a scheme other than http or https", raw: "ftp://gns.example.com/key/teams.core"},
		{name: "a route that names no key", raw: "https://gns.example.com/agents/regal-cramling"},
		{name: "a route with no key", raw: "https://gns.example.com/key/"},
		{name: "the browser root", raw: "https://gns.example.com/keys"},
		{name: "a key followed by a further path", raw: "https://gns.example.com/key/teams.core/history"},
		{name: "a key with an empty segment", raw: "https://gns.example.com/key/teams..core"},
		{name: "a key ending in a dot", raw: "https://gns.example.com/key/teams.core."},
		{name: "an unparseable url", raw: "http://[::1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := ParsePage(tt.raw)

			// Assert
			if ok != tt.wantOK || got != tt.want {
				t.Errorf("ParsePage(%q) = %+v, %v; want %+v, %v", tt.raw, got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestPageContains(t *testing.T) {
	owned := Page{Host: "gns.example.com", Key: "teams.core"}
	tests := []struct {
		name string
		sub  Page
		want bool
	}{
		{name: "a child key", sub: Page{Host: "gns.example.com", Key: "teams.core.notes"}, want: true},
		{name: "a deeper descendant", sub: Page{Host: "gns.example.com", Key: "teams.core.notes.2026"}, want: true},
		{name: "the page itself", sub: owned, want: false},
		{name: "a sibling sharing the key's prefix", sub: Page{Host: "gns.example.com", Key: "teams.core-chess"}, want: false},
		{name: "the page's parent", sub: Page{Host: "gns.example.com", Key: "teams"}, want: false},
		{name: "a child key on another host", sub: Page{Host: "gns.other.com", Key: "teams.core.notes"}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := owned.Contains(tt.sub)

			// Assert
			if got != tt.want {
				t.Errorf("%+v.Contains(%+v) = %v, want %v", owned, tt.sub, got, tt.want)
			}
		})
	}
}
