package pullrefs

import (
	"reflect"
	"strings"
	"testing"
)

var gns613 = Ref{Target: Target{Owner: "ChessCom", Repo: "gns-server"}, Number: "613"}

var doom613 = Ref{Target: Target{Owner: "dodge", Repo: "doom"}, Number: "613"}

func TestRefs(t *testing.T) {
	cases := []struct {
		name     string
		in       string
		arriving bool
		want     []Ref
	}{
		{name: "a bare address", in: "see https://github.com/ChessCom/gns-server/pull/613 now", want: []Ref{gns613}},
		{name: "an address in a link", in: "[#613](https://github.com/ChessCom/gns-server/pull/613)", want: []Ref{gns613}},
		{name: "an address in a code span", in: "`https://github.com/ChessCom/gns-server/pull/613`", want: []Ref{gns613}},
		{name: "a www address", in: "https://www.github.com/ChessCom/gns-server/pull/613", want: []Ref{gns613}},
		{name: "an address with a trailing path", in: "https://github.com/ChessCom/gns-server/pull/613/files", want: []Ref{gns613}},
		{name: "every address in order", in: "https://github.com/dodge/doom/pull/613 https://github.com/ChessCom/gns-server/pull/613",
			want: []Ref{doom613, gns613}},
		{name: "an issue is not a pull request", in: "https://github.com/ChessCom/gns-server/issues/613"},
		{name: "a leading zero is not a number", in: "https://github.com/ChessCom/gns-server/pull/0613"},
		{name: "a number glued to a word is not one", in: "https://github.com/ChessCom/gns-server/pull/613abc"},
		{name: "a dot-only repository is not one", in: "https://github.com/ChessCom/../pull/613"},
		{name: "another host is not GitHub", in: "https://gitlab.com/ChessCom/gns-server/pull/613"},
		{name: "a settled text reads its last address", in: "https://github.com/ChessCom/gns-server/pull/613", want: []Ref{gns613}},
		{name: "an arriving text leaves its last address", in: "https://github.com/ChessCom/gns-server/pull/61", arriving: true},
		{name: "an arriving text reads an earlier address", in: "https://github.com/ChessCom/gns-server/pull/613 and", arriving: true,
			want: []Ref{gns613}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Refs(tc.in, tc.arriving)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Refs(%q) = %+v, want %+v", tc.in, got, tc.want)
			}
		})
	}
}

func TestRefURL(t *testing.T) {
	// Act
	got := gns613.URL()

	// Assert
	if want := "https://github.com/ChessCom/gns-server/pull/613"; got != want {
		t.Fatalf("URL = %q, want %q", got, want)
	}
}

func TestKnownLearn(t *testing.T) {
	cases := []struct {
		name    string
		learned []Ref
		learn   Ref
		wantNew bool
	}{
		{name: "a new pull request is new", learned: []Ref{doom613}, learn: gns613, wantNew: true},
		{name: "a known pull request is not new", learned: []Ref{gns613}, learn: gns613},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			k := NewKnown(tc.learned)

			// Act
			got := k.Learn(tc.learn)

			// Assert
			if got != tc.wantNew {
				t.Fatalf("Learn = %v, want %v", got, tc.wantNew)
			}
		})
	}
}

func TestKnownForNumber(t *testing.T) {
	cases := []struct {
		name   string
		known  *Known
		number string
		want   Ref
		wantOK bool
	}{
		{name: "the one pull request so numbered", known: NewKnown([]Ref{gns613}), number: "613", want: gns613, wantOK: true},
		{name: "the latest learned of two", known: NewKnown([]Ref{doom613, gns613}), number: "613", want: gns613, wantOK: true},
		{name: "a relearned pull request keeps its first place", known: NewKnown([]Ref{doom613, gns613, doom613}), number: "613", want: gns613, wantOK: true},
		{name: "no pull request so numbered", known: NewKnown([]Ref{gns613}), number: "7"},
		{name: "a nil Known knows nothing", number: "613"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := tc.known.ForNumber(tc.number)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("ForNumber(%q) = %+v, %v, want %+v, %v", tc.number, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestKnownOwnerOf(t *testing.T) {
	cases := []struct {
		name   string
		known  *Known
		repo   string
		want   string
		wantOK bool
	}{
		{name: "a known repository's owner", known: NewKnown([]Ref{gns613}), repo: "gns-server", want: "ChessCom", wantOK: true},
		{name: "a repository named in another case", known: NewKnown([]Ref{gns613}), repo: "GNS-Server", want: "ChessCom", wantOK: true},
		{name: "the latest learned owner", known: NewKnown([]Ref{gns613, {Target: Target{Owner: "fork", Repo: "gns-server"}, Number: "1"}}),
			repo: "gns-server", want: "fork", wantOK: true},
		{name: "an unknown repository", known: NewKnown([]Ref{gns613}), repo: "doom"},
		{name: "a nil Known knows nothing", repo: "gns-server"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := tc.known.OwnerOf(tc.repo)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("OwnerOf(%q) = %q, %v, want %q, %v", tc.repo, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestLinkifyResolvesThroughKnownPullRequests(t *testing.T) {
	home := Target{Owner: "dodge", Repo: "doom"}
	cases := []struct {
		name  string
		links Links
		in    string
		want  string
	}{
		{name: "a known number links to its repository", links: Links{Home: &home, Known: NewKnown([]Ref{gns613})},
			in: "#613", want: "[#613](https://github.com/ChessCom/gns-server/pull/613)"},
		{name: "an unknown number links into the home repository", links: Links{Home: &home, Known: NewKnown([]Ref{gns613})},
			in: "#7", want: "[#7](https://github.com/dodge/doom/pull/7)"},
		{name: "a known number links with no home repository", links: Links{Known: NewKnown([]Ref{gns613})},
			in: "#613", want: "[#613](https://github.com/ChessCom/gns-server/pull/613)"},
		{name: "an unknown number with no home repository is left alone", links: Links{Known: NewKnown([]Ref{gns613})},
			in: "#7", want: "#7"},
		{name: "a known repository links under its owner", links: Links{Home: &home, Known: NewKnown([]Ref{gns613})},
			in: "gns-server#9", want: "[gns-server#9](https://github.com/ChessCom/gns-server/pull/9)"},
		{name: "a known repository links with no home repository", links: Links{Known: NewKnown([]Ref{gns613})},
			in: "gns-server#9", want: "[gns-server#9](https://github.com/ChessCom/gns-server/pull/9)"},
		{name: "an unknown repository with no home repository is left alone", links: Links{Known: NewKnown([]Ref{gns613})},
			in: "doom#9", want: "doom#9"},
		{name: "nothing links nowhere", in: "#613 and gns-server#9", want: "#613 and gns-server#9"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, _ := Linkify(tc.in, tc.links, false)

			// Assert
			if got != tc.want {
				t.Fatalf("Linkify(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestLinkifyAnswersTheRepoReferencesItLinked(t *testing.T) {
	cases := []struct {
		name  string
		links Links
		in    string
		want  []Ref
	}{
		{name: "a linked repo reference is named", links: Links{Home: &work}, in: "gns-server#613 and #12", want: []Ref{gns613}},
		{name: "a bare reference is not named", links: Links{Home: &work}, in: "#12"},
		{name: "a repo reference left alone is not named", in: "gns-server#613"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, got := Linkify(tc.in, tc.links, false)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Linkify(%q) named %+v, want %+v", tc.in, got, tc.want)
			}
		})
	}
}

func TestRefsScansThePullPathPattern(t *testing.T) {
	// Assert: the address Refs scans for is the shape prwatch.ParseURL reads.
	if !strings.Contains(pullURL.String(), PullPathPattern) {
		t.Fatalf("pullURL = %q, want it built on PullPathPattern", pullURL.String())
	}
}

func TestParseURL(t *testing.T) {
	type parsed struct {
		owner, repo string
		number      uint32
		ok          bool
	}
	pr := parsed{owner: "ChessCom", repo: "explanation-engine", number: 8047, ok: true}
	tests := []struct {
		name string
		url  string
		want parsed
	}{
		{name: "the pull request", url: "https://github.com/ChessCom/explanation-engine/pull/8047", want: pr},
		{name: "one of its tabs", url: "https://github.com/ChessCom/explanation-engine/pull/8047/files", want: pr},
		{name: "a comment's anchor", url: "https://github.com/ChessCom/explanation-engine/pull/8047#issuecomment-1", want: pr},
		{name: "surrounding whitespace", url: "  https://github.com/ChessCom/explanation-engine/pull/8047\n", want: pr},
		{name: "an http address", url: "http://GitHub.com/ChessCom/explanation-engine/pull/8047", want: pr},
		{name: "an issue", url: "https://github.com/ChessCom/explanation-engine/issues/8047"},
		{name: "another host", url: "https://gitlab.com/ChessCom/explanation-engine/pull/8047"},
		{name: "another scheme", url: "ftp://github.com/ChessCom/explanation-engine/pull/8047"},
		{name: "a number past 32 bits", url: "https://github.com/ChessCom/explanation-engine/pull/4294967296"},
		{name: "not a url", url: "#8047"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			owner, repo, number, ok := ParseURL(tt.url)

			// Assert
			if got := (parsed{owner, repo, number, ok}); got != tt.want {
				t.Fatalf("ParseURL(%q) = %+v, want %+v", tt.url, got, tt.want)
			}
		})
	}
}

func TestPullPathIsThePullPathPattern(t *testing.T) {
	// Assert: the path ParseURL reads is the shape Refs scans text for.
	if !strings.Contains(pullPath.String(), PullPathPattern) {
		t.Fatalf("pullPath = %q, want it built on PullPathPattern", pullPath.String())
	}
}
