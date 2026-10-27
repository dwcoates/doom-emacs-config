package pullrefs

import (
	"strings"
	"testing"

	"agent-repld/internal/usersetup"
)

var work = Target{Owner: "ChessCom", Repo: "explanation-engine"}

const pull12 = "[#12](https://github.com/ChessCom/explanation-engine/pull/12)"

func TestLinkify(t *testing.T) {
	cases := []struct {
		name     string
		in       string
		arriving bool
		want     string
	}{
		{name: "a bare reference is linked", in: "see #12 now", want: "see " + pull12 + " now"},
		{name: "a reference opening the text is linked", in: "#12 landed", want: pull12 + " landed"},
		{name: "a parenthesized reference is linked", in: "fixed (#12).", want: "fixed (" + pull12 + ")."},
		{name: "a reference on a later line is linked", in: "a\n- #12\n", want: "a\n- " + pull12 + "\n"},
		{name: "a code span is left alone", in: "run `gh pr view #12` here", want: "run `gh pr view #12` here"},
		{name: "a double-backtick span is left alone", in: "``a ` #12`` and #12", want: "``a ` #12`` and " + pull12},
		{name: "an unclosed backtick is literal", in: "a ` #12", want: "a ` " + pull12},
		{name: "a fenced block is left alone", in: "```go\nx := #12\n```\n#12", want: "```go\nx := #12\n```\n" + pull12},
		{name: "a tilde fence is left alone", in: "~~~\n#12\n~~~", want: "~~~\n#12\n~~~"},
		{name: "a url fragment is left alone", in: "https://x.dev/page#12", want: "https://x.dev/page#12"},
		{name: "a link destination is left alone", in: "[top](#12)", want: "[top](#12)"},
		{name: "an already written link is left alone", in: "[#12](https://example.com)", want: "[#12](https://example.com)"},
		{name: "an entity is left alone", in: "&#12;", want: "&#12;"},
		{name: "a word glued after is not a reference", in: "#12abc", want: "#12abc"},
		{name: "a heading marker is not a reference", in: "# 12", want: "# 12"},
		{name: "a leading zero is not a reference", in: "#012", want: "#012"},
		{name: "a bare hash is not a reference", in: "# and #", want: "# and #"},
		{name: "a settled text links its last reference", in: "see #12", want: "see " + pull12},
		{name: "an arriving text leaves its last reference", in: "see #12", arriving: true, want: "see #12"},
		{name: "an arriving text links an earlier reference", in: "#12 and #3", arriving: true, want: pull12 + " and #3"},
		{name: "an arriving text links a reference before its last newline", in: "see #12\n", arriving: true, want: "see " + pull12 + "\n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Linkify(tc.in, work, tc.arriving)

			// Assert
			if got != tc.want {
				t.Fatalf("Linkify(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestOwnersFromEnv(t *testing.T) {
	cases := []struct {
		name    string
		env     string
		want    Owners
		wantErr string
	}{
		{name: "the work root is ChessCom with nothing set", want: Owners{"/work/root": "ChessCom"}},
		{name: "a pair adds an account", env: "/work/personal=someone",
			want: Owners{"/work/root": "ChessCom", "/work/personal": "someone"}},
		{name: "a home-relative pair expands", env: " ~/.claude = some-one ",
			want: Owners{"/work/root": "ChessCom", "/work/home/.claude": "some-one"}},
		{name: "a pair naming the work root replaces its owner", env: "/work/root/=Other",
			want: Owners{"/work/root": "Other"}},
		{name: "a pair without an owner fails", env: "/work/personal", wantErr: "is not `<config-dir>=<owner>`"},
		{name: "a pair with a bad owner fails", env: "/work/personal=no/pe", wantErr: "is not a GitHub login"},
		{name: "a relative dir fails", env: "personal=someone", wantErr: "is not an absolute path"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getenv := func(name string) string {
				if name == EnvOwners {
					return tc.env
				}
				return ""
			}

			// Act
			got, err := OwnersFromEnv(getenv, "/work/home", "/work/root")

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) || !strings.Contains(err.Error(), usersetup.Doc) {
					t.Fatalf("err = %v, want one containing %q and pointing at the setup doc", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("OwnersFromEnv: %v", err)
			}
			if len(got) != len(tc.want) {
				t.Fatalf("owners = %v, want %v", got, tc.want)
			}
			for dir, owner := range tc.want {
				if got[dir] != owner {
					t.Fatalf("owners = %v, want %v", got, tc.want)
				}
			}
		})
	}
}

func TestOwnersFromEnvRequiresTheWorkRoot(t *testing.T) {
	// Act
	_, err := OwnersFromEnv(func(string) string { return "" }, "/work/home", "")

	// Assert
	if err == nil {
		t.Fatal("OwnersFromEnv with no work root succeeded")
	}
}

func TestTargetFor(t *testing.T) {
	owners := Owners{"/work/root": "ChessCom"}
	cases := []struct {
		name      string
		configDir string
		want      Target
		wantOK    bool
	}{
		{name: "a configured root targets its owner and the repository's name", configDir: "/work/root/",
			want: Target{Owner: "ChessCom", Repo: "explanation-engine"}, wantOK: true},
		{name: "an unconfigured root has no target", configDir: "/work/other"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := owners.TargetFor(tc.configDir, "/work/repos/explanation-engine/")

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("TargetFor = %v, %v, want %v, %v", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestPullURL(t *testing.T) {
	// Act
	got := work.PullURL("7")

	// Assert
	if want := "https://github.com/ChessCom/explanation-engine/pull/7"; got != want {
		t.Fatalf("PullURL = %q, want %q", got, want)
	}
}
