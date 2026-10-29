package pullrefs

import (
	"strings"
	"testing"

	"agent-repld/internal/usersetup"
)

var work = Target{Owner: "ChessCom", Repo: "explanation-engine"}

const pull12 = "[#12](https://github.com/ChessCom/explanation-engine/pull/12)"

const gnsPull = "[gns-server#3617](https://github.com/ChessCom/gns-server/pull/3617)"

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
		{name: "a reference in link text with a parenthesis is left alone",
			in:   "[Run Torch Human requests (#8045)](https://github.com/ChessCom/explanation-engine/pull/8045)",
			want: "[Run Torch Human requests (#8045)](https://github.com/ChessCom/explanation-engine/pull/8045)"},
		{name: "a spaced reference in link text is left alone", in: "[see #12 here](https://x.dev) #12", want: "[see #12 here](https://x.dev) " + pull12},
		{name: "a repo reference in link text is left alone", in: "[gns-server#3617](https://x.dev)", want: "[gns-server#3617](https://x.dev)"},
		{name: "a repo reference in a link destination is left alone", in: "[a](https://x.dev/gns-server#3617)", want: "[a](https://x.dev/gns-server#3617)"},
		{name: "a link with a code span holding a bracket stays whole", in: "[a `]` #12](u) #12", want: "[a `]` #12](u) " + pull12},
		{name: "nested brackets in link text stay whole", in: "[a [b] #12](u) #12", want: "[a [b] #12](u) " + pull12},
		{name: "a bracket that opens no link leaves its reference linked", in: "[see #12]", want: "[see " + pull12 + "]"},
		{name: "a destination still arriving stays whole", in: "[a](https://x.dev/#12", want: "[a](https://x.dev/#12"},
		{name: "a reference after a link is linked", in: "[a](u) (#12)", want: "[a](u) (" + pull12 + ")"},
		{name: "a repo reference is linked", in: "see gns-server#3617 now", want: "see " + gnsPull + " now"},
		{name: "a repo reference opening the text is linked", in: "gns-server#3617", want: gnsPull},
		{name: "a parenthesized repo reference is linked", in: "fixed (gns-server#3617).", want: "fixed (" + gnsPull + ")."},
		{name: "a repo name with dots and underscores is linked", in: "my_repo.js#5", want: "[my_repo.js#5](https://github.com/ChessCom/my_repo.js/pull/5)"},
		{name: "a repo reference in a code span is left alone", in: "`gns-server#3617` x", want: "`gns-server#3617` x"},
		{name: "a repo reference in a fence is left alone", in: "```\ngns-server#3617\n```", want: "```\ngns-server#3617\n```"},
		{name: "a repo reference glued to a url is left alone", in: "https://x.dev/gns-server#3617", want: "https://x.dev/gns-server#3617"},
		{name: "an owner-qualified reference is left alone", in: "ChessCom/gns-server#3617", want: "ChessCom/gns-server#3617"},
		{name: "a repo reference with a leading zero is left alone", in: "gns-server#012", want: "gns-server#012"},
		{name: "a repo reference followed by a letter is left alone", in: "gns-server#12a", want: "gns-server#12a"},
		{name: "a repo name without a number is left alone", in: "gns-server# and gns-server", want: "gns-server# and gns-server"},
		{name: "a dot-only name is not a repository", in: "..#12", want: "..#12"},
		{name: "an arriving text leaves its last repo reference", in: "gns-server#36", arriving: true, want: "gns-server#36"},
		{name: "an arriving text links an earlier repo reference", in: "gns-server#3617 and a#3", arriving: true, want: gnsPull + " and a#3"},
		{name: "a settled text links its last repo reference", in: "gns-server#3617", want: gnsPull},
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

func TestLinkifyRepoReferenceUsesTheTargetsOwner(t *testing.T) {
	// Arrange
	target := Target{Owner: "someone", Repo: "doom"}

	// Act
	got := Linkify("see gns-server#7 and #8", target, false)

	// Assert
	want := "see [gns-server#7](https://github.com/someone/gns-server/pull/7) and [#8](https://github.com/someone/doom/pull/8)"
	if got != want {
		t.Fatalf("Linkify = %q, want %q", got, want)
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
