package prwatch

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/wsm"
)

func TestParseURLNamesAGitHubPullRequest(t *testing.T) {
	want := wsm.PullRequestKey{Owner: "ChessCom", Repo: "explanation-engine", Number: 8047}
	tests := []struct {
		name string
		url  string
		ok   bool
	}{
		{name: "the pull request", url: "https://github.com/ChessCom/explanation-engine/pull/8047", ok: true},
		{name: "one of its tabs", url: "https://github.com/ChessCom/explanation-engine/pull/8047/files", ok: true},
		{name: "a comment's anchor", url: "https://github.com/ChessCom/explanation-engine/pull/8047#issuecomment-1", ok: true},
		{name: "an issue", url: "https://github.com/ChessCom/explanation-engine/issues/8047"},
		{name: "another host", url: "https://gitlab.com/ChessCom/explanation-engine/pull/8047"},
		{name: "not a url", url: "#8047"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := ParseURL(tt.url)

			// Assert
			if ok != tt.ok || (ok && got != want) {
				t.Fatalf("ParseURL(%q) = %v, %v; want ok=%v", tt.url, got, ok, tt.ok)
			}
		})
	}
}

func TestRepositoryOfRemoteReadsEveryGitHubSpelling(t *testing.T) {
	tests := []struct {
		remote      string
		owner, repo string
		ok          bool
	}{
		{remote: "git@github.com:/dwcoates/doom-emacs-config.git", owner: "dwcoates", repo: "doom-emacs-config", ok: true},
		{remote: "git@github.com:ChessCom/explanation-engine.git", owner: "ChessCom", repo: "explanation-engine", ok: true},
		{remote: "https://github.com/ChessCom/explanation-engine", owner: "ChessCom", repo: "explanation-engine", ok: true},
		{remote: "ssh://git@github.com/ChessCom/definitions.git", owner: "ChessCom", repo: "definitions", ok: true},
		{remote: "git@gitlab.com:a/b.git"},
	}
	for _, tt := range tests {
		t.Run(tt.remote, func(t *testing.T) {
			// Act
			owner, repo, ok := RepositoryOfRemote(tt.remote)

			// Assert
			if ok != tt.ok || owner != tt.owner || repo != tt.repo {
				t.Fatalf("RepositoryOfRemote = %q, %q, %v; want %q, %q, %v", owner, repo, ok, tt.owner, tt.repo, tt.ok)
			}
		})
	}
}

type fakeRemotes struct {
	url string
	err error
}

func (f fakeRemotes) RemoteURL(context.Context, string, string) (string, error) { return f.url, f.err }

func TestResolveNamesAPullRequestOrSaysWhyNot(t *testing.T) {
	gitErr := errors.New("no such remote")
	tests := []struct {
		name       string
		text       string
		remotes    fakeRemotes
		want       wsm.PullRequestKey
		unparsable bool
		err        error
	}{
		{name: "a url", text: "https://github.com/o/r/pull/3", want: wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 3}},
		{name: "a number in the repository, by its remote rather than its directory", text: "#4",
			remotes: fakeRemotes{url: "git@github.com:/dwcoates/doom-emacs-config.git"}, want: wsm.PullRequestKey{Owner: "dwcoates", Repo: "doom-emacs-config", Number: 4}},
		{name: "a number in a repository not on github.com", text: "#4", remotes: fakeRemotes{url: "git@gitlab.com:a/b.git"}, unparsable: true},
		{name: "text that names nothing", text: "the PR", unparsable: true},
		{name: "a remote that cannot be read", text: "#4", remotes: fakeRemotes{err: gitErr}, err: gitErr},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := Resolve(context.Background(), tt.remotes, "/repo", tt.text)

			// Assert
			var unparsable *UnparsableError
			switch {
			case tt.unparsable:
				if !errors.As(err, &unparsable) {
					t.Fatalf("Resolve = %v, want unparsable", err)
				}
			case tt.err != nil:
				if !errors.Is(err, tt.err) {
					t.Fatalf("Resolve = %v, want %v", err, tt.err)
				}
			case err != nil || got != tt.want:
				t.Fatalf("Resolve = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}
