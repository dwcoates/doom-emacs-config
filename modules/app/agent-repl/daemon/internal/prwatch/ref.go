// Package prwatch follows GitHub pull requests for workspaces: it reads each
// followed pull request's authoritative lists on a schedule, tells each
// subscriber what changed since it was last told, and posts a workspace's
// comments so it is never told its own words back
// (agentrepl/v1/pull_request.proto has the contract and why nothing can be
// lost or told twice).
package prwatch

import (
	"context"
	"fmt"
	"net/url"
	"regexp"
	"strconv"
	"strings"

	"agent-repld/internal/wsm"
)

// pullPath is a github.com pull request's path: /owner/repo/pull/number,
// optionally followed by a tab (/files, /commits) or a fragment.
var pullPath = regexp.MustCompile(`^/([A-Za-z0-9][A-Za-z0-9-]*)/([A-Za-z0-9._-]+)/pull/([1-9][0-9]*)(?:/.*)?$`)

// ParseURL answers the pull request a github.com url names, and false for any
// other url.
func ParseURL(raw string) (wsm.PullRequestKey, bool) {
	u, err := url.Parse(strings.TrimSpace(raw))
	if err != nil || (u.Scheme != "https" && u.Scheme != "http") || !strings.EqualFold(u.Host, "github.com") {
		return wsm.PullRequestKey{}, false
	}
	m := pullPath.FindStringSubmatch(u.Path)
	if m == nil {
		return wsm.PullRequestKey{}, false
	}
	n, err := strconv.ParseUint(m[3], 10, 32)
	if err != nil {
		return wsm.PullRequestKey{}, false
	}
	return wsm.PullRequestKey{Owner: m[1], Repo: m[2], Number: uint32(n)}, true
}

// URLOf is the pull request's web address.
func URLOf(pr wsm.PullRequestKey) string {
	return fmt.Sprintf("https://github.com/%s/%s/pull/%d", pr.Owner, pr.Repo, pr.Number)
}

// remoteShapes are the spellings of a github.com remote: scp-like ssh
// (`git@github.com:owner/repo.git`, with or without a leading slash in the
// path) and url forms (https, ssh, git).
var remoteShapes = []*regexp.Regexp{
	regexp.MustCompile(`^[A-Za-z0-9_.-]+@github\.com:/?([A-Za-z0-9][A-Za-z0-9-]*)/([A-Za-z0-9._-]+?)(?:\.git)?/?$`),
	regexp.MustCompile(`^(?:https?|ssh|git)://(?:[^@/]+@)?github\.com(?::[0-9]+)?/([A-Za-z0-9][A-Za-z0-9-]*)/([A-Za-z0-9._-]+?)(?:\.git)?/?$`),
}

// RepositoryOfRemote answers the GitHub owner and name a remote url names,
// and false for a remote that is not on github.com.
func RepositoryOfRemote(remote string) (owner, repo string, ok bool) {
	for _, shape := range remoteShapes {
		if m := shape.FindStringSubmatch(strings.TrimSpace(remote)); m != nil {
			return m[1], m[2], true
		}
	}
	return "", "", false
}

// RemoteReader is the part of the git leaf a `#<number>` reference reads.
type RemoteReader interface {
	RemoteURL(ctx context.Context, repoDir, remote string) (string, error)
}

// shortRef is `#<number>`.
var shortRef = regexp.MustCompile(`^#([1-9][0-9]*)$`)

// UnparsableError is text that names no pull request. It is an answer to the
// caller, never a failure of the daemon.
type UnparsableError struct {
	Text   string
	Reason string
}

func (e *UnparsableError) Error() string {
	return fmt.Sprintf("prwatch: %q names no pull request: %s", e.Text, e.Reason)
}

// Resolve answers the pull request TEXT names: a github.com pull request url,
// or `#<number>` in the repository whose main checkout is REPODIR, through its
// `origin` remote. Text that names none is an *UnparsableError; a remote that
// cannot be read is the git leaf's error.
func Resolve(ctx context.Context, git RemoteReader, repoDir, text string) (wsm.PullRequestKey, error) {
	if pr, ok := ParseURL(text); ok {
		return pr, nil
	}
	m := shortRef.FindStringSubmatch(strings.TrimSpace(text))
	if m == nil {
		return wsm.PullRequestKey{}, &UnparsableError{Text: text, Reason: "not a github.com pull request url, nor `#<number>`"}
	}
	n, err := strconv.ParseUint(m[1], 10, 32)
	if err != nil {
		return wsm.PullRequestKey{}, &UnparsableError{Text: text, Reason: "the number is out of range"}
	}
	remote, err := git.RemoteURL(ctx, repoDir, "origin")
	if err != nil {
		return wsm.PullRequestKey{}, fmt.Errorf("prwatch: read the origin remote of %s: %w", repoDir, err)
	}
	owner, repo, ok := RepositoryOfRemote(remote)
	if !ok {
		return wsm.PullRequestKey{}, &UnparsableError{Text: text, Reason: fmt.Sprintf("the repository's origin %q is not on github.com", remote)}
	}
	return wsm.PullRequestKey{Owner: owner, Repo: repo, Number: uint32(n)}, nil
}
