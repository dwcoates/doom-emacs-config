// Package pullrefs turns a pull request reference written as `#<number>` or
// `<repo>#<number>` into a link to that pull request on GitHub. A bare
// reference links to the pull request of that number the workspace's text last
// named by its full address or by its repository (Known), and otherwise into
// the repository of the workspace whose text it is. A `<repo>#<number>` one
// links into that repository under the owner a known pull request of it names,
// and otherwise under the workspace's owner.
//
// WHICH GITHUB OWNER a repository belongs to is decided by the account its
// work is routed to (account.Resolver.ConfigDirFor): the work account's
// repositories live under the `ChessCom` organization, and any other account's
// owner is configuration (EnvOwners). An account with no owner rewrites
// nothing, because there is no repository to link to.
package pullrefs

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/mdinline"
	"agent-repld/internal/usersetup"
)

// EnvOwners names the GitHub owner of each further account root, as
// comma-separated `<config-dir>=<owner>` pairs. A config dir may start with
// `~/`. A pair naming the work root replaces its built-in owner.
const EnvOwners = "AGENT_REPL_GITHUB_OWNERS"

// WorkOwner is the GitHub organization the work account's repositories live
// under. It is an organization-level value, so it is literal.
const WorkOwner = "ChessCom"

// ownerShape is what GitHub accepts as a user or organization login.
var ownerShape = regexp.MustCompile(`^[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?$`)

// Owners is the GitHub owner of each account root, keyed by the root cleaned.
type Owners map[string]string

// OwnersFromEnv builds the owners: WorkOwner for workRoot, then every pair
// EnvOwners names. HOME expands a pair's leading `~` (dirpath.Absolute). A
// pair that does not parse is an error naming it, never a pair skipped.
func OwnersFromEnv(getenv func(string) string, home, workRoot string) (Owners, error) {
	if workRoot == "" {
		return nil, fmt.Errorf("pullrefs: the work account root is required")
	}
	owners := Owners{filepath.Clean(workRoot): WorkOwner}
	pairs, err := usersetup.Pairs("pullrefs", EnvOwners, "<config-dir>=<owner>", getenv(EnvOwners))
	if err != nil {
		return nil, err
	}
	for _, p := range pairs {
		dir, owner, pair := p.Key, p.Value, p.Entry
		if !ownerShape.MatchString(owner) {
			return nil, usersetup.Errorf("pullrefs: %s pair %q names %q, which is not a GitHub login", EnvOwners, pair, owner)
		}
		root, err := dirpath.Absolute(dir, home)
		if err != nil {
			return nil, usersetup.Errorf("pullrefs: %s pair %q names no config dir: %w", EnvOwners, pair, err)
		}
		owners[root] = owner
	}
	return owners, nil
}

// Target is the GitHub repository a workspace's references link into.
type Target struct {
	Owner string
	Repo  string
}

// OwnerFor answers the GitHub owner of the account root CONFIG_DIR, and false
// for an account with no owner, whose references link nowhere.
func (o Owners) OwnerFor(configDir string) (string, bool) {
	owner, ok := o[filepath.Clean(configDir)]
	return owner, ok
}

// TargetFor answers the target of the GitHub repository named REPO whose work
// is routed to the account root CONFIG_DIR. REPO is the name the repository's
// `origin` remote gives it, never its checkout's directory name: the two
// differ (the `doom` checkout's origin is `doom-emacs-config`). An account
// with no owner has no target.
func (o Owners) TargetFor(configDir, repo string) (Target, bool) {
	owner, ok := o.OwnerFor(configDir)
	if !ok {
		return Target{}, false
	}
	return Target{Owner: owner, Repo: repo}, true
}

// PullURL is the address of pull request NUMBER in T: THE ONE spelling of a
// pull request's web address (prwatch.URLOf builds through it too).
func (t Target) PullURL(number string) string {
	return "https://github.com/" + t.Owner + "/" + t.Repo + "/pull/" + number
}

// Linkify answers MARKDOWN with every `#<number>` and every
// `<repo>#<number>` written as a link to the pull request LINKS resolves it to
// (Links.bare, Links.inRepo); one LINKS resolves nowhere is left as written.
// It also answers the pull requests its `<repo>#<number>` references named,
// in order, which the caller learns (Known.Learn): a later bare reference to
// the same number means the same pull request.
//
// A reference is linked only where it stands as a word of prose
// (mdinline.RewriteProse): never in a fenced code block or a code span, never inside
// a link of any kind (its text, its destination, a bare URL, an autolink, a
// reference definition), and never glued to what precedes it (an entity). A
// text still ARRIVING leaves a reference at its very end alone, because the
// next push may add digits to it.
func Linkify(markdown string, links Links, arriving bool) (string, []Ref) {
	var named []Ref
	linked := mdinline.RewriteProse(markdown, arriving, func(out *strings.Builder, s mdinline.Stretch) {
		named = linkifyText(out, s.Line[:s.To], s.From, links, s.AtEnd, named)
	})
	return linked, named
}

// linkifyText writes LINE[FROM:], prose, to OUT with its references linked,
// and answers NAMED with the pull requests its `<repo>#<number>` references
// named appended. ATEND says LINE ends a text still arriving. A reference is
// linked only where the scan meets it at the start of a word, and any other
// word is copied whole.
func linkifyText(out *strings.Builder, line string, from int, links Links, atEnd bool, named []Ref) []Ref {
	i := from
	for i < len(line) {
		c := line[i]
		switch {
		case c == '#' && mdinline.StartsWord(line, i):
			number, end, ok := pullNumber(line, i, atEnd)
			ref, linked := links.bare(number)
			if !ok || !linked {
				out.WriteByte(c)
				i++
				continue
			}
			mdinline.WriteLink(out, "#"+number, ref.URL())
			i = end
		case isRepoByte(c) && mdinline.StartsWord(line, i):
			name := repoName(line, i)
			if hash := i + len(name); hash < len(line) && line[hash] == '#' && validRepoName(name) {
				if number, end, ok := pullNumber(line, hash, atEnd); ok {
					if ref, linked := links.inRepo(name, number); linked {
						mdinline.WriteLink(out, name+"#"+number, ref.URL())
						named = append(named, ref)
						i = end
						continue
					}
				}
			}
			out.WriteString(name)
			i += len(name)
		default:
			out.WriteByte(c)
			i++
		}
	}
	return named
}

// pullNumber reads the pull request number after the `#` at HASH. END is where
// the reference stops. It is not a number when no digits follow, when they
// lead with a zero, when a word byte follows them, or when ATEND says the text
// is still arriving and the digits run to the line's end (more may follow).
func pullNumber(line string, hash int, atEnd bool) (number string, end int, ok bool) {
	end = hash + 1
	for end < len(line) && line[end] >= '0' && line[end] <= '9' {
		end++
	}
	if end == hash+1 || line[hash+1] == '0' || (end < len(line) && mdinline.IsWordByte(line[end])) || (atEnd && end == len(line)) {
		return "", 0, false
	}
	return line[hash+1 : end], end, true
}

// isRepoByte reports whether C may appear in a GitHub repository name.
func isRepoByte(c byte) bool {
	return mdinline.IsWordByte(c) || c == '-' || c == '.'
}

// repoName is the run of repository-name bytes starting at I.
func repoName(line string, i int) string {
	j := i
	for j < len(line) && isRepoByte(line[j]) {
		j++
	}
	return line[i:j]
}

// validRepoName reports whether NAME can be a GitHub repository name: `.` and
// `..` are reserved.
func validRepoName(name string) bool {
	return name != "." && name != ".."
}
