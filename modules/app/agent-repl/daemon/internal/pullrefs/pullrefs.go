// Package pullrefs turns a pull request reference written as `#<number>` or
// `<repo>#<number>` into a link to that pull request on GitHub. A bare
// reference links into the repository of the workspace whose text it is, and a
// `<repo>#<number>` one into that repository under the same owner.
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
	raw := strings.TrimSpace(getenv(EnvOwners))
	if raw == "" {
		return owners, nil
	}
	for pair := range strings.SplitSeq(raw, ",") {
		dir, owner, ok := strings.Cut(strings.TrimSpace(pair), "=")
		dir, owner = strings.TrimSpace(dir), strings.TrimSpace(owner)
		if !ok || dir == "" {
			return nil, usersetup.Errorf("pullrefs: %s pair %q is not `<config-dir>=<owner>`", EnvOwners, pair)
		}
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

// TargetFor answers the target of a repository whose main checkout is
// REPO_DIR and whose work is routed to the account root CONFIG_DIR. An account
// with no owner has no target.
func (o Owners) TargetFor(configDir, repoDir string) (Target, bool) {
	owner, ok := o[filepath.Clean(configDir)]
	if !ok {
		return Target{}, false
	}
	return Target{Owner: owner, Repo: filepath.Base(filepath.Clean(repoDir))}, true
}

// PullURL is the address of pull request NUMBER in T.
func (t Target) PullURL(number string) string {
	return "https://github.com/" + t.Owner + "/" + t.Repo + "/pull/" + number
}

// Linkify answers MARKDOWN with every `#<number>` written as a link to that
// pull request in T, and every `<repo>#<number>` as a link to that pull
// request in T's owner's repository REPO.
//
// A reference is linked only where it stands as a word of prose: never in a
// fenced code block or a code span, never inside an existing link (its text or
// its destination), and never glued to what precedes it (a url's fragment, an
// entity). A text still
// ARRIVING leaves a reference at its very end alone, because the next push
// may add digits to it.
func Linkify(markdown string, t Target, arriving bool) string {
	var out strings.Builder
	out.Grow(len(markdown))
	var fence string
	lines := strings.SplitAfter(markdown, "\n")
	offset := 0
	for _, line := range lines {
		start := offset
		offset += len(line)
		if marker := fenceMarker(line); fence == "" && marker != "" {
			fence = marker
			out.WriteString(line)
			continue
		} else if fence != "" {
			if closesFence(line, fence) {
				fence = ""
			}
			out.WriteString(line)
			continue
		}
		linkifyLine(&out, line, t, arriving && start+len(line) == len(markdown))
	}
	return out.String()
}

// fenceMarker answers the backtick or tilde run LINE opens a fence with, or ""
// when LINE opens none.
func fenceMarker(line string) string {
	trimmed := strings.TrimLeft(line, " ")
	if len(line)-len(trimmed) > 3 {
		return ""
	}
	for _, c := range []byte{'`', '~'} {
		n := 0
		for n < len(trimmed) && trimmed[n] == c {
			n++
		}
		if n >= 3 {
			return trimmed[:n]
		}
	}
	return ""
}

// closesFence reports whether LINE closes the fence MARKER opened.
func closesFence(line, marker string) bool {
	trimmed := strings.TrimSpace(line)
	if len(strings.TrimLeft(line, " "))+3 < len(line) || len(trimmed) < len(marker) {
		return false
	}
	return strings.Trim(trimmed, marker[:1]) == ""
}

// linkifyLine writes LINE to OUT with its references linked. ATEND says the
// line ends a text still arriving.
//
// The scan walks LINE once and copies three kinds of span verbatim before a
// reference can be found in them: a code span, a link (its text and its
// destination, see linkEnd), and any word that is not a reference. A reference
// is linked only where the scan meets it at the start of a word.
func linkifyLine(out *strings.Builder, line string, t Target, atEnd bool) {
	i := 0
	for i < len(line) {
		c := line[i]
		switch {
		case c == '`':
			run := backtickRun(line, i)
			if end := closingRun(line, i+run, run); end >= 0 {
				out.WriteString(line[i : end+run])
				i = end + run
				continue
			}
			out.WriteString(line[i : i+run])
			i += run
		case c == '[':
			end := linkEnd(line, i)
			if end < 0 {
				out.WriteByte(c)
				i++
				continue
			}
			out.WriteString(line[i:end])
			i = end
		case c == '#' && startsWord(line, i):
			number, end, ok := pullNumber(line, i, atEnd)
			if !ok {
				out.WriteByte(c)
				i++
				continue
			}
			writePullLink(out, "#"+number, t.PullURL(number))
			i = end
		case isRepoByte(c) && startsWord(line, i):
			name := repoName(line, i)
			if hash := i + len(name); hash < len(line) && line[hash] == '#' && validRepoName(name) {
				if number, end, ok := pullNumber(line, hash, atEnd); ok {
					writePullLink(out, name+"#"+number, Target{Owner: t.Owner, Repo: name}.PullURL(number))
					i = end
					continue
				}
			}
			out.WriteString(name)
			i += len(name)
		default:
			out.WriteByte(c)
			i++
		}
	}
}

// writePullLink writes the markdown link TEXT to URL.
func writePullLink(out *strings.Builder, text, url string) {
	out.WriteString("[" + text + "](" + url + ")")
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
	if end == hash+1 || line[hash+1] == '0' || (end < len(line) && isWordByte(line[end])) || (atEnd && end == len(line)) {
		return "", 0, false
	}
	return line[hash+1 : end], end, true
}

// startsWord reports whether position I begins a word of prose: the line's
// start, after whitespace, or after an opening parenthesis. A parenthesis that
// opens a link's destination never reaches here, because linkEnd consumes the
// whole link.
func startsWord(line string, i int) bool {
	if i == 0 {
		return true
	}
	switch line[i-1] {
	case ' ', '\t', '(':
		return true
	}
	return false
}

// linkEnd answers where the inline link opening with the `[` at I ends, or -1
// when the `[` opens no link. A link is its text up to the matching `]`
// (brackets nest, code spans hide theirs) followed at once by `(`, then its
// destination up to the matching `)`. A destination still unclosed at the
// line's end runs to it, so a link arriving mid-destination stays whole.
func linkEnd(line string, i int) int {
	depth := 0
	j := i
	for j < len(line) {
		switch line[j] {
		case '`':
			run := backtickRun(line, j)
			if end := closingRun(line, j+run, run); end >= 0 {
				j = end + run
			} else {
				j += run
			}
			continue
		case '\\':
			j += 2
			continue
		case '[':
			depth++
		case ']':
			depth--
		}
		j++
		if depth == 0 {
			break
		}
	}
	if depth != 0 || j >= len(line) || line[j] != '(' {
		return -1
	}
	parens := 0
	for ; j < len(line); j++ {
		switch line[j] {
		case '\\':
			j++
		case '(':
			parens++
		case ')':
			parens--
			if parens == 0 {
				return j + 1
			}
		}
	}
	return len(line)
}

// isRepoByte reports whether C may appear in a GitHub repository name.
func isRepoByte(c byte) bool {
	return isWordByte(c) || c == '-' || c == '.'
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

// isWordByte reports whether C continues a word, so a reference followed by it
// is not a reference.
func isWordByte(c byte) bool {
	return c == '_' || c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
}

// backtickRun is the length of the backtick run at I.
func backtickRun(line string, i int) int {
	n := 0
	for i+n < len(line) && line[i+n] == '`' {
		n++
	}
	return n
}

// closingRun is where the next backtick run of exactly N at or after FROM
// begins, or -1 when the span is never closed.
func closingRun(line string, from, n int) int {
	for i := from; i < len(line); {
		if line[i] != '`' {
			i++
			continue
		}
		run := backtickRun(line, i)
		if run == n {
			return i
		}
		i += run
	}
	return -1
}
