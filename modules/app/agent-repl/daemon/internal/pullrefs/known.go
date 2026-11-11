package pullrefs

import (
	"regexp"
	"strings"
)

// Ref is one pull request: pull request NUMBER of the repository it names.
// NUMBER is the digits as written, never with a leading zero.
type Ref struct {
	Target
	Number string
}

// URL is the address of the pull request R names.
func (r Ref) URL() string {
	return r.PullURL(r.Number)
}

// PullPathPattern is the unanchored pattern of a github.com pull request's
// path, `/<owner>/<repo>/pull/<number>`, capturing the three in that order.
// It is THE ONE spelling of that shape: prwatch.ParseURL anchors it to read
// one url, Refs scans text for it.
const PullPathPattern = `/([A-Za-z0-9][A-Za-z0-9-]*)/([A-Za-z0-9._-]+)/pull/([1-9][0-9]*)`

// pullURL is a pull request's address on GitHub, wherever it is written:
// owner, repository, number. A url that runs on into a word byte (more digits,
// a letter) is not one, which the scan checks after the match.
var pullURL = regexp.MustCompile(`https?://(?:www\.)?github\.com` + PullPathPattern)

// Refs answers every pull request MARKDOWN names by its full address, in the
// order written. It reads the text whole, code and links alike: an address is
// what the pull request is, wherever it stands. A text still ARRIVING leaves an
// address at its very end alone, because the next push may add digits to it.
func Refs(markdown string, arriving bool) []Ref {
	var refs []Ref
	for _, m := range pullURL.FindAllStringSubmatchIndex(markdown, -1) {
		end := m[1]
		if end < len(markdown) && isWordByte(markdown[end]) || arriving && end == len(markdown) {
			continue
		}
		repo := markdown[m[4]:m[5]]
		if !validRepoName(repo) {
			continue
		}
		refs = append(refs, Ref{
			Target: Target{Owner: markdown[m[2]:m[3]], Repo: repo},
			Number: markdown[m[6]:m[7]],
		})
	}
	return refs
}

// Known is the pull requests a workspace's text has named, in the order it
// learned them: what a bare `#<number>` or a `<repo>#<number>` it writes later
// most plausibly means. A nil Known knows nothing.
type Known struct {
	refs []Ref
	seen map[Ref]bool
}

// NewKnown answers the Known that learned REFS in their order.
func NewKnown(refs []Ref) *Known {
	k := &Known{seen: map[Ref]bool{}}
	for _, ref := range refs {
		k.Learn(ref)
	}
	return k
}

// Learn adds REF as the latest pull request learned, and reports whether it
// was new. Learning a known pull request again changes nothing, so the order
// is when each was FIRST named, whatever order old text is read again in.
func (k *Known) Learn(ref Ref) bool {
	if k.seen[ref] {
		return false
	}
	k.seen[ref] = true
	k.refs = append(k.refs, ref)
	return true
}

// ForNumber answers the latest learned pull request numbered NUMBER.
func (k *Known) ForNumber(number string) (Ref, bool) {
	if k == nil {
		return Ref{}, false
	}
	for i := len(k.refs) - 1; i >= 0; i-- {
		if k.refs[i].Number == number {
			return k.refs[i], true
		}
	}
	return Ref{}, false
}

// OwnerOf answers the owner of the latest learned pull request of a
// repository named REPO. GitHub's repository names ignore case, so this
// lookup does too.
func (k *Known) OwnerOf(repo string) (string, bool) {
	if k == nil {
		return "", false
	}
	for i := len(k.refs) - 1; i >= 0; i-- {
		if strings.EqualFold(k.refs[i].Repo, repo) {
			return k.refs[i].Owner, true
		}
	}
	return "", false
}

// Links is what a workspace's references link into: the pull requests its text
// has named (Known), and its own repository (Home), nil for none.
type Links struct {
	Home  *Target
	Known *Known
}

// bare answers where `#NUMBER` links: the latest known pull request so
// numbered, else that number in Home.
func (l Links) bare(number string) (Ref, bool) {
	if ref, ok := l.Known.ForNumber(number); ok {
		return ref, true
	}
	if l.Home == nil {
		return Ref{}, false
	}
	return Ref{Target: *l.Home, Number: number}, true
}

// inRepo answers where `REPO#NUMBER` links: that repository under the owner a
// known pull request of it names, else under Home's owner.
func (l Links) inRepo(repo, number string) (Ref, bool) {
	if owner, ok := l.Known.OwnerOf(repo); ok {
		return Ref{Target: Target{Owner: owner, Repo: repo}, Number: number}, true
	}
	if l.Home == nil {
		return Ref{}, false
	}
	return Ref{Target: Target{Owner: l.Home.Owner, Repo: repo}, Number: number}, true
}
