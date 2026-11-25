// Package skillrefs turns an inline code span that names one of a
// workspace's skills (`ceac-resource-usage`, or `/ceac-resource-usage`) into
// a link drawn `/ceac-resource-usage` whose destination is that skill's
// `SKILL.md`: the file the workspace's session resolves the name to.
//
// WHICH FILE A NAME IS, IS THE SESSION'S. The workspace's command catalog
// states each skill's file as its runtime resolved it
// (conversation.v1.SessionCommandCatalogEntry.skill_file): the worktree's
// project skill, the account's own, or a plugin's. Nothing here looks a skill
// up on disk, since a lookup of its own could choose a source the session did
// not.
//
// THE LINK IS A FILE LINK. Its destination is the file's path spelled from the
// home directory (`~/...`) when the file lies beneath it, the form every file
// link the feed draws takes, so a click opens it in the editor through the one
// path every file link takes (OpenInEditorFeedLink).
package skillrefs

import (
	"strings"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/mdinline"
)

// Files answers the `SKILL.md` the skill NAME resolves to, and false for a
// name that is no skill with a file.
type Files func(name string) (file string, ok bool)

// Linkify answers MARKDOWN with every inline code span that is wholly a name
// FILES knows, optionally led by `/`, written as a link to that skill's file
// (dirpath.Tilde, under HOME). Only a code span of prose is linked: never one inside a
// link of any kind, a fenced block or a reference definition, and never an
// unknown name, which stays inline code. A text still ARRIVING leaves a code
// span at its very end alone, because the next push may lengthen its closing
// backtick run into an opening one.
func Linkify(markdown string, files Files, home string, arriving bool) string {
	if !strings.Contains(markdown, "`") {
		return markdown
	}
	var out strings.Builder
	out.Grow(len(markdown))
	doc := mdinline.Read(markdown)
	offset := 0
	for _, line := range doc.Lines {
		start := offset
		offset += len(line.Text)
		if line.Kind != mdinline.Prose {
			out.WriteString(line.Text)
			continue
		}
		for _, s := range doc.Spans(line.Text) {
			atEnd := arriving && start+s.At+len(s.Raw) == len(markdown)
			if s.Kind != mdinline.Code || atEnd {
				out.WriteString(s.Raw)
				continue
			}
			name := strings.TrimPrefix(strings.TrimSpace(s.Inner), "/")
			file, ok := lookup(files, name)
			if !ok {
				out.WriteString(s.Raw)
				continue
			}
			out.WriteString("[" + linkText(name) + "](" + destination(dirpath.Tilde(file, home)) + ")")
		}
	}
	return out.String()
}

// lookup answers FILES' file for NAME. A name with whitespace or a backtick is
// never a skill's, so it is never asked.
func lookup(files Files, name string) (string, bool) {
	if name == "" || strings.ContainsAny(name, " \t\n`") {
		return "", false
	}
	return files(name)
}

// linkText is the link's drawn text, `/` and NAME, with every character that
// could open markup in a link's text escaped, so it draws as written.
func linkText(name string) string {
	var out strings.Builder
	out.WriteByte('/')
	for i := 0; i < len(name); i++ {
		switch c := name[i]; c {
		case '\\', '`', '*', '_', '[', ']', '<', '>', '&', '$', '~', '!':
			out.WriteByte('\\')
			out.WriteByte(c)
		default:
			out.WriteByte(c)
		}
	}
	return out.String()
}

// destination is HREF as a link destination: bare when it holds nothing a
// bare destination cannot, and in angle brackets otherwise.
func destination(href string) string {
	if !strings.ContainsAny(href, " \t()<>") {
		return href
	}
	return "<" + strings.NewReplacer("<", `\<`, ">", `\>`).Replace(href) + ">"
}
