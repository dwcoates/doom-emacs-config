// Package slackrefs turns a Slack channel written as `#<channel-name>` into a
// link to that channel's permalink, `https://<subdomain>.slack.com/archives/
// <channel-id>`, when the name is one of the user's channels
// (slackchannels.Directory). The permalink is the one shape a Slack link
// takes: clicking it opens the daemon's OpenExternal, which hands a
// permalink on a configured subdomain to the Slack desktop app
// (externalbrowser.SlackDeepLink), so this package never spells `slack://`.
//
// A NAME NOT AMONG THE USER'S CHANNELS STAYS PLAIN TEXT, and so does an
// all-digit one: `#123` is a pull request reference (pullrefs).
package slackrefs

import (
	"strings"

	"agent-repld/internal/mdinline"
)

// maxName is the longest channel name Slack accepts.
const maxName = 80

// Channels is the user's channels on one Slack workspace.
type Channels struct {
	// Subdomain is the Slack workspace's subdomain, the part before
	// `.slack.com`.
	Subdomain string
	// IDs is each channel's id, keyed by its name. It is never written once
	// handed over.
	IDs map[string]string
}

// Permalink is the address of the channel ID on the Slack workspace at
// SUBDOMAIN.
func Permalink(subdomain, id string) string {
	return "https://" + subdomain + ".slack.com/archives/" + id
}

// Linkify answers MARKDOWN with every `#<channel-name>` CHANNELS names written
// as a link to its permalink. A channel is linked only where it stands as a
// word of prose (mdinline.RewriteProse): never in a fenced code block or a
// code span, never inside a link of any kind, and never glued to what
// precedes or follows it. A text still ARRIVING leaves a channel at its very
// end alone, because the next push may lengthen its name.
func Linkify(markdown string, channels Channels, arriving bool) string {
	if len(channels.IDs) == 0 || !strings.Contains(markdown, "#") {
		return markdown
	}
	return mdinline.RewriteProse(markdown, arriving, func(out *strings.Builder, s mdinline.Stretch) {
		linkifyText(out, s, channels)
	})
}

// linkifyText writes S's stretch to OUT with its channels linked.
func linkifyText(out *strings.Builder, s mdinline.Stretch, channels Channels) {
	line := s.Line[:s.To]
	for i := s.From; i < len(line); {
		if line[i] == '#' && mdinline.StartsWord(line, i) {
			if name, ok := channelName(line, i, s.AtEnd); ok {
				if id, known := channels.IDs[name]; known {
					out.WriteString("[#" + name + "](" + Permalink(channels.Subdomain, id) + ")")
					i += 1 + len(name)
					continue
				}
			}
		}
		out.WriteByte(line[i])
		i++
	}
}

// channelName reads the channel name after the `#` at HASH. It is no name
// when nothing a name holds follows, when it is all digits (a pull request
// reference), when it is longer than Slack allows, when a byte no name holds
// but a word does follows it (an uppercase letter, a non-ASCII rune), or when
// ATEND says the text is still arriving and the name runs to the line's end.
func channelName(line string, hash int, atEnd bool) (string, bool) {
	end := hash + 1
	digits := true
	for end < len(line) && isNameByte(line[end]) {
		digits = digits && line[end] >= '0' && line[end] <= '9'
		end++
	}
	name := line[hash+1 : end]
	switch {
	case name == "", digits, len(name) > maxName:
		return "", false
	case end < len(line) && (mdinline.IsWordByte(line[end]) || line[end] >= 0x80):
		return "", false
	case atEnd && end == len(line):
		return "", false
	}
	return name, true
}

// isNameByte reports whether C may appear in a Slack channel name: lowercase
// letters, digits, hyphens and underscores.
func isNameByte(c byte) bool {
	return c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '-' || c == '_'
}
