package composermentions

import (
	"regexp"
	"strings"
	"unicode"
	"unicode/utf8"
)

// The slash word's punctuation, as proto/vocab/slash-word.json spells it: a
// slash word may follow OPENING in its whitespace-bounded run, and drops
// CLOSING from its end. The test asserts both against the file and runs every
// case it lists, which is what keeps the daemon, the composer
// (lisp/command-completion.el) and the webapp (feed/rows/known-commands.ts) on
// one rule.
const (
	opening = "([{<\"'`“‘"
	closing = ")]}>\"'`.,;:!?”’…"
)

// commandName is the shape of a name a slash word holds that could name a
// command: a runtime's command names, a plugin's `plugin:command` among them.
// `/usr/local` holds `usr/local`, which no command is called.
var commandName = regexp.MustCompile(`^[A-Za-z0-9][A-Za-z0-9_:.-]*$`)

// SlashWords answers the slash words of TEXT, each without its slash, in order
// of first appearance and without repeats, by the rule
// proto/vocab/slash-word.json pins. A slash word's name is answered only when
// it could name a command (commandName).
func SlashWords(text string) []string {
	var out []string
	seen := map[string]bool{}
	for run := range strings.FieldsSeq(text) {
		word := strings.TrimLeft(run, opening)
		if !strings.HasPrefix(word, "/") {
			continue
		}
		name := strings.TrimRight(word[1:], closing)
		if name == "" || !commandName.MatchString(name) || seen[name] {
			continue
		}
		seen[name] = true
		out = append(out, name)
	}
	return out
}

// nameChar reports whether R is a character a workspace name is made of, as
// the webapp's mention rule (feed/workspace-mentions.ts, NAME_CHAR) has it: a
// mention has none on either side, so `alpha` is never mentioned inside
// `alpha-beta`.
func nameChar(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsNumber(r) || r == '_' || r == '-'
}

// Mentions answers the NAMES TEXT mentions, in NAMES's order: a name occurs
// whole, with no name character on either side, exactly as the feed links a
// workspace's name.
func Mentions(text string, names []string) []string {
	var out []string
	for _, name := range names {
		if name != "" && occursWhole(text, name) {
			out = append(out, name)
		}
	}
	return out
}

// occursWhole reports whether NAME occurs in TEXT with no name character
// directly before or after it.
func occursWhole(text, name string) bool {
	for from := 0; ; {
		i := strings.Index(text[from:], name)
		if i < 0 {
			return false
		}
		start, end := from+i, from+i+len(name)
		before, _ := utf8.DecodeLastRuneInString(text[:start])
		after, _ := utf8.DecodeRuneInString(text[end:])
		if (start == 0 || !nameChar(before)) && (end == len(text) || !nameChar(after)) {
			return true
		}
		_, size := utf8.DecodeRuneInString(text[start:])
		from = start + size
	}
}
