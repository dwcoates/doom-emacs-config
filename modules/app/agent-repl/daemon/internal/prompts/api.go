// Package prompts reads the prompts/ directory: the briefs the daemon splices
// and sends as prompts.
//
// Briefs are read AT USE TIME, never cached across a use, so editing one takes
// effect without a daemon bounce. A missing or malformed brief is LOUD: the
// caller refuses rather than sending an unspliced or partial prompt. See
// ARCHITECTURE.md "merge (internal/merge)" and the RequestCommandSupport
// ruling (`prompts/add-support-slash-command.md`).
package prompts

import (
	"claude-repld/internal/notimpl"
)

// Prompt is one loaded brief: its parsed header and its body.
type Prompt struct {
	// Name is the brief's file name without the .md suffix.
	Name string
	// Header is the brief's front-matter keys, as parsed.
	Header map[string]string
	// Body is the brief's text, placeholders unspliced.
	Body string
	// Placeholders are the placeholder names the body uses, in first-use
	// order. Splice refuses a values map that does not cover them exactly.
	Placeholders []string
}

// Load reads one brief by name from dir. A brief that is absent, unreadable,
// or whose header does not parse is an error the caller surfaces; nothing is
// defaulted.
func Load(dir, name string) (Prompt, error) {
	return Prompt{}, notimpl.Err
}

// Splice substitutes values into the body's placeholders. A value for an
// unknown placeholder, or a placeholder with no value, is an error: a brief is
// never sent with a hole in it.
func (p Prompt) Splice(values map[string]string) (string, error) {
	return "", notimpl.Err
}
