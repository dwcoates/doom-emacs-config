package sessiondrv

import (
	"fmt"
	"os"
	"path/filepath"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// metapromptRelPath is the in-repo path (relative to a session's cwd) of the
// canonical metaprompt file. It mirrors the Emacs side
// (input.el:agent-repl-metaprompt-file, which resolves the same
// modules/app/agent-repl/metaprompt.md), so a resumed/compact-continued
// session re-reads the exact same guidelines the interactive editor prepends.
const metapromptRelPath = "modules/app/agent-repl/metaprompt.md"

// metapromptDirectiveTemplate is the daemon-side twin of Emacs's
// agent-repl-command-prefix-template (input.el). It is a plain instruction to
// read the metaprompt file — the "metaprompt" framing lives inside the .md
// file, not here — filled with the absolute path to that file. Kept verbatim
// so the read-directive a resumed session receives is identical to the one the
// interactive editor fires.
const metapromptDirectiveTemplate = "Before taking any action, read the file at %s into context, " +
	"even if you have previously done so during this session. " +
	"It contains guidelines for answering the remaining messages in this session, " +
	"and you must follow them. " +
	"Read them into context even if you have already done so previously " +
	"and they have not changed since. Everything that follows this sentence " +
	"is a real user prompt, and for answering it, and for answering ALL subsequent user " +
	"prompts, obey precisely the guidelines that you just read for answering, and without failure."

// metapromptDirective returns the read-directive block for a session whose cwd
// is cwd, or "" (with ok=false) when no directive applies: the metaprompt file
// does not exist under cwd. The path is <cwd>/modules/app/agent-repl/metaprompt.md.
func metapromptDirective(cwd string) (string, bool) {
	if cwd == "" {
		return "", false
	}
	path := filepath.Join(cwd, metapromptRelPath)
	if _, err := os.Stat(path); err != nil {
		return "", false
	}
	return fmt.Sprintf(metapromptDirectiveTemplate, path), true
}

// wantsMetapromptRefire reports whether a SessionStarted event should arm the
// metaprompt read-directive re-fire (design §10, task step 4): the source is a
// RESUME or COMPACT_CONTINUE (a fresh session started interactively already got
// the directive from Emacs; a resumed/continued one lost that context and must
// be reminded once).
func wantsMetapromptRefire(ss *corev1.SessionStarted) bool {
	if ss == nil {
		return false
	}
	switch ss.GetSource() {
	case corev1.SessionSource_SESSION_SOURCE_RESUME,
		corev1.SessionSource_SESSION_SOURCE_COMPACT_CONTINUE:
		return true
	default:
		return false
	}
}

// prependMetaprompt returns the directive block followed by a blank line and
// the original prompt text. Used to fold the read-directive into the NEXT
// SubmitPrompt for an armed session (once).
func prependMetaprompt(directive, text string) string {
	return directive + "\n\n" + text
}
