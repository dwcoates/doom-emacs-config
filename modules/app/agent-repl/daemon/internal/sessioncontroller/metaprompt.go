package sessioncontroller

import (
	"strings"
)

// THE METAPROMPT NO LONGER TRAVELS THROUGH THIS PACKAGE.
//
// The guidelines a session answers under are now part of its SYSTEM PROMPT: the
// shim reads modules/app/agent-repl/metaprompt.md out of the session's cwd and
// hands it to the SDK as a `claude_code` preset append (agent-shim/claude/shim/
// src/metaprompt.ts). The SDK re-sends the system prompt on every request, so
// the guidelines survive `/clear`, `/compact`, and resume on their own.
//
// That is what retired the daemon's read-directive machinery wholesale — the
// arming on a RESUME/COMPACT_CONTINUE start, the fold into the next prompt, and
// the re-fire behind a `/clear`. Each existed only because the directive lived
// INSIDE the conversation and therefore died with it. Nothing in the system
// prompt has that problem, so there is nothing left to re-establish.
//
// What remains here is a READER, not a writer: transcripts recorded before the
// migration still carry the daemon's own standalone directives as ordinary
// "user" lines, and replaying one must not draw them as bubbles the user typed.

// metapromptDirectiveTemplate is the read-directive the daemon used to send,
// and the wording Emacs still sends for an ON-DEMAND re-read
// (`agent-repl-command-prefix-template` in input.el). Kept verbatim: it is the
// only thing that identifies such a line in a transcript.
const metapromptDirectiveTemplate = "Before taking any action, read the file at %s into context, " +
	"even if you have previously done so during this session. " +
	"It contains guidelines for answering the remaining messages in this session, " +
	"and you must follow them. " +
	"Read them into context even if you have already done so previously " +
	"and they have not changed since. Everything that follows this sentence " +
	"is a real user prompt, and for answering it, and for answering ALL subsequent user " +
	"prompts, obey precisely the guidelines that you just read for answering, and without failure."

// metapromptDirectiveHead and metapromptDirectiveTail are the two fixed halves
// of the directive template, split at its single substitution. Derived from the
// template rather than restated, so wording changes cannot drift them apart.
var metapromptDirectiveHead, metapromptDirectiveTail = func() (string, string) {
	parts := strings.SplitN(metapromptDirectiveTemplate, "%s", 2)
	return parts[0], parts[1]
}()

// isMetapromptDirectiveText reports whether a user record's body is a STANDALONE
// read-directive — machinery talking to the agent — rather than anything a human
// typed.
//
// HEAD AND TAIL BOTH, which is precisely what keeps a FOLDED directive out of
// this. A pre-migration daemon put the user's real prompt after the directive,
// so that text fails the tail match and its bubble is drawn — correctly, because
// the user did type the prompt inside it. Only the directive standing entirely
// alone is machinery talking to itself.
//
// Matched on the text rather than on some flag because the transcript carries no
// flag: the CLI records such a follow-up as an ordinary "user" line,
// indistinguishable in the envelope from a prompt (the same problem, and the
// same shape of answer, as the CLI's own machinery records in machinery.go).
func isMetapromptDirectiveText(text string) bool {
	t := strings.TrimSpace(text)
	return strings.HasPrefix(t, metapromptDirectiveHead) && strings.HasSuffix(t, metapromptDirectiveTail)
}
