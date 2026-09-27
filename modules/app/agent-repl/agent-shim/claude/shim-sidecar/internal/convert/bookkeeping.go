package convert

// bookkeeping.go — "USER" RECORDS NOBODY TYPED.
//
// THE FILE PLANE NEVER MINTS A PROMPT FOR A LINE THE PERSON DID NOT TYPE. The
// vendor files a great deal under the user's role that no person said: its own
// slash-command bookkeeping, a local command's printed output, the interrupt
// marker, and the notification a background task delivers into the
// conversation. On the STREAM plane every one of these is already classified —
// the daemon recognizes a session command before it is ever sent, the shim
// files `local_command_output` as vendor-specific residue, drops every
// prompt-shaped user record (R15), and settles a detached run from the task
// stream's own `task_notification`. On this plane they arrive as ordinary
// user records, and before this file each one fell through to the adopted
// external-prompt path and was drawn as a USER PROMPT BUBBLE in every replayed
// or re-ingested history.
//
// SO EACH SHAPE IS SENT TO THE ARM THE STREAM PLANE USES FOR THE SAME FACT:
//
//   - A TASK NOTIFICATION is documented residue, never a prompt.
//   - A SLASH COMMAND the CLI answers itself (its expanded envelope, or the bare
//     `/compact` line some CLI versions also write) is vendor-specific residue:
//     the daemon consumes a session command before it reaches any stream, and
//     the context cut a /compact leaves is the compaction boundary's own row
//     (contextcut.go). `/clear` keeps its context-cut arm (user.go).
//   - A LOCAL COMMAND'S OUTPUT (`<local-command-stdout>`, including the
//     "Compacted" notice) is vendor-specific residue, as the shim files
//     `local_command_output`.
//   - THE INTERRUPT MARKER (`[Request interrupted by user…]`) is
//     vendor-specific residue: the stream plane states a stop on the turn's
//     terminal, never as something the person said.
//   - A record whose `origin.kind` names a source that is not a person, and no
//     arm above, is `unknown` residue: parsed, not modelled.
//
// A PROMPT COMMAND IS STILL A PROMPT. A custom command (a skill, a project
// command) expands into a prompt for the agent; the vendor writes its envelope
// WITHOUT the local-command caveat the CLI puts before every command it answers
// itself, and the schema's SessionCommand table does not name it. Its envelope
// is served as the prompt the person typed — `/name args` — never as markup.
//
// A PASTE STAYS A PROMPT. `<pasted_content …>` is the CLI's rendering of text
// the person pasted into a prompt they typed; no rule here matches it, so it
// reaches the prompt path verbatim, as the daemon draws a pasted prompt.

import (
	"strings"

	storev1 "agentrepl/proto/store/v1"
)

// The vendor_specific kinds this file withholds under. Each is a DECLARED
// withholding class (the sidecar's AGENTS.md, "Withholding classes").
const (
	kindUserSlashCommand       = "user/slash_command"
	kindUserLocalCommandOutput = "user/local_command_output"
	kindUserInterrupt          = "user/interrupt"
	kindUserTaskNotification   = "user/task_notification"
)

// The vendor's markup for the shapes above.
const (
	localCommandCaveatOpen = "<local-command-caveat>"
	commandNameOpen        = "<command-name>"
	taskNotificationOpen   = "<task-notification>"
	taskNotificationClose  = "</task-notification>"
	interruptMarkerPrefix  = "[Request interrupted by user"
)

// localCommandOutputTags are the elements the CLI wraps a local command's
// printed output in.
var localCommandOutputTags = []string{"local-command-stdout", "local-command-stderr"}

// originHuman is the `origin.kind` of a prompt a person typed; originTaskNotification
// that of a background task's notification.
const (
	originHuman            = "human"
	originTaskNotification = "task-notification"
)

// noteLocalCommandCaveat remembers the uuid of the caveat the CLI writes
// immediately before every command it answers ITSELF. The command's envelope is
// that caveat's child, which is the vendor's own statement that the envelope is
// a local command rather than a prompt command.
//
// ONE VALUE, NOT A SET: the caveat and its envelope are adjacent records.
func (c *Converter) noteLocalCommandCaveat(record map[string]any, env envelope) {
	if !strings.HasPrefix(strings.TrimSpace(firstText(obj(record["message"]))), localCommandCaveatOpen) {
		return
	}
	c.localCommandCaveat = env.uuid
}

// notTypedByPerson classifies a user record that reached the prompt path but
// that no person typed. It answers the record's entries and true, or nil and
// false when the record IS something a person typed.
func (c *Converter) notTypedByPerson(record, message map[string]any, at Attribution, env envelope, agent string) ([]*storev1.StoreEntry, bool) {
	text, sole := soleText(message)
	// THE VENDOR'S OWN WORD WINS. A record stating its origin is classified by
	// it; only a record from a CLI that writes no origin is recognized by its
	// shape, so a person who typed the markup is never overruled.
	if env.originKind == originTaskNotification || (env.originKind == "" && sole && isTaskNotification(text)) {
		c.log.With(at.ctxFor("not-typed")).
			LogVerbose("a background task's notification withheld as vendor_specific, never a prompt")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, kindUserTaskNotification, record)}, true
	}
	if env.originKind != "" && env.originKind != originHuman {
		c.log.With(at.ctxFor("not-typed")).
			LogVerbose("user record's origin.kind=%q names a source that is not a person and no arm models it; classified as unknown residue, never a prompt", env.originKind)
		return []*storev1.StoreEntry{UnknownEntry(at, env.originKind, "origin.kind", record)}, true
	}
	if !sole {
		return nil, false
	}
	if strings.Contains(text, commandNameOpen) {
		return c.commandEnvelope(record, text, at, env, agent)
	}
	switch {
	case isLocalCommandOutput(text):
		c.log.With(at.ctxFor("not-typed")).
			LogVerbose("a local command's printed output withheld as vendor_specific (the stream plane files local_command_output the same way)")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, kindUserLocalCommandOutput, record)}, true
	case isInterruptMarker(text):
		c.log.With(at.ctxFor("not-typed")).
			LogVerbose("the interrupt marker withheld as vendor_specific (a stop is the turn terminal's to state, never something the person said)")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, kindUserInterrupt, record)}, true
	case typedSessionCommand(text):
		c.log.With(at.ctxFor("not-typed")).
			LogVerbose("a session command typed bare (%q) withheld as vendor_specific (the CLI answers it itself; a context cut is its boundary's own row)", strings.TrimSpace(text))
		return []*storev1.StoreEntry{VendorSpecificEntry(at, kindUserSlashCommand, record)}, true
	default:
		return nil, false
	}
}

// commandEnvelope classifies the CLI's expanded slash-command envelope.
//
// A LOCAL COMMAND is withheld: the caveat the CLI wrote before it names it one,
// and so does the schema's SessionCommand table, which is the fallback when the
// caveat lies behind this reader's window. Anything else the envelope names is a
// PROMPT COMMAND the person typed, served as the text they typed. An envelope
// with prose around it is a prompt QUOTING a command and stays that prompt.
func (c *Converter) commandEnvelope(record map[string]any, text string, at Attribution, env envelope, agent string) ([]*storev1.StoreEntry, bool) {
	name, args, ok := unwrapCommandEnvelope(text)
	if !ok || name == "" {
		return nil, false
	}
	caveated := c.localCommandCaveat != "" && str(record["parentUuid"]) == c.localCommandCaveat
	if caveated || isSessionCommand(name, args) {
		c.log.With(at.ctxFor("not-typed")).
			LogVerbose("slash command %s withheld as vendor_specific: the CLI answers it itself (caveated=%t)", name, caveated)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, kindUserSlashCommand, record)}, true
	}
	typed := name
	if args != "" {
		typed += " " + args
	}
	c.log.With(at.ctxFor("not-typed")).
		LogVerbose("slash command %s is a prompt command (no local-command caveat, not a session command); served as the text the person typed", name)
	typedMessage := map[string]any{"role": str(obj(record["message"])["role"]), "content": typed}
	return []*storev1.StoreEntry{c.humanPrompt(record, typedMessage, at, env, agent)}, true
}

// soleText answers a user message's text when the message is NOTHING BUT text:
// a bare string, or a block list holding exactly one block, a text block. A
// message carrying an image or several blocks is something a person composed,
// never one of the vendor's single-element records.
func soleText(message map[string]any) (string, bool) {
	switch content := message["content"].(type) {
	case string:
		return content, true
	case []any:
		if len(content) != 1 {
			return "", false
		}
		block := obj(content[0])
		if block == nil || str(block["type"]) != "text" {
			return "", false
		}
		return str(block["text"]), true
	default:
		return "", false
	}
}

// isTaskNotification reports whether text is one `<task-notification>` element
// and nothing else.
func isTaskNotification(text string) bool {
	trimmed := strings.TrimSpace(text)
	return strings.HasPrefix(trimmed, taskNotificationOpen) && strings.HasSuffix(trimmed, taskNotificationClose)
}

// isLocalCommandOutput reports whether text is one local-command output element
// and nothing else.
func isLocalCommandOutput(text string) bool {
	trimmed := strings.TrimSpace(text)
	for _, tag := range localCommandOutputTags {
		open, closing := "<"+tag+">", "</"+tag+">"
		if strings.HasPrefix(trimmed, open) && strings.HasSuffix(trimmed, closing) &&
			strings.Count(trimmed, open) == 1 {
			return true
		}
	}
	return false
}

// isInterruptMarker reports whether text is the vendor's interrupt marker — the
// bracketed line it writes when a person stops a turn, with or without the
// "for tool use" qualifier — and nothing else.
func isInterruptMarker(text string) bool {
	trimmed := strings.TrimSpace(text)
	return strings.HasPrefix(trimmed, interruptMarkerPrefix) && strings.HasSuffix(trimmed, "]") &&
		!strings.Contains(trimmed, "\n")
}
