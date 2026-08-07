// freshgate.go holds the ONE rule that decides whether a shim may be started
// with no --resume, and the proof object that makes the rule structural rather
// than a convention.
//
// THE RULING. A workspace's Claude conversation is not replaceable. No
// frontend, no user command, and no automatic recovery may abandon one: the
// abandonment is silent and irreversible, while every alternative — resume it,
// restore it, or refuse loudly — is recoverable. A fresh conversation may
// therefore exist for exactly one kind of workspace: one the daemon can PROVE
// never had a conversation at all.
//
// WHY A PROOF OBJECT AND NOT A BOOLEAN. "Start fresh" used to be reachable
// from four unrelated places — a retired wire enum, an editor command, a
// client-side degrade on a lost transcript, and a bare `return "", nil` when a
// resolver found nothing — and each of them was one `if` away from every
// ordinary create. A bool parameter would have been the fifth. freshEligibility
// has no exported name, no literal any caller outside this package can write,
// and exactly one constructor (proveFreshEligible), so the ONLY way to reach
// the no-resume spawn is to have gone through the evidence check. Adding a new
// fresh path is not a matter of passing true; it requires minting a proof, and
// there is one place that can.
//
// THE EVIDENCE. registry.Record.LastTurnEndMs is the authority, and it is the
// same one resumeTargetCarriesAConversation already uses for the handshake-only
// waiver. It is written by the turn-boundary sink when a turn ENDS, it is
// durable precisely so it survives the restarts an in-memory flag would not,
// and zero means no turn has ever ended under that record. It is deliberately
// NOT re-derived from the file plane or the event store: a second definition of
// "has this workspace ever said anything" is how two subsystems come to
// disagree about one workspace, and the disagreement would be resolved in
// favor of destroying a conversation.
//
// AMBIGUITY IS NOT PERMISSION. Evidence that cannot be consulted at all (no
// registry) yields no proof. "I cannot tell" and "I proved it never happened"
// are different answers, and only the second one may start a blank
// conversation.
package server

import (
	"fmt"
	"sort"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// freshEligibility is PROOF that a workspace may be brought up with no
// conversation to resume. It carries no data — its existence IS the fact — and
// its unexported empty-struct field means no package outside this one can
// construct it even by writing a composite literal.
//
// It is always passed as a POINTER. A value type would have a usable zero
// value, and a zero value that means "proved" is not a proof.
type freshEligibility struct{ _ struct{} }

// conversationEvidence is the daemon's durable answer to "has this workspace
// ever had a Claude conversation?", together with what it is able to say about
// it. Every field is needed at the refusal site: a user told only "refusing"
// cannot act, where a user told which uuids the daemon is protecting can.
type conversationEvidence struct {
	// available reports whether the evidence could be consulted at all. False
	// means the daemon has no registry to ask, which is NOT proof of absence.
	available bool
	// everRan is true iff some record at this location names a vendor
	// conversation AND a turn has ended under it.
	everRan bool
	// conversations is every vendor uuid the registry holds for this location,
	// sorted, so the refusal names what it is protecting.
	conversations []string
}

// gatherConversationEvidence reads the registry for everything it knows about
// (configDir, cwd). It is the single evidence reader; both the create
// resolution path and the respawn gate call THIS, so the two cannot come to
// different conclusions about one workspace.
func gatherConversationEvidence(reg *registry.Registry, configDir, cwd string) conversationEvidence {
	if reg == nil || cwd == "" {
		return conversationEvidence{}
	}
	ev := conversationEvidence{available: true}
	seen := make(map[string]bool)
	for _, rec := range reg.All() {
		if rec.CWD != cwd || rec.ConfigDir != configDir || rec.ClaudeSessionID == "" {
			continue
		}
		if !seen[rec.ClaudeSessionID] {
			seen[rec.ClaudeSessionID] = true
			ev.conversations = append(ev.conversations, rec.ClaudeSessionID)
		}
		// ONE record proving a turn ran is enough. A conversation is not
		// un-had by a later record that never spoke.
		if resumeTargetCarriesAConversation(rec) {
			ev.everRan = true
		}
	}
	// Checkpoints contribute NAMES ONLY, never the verdict. A checkpoint is
	// derived from any record carrying a complete conversation identity — which
	// includes a record whose uuid the vendor minted at system:init and under
	// which nothing was ever said — so letting one vote for everRan would
	// silently retire the handshake-only waiver. LastTurnEndMs stays the single
	// authority; the checkpoints are here so a refusal can name a conversation
	// whose record has since been pruned.
	for _, cp := range reg.AllCheckpoints() {
		if cp.CWD != cwd || cp.ConfigDir != configDir || cp.ClaudeSessionID == "" {
			continue
		}
		if !seen[cp.ClaudeSessionID] {
			seen[cp.ClaudeSessionID] = true
			ev.conversations = append(ev.conversations, cp.ClaudeSessionID)
		}
	}
	sort.Strings(ev.conversations)
	return ev
}

// proveFreshEligible is the ONLY constructor of freshEligibility. It returns a
// proof exactly when the evidence was consultable AND says no conversation has
// ever run at this location; otherwise it returns nil and the reason, which is
// what the caller turns into the hard fault.
func proveFreshEligible(ev conversationEvidence) (*freshEligibility, string) {
	if !ev.available {
		return nil, "the daemon could not consult its session registry, so it cannot prove this workspace never had a conversation"
	}
	if ev.everRan {
		return nil, fmt.Sprintf("this workspace HAS run a conversation (%s)", describeConversations(ev.conversations))
	}
	return &freshEligibility{}, ""
}

// describeConversations renders the uuids for a human. Empty is said out loud
// rather than rendered as an empty list, because "a conversation exists but I
// can name none of it" is itself information.
func describeConversations(uuids []string) string {
	if len(uuids) == 0 {
		return "no uuid is recorded for it"
	}
	out := "uuid=" + uuids[0]
	for _, u := range uuids[1:] {
		out += ", uuid=" + u
	}
	return out
}

// spawnShim is the daemon's single shim-launch site and the one place a
// fresh-conversation proof is REDEEMED.
//
// The proof is a PARAMETER rather than a field on CreateOpts or a flag on the
// spawner because a parameter cannot be forgotten: every caller must decide
// what to pass, and the only non-nil value in existence comes from
// proveFreshEligible. A no-resume spawn without one is a gap in this file's own
// ladder, so it fails loudly here rather than starting the blank conversation
// the ladder exists to prevent.
func (s *ShimSpawner) spawnShim(sessionID string, opts CreateOpts, fresh *freshEligibility) (ShimHandle, error) {
	if opts.Resume == "" && fresh == nil && !opts.Fake {
		return ShimHandle{}, fmt.Errorf("%w: session %s (cwd=%s) reached the spawn with no conversation to resume and no proof that this workspace never had one — this is a gap in the resume ladder, not a fresh workspace",
			errclass.ErrConversationUnresumable, sessionID, opts.CWD)
	}
	return s.spawn(sessionID, opts)
}

// unresumableConversation builds the resume ladder's terminal fault. It is the
// only construction of that error, so every rung that reaches the bottom says
// the same thing.
func unresumableConversation(cwd, configDir, reason string) error {
	return fmt.Errorf("%w: cwd=%s config_dir=%s: %s; resume it, restore its transcript, or delete the session deliberately — no blank conversation will be started in its place",
		errclass.ErrConversationUnresumable, cwd, configDir, reason)
}
