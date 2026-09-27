package convert

// resend.go — AN EDITED OR RE-SENT PROMPT IS ONE ROW, holding the version the
// conversation continued from (owner ruling 2026-09-27: the feed shows ONLY the
// version that got the answer).
//
// WHAT THE VENDOR WRITES. The transcript is a tree, every record naming its
// parent in `parentUuid`. When a person submits a prompt and, before any answer,
// edits it or presses enter on it again, the CLI writes a NEW user record — its
// own uuid, promptId and timestamp — whose parent is the SAME record the first
// version's was, and it keeps the abandoned version in the file. The answer and
// everything after it descend from the new version. An observed book held 24
// such sets in one transcript (two to four versions each, identical or edited
// text, the abandoned ones followed only by their attachments and the odd local
// command); drawing each record as its own prompt put every abandoned version in
// the feed beside the one that was answered.
//
// THE RULE, read off the transcript's own links and never arrival order alone:
//
//   - The newest emitted external prompt is remembered as an UNDECIDED SIBLING
//     SET until an ASSISTANT record descends from one of its members. Nothing
//     but an answer decides it: attachments, local commands and harness
//     bookkeeping land under an abandoned version too.
//   - A new external prompt whose parent is the undecided set's parent JOINS
//     the set. It is emitted on the set's ROW — the first member's upsert key
//     and turn — so its write supersedes the words the row held. There is never
//     a second row, so no reader, live or replaying, can draw two bubbles for
//     one prompt: a live re-send REPLACES the pending bubble in place.
//   - The first assistant record descending from a member DECIDES the set. When
//     that member is not the one whose words the row holds (an EARLIER version
//     got the answer), the row is written again with that member's words, on the
//     answer record's own file coordinates.
//   - A prompt whose parent differs, or that arrives after the set was decided —
//     a rewind that edits an ALREADY ANSWERED prompt — opens a row of its own:
//     each answered version keeps the prompt it answered, because an answer
//     drawn under no prompt, or under another version's words, would misstate
//     what was asked.
//   - A PARENTLESS prompt never joins a set: with no parent there is no sibling
//     relation to read, and the file's opening prompt is not an edit of anything.
//
// NOTHING IS DELETED. Every version's record is read, and each write lands in
// the store's ledger; only the one row's content is superseded, as any upsert
// supersedes it.
//
// WHAT IT HOLDS: at most one set — its members' words and the uuids of the
// records descending from them — and only from the set's first prompt until its
// answer. The branch map is a single indexed lookup per record, never a lineage
// walk; the decision empties it.
//
// A RESTART LOSES NOTHING THE RULE NEEDS. The boot rewind re-reads the file from
// its last turn start, which is the newest prompt, so an undecided set's newest
// member is re-read and re-remembered, and every write it re-emits carries the
// identical write_id the store already absorbed. What a restart cannot recover
// is an EARLIER member the rewind stopped short of; an answer descending from
// it is then read as descending from no member, and the row keeps the newest
// member's words.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// answeredPromptDiscriminator separates the re-emitted row from every other
// entry the deciding answer record mints, so its write_id is its own.
const answeredPromptDiscriminator = "answered_prompt"

// siblingSet is one undecided set of prompt versions sharing a parent.
type siblingSet struct {
	// parent is the parentUuid every member shares.
	parent string
	// turn is the set's row identity: the FIRST member's uuid, spelled into the
	// turn id and the upsert key exactly as a lone prompt's own uuid is.
	turn string
	// agent is the book the row is filed in.
	agent string
	// said maps each member's uuid to its words.
	said map[string]*conversationv1.UserSaid
	// drawn is the member whose words the row holds now.
	drawn string
	// branch maps a record uuid to the member it descends from, the members
	// themselves included.
	branch map[string]string
}

// joinPromptSet decides the row an external prompt is emitted on and records
// the prompt as a member of the undecided set it opens or joins. It returns the
// row's turn and whether the prompt JOINED an existing set.
func (c *Converter) joinPromptSet(uuid, parent, agent string, said *conversationv1.UserSaid) (string, bool) {
	set := c.promptSet
	if set != nil && parent != "" && parent == set.parent && agent == set.agent {
		set.said[uuid] = said
		set.drawn = uuid
		set.branch[uuid] = uuid
		return set.turn, true
	}
	c.promptSet = nil
	if parent == "" {
		return uuid, false
	}
	c.promptSet = &siblingSet{
		parent: parent,
		turn:   uuid,
		agent:  agent,
		said:   map[string]*conversationv1.UserSaid{uuid: said},
		drawn:  uuid,
		branch: map[string]string{uuid: uuid},
	}
	return uuid, false
}

// followPromptBranch reads one record's link into the undecided set: a record
// descending from a member joins that member's branch, and an ASSISTANT record
// descending from one decides the set. It returns the row re-written with the
// answered member's words when the row held another member's, and nothing
// otherwise.
func (c *Converter) followPromptBranch(facts keepaliveFacts, at Attribution) []*storev1.StoreEntry {
	set := c.promptSet
	if set == nil || facts.parent == "" {
		return nil
	}
	member, descends := set.branch[facts.parent]
	if !descends {
		return nil
	}
	if facts.kind != "assistant" {
		if facts.uuid != "" {
			set.branch[facts.uuid] = member
		}
		return nil
	}
	c.promptSet = nil
	if member == set.drawn {
		c.log.With(at.ctxFor("prompt-resend")).With(logging.Context{UpsertKey: PromptKey(set.turn)}).
			LogVerbose("the prompt row's version got the answer; %d version(s) share its parent", len(set.said))
		return nil
	}
	c.log.With(at.ctxFor("prompt-resend")).With(logging.Context{UpsertKey: PromptKey(set.turn)}).
		Log("an earlier version of a re-sent prompt got the answer; its row is re-written with that version's words (%d version(s) share its parent)", len(set.said))
	prompt := &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: set.turn},
		Agent:  agentID(set.agent),
		Said:   set.said[member],
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
	}
	return []*storev1.StoreEntry{c.landPrompt(at, set.agent, PromptKey(set.turn), answeredPromptDiscriminator, prompt)}
}
