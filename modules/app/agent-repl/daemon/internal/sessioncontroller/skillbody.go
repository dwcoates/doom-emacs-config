package sessioncontroller

import (
	"strings"
	"sync"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/frontend"
)

// THE DEFECT THIS FILE CURATES AWAY.
//
// Launching a skill writes THREE things to the transcript: the `Skill` tool
// call, its tool_result, and then a SEPARATE record of type "user" — flagged
// isMeta — whose text is "Base directory for this skill: <dir>" followed by the
// entire SKILL.md. That third record is the skill's body being handed to the
// model. It is not a prompt, nobody typed it, and rendering it as a user turn
// draws a page-long prompt bubble the user never wrote.
//
// It is curated here into the Message skill_body arm addressed to the
// Skill call's tool_use_id, so ONE card carries the invocation, its result and
// its body, updated in place as each lands.
//
// WHY isMeta ALONE IS NOT THE RULE. The harness writes several other isMeta
// "user" records that are equally not prompts and equally not skill bodies —
// a "(Re-invocation of /<skill> …)" notice, a "Continue from where you left
// off." nudge after an AskUserQuestion. Attaching the first isMeta record that
// follows a Skill call would file the re-invocation notice as the body and then
// have nowhere to put the body itself. So every isMeta user record is withheld
// from the feed, and only the ones that are demonstrably bodies are attached.

// skillBodyMarker is the head the harness prepends to a launched skill's body,
// naming the directory the SKILL.md was read from. It is the one thing that
// distinguishes a body from the harness's other isMeta records, all of which
// can hang off the very same Skill call.
const skillBodyMarker = "Base directory for this skill:"

// isSkillBodyText reports whether one isMeta record's text is a skill body.
//
// HEAD ONLY, after leading whitespace, for the same reason the slash-command
// predicate matches on the head (machinery.go): the marker is what the harness
// PREPENDS, and a SKILL.md that merely discusses the phrase further down is a
// body of some other skill, not this one's header.
func isSkillBodyText(text string) bool {
	return strings.HasPrefix(strings.TrimLeft(text, " \t\r\n"), skillBodyMarker)
}

// skillCorrelator answers "which Skill call caused this record?" from the
// record linkage the transcript already carries.
//
// THE CORRELATION RULE, and why it is this one. The harness threads its
// synthetic records onto the record they answer via parentUuid, and a skill's
// body is written as a child of the Skill call's TOOL_RESULT record. So the
// chain is exact and needs no proximity heuristic:
//
//	assistant record ─ tool_use  id=T name=Skill
//	user record  U1  ─ tool_result tool_use_id=T
//	user record  U2  ─ isMeta, parentUuid=U1, text="Base directory for this skill: …"
//
// A "nearest preceding Skill call" rule would agree with this one in the easy
// case and diverge in exactly the cases that matter: concurrent or nested
// skills, and a re-invocation whose notice and body are two separate records
// hanging off one call. The chain is also what makes an out-of-order body
// STRUCTURALLY unattachable rather than mis-attachable — a body whose parent
// has not been seen resolves to nothing and is withheld, instead of silently
// landing on whichever card happened to be most recent.
//
// The chain is followed THROUGH isMeta records (U2's own children resolve to T
// as well), because the harness does chain them: a re-invocation notice is the
// tool_result's child and the body is the NOTICE's child.
//
// WHAT IT REMEMBERS. Only skill-related uuids: the Skill call ids, and the
// records that descend from one. Everything else is dropped on the floor, so
// the maps stay proportional to the number of skill invocations rather than to
// the length of the conversation.
type skillCorrelator struct {
	mu sync.Mutex
	// calls is the set of tool_use ids of the Skill calls seen this session.
	calls map[string]struct{}
	// chain maps a record uuid to the Skill call its record chain descends
	// from.
	chain map[string]string
}

func newSkillCorrelator() *skillCorrelator {
	return &skillCorrelator{calls: map[string]struct{}{}, chain: map[string]string{}}
}

// skillToolName is the harness's tool for launching a skill. It is the
// frontend apparatus's own constant rather than a second spelling of the same
// string: the merge classifier (frontend/mergeskill.go) has to find its
// invocation among exactly the calls this correlator files, and two literals
// could drift apart.
const skillToolName = frontend.SkillToolName

// observe records whatever skill linkage one curated item establishes: a Skill
// call, or a record answering one.
func (s *skillCorrelator) observe(it *frontendv1.Message) {
	s.mu.Lock()
	defer s.mu.Unlock()
	for id, name := range toolUseCalls(it) {
		if name == skillToolName {
			s.calls[id] = struct{}{}
		}
	}
	for _, id := range toolResultIDs(it) {
		if _, ok := s.calls[id]; ok {
			s.chain[it.GetUuid()] = id
		}
	}
}

// resolve reports the Skill call a record with this parent descends from, or ""
// when the parent is not part of any skill's chain.
func (s *skillCorrelator) resolve(parentUUID string) string {
	if parentUUID == "" {
		return ""
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.chain[parentUUID]
}

// extend files a record as part of a Skill call's chain, so records hanging off
// IT resolve to the same call.
func (s *skillCorrelator) extend(uuid, toolUseID string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.chain[uuid] = toolUseID
}

// reset forgets every correlation. Called on a session ROTATION, where the
// vendor retires the conversation uuid: the records the maps name belong to a
// conversation nothing in the new one refers to (see purgeRetained).
func (s *skillCorrelator) reset() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = map[string]struct{}{}
	s.chain = map[string]string{}
}

// curateMetaRecords replaces each isMeta "user" item in a curated delta with
// the skill card it belongs to, hands it to the skill's own work, or withholds
// it. It returns whatever async push the work deliveries produced.
//
// FOUR OUTCOMES, all loud:
//   - an isMeta record carrying the body marker whose Skill call OPENED A SKILL
//     DETACHED WORK becomes that work's own body and leaves the feed entirely.
//   - an isMeta record that resolves to a Skill call AND carries the body
//     marker, whose call opened no skill work, becomes a skill_body item
//     addressed to that call.
//   - any other isMeta record is withheld from the feed entirely.
//   - a non-isMeta record is untouched.
//
// THE BODY HAS EXACTLY ONE HOME. async-work.proto puts a skill's contents on
// DetachedWorkSkill.body and retires the old rendering in the same breath, so a
// call with a work emits NO skill_body card: two homes for one document would
// draw the whole SKILL.md twice. The card path remains for the invocations that
// open no skill work — the merge run, whose work has no body field, and a
// Skill call the daemon could not classify.
//
// WITHHELD, NOT DELETED, exactly as in machinery.go: the store keeps every
// record faithfully, and what is suppressed is only the rendered item. The
// delta is still pushed so its through_seq advances the frontend's cursor.
//
// IDEMPOTENT ON REPLAY. The skill_body item's identity is the tool_use_id it
// addresses, which a replay derives from the same chain and therefore resolves
// identically, so a replayed body REPLACES the body already on the card rather
// than adding a second one. The correlator is cumulative for the same reason —
// a re-pull of old events finds the chain it already learned.
//
// Runs with the other user-record curators, like withholdMachinery and for the
// same reason: a record nobody typed must never reach the feed as a prompt.
func (c *consumer) curateMetaRecords(cd *frontendv1.ConversationDelta, envs map[string]frontend.RecordEnvelope) asyncPush {
	var push asyncPush
	items := cd.GetMessages()
	kept := items[:0]
	for _, it := range items {
		c.skills.observe(it)
		env, ok := envs[it.GetUuid()]
		// THE local_command SHAPE IS ISMETA AND IS NOT THIS CURATOR'S. The
		// harness flags its `system`/`local_command` slash-command record
		// isMeta like every other record it wrote for the model, but that
		// record has its own classifier downstream (machinery.go), which turns
		// it into the feed's account of the command the user ran. Withholding
		// it here as an uncorrelated meta record would delete the fact before
		// anything could state it.
		if !ok || !env.IsMeta || env.LocalCommandSubtype {
			kept = append(kept, it)
			continue
		}
		text := userRecordText(it)
		toolUseID := c.skills.resolve(env.ParentUUID)
		if toolUseID != "" {
			// Whatever this record is, records hanging off it belong to the
			// same skill — the harness chains a body onto a re-invocation
			// notice onto the tool_result.
			c.skills.extend(it.GetUuid(), toolUseID)
		}
		if toolUseID != "" && isSkillBodyText(text) {
			if messageID := c.work.skillWindowMessageID(toolUseID); messageID != "" {
				// THE DETACHED WORK'S OWN BODY, AND NOTHING ON THE FEED. The contents
				// belong to the skill, not to the conversation, so they land on
				// the work and the card the old rendering drew is not emitted.
				c.logf("session-controller: skill body DELIVERED to its work ws=%q session=%s seq=%d uuid=%s tool_use_id=%s work=%s len=%d — the card rendering of these contents is retired by the contract's own arm",
					c.workspace, c.sessionID, cd.GetThroughSeq(), it.GetUuid(), toolUseID, messageID, len(text))
				push.absorb(c.resolveSkillBodyIntoWindow(toolUseID, text, cd.GetThroughSeq()))
				continue
			}
			c.logf("session-controller: skill body ATTACHED to its card ws=%q session=%s seq=%d uuid=%s tool_use_id=%s len=%d",
				c.workspace, c.sessionID, cd.GetThroughSeq(), it.GetUuid(), toolUseID, len(text))
			// THE CARD IS DURABLE, and it says so through the constructor. It
			// re-payloads a record the CLI wrote and the store holds, under
			// that record's own uuid, so a reload serves it again — which is
			// precisely the claim the durable arm makes.
			card, err := frontend.NewDurableFeedRow(&frontendv1.Message{
				Uuid:      it.GetUuid(),
				TsMs:      it.GetTsMs(),
				RequestId: it.GetRequestId(),
				Payload: &frontendv1.Message_Agent{Agent: &frontendv1.AgentEmission{
					Emission: &frontendv1.AgentEmission_SkillBody{SkillBody: &frontendv1.SkillBodyItem{
						ToolUseId:    toolUseID,
						BodyMarkdown: text,
					}},
				}},
			})
			if err != nil {
				// WITHHELD, LOUDLY. An item with no durability arm reads later
				// as a durable message the store lost, so the card is kept off
				// the feed and the refusal reported in full rather than
				// repaired or published unclassified.
				c.warn("session-controller: skill body card NOT drawn ws=%q session=%s seq=%d uuid=%s tool_use_id=%s — the message constructor refused it, so no classified card could be built and none is delivered: %v",
					c.workspace, c.sessionID, cd.GetThroughSeq(), it.GetUuid(), toolUseID, err)
				continue
			}
			kept = append(kept, card)
			continue
		}
		c.logf("session-controller: user turn WITHHELD as harness meta record ws=%q session=%s seq=%d uuid=%s parent=%s skill=%q head=%q — the harness flagged this record isMeta, meaning it wrote it FOR THE MODEL rather than a person typing it; the store keeps it, the conversation feed does not",
			c.workspace, c.sessionID, cd.GetThroughSeq(), it.GetUuid(), env.ParentUUID, toolUseID, head(text))
	}
	cd.Messages = kept
	return push
}

// head caps a record body for a log line, so withholding a page-long SKILL.md
// that failed to correlate still reports WHAT it withheld without pasting the
// whole document into the daemon log.
func head(text string) string {
	const max = 80
	flat := strings.ReplaceAll(strings.TrimLeft(text, " \t\r\n"), "\n", " ")
	if len(flat) <= max {
		return flat
	}
	return flat[:max] + "…"
}
