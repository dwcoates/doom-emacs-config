package convert

// user.go — A `user` RECORD IS FOUR DIFFERENT THINGS wearing one type tag: a
// person's prompt, a tool's result the vendor filed under the user, the
// harness's own compaction summary, and the expanded `/clear` envelope.
//
// R15: AGENT-REPL'S OWN FILE-PLANE PROMPT IS NEVER A PAGE LINE. Its AgentPrompt
// carries a TurnId and a PromptOrigin, both DAEMON-MINTED — the daemon drew the
// bubble live and the shim's stream-plane AgentPrompt is the one served form —
// so the file-plane copy is classified as vendor_specific: durable and
// investigable without regrowing the bubble a second time. Such a prompt is
// recognized by an SDK entrypoint (`sdk-cli`, `sdk-ts`, …): see isSDKEntrypoint.
//
// AN ADOPTED EXTERNAL PROMPT IS EMITTED (owner-approved R15 crossing). A prompt
// typed in interactive Claude Code (any non-SDK entrypoint, e.g. "cli") was
// never submitted through agent-repl, so the daemon never minted or drew it;
// withholding it left an adopted conversation showing answers with no prompts.
// It is emitted here as a real prompt page line on a STABLE identity derived
// from the record's own uuid, so replay is idempotent. See humanPrompt.

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// userLine converts a `user` record.
func (c *Converter) userLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	env := readEnvelope(record)
	message := obj(record["message"])
	agent := c.frameAgent(at, env)

	// Tool results are lifted out of the user's content and become the settled
	// state of the units that MADE the calls.
	out := c.toolReturns(record, message, at, env, agent)

	switch {
	case env.isSummary:
		// The harness's own prose standing in for discarded history. It is
		// CONSUMED by the compaction boundary that precedes it, so emitting it
		// here would render the summary twice and attribute the harness's text
		// to the person.
		//
		// CONSUMED HERE TOO WHEN IT WAS NOT THE NEXT LINE. The boundary's own
		// lookahead reaches exactly one record, and the harness does not always
		// write the summary there — an observed transcript put a
		// `system/scheduled_task_fire` between them. A summary that names a
		// boundary this converter drew with the placeholder supersedes that
		// draw on the cut's own key rather than being dropped.
		if attached := c.attachCompactSummary(record, at); attached != nil {
			return append(out, attached)
		}
		c.log.With(at.ctxFor("compact-summary")).
			LogVerbose("compaction summary folded into its boundary")
		return out
	case c.isClearCommand(message):
		return append(out, c.contextCleared(record, at, env, agent))
	case c.skillDocument(record, at, env, agent, &out):
		// The skill's document landed and settled its invocation.
		return out
	case hasToolResults(message):
		// A carrier for tool results, already lifted above. The vendor files
		// results under the user's role; emitting an empty prompt beside them
		// would put words in a person's mouth.
		return out
	case env.originKind == "peer":
		// A MESSAGE FROM ANOTHER CLAUDE — an inter-session peer message or a
		// subagent hand-back (`origin.handback` set). It carries `isMeta:true`,
		// so it MUST be recognized BEFORE the isMeta withhold below, which would
		// otherwise make it vanish from an adopted conversation. Emitted as the
		// one peer-message page line on the record's own uuid — the same uuid the
		// live stream keys on, so the two planes' rows collapse to one.
		return append(out, c.peerMessage(record, message, at, env, agent))
	case env.isMeta:
		// A harness-injected user record: a system reminder, an attachment
		// carrier. Not something a person said. The local-command caveat among
		// them is remembered, because it names the record after it a command
		// the CLI answers itself (bookkeeping.go).
		c.noteLocalCommandCaveat(record, env)
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("harness-injected user record withheld as vendor_specific")
		return append(out, VendorSpecificEntry(at, "user/meta", record))
	default:
		// NOT EVERY PROMPT-SHAPED RECORD IS SOMETHING A PERSON TYPED: the
		// vendor's slash-command bookkeeping, a local command's output, the
		// interrupt marker and a background task's notification all land here,
		// and each goes to the arm the stream plane uses for the same fact.
		if entries, handled := c.notTypedByPerson(record, message, at, env, agent); handled {
			return append(out, entries...)
		}
		// A GENUINE HUMAN PROMPT — not a summary, clear, skill, tool-result
		// carrier, meta record, or bookkeeping.
		return append(out, c.humanPrompt(record, message, at, env, agent))
	}
}

// sdkEntrypointPrefix begins every `entrypoint` an SDK-hosted session stamps on
// the prompts it submits, and agent-repl's OWN sessions are SDK-hosted.
// Interactive Claude Code stamps `"cli"`; the discriminator is the same field
// that tells an SDK transcript from an interactive one.
//
// IT IS A PREFIX, NOT ONE VALUE, because the suffix names the SDK's host and
// changes with it: the CLI-hosted SDK stamped `sdk-cli`, the TypeScript Agent SDK
// stamps `sdk-ts`. Matching only `sdk-cli` let every `sdk-ts` prompt through as
// an adopted external one, and the daemon drew each of agent-repl's own prompts
// twice: once live under its TurnId, once again under the record's uuid.
const sdkEntrypointPrefix = "sdk-"

// isSDKEntrypoint reports whether a record's `entrypoint` was stamped by an
// SDK-hosted session, i.e. agent-repl's own.
func isSDKEntrypoint(entrypoint string) bool {
	return strings.HasPrefix(entrypoint, sdkEntrypointPrefix)
}

// humanPrompt decides what becomes of a genuine human file-plane prompt.
//
// THE RULE IS "A TOP-LEVEL PROMPT THE DAEMON NEVER DREW IS EMITTED; every other
// prompt is withheld." R15 withholds because the daemon already draws the
// prompt, and it draws two kinds:
//
//   - AGENT-REPL'S OWN PROMPTS carry an SDK entrypoint (isSDKEntrypoint): the daemon
//     minted the TurnId and PromptOrigin and drew the bubble live, and the shim's
//     stream-plane AgentPrompt is the served form. Withheld to avoid a
//     double-render, whatever plane it is read on. New prompts typed after
//     adoption are agent-repl's own and carry an SDK entrypoint, so they remain withheld.
//   - A SUBAGENT COMMISSION is the opening user message of a sidechain
//     transcript — an agent-addressed prompt the daemon draws at BOTH ends
//     (sender's feed and recipient's). It carries `entrypoint == "cli"` like an
//     interactive prompt, so `isSidechain` is what tells the two apart. Withheld
//     for the same double-render reason.
//
// A TOP-LEVEL prompt with any non-SDK entrypoint (`"cli"`, or a future non-sdk
// value) was typed in an EXTERNAL interactive session and adopted into a
// workspace — never submitted through agent-repl, so the daemon never minted or
// drew it. Withholding it is why an adopted conversation showed the assistant's
// answers with no prompts above them. It is emitted here as a real prompt page
// line on a STABLE identity derived from the record's own uuid (externalPrompt),
// so a re-ingest supersedes its own row rather than growing a second bubble.
func (c *Converter) humanPrompt(record, message map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	if entrypoint := str(record["entrypoint"]); isSDKEntrypoint(entrypoint) {
		c.log.With(at.ctxFor("user-prompt")).
			LogVerbose("file-plane user prompt withheld as vendor_specific (R15: agent-repl's own SDK prompt (entrypoint=%q) is daemon-minted and drawn live)", entrypoint)
		return VendorSpecificEntry(at, "user_prompt", record)
	}
	if boolean(record["isSidechain"]) {
		c.log.With(at.ctxFor("user-prompt")).
			LogVerbose("file-plane user prompt withheld as vendor_specific (R15: a sidechain's opening commission is an agent-addressed prompt the daemon draws at both ends)")
		return VendorSpecificEntry(at, "user_prompt", record)
	}
	return c.externalPrompt(record, message, at, env, agent)
}

// externalPrompt emits an adopted external transcript's human prompt as a served
// prompt page line, MINTING A STABLE IDENTITY for it from the record's uuid.
//
// THE UUID IS THE IDENTITY, AND THAT IS WHY REPLAY IS IDEMPOTENT. These historic
// prompts have no daemon-minted TurnId, so one is derived from the vendor's own
// per-record uuid — stable and unique per record — and spelled into both the
// turn id and the upsert key (PromptKey, the shim's own space). Re-ingesting the
// same record mints the identical turn id, the identical upsert key and the
// identical write id, so it supersedes its own row instead of appending a
// second bubble. AN EDITED OR RE-SENT VERSION of a still-unanswered prompt is
// the one exception: it takes the FIRST version's uuid, so it supersedes that
// row rather than drawing beside it (resend.go). The recipient is the frame's
// agent (the main agent for a session transcript), which the store requires to
// match the book.
//
// THE ORIGIN IS LEFT UNSPECIFIED, which the daemon draws as the plain "You"
// author label — the same bubble a person's own prompt gets. The closed
// PromptOrigin vocabulary has no send site for an externally-typed prompt (each
// value names exactly one agent-repl send site), and inventing one is a proto
// design decision left to the owner; UNSPECIFIED is the honest "not from an
// agent-repl send site" and renders identically to a human prompt.
func (c *Converter) externalPrompt(record, message map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	said := userSaid(message)
	// AN EDITED OR RE-SENT VERSION IS EMITTED ON ITS SET'S ROW (resend.go): the
	// first version's uuid names the turn and the key, so this write supersedes
	// the words the row held instead of drawing a second bubble.
	turn, joined := c.joinPromptSet(env.uuid, str(record["parentUuid"]), agent, said)
	// THIS RECORD OPENS THE TURN, and every record of it is stamped with it.
	c.openedTurn = turn
	prompt := &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  agentID(agent),
		Said:   said,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
	}
	if joined {
		c.log.With(at.ctxFor("prompt-resend")).With(logging.Context{UpsertKey: PromptKey(turn)}).
			Log("an edited or re-sent prompt shares an unanswered version's parent; it supersedes that version's row instead of drawing a second one")
	} else {
		c.log.With(at.ctxFor("user-prompt")).With(logging.Context{UpsertKey: PromptKey(turn)}).
			LogVerbose("adopted external prompt emitted as a page line on a uuid-derived identity (entrypoint=%q, not agent-repl's own SDK entrypoint)", str(record["entrypoint"]))
	}
	return c.landPrompt(at, agent, PromptKey(turn), "agent_prompt", prompt)
}

// peerMessage emits a message another Claude session sent into this
// conversation (an inter-session peer, or a subagent hand-back) as a served peer
// page line, keyed on the record's own uuid.
//
// THE UUID IS THE IDENTITY, AND IT IS THE CROSS-PLANE KEY. The live stream keys
// the very same vendor record `peer:<uuid>` and spells the uuid into
// PeerMessage.id, so a running session that already drew this message and this
// adopted copy supersede each other on one row rather than drawing two. The
// sender is `origin.from`/`origin.senderTaskId`; the body is `origin.body` when
// the vendor states it, else the record's own text (envelope and all), so
// nothing the sender wrote is lost. The recipient is the frame's agent — the
// main agent for a session transcript — which the store requires to match the
// book.
func (c *Converter) peerMessage(record, message map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	body := env.peerBody
	if body == "" {
		body = firstText(message)
	}
	peer := &conversationv1.PeerMessage{
		Agent:  agentID(agent),
		Sender: env.peerSender,
		Body:   body,
		Id:     env.uuid,
	}
	// THE KIND IS THE VENDOR'S OWN MARKING, read exactly as the stream plane
	// reads it (shim/src/convert/peer.ts peerKindOf): `origin.handback` is a
	// subagent's hand-back, anything else another session's message.
	if env.peerHandback {
		peer.Kind = &conversationv1.PeerMessage_SubagentHandback{SubagentHandback: &conversationv1.PeerMessageSubagentHandback{}}
	} else {
		peer.Kind = &conversationv1.PeerMessage_InterSession{InterSession: &conversationv1.PeerMessageInterSession{}}
	}
	c.log.With(at.ctxFor("peer-message")).With(logging.Context{UpsertKey: PeerKey(env.uuid)}).
		LogVerbose("a peer message (origin.kind=peer, sender=%q, handback=%t) emitted as a page line on the record uuid", env.peerSender, env.peerHandback)
	return c.landPeerMessage(at, agent, PeerKey(env.uuid), "peer_message", peer)
}

// userSaid builds the one canonical prompt form from a vendor user message: its
// text and images as UserContent blocks, in the order the person composed them.
//
// A block kind this schema does not model is kept WHOLE on the unsupported arm
// rather than dropped, so nothing a person sent vanishes and the decision to not
// model it stays reversible from stored data.
func userSaid(message map[string]any) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: userContentBlocks(message)}}
}

// userContentBlocks renders a user message's content into UserContentBlocks. The
// vendor writes content either as a bare string or as a block list; both are
// carried here, and a string becomes one text block.
func userContentBlocks(message map[string]any) []*conversationv1.UserContentBlock {
	switch content := message["content"].(type) {
	case string:
		return []*conversationv1.UserContentBlock{textContentBlock(content)}
	case []any:
		blocks := make([]*conversationv1.UserContentBlock, 0, len(content))
		for _, raw := range content {
			block := obj(raw)
			if block == nil {
				continue
			}
			switch str(block["type"]) {
			case "text":
				blocks = append(blocks, textContentBlock(str(block["text"])))
			case "image":
				blocks = append(blocks, &conversationv1.UserContentBlock{
					Block: &conversationv1.UserContentBlock_Image{Image: imageBlock(block)},
				})
			default:
				blocks = append(blocks, &conversationv1.UserContentBlock{
					Block: &conversationv1.UserContentBlock_Unsupported{Unsupported: &conversationv1.UnsupportedBlock{
						Kind: str(block["type"]),
						Raw:  rawStruct(block),
					}},
				})
			}
		}
		return blocks
	default:
		return nil
	}
}

// textContentBlock wraps words a person typed as a text UserContentBlock.
func textContentBlock(text string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
	}
}

// hasToolResults reports whether a user message carries any tool result at all,
// which is what makes it a carrier rather than something a person said.
func hasToolResults(message map[string]any) bool {
	for _, raw := range list(message["content"]) {
		block := obj(raw)
		if block != nil && resultBlockTypes[str(block["type"])] {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// the skill document
// ---------------------------------------------------------------------------

// rememberSkillCall notes a skill invocation so the document that arrives later
// settles it. The join is the vendor's own `sourceToolUseID`, which is DIRECT
// AND STRUCTURAL — never a skill-name map matched against whatever arrives next.
func (c *Converter) rememberSkillCall(call openCall) {
	c.openSkills[call.activityID] = call
}

// skillDocument settles a skill invocation from the isMeta user record that
// carries its body, joined by `sourceToolUseID`.
//
// Returns true when this record WAS a skill document, so the caller does not
// also classify it as a prompt.
func (c *Converter) skillDocument(record map[string]any, at Attribution, env envelope, agent string, out *[]*storev1.StoreEntry) bool {
	if env.sourceTool == "" {
		return false
	}
	call, ok := c.openSkills[env.sourceTool]
	if !ok {
		return false
	}
	delete(c.openSkills, env.sourceTool)

	markdown := firstText(obj(record["message"]))
	c.log.With(at.ctxFor("skill-document")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID)}).
		LogVerbose("skill document (%d characters) settles its invocation", len(markdown))

	activity := c.skillSettled(call, markdown, env.timestampMs)
	activity.ActivityId = activityID(call.activityID)
	*out = append(*out, c.settledEntry(at, agent, call.activityID, activity))
	return true
}
