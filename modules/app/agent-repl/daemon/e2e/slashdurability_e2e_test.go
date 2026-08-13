// Slash-command durability, end to end over the REAL processes.
//
// This file is the OPERATIONAL READING of
// proto/FROZEN-slash-command-durability.md Part 5, tests 1-4 and 7-10. Tests 5
// and 6 are the two constructor refusals and live in
// slashdurabilityctor_e2e_test.go, because they are the only two that do not
// need a running stack.
//
// THE SYMPTOM THIS SUITE EXISTS FOR. The user reported duplicate prompt bubbles
// and a full history replay on every workspace open. The root cause was TWO
// identity schemes for one prompt: the webapp minted an optimistic bubble at
// submit and then had to reconcile it against the durable line the CLI later
// wrote. The chosen fix kills the optimistic render outright (Part 2), on the
// ground that a correlation which CAN attach the wrong record is worse than a
// prompt that appears a round trip late. Test 7 is the regression guard for
// exactly that, and test 10 is the guard for the identity confusion behind it.
//
// WHY THE CLI'S RECORDS ARE INJECTED RATHER THAN PROVOKED. Same reason as
// machinery_e2e_test.go and clearcompact_e2e_test.go, whose helpers this file
// reuses READ-ONLY (liveSession, storeProducer.write, awaitItem, deltaItems,
// dialForReplay, replayItems, tailStore): the sidecar is the sole producer of
// file-plane records and it produces them by tailing a real vendor transcript,
// which the `--fake` harness has none of. So these tests write to the store
// exactly the event shape the sidecar writes for a transcript line and exercise
// everything downstream for real.
//
// WHAT MAKES A NEGATIVE MEAN ANYTHING HERE. The store assigns seq in arrival
// order and every hop below it preserves that order, so a record that WOULD
// have been produced necessarily precedes one produced after it. Every "this
// never arrived" assertion below is therefore chased with a distinct SENTINEL
// and read up to it, rather than waited out against a duration.
package e2e

import (
	"fmt"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// --- the CLI's two record shapes, as the sidecar writes them ----------------

// slashShapeAContent is the VERBATIM content head the Claude CLI writes for a
// slash command it handled itself (FROZEN contract Part 1, Shape A): type
// "user", NOT flagged isMeta, and the content head is the only envelope signal.
func slashShapeAContent(literal string) string {
	return fmt.Sprintf("<command-message>%s</command-message>\n<command-name>%s</command-name>\n<command-args></command-args>",
		literal[1:], literal)
}

// slashShapeAEvent is handler.vendorEvent's shape for a Shape-A record,
// carrying the promptId that GROUPS a submission's records.
//
// promptId is the correlation handle the contract restores (Part 1, "prompt_id
// is already parsed and never read"): transcript.proto declares it, the sidecar
// carries it, and nothing downstream has ever called GetPromptId. Test 10 is
// the reason it is populated here rather than left empty — identity, not
// arrival order, is what must keep two close-together commands apart.
func slashShapeAEvent(t *testing.T, vendorSessionID, lineUUID, promptID, content string) *corev1.Event {
	t.Helper()
	return slashVendorEvent(t, vendorSessionID, &datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: lineUUID, PromptId: promptID},
			Message: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: content},
			},
		}},
	})
}

// slashShapeBEvent is the sidecar's shape for a system/local_command record
// (FROZEN contract Part 1, Shape B): type "system", subtype "local_command",
// and IS flagged isMeta. It carries NO promptId, which is one of the two
// reasons the contract rules it out of the durable set.
//
// The ENVELOPE is the classification signal here, not the content head. That
// distinction is the whole of test 8, and it is why this constructor takes the
// content as a parameter rather than baking a machinery-shaped one in.
func slashShapeBEvent(t *testing.T, vendorSessionID, lineUUID, content string) *corev1.Event {
	t.Helper()
	return slashVendorEvent(t, vendorSessionID, &datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_System{System: &datav1.SystemLine{
			Envelope: &datav1.LineEnvelope{Uuid: lineUUID, IsMeta: true},
			Subtype: &datav1.SystemLine_LocalCommand{LocalCommand: &datav1.LocalCommandLine{
				Content: content,
			}},
		}},
	})
}

// slashVendorEvent wraps one transcript line the way handler.vendorEvent does:
// file plane, PERSISTENT class, and NO dedup key, because the store derives its
// own `uuid:` key for a vendor line.
func slashVendorEvent(t *testing.T, vendorSessionID string, line *datav1.TranscriptLine) *corev1.Event {
	t.Helper()
	a, err := anypb.New(line)
	if err != nil {
		t.Fatalf("anypb.New(TranscriptLine): %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// slashLineUUID reports the transcript line uuid a store event carries, or ""
// when the event is not a vendor transcript line.
//
// It is how a DURABLE message's claim is CHECKED against the store: feed.proto
// states that a durable message's own uuid IS its record's key, so a durable
// message must be findable in the store under exactly that string. Test 4 is
// that check, and test 3 is its converse for an ephemeral id.
func slashLineUUID(ev *corev1.Event) string {
	vendor := ev.GetVendor()
	if vendor == nil {
		return ""
	}
	var line datav1.TranscriptLine
	if err := vendor.UnmarshalTo(&line); err != nil {
		return ""
	}
	switch l := line.GetLine().(type) {
	case *datav1.TranscriptLine_User:
		return l.User.GetEnvelope().GetUuid()
	case *datav1.TranscriptLine_System:
		return l.System.GetEnvelope().GetUuid()
	}
	return ""
}

// --- frontend predicates ----------------------------------------------------

// slashIsCommand matches the intercepted-command message for one command.
func slashIsCommand(command frontendv1.SessionCommand) func(*frontendv1.Message) bool {
	return func(item *frontendv1.Message) bool {
		return item.GetDaemonInterceptedCommand().GetCommand() == command
	}
}

// slashCommandsIn returns every intercepted-command message for one command.
func slashCommandsIn(items []*frontendv1.Message, command frontendv1.SessionCommand) []*frontendv1.Message {
	var out []*frontendv1.Message
	match := slashIsCommand(command)
	for _, item := range items {
		if match(item) {
			out = append(out, item)
		}
	}
	return out
}

// slashUserMessagesIn returns every user prompt message carrying text.
func slashUserMessagesIn(items []*frontendv1.Message, text string) []*frontendv1.Message {
	var out []*frontendv1.Message
	for _, item := range items {
		if item.GetUserMessage().GetContentString() == text {
			out = append(out, item)
		}
	}
	return out
}

// slashSubmitJSON is the SubmitPromptCmd wire form used throughout this file.
func slashSubmitJSON(requestID, text string) string {
	return fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`,
		requestID, text)
}

// slashDurabilityOf spells a message's durability class for a failure message.
// A message carrying NEITHER arm is malformed by the contract, and naming that
// state explicitly is what keeps "no arm" from reading as "not durable".
func slashDurabilityOf(m *frontendv1.Message) string {
	switch {
	case m.GetDurable() != nil:
		return "durable"
	case m.GetEphemeral() != nil:
		return "ephemeral"
	default:
		return "NEITHER ARM SET (malformed: the contract requires every Message to state its class)"
	}
}

// --- 1. a /compact produces a durable record that survives a reload ---------

// TestE2ECompactCommandSurvivesAReload covers contract Part 5 test 1.
//
// `/compact` is CLI-HANDLED: it reaches the SDK and the CLI writes a transcript
// record for it, so the message the feed carries for it is DURABLE. The claim
// under test is that a frontend which reconnects after the command has settled
// is served that command FROM THE STORE.
//
// "FROM THE STORE RATHER THAN FROM LOCAL RETENTION" IS READ OFF THE MESSAGE
// ITSELF, and deliberately so. feed.proto makes the durability arm the message's
// own statement of whether a record exists for it, precisely so a reader does
// not have to guess provenance from where a frame happened to come from. A
// replayed command message carrying `durable` is a command backed by a store
// record; one carrying `ephemeral` is the daemon's own retention, which is what
// this test forbids.
func TestE2ECompactCommandSurvivesAReload(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, live, vendorID, store := liveSession(t, h, cwd)

	// Act — the user issues /compact, and the CLI writes its record for it.
	writeCmd(t, live, slashSubmitJSON("r-compact", "/compact"))
	store.write(slashShapeAEvent(t, vendorID, "e2e-slash-compact-line", "e2e-prompt-compact",
		slashShapeAContent("/compact")))
	awaitItem(t, live, cwd, "the /compact intercepted-command message",
		slashIsCommand(frontendv1.SessionCommand_SESSION_COMMAND_COMPACT))

	// Assert — a fresh frontend replays it, and it is durable.
	fresh, state := dialForReplay(t, h, id, cwd)
	replayed := replayItems(t, fresh, state, cwd, "r-replay-compact")
	commands := slashCommandsIn(replayed, frontendv1.SessionCommand_SESSION_COMMAND_COMPACT)
	if len(commands) != 1 {
		t.Fatalf("the replay carried %d /compact intercepted-command messages, want exactly 1 — a reload must reproduce the command once, from the store", len(commands))
	}
	if commands[0].GetDurable() == nil {
		t.Errorf("the replayed /compact message is %s, want durable — /compact is CLI-handled, so a transcript record exists for it and the reload must be serving that record rather than the daemon's own retention",
			slashDurabilityOf(commands[0]))
	}
}

// --- 2. a /model produces an ephemeral item that does NOT survive a reload ---

// TestE2EModelCommandDoesNotSurviveAReload covers contract Part 5 test 2.
//
// `/model <name>` is in the ephemeral class: the daemon performs it itself and
// the CLI never sees it, so nothing durable can exist for it and what a
// reconnecting frontend gets is a RE-PUSH from the daemon's own retention
// rather than a replay of a record.
//
// THE ARGUMENT FORM IS NOT INCIDENTAL. Contract Part 4 named `/model` flatly,
// which the code does not bear out: BARE `/model` is forwarded to the CLI, so
// the CLI writes a Shape A record and the invocation is DURABLE. What decides
// the class is who answered the command, exactly as Part 3 says, and for this
// command that turns on whether an argument followed it.
//
// The two halves are both asserted because either alone is satisfiable by a
// defect. An item that survives the reload but is marked durable is a lie about
// a record that does not exist; an item correctly marked ephemeral that does not
// survive the reload leaves the feed with no account of why the session's model
// changed.
func TestE2EModelCommandDoesNotSurviveAReload(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, live, vendorID, store := liveSession(t, h, cwd)
	tail := tailStore(t, vendorID)

	// Act
	writeCmd(t, live, slashSubmitJSON("r-model", "/model opus"))
	model, _ := awaitItem(t, live, cwd, "the /model intercepted-command message",
		slashIsCommand(frontendv1.SessionCommand_SESSION_COMMAND_MODEL))

	// Assert — re-pushed on reconnect, and stated ephemeral.
	fresh, state := dialForReplay(t, h, id, cwd)
	replayed := replayItems(t, fresh, state, cwd, "r-replay-model")
	commands := slashCommandsIn(replayed, frontendv1.SessionCommand_SESSION_COMMAND_MODEL)
	if len(commands) != 1 {
		t.Fatalf("the reconnect carried %d /model intercepted-command messages, want exactly 1 re-pushed from daemon retention", len(commands))
	}
	if commands[0].GetEphemeral() == nil {
		t.Errorf("the re-pushed /model message is %s, want ephemeral — the CLI never saw the command, so no record exists for it and none ever will",
			slashDurabilityOf(commands[0]))
	}

	// And no store record was ever written under its id. The sentinel is a
	// distinct compaction written AFTER the command: the store preserves arrival
	// order, so reading up to the sentinel proves the absence rather than merely
	// outrunning it.
	store.write(sidecarCompactEvent(vendorID, "e2e-slash-model-sentinel", "sentinel"))
	tail.awaitSentinel(t, "the sentinel compaction",
		func(ev *corev1.Event) string {
			if slashLineUUID(ev) == model.GetUuid() {
				return fmt.Sprintf("a durable record was written under the ephemeral /model message's id %q; an ephemeral message is one for which nothing was ever written", model.GetUuid())
			}
			return ""
		},
		func(ev *corev1.Event) bool { return ev.GetContextCompacted() != nil })
}

// --- 3. an ephemeral message never appears in a store page query ------------

// TestE2EEphemeralMessageIsAbsentFromTheStoreAndThatIsCorrect covers contract
// Part 5 test 3.
//
// It asserts the query directly, and — the half that is easy to lose — that the
// empty result is reported as CORRECT rather than as a miss. The message itself
// is what reports it: the `ephemeral` arm turns "no record exists" into a stated
// property, which is the only thing that distinguishes it from "the record was
// not found". Without that arm a page missing an ephemeral card is
// indistinguishable from a page that LOST a durable one, and the second is data
// loss reported as normal.
//
// This test does NOT reload. Test 2 owns the reload edge; this one owns the
// store query and the class statement, so each covers one thing.
func TestE2EEphemeralMessageIsAbsentFromTheStoreAndThatIsCorrect(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)
	tail := tailStore(t, vendorID)

	// Act
	writeCmd(t, live, slashSubmitJSON("r-model-query", "/model opus"))
	model, _ := awaitItem(t, live, cwd, "the /model intercepted-command message",
		slashIsCommand(frontendv1.SessionCommand_SESSION_COMMAND_MODEL))

	// Assert — the message states its class, so its absence from the store is a
	// property a reader can check rather than a hole they must interpret.
	if model.GetEphemeral() == nil {
		t.Fatalf("the /model message is %s, want ephemeral — without the stated class its absence from a store query is indistinguishable from a lost record", slashDurabilityOf(model))
	}
	// And the query over durable records really does return nothing for it.
	store.write(sidecarCompactEvent(vendorID, "e2e-slash-ephemeral-sentinel", "sentinel"))
	tail.awaitSentinel(t, "the sentinel compaction",
		func(ev *corev1.Event) string {
			if slashLineUUID(ev) == model.GetUuid() {
				return fmt.Sprintf("the store holds a durable record under ephemeral message id %q", model.GetUuid())
			}
			return ""
		},
		func(ev *corev1.Event) bool { return ev.GetContextCompacted() != nil })
}

// --- 4. a durable message absent from the store is a LOUD failure -----------

// TestE2EDurableMessageMissingFromTheStoreIsALoudFailure covers contract Part 5
// test 4, the converse of test 3 and the one that keeps test 3 from being a
// licence to lose things.
//
// A durable message CLAIMS a record exists (feed.proto: "The claim it makes is
// checkable and must be checked: a durable message the store cannot produce is
// corruption"). This test performs that check for the /compact command message
// and fails loudly when the store cannot produce the record, rather than
// treating the empty result as an ordinary miss the way test 3's ephemeral id
// legitimately is.
func TestE2EDurableMessageMissingFromTheStoreIsALoudFailure(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)
	tail := tailStore(t, vendorID)
	const lineUUID = "e2e-slash-durable-check-line"

	// Act
	writeCmd(t, live, slashSubmitJSON("r-compact-durable", "/compact"))
	store.write(slashShapeAEvent(t, vendorID, lineUUID, "e2e-prompt-compact-durable",
		slashShapeAContent("/compact")))
	compact, _ := awaitItem(t, live, cwd, "the /compact intercepted-command message",
		slashIsCommand(frontendv1.SessionCommand_SESSION_COMMAND_COMPACT))

	// Assert — the message claims durability, so the store must produce the
	// record it is keyed by. Anything else is corruption and says so.
	if compact.GetDurable() == nil {
		t.Fatalf("the /compact message is %s, want durable — the CLI wrote a transcript record for it, so the message must claim that record", slashDurabilityOf(compact))
	}
	tail.await(t, fmt.Sprintf("the store record backing durable message %q", compact.GetUuid()),
		func(ev *corev1.Event) bool { return slashLineUUID(ev) == compact.GetUuid() })
}

// --- 7. a prompt renders ONLY after its round trip --------------------------

// TestE2EPromptRendersOnlyAfterItsRoundTrip covers contract Part 5 test 7, and
// is the regression guard for the duplicate bubbles the user reported.
//
// Part 2 removes the optimistic prompt render at the source: `addLocalPrompt`,
// the adoption and collapse path that reconciled it, `dropUnackedPrompt`, and
// the prompt receipts that stood in for a durable line that had not yet
// arrived. What remains is one identity for one prompt, arriving when the CLI's
// record does.
//
// The two halves are asserted separately because each guards a different
// failure: nothing before the round trip guards against the optimistic bubble
// coming back, and EXACTLY ONE after guards against the reconciliation coming
// back with it.
func TestE2EPromptRendersOnlyAfterItsRoundTrip(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)
	const prompt = "carry on with the plan"

	// Act — submit, then chase the submit with a sentinel the frontend WILL be
	// shown. The store preserves arrival order, so anything the submit alone
	// would have drawn necessarily precedes the sentinel's item.
	writeCmd(t, live, slashSubmitJSON("r-roundtrip", prompt))
	store.write(sidecarCompactEvent(vendorID, "e2e-slash-prompt-sentinel-a", "before the round trip"))
	_, beforeRoundTrip := awaitItem(t, live, cwd, "the first sentinel's ContextCompacted item", isCompact)

	// Assert — nothing was drawn for the prompt before its record existed.
	if drawn := slashUserMessagesIn(beforeRoundTrip, prompt); len(drawn) != 0 {
		t.Errorf("%d prompt messages reached the feed BEFORE the prompt's durable line arrived, want 0 — an optimistic render is a second identity for one prompt, and reconciling it is what produced the duplicate bubbles", len(drawn))
	}

	// Act — the CLI's own record for the prompt lands.
	store.write(slashShapeAEvent(t, vendorID, "e2e-slash-roundtrip-line", "e2e-prompt-roundtrip", prompt))
	first, _ := awaitItem(t, live, cwd, "the prompt's user message", func(item *frontendv1.Message) bool {
		return item.GetUserMessage().GetContentString() == prompt
	})
	if first.GetDurable() == nil {
		t.Errorf("the prompt message is %s, want durable — it exists because the CLI wrote a record for it", slashDurabilityOf(first))
	}

	// Assert — and exactly one, not one plus an adopted optimistic twin.
	store.write(sidecarCompactEvent(vendorID, "e2e-slash-prompt-sentinel-b", "after the round trip"))
	_, afterRoundTrip := awaitItem(t, live, cwd, "the second sentinel's ContextCompacted item", func(item *frontendv1.Message) bool {
		return isCompact(item) && item.GetContextCompacted().GetSummary() == "after the round trip"
	})
	if extra := slashUserMessagesIn(afterRoundTrip, prompt); len(extra) != 0 {
		t.Errorf("%d further prompt messages reached the feed after the first, want 0 — exactly one bubble per prompt", len(extra))
	}
}

// --- 8. Shape B is classified by ENVELOPE, not content head -----------------

// TestE2EShapeBIsClassifiedByItsEnvelopeNotItsContentHead covers contract Part 5
// test 8.
//
// `machinery.go` today states that these records "are NOT flagged isMeta, so
// nothing in the envelope distinguishes them from something the user typed; the
// content head is the only signal the CLI leaves." That is TRUE for Shape A and
// FALSE for Shape B: a system/local_command record carries BOTH `isMeta` and
// `subtype: "local_command"`, which are structural envelope signals and must be
// used in preference to the content head for that shape.
//
// The record injected here has a content head that matches NO machinery prefix,
// so a classifier reading only the content head lets it through as conversation.
// The sentinel is what makes the negative mean something.
func TestE2EShapeBIsClassifiedByItsEnvelopeNotItsContentHead(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)
	// Deliberately NOT machinery-shaped: no <command-name>, no <command-message>,
	// no <local-command-stdout>. The envelope is the only signal left.
	const shapeBContent = "Context usage: 41k/200k tokens"

	// Act
	store.write(slashShapeBEvent(t, vendorID, "e2e-slash-shape-b-line", shapeBContent))
	store.write(sidecarCompactEvent(vendorID, "e2e-slash-shape-b-sentinel", "sentinel"))

	// Assert
	_, before := awaitItem(t, live, cwd, "the sentinel's ContextCompacted item", isCompact)
	for _, item := range before {
		if item.GetUuid() == "e2e-slash-shape-b-line" {
			t.Errorf("the system/local_command record reached the frontend as message uuid=%q — it is classified by its envelope (isMeta + subtype local_command), not by whether its content happens to start with a machinery prefix", item.GetUuid())
		}
		if got := item.GetUserMessage().GetContentString(); got == shapeBContent {
			t.Errorf("a user message carrying the system/local_command record's content reached the frontend: %q", got)
		}
	}
}

// --- 9. a human prompt quoting a machinery tag stays a prompt ---------------

// TestE2EHumanPromptQuotingAMachineryTagStaysAPrompt covers contract Part 5
// test 9. `machinery.go` already warns about this case and it must not regress.
//
// The body BEGINS with `<command-name>` and then continues as ordinary prose —
// the worst case for a prefix match, and a real one: a user asking about the
// CLI's own transcript format types exactly this. Suppressing it would silently
// delete the opening line of their turn.
func TestE2EHumanPromptQuotingAMachineryTagStaysAPrompt(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)
	const quoted = "<command-name> is the tag the CLI writes into the transcript; why does it show up in my feed?"

	// Act
	store.write(slashShapeAEvent(t, vendorID, "e2e-slash-quoted-line", "e2e-prompt-quoted", quoted))

	// Assert — it reaches the feed, untouched.
	item, _ := awaitItem(t, live, cwd, "the quoting prompt's user message", func(m *frontendv1.Message) bool {
		return m.GetUuid() == "e2e-slash-quoted-line"
	})
	if got := item.GetUserMessage().GetContentString(); got != quoted {
		t.Errorf("the prompt reached the feed as %q, want the text the user typed VERBATIM %q — a human prompt that quotes a machinery tag is still a prompt", got, quoted)
	}
}

// --- 10. two slash commands close together do not swap identities -----------

// TestE2ETwoSlashCommandsDoNotSwapIdentities covers contract Part 5 test 10, the
// ordering guard.
//
// The old attribution was POSITIONAL: a transcript line was given the oldest
// outstanding receipt, because a UserLine carried no request id of its own. Two
// concurrent submits could enqueue B's receipt ahead of A's while the SDK had
// taken A first, and A's line would then be stamped with B's identity — two
// commands swapping identities with no error anywhere.
//
// With `promptId` available the correlation is by IDENTITY, and this test pins
// that. Each command is looked up BY ITS OWN request id rather than by its
// position in the arrival order, so a suite that passed only because the two
// happened to arrive in submit order fails here.
//
// THE PAIR CROSSES THE DURABILITY CLASSES ON PURPOSE. `/compact` is CLI-handled
// and durable; `/model <name>` is daemon-handled and ephemeral. Identity must
// hold ACROSS the classes, not only within one — a correlation that works
// because both commands took the same path has not been tested at all.
//
// Only the durable one has a CLI record to invert, which is the point: the
// ephemeral item is pushed at submit and the durable record lands afterwards,
// so the two arrive by different routes in an order neither chose.
func TestE2ETwoSlashCommandsDoNotSwapIdentities(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)

	// Act — two commands issued back to back. The daemon-handled one is
	// answered and pushed at submit; the CLI-handled one's record arrives on the
	// file plane afterwards, so the two land by different routes in an order
	// neither of them chose. A positional correlation gets that wrong and an
	// identity-based one does not.
	//
	// THE DAEMON-HANDLED ONE GOES FIRST, and that ordering is load-bearing for
	// the arrangement rather than for the assertion. `/compact` opens a turn,
	// and the prompt queue forms while a turn is running — issued second, the
	// `/model` submit is QUEUED behind that turn and never dispatched, so the
	// test would fail having never made the pair it is about.
	writeCmd(t, live, slashSubmitJSON("r-cmd-model", "/model opus"))
	writeCmd(t, live, slashSubmitJSON("r-cmd-compact", "/compact"))
	store.write(slashShapeAEvent(t, vendorID, "e2e-slash-pair-compact", "e2e-prompt-pair-compact",
		slashShapeAContent("/compact")))
	store.write(sidecarCompactEvent(vendorID, "e2e-slash-pair-sentinel", "sentinel"))

	// Assert — each command is the one that was issued, found by identity.
	sentinel, seen := awaitItem(t, live, cwd, "the sentinel's ContextCompacted item", isCompact)
	seen = append(seen, sentinel)
	compacts := slashCommandsIn(seen, frontendv1.SessionCommand_SESSION_COMMAND_COMPACT)
	models := slashCommandsIn(seen, frontendv1.SessionCommand_SESSION_COMMAND_MODEL)
	if len(compacts) != 1 {
		t.Fatalf("saw %d /compact intercepted-command messages, want exactly 1", len(compacts))
	}
	if len(models) != 1 {
		t.Fatalf("saw %d /model intercepted-command messages, want exactly 1", len(models))
	}
	// EACH CLASS IS IDENTIFIED BY THE HANDLE ITS OWN PRODUCER CARRIES, and they
	// are deliberately different handles.
	//
	// The durable one is the CLI's record, classified: the daemon produces no
	// item for a forwarded command at all, so there is no daemon request id on
	// it and an empty one is the CORRECT reading rather than a lost value. Its
	// identity is the record's own uuid, which is what a page query returns it
	// by. Stamping a request id onto it would mean correlating the CLI's
	// promptId back to a submit — the prompt-identity correlation contract
	// Part 2 removed, and the thing whose two-identity failure started all of
	// this.
	//
	// The ephemeral one has no record to be identified by, so the submit that
	// caused it IS its identity.
	if got := compacts[0].GetUuid(); got != "e2e-slash-pair-compact" {
		t.Errorf("the /compact message has uuid %q, want the uuid of the record that produced it, %q — the two commands swapped identities",
			got, "e2e-slash-pair-compact")
	}
	if got := compacts[0].GetRequestId(); got != "" {
		t.Errorf("the /compact message names request id %q, want empty — a forwarded command's account comes from the CLI's record, and a request id on it would mean a second producer had run", got)
	}
	if got := models[0].GetRequestId(); got != "r-cmd-model" {
		t.Errorf("the /model message names request id %q, want %q — the two commands swapped identities", got, "r-cmd-model")
	}
}
