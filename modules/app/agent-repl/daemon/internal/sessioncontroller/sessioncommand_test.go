package sessioncontroller

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// The session commands: what the daemon recognizes as one, what it draws in
// place of the prompt bubble it withholds, and the structural guarantee that
// the thing it draws cannot carry the prompt.

// --- helpers ----------------------------------------------------------------

// commandItems are every session_command item the frontend was pushed, in push
// order.
func (h *queueHarness) commandItems() []*frontendv1.SessionCommandItem {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.SessionCommandItem
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
			if sc := it.GetSessionCommand(); sc != nil {
				out = append(out, sc)
			}
		}
	}
	return out
}

// --- the reading ------------------------------------------------------------

func TestLookupSessionCommand(t *testing.T) {
	tests := []struct {
		name string
		text string
		want frontendv1.SessionCommand
	}{
		{
			name: "the bare model command",
			text: "/model",
			want: frontendv1.SessionCommand_SESSION_COMMAND_MODEL,
		},
		{
			// Whitespace around the command is still the command.
			name: "surrounded by whitespace",
			text: "  /model\n",
			want: frontendv1.SessionCommand_SESSION_COMMAND_MODEL,
		},
		{
			// `/model <name>` is the command's documented argument form.
			name: "a command that takes an argument, with one",
			text: "/model opus",
			want: frontendv1.SessionCommand_SESSION_COMMAND_MODEL,
		},
		{
			// `/compact <instructions>` steers the summary.
			name: "a compaction with instructions",
			text: "/compact focus on the parser",
			want: frontendv1.SessionCommand_SESSION_COMMAND_COMPACT,
		},
		{
			// A command that takes NO argument is the whole prompt or nothing:
			// "/status of the build" is something the user asked for.
			name: "an argument-free command with an argument",
			text: "/status of the build",
			want: frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED,
		},
		{
			// `/clear` is deliberately argument-free: mistaking "/clear the
			// build cache" for the command would DISCARD the conversation the
			// user was speaking into.
			name: "the clear with an argument",
			text: "/clear the build cache",
			want: frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED,
		},
		{
			// The argument must be behind whitespace, so a LONGER command name
			// can never be swallowed by a shorter one.
			name: "a longer command sharing a prefix",
			text: "/models",
			want: frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED,
		},
		{
			name: "a custom command, which expands into a real prompt",
			text: "/create-or-update-workspace merge",
			want: frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED,
		},
		{
			name: "an ordinary prompt",
			text: "hello there",
			want: frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := lookupSessionCommand(tc.text)

			// Assert.
			if got != tc.want {
				t.Fatalf("lookupSessionCommand(%q) = %s, want %s", tc.text, got, tc.want)
			}
		})
	}
}

func TestEverySessionCommandSpecNamesADistinctCommand(t *testing.T) {
	// Arrange — the table IS the wire enum's mirror, and a duplicate on either
	// side would make one entry unreachable rather than loudly wrong.
	seenCommand := map[frontendv1.SessionCommand]string{}
	seenLiteral := map[string]bool{}

	// Act / Assert.
	for _, spec := range sessionCommandSpecs {
		if spec.command == frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
			t.Fatalf("spec %q maps to UNSPECIFIED, which is the ordinary-prompt verdict", spec.literal)
		}
		if prev, dup := seenCommand[spec.command]; dup {
			t.Fatalf("%s is claimed by both %q and %q", spec.command, prev, spec.literal)
		}
		if seenLiteral[spec.literal] {
			t.Fatalf("literal %q appears twice", spec.literal)
		}
		seenCommand[spec.command] = spec.literal
		seenLiteral[spec.literal] = true
	}
}

// --- the structural guarantee -----------------------------------------------

func TestTheInvocationItemHasNowhereToPutAPrompt(t *testing.T) {
	// Arrange — this is THE invariant the whole change rests on. A frontend
	// cannot render the submitted prompt from a SessionCommandItem because
	// there is no field to read it out of, and no producer can leak an
	// argument the user typed for the same reason. A field added here would
	// reopen exactly that, which is why the shape is asserted rather than
	// assumed.
	fields := (&frontendv1.SessionCommandItem{}).ProtoReflect().Descriptor().Fields()

	// Act / Assert.
	if fields.Len() != 1 {
		t.Fatalf("SessionCommandItem has %d field(s), want exactly 1 — a second field is somewhere a prompt could ride", fields.Len())
	}
	f := fields.Get(0)
	if f.Name() != "command" {
		t.Fatalf("the single field is %q, want \"command\"", f.Name())
	}
	if f.Kind() != protoreflect.EnumKind {
		t.Fatalf("`command` is a %s, want an enum — only an enum can carry an identity without also being able to carry a message", f.Kind())
	}
}

// --- the withheld bubble ----------------------------------------------------

func TestSubmittingAModelCommandPushesNoPromptBubble(t *testing.T) {
	// Arrange — `/model` never reaches the model: the CLI answers it locally.
	// A purple bubble reading "/model" claims a question was asked that
	// nobody received.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a /model, want none", len(turns))
	}
}

func TestSubmittingAModelCommandPushesItsInvocationItem(t *testing.T) {
	// Arrange — withholding the bubble must not leave the feed silent: this
	// item is the only account the user will get of why the model changed.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	items := h.commandItems()
	if len(items) != 1 {
		t.Fatalf("pushed %d session-command item(s), want 1", len(items))
	}
	if got := items[0].GetCommand(); got != frontendv1.SessionCommand_SESSION_COMMAND_MODEL {
		t.Fatalf("command = %s, want SESSION_COMMAND_MODEL", got)
	}
}

func TestSubmittingAModelCommandStillForwardsItToTheShim(t *testing.T) {
	// Arrange — withholding the BUBBLE must not withhold the COMMAND.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	got := h.client.promptTexts()
	if len(got) != 1 || got[0] != "/model opus" {
		t.Fatalf("forwarded %q, want the /model verbatim", got)
	}
}

func TestSubmittingAModelCommandStillClaimsTheTurn(t *testing.T) {
	// Arrange — the CLI runs `/model` and closes a turn over it, so the shim
	// IS busy. A workspace left green through it could not be told apart from
	// one that dropped the command.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if !h.turnActiveFlag() {
		t.Error("the session did not claim a turn for /model; the workspace would stay green over a busy shim")
	}
}

func TestSubmittingAClearAlsoPushesItsInvocationItem(t *testing.T) {
	// Arrange — every recognized command is reported the same way. The clear's
	// own item is later dropped with the history its cut hides, which is the
	// cut's doing rather than a hole in the reporting.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	items := h.commandItems()
	if len(items) != 1 || items[0].GetCommand() != frontendv1.SessionCommand_SESSION_COMMAND_CLEAR {
		t.Fatalf("session-command items = %v, want one CLEAR", items)
	}
}

func TestAnOrdinaryPromptPushesNoInvocationItem(t *testing.T) {
	// Arrange — an ordinary prompt is the user speaking, and it gets its
	// bubble and nothing else.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if items := h.commandItems(); len(items) != 0 {
		t.Fatalf("pushed %d session-command item(s) for a prompt, want none", len(items))
	}
	if turns := h.userTurns(); len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s) for a prompt, want 1", len(turns))
	}
}

func TestASubmitWithNoRequestIdIsRejectedBeforeInvocationItem(t *testing.T) {
	h := newQueueHarness(t, nil)

	if err := h.submitAs("", "/model"); err == nil {
		t.Fatal("submit accepted an empty request id")
	}

	if items := h.commandItems(); len(items) != 0 {
		t.Fatalf("pushed %d session-command item(s) for rejected submit, want none", len(items))
	}
}

// --- retention --------------------------------------------------------------

func TestTheInvocationItemIsReplayedOnResync(t *testing.T) {
	// Arrange — the item carries no store seq, so no from_seq a resync names
	// could ever cover it, and no receipt stands in for it either.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "/model"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.controller().consumer.resync(0)

	// Assert.
	if items := h.commandItems(); len(items) != 2 {
		t.Fatalf("pushed %d session-command item(s) after a replay, want 2 (the live one and its replay)", len(items))
	}
}

func TestARepeatedInvocationUnderOneRequestIdReplacesTheRetainedItem(t *testing.T) {
	// Arrange — the uuid is derived from the request id, so a re-push must
	// REPLACE rather than accumulate; otherwise a resync would draw the same
	// invocation twice.
	h := newQueueHarness(t, nil)
	c := h.controller().consumer

	// Act.
	c.pushSessionCommand("r1", frontendv1.SessionCommand_SESSION_COMMAND_MODEL)
	c.pushSessionCommand("r1", frontendv1.SessionCommand_SESSION_COMMAND_MODEL)

	// Assert.
	if got := len(c.snapshotCommandItems()); got != 1 {
		t.Fatalf("retained %d item(s) under one request id, want 1", got)
	}
}

func TestAContextCutDropsTheRetainedInvocationItems(t *testing.T) {
	// Arrange — an invocation from BELOW the cut, replayed above it, would sit
	// in a feed the cut exists to open.
	h := newQueueHarness(t, nil)
	c := h.controller().consumer
	c.pushSessionCommand("r1", frontendv1.SessionCommand_SESSION_COMMAND_MODEL)

	// Act.
	c.Consume(clearEvent(7, "u-clear"))

	// Assert.
	if got := len(c.snapshotCommandItems()); got != 0 {
		t.Fatalf("retained %d item(s) across a cut, want 0", got)
	}
}
