package footer

import (
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// unpinnedCell is what every unpinned container carries, whether or not its
// status admits the quiet tier.
type unpinnedCell interface {
	GetTransient() *frontendv1.FooterActivityTransient
	GetEnduring() *frontendv1.FooterActivityEnduring
}

// unpinnedOf reads the unpinned tiers whatever status arm carries them, a nil
// container when the arm's cell is salient (or the arm is waiting, whose cell
// has no unpinned branch).
func unpinnedOf(status *frontendv1.FooterStatus) unpinnedCell {
	switch arm := status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Idle:
		return arm.Idle.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_TurnFailed:
		return arm.TurnFailed.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Degraded:
		return arm.Degraded.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Working:
		return arm.Working.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Interrupted:
		return arm.Interrupted.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Merging:
		return arm.Merging.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_MergeFailed:
		return arm.MergeFailed.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Merged:
		return arm.Merged.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Background:
		return arm.Background.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Blocked:
		return arm.Blocked.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Disconnected:
		return arm.Disconnected.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Closing:
		return arm.Closing.GetActivity().GetUnpinned()
	case *frontendv1.FooterStatus_Loading:
		return arm.Loading.GetActivity().GetUnpinned()
	default:
		return (*frontendv1.FooterActivityTransientOverEnduring)(nil)
	}
}

// raisedCount counts the transients the resolver raised.
func raisedCount(h *harness) int {
	return len(recordsOf(h.log.Records(), "daemon.footer.transient_raised"))
}

// transientOf is the live transient the last published view carries, nil when
// none.
func transientOf(t *testing.T, h *harness) *frontendv1.FooterActivityTransient {
	t.Helper()
	return unpinnedOf(h.view(t).GetStrip().GetStatus()).GetTransient()
}

// notificationFrame is the agent's push notification.
func notificationFrame(text string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "note-1"},
		Item: &conversationv1.AgentActivity_PushNotification{
			PushNotification: &conversationv1.AgentPushNotification{
				State: &conversationv1.AgentPushNotification_Start{
					Start: &conversationv1.AgentPushNotificationStart{Message: text},
				},
			},
		},
	}
}

// hookFrame is a hook starting, or settling.
func hookFrame(name string, running bool) *conversationv1.AgentActivity {
	hook := &conversationv1.AgentHook{}
	if running {
		hook.Result = &conversationv1.AgentHook_Start{Start: &conversationv1.AgentHookStart{HookName: name}}
	} else {
		hook.Result = &conversationv1.AgentHook_Succeeded{Succeeded: &conversationv1.AgentHookSucceeded{}}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "hook-1"},
		Item:       &conversationv1.AgentActivity_Hook{Hook: hook},
	}
}

// modelChanged is a model change to the named model.
func modelChanged(name string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ModelChanged{
		ModelChanged: &conversationv1.SessionModelChanged{EffectiveModel: &conversationv1.AgentModel{Name: name}}}}
}

func TestATransientIsStampedWithItsEventInstantAndExpiry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	got := transientOf(t, h)
	if got.GetAt().GetAtMs() != instant.UnixMilli() {
		t.Fatalf("at = %d, want the event instant %d", got.GetAt().GetAtMs(), instant.UnixMilli())
	}
	if want := instant.Add(DefaultTransientWindow).UnixMilli(); got.GetExpiry().GetExpiresAtMs() != want {
		t.Fatalf("expires_at_ms = %d, want at + the default window %d", got.GetExpiry().GetExpiresAtMs(), want)
	}
}

func TestTheTransientWindowIsInjectable(t *testing.T) {
	// Arrange
	h := newHarness(t, WithTransientWindow(3*time.Second))
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	if want := instant.Add(3 * time.Second).UnixMilli(); transientOf(t, h).GetExpiry().GetExpiresAtMs() != want {
		t.Fatalf("expires_at_ms = %d, want %d", transientOf(t, h).GetExpiry().GetExpiresAtMs(), want)
	}
}

func TestANonPositiveTransientWindowIsRefused(t *testing.T) {
	tests := []struct {
		name   string
		window time.Duration
	}{
		{name: "zero", window: 0},
		{name: "negative", window: -time.Second},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			_, err := newResolver(testColors(), dlog.NewTestSurfaces(), WithTransientWindow(tt.window))

			// Assert
			if err == nil {
				t.Fatalf("a %s transient window was accepted; a line that never shows is a misconfiguration", tt.window)
			}
		})
	}
}

func TestANewerTransientReplacesAnOlderOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))
	h.clock.Advance(time.Second)

	// Act
	h.r.OnSessionUpdate(testWS, modelChanged("claude-opus-5"))

	// Assert
	got := transientOf(t, h)
	if got.GetSessionChange() == nil || got.GetHook() != nil {
		t.Fatalf("transient = %+v, want the newer session change alone", got)
	}
	if got.GetAt().GetAtMs() != instant.Add(time.Second).UnixMilli() {
		t.Fatalf("at = %d, want the newer event's instant", got.GetAt().GetAtMs())
	}
}

func TestALapsedTransientIsOmittedFromTheNextView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))
	h.clock.Advance(DefaultTransientWindow)

	// Act: an unrelated fact composes a fresh view.
	h.r.SetParked(testWS, false)

	// Assert
	unpinned := unpinnedOf(h.view(t).GetStrip().GetStatus())
	if unpinned.GetTransient() != nil {
		t.Fatalf("transient = %+v, want none once its expiry passed", unpinned.GetTransient())
	}
	if unpinned.GetEnduring() == nil {
		t.Fatalf("the enduring line is unset; it is always drawn")
	}
}

func TestALiveTransientIsKeptInsideItsWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))
	h.clock.Advance(DefaultTransientWindow - time.Millisecond)

	// Act
	h.r.SetParked(testWS, false)

	// Assert
	if transientOf(t, h).GetHook() == nil {
		t.Fatalf("transient = %+v, want the hook line still inside its window", transientOf(t, h))
	}
}

func TestRaisingATransientArmsNoTimer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	if n := len(h.clock.pending); n != 0 {
		t.Fatalf("%d timers pending, want none: the client's clock retires a transient", n)
	}
}

func TestATransientOutlivesTheStatusItWasRaisedUnder(t *testing.T) {
	// Arrange: a hook line raised mid-turn.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, hookFrame("stop-hook", true))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, ptr(ids.TurnID("turn-1")), completed(), nil)

	// Assert
	if got := transientOf(t, h).GetHook().GetName(); got != "stop-hook" {
		t.Fatalf("idle transient = %q, want the turn's last hook line carried over", got)
	}
}

func TestASalientLineOutranksALiveTransient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Act
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Assert
	activity := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity()
	if activity.GetSalient().GetCompaction() == nil || activity.GetUnpinned() != nil {
		t.Fatalf("activity = %+v, want the salient compaction and no transient", activity)
	}
}

func TestAMainAgentTransientCarriesNoAgent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, notificationFrame("hello"))

	// Assert
	if got := transientOf(t, h).GetAgent(); got != nil {
		t.Fatalf("agent = %+v, want unset for the main agent", got)
	}
}

func TestASubagentsTransientCarriesItsLabel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("u-1", "agent-sub", "Explore", "find the call sites"))

	// Act
	h.r.OnActivity(testWS, &conversationv1.AgentId{Value: "agent-sub"}, hookFrame("pre-commit", true))

	// Assert
	if got := transientOf(t, h).GetAgent().GetLabel(); got != "find the call sites" {
		t.Fatalf("agent label = %q, want the subagent's description", got)
	}
}

func TestASubagentWithNoDescriptionIsLabelledByItsType(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("u-1", "agent-sub", "Explore", ""))

	// Act
	h.r.OnActivity(testWS, &conversationv1.AgentId{Value: "agent-sub"}, hookFrame("pre-commit", true))

	// Assert
	if got := transientOf(t, h).GetAgent().GetLabel(); got != "Explore" {
		t.Fatalf("agent label = %q, want the subagent's type", got)
	}
}

func TestSubmittingATurnRaisesThePromptsFirstLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Prompt: "\n  fix the flaky test  \nand then commit"})

	// Assert
	if got := transientOf(t, h).GetSubmitting().GetPromptLead(); got != "fix the flaky test" {
		t.Fatalf("prompt lead = %q, want the first non-blank line", got)
	}
}

func TestATurnWithNoPromptTextRaisesNoSubmittingLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := transientOf(t, h); got != nil {
		t.Fatalf("transient = %+v, want none for a turn with no text", got)
	}
}

func TestTheSubmittingLineIsCapped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Prompt: strings.Repeat("x", 400)})

	// Assert
	if got := []rune(transientOf(t, h).GetSubmitting().GetPromptLead()); len(got) != DefaultWarningRowWidth {
		t.Fatalf("prompt lead is %d runes, want capped at %d", len(got), DefaultWarningRowWidth)
	}
}

func TestAHookStartRaisesTheHookLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	if got := transientOf(t, h).GetHook().GetName(); got != "pre-commit" {
		t.Fatalf("hook = %q, want pre-commit", got)
	}
}

func TestAHookSettlingRaisesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", false))

	// Assert
	if got := transientOf(t, h); got != nil {
		t.Fatalf("transient = %+v, want none for a settling hook", got)
	}
}

func TestAContextInjectionRaisesTheInjectedLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, memoryInjection("webapp/CLAUDE.md"))

	// Assert
	loading := h.view(t).GetStrip().GetStatus().GetLoading()
	if got := loading.GetActivity().GetUnpinned().GetTransient().GetContextInjected().GetText(); got != "webapp/CLAUDE.md" {
		t.Fatalf("injected = %q under the loading status, want the item", got)
	}
}

func TestASessionChangeRaisesItsComposedLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, modelChanged("claude-opus-5"))

	// Assert
	if got := transientOf(t, h).GetSessionChange().GetText(); got != "model → claude-opus-5" {
		t.Fatalf("session change = %q, want the composed model line", got)
	}
}

func TestSessionChangeTextComposesEverySettingArm(t *testing.T) {
	tests := []struct {
		name   string
		update *conversationv1.SessionUpdate
		want   string
	}{
		{name: "a named model", update: modelChanged("claude-opus-5"), want: "model → claude-opus-5"},
		{name: "an unnamed model", update: modelChanged(""), want: "model changed"},
		{
			name: "a permission mode",
			update: &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_PermissionModeChanged{
				PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{PermissionMode: &conversationv1.AgentPermissionMode{
					Mode: &conversationv1.AgentPermissionMode_AcceptEdits{AcceptEdits: &conversationv1.AgentPermissionModeAcceptEdits{}}}}}},
			want: "permission mode → accept edits",
		},
		{
			name: "an unstated permission mode",
			update: &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_PermissionModeChanged{
				PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{}}},
			want: "permission mode → unstated",
		},
		{name: "a connected mcp server", update: mcpServer("github", &conversationv1.SessionMcpServer_Connected{Connected: &conversationv1.SessionMcpServerConnected{}}), want: "mcp github connected"},
		{name: "a failed mcp server", update: mcpServer("github", &conversationv1.SessionMcpServer_Failed{Failed: &conversationv1.SessionMcpServerFailed{Error: "ECONNREFUSED\nstack"}}), want: "mcp github failed — ECONNREFUSED"},
		{name: "a failed mcp server with no error", update: mcpServer("github", &conversationv1.SessionMcpServer_Failed{Failed: &conversationv1.SessionMcpServerFailed{}}), want: "mcp github failed"},
		{name: "an mcp server needing auth", update: mcpServer("github", &conversationv1.SessionMcpServer_NeedsAuth{NeedsAuth: &conversationv1.SessionMcpServerNeedsAuth{}}), want: "mcp github needs auth"},
		{name: "a pending mcp server", update: mcpServer("github", &conversationv1.SessionMcpServer_Pending{Pending: &conversationv1.SessionMcpServerPending{}}), want: "mcp github connecting"},
		{name: "a disabled mcp server", update: mcpServer("github", &conversationv1.SessionMcpServer_Disabled{Disabled: &conversationv1.SessionMcpServerDisabled{}}), want: "mcp github disabled"},
		{name: "an mcp server with no health", update: mcpServer("github", nil), want: "mcp github changed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got, ok := sessionChangeText(tt.update)

			// Assert
			if !ok || got != tt.want {
				t.Fatalf("sessionChangeText = (%q, %v), want (%q, true)", got, ok, tt.want)
			}
		})
	}
}

func TestSessionChangeTextIgnoresEveryOtherArm(t *testing.T) {
	// Arrange, Act
	_, ok := sessionChangeText(vendorCompacting())

	// Assert
	if ok {
		t.Fatalf("a compaction arm composed a session change")
	}
}

func TestATaskMoveRaisesItsSubjectAndCounts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t-1", pendingTask(stated("write the tests"))))

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t-2", completedTask(stated("land the fix"))))

	// Assert
	task := transientOf(t, h).GetTask()
	if task.GetSubject() != "land the fix" || task.GetCompleted() != 1 || task.GetTotal() != 2 {
		t.Fatalf("task = %+v, want the moved task's subject with 1/2 complete", task)
	}
}

func TestARejectedTaskActRaisesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t-1", pendingTask(stated("write the tests"))))
	h.clock.Advance(DefaultTransientWindow)
	rejected := taskAct("t-1", completedTask(nil))
	rejected.GetTaskAct().Act = &conversationv1.AgentTaskAct_Rejected{Rejected: &conversationv1.AgentTaskRejected{}}

	// Act
	h.r.OnActivity(testWS, mainAgent, rejected)

	// Assert
	if got := transientOf(t, h); got != nil {
		t.Fatalf("transient = %+v, want none: a refused act moved nothing", got)
	}
}

func TestADeletedTaskIsAnnouncedByItsHeldSubject(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t-1", pendingTask(stated("write the tests"))))

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t-1", deletedTask(nil)))

	// Assert
	task := transientOf(t, h).GetTask()
	if task.GetSubject() != "write the tests" || task.GetTotal() != 0 {
		t.Fatalf("task = %+v, want the deleted task's subject over an empty tracker", task)
	}
}

func TestATaskActTheChecklistCannotHoldRaisesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: no subject, no held entry.
	h.r.OnActivity(testWS, mainAgent, taskAct("t-9", completedTask(nil)))

	// Assert
	if got := transientOf(t, h); got != nil {
		t.Fatalf("transient = %+v, want none for an act the checklist could not hold", got)
	}
}

func TestFirstLineIsTheFirstNonBlankLine(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "one line", text: "hello", want: "hello"},
		{name: "leading blank lines", text: "\n \n  hello  \nworld", want: "hello"},
		{name: "nothing but blanks", text: " \n\t\n", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got := firstLine(tt.text)

			// Assert
			if got != tt.want {
				t.Fatalf("firstLine(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestTransientKindNamesAnUnsetKind(t *testing.T) {
	// Arrange, Act
	got := transientKind(&frontendv1.FooterActivityTransient{})

	// Assert
	if got != "unset" {
		t.Fatalf("transientKind = %q, want unset", got)
	}
}

// mcpServer is one MCP server health statement.
func mcpServer(name string, health any) *conversationv1.SessionUpdate {
	server := &conversationv1.SessionMcpServer{Name: name}
	switch h := health.(type) {
	case *conversationv1.SessionMcpServer_Connected:
		server.Health = h
	case *conversationv1.SessionMcpServer_Failed:
		server.Health = h
	case *conversationv1.SessionMcpServer_NeedsAuth:
		server.Health = h
	case *conversationv1.SessionMcpServer_Pending:
		server.Health = h
	case *conversationv1.SessionMcpServer_Disabled:
		server.Health = h
	}
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_McpServer{McpServer: server}}
}
