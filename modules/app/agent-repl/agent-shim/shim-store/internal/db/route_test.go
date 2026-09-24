package db

import (
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/types/known/structpb"
)

// ---- the routed kind of every arm ----

func TestClassifyRoutesEveryArmToItsKind(t *testing.T) {
	// Arrange
	tests := []struct {
		name     string
		entry    *storev1.StoreEntry
		wantKind string
		wantBook string
	}{
		{
			name:     "agent update is a page line in its book",
			entry:    pageEntry("w", "u", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "context cut is an ordinary update page line",
			entry:    pageEntry("w", "u", "agent-1", frameItem(contextCutFrame("agent-1"))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "api error is an ordinary update page line",
			entry:    pageEntry("w", "u", "agent-1", frameItem(apiErrorFrame("agent-1"))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "success is a page line",
			entry:    pageEntry("w", "u", "agent-1", frameItem(successFrame("agent-1"))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "failure is a page line",
			entry:    pageEntry("w", "u", "agent-1", frameItem(failureFrame("agent-1"))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "prompt is a page line",
			entry:    pageEntry("w", "u", "agent-1", promptItem("agent-1")),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "peer message is a page line in its recipient's book",
			entry:    pageEntry("w", "u", "agent-1", peerItem("agent-1")),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			// The handoff is what the book's reader has to see, and the
			// announcement is the one durable copy of what was announced.
			name:     "detached work announcement is a page line",
			entry:    pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", bashWork())))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			// A workflow-kind announcement is a page line like every other
			// announcement; what this wave does not do is SERVE the workflow.
			name:     "a detached workflow announcement is a page line too",
			entry:    pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", workflowWork())))),
			wantKind: kindPageLine,
			wantBook: "agent-1",
		},
		{
			name:     "vendor specific residue is never served",
			entry:    unservedEntry("w", "u", &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook", Raw: rawRecord("hook")}}}),
			wantKind: kindVendorSpecific,
		},
		{
			name:     "unknown residue is never served",
			entry:    unservedEntry("w", "u", &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{Discriminator: "widget", Raw: rawRecord("widget")}}}),
			wantKind: kindUnknown,
		},
		{
			name:     "unparsed residue is never served",
			entry:    unservedEntry("w", "u", &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{Source: "transcript.jsonl", Raw: "{\"broken\":"}}}),
			wantKind: kindUnparsed,
		},
		{
			name:     "a bash run frame is never a page line",
			entry:    bashEntry("w", "u", "run-1", bashStart()),
			wantKind: kindBash,
		},
		{
			name:     "a workflow run frame is residue this wave",
			entry:    workflowRunEntry("w", "u", "run-agent-1"),
			wantKind: kindWorkflow,
		},
		{
			name:     "a session update belongs to no book",
			entry:    sessionUpdateEntry("w", "u"),
			wantKind: kindSessionUpdate,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			r, err := classify(test.entry, 0)

			// Assert
			if err != nil {
				t.Fatalf("classify: %v", err)
			}
			if r.kind != test.wantKind {
				t.Fatalf("kind = %q, want %q", r.kind, test.wantKind)
			}
			if r.book.String != test.wantBook || r.book.Valid != (test.wantBook != "") {
				t.Fatalf("book = %#v, want %q", r.book, test.wantBook)
			}
			if (r.pageLine != nil) != (test.wantKind == kindPageLine) {
				t.Fatalf("pageLine presence = %t for kind %q", r.pageLine != nil, r.kind)
			}
		})
	}
}

func TestClassifyCarriesTopLevelWhenTheProducerResolvedIt(t *testing.T) {
	// Arrange
	entry := pageEntry("w", "u", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	entry.GetAgentUpdate().TopLevel = &conversationv1.AgentId{Value: "main-agent"}

	// Act
	r, err := classify(entry, 0)

	// Assert
	if err != nil {
		t.Fatalf("classify: %v", err)
	}
	if !r.topLevel.Valid || r.topLevel.String != "main-agent" {
		t.Fatalf("top_level = %#v, want main-agent", r.topLevel)
	}
}

func TestClassifyLeavesTopLevelUnsetWhenItIsUnresolvable(t *testing.T) {
	// Arrange: an unparsed record may name no agent at all, and absence is
	// expressed by absence.
	entry := unservedEntry("w", "u", &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{Source: "x", Raw: "{"}},
	})

	// Act
	r, err := classify(entry, 0)

	// Assert
	if err != nil {
		t.Fatalf("classify: %v", err)
	}
	if r.topLevel.Valid {
		t.Fatalf("top_level = %#v, want unset", r.topLevel)
	}
}

// ---- refusals: one edge per case ----

func TestClassifyRefusesEachUnsetRequiredField(t *testing.T) {
	// Arrange: every entry below is illegal for exactly one reason, and each
	// reason is a real refusal site the server turns into a failure arm.
	tests := []struct {
		name  string
		entry *storev1.StoreEntry
	}{
		{name: "nil entry", entry: nil},
		{
			name:  "unset plane",
			entry: withoutPlane(pageEntry("w", "u", "agent-1", promptItem("agent-1"))),
		},
		{
			name:  "empty write_id",
			entry: pageEntry("", "u", "agent-1", promptItem("agent-1")),
		},
		{
			name:  "empty upsert_key",
			entry: pageEntry("w", "", "agent-1", promptItem("agent-1")),
		},
		{
			name:  "unset entry arm",
			entry: &storev1.StoreEntry{Plane: streamPlane(), WriteId: "w", UpsertKey: "u"},
		},
		{
			name:  "turn present with an empty value",
			entry: stampedTurn(pageEntry("w", "u", "agent-1", promptItem("agent-1")), ""),
		},
		{
			name:  "unset agent_info arm",
			entry: agentUpdateEntry("w", "u", &storev1.StoreAgentUpdate{}),
		},
		{
			name:  "top_level present with an empty value",
			entry: withEmptyTopLevel(pageEntry("w", "u", "agent-1", promptItem("agent-1"))),
		},
		{
			name:  "page line with no book",
			entry: pageEntry("w", "u", "", promptItem("agent-1")),
		},
		{
			name:  "page line with no item arm",
			entry: pageEntry("w", "u", "agent-1", &storev1.StoreAgentItem{}),
		},
		{
			name:  "prompt with no recipient",
			entry: pageEntry("w", "u", "agent-1", promptItem("")),
		},
		{
			name:  "frame with no agent id",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Update{Update: proseUpdate("act-1")}})),
		},
		{
			name:  "frame with no result arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{AgentId: &conversationv1.AgentId{Value: "agent-1"}})),
		},
		{
			name:  "update with no update arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{AgentId: &conversationv1.AgentId{Value: "agent-1"}, Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{}}})),
		},
		{
			name:  "activity with no identity",
			entry: pageEntry("w", "u", "agent-1", frameItem(activityFrame("agent-1", "", prose()))),
		},
		{
			name:  "activity with no item arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(emptyActivityFrame("agent-1", "act-1"))),
		},
		{
			name:  "success with no outcome arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{AgentId: &conversationv1.AgentId{Value: "agent-1"}, Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{}}})),
		},
		{
			name:  "failure with no failure arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{AgentId: &conversationv1.AgentId{Value: "agent-1"}, Result: &conversationv1.AgentFrame_Failure{Failure: &conversationv1.AgentFailure{}}})),
		},
		{
			name:  "unserved item with no arm",
			entry: unservedEntry("w", "u", &storev1.StoreUnservedItem{}),
		},
		{
			name:  "the retired keepalive arm",
			entry: unservedEntry("w", "u", &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: promptItem("agent-1")}}),
		},
		{
			name:  "bash frame with no run identity",
			entry: bashEntry("w", "u", "", bashStart()),
		},
		{
			name:  "bash frame with no result arm",
			entry: bashEntry("w", "u", "run-1", &conversationv1.AgentBash{}),
		},
		{
			name:  "session update with no arm",
			entry: agentEntryWithSessionUpdate("w", "u", &conversationv1.SessionUpdate{}),
		},
		{
			name:  "detached work with no handle",
			entry: pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", createdWork("", bashWork())))),
		},
		{
			name:  "detached work with no origin arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", &conversationv1.AgentDetachedWork{Work: &conversationv1.DetachedWorkId{Value: "work-1"}}))),
		},
		{
			name:  "detached work created with no work arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", &conversationv1.DetachableWork{})))),
		},
		{
			name:  "detached work detached with no origin unit",
			entry: pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", detachedWork("work-1", "", causeRequestedArm())))),
		},
		{
			name:  "detached work detached with no cause arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", detachedWork("work-1", "act-1", nil)))),
		},
		{
			name:  "detached work output with no readability arm",
			entry: pageEntry("w", "u", "agent-1", frameItem(detachedFrame("agent-1", withOutput(createdWork("work-1", bashWork()), &conversationv1.DetachedWorkOutput{Path: "/tmp/spool"})))),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			_, err := classify(test.entry, 7)

			// Assert
			if !errors.Is(err, ErrInvalid) {
				t.Fatalf("error = %v, want ErrInvalid", err)
			}
		})
	}
}

func TestClassifyNamesTheOffendingEntryIndex(t *testing.T) {
	// Arrange: the detail is what a producer reads to find the bad record in a
	// batch it must now fix, so the index must survive into it.
	entry := pageEntry("", "u", "agent-1", promptItem("agent-1"))

	// Act
	_, err := classify(entry, 3)

	// Assert
	if err == nil {
		t.Fatal("classify accepted an entry with an empty write_id")
	}
	if want := "entries[3]"; !strings.Contains(err.Error(), want) {
		t.Fatalf("detail %q does not name %q", err, want)
	}
}

// ---- terminal detection ----

func TestActivityIsTerminalReadsEveryUnitVocabulary(t *testing.T) {
	// Arrange
	tests := []struct {
		name string
		item any
		want bool
	}{
		{name: "bash success concludes", item: bashSuccess(), want: true},
		{name: "bash start does not", item: bashStart(), want: false},
		{name: "subagent success concludes", item: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{}}}, want: true},
		{name: "subagent start does not", item: subagentStart("agent-2"), want: false},
		{name: "prose start does not", item: prose(), want: false},
		{name: "monitor ended concludes", item: &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Ended{Ended: &conversationv1.AgentMonitorEnded{}}}, want: true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := activityIsTerminal(terminalFixture(test.item))

			// Assert
			if got != test.want {
				t.Fatalf("activityIsTerminal = %t, want %t", got, test.want)
			}
		})
	}
}

func TestActivityIsTerminalIsFalseForAnItemWithNoArm(t *testing.T) {
	// Arrange
	activity := &conversationv1.AgentActivity{ActivityId: &conversationv1.AgentActivityId{Value: "act-1"}}

	// Act, Assert
	if activityIsTerminal(activity) {
		t.Fatal("an activity with no item arm was read as terminal")
	}
}

// ---- fixtures used only by this file ----

func withoutPlane(entry *storev1.StoreEntry) *storev1.StoreEntry {
	entry.Plane = nil
	return entry
}

func withEmptyTopLevel(entry *storev1.StoreEntry) *storev1.StoreEntry {
	entry.GetAgentUpdate().TopLevel = &conversationv1.AgentId{}
	return entry
}

func unservedEntry(writeID, upsertKey string, item *storev1.StoreUnservedItem) *storev1.StoreEntry {
	return agentUpdateEntry(writeID, upsertKey, &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: item},
	})
}

func bashEntry(writeID, upsertKey, run string, frame *conversationv1.AgentBash) *storev1.StoreEntry {
	return agentUpdateEntry(writeID, upsertKey, &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_Bash{Bash: &storev1.StoreAgentBash{
			Run:   &conversationv1.AgentActivityId{Value: run},
			Frame: frame,
		}},
	})
}

func workflowRunEntry(writeID, upsertKey, run string) *storev1.StoreEntry {
	return agentUpdateEntry(writeID, upsertKey, &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_Workflow{Workflow: &storev1.StoreAgentWorkflow{
			Run: &conversationv1.AgentId{Value: run},
			Frame: &conversationv1.AgentWorkflow{Result: &conversationv1.AgentWorkflow_Start{
				Start: &conversationv1.AgentWorkflowStart{Name: "nightly"},
			}},
		}},
	})
}

func sessionUpdateEntry(writeID, upsertKey string) *storev1.StoreEntry {
	return agentEntryWithSessionUpdate(writeID, upsertKey, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	})
}

func agentEntryWithSessionUpdate(writeID, upsertKey string, update *conversationv1.SessionUpdate) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:     streamPlane(),
		WriteId:   writeID,
		UpsertKey: upsertKey,
		Entry:     &storev1.StoreEntry_SessionUpdate{SessionUpdate: update},
	}
}

func bashStart() *conversationv1.AgentBash {
	return &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "make test"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 7},
	}}}
}

func bashFailure() *conversationv1.AgentBash {
	return &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{}}}
}

// terminalBash is the run's activity reaching a terminal arm IN THE SPAWNING
// AGENT'S OWN BOOK — the frame whose activity_id is the run's unit id and whose
// item has concluded, which is what closes the detached row by origin unit.
func terminalBash() *conversationv1.AgentBash {
	return bashSuccess()
}

func bashSuccess() *conversationv1.AgentBash {
	return &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "make test"},
	}}}
}

// rawRecord is the VERBATIM record every residue arm exists to carry. A fixture
// that omitted it was testing a refusal by accident.
func rawRecord(kind string) *structpb.Struct {
	raw, err := structpb.NewStruct(map[string]any{"type": kind})
	if err != nil {
		panic("shim-store db test: building a raw residue record: " + err.Error())
	}
	return raw
}

func bashWork() *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Bash{Bash: bashStart()}}
}

func subagentWork(createdAgentID string) *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Subagent{Subagent: subagentStart(createdAgentID)}}
}

func workflowWork() *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Workflow{
		Workflow: &conversationv1.AgentWorkflowStart{Name: "nightly"},
	}}
}

func monitorWork() *conversationv1.DetachableWork {
	return &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Monitor{
		Monitor: &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Start{
			Start: &conversationv1.AgentMonitorStart{Description: "watch the log"},
		}},
	}}
}

func createdWork(workID string, work *conversationv1.DetachableWork) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work:   &conversationv1.DetachedWorkId{Value: workID},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{WorkCreated: work}},
	}
}

func detachedWork(workID, originUnit string, cause any) *conversationv1.AgentDetachedWork {
	detached := &conversationv1.DetachedWorkDetached{}
	if originUnit != "" {
		detached.DetachedFromId = &conversationv1.AgentActivityId{Value: originUnit}
	}
	switch typed := cause.(type) {
	case *conversationv1.DetachedWorkDetached_Requested:
		detached.Cause = typed
	case *conversationv1.DetachedWorkDetached_ByUser:
		detached.Cause = typed
	case *conversationv1.DetachedWorkDetached_TimedOut:
		detached.Cause = typed
	}
	return &conversationv1.AgentDetachedWork{
		Work:   &conversationv1.DetachedWorkId{Value: workID},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: detached},
	}
}

func causeRequestedArm() *conversationv1.DetachedWorkDetached_Requested {
	return &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}}
}

func withOutput(work *conversationv1.AgentDetachedWork, output *conversationv1.DetachedWorkOutput) *conversationv1.AgentDetachedWork {
	work.Output = output
	return work
}

func proseUpdate(activityID string) *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: activityID},
		Item:       &conversationv1.AgentActivity_Response{Response: prose()},
	}}}
}

func emptyActivityFrame(agentID, activityID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: activityID},
			}},
		}},
	}
}

func contextCutFrame(agentID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{}},
		}},
	}
}

func apiErrorFrame(agentID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ApiError{ApiError: &conversationv1.ApiRequestFailed{}},
		}},
	}
}

func terminalFixture(item any) *conversationv1.AgentActivity {
	activity := &conversationv1.AgentActivity{ActivityId: &conversationv1.AgentActivityId{Value: "act-1"}}
	switch typed := item.(type) {
	case *conversationv1.AgentBash:
		activity.Item = &conversationv1.AgentActivity_Bash{Bash: typed}
	case *conversationv1.AgentSubagent:
		activity.Item = &conversationv1.AgentActivity_Subagent{Subagent: typed}
	case *conversationv1.AgentResponse:
		activity.Item = &conversationv1.AgentActivity_Response{Response: typed}
	case *conversationv1.AgentMonitor:
		activity.Item = &conversationv1.AgentActivity_Monitor{Monitor: typed}
	default:
		panic("unsupported activity item in test fixture")
	}
	return activity
}

// TestClassifyBlamesAFullEnvelopePath is the field-path vocabulary: the string
// a refusal blames is walkable from the StoreEntry root.
//
// A frame-depth refusal that blamed `agent_frame.agent_id` named a field that
// appears nowhere in the message the producer sent — the frame is reached
// through agent_update.serveable_frame.agent_item — so a caller had to guess
// the top of the path the store had already computed for it.
func TestClassifyBlamesAFullEnvelopePath(t *testing.T) {
	// Arrange
	entry := pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{
		Result: &conversationv1.AgentFrame_Update{Update: proseUpdate("act-1")},
	}))

	// Act
	_, err := classify(entry, 0)

	// Assert
	if err == nil {
		t.Fatal("classify accepted a frame with no agent id")
	}
	const want = "entries[0].agent_update.serveable_frame.agent_item.agent_frame.agent_id"
	if got := RefusalField(err); got != want {
		t.Fatalf("the refusal blames field %q, want the full envelope path %q", got, want)
	}
}

// A oneof arm that is SET but carries a nil message is a distinct breach from
// an arm that was never set: protobuf's getters hand back a zero value for
// both, so a validator that only checked the getter would accept the nil one
// and then route an entry with no content at all.
func TestClassifyRefusesAnArmSetToANilMessage(t *testing.T) {
	tests := []struct {
		name  string
		entry *storev1.StoreEntry
	}{
		{
			name:  "session_update arm set to nil",
			entry: agentEntryWithSessionUpdate("w", "u", nil),
		},
		{
			name:  "agent_update arm set to nil",
			entry: agentUpdateEntry("w", "u", nil),
		},
		{
			name: "serveable_frame arm set to nil",
			entry: agentUpdateEntry("w", "u", &storev1.StoreAgentUpdate{
				AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: nil},
			}),
		},
		{
			name:  "agent_item arm set to nil",
			entry: pageEntry("w", "u", "agent-1", nil),
		},
		{
			name: "agent_prompt arm set to nil",
			entry: pageEntry("w", "u", "agent-1", &storev1.StoreAgentItem{
				Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: nil},
			}),
		},
		{
			name:  "agent_frame arm set to nil",
			entry: pageEntry("w", "u", "agent-1", frameItem(nil)),
		},
		{
			name: "peer_message arm set to nil",
			entry: pageEntry("w", "u", "agent-1", &storev1.StoreAgentItem{
				Item: &storev1.StoreAgentItem_PeerMessage{PeerMessage: nil},
			}),
		},
		{
			name: "agent_frame.update arm set to nil",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{
				AgentId: &conversationv1.AgentId{Value: "agent-1"},
				Result:  &conversationv1.AgentFrame_Update{Update: nil},
			})),
		},
		{
			name: "activity arm set to nil",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{
				AgentId: &conversationv1.AgentId{Value: "agent-1"},
				Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
					Update: &conversationv1.AgentUpdate_Activity{Activity: nil},
				}},
			})),
		},
		{
			name:  "unserved_item arm set to nil",
			entry: unservedEntry("w", "u", nil),
		},
		{
			name: "bash arm set to nil",
			entry: agentUpdateEntry("w", "u", &storev1.StoreAgentUpdate{
				AgentInfo: &storev1.StoreAgentUpdate_Bash{Bash: nil},
			}),
		},
		{
			name: "workflow arm set to nil",
			entry: agentUpdateEntry("w", "u", &storev1.StoreAgentUpdate{
				AgentInfo: &storev1.StoreAgentUpdate_Workflow{Workflow: nil},
			}),
		},
		{
			name: "detached_work arm set to nil",
			entry: pageEntry("w", "u", "agent-1", frameItem(&conversationv1.AgentFrame{
				AgentId: &conversationv1.AgentId{Value: "agent-1"},
				Result:  &conversationv1.AgentFrame_DetachedWork{DetachedWork: nil},
			})),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			_, err := classify(test.entry, 7)

			// Assert
			if !errors.Is(err, ErrInvalid) {
				t.Fatalf("error = %v, want ErrInvalid", err)
			}
		})
	}
}

// The workflow arm carries the same two identity obligations as the bash arm —
// a run to join on and a result arm to state — and had neither refusal proven.
func TestClassifyRefusesAWorkflowMissingItsIdentityOrResult(t *testing.T) {
	tests := []struct {
		name     string
		workflow *storev1.StoreAgentWorkflow
	}{
		{
			name: "no run identity",
			workflow: &storev1.StoreAgentWorkflow{
				Frame: &conversationv1.AgentWorkflow{Result: &conversationv1.AgentWorkflow_Start{
					Start: &conversationv1.AgentWorkflowStart{Name: "nightly"},
				}},
			},
		},
		{
			name: "no result arm",
			workflow: &storev1.StoreAgentWorkflow{
				Run:   &conversationv1.AgentId{Value: "run-1"},
				Frame: &conversationv1.AgentWorkflow{},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			entry := agentUpdateEntry("w", "u", &storev1.StoreAgentUpdate{
				AgentInfo: &storev1.StoreAgentUpdate_Workflow{Workflow: test.workflow},
			})

			// Act
			_, err := classify(entry, 7)

			// Assert
			if !errors.Is(err, ErrInvalid) {
				t.Fatalf("error = %v, want ErrInvalid", err)
			}
		})
	}
}
