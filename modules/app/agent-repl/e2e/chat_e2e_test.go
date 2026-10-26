// chat_e2e_test.go — zettel chat threads, end to end: the real daemon, the
// real shim answering through the fake SDK (whose plain answer is
// "echo: <prompt> ..."), and the REAL zettel holding the graph.
//
// ONE TEST, MANY FACTS (owner ruling, 2026-10-24: chat e2e tests are few and
// packed). A thread's whole life is one world: every step below is a fact the
// next one builds on, and each is asserted where it happens, so a failure
// names the step that broke.
package e2e

import (
	"os"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/integration/harness"
)

// exchangesOf is a chat view's shown path, or none for an empty view.
func exchangesOf(v *frontendv1.ChatView) []*frontendv1.ChatExchange {
	return v.GetConversation().GetExchanges()
}

// settled reports whether V shows N exchanges with no question in flight, the
// last answered with WORDS.
func settled(v *frontendv1.ChatView, n int, words string) bool {
	ex := exchangesOf(v)
	return len(ex) == n && v.GetConversation().Pending == nil &&
		strings.Contains(ex[n-1].GetAnswer().GetMarkdown().GetText(), words)
}

// graphOf reads THREAD's nodes from the world's zettel, oldest first.
func graphOf(t *testing.T, w *World, thread *workspacev1.WorkspaceRef) []*zettelv1.Node {
	t.Helper()
	resp, err := w.Zettel.Client.GetThread(w.Ctx(), connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: thread.GetId()}}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("zettel GetThread = %v, %v", resp, err)
	}
	return resp.Msg.GetSuccess().GetNodes()
}

// threadRow is THREAD's row in ROSTER's thread section, or nil.
func threadRow(r *frontendv1.WorkspaceRoster, thread *workspacev1.WorkspaceRef) *frontendv1.RosterThreadRow {
	for _, row := range r.GetThreads().GetRows() {
		if row.GetThread().GetId() == thread.GetId() {
			return row
		}
	}
	return nil
}

// searched answers the threads SearchChats finds for QUERY.
func searched(t *testing.T, w *World, query string) []string {
	t.Helper()
	resp, err := w.Client().SearchChats(w.Ctx(), connect.NewRequest(&agentreplv1.SearchChatsRequest{Query: query}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SearchChats(%q) = %v, %v", query, resp, err)
	}
	var ids []string
	for _, m := range resp.Msg.GetSuccess().GetMatches() {
		ids = append(ids, m.GetThread().GetId())
	}
	return ids
}

func TestAChatThreadLivesItsWholeLife(t *testing.T) {
	w := NewWorld(t, WorldOpts{Zettel: true})
	ctx := w.Ctx()
	roster := w.WatchRoster()
	defer roster.Close()

	// ---- A thread starts from its first question, and opens ----
	created, err := w.Client().CreateThread(ctx, connect.NewRequest(&agentreplv1.CreateThreadRequest{
		Question: &agentreplv1.CreateThreadQuestion{Text: "Why is the sky blue?"},
	}))
	if err != nil || created.Msg.GetSuccess() == nil {
		t.Fatalf("CreateThread = %v, %v", created, err)
	}
	thread := created.Msg.GetSuccess().GetThread()
	if !strings.Contains(thread.GetDir(), "/zettel/threads/t-") {
		t.Fatalf("thread dir = %q, want a token directory under the zettel home", thread.GetDir())
	}
	chat := w.WatchChat(thread)
	defer chat.Close()
	view := harness.AwaitView(t, ctx, chat, "the first answer", func(v *frontendv1.ChatView) bool {
		return settled(v, 1, "echo: Why is the sky blue?")
	})
	root := exchangesOf(view)[0].GetNode()
	if nodes := graphOf(t, w, thread); len(nodes) != 1 || nodes[0].GetQuestion().GetText() != "Why is the sky blue?" {
		t.Fatalf("graph = %v, want the first question as its root", nodes)
	}
	harness.AwaitView(t, ctx, roster, "the thread listed open and current, under its title", func(r *frontendv1.WorkspaceRoster) bool {
		row := threadRow(r, thread)
		return row != nil && row.GetStanding().GetOpen() != nil && row.GetCurrent() != nil &&
			r.GetAppSwitcher().GetShown().GetChat() != nil && row.GetName().GetText() == harness.FakeClaudeMintedTitle
	})

	// ---- A question typed in the composer is asked of the shown path ----
	// It is long, so the outline draws it abbreviated (checked below, once
	// the root's branches make it a run of its own).
	const longFollowUp = "And what color does the sky turn at sunset, and why?"
	SubmitPrompt(t, w, thread, longFollowUp)
	view = harness.AwaitView(t, ctx, chat, "the follow-up's answer", func(v *frontendv1.ChatView) bool {
		return settled(v, 2, longFollowUp)
	})
	followUp := exchangesOf(view)[1].GetNode()
	if nodes := graphOf(t, w, thread); len(nodes) != 2 || nodes[1].GetParent().GetNode().GetNode().GetId() != root.GetId() {
		t.Fatalf("graph = %v, want the follow-up asked of the root", nodes)
	}

	// ---- A highlight is quoted, and the question asked of its node ----
	if _, err := w.Client().SelectChatText(ctx, connect.NewRequest(&agentreplv1.SelectChatTextRequest{
		Thread: thread,
		Selection: &agentreplv1.SelectChatTextSelection{State: &agentreplv1.SelectChatTextSelection_Highlighted{
			Highlighted: &agentreplv1.SelectChatTextHighlighted{Node: root, Quote: &zettelv1.NodeQuote{Text: "sky"}},
		}},
	})); err != nil {
		t.Fatalf("SelectChatText: %v", err)
	}
	SubmitPrompt(t, w, thread, "Why that word?")
	view = harness.AwaitView(t, ctx, chat, "the highlight's answer, the newest of the root's children", func(v *frontendv1.ChatView) bool {
		ex := exchangesOf(v)
		return settled(v, 2, "Why that word?") && ex[1].GetQuestion().GetQuote().GetText() == "sky"
	})
	highlighted := exchangesOf(view)[1]
	if highlighted.GetSiblings() == nil {
		t.Fatalf("exchange = %v, want a sibling control: the root has two children", highlighted)
	}

	// ---- The long question is abbreviated, in zettel and in the outline ----
	harness.AwaitView(t, ctx, roster, "the follow-up's outline row drawn abbreviated", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range threadRow(r, thread).GetOutline().GetRows() {
			if row.GetNode().GetId() == followUp.GetId() {
				return row.GetLabel().GetText() == harness.FakeClaudeMintedAbbreviation && row.GetLabel().GetQuestion() == longFollowUp
			}
		}
		return false
	})
	if nodes := graphOf(t, w, thread); nodes[1].GetQuestion().GetAbbreviation() != harness.FakeClaudeMintedAbbreviation || nodes[0].GetQuestion().Abbreviation != nil {
		t.Fatalf("graph = %v, want the long follow-up abbreviated and the short root not", nodes)
	}

	// ---- The siblings switch, and sightings are counted ----
	if _, err := w.Client().SwitchChatSibling(ctx, connect.NewRequest(&agentreplv1.SwitchChatSiblingRequest{
		Thread: thread, Shown: highlighted.GetNode(),
		Direction: &agentreplv1.SwitchChatSiblingDirection{Way: &agentreplv1.SwitchChatSiblingDirection_Previous{Previous: &agentreplv1.SwitchChatSiblingPrevious{}}},
	})); err != nil {
		t.Fatalf("SwitchChatSibling: %v", err)
	}
	harness.AwaitView(t, ctx, chat, "the follow-up shown again", func(v *frontendv1.ChatView) bool {
		ex := exchangesOf(v)
		return len(ex) == 2 && ex[1].GetNode().GetId() == followUp.GetId()
	})
	if _, err := w.Client().ReportChatSightings(ctx, connect.NewRequest(&agentreplv1.ReportChatSightingsRequest{
		Thread: thread, Nodes: []*zettelv1.NodeId{root},
	})); err != nil {
		t.Fatalf("ReportChatSightings: %v", err)
	}
	if nodes := graphOf(t, w, thread); nodes[0].GetSightings().GetCount() != 1 {
		t.Fatalf("root sightings = %v, want 1", nodes[0].GetSightings())
	}

	// ---- The chats are searched by what was asked and answered ----
	if got := searched(t, w, "sunset"); !slices.Equal(got, []string{thread.GetId()}) {
		t.Fatalf("SearchChats(sunset) = %v, want the thread", got)
	}
	if got := searched(t, w, "lifetimes"); len(got) != 0 {
		t.Fatalf("SearchChats(lifetimes) = %v, want none", got)
	}

	// ---- A thread's session restarts like any workspace's ----
	if resp, err := w.Client().RestartWorkspace(ctx, connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: thread})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, %v", resp, err)
	}

	// ---- Archived, a thread is listed last; restored, it answers again ----
	if resp, err := w.Client().CloseWorkspace(ctx, connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: thread, Requester: harness.User()})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseWorkspace = %v, %v", resp, err)
	}
	harness.AwaitView(t, ctx, roster, "the thread archived", func(r *frontendv1.WorkspaceRoster) bool {
		row := threadRow(r, thread)
		return row != nil && row.GetStanding().GetArchived() != nil
	})
	if resp, err := w.Client().OpenWorkspace(ctx, connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: thread})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace = %v, %v", resp, err)
	}
	harness.AwaitView(t, ctx, roster, "the thread restored", func(r *frontendv1.WorkspaceRoster) bool {
		row := threadRow(r, thread)
		return row != nil && row.GetStanding().GetOpen() != nil
	})
	SubmitPrompt(t, w, thread, "Still there?")
	harness.AwaitView(t, ctx, chat, "the restored thread's answer", func(v *frontendv1.ChatView) bool {
		return settled(v, 3, "Still there?")
	})

	// ---- Deleted, the thread and its graph are gone ----
	chat.Close()
	if resp, err := w.Client().NukeWorkspace(ctx, connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: thread})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("NukeWorkspace = %v, %v", resp, err)
	}
	if nodes := graphOf(t, w, thread); len(nodes) != 0 {
		t.Fatalf("graph after delete = %v, want none", nodes)
	}
	if _, statErr := os.Stat(thread.GetDir()); !os.IsNotExist(statErr) {
		t.Fatalf("the thread's directory survived its delete: %v", statErr)
	}
	if got := searched(t, w, "sunset"); len(got) != 0 {
		t.Fatalf("SearchChats(sunset) after delete = %v, want none", got)
	}
	harness.AwaitView(t, ctx, roster, "the thread gone from the list", func(r *frontendv1.WorkspaceRoster) bool {
		return threadRow(r, thread) == nil
	})
}
