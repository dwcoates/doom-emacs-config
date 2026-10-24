package server

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/chat"
	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// THE CHAT VERBS: a zettel chat thread's creation, its view and the gestures
// that move it. A thread is a workspace (wsm.WorkspaceKindThread), so each verb
// resolves it as any workspace is resolved, and refuses a workspace that is
// not a thread.

// Chat is the chat service, as the server drives it (chat.Service).
type Chat interface {
	// Topic is a thread's chat view topic.
	Topic(thread ids.WorkspaceID) *publish.Topic[*frontendv1.ChatView]
	// Open loads a thread and publishes its view.
	Open(ctx context.Context, thread ids.WorkspaceID) error
	// Ask decides where the thread's next question is asked from.
	Ask(ctx context.Context, thread ids.WorkspaceID) (*zettelv1.NodeParent, error)
	// Asked is told a question asked from ASKED was submitted.
	Asked(ctx context.Context, thread ids.WorkspaceID, asked *zettelv1.NodeParent) error
	// SwitchSibling shows a shown node's previous or next sibling.
	SwitchSibling(ctx context.Context, thread ids.WorkspaceID, shown chat.NodeID, way chat.Direction) error
	// Select holds a highlight, or clears it (nil).
	Select(ctx context.Context, thread ids.WorkspaceID, h *chat.Highlight) error
	// Sightings counts one sighting per named node.
	Sightings(ctx context.Context, thread ids.WorkspaceID, nodes []chat.NodeID) error
}

// threadSubject resolves a chat verb's `thread` ref as subjectFor resolves a
// workspace's, and refuses a workspace that is not a thread with not_a_thread.
func (s *server) threadSubject(ctx context.Context, rpc string, ref *workspacev1.WorkspaceRef, resp proto.Message) (resolved, *connect.Error, bool) {
	subject, cerr, done := s.subjectForField(ctx, rpc, "thread", ref, resp)
	if done {
		return subject, cerr, true
	}
	if !subject.Record.IsThread() {
		return resolved{}, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
			Arm: "not_a_thread", Reason: fmt.Sprintf("workspace %q is not a chat thread", subject.Record.ID),
		})), true
	}
	return subject, nil, false
}

// threadStart is a thread's first question's parent.
func threadStart() *zettelv1.NodeParent {
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}
}

// CreateThread makes a new chat thread and submits its first question.
func (s *server) CreateThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateThreadRequest],
) (*connect.Response[agentreplv1.CreateThreadResponse], error) {
	const rpc = "CreateThread"
	text := req.Msg.GetQuestion().GetText()
	if strings.TrimSpace(text) == "" {
		return nil, invalid("question.text", "a thread starts from a question")
	}
	resp := &agentreplv1.CreateThreadResponse{}
	record, err := s.deps.Verbs.CreateThread(ctx, text)
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	log := s.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(record.ID)})
	// THE FIRST QUESTION GOES THE ONE SUBMISSION PATH, asked from the start of
	// the thread, under a key minted for this thread's creation so the
	// submission is claimed once.
	if _, err := s.deps.Prompts.SubmitThreadQuestion(ctx, record.ID, workspace.SaidText(text),
		"create-thread:"+string(record.ID), conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&wsm.ThreadAsk{Parent: threadStart()}); err != nil {
		return nil, fail(log, rpc, fmt.Errorf("submit the thread's first question: %w", err))
	}
	// THE NEW THREAD OPENS (owner ruling, 2026-10-24: "on Enter the new thread
	// opens in the webapp"): it is selected, and the roster's `current' moves
	// the editor onto it once its workspace is open there.
	if err := s.deps.Verbs.Select(ctx, record.ID, workspace.RecordSelection); err != nil {
		return nil, fail(log, rpc, fmt.Errorf("the new thread %q was created but could not be selected: %w", record.ID, err))
	}
	log.Info(rpc, "created a thread, submitted its first question and selected it", dlog.Context{"thread": string(record.ID), "name": record.Name})
	resp.Result = &agentreplv1.CreateThreadResponse_Success{Success: &agentreplv1.CreateThreadSuccess{Thread: record.Ref()}}
	return connect.NewResponse(resp), nil
}

// RequestNewThread asks every connected Emacs to open its new-thread prompt:
// the sidebar's new-thread button, since the webapp takes no typed input.
func (s *server) RequestNewThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequestNewThreadRequest],
) (*connect.Response[agentreplv1.RequestNewThreadResponse], error) {
	const rpc = "RequestNewThread"
	resp := &agentreplv1.RequestNewThreadResponse{}
	push := &agentreplv1.WatchDaemonResponse{Push: &agentreplv1.WatchDaemonResponse_NewThreadRequested{
		NewThreadRequested: &agentreplv1.DaemonNewThreadRequested{},
	}}
	reached := s.pushToEditors(rpc, push, func(*daemonWatcher) bool { return true }, nil)
	if reached == 0 {
		return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{Arm: "no_editor", Reason: "no Emacs is connected to open the new-thread prompt"})))
	}
	s.log.Info(rpc, "asked the editors to open the new-thread prompt", dlog.Context{"editors": reached})
	resp.Result = &agentreplv1.RequestNewThreadResponse_Success{Success: &agentreplv1.RequestNewThreadSuccess{}}
	return connect.NewResponse(resp), nil
}

// WatchChat serves one thread's chat view.
func (s *server) WatchChat(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchChatRequest],
	out *connect.ServerStream[agentreplv1.WatchChatResponse],
) error {
	return s.watchChat(ctx, req.Msg, out)
}

// watchChat is the body, written to whatever sink carries it.
func (s *server) watchChat(ctx context.Context, msg *agentreplv1.WatchChatRequest, out streamSink[agentreplv1.WatchChatResponse]) error {
	const rpc = "WatchChat"
	if err := validateWorkspaceRef("thread", msg.GetThread()); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, msg.GetThread())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}
	if !subject.Record.IsThread() {
		return refuseStream(subject.Log, rpc, s.fill(refusal{
			Arm: "not_a_thread", Reason: fmt.Sprintf("workspace %q is not a chat thread", subject.Record.ID),
		}))
	}
	// LOADED AND PUBLISHED BEFORE SUBSCRIBING, so the topic's latest value is
	// the opening push. The chat service logged a failure where it happened.
	if err := s.deps.Chat.Open(ctx, subject.Record.ID); err != nil {
		return connect.NewError(connect.CodeUnavailable, fmt.Errorf("open the thread's chat view: %w", err))
	}
	// AN ANSWER THE DAEMON NEVER RECORDED -- one that came while it was down,
	// or before it could reach zettel -- is learned again from the newest
	// page's replay, which tells the chat service every concluded answer it
	// draws (feed.ChatSink.TurnAnswered, recorded idempotently by turn).
	go s.replayThreadAnswers(ctx, subject)
	return serveTopic(s, ctx, rpc, subject.Log, s.deps.Chat.Topic(subject.Record.ID), out,
		func(view *frontendv1.ChatView) *agentreplv1.WatchChatResponse {
			return &agentreplv1.WatchChatResponse{View: view}
		})
}

// SwitchChatSibling shows the sibling before or after a shown node.
func (s *server) SwitchChatSibling(
	ctx context.Context,
	req *connect.Request[agentreplv1.SwitchChatSiblingRequest],
) (*connect.Response[agentreplv1.SwitchChatSiblingResponse], error) {
	const rpc = "SwitchChatSibling"
	if strings.TrimSpace(req.Msg.GetShown().GetId()) == "" {
		return nil, invalid("shown.id", "the shown node is required")
	}
	var way chat.Direction
	switch req.Msg.GetDirection().GetWay().(type) {
	case *agentreplv1.SwitchChatSiblingDirection_Previous:
		way = chat.Previous
	case *agentreplv1.SwitchChatSiblingDirection_Next:
		way = chat.Next
	default:
		return nil, invalid("direction.way", "a direction is required")
	}
	resp := &agentreplv1.SwitchChatSiblingResponse{}
	subject, cerr, done := s.threadSubject(ctx, rpc, req.Msg.GetThread(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Chat.SwitchSibling(ctx, subject.Record.ID, chat.NodeID(req.Msg.GetShown().GetId()), way); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SwitchChatSiblingResponse_Success{Success: &agentreplv1.SwitchChatSiblingSuccess{}}
	return connect.NewResponse(resp), nil
}

// ReportChatSightings counts one sighting per node that came fully on screen.
func (s *server) ReportChatSightings(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportChatSightingsRequest],
) (*connect.Response[agentreplv1.ReportChatSightingsResponse], error) {
	const rpc = "ReportChatSightings"
	if len(req.Msg.GetNodes()) == 0 {
		return nil, invalid("nodes", "at least one node is required")
	}
	nodes := make([]chat.NodeID, 0, len(req.Msg.GetNodes()))
	for i, n := range req.Msg.GetNodes() {
		if strings.TrimSpace(n.GetId()) == "" {
			return nil, invalid(fmt.Sprintf("nodes[%d].id", i), "a node id is required")
		}
		nodes = append(nodes, chat.NodeID(n.GetId()))
	}
	resp := &agentreplv1.ReportChatSightingsResponse{}
	subject, cerr, done := s.threadSubject(ctx, rpc, req.Msg.GetThread(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Chat.Sightings(ctx, subject.Record.ID, nodes); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.ReportChatSightingsResponse_Success{Success: &agentreplv1.ReportChatSightingsSuccess{}}
	return connect.NewResponse(resp), nil
}

// SelectChatText holds the words highlighted in an answer, or clears them.
func (s *server) SelectChatText(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectChatTextRequest],
) (*connect.Response[agentreplv1.SelectChatTextResponse], error) {
	const rpc = "SelectChatText"
	var highlight *chat.Highlight
	switch state := req.Msg.GetSelection().GetState().(type) {
	case *agentreplv1.SelectChatTextSelection_Highlighted:
		if strings.TrimSpace(state.Highlighted.GetNode().GetId()) == "" {
			return nil, invalid("selection.highlighted.node.id", "the highlighted node is required")
		}
		if strings.TrimSpace(state.Highlighted.GetQuote().GetText()) == "" {
			return nil, invalid("selection.highlighted.quote.text", "the highlighted words are required")
		}
		highlight = &chat.Highlight{Node: chat.NodeID(state.Highlighted.GetNode().GetId()), Quote: state.Highlighted.GetQuote()}
	case *agentreplv1.SelectChatTextSelection_Cleared:
	default:
		return nil, invalid("selection.state", "a selection is highlighted or cleared")
	}
	resp := &agentreplv1.SelectChatTextResponse{}
	subject, cerr, done := s.threadSubject(ctx, rpc, req.Msg.GetThread(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Chat.Select(ctx, subject.Record.ID, highlight); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SelectChatTextResponse_Success{Success: &agentreplv1.SelectChatTextSuccess{}}
	return connect.NewResponse(resp), nil
}

// submitThreadQuestion is SubmitPrompt's body for a chat thread: the question
// is asked from where the chat service says (the held highlight, else the end
// of the shown path), quoting the highlighted words for the model, and goes
// the prompt handler's thread path.
func (s *server) submitThreadQuestion(
	ctx context.Context,
	subject resolved,
	req *agentreplv1.SubmitPromptRequest,
	resp *agentreplv1.SubmitPromptResponse,
) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	const rpc = "SubmitPrompt"
	parent, err := s.deps.Chat.Ask(ctx, subject.Record.ID)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			refused.Op = opSubmit
			return answer(resp, s.refuse(subject.Log, rpc, resp, submitRefusal(refused)))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	said := req.GetSaid()
	if words := parent.GetNode().GetQuote().GetText(); words != "" {
		said = replyquote.Highlighted(said, words)
	}
	outcome, err := s.deps.Prompts.SubmitThreadQuestion(ctx, subject.Record.ID, said,
		req.GetIdempotencyKey(), req.GetOrigin(), &wsm.ThreadAsk{Parent: parent})
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, submitRefusal(modelActRefused(err, bubbleRefused(askRefused(refused))))))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	cerr := s.encodeSubmitOutcome(subject.Log, resp, outcome)
	if cerr == nil && resp.GetError() == nil {
		// THE VIEW IS READ AGAIN for the question now in flight. The chat
		// service logged a failure where it happened; the question itself is
		// submitted either way, and the next edge reads the view again.
		if err := s.deps.Chat.Asked(ctx, subject.Record.ID, parent); err != nil && !errors.Is(err, context.Canceled) {
			subject.Log.Debug(rpc, "the thread's view was not read again after the submission", dlog.Context{"cause": err.Error()})
		}
	}
	return answer(resp, cerr)
}

// ShowSidebarApp shows one of the sidebar's apps by SELECTING that app's most
// recent workspace: which app the sidebar shows follows what is on screen, so
// the roster's `current` moves the editor exactly as a row click does.
func (s *server) ShowSidebarApp(
	ctx context.Context,
	req *connect.Request[agentreplv1.ShowSidebarAppRequest],
) (*connect.Response[agentreplv1.ShowSidebarAppResponse], error) {
	const rpc = "ShowSidebarApp"
	var threads bool
	switch req.Msg.GetApp().GetApp().(type) {
	case *frontendv1.SidebarApp_Workspaces:
	case *frontendv1.SidebarApp_Chat:
		threads = true
	default:
		return nil, invalid("app.app", "an app is required")
	}
	resp := &agentreplv1.ShowSidebarAppResponse{}
	records, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the workspaces: %w", err))
	}
	target, ok := mostRecentOf(records, threads)
	if !ok {
		return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
			Arm: "nothing_to_show", Reason: fmt.Sprintf("the app has no workspace to show (threads: %v)", threads), Info: true,
		})))
	}
	log := s.deps.Log.WorkspaceOrCentral(target.Dir).With(dlog.Context{"workspace": string(target.ID)})
	if err := s.deps.Verbs.Select(ctx, target.ID, workspace.RecordSelection); err != nil {
		return answer(resp, s.answerRefusal(log, rpc, resp, err, nil))
	}
	s.deps.Banners.Forget("a sidebar app was shown")
	log.Info(rpc, "showed a sidebar app by selecting its most recent workspace", dlog.Context{"threads": threads})
	resp.Result = &agentreplv1.ShowSidebarAppResponse_Success{Success: &agentreplv1.ShowSidebarAppSuccess{RosterRevision: s.deps.Sidebar.Revision()}}
	return connect.NewResponse(resp), nil
}

// mostRecentOf is the open, unhidden workspace of one app (the threads, or the
// repository workspaces) the user last interacted with.
func mostRecentOf(records []wsm.Workspace, threads bool) (wsm.Workspace, bool) {
	var best wsm.Workspace
	found := false
	for _, r := range records {
		if r.IsThread() != threads || r.Closed || r.Hidden {
			continue
		}
		if !found || r.LastInteractedAt().After(best.LastInteractedAt()) {
			best, found = r, true
		}
	}
	return best, found
}

// replayThreadAnswers reads a thread's newest feed page, so the chat service
// is told every answer it draws. No history source up is ordinary: the page is
// read when the session is.
func (s *server) replayThreadAnswers(ctx context.Context, subject resolved) {
	const op = "daemon.server.chat_replay"
	err := s.deps.Feed.ReadNewest(ctx, subject.Record.ID)
	switch {
	case err == nil:
		subject.Log.Debug(op, "the thread's newest page was read for its answers", nil)
	case errors.Is(err, feed.ErrNoHistorySource), ctxend.Is(err):
		subject.Log.Debug(op, "the thread's newest page was not read for its answers", dlog.Context{"cause": err.Error()})
	default:
		subject.Log.Error(op, "the thread's newest page could not be read for its answers", dlog.Context{"cause": err.Error()})
	}
}
