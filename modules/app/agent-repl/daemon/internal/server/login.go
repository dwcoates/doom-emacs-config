package server

import (
	"context"
	"errors"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/login"
)

// The login flow and the two link verbs. The login pty's output is a SERVER
// stream (a WKWebView cannot speak a bidirectional Connect stream) and the
// input direction is the unary SendLoginInput.

// OpenLogin begins, or joins, the account login flow the daemon owns.
func (s *server) OpenLogin(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenLoginRequest],
) (*connect.Response[agentreplv1.OpenLoginResponse], error) {
	const rpc = "OpenLogin"
	resp := &agentreplv1.OpenLoginResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	configDir, err := s.deps.Login.Open(ctx, subject.Record.ID)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
			Arm:    "spawn_failed",
			Reason: err.Error(),
		})))
	}
	subject.Log.Info("daemon.server.open_login", "opened a login flow",
		dlog.Context{"config_dir": configDir})
	resp.Result = &agentreplv1.OpenLoginResponse_Success{
		Success: &agentreplv1.OpenLoginSuccess{ConfigDir: configDir},
	}
	return connect.NewResponse(resp), nil
}

// WatchLoginTerminal streams the login pty's raw output, replaying the
// scrollback on attach. It ends on the pty's `closed` frame or on the client's
// cancellation.
func (s *server) WatchLoginTerminal(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchLoginTerminalRequest],
	out *connect.ServerStream[agentreplv1.LoginTerminalOutput],
) error {
	return s.watchLoginTerminal(ctx, req.Msg, out)
}

// watchLoginTerminal is the body, written to whatever sink carries it.
func (s *server) watchLoginTerminal(
	ctx context.Context,
	msg *agentreplv1.WatchLoginTerminalRequest,
	out streamSink[agentreplv1.LoginTerminalOutput],
) error {
	const rpc = "WatchLoginTerminal"
	if err := validateWorkspaceRef("workspace", msg.GetWorkspace()); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, msg.GetWorkspace())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}

	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	frames, err := s.deps.Login.Watch(streamCtx, subject.Record.ID)
	if err != nil {
		if errors.Is(err, login.ErrNoSession) {
			return TransportClosed(subject.Log, rpc, "no_login_open", err.Error(), false)
		}
		return endStream(subject.Log, rpc, err)
	}
	s.acceptStream(ctx, rpc)
	subject.Log.Debug(rpc, "accepted a login terminal stream", nil)

	for {
		select {
		case <-streamCtx.Done():
			subject.Log.Debug(rpc, "the login terminal stream ended on cancellation", nil)
			return nil
		case frame, ok := <-frames:
			if !ok {
				subject.Log.Debug(rpc, "the login terminal's source closed", nil)
				return nil
			}
			if err := out.Send(loginFrame(frame)); err != nil {
				subject.Log.Debug(rpc, "the login terminal's client went away",
					dlog.Context{"cause": err.Error()})
				return nil
			}
			if frame.Closed {
				subject.Log.Debug(rpc, "the login pty ended; the stream's last frame was sent", nil)
				return nil
			}
		}
	}
}

// loginFrame renders one pty frame. `closed` is the stream's LAST frame, and
// the two arms are never both set.
func loginFrame(frame login.Output) *agentreplv1.LoginTerminalOutput {
	if frame.Closed {
		return &agentreplv1.LoginTerminalOutput{
			Output: &agentreplv1.LoginTerminalOutput_Closed{
				Closed: &agentreplv1.LoginTerminalClosed{},
			},
		}
	}
	return &agentreplv1.LoginTerminalOutput{
		Output: &agentreplv1.LoginTerminalOutput_Bytes{
			Bytes: &agentreplv1.LoginTerminalBytes{Data: frame.Bytes},
		},
	}
}

// SendLoginInput carries keystrokes or a resize into the login pty.
func (s *server) SendLoginInput(
	ctx context.Context,
	req *connect.Request[agentreplv1.SendLoginInputRequest],
) (*connect.Response[agentreplv1.SendLoginInputResponse], error) {
	const rpc = "SendLoginInput"
	if err := validateSendLoginInputRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SendLoginInputResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var err error
	if keystrokes := req.Msg.GetKeystrokes(); keystrokes != nil {
		err = s.deps.Login.SendKeystrokes(ctx, subject.Record.ID, keystrokes.GetData())
	} else {
		resize := req.Msg.GetResize()
		err = s.deps.Login.SendResize(ctx, subject.Record.ID, login.Resize{
			Rows: resize.GetRows(), Cols: resize.GetCols(),
		})
	}
	if err != nil {
		if errors.Is(err, login.ErrNoSession) {
			return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
				Arm: "no_login_open", Reason: err.Error(),
			})))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	resp.Result = &agentreplv1.SendLoginInputResponse_Success{
		Success: &agentreplv1.SendLoginInputSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// CloseLogin ends the login session. Closing an ABSENT one is a success.
func (s *server) CloseLogin(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseLoginRequest],
) (*connect.Response[agentreplv1.CloseLoginResponse], error) {
	const rpc = "CloseLogin"
	resp := &agentreplv1.CloseLoginResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Login.Close(ctx, subject.Record.ID); err != nil && !errors.Is(err, login.ErrNoSession) {
		return nil, fail(subject.Log, rpc, err)
	}
	resp.Result = &agentreplv1.CloseLoginResponse_Success{
		Success: &agentreplv1.CloseLoginSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// OpenExternal opens a clicked link in the pinned external browser profile.
func (s *server) OpenExternal(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenExternalRequest],
) (*connect.Response[agentreplv1.OpenExternalResponse], error) {
	const rpc = "OpenExternal"
	if err := validateOpenExternalRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.OpenExternalResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.OpenExternal(ctx, subject.Record.ID, req.Msg.GetUrl()); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.OpenExternalResponse_Success{
		Success: &agentreplv1.OpenExternalSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// OpenInEditor RELAYS a web link click onto the workspace's host stream. The
// daemon validates the workspace and opens nothing itself: no ack, no command
// loop.
func (s *server) OpenInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenInEditorRequest],
) (*connect.Response[agentreplv1.OpenInEditorResponse], error) {
	const rpc = "OpenInEditor"
	if err := validateOpenInEditorRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.OpenInEditorResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	switch target := req.Msg.GetTarget().(type) {
	case *agentreplv1.OpenInEditorRequest_WorkspaceFile:
		var line *uint32
		if target.WorkspaceFile.Line != nil {
			value := target.WorkspaceFile.GetLine()
			line = &value
		}
		if err := s.deps.Verbs.OpenInEditor(ctx, subject.Record.ID, target.WorkspaceFile.GetPath(), line); err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
	case *agentreplv1.OpenInEditorRequest_MergeTestLog:
		// THE LOG LIVES IN THE DAEMON'S STATE, outside the worktree, so it is
		// named by the token the bubble served and resolved here, for this
		// workspace, rather than by a path a client could choose.
		path, err := s.deps.Merge.TestLogPath(ctx, subject.Record.ID, target.MergeTestLog.GetValue())
		if err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
		if err := s.deps.Verbs.OpenDaemonFileInEditor(ctx, subject.Record.ID, path); err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
	case *agentreplv1.OpenInEditorRequest_FeedLink:
		report := target.FeedLink.GetReport() != nil
		unresolved, err := s.deps.Verbs.OpenFeedLink(ctx, subject.Record.ID, target.FeedLink.GetHref(), report)
		if unresolved != nil {
			// THE QUESTION IS SENT BEFORE THE REFUSAL IS ANSWERED: the arm
			// promises the workspace has already been asked.
			if askErr := s.askAboutUnresolvedLink(ctx, subject.Log, subject.Record.ID, target.FeedLink.GetSourceRow(), unresolved); askErr != nil {
				return nil, fail(subject.Log, rpc, askErr)
			}
		}
		if err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
	}
	resp.Result = &agentreplv1.OpenInEditorResponse_Success{
		Success: &agentreplv1.OpenInEditorSuccess{},
	}
	return connect.NewResponse(resp), nil
}
