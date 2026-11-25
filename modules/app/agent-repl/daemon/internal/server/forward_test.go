package server

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"syscall"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/h2cclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
)

// hostOf answers the host:port of a harness's server.
func hostOf(t *testing.T, h *harness) string {
	t.Helper()
	u, err := url.Parse(h.HTTP.URL)
	if err != nil {
		t.Fatal(err)
	}
	return u.Host
}

func TestACallIsServedByTheDaemonThatServesItsWorkspace(t *testing.T) {
	cases := []struct {
		name      string
		standing  workspace.Standing
		successor bool
		incumbent bool
		forwarded bool
		want      string
	}{
		{name: "a workspace this daemon serves is served here", standing: workspace.StandingOwned, successor: true, want: "here"},
		{name: "a transferred workspace is served by the successor", standing: workspace.StandingTransferringAway, successor: true, want: "there"},
		{name: "a workspace not yet adopted is served by the daemon its serving row names", standing: workspace.StandingNotYetAdopted, incumbent: true, want: "there"},
		{name: "a workspace whose row stands released for its adoption here is served here", standing: workspace.StandingNotYetAdopted, want: "here"},
		{name: "a call already forwarded is never forwarded again", standing: workspace.StandingTransferringAway, successor: true, forwarded: true, want: "here"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the serving daemon, and this one.
			there := newHarness(t)
			there.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: ref(), Name: "there"}
			target := hostOf(t, there)
			here := newHarness(t, func(d *Deps) {
				d.Ownership = &fakeOwnership{standing: tc.standing}
				d.SuccessorAddress = func() string {
					if tc.successor {
						return target
					}
					return ""
				}
				d.ServingElsewhere = func(context.Context, ids.WorkspaceID) (string, bool, error) {
					if tc.incumbent {
						return target, true, nil
					}
					return "", false, nil
				}
			})
			here.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: ref(), Name: "here"}
			req := connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()})
			if tc.forwarded {
				req.Header().Set(ForwardedHeader, "1")
			}

			// Act
			resp, err := here.Unmarked.GetWorkspaceStatus(context.Background(), req)

			// Assert
			if err != nil {
				t.Fatalf("GetWorkspaceStatus: %v", err)
			}
			if got := resp.Msg.GetSuccess().GetStatus().GetName(); got != tc.want {
				t.Fatalf("served by %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAForwardedStreamRelaysEveryMessageFromTheServingDaemon(t *testing.T) {
	// Arrange: the serving daemon's stream sends two messages and ends; this
	// daemon's forwards it.
	const procedure = "/agentrepl.v1.AgentRepl/WatchHostWorkspace"
	sawForwarded := make(chan bool, 1)
	thereMux := http.NewServeMux()
	thereMux.Handle(procedure, connect.NewServerStreamHandler(procedure,
		func(_ context.Context, req *connect.Request[agentreplv1.WatchHostWorkspaceRequest], stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse]) error {
			sawForwarded <- req.Header().Get(ForwardedHeader) != ""
			for range 2 {
				if err := stream.Send(&agentreplv1.WatchHostWorkspaceResponse{}); err != nil {
					return err
				}
			}
			return nil
		}))
	there := httptest.NewServer(h2c.NewHandler(thereMux, &http2.Server{}))
	defer there.Close()
	target, _ := url.Parse(there.URL)
	s := &server{log: dlog.NewTestLogger(), life: context.Background(), deps: Deps{Forward: h2cclient.Client()}}
	hereMux := http.NewServeMux()
	hereMux.Handle(procedure, connect.NewServerStreamHandler(procedure,
		func(ctx context.Context, req *connect.Request[agentreplv1.WatchHostWorkspaceRequest], stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse]) error {
			return forwardStream[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](ctx, s, "WatchHostWorkspace", target.Host, req, stream)
		}))
	here := httptest.NewServer(h2c.NewHandler(hereMux, &http2.Server{}))
	defer here.Close()
	client := connect.NewClient[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](h2cclient.Client(), here.URL+procedure)

	// Act
	stream, err := client.CallServerStream(context.Background(), connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("CallServerStream: %v", err)
	}
	defer stream.Close()
	received := 0
	for stream.Receive() {
		received++
	}

	// Assert
	if stream.Err() != nil || received != 2 {
		t.Fatalf("relayed %d messages (%v), want both", received, stream.Err())
	}
	if !<-sawForwarded {
		t.Fatal("the serving daemon saw no forwarded mark on the call")
	}
}

func TestACallWhoseStandingCannotBeReadIsAFailureNotAForward(t *testing.T) {
	// Arrange
	here := newHarness(t, func(d *Deps) {
		d.Ownership = &fakeOwnership{err: errors.New("the rollout is gone")}
	})

	// Act
	_, err := here.Unmarked.GetWorkspaceStatus(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()}))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the rollout is gone") {
		t.Fatalf("GetWorkspaceStatus = %v, want the standing's failure", err)
	}
}

func TestNewRefusesNoForwardingClientOrSharedAddressSource(t *testing.T) {
	cases := []struct {
		name    string
		without func(*Deps)
		want    string
	}{
		{name: "no forwarding client", without: func(d *Deps) { d.Forward = nil }, want: "the forwarding client"},
		{name: "no shared address source", without: func(d *Deps) { d.SharedAddress = nil }, want: "a shared address source"},
		{name: "no daemon instance address lookup", without: func(d *Deps) { d.InstanceAddress = nil }, want: "the daemon instance address lookup"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			deps := h.withoutDep(tc.without)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("New = %v, want the refusal naming %s", err, tc.want)
			}
		})
	}
}

func TestARefusalNamesTheSharedAddressNeverAPrivateOne(t *testing.T) {
	cases := []struct {
		name     string
		standing workspace.Standing
	}{
		{name: "transferring_away", standing: workspace.StandingTransferringAway},
		{name: "not_yet_adopted", standing: workspace.StandingNotYetAdopted},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a forwarded call reaches a daemon that does not serve it.
			h := newHarness(t)
			h.SharedAddress = "127.0.0.1:41000"
			h.Ownership.standing = tc.standing

			// Act
			s := h.Server.(*server)
			got := s.fill(refusal{Arm: map[workspace.Standing]string{
				workspace.StandingTransferringAway: workspace.ArmTransferringAway,
				workspace.StandingNotYetAdopted:    workspace.ArmNotYetAdopted,
			}[tc.standing]})

			// Assert
			if got.Fields["address"] != "127.0.0.1:41000" {
				t.Fatalf("the refusal named %v, want the shared address", got.Fields["address"])
			}
		})
	}
}

func TestARelayedStreamEndsWithTheForwardingDaemonsLife(t *testing.T) {
	// Arrange: the serving daemon's stream sends one message and stands.
	const procedure = "/agentrepl.v1.AgentRepl/WatchHostWorkspace"
	thereMux := http.NewServeMux()
	thereMux.Handle(procedure, connect.NewServerStreamHandler(procedure,
		func(ctx context.Context, _ *connect.Request[agentreplv1.WatchHostWorkspaceRequest], stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse]) error {
			if err := stream.Send(&agentreplv1.WatchHostWorkspaceResponse{}); err != nil {
				return err
			}
			<-ctx.Done()
			return nil
		}))
	there := httptest.NewServer(h2c.NewHandler(thereMux, &http2.Server{}))
	defer there.Close()
	target, _ := url.Parse(there.URL)
	life, endLife := context.WithCancel(context.Background())
	defer endLife()
	s := &server{log: dlog.NewTestLogger(), life: life, deps: Deps{Forward: h2cclient.Client()}}
	hereMux := http.NewServeMux()
	hereMux.Handle(procedure, connect.NewServerStreamHandler(procedure,
		func(ctx context.Context, req *connect.Request[agentreplv1.WatchHostWorkspaceRequest], stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse]) error {
			return forwardStream[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](ctx, s, "WatchHostWorkspace", target.Host, req, stream)
		}))
	here := httptest.NewServer(h2c.NewHandler(hereMux, &http2.Server{}))
	defer here.Close()
	client := connect.NewClient[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](h2cclient.Client(), here.URL+procedure)
	stream, err := client.CallServerStream(context.Background(), connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("CallServerStream: %v", err)
	}
	defer stream.Close()
	if !stream.Receive() {
		t.Fatalf("the first relayed message never came: %v", stream.Err())
	}

	// Act: the forwarding daemon begins its exit.
	endLife()

	// Assert: the relayed stream ends, so the client re-opens it elsewhere.
	if stream.Receive() {
		t.Fatal("the relayed stream sent again after the forwarding daemon's life ended, want it ended")
	}
}

func TestAnAdoptCallGoesToTheSuccessorWhileAHandoverHasOne(t *testing.T) {
	cases := []struct {
		name      string
		rpc       string
		successor string
		want      string
		wantOK    bool
	}{
		{name: "a web adopt goes to the successor", rpc: "AdoptWebWorkspace", successor: "127.0.0.1:2", want: "127.0.0.1:2", wantOK: true},
		{name: "a host adopt goes to the successor", rpc: "AdoptHostWorkspace", successor: "127.0.0.1:2", want: "127.0.0.1:2", wantOK: true},
		{name: "an adopt with no handover in flight is decided by its standing", rpc: "AdoptWebWorkspace"},
		{name: "any other call is decided by its standing", rpc: "GetWorkspaceStatus", successor: "127.0.0.1:2"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := adoptTarget(tc.rpc, tc.successor)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("adoptTarget(%q, %q) = (%q, %v), want (%q, %v)", tc.rpc, tc.successor, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestRequestInstanceNamesTheDaemonACallNames(t *testing.T) {
	attachment := &agentreplv1.PageAttachment{Instance: "daemon-there", Sequence: 3}
	cases := []struct {
		name      string
		msg       proto.Message
		want      string
		wantNamed bool
	}{
		{name: "a subscribe names the daemon that issued its attachment", msg: &agentreplv1.SubscribePageRequest{Attachment: attachment}, want: "daemon-there", wantNamed: true},
		{name: "an unsubscribe names the daemon that issued its attachment", msg: &agentreplv1.UnsubscribePageRequest{Attachment: attachment}, want: "daemon-there", wantNamed: true},
		{name: "a feed tail names the daemon that minted its token", msg: &agentreplv1.WatchFeedRequest{Watch: &agentreplv1.FeedWatchToken{Value: "ft-1", Instance: "daemon-there"}}, want: "daemon-there", wantNamed: true},
		{name: "a page call with no attachment still names no workspace to route by", msg: &agentreplv1.SubscribePageRequest{
			Request: &agentreplv1.SubscribePageRequest_Footer{Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()}},
		}, wantNamed: true},
		{name: "any other call names no daemon", msg: &agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, named := requestInstance(tc.msg)

			// Assert
			if got != tc.want || named != tc.wantNamed {
				t.Fatalf("requestInstance = (%q, %v), want (%q, %v)", got, named, tc.want, tc.wantNamed)
			}
		})
	}
}

func TestAPageCallIsServedByTheDaemonItsAttachmentNames(t *testing.T) {
	lookupFailed := errors.New("the life lock could not be probed")
	cases := []struct {
		name      string
		instance  string
		forwarded bool
		lookupErr error
		want      string
		wantErr   bool
	}{
		{name: "this daemon's own attachment is served here, whichever daemon serves the arm's workspace", instance: string(testInstance)},
		{name: "another running daemon's attachment goes to that daemon", instance: "daemon-there", want: "127.0.0.1:2"},
		{name: "an attachment of a daemon that no longer runs is answered here", instance: "daemon-that-exited"},
		{name: "a call already forwarded is never forwarded again", instance: "daemon-there", forwarded: true},
		{name: "a daemon that cannot be looked up is a failure, not a forward", instance: "daemon-there", lookupErr: lookupFailed, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a workspace handed to a successor, which the page call
			// must NOT be routed by.
			log := &recordingLogger{}
			h := newHarness(t, func(d *Deps) {
				d.Log = &fakeSurfaces{global: log}
				d.Ownership = &fakeOwnership{standing: workspace.StandingTransferringAway}
				d.SuccessorAddress = func() string { return "127.0.0.1:9" }
				if tc.lookupErr != nil {
					d.InstanceAddress = func(ids.InstanceID) (string, bool, error) { return "", false, tc.lookupErr }
				}
			})
			h.Instances = map[ids.InstanceID]string{"daemon-there": "127.0.0.1:2"}
			header := http.Header{}
			if tc.forwarded {
				header.Set(ForwardedHeader, "1")
			}
			msg := footerSubscription("page-1", &agentreplv1.PageAttachment{Instance: tc.instance, Sequence: 1})

			// Act
			target, ok, err := h.Server.(*server).forwardTarget(context.Background(), "SubscribePage", header, msg)

			// Assert
			if (err != nil) != tc.wantErr || target != tc.want || ok != (tc.want != "") {
				t.Fatalf("forwardTarget = (%q, %v, %v), want %q (error %v)", target, ok, err, tc.want, tc.wantErr)
			}
			if tc.wantErr {
				var recorded bool
				for _, rec := range log.at("ERROR") {
					if rec.Operation == "daemon.server.forward" && rec.Context["instance"] == tc.instance && rec.Context["rpc"] == "SubscribePage" {
						recorded = true
					}
				}
				if !recorded {
					t.Fatalf("the failed lookup was not recorded; got %v", log.records())
				}
			}
		})
	}
}

func TestAnAdoptCallGoesWhereItsWorkspacesRendezvousIs(t *testing.T) {
	cases := []struct {
		name      string
		rpc       string
		standing  workspace.Standing
		elsewhere string
		// armed is whether this daemon armed the workspace's rendezvous as
		// the joining daemon.
		armed bool
		want  string
	}{
		{name: "a workspace this daemon serves adopts on its successor", standing: workspace.StandingOwned, want: "successor"},
		{name: "a workspace this daemon handed on adopts on its successor", standing: workspace.StandingTransferringAway, want: "successor"},
		{name: "a workspace an older daemon still serves adopts there, never on this daemon's successor", standing: workspace.StandingNotYetAdopted, elsewhere: "elder", want: "elder"},
		{name: "a workspace this daemon is adopting adopts here", standing: workspace.StandingNotYetAdopted},
		{name: "a workspace whose rendezvous this daemon armed adopts here, though an older daemon still holds its serving row", standing: workspace.StandingNotYetAdopted, elsewhere: "elder", armed: true},
		{name: "a call other than an adoption for a workspace whose rendezvous this daemon armed still goes to the older daemon holding its serving row", rpc: "SubmitPrompt", standing: workspace.StandingNotYetAdopted, elsewhere: "elder", armed: true, want: "elder"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a handover to a successor is in flight.
			rpc := tc.rpc
			if rpc == "" {
				rpc = "AdoptWebWorkspace"
			}
			h := newHarness(t, func(d *Deps) {
				d.Rollout = &fakeRollout{rendezvousArmed: tc.armed}
				d.Ownership = &fakeOwnership{standing: tc.standing}
				d.SuccessorAddress = func() string { return "successor" }
				d.ServingElsewhere = func(context.Context, ids.WorkspaceID) (string, bool, error) {
					return tc.elsewhere, tc.elsewhere != "", nil
				}
			})

			// Act
			target, ok, err := h.Server.(*server).servingTarget(context.Background(), rpc, "ws-1")

			// Assert
			if err != nil || target != tc.want || ok != (tc.want != "") {
				t.Fatalf("servingTarget = (%q, %v, %v), want %q", target, ok, err, tc.want)
			}
		})
	}
}

func TestAForwardedStreamIsAcceptedWhenItsTargetAcceptsWithNothingPushed(t *testing.T) {
	// Arrange: the serving daemon accepts a web link and pushes nothing.
	there := newHarness(t, func(d *Deps) { d.Instance = "daemon-there" })
	here := servingElsewhereAt(t, hostOf(t, there))
	ctx, cancel := context.WithTimeout(context.Background(), relayBound)
	defer cancel()

	// Act
	stream, err := here.Unmarked.WatchWebWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "webapp-test"}))
	if err != nil {
		t.Fatalf("open the stream: %v", err)
	}
	header := stream.ResponseHeader()

	// Assert
	if header.Get(AcceptedHeader) == "" {
		t.Fatalf("the forwarded stream stated no acceptance within %s", relayBound)
	}
}

// sinkOf collects what a piped stream wrote.
type sinkOf[R any] struct{ got []*R }

func (s *sinkOf[R]) Send(msg *R) error {
	s.got = append(s.got, msg)
	return nil
}

func TestPipeStreamStatesItsTargetsAcceptanceOnce(t *testing.T) {
	refusal := connect.NewError(connect.CodeFailedPrecondition, errors.New("refused"))
	cases := []struct {
		name string
		// states is whether the target states acceptance in its headers.
		states bool
		// messages is how many messages the target sends.
		messages int
		// end is what the target's stream returns.
		end          error
		wantAccepted int
		wantErr      bool
	}{
		{name: "a target that states acceptance and pushes nothing is accepted", states: true, wantAccepted: 1},
		{name: "a target that states acceptance and pushes is accepted once", states: true, messages: 2, wantAccepted: 1},
		{name: "a target that pushes without stating acceptance is accepted by its first message", messages: 2, wantAccepted: 1},
		{name: "a target that refuses is never accepted, and its refusal is the answer", end: refusal, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			target := scriptedFooterDaemon(t, func(_ context.Context, _ *connect.Request[agentreplv1.WatchFooterRequest], stream *connect.ServerStream[agentreplv1.WatchFooterResponse]) error {
				if tc.states {
					stream.ResponseHeader().Set(AcceptedHeader, "1")
				}
				for range tc.messages {
					if err := stream.Send(&agentreplv1.WatchFooterResponse{}); err != nil {
						return err
					}
				}
				return tc.end
			})
			s := &server{log: dlog.NewTestLogger(), life: context.Background(), deps: Deps{Forward: h2cclient.Client()}}
			sink := &sinkOf[agentreplv1.WatchFooterResponse]{}
			accepted := 0

			// Act
			err := pipeStream[agentreplv1.WatchFooterRequest, agentreplv1.WatchFooterResponse](context.Background(), s, "WatchFooter", target,
				agentreplv1connect.AgentReplWatchFooterProcedure, connect.NewRequest(&agentreplv1.WatchFooterRequest{Workspace: ref()}), sink,
				func() { accepted++ })

			// Assert
			if (err != nil) != tc.wantErr || accepted != tc.wantAccepted || len(sink.got) != tc.messages {
				t.Fatalf("pipeStream = %v with %d acceptances and %d messages, want error %v, %d and %d",
					err, accepted, len(sink.got), tc.wantErr, tc.wantAccepted, tc.messages)
			}
		})
	}
}

// refusingAddress answers a loopback address nothing listens on: a listener
// opened and closed, as an exiting daemon's private listener is.
func refusingAddress(t *testing.T) string {
	t.Helper()
	closed := httptest.NewServer(http.NotFoundHandler())
	u, err := url.Parse(closed.URL)
	if err != nil {
		t.Fatal(err)
	}
	closed.Close()
	return u.Host
}

func TestTargetGoneReadsARefusedConnection(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{name: "a refused connection, as Connect wraps it", err: connect.NewError(connect.CodeUnavailable, &url.Error{Op: "Post", URL: "http://127.0.0.1:1", Err: syscall.ECONNREFUSED}), want: true},
		{name: "a target's own unavailable answer", err: connect.NewError(connect.CodeUnavailable, errors.New("the workspace is standing down"))},
		{name: "a cancelled call", err: context.Canceled},
		{name: "no error", err: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := targetGone(tc.err); got != tc.want {
				t.Fatalf("targetGone(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}

// THE 2026-11-19 FAILURE, PINNED: an exiting daemon closes its listener
// before its life lock is freed, so a call forwarded to it in that span is
// refused. It is answered here, as a call for a daemon that no longer runs
// is, and never fails at the client.
func TestACallForwardedToADaemonThatNoLongerAcceptsIsAnsweredHere(t *testing.T) {
	t.Run("a page call for an exiting daemon's attachment is answered page_gone", func(t *testing.T) {
		// Arrange: the attachment's daemon still holds its life lock, and
		// its listener is closed.
		log := &recordingLogger{}
		h := newHarness(t, func(d *Deps) { d.Log = &fakeSurfaces{global: log} })
		gone := refusingAddress(t)
		h.Instances = map[ids.InstanceID]string{"daemon-exiting": gone}
		msg := footerSubscription("page-1", &agentreplv1.PageAttachment{Instance: "daemon-exiting", Sequence: 1})

		// Act
		resp, err := h.Unmarked.SubscribePage(context.Background(), connect.NewRequest(msg))

		// Assert
		if err != nil || resp.Msg.GetSuccess().GetPageGone() == nil {
			t.Fatalf("SubscribePage = (%v, %v), want page_gone answered here", resp, err)
		}
		var recorded bool
		for _, rec := range log.at("INFO") {
			if rec.Operation == "daemon.server.forward" && rec.Context["rpc"] == "SubscribePage" && rec.Context["target"] == gone &&
				rec.Message == "the daemon a call was forwarded to no longer accepts connections; the call is answered here" {
				recorded = true
			}
		}
		if !recorded {
			t.Fatalf("the refused forward was not recorded; got %v", log.records())
		}
	})
	t.Run("a call for a workspace whose serving daemon is exiting is answered here", func(t *testing.T) {
		// Arrange
		gone := refusingAddress(t)
		here := newHarness(t, func(d *Deps) {
			d.Ownership = &fakeOwnership{standing: workspace.StandingTransferringAway}
			d.SuccessorAddress = func() string { return gone }
		})
		here.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: ref(), Name: "here"}

		// Act
		resp, err := here.Unmarked.GetWorkspaceStatus(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()}))

		// Assert
		if err != nil || resp.Msg.GetSuccess().GetStatus().GetName() != "here" {
			t.Fatalf("GetWorkspaceStatus = (%v, %v), want it served here", resp, err)
		}
	})
}

func TestPipeStreamAnswersATargetThatNoLongerAcceptsAsGone(t *testing.T) {
	// Arrange
	const procedure = "/agentrepl.v1.AgentRepl/WatchHostWorkspace"
	log := dlog.NewTestLogger()
	s := &server{log: log, life: context.Background(), deps: Deps{Forward: h2cclient.Client()}}
	gone := refusingAddress(t)
	accepted := false

	// Act
	err := pipeStream[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](context.Background(), s, "WatchHostWorkspace", gone, procedure,
		connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}), discardSink[agentreplv1.WatchHostWorkspaceResponse]{}, func() { accepted = true })

	// Assert
	if !errors.Is(err, errTargetGone) || accepted {
		t.Fatalf("pipeStream = %v (accepted %v), want errTargetGone and nothing accepted", err, accepted)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.server.forward",
		Message: "the daemon a stream was forwarded to no longer accepts connections; the stream is served here"}); !ok {
		t.Fatalf("records = %v, want the refused stream recorded", log.Records())
	}
}

// discardSink is a stream sink that keeps nothing.
type discardSink[Resp any] struct{}

func (discardSink[Resp]) Send(*Resp) error { return nil }
