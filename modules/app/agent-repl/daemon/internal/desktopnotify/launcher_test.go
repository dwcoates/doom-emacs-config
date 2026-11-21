package desktopnotify

import (
	"bufio"
	"context"
	"errors"
	"io"
	"net"
	"os"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/encoding/protodelim"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	launcherv1 "agentrepl/proto/launcher/v1"

	"agent-repld/internal/dlog"
)

// launcherTarget is the fixture's banner target.
var launcherTarget = Target{ID: "ws1", Dir: "/work/ws1"}

// fakeLauncher serves one exchange on the far end of a pipe: it reads the
// request, hands it back on got, and writes answer (none when nil).
func fakeLauncher(t *testing.T, answer *launcherv1.PostBannerResponse) (dial func(context.Context, string) (net.Conn, error), got <-chan *launcherv1.PostBannerRequest) {
	t.Helper()
	requests := make(chan *launcherv1.PostBannerRequest, 1)
	dial = func(context.Context, string) (net.Conn, error) {
		daemonEnd, launcherEnd := net.Pipe()
		go func() {
			defer launcherEnd.Close()
			var req launcherv1.PostBannerRequest
			if err := (protodelim.UnmarshalOptions{}).UnmarshalFrom(bufio.NewReader(launcherEnd), &req); err != nil {
				t.Errorf("the fake launcher could not read the request: %v", err)
				return
			}
			requests <- &req
			if answer == nil {
				// Hold the connection unanswered until the daemon hangs up.
				_, _ = io.Copy(io.Discard, launcherEnd)
				return
			}
			// An empty answer's zero-length body is a write the daemon, done
			// reading, never takes: a hung-up pipe is the end of that exchange.
			if _, err := protodelim.MarshalTo(launcherEnd, answer); err != nil && !errors.Is(err, io.ErrClosedPipe) {
				t.Errorf("the fake launcher could not answer: %v", err)
			}
		}()
		return daemonEnd, nil
	}
	return dial, requests
}

// outcomeAnswer is the launcher's answer carrying outcome.
func outcomeAnswer(outcome *agentreplv1.DesktopBannerPostOutcome) *launcherv1.PostBannerResponse {
	return &launcherv1.PostBannerResponse{Result: &launcherv1.PostBannerResponse_Outcome{Outcome: outcome}}
}

func TestLauncherPostHandsTheBannerAndIsMadeOnceMacOSTookIt(t *testing.T) {
	// Arrange
	dial, got := fakeLauncher(t, outcomeAnswer(&agentreplv1.DesktopBannerPostOutcome{
		Outcome: &agentreplv1.DesktopBannerPostOutcome_Posted{Posted: &agentreplv1.DesktopBannerPostOutcomePosted{}},
	}))
	l := newLauncher("/sock/launcher.sock", time.Minute, dlog.NewTestLogger(), dial)
	made := false

	// Act
	outcome, err := l.Post(context.Background(), launcherTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

	// Assert
	if err != nil || outcome != Handed || !made {
		t.Fatalf("Post = %v, %v (made %v), want Handed and made", outcome, err, made)
	}
	req := <-got
	b := req.GetBanner()
	if b.GetTitle().GetText() != "T" || b.GetBody().GetText() != "B" || b.GetGroup().GetId() != bannerGroup("ws1") {
		t.Fatalf("banner = %v, want the title, body and the workspace's group", b)
	}
	if ref := b.GetClick().GetWorkspace(); ref.GetId() != "ws1" || ref.GetDir() != "/work/ws1" {
		t.Fatalf("click token = %v, want the target's workspace", ref)
	}
	if req.GetDaemon().GetPid() != int32(os.Getpid()) {
		t.Fatalf("daemon pid = %d, want this process's", req.GetDaemon().GetPid())
	}
}

func TestLauncherPostSurfacesEveryRefusal(t *testing.T) {
	cases := []struct {
		name   string
		answer *launcherv1.PostBannerResponse
		want   string
	}{
		{"macOS does not allow Zettel to notify", outcomeAnswer(&agentreplv1.DesktopBannerPostOutcome{
			Outcome: &agentreplv1.DesktopBannerPostOutcome_NotAllowed{NotAllowed: &agentreplv1.DesktopBannerPostOutcomeNotAllowed{}},
		}), ErrNotAllowed.Error()},
		{"macOS refused it for another reason", outcomeAnswer(&agentreplv1.DesktopBannerPostOutcome{
			Outcome: &agentreplv1.DesktopBannerPostOutcome_Failed{Failed: &agentreplv1.DesktopBannerPostOutcomeFailed{PlatformError: "center busy"}},
		}), "center busy"},
		{"the launcher could not read the request", &launcherv1.PostBannerResponse{Result: &launcherv1.PostBannerResponse_Refused{
			Refused: &launcherv1.PostBannerRefused{Reason: "banner.title is empty"},
		}}, "banner.title is empty"},
		{"the launcher answered with no arm", &launcherv1.PostBannerResponse{}, "nothing this daemon knows"},
		{"the launcher answered an outcome with no arm", outcomeAnswer(&agentreplv1.DesktopBannerPostOutcome{}), "no outcome this daemon knows"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dial, _ := fakeLauncher(t, tc.answer)
			l := newLauncher("/sock/launcher.sock", time.Minute, dlog.NewTestLogger(), dial)
			made := false

			// Act
			outcome, err := l.Post(context.Background(), launcherTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) || outcome != Unclicked || made {
				t.Fatalf("Post = %v, %v (made %v), want Unclicked naming %q and nothing made", outcome, err, made, tc.want)
			}
		})
	}
}

func TestLauncherPostFailsWhenTheLauncherCannotBeReached(t *testing.T) {
	// Arrange
	dial := func(context.Context, string) (net.Conn, error) { return nil, errors.New("connect: no such file or directory") }
	l := newLauncher("/sock/launcher.sock", time.Minute, dlog.NewTestLogger(), dial)
	made := false

	// Act
	outcome, err := l.Post(context.Background(), launcherTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

	// Assert
	if err == nil || !strings.Contains(err.Error(), "/sock/launcher.sock") || !strings.Contains(err.Error(), "no such file") || outcome != Unclicked || made {
		t.Fatalf("Post = %v, %v (made %v), want an error naming the socket and its cause", outcome, err, made)
	}
}

func TestLauncherPostFailsWhenNoAnswerArrivesWithinTheBound(t *testing.T) {
	// Arrange
	dial, got := fakeLauncher(t, nil)
	l := newLauncher("/sock/launcher.sock", 50*time.Millisecond, dlog.NewTestLogger(), dial)
	made := false

	// Act
	outcome, err := l.Post(context.Background(), launcherTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

	// Assert
	<-got
	if err == nil || !strings.Contains(err.Error(), "did not answer") || outcome != Unclicked || made {
		t.Fatalf("Post = %v, %v (made %v), want an unanswered post", outcome, err, made)
	}
}

func TestNewLauncherRefusesAMissingDependency(t *testing.T) {
	dial := func(context.Context, string) (net.Conn, error) { return nil, nil }
	cases := []struct {
		name  string
		build func()
	}{
		{"no socket", func() { newLauncher("", time.Second, dlog.NewTestLogger(), dial) }},
		{"no bound", func() { newLauncher("/s", 0, dlog.NewTestLogger(), dial) }},
		{"no logger", func() { newLauncher("/s", time.Second, nil, dial) }},
		{"no dial", func() { newLauncher("/s", time.Second, dlog.NewTestLogger(), nil) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			defer func() {
				if recover() == nil {
					t.Fatal("a launcher missing a dependency was built")
				}
			}()

			// Act
			tc.build()
		})
	}
}

func TestLauncherPostsOverARealUnixSocket(t *testing.T) {
	// Arrange
	dir, err := os.MkdirTemp("", "lch")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	socket := dir + "/l.sock"
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		conn, err := ln.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		var req launcherv1.PostBannerRequest
		if err := (protodelim.UnmarshalOptions{}).UnmarshalFrom(bufio.NewReader(conn), &req); err != nil {
			t.Errorf("read the request: %v", err)
			return
		}
		_, _ = protodelim.MarshalTo(conn, outcomeAnswer(&agentreplv1.DesktopBannerPostOutcome{
			Outcome: &agentreplv1.DesktopBannerPostOutcome_Posted{Posted: &agentreplv1.DesktopBannerPostOutcomePosted{}},
		}))
	}()
	l := NewLauncher(socket, time.Minute, dlog.NewTestLogger())

	// Act
	outcome, err := l.Post(context.Background(), launcherTarget, Banner{Title: "T", Body: "B"}, func() {})

	// Assert
	if err != nil || outcome != Handed {
		t.Fatalf("Post = %v, %v, want Handed", outcome, err)
	}
}
