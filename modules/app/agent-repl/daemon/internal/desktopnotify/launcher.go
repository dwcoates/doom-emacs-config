package desktopnotify

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"time"

	"google.golang.org/protobuf/encoding/protodelim"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	launcherv1 "agentrepl/proto/launcher/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opLauncher is the operation every post to the launcher is recorded under.
const opLauncher = "daemon.desktopnotify.launcher"

// LauncherProgram names the macOS launcher, for the record.
const LauncherProgram = "Zettel.app"

// EnvLauncherSocket names the launcher's socket, overriding
// DefaultLauncherSocket; the daemon's -launcher-socket flag beats it.
const EnvLauncherSocket = "AGENT_REPL_LAUNCHER_SOCKET"

// DefaultLauncherSocket is the socket the launcher's socket holder keeps
// (app.zettel.sockethold.launcher, launcher/requirements.json).
const DefaultLauncherSocket = "~/.cache/agent-repl/sock/launcher.sock"

// LauncherPostBound bounds one post: the dial, the request, and the
// launcher's answer, which it writes once macOS's notification center took
// or refused the banner. That is milliseconds while the launcher accepts. A
// dial made while the launcher restarts queues in the held socket's backlog
// until it accepts again, so the bound also covers a launcher restart. NOT
// YET MEASURED: every post records its duration_ms, from which the bound is
// to be resized.
const LauncherPostBound = 5 * time.Second

// ErrNotAllowed is a banner macOS refused because the user has not allowed
// Zettel to post notifications.
var ErrNotAllowed = errors.New("macOS refused the banner: Zettel is not allowed to post notifications (System Settings → Notifications → Zettel)")

// Launcher is the macOS Backend: the launcher (Zettel.app) posts every
// banner, and each Post hands it one over the launcher's own socket.
//
// THE LAUNCHER IS THE SERVER. Its socket belongs to a socket holder, not to
// any daemon or to the launcher, so a daemon standing down, or either daemon
// of a handover, posts exactly as a serving one does (launcher.v1).
type Launcher struct {
	socket string
	pid    int32
	bound  time.Duration
	dial   func(ctx context.Context, socket string) (net.Conn, error)
	log    dlog.Logger
}

// NewLauncher builds the launcher's Backend posting over socket within bound
// (LauncherPostBound in production).
func NewLauncher(socket string, bound time.Duration, log dlog.Logger) *Launcher {
	return newLauncher(socket, bound, log, func(ctx context.Context, socket string) (net.Conn, error) {
		var d net.Dialer
		return d.DialContext(ctx, "unix", socket)
	})
}

// newLauncher is NewLauncher with its dial injected.
func newLauncher(socket string, bound time.Duration, log dlog.Logger, dial func(ctx context.Context, socket string) (net.Conn, error)) *Launcher {
	if socket == "" || bound <= 0 || log == nil || dial == nil {
		panic("desktopnotify: a launcher needs its socket, a positive bound, a logger and a dial")
	}
	return &Launcher{socket: socket, pid: int32(os.Getpid()), bound: bound, dial: dial, log: log}
}

// Program implements Backend.
func (l *Launcher) Program() string { return LauncherProgram }

// Post implements Backend: it hands the banner to the launcher and answers
// once the launcher says what macOS did with it. A banner macOS took is made
// and answered Handed; its click reaches the daemon later, on its own
// (ReportDesktopBannerClicked, Notifier.Clicked).
func (l *Launcher) Post(ctx context.Context, target Target, banner Banner, made func()) (Outcome, error) {
	began := time.Now()
	ctx, cancel := context.WithTimeout(ctx, l.bound)
	defer cancel()
	outcome, err := l.exchange(ctx, &launcherv1.PostBannerRequest{
		Banner: &agentreplv1.DesktopBanner{
			Title: &agentreplv1.DesktopBannerTitle{Text: banner.Title},
			Body:  &agentreplv1.DesktopBannerBody{Text: banner.Body},
			Group: &agentreplv1.DesktopBannerGroup{Id: bannerGroup(target.ID)},
			Click: &agentreplv1.DesktopBannerClickToken{Workspace: wsm.FamilyRef{ID: target.ID, Dir: target.Dir}.Ref()},
		},
		Daemon: &launcherv1.PostingDaemon{Pid: l.pid},
	})
	fields := dlog.Context{"socket": l.socket, "workspace": string(target.ID), "duration_ms": time.Since(began).Milliseconds()}
	if err != nil {
		return Unclicked, err
	}
	switch arm := outcome.GetOutcome().(type) {
	case *agentreplv1.DesktopBannerPostOutcome_Posted:
		l.log.Debug(opLauncher, "the launcher posted the banner", fields)
		made()
		return Handed, nil
	case *agentreplv1.DesktopBannerPostOutcome_NotAllowed:
		return Unclicked, ErrNotAllowed
	case *agentreplv1.DesktopBannerPostOutcome_Failed:
		return Unclicked, fmt.Errorf("macOS refused the banner: %s", arm.Failed.GetPlatformError())
	default:
		return Unclicked, fmt.Errorf("the launcher answered the banner with no outcome this daemon knows (%T)", arm)
	}
}

// exchange is one connection: the request written, the answer read.
func (l *Launcher) exchange(ctx context.Context, req *launcherv1.PostBannerRequest) (*agentreplv1.DesktopBannerPostOutcome, error) {
	conn, err := l.dial(ctx, l.socket)
	if err != nil {
		return nil, fmt.Errorf("could not reach the launcher at %s; is app.zettel.sockethold.launcher loaded? %w", l.socket, err)
	}
	defer conn.Close()
	if deadline, ok := ctx.Deadline(); ok {
		if err := conn.SetDeadline(deadline); err != nil {
			return nil, fmt.Errorf("bound the exchange with the launcher at %s: %w", l.socket, err)
		}
	}
	if _, err := protodelim.MarshalTo(conn, req); err != nil {
		return nil, fmt.Errorf("hand the banner to the launcher at %s: %w", l.socket, err)
	}
	var resp launcherv1.PostBannerResponse
	if err := (protodelim.UnmarshalOptions{}).UnmarshalFrom(bufio.NewReader(conn), &resp); err != nil {
		return nil, fmt.Errorf("the launcher at %s did not answer the banner within %s: %w", l.socket, l.bound, err)
	}
	switch arm := resp.GetResult().(type) {
	case *launcherv1.PostBannerResponse_Outcome:
		return arm.Outcome, nil
	case *launcherv1.PostBannerResponse_Refused:
		return nil, fmt.Errorf("the launcher refused the banner: %s", arm.Refused.GetReason())
	default:
		return nil, fmt.Errorf("the launcher answered the banner with nothing this daemon knows (%T)", arm)
	}
}
