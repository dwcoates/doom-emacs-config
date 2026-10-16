package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"sync"
	"time"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shellword"
	"agent-repld/internal/wsm"
)

// opDesktopNotify is the operation the notifier's boot records carry.
const opDesktopNotify = "daemon.cmd.desktop_notify"

// resolveBannerBackend resolves the platform's banner program once, at boot.
// click is the command a banner the platform holds runs when it is clicked.
// deliveredBound bounds each query of which banners the platform still holds
// (resolveNotifierListBound).
func resolveBannerBackend(ctx context.Context, log dlog.Logger, click desktopnotify.ClickAction, deliveredBound time.Duration) (desktopnotify.Backend, error) {
	return resolveNotifyProgram(log, "banner", desktopnotify.EnvNotifierCmd,
		func(goos, override string) (desktopnotify.Backend, error) {
			platform, err := desktopnotify.PlatformFor(goos, click)
			if err != nil {
				return nil, err
			}
			return desktopnotify.NewBackend(ctx, platform, override, exec.LookPath, desktopnotify.ExecRunner{}, deliveredBound)
		})
}

// envNotifierListBound overrides desktopnotify.DefaultDeliveredBound. It
// exists for the integration suite, whose banner program is a recorder
// writing to a throttled RAM disk rather than the platform's program the
// default is measured against.
const envNotifierListBound = "AGENT_REPL_NOTIFIER_LIST_BOUND"

// resolveNotifierListBound reads the banner query bound's override. Empty is
// desktopnotify.DefaultDeliveredBound; a malformed or non-positive value is a
// REFUSAL, as every duration knob's is.
func resolveNotifierListBound(value string) (time.Duration, error) {
	return resolveDurationKnob(envNotifierListBound, value, desktopnotify.DefaultDeliveredBound)
}

// selectWorkspaceClick is the click a banner the platform holds runs: this
// daemon binary's own `call SelectWorkspace` for the banner's workspace,
// against this daemon's state root.
//
// THE CLICK REACHES WHICHEVER DAEMON SERVES WHEN IT LANDS. The command names
// the state root, not an address, so a banner clicked after a restart or a
// deploy selects the workspace on the daemon serving then; the binary path is
// the deployed one, which a deploy replaces in place. The state root is
// spelled out because the platform runs the command in a bare environment,
// without the $AGENT_REPL_STATE_DIR this daemon may have been started with.
func selectWorkspaceClick(selfExe, stateDir string) desktopnotify.ClickAction {
	return func(target desktopnotify.Target) string {
		type ref struct {
			ID  string `json:"id"`
			Dir string `json:"dir"`
		}
		request, err := json.Marshal(struct {
			Workspace ref `json:"workspace"`
		}{Workspace: ref{ID: string(target.ID), Dir: target.Dir}})
		if err != nil {
			panic(fmt.Sprintf("agent-repld: encode a SelectWorkspace request of two strings: %v", err))
		}
		return "exec " + shellword.Quote(selfExe) + " " + callVerb +
			" -state-dir " + shellword.Quote(stateDir) +
			" SelectWorkspace " + shellword.Quote(string(request))
	}
}

// resolveChime resolves the platform's chime player once, at boot: the one
// sound every desktop banner rings the instant it is made.
func resolveChime(log dlog.Logger) (desktopnotify.Chime, error) {
	return resolveNotifyProgram(log, "chime", desktopnotify.EnvChimeCmd,
		func(goos, override string) (desktopnotify.Chime, error) {
			platform, err := desktopnotify.ChimePlatformFor(goos)
			if err != nil {
				return nil, err
			}
			return desktopnotify.NewChime(platform, override, exec.LookPath, desktopnotify.ExecRunner{})
		})
}

// resolveNotifyProgram builds one of the notifier's programs for this GOOS,
// overridden by $env when set. A platform with none, or a program that is not
// installed, is recorded at ERROR and answered as the reason every use will
// record: the daemon still serves, and each banner or chime that cannot run
// says why.
func resolveNotifyProgram[T interface{ Program() string }](log dlog.Logger, role, env string, build func(goos, override string) (T, error)) (T, error) {
	program, err := build(runtime.GOOS, os.Getenv(env))
	if err != nil {
		log.Error(opDesktopNotify, "no desktop "+role+" program; it will not run", dlog.Context{
			"role": role, "goos": runtime.GOOS, "cause": err.Error(), "override_env": env,
		})
		var none T
		return none, err
	}
	log.Info(opDesktopNotify, "resolved the desktop "+role+" program", dlog.Context{
		"role": role, "program": program.Program(), "goos": runtime.GOOS,
	})
	return program, nil
}

// bannerWorkspaces answers a banner's workspace from the registry: the name
// it is titled by, which is also the name its tab carries, and the directory
// its click's WorkspaceRef echoes.
type bannerWorkspaces struct {
	db wsm.DB
}

// Workspace answers the workspace's registry name and directory.
func (w bannerWorkspaces) Workspace(ctx context.Context, ws ids.WorkspaceID) (desktopnotify.Workspace, error) {
	record, err := w.db.Workspace(ctx, ws)
	if err != nil {
		return desktopnotify.Workspace{}, fmt.Errorf("read workspace %q for its banner: %w", ws, err)
	}
	return desktopnotify.Workspace{Name: record.Name, Dir: record.Dir}, nil
}

// clickForwarder carries a READ-BACK banner's click to the server's host
// stream. The
// notifier is built before the server exists, so the server is bound late,
// exactly as the host relay is.
type clickForwarder struct {
	mu     sync.RWMutex
	target desktopnotify.ClickSink
}

func (f *clickForwarder) bind(target desktopnotify.ClickSink) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

// NotificationClicked relays the click. A click can only follow a banner the
// serving daemon posted, so an unbound forwarder is a broken boot order.
func (f *clickForwarder) NotificationClicked(ws ids.WorkspaceID) {
	f.mu.RLock()
	target := f.target
	f.mu.RUnlock()
	if target == nil {
		panic("agent-repld: a banner click arrived before the server was bound")
	}
	target.NotificationClicked(ws)
}
