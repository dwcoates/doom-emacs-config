package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"sync"

	"claude-repld/internal/desktopnotify"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// opDesktopNotify is the operation the notifier's boot records carry.
const opDesktopNotify = "daemon.cmd.desktop_notify"

// resolveBannerBackend resolves the platform's banner program once, at boot.
func resolveBannerBackend(log dlog.Logger) (desktopnotify.Backend, error) {
	return resolveNotifyProgram(log, "banner", desktopnotify.EnvNotifierCmd,
		func(goos, override string) (desktopnotify.Backend, error) {
			platform, err := desktopnotify.PlatformFor(goos)
			if err != nil {
				return nil, err
			}
			return desktopnotify.NewBackend(platform, override, exec.LookPath, desktopnotify.ExecRunner{})
		})
}

// resolveChime resolves the platform's turn-end chime player once, at boot.
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

// workspaceNames titles a banner by the roster row's name: the workspace's
// registry name, which is also the name its tab carries.
type workspaceNames struct {
	db wsm.DB
}

// WorkspaceName answers the workspace's registry name.
func (n workspaceNames) WorkspaceName(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	record, err := n.db.Workspace(ctx, ws)
	if err != nil {
		return "", fmt.Errorf("name workspace %q: %w", ws, err)
	}
	return record.Name, nil
}

// clickForwarder carries a banner's click to the server's host stream. The
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
		panic("claude-repld: a banner click arrived before the server was bound")
	}
	target.NotificationClicked(ws)
}
