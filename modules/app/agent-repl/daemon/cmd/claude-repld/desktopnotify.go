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
// A platform with none, or a program that is not installed, is recorded at
// ERROR and answered as the reason every banner will record: the daemon still
// serves, and each banner that cannot be posted says why.
func resolveBannerBackend(log dlog.Logger) (desktopnotify.Backend, error) {
	platform, err := desktopnotify.PlatformFor(runtime.GOOS)
	if err == nil {
		var backend desktopnotify.Backend
		backend, err = desktopnotify.NewBackend(platform, os.Getenv(desktopnotify.EnvNotifierCmd), exec.LookPath, desktopnotify.ExecRunner{})
		if err == nil {
			log.Info(opDesktopNotify, "resolved the desktop banner program", dlog.Context{
				"program": backend.Program(), "goos": runtime.GOOS,
			})
			return backend, nil
		}
	}
	log.Error(opDesktopNotify, "no desktop banner program; banners will not be posted", dlog.Context{
		"goos": runtime.GOOS, "cause": err.Error(), "override_env": desktopnotify.EnvNotifierCmd,
	})
	return nil, err
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
