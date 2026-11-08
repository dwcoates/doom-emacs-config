package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"runtime"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// opDesktopNotify is the operation the notifier's boot records carry.
const opDesktopNotify = "daemon.cmd.desktop_notify"

// resolveBannerBackend resolves the platform's banner poster once, at boot.
// On macOS it is the Zettel notifier app, which posts each banner through the
// notification center under its own name and icon, over its own stream
// (desktopnotify.App); everywhere else it is the platform's banner program.
func resolveBannerBackend(log dlog.Logger, app *desktopnotify.App) (desktopnotify.Backend, error) {
	return resolveNotifyProgram(log, "banner", desktopnotify.EnvNotifierCmd,
		func(goos, override string) (desktopnotify.Backend, error) {
			if goos == "darwin" {
				return app, nil
			}
			platform, err := desktopnotify.PlatformFor(goos)
			if err != nil {
				return nil, err
			}
			return desktopnotify.NewBackend(platform, override, exec.LookPath, desktopnotify.ExecRunner{})
		})
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

// buildSpeech builds the speech every made banner is read aloud through when
// the daemon's persistent wifi mode is on and a private listening device is
// the audio output: this GOOS's programs, each overridden by its environment variable
// when set, run through the one exec Runner the banner program runs through.
func buildSpeech(standing desktopnotify.WifiStanding, selfExe string) *desktopnotify.Speech {
	return desktopnotify.NewSpeech(runtime.GOOS, speechPlatform(selfExe), desktopnotify.ExecRunner{}, standing)
}

// speechPlatform is this GOOS's speech programs, the audio output read
// through this daemon binary (selfExe), with the environment's overrides in
// place.
func speechPlatform(selfExe string) desktopnotify.SpeechPlatform {
	return desktopnotify.SpeechPlatformFor(runtime.GOOS, selfExe).WithOverrides(
		os.Getenv(desktopnotify.EnvSayCmd), os.Getenv(desktopnotify.EnvAudioOutputCmd), os.Getenv(desktopnotify.EnvBluetoothCmd))
}

// wifiStandingForwarder carries the persistent-wifi controller's published
// standing to the banners' speech. The notifier is built before the
// controller exists, so the controller's topic is bound late, exactly as the
// click forwarder's server is.
type wifiStandingForwarder struct {
	lateBound[desktopnotify.WifiStanding]
}

// Latest answers the controller's last published standing. A banner is only
// made once the daemon serves, after the controller is bound, so an unbound
// forwarder is a broken boot order.
func (f *wifiStandingForwarder) Latest() (*agentreplv1.PersistentWifiState, bool) {
	target, ok := f.load()
	if !ok {
		panic("agent-repld: a banner was read aloud before the persistent-wifi controller was bound")
	}
	return target.Latest()
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

// clickForwarder carries a banner's click to the server's host stream. The
// notifier is built before the server exists, so the server is bound late,
// exactly as the host relay is.
type clickForwarder struct {
	lateBound[desktopnotify.ClickSink]
}

// NotificationClicked relays the click. A click can only follow a banner the
// serving daemon posted, so an unbound forwarder is a broken boot order.
func (f *clickForwarder) NotificationClicked(ws ids.WorkspaceID) {
	target, ok := f.load()
	if !ok {
		panic("agent-repld: a banner click arrived before the server was bound")
	}
	target.NotificationClicked(ws)
}
