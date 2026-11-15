package persistentwifi

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/childproc"
	"agent-repld/internal/inflight"
)

// LINUX IS UNTESTED ON A REAL LAPTOP (owner ruling, 2026-11-14): no Linux
// laptop was at hand, so linuxHost is proven only against fake tools. The
// design is the macOS one on freedesktop standards, and a Linux user whose
// machine answers differently fixes it here.
//
//   - The mode is a logind inhibitor of sleep and of the lid switch's
//     handling, held by a `systemd-inhibit' process agent-repl starts in its
//     own session, so it outlives the daemon that started it exactly as
//     macOS's `pmset disablesleep' outlives the call, and logind's own list
//     is the standing every daemon reads.
//   - The Wi-Fi interface is NetworkManager's, through nmcli; network names
//     are SSIDs, as macOS's are.
//   - The energy mode is power-profiles-daemon's profile (powerprofilesctl);
//     the quiet state's low-power profile is power-saver.
//   - The display sleeps and wakes through X11's DPMS (xset), the toolkit
//     agent-repl's Emacs runs on.

// LinuxTools are the absolute paths of every tool linuxHost runs.
type LinuxTools struct {
	// Nmcli reads and drives the Wi-Fi interface.
	Nmcli string
	// SystemdInhibit holds the sleep inhibitor (--what=sleep:handle-lid-switch)
	// and lists logind's inhibitors.
	SystemdInhibit string
	// Powerprofilesctl reads and sets the energy profile.
	Powerprofilesctl string
	// Xset sleeps and wakes the display (DPMS).
	Xset string
}

// DefaultLinuxTools is the freedesktop layout.
func DefaultLinuxTools() LinuxTools {
	return LinuxTools{
		Nmcli:            "/usr/bin/nmcli",
		SystemdInhibit:   "/usr/bin/systemd-inhibit",
		Powerprofilesctl: "/usr/bin/powerprofilesctl",
		Xset:             "/usr/bin/xset",
	}
}

// LinuxToolsIn is every Linux tool resolved from one directory by its base
// name (EnvToolsDir).
func LinuxToolsIn(dir string) LinuxTools {
	d := DefaultLinuxTools()
	in := func(path string) string { return filepath.Join(dir, filepath.Base(path)) }
	return LinuxTools{
		Nmcli: in(d.Nmcli), SystemdInhibit: in(d.SystemdInhibit),
		Powerprofilesctl: in(d.Powerprofilesctl), Xset: in(d.Xset),
	}
}

// inhibitorWho names agent-repl's inhibitor in logind's list, which is how
// every daemon finds the one that holds the mode.
const inhibitorWho = "agent-repl persistent wifi"

// inhibitorWhat is what the inhibitor blocks: sleep, and logind's own
// handling of the lid switch, so a closed lid neither suspends the machine
// nor triggers HandleLidSwitch.
const inhibitorWhat = "sleep:handle-lid-switch"

// powerSaver is power-profiles-daemon's low-power profile.
const powerSaver = "power-saver"

// linuxHost is Linux's Host. Every short tool runs through the Runner; the
// inhibitor, which runs until it is killed, is started through spawn and
// ended through kill, so a test fakes both.
type linuxHost struct {
	runner Runner
	tools  LinuxTools
	// spawn starts argv in its own session, detached, answering once the
	// inhibitor is taken or refused: it outlives the daemon.
	spawn func(ctx context.Context, argv []string) error
	// kill signals pid.
	kill func(pid int, sig syscall.Signal) error
}

func newLinuxHost(tools LinuxTools, runner Runner) (Host, error) {
	return &linuxHost{runner: runner, tools: tools, spawn: spawnDetached, kill: syscall.Kill}, nil
}

// Read reads the mode (agent-repl's inhibitor in logind's list), the energy
// profile, and the Wi-Fi link.
func (h *linuxHost) Read(ctx context.Context) reading {
	rd := reading{state: &agentreplv1.PersistentWifiState{}}
	if pids, err := h.inhibitors(ctx); err != nil {
		rd.modeErr = err
	} else if len(pids) > 0 {
		rd.state.Mode = &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}
	} else {
		rd.state.Mode = &agentreplv1.PersistentWifiState_Off{Off: &agentreplv1.PersistentWifiModeOff{}}
	}
	if energy, err := h.ReadEnergy(ctx); err == nil {
		rd.energy = energy
	}
	device, err := h.wifiDevice(ctx)
	switch {
	case err != nil:
		rd.wifiErr = err
		return rd
	case device == "":
		rd.state.Wifi = &agentreplv1.PersistentWifiState_NotJoined{NotJoined: &agentreplv1.PersistentWifiNotJoined{}}
		return rd
	}
	rd.device = device
	rd.link, rd.summary, rd.wifiErr = h.ReadLink(ctx, device)
	switch {
	case rd.wifiErr != nil:
	case rd.link.joined:
		joined := &agentreplv1.PersistentWifiJoined{}
		if rd.link.name != "" {
			joined.NetworkName = &rd.link.name
		}
		rd.state.Wifi = &agentreplv1.PersistentWifiState_Joined{Joined: joined}
	default:
		rd.state.Wifi = &agentreplv1.PersistentWifiState_NotJoined{NotJoined: &agentreplv1.PersistentWifiNotJoined{}}
	}
	return rd
}

// inhibitors answers the pids of agent-repl's sleep inhibitors in logind's
// list (`systemd-inhibit --list`).
func (h *linuxHost) inhibitors(ctx context.Context) ([]int, error) {
	out, err := run(ctx, h.runner, h.tools.SystemdInhibit, "--list", "--no-pager", "--no-legend")
	if err != nil {
		return nil, err
	}
	return parseInhibitors(out), nil
}

// parseInhibitors reads `systemd-inhibit --list --no-legend`: one inhibitor a
// line, its columns WHO UID USER PID COMM WHAT WHY MODE separated by runs of
// spaces. WHO may hold spaces, so agent-repl's is found by its own name
// leading the line, and the columns after it are counted from there.
func parseInhibitors(out string) []int {
	var pids []int
	for line := range strings.SplitSeq(out, "\n") {
		rest, ok := strings.CutPrefix(strings.TrimSpace(line), inhibitorWho)
		if !ok {
			continue
		}
		fields := strings.Fields(rest)
		// UID USER PID COMM WHAT ...
		if len(fields) < 5 || !strings.Contains(fields[4], "sleep") {
			continue
		}
		if pid, err := strconv.Atoi(fields[2]); err == nil && pid > 0 {
			pids = append(pids, pid)
		}
	}
	return pids
}

// SetSleepDisabled holds the inhibitor (started once: one already held is
// the mode already on), or ends every inhibitor agent-repl holds.
func (h *linuxHost) SetSleepDisabled(ctx context.Context, on bool) error {
	pids, err := h.inhibitors(ctx)
	if err != nil {
		return err
	}
	if on {
		if len(pids) > 0 {
			return nil
		}
		argv := []string{h.tools.SystemdInhibit, "--what=" + inhibitorWhat, "--who=" + inhibitorWho,
			"--why=lid-closed operation (persistent wifi)", "--mode=block",
			"/bin/sh", "-c", "echo " + heldMark + " && exec sleep infinity"}
		if err := h.spawn(ctx, argv); err != nil {
			return fmt.Errorf("%s could not be started: %w", h.tools.SystemdInhibit, err)
		}
		return nil
	}
	for _, pid := range pids {
		if err := h.kill(pid, syscall.SIGTERM); err != nil && !errors.Is(err, syscall.ESRCH) {
			return fmt.Errorf("the inhibitor (pid %d) could not be ended: %w", pid, err)
		}
	}
	return nil
}

// wifiDevice finds the Wi-Fi device in `nmcli -t -f DEVICE,TYPE device`;
// empty when the machine has none.
func (h *linuxHost) wifiDevice(ctx context.Context) (string, error) {
	out, err := run(ctx, h.runner, h.tools.Nmcli, "-t", "-f", "DEVICE,TYPE", "device")
	if err != nil {
		return "", err
	}
	for line := range strings.SplitSeq(out, "\n") {
		if f := nmcliFields(line); len(f) == 2 && f[1] == "wifi" {
			return f[0], nil
		}
	}
	return "", nil
}

// ReadLink reads device's state and the SSID in use: `nmcli -t -f
// GENERAL.STATE device show <device>`, then the in-use network of `nmcli -t
// -f IN-USE,SSID device wifi list ifname <device> --rescan no`.
func (h *linuxHost) ReadLink(ctx context.Context, device string) (link, string, error) {
	state, err := run(ctx, h.runner, h.tools.Nmcli, "-t", "-f", "GENERAL.STATE", "device", "show", device)
	if err != nil {
		return link{}, state, err
	}
	l := link{joined: strings.Contains(state, "(connected)")}
	if !l.joined {
		return l, strings.TrimSpace(state), nil
	}
	list, err := run(ctx, h.runner, h.tools.Nmcli, "-t", "-f", "IN-USE,SSID", "device", "wifi", "list", "ifname", device, "--rescan", "no")
	if err != nil {
		return link{}, strings.TrimSpace(state), err
	}
	for line := range strings.SplitSeq(list, "\n") {
		if f := nmcliFields(line); len(f) == 2 && f[0] == "*" {
			l.name = f[1]
		}
	}
	return l, strings.TrimSpace(state + "\n" + list), nil
}

// RadioPower reads `nmcli radio wifi`: "enabled" or "disabled".
func (h *linuxHost) RadioPower(ctx context.Context, _ string) (string, error) {
	out, err := run(ctx, h.runner, h.tools.Nmcli, "radio", "wifi")
	if err != nil {
		return "", err
	}
	switch strings.TrimSpace(out) {
	case "enabled":
		return "on", nil
	case "disabled":
		return "off", nil
	}
	return "", fmt.Errorf("%s radio wifi named no power state: %q", h.tools.Nmcli, strings.TrimSpace(out))
}

// PreferredNetworks lists the SSIDs of NetworkManager's saved Wi-Fi
// connections: `nmcli -t -f NAME,TYPE connection show`, then each Wi-Fi
// connection's SSID (`nmcli -g 802-11-wireless.ssid connection show id NAME`).
func (h *linuxHost) PreferredNetworks(ctx context.Context, _ string) ([]string, error) {
	out, err := run(ctx, h.runner, h.tools.Nmcli, "-t", "-f", "NAME,TYPE", "connection", "show")
	if err != nil {
		return nil, err
	}
	var ssids []string
	for line := range strings.SplitSeq(out, "\n") {
		f := nmcliFields(line)
		if len(f) != 2 || f[1] != "802-11-wireless" {
			continue
		}
		ssid, err := run(ctx, h.runner, h.tools.Nmcli, "-g", "802-11-wireless.ssid", "connection", "show", "id", f[0])
		if err != nil {
			return nil, err
		}
		if s := strings.TrimSpace(ssid); s != "" {
			ssids = append(ssids, s)
		}
	}
	return ssids, nil
}

// JoinNetwork: `nmcli device wifi connect SSID ifname DEVICE`, which uses the
// saved connection for the SSID.
func (h *linuxHost) JoinNetwork(ctx context.Context, device, ssid string) error {
	_, err := run(ctx, h.runner, h.tools.Nmcli, "device", "wifi", "connect", ssid, "ifname", device)
	return err
}

// CanDisconnect: nmcli always can.
func (h *linuxHost) CanDisconnect() error { return nil }

// Disconnect: `nmcli device disconnect DEVICE`, the radio left on.
func (h *linuxHost) Disconnect(ctx context.Context, device string) error {
	_, err := run(ctx, h.runner, h.tools.Nmcli, "device", "disconnect", device)
	return err
}

// SetRadio: `nmcli radio wifi on|off`.
func (h *linuxHost) SetRadio(ctx context.Context, _ string, on bool) error {
	power := "off"
	if on {
		power = "on"
	}
	_, err := run(ctx, h.runner, h.tools.Nmcli, "radio", "wifi", power)
	return err
}

// Scan: `nmcli -t -f IN-USE,SSID,SIGNAL device wifi list --rescan yes`. Linux
// names networks to any user, so the scan is always authorized.
func (h *linuxHost) Scan(ctx context.Context) (wifiScan, error) {
	out, err := run(ctx, h.runner, h.tools.Nmcli, "-t", "-f", "IN-USE,SSID,SIGNAL", "device", "wifi", "list", "--rescan", "yes")
	if err != nil {
		return wifiScan{}, fmt.Errorf("the Wi-Fi scan could not be run: %w", err)
	}
	scan := wifiScan{Authorization: "authorized"}
	for line := range strings.SplitSeq(out, "\n") {
		f := nmcliFields(line)
		if len(f) != 3 || f[1] == "" {
			continue
		}
		signal, _ := strconv.Atoi(f[2])
		scan.Visible = append(scan.Visible, visibleNetwork{SSID: f[1], RSSI: signal})
		if f[0] == "*" {
			scan.Current = f[1]
		}
	}
	return scan, nil
}

// ReadEnergy: `powerprofilesctl get`.
func (h *linuxHost) ReadEnergy(ctx context.Context) (string, error) {
	out, err := run(ctx, h.runner, h.tools.Powerprofilesctl, "get")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(out), nil
}

func (h *linuxHost) LowPowerEnergy() string { return powerSaver }

// SetEnergy: `powerprofilesctl set MODE`.
func (h *linuxHost) SetEnergy(ctx context.Context, mode string) error {
	_, err := run(ctx, h.runner, h.tools.Powerprofilesctl, "set", mode)
	return err
}

// SleepDisplay: `xset dpms force off`.
func (h *linuxHost) SleepDisplay(ctx context.Context) error {
	_, err := run(ctx, h.runner, h.tools.Xset, "dpms", "force", "off")
	return err
}

// WakeDisplay: `xset dpms force on`.
func (h *linuxHost) WakeDisplay(ctx context.Context) error {
	_, err := run(ctx, h.runner, h.tools.Xset, "dpms", "force", "on")
	return err
}

// SetBrightness: no Linux build ever dimmed the lid, so there is nothing to
// undo and no tool to undo it with.
func (h *linuxHost) SetBrightness(context.Context, float64) error {
	return errNoBrightness
}

// nmcliFields splits one line of nmcli's terse (-t) output at its unescaped
// colons, unescaping `\:' and `\\'.
func nmcliFields(line string) []string {
	line = strings.TrimRight(line, "\r")
	if line == "" {
		return nil
	}
	var fields []string
	var cur strings.Builder
	escaped := false
	for _, r := range line {
		switch {
		case escaped:
			cur.WriteRune(r)
			escaped = false
		case r == '\\':
			escaped = true
		case r == ':':
			fields = append(fields, cur.String())
			cur.Reset()
		default:
			cur.WriteRune(r)
		}
	}
	return append(fields, cur.String())
}

// heldMark is what the inhibitor's command prints once logind granted it:
// systemd-inhibit runs its command only after the inhibitor is taken.
const heldMark = "held"

// spawnDetached starts argv in a session of its own, so it outlives the
// daemon: the inhibitor is the mode, which a daemon's restart must not end.
// IT ANSWERS ONLY ONCE logind HAS ANSWERED: argv's command prints heldMark
// once the inhibitor is taken, and a refused inhibitor (polkit's "Access
// denied") ends systemd-inhibit before its command runs, so the start reads
// heldMark or the end, never a guess, and a refusal is the error with
// systemd-inhibit's own words. ctx bounds the wait. While this daemon lives it
// reaps the process when it ends, so it is never left a zombie.
func spawnDetached(ctx context.Context, argv []string) error {
	// THE CHILD'S OUTPUT IS ONE PIPE IT HOLDS ITSELF, both streams on its
	// write end: the granted mark arrives on it, and a refusal's words, and
	// the parent keeps no copy of the write end, so its read ends when the
	// child closes the pipe and Wait never waits on a copying goroutine.
	r, w, err := os.Pipe()
	if err != nil {
		return fmt.Errorf("make the inhibitor's output pipe: %w", err)
	}
	defer r.Close()
	cmd := childproc.Command(childproc.Stable, argv[0], argv[1:]...)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	cmd.Stdin = nil
	cmd.Stdout, cmd.Stderr = w, w
	startErr := cmd.Start()
	_ = w.Close()
	if startErr != nil {
		return startErr
	}
	answered := make(chan string, 1)
	// inflight: joined: the function that starts it waits for it.
	go func() {
		out := bufio.NewReader(r)
		first, _ := out.ReadString('\n')
		if strings.TrimSpace(first) == heldMark {
			answered <- heldMark
			return
		}
		rest, _ := io.ReadAll(out)
		answered <- first + string(rest)
	}()
	select {
	case said := <-answered:
		if said == heldMark {
			// The inhibitor runs until it is ended (SetSleepDisabled off), so
			// its reap stands as long as it does.
			inflight.Standing(func() { _ = cmd.Wait() })
			return nil
		}
		werr := cmd.Wait()
		return fmt.Errorf("logind did not grant the inhibitor (%v): %s", werr, strings.TrimSpace(said))
	case <-ctx.Done():
		_ = cmd.Process.Kill()
		_ = r.Close()
		<-answered
		_ = cmd.Wait()
		return fmt.Errorf("logind did not answer the inhibitor before the request ended: %w", ctx.Err())
	}
}
