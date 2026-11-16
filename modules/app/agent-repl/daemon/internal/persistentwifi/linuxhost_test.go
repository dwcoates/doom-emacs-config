package persistentwifi

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"syscall"
	"testing"
)

// The Linux host is proven against scripted nmcli, systemd-inhibit,
// powerprofilesctl and xset answers: no Linux laptop was at hand (owner
// ruling, 2026-11-14), so these are the whole of its proof.

var ltools = LinuxToolsIn("/t")

// The argvs linuxHost runs.
const (
	lInhibitList  = "/t/systemd-inhibit --list --no-pager --no-legend"
	lDevices      = "/t/nmcli -t -f DEVICE,TYPE device"
	lState        = "/t/nmcli -t -f GENERAL.STATE device show wlp2s0"
	lInUse        = "/t/nmcli -t -f IN-USE,SSID device wifi list ifname wlp2s0 --rescan no"
	lEnergy       = "/t/powerprofilesctl get"
	lConnections  = "/t/nmcli -t -f NAME,TYPE connection show"
	lScan         = "/t/nmcli -t -f IN-USE,SSID,SIGNAL device wifi list --rescan yes"
	lRadio        = "/t/nmcli radio wifi"
	ourInhibitor  = "agent-repl persistent wifi 1000 u 4242 systemd-inhibit sleep:handle-lid-switch lid-closed operation block\n"
	someInhibitor = "NetworkManager 0 root 812 NetworkManager sleep NetworkManager needs to turn off networks delay\n"
)

// spawned and killed record what the inhibitor seams were asked.
type inhibitorSeams struct {
	spawned  [][]string
	killed   []int
	spawnErr error
	killErr  error
}

func newLinuxHostFor(t *testing.T) (*linuxHost, *fakeRunner, *inhibitorSeams) {
	t.Helper()
	r := newFakeRunner(t)
	seams := &inhibitorSeams{}
	h := &linuxHost{
		runner: r, tools: ltools,
		spawn: func(_ context.Context, argv []string) error {
			seams.spawned = append(seams.spawned, argv)
			return seams.spawnErr
		},
		kill: func(pid int, _ syscall.Signal) error {
			seams.killed = append(seams.killed, pid)
			return seams.killErr
		},
	}
	return h, r, seams
}

func TestParseInhibitors(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want []int
	}{
		{name: "agent-repl's sleep inhibitor among others", out: someInhibitor + ourInhibitor, want: []int{4242}},
		{name: "another program's inhibitor is not the mode", out: someInhibitor},
		{name: "an agent-repl inhibitor of something other than sleep is not the mode",
			out: "agent-repl persistent wifi 1000 u 4242 systemd-inhibit shutdown x block\n"},
		{name: "no inhibitors at all", out: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := parseInhibitors(tt.out)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("parseInhibitors() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNmcliFields(t *testing.T) {
	tests := []struct {
		line string
		want []string
	}{
		{line: "wlp2s0:wifi", want: []string{"wlp2s0", "wifi"}},
		{line: `*:Cafe\: Upstairs:70`, want: []string{"*", "Cafe: Upstairs", "70"}},
		{line: `:back\\slash:5`, want: []string{"", `back\slash`, "5"}},
		{line: "", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.line, func(t *testing.T) {
			// Act
			got := nmcliFields(tt.line)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("nmcliFields(%q) = %q, want %q", tt.line, got, tt.want)
			}
		})
	}
}

func TestLinuxReadJoinedWithTheModeOn(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on(lInhibitList, answer{out: ourInhibitor})
	r.on(lEnergy, answer{out: "balanced\n"})
	r.on(lDevices, answer{out: "enp0s1:ethernet\nwlp2s0:wifi\nlo:loopback\n"})
	r.on(lState, answer{out: "GENERAL.STATE:100 (connected)\n"})
	r.on(lInUse, answer{out: " :Other\n*:Home Net\n"})

	// Act
	rd := h.Read(context.Background())

	// Assert
	if rd.modeErr != nil || rd.wifiErr != nil || rd.state.GetOn() == nil || rd.energy != "balanced" {
		t.Fatalf("reading = %+v, want the mode on and balanced", rd)
	}
	if rd.device != "wlp2s0" || !rd.link.joined || rd.link.name != "Home Net" || rd.state.GetJoined().GetNetworkName() != "Home Net" {
		t.Fatalf("link = %+v on %q, want Home Net joined on wlp2s0", rd.link, rd.device)
	}
}

func TestLinuxReadDisconnectedWithTheModeOff(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on(lInhibitList, answer{out: someInhibitor})
	r.on(lEnergy, answer{out: "power-saver\n"})
	r.on(lDevices, answer{out: "wlp2s0:wifi\n"})
	r.on(lState, answer{out: "GENERAL.STATE:30 (disconnected)\n"})

	// Act
	rd := h.Read(context.Background())

	// Assert
	if rd.state.GetOff() == nil || rd.state.GetNotJoined() == nil || rd.link.joined || rd.energy != "power-saver" {
		t.Fatalf("reading = %+v, want the mode off and nothing joined", rd)
	}
	if strings.Contains(strings.Join(r.ran(), "\n"), "IN-USE") {
		t.Fatalf("calls = %v: a disconnected device has no network to name", r.ran())
	}
}

func TestLinuxReadWithNoWifiDeviceIsNotJoined(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on(lInhibitList, answer{})
	r.on(lEnergy, answer{out: "balanced\n"})
	r.on(lDevices, answer{out: "enp0s1:ethernet\n"})

	// Act
	rd := h.Read(context.Background())

	// Assert
	if rd.wifiErr != nil || rd.device != "" || rd.state.GetNotJoined() == nil {
		t.Fatalf("reading = %+v, want no device and not joined", rd)
	}
}

func TestLinuxReadKeepsEachFactsCause(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on(lInhibitList, answer{out: "Failed to connect to bus", code: 1})
	r.on(lEnergy, answer{err: errors.New("no such file")})
	r.on(lDevices, answer{out: "Error: NetworkManager is not running.", code: 8})

	// Act
	rd := h.Read(context.Background())

	// Assert
	if rd.modeErr == nil || rd.state.Mode != nil || rd.wifiErr == nil || rd.state.Wifi != nil || rd.energy != "" {
		t.Fatalf("reading = %+v, want both facts unread with their causes and no energy", rd)
	}
	if !strings.Contains(rd.wifiErr.Error(), "NetworkManager is not running") {
		t.Fatalf("wifiErr = %v, want nmcli's own words", rd.wifiErr)
	}
}

func TestLinuxSetSleepDisabled(t *testing.T) {
	tests := []struct {
		name        string
		list        string
		on          bool
		wantSpawned bool
		wantKilled  []int
	}{
		{name: "turning on starts the inhibitor", list: someInhibitor, on: true, wantSpawned: true},
		{name: "turning on with the inhibitor held starts no second one", list: ourInhibitor, on: true},
		{name: "turning off ends agent-repl's inhibitor and no other", list: someInhibitor + ourInhibitor, on: false, wantKilled: []int{4242}},
		{name: "turning off with none held ends nothing", list: someInhibitor, on: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, r, seams := newLinuxHostFor(t)
			r.on(lInhibitList, answer{out: tt.list})

			// Act
			err := h.SetSleepDisabled(context.Background(), tt.on)

			// Assert
			if err != nil {
				t.Fatalf("SetSleepDisabled() = %v", err)
			}
			if (len(seams.spawned) == 1) != tt.wantSpawned || !reflect.DeepEqual(seams.killed, tt.wantKilled) {
				t.Fatalf("spawned %q, killed %v; want spawned %v, killed %v", seams.spawned, seams.killed, tt.wantSpawned, tt.wantKilled)
			}
			if tt.wantSpawned {
				want := []string{"/t/systemd-inhibit", "--what=sleep:handle-lid-switch", "--who=agent-repl persistent wifi",
					"--why=lid-closed operation (persistent wifi)", "--mode=block", "/bin/sh", "-c", "echo held && exec sleep infinity"}
				if !reflect.DeepEqual(seams.spawned[0], want) {
					t.Fatalf("spawned %q, want %q", seams.spawned[0], want)
				}
			}
		})
	}
}

func TestLinuxSetSleepDisabledFailures(t *testing.T) {
	tests := []struct {
		name    string
		list    answer
		on      bool
		seams   inhibitorSeams
		wantErr string
	}{
		{name: "an unreadable list changes nothing", list: answer{out: "Failed to connect to bus", code: 1}, on: true, wantErr: "Failed to connect to bus"},
		{name: "an inhibitor that will not start", list: answer{}, on: true, seams: inhibitorSeams{spawnErr: errors.New("no such file")}, wantErr: "could not be started: no such file"},
		{name: "an inhibitor that will not end", list: answer{out: ourInhibitor}, on: false, seams: inhibitorSeams{killErr: syscall.EPERM}, wantErr: "(pid 4242) could not be ended"},
		{name: "an inhibitor already gone is no failure", list: answer{out: ourInhibitor}, on: false, seams: inhibitorSeams{killErr: syscall.ESRCH}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, r, seams := newLinuxHostFor(t)
			seams.spawnErr, seams.killErr = tt.seams.spawnErr, tt.seams.killErr
			r.on(lInhibitList, tt.list)

			// Act
			err := h.SetSleepDisabled(context.Background(), tt.on)

			// Assert
			if tt.wantErr == "" && err != nil || tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("SetSleepDisabled() = %v, want %q", err, tt.wantErr)
			}
		})
	}
}

func TestLinuxPreferredNetworksAreSavedWifiSSIDs(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on(lConnections, answer{out: "Wired connection 1:802-3-ethernet\nHome:802-11-wireless\nPhone profile:802-11-wireless\n"})
	r.on("/t/nmcli -g 802-11-wireless.ssid connection show id Home", answer{out: "Home Net\n"})
	r.on("/t/nmcli -g 802-11-wireless.ssid connection show id Phone profile", answer{out: "Dodge’s iPhone\n"})

	// Act
	got, err := h.PreferredNetworks(context.Background(), "wlp2s0")

	// Assert
	if err != nil || !reflect.DeepEqual(got, []string{"Home Net", "Dodge’s iPhone"}) {
		t.Fatalf("PreferredNetworks() = (%q, %v), want the Wi-Fi connections' SSIDs", got, err)
	}
}

func TestLinuxWifiSteps(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *linuxHost) error
		want string
	}{
		{name: "a join connects the SSID on the device", act: func(h *linuxHost) error {
			return h.JoinNetwork(context.Background(), "wlp2s0", "Home Net")
		}, want: "/t/nmcli device wifi connect Home Net ifname wlp2s0"},
		{name: "a disconnect leaves the radio on", act: func(h *linuxHost) error {
			return h.Disconnect(context.Background(), "wlp2s0")
		}, want: "/t/nmcli device disconnect wlp2s0"},
		{name: "the radio powers off", act: func(h *linuxHost) error {
			return h.SetRadio(context.Background(), "wlp2s0", false)
		}, want: "/t/nmcli radio wifi off"},
		{name: "the radio powers on", act: func(h *linuxHost) error {
			return h.SetRadio(context.Background(), "wlp2s0", true)
		}, want: "/t/nmcli radio wifi on"},
		{name: "the energy profile is set", act: func(h *linuxHost) error {
			return h.SetEnergy(context.Background(), "power-saver")
		}, want: "/t/powerprofilesctl set power-saver"},
		{name: "the display sleeps through DPMS", act: func(h *linuxHost) error {
			return h.SleepDisplay(context.Background())
		}, want: "/t/xset dpms force off"},
		{name: "the display wakes through DPMS", act: func(h *linuxHost) error {
			return h.WakeDisplay(context.Background())
		}, want: "/t/xset dpms force on"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, r, _ := newLinuxHostFor(t)
			r.on(tt.want, answer{})

			// Act
			err := tt.act(h)

			// Assert
			if err != nil || !reflect.DeepEqual(r.ran(), []string{tt.want}) {
				t.Fatalf("calls = %v (err %v), want exactly %q", r.ran(), err, tt.want)
			}
		})
	}
}

func TestLinuxAFailedJoinNamesNmclisWords(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on("/t/nmcli device wifi connect Home Net ifname wlp2s0", answer{out: "Error: No network with SSID 'Home Net' found.", code: 10})

	// Act
	err := h.JoinNetwork(context.Background(), "wlp2s0", "Home Net")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "No network with SSID") {
		t.Fatalf("JoinNetwork() = %v, want nmcli's own words", err)
	}
}

func TestLinuxRadioPower(t *testing.T) {
	tests := []struct {
		out, want string
		wantErr   bool
	}{
		{out: "enabled\n", want: "on"},
		{out: "disabled\n", want: "off"},
		{out: "missing\n", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.out, func(t *testing.T) {
			// Arrange
			h, r, _ := newLinuxHostFor(t)
			r.on(lRadio, answer{out: tt.out})

			// Act
			got, err := h.RadioPower(context.Background(), "wlp2s0")

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("RadioPower() = (%q, %v), want %q", got, err, tt.want)
			}
		})
	}
}

func TestLinuxScanNamesTheVisibleAndTheJoined(t *testing.T) {
	// Arrange
	h, r, _ := newLinuxHostFor(t)
	r.on(lScan, answer{out: "*:Home Net:82\n :Cafe\\: Upstairs:40\n ::30\n"})

	// Act
	scan, err := h.Scan(context.Background())

	// Assert
	want := wifiScan{Authorization: "authorized", Current: "Home Net",
		Visible: []visibleNetwork{{SSID: "Home Net", RSSI: 82}, {SSID: "Cafe: Upstairs", RSSI: 40}}}
	if err != nil || !reflect.DeepEqual(scan, want) {
		t.Fatalf("Scan() = (%+v, %v), want %+v (a hidden network has no name to join)", scan, err, want)
	}
}

func TestLinuxHostConstants(t *testing.T) {
	// Arrange
	h, _, _ := newLinuxHostFor(t)

	// Act
	low, can, bright := h.LowPowerEnergy(), h.CanDisconnect(), h.SetBrightness(context.Background(), 0.5)
	_, askErr := h.AskScanPermission(context.Background())

	// Assert
	if low != "power-saver" || can != nil || !errors.Is(bright, errNoBrightness) || askErr != nil {
		t.Fatalf("low %q, can disconnect %v, brightness %v, ask %v", low, can, bright, askErr)
	}
}

func TestNewHostBuildsTheLinuxHost(t *testing.T) {
	// Act
	host, err := NewHost(Config{Platform: "linux", Linux: ltools}, newFakeRunner(t), nil, "")

	// Assert
	if _, ok := host.(*linuxHost); err != nil || !ok {
		t.Fatalf("NewHost(linux) = (%T, %v), want the Linux host", host, err)
	}
}

func TestSpawnDetached(t *testing.T) {
	cancelled, cancel := context.WithCancel(context.Background())
	cancel()
	tests := []struct {
		name    string
		ctx     context.Context
		script  string
		wantErr string
	}{
		{name: "a granted inhibitor answers once its command runs", ctx: context.Background(), script: "echo held"},
		{name: "a refused inhibitor answers its own words", ctx: context.Background(),
			script: "echo 'Failed to inhibit: Access denied' >&2; exit 1", wantErr: "Failed to inhibit: Access denied"},
		{name: "a request that ends first stops the wait", ctx: cancelled, script: "exec sleep 30", wantErr: "before the request ended"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := spawnDetached(tt.ctx, []string{"/bin/sh", "-c", tt.script})

			// Assert
			if tt.wantErr == "" && err != nil || tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("spawnDetached() = %v, want %q", err, tt.wantErr)
			}
		})
	}
}
