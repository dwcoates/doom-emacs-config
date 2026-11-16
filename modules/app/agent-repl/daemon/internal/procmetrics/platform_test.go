package procmetrics

import (
	"reflect"
	"testing"
)

// linuxHost is a Linux host's table: the system manager (1), the user's
// manager (2), Emacs, the daemon, and WebKitGTK's processes, one of them
// sandboxed through two bubblewrap processes.
func linuxHost(extra ...Identity) map[int]Identity {
	return host(append([]Identity{
		{PID: 1, Name: "systemd"},
		{PID: 2, PPID: 1, Name: "systemd"},
		{PID: testEmacs, PPID: 2, Name: "emacs-30.2"},
		{PID: testSelf, PPID: 2, Name: "agent-repld"},
		{PID: 60, PPID: testEmacs, Name: "bwrap"},
		{PID: 61, PPID: 60, Name: "bwrap"},
		{PID: 62, PPID: 61, Name: "WebKitWebProcess"},
		{PID: 63, PPID: testEmacs, Name: "WebKitNetworkProcess"},
	}, extra...)...)
}

func TestClassifyOnLinux(t *testing.T) {
	tests := []struct {
		name string
		ids  map[int]Identity
		want map[int]Group
	}{
		{
			name: "the user's systemd runs the store and the sidecar",
			ids:  linuxHost(Identity{PID: 70, PPID: 2, Name: storeName}, Identity{PID: 71, PPID: 2, Name: sidecarName}),
			want: map[int]Group{70: {Process: ProcessStore}, 71: {Process: ProcessSidecar}},
		},
		{
			name: "a store a test started is not ours",
			ids:  linuxHost(Identity{PID: 80, PPID: 2, Name: "go"}, Identity{PID: 70, PPID: 80, Name: storeName}),
			want: map[int]Group{},
		},
		{
			name: "WebKitGTK's processes descending from Emacs are its WebKit, sandboxed or not",
			ids:  linuxHost(),
			want: map[int]Group{62: {Process: ProcessWebKitWebContent}, 63: {Process: ProcessWebKitNetworking}},
		},
		{
			name: "a WebKit process of another program is not Emacs's",
			ids:  linuxHost(Identity{PID: 90, PPID: 2, Name: "epiphany"}, Identity{PID: 91, PPID: 90, Name: "WebKitWebProcess"}),
			want: map[int]Group{},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := classify(Linux, tt.ids, testSelf, testEmacs, testSock, argvOf(nil))

			// Assert
			want := map[int]Group{
				testSelf: {Process: ProcessDaemon}, testEmacs: {Process: ProcessEmacs},
				62: {Process: ProcessWebKitWebContent}, 63: {Process: ProcessWebKitNetworking},
			}
			for pid, g := range tt.want {
				want[pid] = g
			}
			if !reflect.DeepEqual(got.groups, want) {
				t.Fatalf("groups = %v, want %v", got.groups, want)
			}
		})
	}
}

func TestClassifyOnLinuxNamesAnUnknownWebKitRole(t *testing.T) {
	// Arrange
	ids := linuxHost(Identity{PID: 64, PPID: testEmacs, Name: "WebKitStorageProcess"})

	// Act
	got := classify(Linux, ids, testSelf, testEmacs, testSock, argvOf(nil))

	// Assert
	if _, grouped := got.groups[64]; grouped || !reflect.DeepEqual(got.unknownWebKit, []string{"WebKitStorageProcess"}) {
		t.Fatalf("groups = %v, unknown = %v, want 64 in no group and named unknown", got.groups, got.unknownWebKit)
	}
}

func TestLinuxRulesIgnoreResponsibility(t *testing.T) {
	// Arrange: a process macOS would call Emacs's WebKit, outside Emacs's tree.
	id := Identity{PID: 91, PPID: 2, Name: "com.apple.WebKit.WebContent", Responsible: testEmacs}

	// Act
	_, webkit := Linux.WebKitRole(linuxHost(id), id, testEmacs)

	// Assert
	if webkit {
		t.Fatal("Linux.WebKitRole read a responsibility Linux does not have")
	}
}

func TestDarwinRulesAreLaunchdAndResponsibility(t *testing.T) {
	// Arrange
	ids := host(Identity{PID: 2, PPID: 1, Name: "systemd"})
	webkit := Identity{PID: 3, PPID: 1, Name: "com.apple.WebKit.GPU.Development", Responsible: testEmacs}

	// Act
	launchd, systemd := Darwin.ServiceParent(ids, 1), Darwin.ServiceParent(ids, 2)
	role, isWebKit := Darwin.WebKitRole(ids, webkit, testEmacs)

	// Assert
	if !launchd || systemd || role != "GPU" || !isWebKit {
		t.Fatalf("launchd %v, systemd %v, role (%q, %v), want true, false, (GPU, true)", launchd, systemd, role, isWebKit)
	}
}

func TestDescendsFrom(t *testing.T) {
	tests := []struct {
		name     string
		ids      map[int]Identity
		pid      int
		ancestor int
		want     bool
	}{
		{name: "a grandchild descends", ids: linuxHost(), pid: 62, ancestor: testEmacs, want: true},
		{name: "a sibling does not", ids: linuxHost(), pid: testSelf, ancestor: testEmacs, want: false},
		{name: "no process is nobody's ancestor", ids: linuxHost(), pid: 62, ancestor: 0, want: false},
		{name: "a process is not its own descendant", ids: linuxHost(), pid: testEmacs, ancestor: testEmacs, want: false},
		{
			name:     "a parent cycle ends the walk",
			ids:      host(Identity{PID: 5, PPID: 6}, Identity{PID: 6, PPID: 5}),
			pid:      5,
			ancestor: 9,
			want:     false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := descendsFrom(tt.ids, tt.pid, tt.ancestor)

			// Assert
			if got != tt.want {
				t.Fatalf("descendsFrom(%d, %d) = %v, want %v", tt.pid, tt.ancestor, got, tt.want)
			}
		})
	}
}
