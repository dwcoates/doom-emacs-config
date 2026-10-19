package procmetrics

import (
	"errors"
	"reflect"
	"testing"
)

const (
	testSelf  = 500
	testEmacs = 100
	testSock  = "/state/sock"
)

// host builds an identity table from (pid, ppid, name, responsible) rows.
func host(rows ...Identity) map[int]Identity {
	out := map[int]Identity{}
	for _, r := range rows {
		out[r.PID] = r
	}
	return out
}

// argvOf answers scripted argvs, ErrGone for a pid it does not know.
func argvOf(table map[int][]string) func(int) ([]string, error) {
	return func(pid int) ([]string, error) {
		if a, ok := table[pid]; ok {
			return a, nil
		}
		return nil, ErrGone
	}
}

func baseHost(extra ...Identity) map[int]Identity {
	rows := append([]Identity{
		{PID: testEmacs, PPID: 1, Name: "Emacs", Responsible: testEmacs},
		{PID: testSelf, PPID: 1, Name: "agent-repld", Responsible: testEmacs},
	}, extra...)
	return host(rows...)
}

func TestClassify(t *testing.T) {
	tests := []struct {
		name string
		ids  map[int]Identity
		argv map[int][]string
		want map[int]Group
	}{
		{
			name: "the daemon and the Emacs responsible for it",
			ids:  baseHost(),
			want: map[int]Group{testSelf: {Process: ProcessDaemon}, testEmacs: {Process: ProcessEmacs}},
		},
		{
			name: "launchd's store and sidecar are ours, a test's are not",
			ids: baseHost(
				Identity{PID: 10, PPID: 1, Name: "shim-store"},
				Identity{PID: 11, PPID: 1, Name: "shim-claude-sidecar"},
				Identity{PID: 12, PPID: 900, Name: "shim-store"},
			),
			want: map[int]Group{testSelf: {Process: ProcessDaemon}, testEmacs: {Process: ProcessEmacs}, 10: {Process: ProcessStore}, 11: {Process: ProcessSidecar}},
		},
		{
			name: "Emacs's WebKit processes by role, another app's are not",
			ids: baseHost(
				Identity{PID: 20, PPID: 1, Name: "com.apple.WebKit.WebContent", Responsible: testEmacs},
				Identity{PID: 21, PPID: 1, Name: "com.apple.WebKit.Networking", Responsible: testEmacs},
				Identity{PID: 22, PPID: 1, Name: "com.apple.WebKit.WebContent", Responsible: 777},
			),
			want: map[int]Group{testSelf: {Process: ProcessDaemon}, testEmacs: {Process: ProcessEmacs}, 20: {Process: "webkit_webcontent"}, 21: {Process: "webkit_networking"}},
		},
		{
			name: "a shim is a node process listening in the daemon's socket directory, adopted or spawned",
			ids: baseHost(
				Identity{PID: 30, PPID: testSelf, Name: "node", Responsible: testEmacs},
				Identity{PID: 31, PPID: 1, Name: "node", Responsible: 4242},
				Identity{PID: 32, PPID: 1, Name: "node"},
				Identity{PID: 33, PPID: 1, Name: "node"},
			),
			argv: map[int][]string{
				30: {"node", "/m/dist/main.js", "--listen", "/state/sock/aaaa000011112222.sock"},
				31: {"node", "/m/dist/main.js", "--listen", "/state/sock/bbbb000011112222.n3.sock"},
				32: {"node", "/m/dist/main.js", "--listen", "/tmp/test/sock/cccc.sock"},
				33: {"node", "server.js"},
			},
			want: map[int]Group{
				testSelf: {Process: ProcessDaemon}, testEmacs: {Process: ProcessEmacs},
				30: {Process: ProcessShim, Workspace: "aaaa000011112222"},
				31: {Process: ProcessShim, Workspace: "bbbb000011112222"},
			},
		},
		{
			name: "a shim's descendants are its tools, the daemon's other descendants are its own",
			ids: baseHost(
				Identity{PID: 30, PPID: testSelf, Name: "node"},
				Identity{PID: 40, PPID: 30, Name: "claude"},
				Identity{PID: 41, PPID: 40, Name: "bash"},
				Identity{PID: 42, PPID: testSelf, Name: "git"},
				Identity{PID: 43, PPID: 42, Name: "git-remote-https"},
			),
			argv: map[int][]string{30: {"node", "main.js", "--listen", "/state/sock/aaaa000011112222.sock"}},
			want: map[int]Group{
				testSelf: {Process: ProcessDaemon}, testEmacs: {Process: ProcessEmacs},
				30: {Process: ProcessShim, Workspace: "aaaa000011112222"},
				40: {Process: ProcessShimTools, Workspace: "aaaa000011112222"},
				41: {Process: ProcessShimTools, Workspace: "aaaa000011112222"},
				42: {Process: ProcessDaemonTools},
				43: {Process: ProcessDaemonTools},
			},
		},
		{
			name: "a daemon no Emacs is responsible for has no Emacs and no WebKit",
			ids: host(
				Identity{PID: testSelf, PPID: 1, Name: "agent-repld", Responsible: testSelf},
				Identity{PID: 20, PPID: 1, Name: "com.apple.WebKit.WebContent", Responsible: testEmacs},
			),
			want: map[int]Group{testSelf: {Process: ProcessDaemon}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := classify(tt.ids, testSelf, testSock, argvOf(tt.argv))

			// Assert
			if len(got.errs) != 0 {
				t.Fatalf("errs = %v", got.errs)
			}
			if !reflect.DeepEqual(got.groups, tt.want) {
				t.Fatalf("groups = %v\nwant     %v", got.groups, tt.want)
			}
		})
	}
}

func TestClassifyFoldsAWebKitDevelopmentBuildIntoItsRole(t *testing.T) {
	tests := []struct {
		name string
		proc string
		want string
	}{
		{"a development GPU process, truncated by the kernel", "com.apple.WebKit.GPU.Developmen", ProcessWebKitGPU},
		{"a development web content process", "com.apple.WebKit.WebContent.Development", ProcessWebKitWebContent},
		{"a development networking process, truncated", "com.apple.WebKit.Networking.Dev", ProcessWebKitNetworking},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ids := baseHost(Identity{PID: 20, PPID: 1, Name: tt.proc, Responsible: testEmacs})

			// Act
			got := classify(ids, testSelf, testSock, argvOf(nil))

			// Assert
			if got.groups[20] != (Group{Process: tt.want}) || len(got.unknownWebKit) != 0 {
				t.Fatalf("group = %+v, unknown %v; want %s", got.groups[20], got.unknownWebKit, tt.want)
			}
		})
	}
}

func TestClassifyLeavesAWebKitRoleNoGroupNamesOutAndSaysWhich(t *testing.T) {
	// Arrange
	ids := baseHost(Identity{PID: 20, PPID: 1, Name: "com.apple.WebKit.Model", Responsible: testEmacs})

	// Act
	got := classify(ids, testSelf, testSock, argvOf(nil))

	// Assert
	if _, ok := got.groups[20]; ok {
		t.Fatalf("a WebKit process of an unknown role was classified as %+v", got.groups[20])
	}
	if len(got.unknownWebKit) != 1 || got.unknownWebKit[0] != "com.apple.WebKit.Model" {
		t.Fatalf("unknownWebKit = %v, want the Model process named", got.unknownWebKit)
	}
}

func TestClassifyReportsAnArgvReadThatFailedForAnotherReason(t *testing.T) {
	// Arrange
	ids := baseHost(Identity{PID: 30, PPID: 1, Name: "node"})
	denied := errors.New("procmetrics: read the argv of pid 30: operation not permitted")

	// Act
	got := classify(ids, testSelf, testSock, func(int) ([]string, error) { return nil, denied })

	// Assert
	if len(got.errs) != 1 || !errors.Is(got.errs[0], denied) {
		t.Fatalf("errs = %v, want the denied read", got.errs)
	}
	if _, ok := got.groups[30]; ok {
		t.Fatalf("a node process whose argv could not be read was classified")
	}
}

func TestShimWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		argv   []string
		want   string
		wantOK bool
	}{
		{name: "a plain socket", argv: []string{"--listen", "/state/sock/aaaa.sock"}, want: "aaaa", wantOK: true},
		{name: "a generation socket", argv: []string{"--listen", "/state/sock/aaaa.n4.sock"}, want: "aaaa", wantOK: true},
		{name: "another directory", argv: []string{"--listen", "/elsewhere/aaaa.sock"}},
		{name: "not a socket", argv: []string{"--listen", "/state/sock/aaaa.txt"}},
		{name: "no listen flag", argv: []string{"main.js"}},
		{name: "a dangling listen flag", argv: []string{"--listen"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := shimWorkspace(tt.argv, testSock)

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("shimWorkspace = (%q, %v), want (%q, %v)", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}
