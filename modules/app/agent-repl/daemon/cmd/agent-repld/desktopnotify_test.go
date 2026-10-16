package main

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"google.golang.org/protobuf/encoding/protojson"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shellword"
	"agent-repld/internal/wsm"
)

// namesDB answers one workspace record, or one error.
type namesDB struct {
	wsm.DB
	record wsm.Workspace
	err    error
}

func (d namesDB) Workspace(context.Context, ids.WorkspaceID) (wsm.Workspace, error) {
	return d.record, d.err
}

func TestBannerWorkspacesAnswersTheRegistryNameAndDirectory(t *testing.T) {
	// Arrange
	workspaces := bannerWorkspaces{db: namesDB{record: wsm.Workspace{Name: "fix-flaky-reconnect", Dir: "/work/fix-flaky-reconnect"}}}

	// Act
	got, err := workspaces.Workspace(context.Background(), "ws1")

	// Assert
	want := desktopnotify.Workspace{Name: "fix-flaky-reconnect", Dir: "/work/fix-flaky-reconnect"}
	if err != nil || got != want {
		t.Fatalf("Workspace = (%+v, %v), want %+v", got, err, want)
	}
}

func TestBannerWorkspacesSurfacesAFailedRead(t *testing.T) {
	// Arrange
	cause := errors.New("no such workspace")
	workspaces := bannerWorkspaces{db: namesDB{err: cause}}

	// Act
	_, err := workspaces.Workspace(context.Background(), "ws1")

	// Assert
	if !errors.Is(err, cause) || !strings.Contains(err.Error(), `"ws1"`) {
		t.Fatalf("Workspace error = %v, want the read's cause naming the workspace", err)
	}
}

// argvRecorder writes an executable that writes each of its arguments on its
// own NUL-terminated line into a file beside it, and answers that file.
func argvRecorder(t *testing.T, dir string) (exe, record string) {
	t.Helper()
	exe = filepath.Join(dir, "agent-repld")
	record = filepath.Join(dir, "argv")
	script := "#!/bin/sh\nfor a in \"$@\"; do printf '%s\\0' \"$a\"; done > " + shellword.Quote(record) + "\n"
	if err := os.WriteFile(exe, []byte(script), 0o755); err != nil {
		t.Fatalf("write %s: %v", exe, err)
	}
	return exe, record
}

func TestSelectWorkspaceClickRunsTheDaemonsSelectWorkspace(t *testing.T) {
	cases := []struct {
		name   string
		target desktopnotify.Target
		state  string
	}{
		{name: "a plain directory", target: desktopnotify.Target{ID: "ws1", Dir: "/work/ws1"}, state: "/state"},
		{name: "quotes and spaces stay inside their words", target: desktopnotify.Target{ID: "ws2", Dir: `/work/it's "here"`}, state: "/state dir/x"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			exe, record := argvRecorder(t, t.TempDir())
			command := selectWorkspaceClick(exe, tc.state)(tc.target)

			// Act
			if out, err := exec.Command("/bin/sh", "-c", command).CombinedOutput(); err != nil {
				t.Fatalf("sh -c %q: %v: %s", command, err, out)
			}

			// Assert
			raw, err := os.ReadFile(record)
			if err != nil {
				t.Fatalf("read %s: %v", record, err)
			}
			argv := strings.Split(strings.TrimSuffix(string(raw), "\x00"), "\x00")
			if len(argv) != 5 || !reflect.DeepEqual(argv[:4], []string{"call", "-state-dir", tc.state, "SelectWorkspace"}) {
				t.Fatalf("the click ran %q, want call -state-dir %q SelectWorkspace <request>", argv, tc.state)
			}
			var request agentreplv1.SelectWorkspaceRequest
			if err := protojson.Unmarshal([]byte(argv[4]), &request); err != nil {
				t.Fatalf("the call verb cannot read the click's request %q: %v", argv[4], err)
			}
			if request.GetWorkspace().GetId() != string(tc.target.ID) || request.GetWorkspace().GetDir() != tc.target.Dir {
				t.Fatalf("request = %v, want the target's id and directory", &request)
			}
		})
	}
}

func TestSelectWorkspaceClickNamesTheCallVerbsMethod(t *testing.T) {
	// Act
	method := agentReplService().Methods().ByName("SelectWorkspace")

	// Assert
	if method == nil || method.IsStreamingServer() || method.IsStreamingClient() {
		t.Fatalf("SelectWorkspace = %v, want a unary AgentRepl method the call verb can send", method)
	}
}

// recordingClicks records the clicks relayed to it.
type recordingClicks struct{ got []ids.WorkspaceID }

func (c *recordingClicks) NotificationClicked(ws ids.WorkspaceID) { c.got = append(c.got, ws) }

func TestClickForwarderRelaysToTheBoundServer(t *testing.T) {
	// Arrange
	target := &recordingClicks{}
	f := &clickForwarder{}
	f.bind(target)

	// Act
	f.NotificationClicked("ws1")

	// Assert
	if len(target.got) != 1 || target.got[0] != "ws1" {
		t.Fatalf("relayed %v, want [ws1]", target.got)
	}
}

func TestClickForwarderRefusesAClickBeforeTheServerIsBound(t *testing.T) {
	// Arrange
	f := &clickForwarder{}

	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unbound forwarder accepted a click")
		}
	}()

	// Act
	f.NotificationClicked("ws1")
}

func TestResolveBannerBackendTakesTheOverride(t *testing.T) {
	// Arrange
	fake := filepath.Join(t.TempDir(), "fake-banner")
	if err := os.WriteFile(fake, []byte("#!/bin/sh\necho 'terminal-notifier 3.1.0.'\n"), 0o755); err != nil {
		t.Fatalf("write %s: %v", fake, err)
	}
	t.Setenv("AGENT_REPL_NOTIFIER_CMD", fake)
	log := dlog.NewTestLogger()

	// Act
	backend, err := resolveBannerBackend(context.Background(), log, selectWorkspaceClick("/bin/agent-repld", "/state"), desktopnotify.DefaultDeliveredBound)

	// Assert
	if err != nil {
		t.Fatalf("resolveBannerBackend: %v", err)
	}
	if backend.Program() != fake {
		t.Fatalf("program = %q, want the override", backend.Program())
	}
}

func TestResolveChimeTakesTheOverride(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_CHIME_CMD", "/tmp/fake-chime")
	log := dlog.NewTestLogger()

	// Act
	chime, err := resolveChime(log)

	// Assert
	if err != nil {
		t.Fatalf("resolveChime: %v", err)
	}
	if chime.Program() != "/tmp/fake-chime" {
		t.Fatalf("program = %q, want the override", chime.Program())
	}
}

// fakeProgram is a resolved program for resolveNotifyProgram.
type fakeProgram struct{ bin string }

func (p fakeProgram) Program() string { return p.bin }

func TestResolveNotifyProgramRecordsTheResolvedProgram(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_OVERRIDE", "/tmp/override")
	log := dlog.NewTestLogger()

	// Act
	got, err := resolveNotifyProgram(log, "chime", "AGENT_REPL_TEST_OVERRIDE", func(_, override string) (fakeProgram, error) {
		return fakeProgram{bin: override}, nil
	})

	// Assert
	if err != nil || got.bin != "/tmp/override" {
		t.Fatalf("resolveNotifyProgram = (%+v, %v), want the override", got, err)
	}
	r := findRecord(t, log.Records(), "resolved the desktop chime program")
	if r.Level != "info" || r.Context["role"] != "chime" || r.Context["program"] != "/tmp/override" {
		t.Fatalf("record = %+v, want the role and program at info", r)
	}
}

func TestResolveNotifyProgramRecordsAMissingProgram(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	cause := errors.New("not installed")

	// Act
	_, err := resolveNotifyProgram(log, "banner", "AGENT_REPL_TEST_UNSET", func(_, _ string) (fakeProgram, error) {
		return fakeProgram{}, cause
	})

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the build's cause", err)
	}
	r := findRecord(t, log.Records(), "no desktop banner program; it will not run")
	if r.Level != "error" || r.Context["role"] != "banner" || r.Context["cause"] != "not installed" || r.Context["override_env"] != "AGENT_REPL_TEST_UNSET" {
		t.Fatalf("record = %+v, want the role, cause and override env at error", r)
	}
}

// TestNotifyProgramsResolveThroughOneHelper fails a resolver that hand-rolls
// its boot record instead of asking resolveNotifyProgram.
func TestNotifyProgramsResolveThroughOneHelper(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("desktopnotify.go")
	if err != nil {
		t.Fatalf("read desktopnotify.go: %v", err)
	}

	// Act
	asks := strings.Count(string(src), "return resolveNotifyProgram(")
	records := strings.Count(string(src), "log.Error(opDesktopNotify")

	// Assert
	if asks != 2 || records != 1 {
		t.Fatalf("resolveNotifyProgram( x%d (want 2), log.Error(opDesktopNotify x%d (want 1)", asks, records)
	}
}
