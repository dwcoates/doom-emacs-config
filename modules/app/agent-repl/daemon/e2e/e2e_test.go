// Package e2e exercises the full post-cutover daemon⇄shim stack end to end:
// a real claude-repld frontend surface + per-session driver spawning the real
// TS claude-shim in UDS mode (--uds-socket + --store-socket, --fake offline),
// writing to a real agent-shim/shim-store, with the daemon consuming the
// merged stream and rendering agentshim.frontend.v1 frames onto the existing
// GET /sessions/{id}/stream WebSocket (scope-filtered per session).
//
// Prerequisites (the test SKIPS loudly, never fails, when any is absent):
//   - node on PATH
//   - the shim bundle built: agent-shim/claude-shim/dist/main.js
//     (run `npm run build` in agent-shim/claude-shim/)
//   - a buildable agent-shim/shim-store (go build)
package e2e

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessiondrv"
	"claude-repld/internal/shim"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/merge"
)

var frameTimeout = 30 * time.Second

func repoRoot(t *testing.T) string {
	t.Helper()
	// daemon/e2e -> daemon -> agent-repl
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve repo root: %v", err)
	}
	return root
}

func shimScriptPath(t *testing.T) string {
	t.Helper()
	path := filepath.Join(repoRoot(t), "agent-shim", "claude-shim", "dist", "main.js")
	if _, err := os.Stat(path); err != nil {
		t.Skipf("shim not built (%s missing): run `npm run build` in agent-shim/claude-shim/", path)
	}
	return path
}

func nodePath(t *testing.T) string {
	t.Helper()
	node, err := exec.LookPath("node")
	if err != nil {
		t.Skip("node not found in PATH")
	}
	return node
}

// buildShimStore compiles the shim-store into a temp binary, skipping loudly if
// the build cannot run (no go toolchain, missing module).
func buildShimStore(t *testing.T) string {
	t.Helper()
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not found in PATH")
	}
	storeDir := filepath.Join(repoRoot(t), "agent-shim", "shim-store")
	if _, err := os.Stat(storeDir); err != nil {
		t.Skipf("shim-store source not present (%s)", storeDir)
	}
	bin := filepath.Join(t.TempDir(), "shim-store")
	cmd := exec.Command("go", "build", "-o", bin, ".")
	cmd.Dir = storeDir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Skipf("shim-store build failed (skipping UDS e2e): %v\n%s", err, out)
	}
	return bin
}

// startShimStore launches the store on a temp socket and waits for it to
// listen. It is torn down at test end.
func startShimStore(t *testing.T, bin, sock string) {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "events.db")
	logPath := filepath.Join(t.TempDir(), "shim-store.log")
	cmd := exec.Command(bin, "-socket", sock, "-db", dbPath, "-log", logPath)
	cmd.Stderr = &testLogWriter{t: t, tag: "shim-store"}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start shim-store: %v", err)
	}
	t.Cleanup(func() {
		_ = cmd.Process.Kill()
		_, _ = cmd.Process.Wait()
	})
	// Wait for the socket to appear (the store creates it on listen).
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if _, err := os.Stat(sock); err == nil {
			return
		}
		time.Sleep(20 * time.Millisecond)
	}
	t.Fatalf("shim-store did not create %s in time", sock)
}

type testLogWriter struct {
	t   *testing.T
	tag string
}

func (w *testLogWriter) Write(p []byte) (int, error) {
	w.t.Logf("[%s] %s", w.tag, bytes.TrimRight(p, "\n"))
	return len(p), nil
}

// --- minimal WireAgentShim stubs (merge/lifecycle unused here) --------------

type stubMergeDirs struct{}

func (stubMergeDirs) Resolve(string) (merge.Request, error) {
	return merge.Request{}, fmt.Errorf("e2e: merge not exercised")
}

type stubLifecycle struct{}

func (stubLifecycle) Close(context.Context, string) error { return nil }
func (stubLifecycle) Open(context.Context, string) error  { return nil }

type e2eHarness struct {
	ts *httptest.Server
}

func newUDSHarness(t *testing.T) *e2eHarness {
	t.Helper()
	node := nodePath(t)
	script := shimScriptPath(t)
	storeBin := buildShimStore(t)
	storeSock := filepath.Join(t.TempDir(), "store.sock")
	startShimStore(t, storeBin, storeSock)

	reg := registry.Open(filepath.Join(t.TempDir(), "reg.json"), t.Logf)
	ssmMgr, err := ssm.Open(ssm.Options{
		DBPath:   filepath.Join(t.TempDir(), "ssm.db"),
		Resolver: server.NewRegistryResolver(reg),
		Logf:     t.Logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = ssmMgr.Close() })

	forwarder := &server.PushForwarder{Logf: t.Logf}
	udsSpawn := func(sessionID string, opts server.CreateOpts, socketPath string) (func() error, error) {
		argv := server.ShimUDSArgv(node, script, sessionID, true /*forceFake*/, opts, socketPath)
		argv = append(argv, "--store-socket", storeSock)
		proc, spawnErr := shim.Spawn(shim.Options{Argv: argv, Dir: opts.CWD, Logf: t.Logf})
		if spawnErr != nil {
			return nil, spawnErr
		}
		go func() {
			for range proc.Events() { //nolint:revive
			}
		}()
		t.Cleanup(func() {
			_ = proc.Terminate()
			_ = proc.Wait()
		})
		return func() error { return proc.Terminate() }, nil
	}
	driver, err := sessiondrv.New(sessiondrv.Config{
		Push:            forwarder,
		SSM:             ssmMgr,
		Spawner:         server.NewShimSpawner(reg, nil, udsSpawn, t.Logf),
		Locator:         &server.SessionLocator{Reg: reg},
		SeqStore:        server.NewRegistrySeqStore(reg, t.Logf),
		Registrar:       server.RegistryRegistrar{Reg: reg, Logf: t.Logf},
		DaemonVersion:   "0.1.0-e2e",
		ProtocolVersion: "1",
		Logf:            t.Logf,
	})
	if err != nil {
		t.Fatalf("build driver: %v", err)
	}
	t.Cleanup(driver.Close)

	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		SSM:       ssmMgr,
		Prompts:   driver,
		MergeDirs: stubMergeDirs{},
		Lifecycle: stubLifecycle{},
		Resyncer:  driver,
		Logf:      t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)
	t.Cleanup(func() { _ = agentShim.Close() })

	srv := server.New(server.Config{
		DaemonVersion: "0.1.0-e2e",
		Registry:      reg,
		Driver:        driver,
		SSM:           ssmMgr,
		Frontend:      agentShim.Server,
		Logf:          t.Logf,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &e2eHarness{ts: ts}
}

func (h *e2eHarness) createSession(t *testing.T, cwd string) string {
	t.Helper()
	body := fmt.Sprintf(`{"fake":true,"cwd":%q}`, cwd)
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("POST /sessions status = %d", resp.StatusCode)
	}
	var created struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		t.Fatalf("decode create: %v", err)
	}
	return created.SessionID
}

func (h *e2eHarness) dial(t *testing.T, sessionID string) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + sessionID + "/stream"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial stream: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

// readFrame reads one frontend.v1 protojson FrontendFrame off the socket.
func readFrame(t *testing.T, conn *websocket.Conn) *frontendv1.FrontendFrame {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(frameTimeout)); err != nil {
		t.Fatalf("deadline: %v", err)
	}
	_, data, err := conn.ReadMessage()
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(data, frame); err != nil {
		t.Fatalf("protojson unmarshal %s: %v", data, err)
	}
	return frame
}

func writeCmd(t *testing.T, conn *websocket.Conn, cmd string) {
	t.Helper()
	if err := conn.WriteMessage(websocket.TextMessage, []byte(cmd)); err != nil {
		t.Fatalf("write: %v", err)
	}
}

// TestE2EUDSTextTurnRendersFrontendFrames drives one text turn through the full
// UDS stack and asserts the daemon renders it as frontend.v1 frames on the
// scoped /stream socket: the connect StateSnapshot first, then (after the
// prompt) at least one ConversationDelta or a WorkspaceState transition for
// this session's workspace.
func TestE2EUDSTextTurnRendersFrontendFrames(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)

	// The scoped connection opens with a StateSnapshot.
	first := readFrame(t, conn)
	if first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// Act — send the webapp's still-Layer-2 user-message command.
	writeCmd(t, conn, `{"type":"user-message","request_id":"r1","content":"hello uds e2e"}`)

	// Assert — a conversation delta or a workspace-state for this session
	// arrives within the timeout (the fake SDK echoes the prompt back).
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_ConversationDelta:
			if f.ConversationDelta.GetSessionId() == id || f.ConversationDelta.GetWorkspace() == cwd {
				return // success: the turn rendered as frontend.v1
			}
		case *frontendv1.FrontendFrame_WorkspaceState:
			if f.WorkspaceState.GetSessionId() == id || f.WorkspaceState.GetWorkspace() == cwd {
				return // success: the driver drove an SSM transition for this session
			}
		}
	}
	t.Fatal("no ConversationDelta or WorkspaceState for the session arrived before the deadline")
}
