package harness

import (
	"bufio"
	"context"
	"encoding/json"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestShimInfoFlagFindsAValue(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js", "--listen", "/s.sock", "--log-fd", "3", "--fake"}}

	// Act
	got, ok := info.Flag("--listen")

	// Assert
	if !ok || got != "/s.sock" {
		t.Fatalf("Flag(--listen) = %q, %v, want the socket path", got, ok)
	}
}

func TestShimInfoFlagReportsAnAbsentFlag(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js"}}

	// Act
	_, ok := info.Flag("--listen")

	// Assert
	if ok {
		t.Fatal("Flag(--listen) reported present, want absent")
	}
}

func TestShimInfoHasFlagFindsABareFlag(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js", "--fake"}}

	// Act / Assert
	if !info.HasFlag("--fake") {
		t.Fatal("HasFlag(--fake) = false, want true")
	}
	if info.HasFlag("--real") {
		t.Fatal("HasFlag(--real) = true, want false")
	}
}

func TestWorkspaceLockPathMatchesTheShimContract(t *testing.T) {
	// Arrange / Act
	got := WorkspaceLockPath("/locks", "/w/one")

	// Assert
	name := filepath.Base(got)
	if len(name) != len("workspace-")+8+len(".lock") {
		t.Fatalf("WorkspaceLockPath = %q, want workspace-<8 hex>.lock", name)
	}
}

func TestWorkspaceLockPathIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := WorkspaceLockPath("/locks", "/w/one")
	noisy := WorkspaceLockPath("/locks", "/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("WorkspaceLockPath = %q and %q, want one key per cleaned path", plain, noisy)
	}
}

func TestProfileFileNameIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := profileFileName("/w/one")
	noisy := profileFileName("/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("profileFileName = %q and %q, want one name per cleaned path", plain, noisy)
	}
}

func TestShimControlConnectCapturesProcessIdentityBeforeStandDown(t *testing.T) {
	// Arrange
	socket := filepath.Join(ShortTempDir(t), "shim.ctl")
	listener, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen on fake control socket: %v", err)
	}
	t.Cleanup(func() { _ = listener.Close() })
	served := make(chan error, 1)
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			served <- err
			return
		}
		defer conn.Close()
		scanner := bufio.NewScanner(conn)
		if !scanner.Scan() {
			served <- scanner.Err()
			return
		}
		var command controlCommand
		if err := json.Unmarshal(scanner.Bytes(), &command); err != nil {
			served <- err
			return
		}
		if command.Op != "info" {
			served <- &unexpectedControlOperation{got: command.Op}
			return
		}
		served <- json.NewEncoder(conn).Encode(controlReply{OK: true, Info: &ShimInfo{PID: 424242}})
	}()
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	t.Cleanup(cancel)
	control := &ShimControl{Socket: socket, t: t, d: &Daemon{t: t, ctx: ctx}}

	// Act
	control.connect()
	t.Cleanup(control.close)

	// Assert
	if err := <-served; err != nil {
		t.Fatalf("serve opening process identity: %v", err)
	}
	if control.pid != 424242 {
		t.Fatalf("captured process id = %d, want 424242", control.pid)
	}
}

type unexpectedControlOperation struct{ got string }

func (e *unexpectedControlOperation) Error() string {
	return "unexpected control operation " + e.got
}

func TestAnOpenedShimThatHangsUpBeforeItsFirstReplyIsADepartedShim(t *testing.T) {
	// Arrange: a control socket that accepts and hangs up without answering,
	// for a shim process that has already exited -- the idle sweep stopping
	// the shim between the harness's connect and its opening info.
	socket := filepath.Join(ShortTempDir(t), "s.ctl")
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			_ = c.Close()
		}
	}()
	exited := exec.Command("true")
	if err := exited.Run(); err != nil {
		t.Fatalf("run a process to exit: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	t.Cleanup(cancel)
	s := &ShimControl{Socket: socket, t: t, d: &Daemon{t: t, ctx: ctx}}

	// Act
	s.connectOrDeparted(exited.Process.Pid)

	// Assert
	if !s.departed || s.pid != exited.Process.Pid {
		t.Fatalf("departed = %v, pid = %d; want the departed shim %d", s.departed, s.pid, exited.Process.Pid)
	}
}

// serveSessionPushes serves a fake control socket: the opening info, then
// every push_session_update answered with the next of counts (the last one
// repeating). It reports each push's op on pushes.
func serveSessionPushes(t *testing.T, counts []int) (socket string, pushes <-chan string) {
	t.Helper()
	socket = filepath.Join(ShortTempDir(t), "p.ctl")
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	seen := make(chan string, 16)
	go func() {
		conn, err := ln.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		scanner := bufio.NewScanner(conn)
		enc := json.NewEncoder(conn)
		next := 0
		for scanner.Scan() {
			var command controlCommand
			if err := json.Unmarshal(scanner.Bytes(), &command); err != nil {
				return
			}
			if command.Op == "info" {
				_ = enc.Encode(controlReply{OK: true, Info: &ShimInfo{PID: 4242}})
				continue
			}
			seen <- command.Op
			count := counts[len(counts)-1]
			if next < len(counts) {
				count = counts[next]
			}
			next++
			_ = enc.Encode(controlReply{OK: true, Count: count})
		}
	}()
	return socket, seen
}

// TestASessionPushIsRetriedUntilASessionStreamReceivesIt pins the shared
// subscribed push: a push no stream received is sent again, and the first
// one received ends it.
func TestASessionPushIsRetriedUntilASessionStreamReceivesIt(t *testing.T) {
	cases := []struct {
		name string
		push func(*ShimControl)
	}{
		{name: "the readiness push", push: func(s *ShimControl) { s.PushHealthyWhenSubscribed() }},
		{name: "an unhealthy push", push: func(s *ShimControl) { s.PushUnhealthy(&conversationv1.SessionFault{Component: "store client"}) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the first two pushes reach no stream, the third one does.
			socket, pushes := serveSessionPushes(t, []int{0, 0, 1})
			ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
			t.Cleanup(cancel)
			s := &ShimControl{Socket: socket, t: t, d: &Daemon{t: t, ctx: ctx}}
			s.connect()
			t.Cleanup(s.close)

			// Act
			tc.push(s)

			// Assert
			if got := len(pushes); got != 3 {
				t.Fatalf("pushes sent = %d, want 3: retried until one was received, then no more", got)
			}
			for range 3 {
				if op := <-pushes; op != "push_session_update" {
					t.Fatalf("op = %q, want push_session_update", op)
				}
			}
		})
	}
}

func TestStampedPush(t *testing.T) {
	tests := []struct {
		name  string
		op    string
		agent string
		at    EntryStamp
		want  controlCommand
	}{
		{name: "an unstamped frame push names only the agent and the frame", op: opPushAgentFrame, agent: "main",
			want: controlCommand{Op: "push_agent_frame", Agent: "main", Payload: "p"}},
		{name: "every stamp rides at once", op: opPushAgentFrame, agent: "main", at: EntryStamp{Pointer: "7", Turn: "turn-1", PlaceMs: 1000},
			want: controlCommand{Op: "push_agent_frame", Agent: "main", Pointer: "7", Turn: "turn-1", PlaceMs: 1000, Payload: "p"}},
		{name: "an empty agent leaves the payload's own agent to the fake", op: opPushAgentFrame, at: EntryStamp{Turn: "turn-1"},
			want: controlCommand{Op: "push_agent_frame", Turn: "turn-1", Payload: "p"}},
		{name: "a prompt push carries the same stamps", op: opPushUserPrompt, agent: "main", at: EntryStamp{Pointer: "g-1", Turn: "turn-1", PlaceMs: 1000},
			want: controlCommand{Op: "push_user_prompt", Agent: "main", Pointer: "g-1", Turn: "turn-1", PlaceMs: 1000, Payload: "p"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := stampedPush(tt.op, tt.agent, tt.at, "p")

			// Assert
			if got != tt.want {
				t.Fatalf("stampedPush = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestEveryEntryPushSendsStampedPush(t *testing.T) {
	// A push variant that builds its own command can drop a stamp the others
	// carry; only the op constants may spell the two entry ops.
	src, err := os.ReadFile("shimctl.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, op := range []string{`"push_agent_frame"`, `"push_user_prompt"`} {
		if n := strings.Count(string(src), op); n != 1 {
			t.Fatalf("shimctl.go spells %s %d times, want once, in its op constant", op, n)
		}
	}
}
