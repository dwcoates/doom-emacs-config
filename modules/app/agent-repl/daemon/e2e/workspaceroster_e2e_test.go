// THE WORKSPACE ROSTER, DELIVERED BY RENDEZVOUS — the acceptance suite for
// agentshim.frontend.v1's WorkspaceRoster frame (FrontendFrame arm 18) and its
// PublishWorkspaceRosterCmd command (FrontendCommand arm 25).
//
// # Why this suite exists
//
// The roster — the sidebar's entire model — used to reach each webview by
// Emacs INJECTING A SCRIPT into that view. That delivery raced page boot: a
// push whose page had not yet planted its JS hook was dropped on the floor, and
// nothing ever re-delivered it, so the sidebar rendered late, or not at all,
// with no evidence anywhere that a roster had been sent. The bug was not a
// timing constant that could be tuned; it was that the delivery had no
// rendezvous point at all.
//
// The structural fix is DELIVERY BY RENDEZVOUS. The daemon RETAINS the latest
// published roster and includes it in EVERY connect snapshot, so a client that
// connects at any instant — before the publish, after it, or after a reconnect
// — receives the current roster BY CONSTRUCTION rather than by being lucky
// enough to be listening at the moment of the push. There is no window in which
// a connected client can hold no roster while the daemon holds one.
//
// The rest of the contract follows from that one move:
//
//   - REVISION GATING. Retention plus reconnect replay means frames can reach a
//     client out of the order they were authored, so every roster carries a
//     monotonic per-Emacs-boot revision and the daemon refuses a publish that is
//     not newer than the one it holds — LOUDLY, on the correlated CommandAck, so
//     a publisher whose counter fell behind learns it instead of believing it
//     published.
//   - THE DAEMON IS A RETAINER AND A FAN-OUT, NOTHING MORE. Emacs is the single
//     author; the daemon never synthesizes, amends, or merges a roster.
//   - THE ROSTER IS EDITOR-GLOBAL. It is deliberately NOT scope-filtered: a
//     session-scoped webview renders the whole sidebar, so it receives the whole
//     roster.
//   - AN UNSET STATUS ONEOF IS A CONTRACT BREACH. The user ruled state enums out
//     entirely: the SET ARM IS THE STATUS, each arm a dedicated empty message,
//     and a row with no arm set is refused rather than defaulted to a dot.
//   - RETENTION IS IN MEMORY AND PER-PUBLISHER. A restarted daemon holds no
//     roster until the next publish, because the retained roster must not
//     outlive the Emacs boot whose revision counter authored it.
//
// # What this file asserts, and what it deliberately does not
//
// Every test here drives the REAL daemon frontend surface: the production
// frontend.Server, the production command dispatch, the production connect
// path, and the production scope filter. Publishes ride the HOST UDS transport,
// which is the transport Emacs — the roster's single author — actually uses.
//
// It is session-free and shim-free by design: the roster is editor state that
// crosses no session boundary, so a harness that spawned a node shim would add
// a skip condition (node, npm ci, a buildable store) to a suite that needs none
// of it. Nothing here can skip.
//
// # Failure mode before the implementation lands
//
// The daemon does not yet retain or rebroadcast a roster; frontend.Dispatch
// answers the publish arm with an explicit unimplemented nack. Every test below
// therefore COMPILES and FAILS with a message naming the missing behaviour. No
// test skips, and none can pass vacuously: each asserts a positive observation
// (an ok ack, a delivered frame carrying the published revision) or an absence
// bounded by an ORDERING BARRIER rather than by elapsed time.
package e2e

import (
	"bufio"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/frontend/rostertest"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
	"claude-repld/internal/workspace/merge"
)

// ---------------------------------------------------------------------------
// The harness: a session-free daemon frontend, restartable in place.
// ---------------------------------------------------------------------------

// rosterDaemon is one boot of the daemon's frontend surface over a state
// directory the caller owns.
//
// stop() is separate from t.Cleanup on purpose: the retention contract is
// "in memory, not across a restart", and the only way to observe a restart is
// for a test to stop a boot and start another over the SAME state directory.
type rosterDaemon struct {
	ts *httptest.Server
	// hostSock is the frontend UDS socket Emacs connects to. Publishes ride it
	// because Emacs is the roster's single author and this is its transport.
	hostSock string
	stop     func()
}

// rosterStateDir is a short-path state directory that also becomes $HOME.
//
// Not t.TempDir(): the test-name-derived path exceeds the 104-byte sun_path
// limit, so bind(2) fails on macOS (e2e_test.go, same reason).
//
// Moving $HOME is not cosmetic. Every path this stack resolves — the session
// lock's ~/.cache/agent-repl/run, the shim socket, the state store — is built
// by joining onto os.UserHomeDir, and on a developer machine those are the LIVE
// daemon's paths. isolatedShimSocket refuses a harness that skipped this.
func rosterStateDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "agent-repl-roster-")
	if err != nil {
		t.Fatalf("make roster state dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	t.Setenv("HOME", dir)
	return dir
}

// bootRosterDaemon stands the daemon's frontend up over stateDir: the real
// frontend.Server, the real command dispatch, the real connect path, and the
// real scope filter. No shim is ever spawned — a spawn here means the stack
// took an unmodeled path, and says so.
func bootRosterDaemon(t *testing.T, stateDir string) *rosterDaemon {
	t.Helper()
	stateStore, err := statedb.Open(filepath.Join(stateDir, "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: t.Logf})
	ssmMgr, err := ssm.Open(ssm.Options{DB: stateStore, Resolver: server.NewRegistryResolver(reg), Logf: t.Logf})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	geometryStore, err := geometry.Open(stateStore, t.Logf)
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}

	forwarder := &server.PushForwarder{Logf: t.Logf}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(isolatedShimSocket(t, stateDir)); err != nil {
		t.Fatalf("listen for shims: %v", err)
	}
	if err := sessionlock.EnsureDir(); err != nil {
		t.Fatalf("make session lock dir: %v", err)
	}
	targets := dlog.NewTargetManager()
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, false)
	if err != nil {
		t.Fatalf("build file diagnostic persister: %v", err)
	}
	noSpawn := func(string, server.CreateOpts) (server.ShimHandle, error) {
		return server.ShimHandle{}, errNoSpawnInRosterHarness
	}
	seqStore := server.NewRegistrySeqStore(reg, t.Logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: t.Logf, ModelCatalogs: modelCatalogs}
	progressMgr := progress.New(progress.Options{Logf: t.Logf})
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, shimListener.Evict, noSpawn, t.Logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		TurnAccountings:   newTestTurnAccountingStore(),
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     "0.1.0-e2e-roster",
		ProtocolVersion:   "1",
		ShimBuildSHA:      func() string { return "" },
		Logf:              t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	mergeQueue, err := merge.NewFileQueue(filepath.Join(stateDir, "merge-queue"), t.Logf)
	if err != nil {
		t.Fatalf("open merge queue: %v", err)
	}
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{Manager: ssmMgr, Queue: mergeQueue, Interrupter: controller})
	if err != nil {
		t.Fatalf("build merge lease: %v", err)
	}
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		// The boot orphan sweep scans THIS directory and no other: a test that
		// let it resolve the process temp dir deleted the live daemon's rebase
		// worktrees mid-merge.
		RebaseRoot:        t.TempDir(),
		Resumes:           &server.ConversationResolver{Reg: reg, Logf: t.Logf},
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Prompts:           controller,
		Turns:             controller,
		Health:            controller,
		Lifecycle:         stubLifecycle{},
		SessionDeaths:     server.RegistrySessionDeaths{Reg: reg},
		Resyncer:          controller,
		Catalogs:          controller,
		SessionCommands:   binding,
		WorkspaceCreation: newEmptyWorkspaceCreation(),
		MergeLease:        mergeLease,
		MergeQueue:        mergeQueue,
		MergeGeometry:     geometryStore,
		Logf:              t.Logf,
		LogVerbosef:       t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)

	srv := server.New(server.Config{
		DaemonVersion: "0.1.0-e2e-roster",
		Registry:      reg,
		ModelCatalogs: modelCatalogs,
		Controller:    controller,
		AgentShim:     agentShim,
		Logf:          t.Logf,
	})
	binding.SetTarget(srv)

	// THE HOST TRANSPORT, bound exactly as daemon startup binds it: listen
	// first, then enter the accept loop, so a dial after this function returns
	// cannot lose the race against a goroutine that had merely been scheduled.
	hostSock := filepath.Join(stateDir, "daemon-frontend.sock")
	hostListener, err := frontend.ListenUDS(hostSock)
	if err != nil {
		t.Fatalf("listen frontend UDS: %v", err)
	}
	go func() { _ = agentShim.Server.Serve(hostListener) }()

	mux := http.NewServeMux()
	// The unscoped GUI bootstrap socket: every workspace's frames.
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	// The SCOPED webview socket, built from the same production entry point the
	// daemon's GET /sessions/{id}/stream route uses (server.handleStream), with
	// the scope named in the query rather than resolved from a session record.
	// The registry lookup that route performs decides WHICH scope a connection
	// gets; it has nothing to say about what a scoped connection then receives,
	// which is the only question this suite asks of it.
	mux.HandleFunc("/scoped", func(w http.ResponseWriter, r *http.Request) {
		agentShim.Server.ServeWSScoped(w, r, frontend.Scope{
			SessionID: r.URL.Query().Get("session"),
			Workspace: r.URL.Query().Get("workspace"),
		}, frontend.ClientKindGUIStream, nil)
	})
	ts := httptest.NewServer(mux)

	var once sync.Once
	stop := func() {
		once.Do(func() {
			ts.Close()
			_ = agentShim.Close()
			_ = hostListener.Close()
			controller.Close()
			_ = shimListener.Close()
			_ = targets.Close()
			_ = ssmMgr.Close()
			_ = stateStore.Close()
		})
	}
	t.Cleanup(stop)
	return &rosterDaemon{ts: ts, hostSock: hostSock, stop: stop}
}

type rosterHarnessError string

func (e rosterHarnessError) Error() string { return string(e) }

const errNoSpawnInRosterHarness = rosterHarnessError(
	"e2e: the workspace-roster harness never spawns a shim; a spawn here means the stack took an unmodeled path")

// ---------------------------------------------------------------------------
// Connections
// ---------------------------------------------------------------------------

// rosterHost is a connection on the daemon's HOST frontend UDS — the transport
// Emacs holds, and therefore the transport a roster publish rides.
type rosterHost struct {
	nc net.Conn
	r  *bufio.Reader
}

func (d *rosterDaemon) dialHost(t *testing.T) *rosterHost {
	t.Helper()
	nc, err := net.Dial("unix", d.hostSock)
	if err != nil {
		t.Fatalf("dial the daemon's host frontend socket %s: %v", d.hostSock, err)
	}
	t.Cleanup(func() { _ = nc.Close() })
	return &rosterHost{nc: nc, r: bufio.NewReader(nc)}
}

// send writes one newline-delimited protojson FrontendCommand, the host
// transport's framing.
func (h *rosterHost) send(t *testing.T, cmd *frontendv1.FrontendCommand) {
	t.Helper()
	data, err := protojson.Marshal(cmd)
	if err != nil {
		t.Fatalf("marshal frontend command: %v", err)
	}
	if _, err := h.nc.Write(append(data, '\n')); err != nil {
		t.Fatalf("write frontend command: %v", err)
	}
}

func (h *rosterHost) readFrame(t *testing.T) *frontendv1.FrontendFrame {
	t.Helper()
	line, err := h.r.ReadBytes('\n')
	if err != nil {
		t.Fatalf("read host frame: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(line, frame); err != nil {
		t.Fatalf("protojson unmarshal host frame %s: %v", line, err)
	}
	return frame
}

// awaitAck reads host frames until the ack correlated with requestID arrives.
// Every other frame is the daemon's ordinary traffic and is skipped.
func (h *rosterHost) awaitAck(t *testing.T, requestID string) *frontendv1.CommandAck {
	t.Helper()
	for {
		frame := h.readFrame(t)
		if ack := frame.GetCommandAck(); ack != nil && ack.GetRequestId() == requestID {
			return ack
		}
	}
}

// publish sends one PublishWorkspaceRosterCmd and returns its correlated ack.
// It never asserts the ack's outcome: the acceptance and the refusal cases are
// separate tests with separate expectations.
func (h *rosterHost) publish(t *testing.T, requestID string, roster *frontendv1.WorkspaceRoster) *frontendv1.CommandAck {
	t.Helper()
	h.send(t, &frontendv1.FrontendCommand{
		RequestId: requestID,
		Command: &frontendv1.FrontendCommand_PublishWorkspaceRoster{
			PublishWorkspaceRoster: &frontendv1.PublishWorkspaceRosterCmd{Roster: roster},
		},
	})
	return h.awaitAck(t, requestID)
}

// publishOrFail publishes and requires acceptance, so a test whose SUBJECT is
// something other than the publish itself fails at the setup line that broke
// rather than three assertions later.
func (h *rosterHost) publishOrFail(t *testing.T, requestID string, roster *frontendv1.WorkspaceRoster) {
	t.Helper()
	ack := h.publish(t, requestID, roster)
	if !ack.GetOk() {
		t.Fatalf("publishing roster revision %d was refused: %s", roster.GetRevision(), ack.GetError())
	}
}

func (d *rosterDaemon) dialGUI(t *testing.T) *websocket.Conn {
	t.Helper()
	return dialRosterWS(t, "ws"+strings.TrimPrefix(d.ts.URL, "http")+"/frontend")
}

// dialScoped opens a SESSION-SCOPED webview connection — the surface the roster
// must reach anyway, because the roster is editor-global and is not
// scope-filtered.
func (d *rosterDaemon) dialScoped(t *testing.T, sessionID, workspace string) *websocket.Conn {
	t.Helper()
	return dialRosterWS(t, fmt.Sprintf("ws%s/scoped?session=%s&workspace=%s",
		strings.TrimPrefix(d.ts.URL, "http"), sessionID, workspace))
}

func dialRosterWS(t *testing.T, url string) *websocket.Conn {
	t.Helper()
	conn, resp, err := websocket.DefaultDialer.Dial(url, nil)
	if err != nil {
		t.Fatalf("dial %s: %v", url, err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

// ---------------------------------------------------------------------------
// The connect-burst barrier
// ---------------------------------------------------------------------------

// connectRoster reports the roster a freshly connected client received as part
// of its connect delivery, or nil when it received none.
//
// # Why a barrier and not a timeout
//
// "The connect snapshot carried no roster" is an ABSENCE, and an absence
// established by waiting is only ever probable. This establishes it by ORDERING
// instead. The server enqueues a connecting client's snapshot frames while
// accepting the connection, BEFORE its read loop has consumed a single inbound
// byte, and one writer drains each lane of that outbox in push order. So a
// command whose ack shares the connect burst's lane is answered strictly AFTER
// every frame the connect produced: reading up to that ack has, by
// construction, read the whole connect burst.
//
// # Why the barrier is a RESYNC
//
// The lane is the whole of it. The outbox has two — a bulk lane carrying state
// (the connect snapshot and the retained roster ride it) and a control lane
// carrying correlated request-keyed replies, which is drained FIRST so an ack
// never queues behind a large backlog. An ordinary command's ack therefore
// OVERTAKES the connect burst, and a barrier built on one reported "no roster"
// for a roster still sitting in the bulk lane — observed as a ~40% failure rate
// under repetition.
//
// `resync` is the one command whose ack rides the BULK lane, because its real
// answer is the snapshot it enqueues there (see the ackLane note in
// frontend/server.go). Its ack cannot overtake the burst, which is what makes
// this barrier structural rather than probable. The resync's own extra
// StateSnapshot is ignored below, and it enqueues no roster of its own, so the
// only roster frame this can ever return is the connect burst's.
func connectRoster(t *testing.T, conn *websocket.Conn) *frontendv1.WorkspaceRoster {
	t.Helper()
	const barrierID = "roster-connect-barrier"
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":"0","fence":""}}`, barrierID))
	var received *frontendv1.WorkspaceRoster
	for {
		frame := readFrame(t, conn)
		if roster := frame.GetWorkspaceRoster(); roster != nil {
			received = roster
		}
		if ack := frame.GetCommandAck(); ack != nil && ack.GetRequestId() == barrierID {
			return received
		}
	}
}

// awaitBroadcastRoster reads until a roster frame arrives on an ALREADY
// connected client. Unlike connectRoster it has no barrier to lean on: the
// publish that produces the frame happens on another connection, so the only
// available fact is that the frame does eventually arrive. readFrame's deadline
// bounds the wait.
func awaitBroadcastRoster(t *testing.T, conn *websocket.Conn) *frontendv1.WorkspaceRoster {
	t.Helper()
	for {
		frame := readFrame(t, conn)
		if roster := frame.GetWorkspaceRoster(); roster != nil {
			return roster
		}
	}
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

// This suite owns NO roster shape of its own. Every roster it publishes is
// built by rostertest, which is the single source the daemon's own unit suite
// builds from and checks against validateRoster directly. That is deliberate:
// this file used to carry a second, hand-built copy of the roster contract, and
// when boot_id became required the copy was left behind — every test here
// failed against a validator that was behaving exactly as specified. A fixture
// that cannot drift from the contract cannot produce that failure again.

// rosterBootID is the publisher epoch this suite publishes under. Tests whose
// subject IS the epoch boundary name their own ids instead.
const rosterBootID = "boot-e2e-a"

// validRoster binds the default epoch onto the shared fixture.
func validRoster(revision int64, opts ...rostertest.Option) *frontendv1.WorkspaceRoster {
	return rostertest.ValidRoster(rosterBootID, revision, opts...)
}

// statuslessRowRoster is valid in every respect EXCEPT that one row leaves the
// status oneof unset. That is a contract breach, not a default dot: the user
// ruled state enums out entirely, so there is no zero value for a row to fall
// back to and no "the author looked and there is none" assertion being made
// (that one has its own arm — RosterRowStatusNone). The breach is produced by a
// NAMED helper so it is visible as an illegality rather than as an edit.
func statuslessRowRoster(revision int64) *frontendv1.WorkspaceRoster {
	return validRoster(revision, rostertest.WithStatuslessRow())
}

// statuslessRowDir is the dir of the row statuslessRowRoster strips, so an
// assertion can ask whether the refusal names the offending row.
const statuslessRowDir = rostertest.MergingRowDir

// ---------------------------------------------------------------------------
// 1. Publish → ack
// ---------------------------------------------------------------------------

// TestPublishingAWorkspaceRosterIsAcked is the base of the contract: a publish
// is an ordinary frontend command, so it is answered on the correlated
// CommandAck like every other one. Emacs learns its roster landed; it never has
// to infer it from a frame it may or may not see.
func TestPublishingAWorkspaceRosterIsAcked(t *testing.T) {
	// Arrange
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	host := daemon.dialHost(t)

	// Act
	ack := host.publish(t, "roster-publish-1", validRoster(1))

	// Assert
	if !ack.GetOk() {
		t.Fatalf("the daemon refused a well-formed roster publish (revision 1): %s", ack.GetError())
	}
}

// ---------------------------------------------------------------------------
// 2. Stale publish → loud nack naming the conflict
// ---------------------------------------------------------------------------

// TestAStaleWorkspaceRosterPublishIsNackedNamingTheConflict covers the revision
// gate. Retention plus reconnect replay means an older roster can arrive after
// a newer one, so a publish that is NOT NEWER than what the daemon holds is
// refused — and refused loudly, naming both revisions, because a publisher
// whose counter fell behind can only fix that if it is told.
//
// "Not newer" is both halves: a repeat of the retained revision and a genuinely
// older one. Neither may quietly succeed.
func TestAStaleWorkspaceRosterPublishIsNackedNamingTheConflict(t *testing.T) {
	const retained = int64(41)
	cases := []struct {
		name    string
		offered int64
	}{
		{name: "a repeat of the retained revision", offered: retained},
		{name: "an older revision arriving late", offered: 17},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — the daemon holds revision 41.
			daemon := bootRosterDaemon(t, rosterStateDir(t))
			host := daemon.dialHost(t)
			host.publishOrFail(t, "roster-retained", validRoster(retained))

			// Act
			ack := host.publish(t, "roster-stale", validRoster(tc.offered))

			// Assert — refused, and the refusal names the conflict on both sides.
			if ack.GetOk() {
				t.Fatalf("the daemon accepted roster revision %d while holding %d: a superseded roster can now overwrite the current one",
					tc.offered, retained)
			}
			for _, revision := range []int64{tc.offered, retained} {
				if !strings.Contains(ack.GetError(), strconv.FormatInt(revision, 10)) {
					t.Errorf("the refusal of revision %d against retained %d never names revision %d: %q",
						tc.offered, retained, revision, ack.GetError())
				}
			}
		})
	}
}

// ---------------------------------------------------------------------------
// 3. A row with no status arm → loud nack
// ---------------------------------------------------------------------------

// TestARosterRowWithNoStatusArmIsNacked covers the oneof's rejection semantics.
// The status oneof exists precisely because a state enum was ruled out: the set
// arm IS the status, so an unset oneof is the ABSENCE of an assertion, not a
// value. The daemon refuses it rather than retaining a row nothing can draw.
//
// The opposite breach — more than one arm set — is unrepresentable in a
// generated oneof, so there is no wire this test could put it on.
func TestARosterRowWithNoStatusArmIsNacked(t *testing.T) {
	// Arrange
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	host := daemon.dialHost(t)

	// Act
	ack := host.publish(t, "roster-statusless", statuslessRowRoster(1))

	// Assert
	if ack.GetOk() {
		t.Fatalf("the daemon accepted a roster whose row %s has no status arm: a row with no lifecycle is a contract breach, not a default dot",
			statuslessRowDir)
	}
	if !strings.Contains(strings.ToLower(ack.GetError()), "status") {
		t.Errorf("the refusal of a statusless row never mentions the status oneof, so a publisher cannot tell what it got wrong: %q", ack.GetError())
	}
}

// TestARejectedWorkspaceRosterIsNotRetained is the structural half of the
// refusal above: a nack that still left the invalid roster in the retainer
// would hand every subsequent connect a roster the daemon had just declared
// illegal. Refusing means refusing to KEEP it.
func TestARejectedWorkspaceRosterIsNotRetained(t *testing.T) {
	// Arrange
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	host := daemon.dialHost(t)
	if ack := host.publish(t, "roster-statusless", statuslessRowRoster(1)); ack.GetOk() {
		t.Fatalf("the daemon accepted a roster whose row %s has no status arm", statuslessRowDir)
	}

	// Act — a client connecting after the refusal.
	roster := connectRoster(t, daemon.dialGUI(t))

	// Assert
	if roster != nil {
		t.Fatalf("a client's connect burst carried revision %d after the ONLY publish was refused: the daemon retained a roster it rejected",
			roster.GetRevision())
	}
}

// ---------------------------------------------------------------------------
// 3b. A roster with no boot_id → loud nack, and nothing retained
// ---------------------------------------------------------------------------

// TestARosterWithNoBootIdIsNackedNamingTheEpoch covers the epoch-identity half
// of the publish contract, end to end on the transport Emacs actually holds.
//
// boot_id is the key the monotonicity floor is scoped by. A publisher that will
// not name its epoch cannot be told apart from one whose epoch has ended, so
// its revision is comparable to nothing: folding it into whatever epoch happens
// to be retained would either bounce a live publisher forever or let a dead
// one's counter gate a live one. The daemon refuses it instead — and the
// refusal has to NAME boot_id, because a publisher told only "rejected" cannot
// tell a missing epoch from a stale revision.
func TestARosterWithNoBootIdIsNackedNamingTheEpoch(t *testing.T) {
	// Arrange
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	host := daemon.dialHost(t)

	// Act
	ack := host.publish(t, "roster-no-boot-id", rostertest.ValidRoster("", 1))

	// Assert — the refusal reaches the publisher, and it names the field.
	if ack.GetOk() {
		t.Fatalf("the daemon accepted a roster with no boot_id: its revision is comparable to no epoch, so the monotonicity floor now means nothing")
	}
	if !strings.Contains(ack.GetError(), "boot_id") {
		t.Errorf("the refusal of an epoch-less roster never names boot_id, so the publisher cannot tell a missing epoch from a stale revision: %q", ack.GetError())
	}
}

// TestARosterWithNoBootIdIsNotRetained is the structural half of the refusal
// above: a nack that still left the roster in the retainer would hand every
// later connect a roster the daemon had just declared illegal.
func TestARosterWithNoBootIdIsNotRetained(t *testing.T) {
	// Arrange
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	if ack := daemon.dialHost(t).publish(t, "roster-no-boot-id", rostertest.ValidRoster("", 1)); ack.GetOk() {
		t.Fatalf("the daemon accepted a roster with no boot_id")
	}

	// Act — a client connecting after the refusal.
	roster := connectRoster(t, daemon.dialGUI(t))

	// Assert
	if roster != nil {
		t.Fatalf("a client's connect burst carried revision %d after the ONLY publish was refused for a missing boot_id: the daemon retained a roster it rejected",
			roster.GetRevision())
	}
}

// ---------------------------------------------------------------------------
// 3c. A new publisher epoch resets the revision floor
// ---------------------------------------------------------------------------

// TestANewPublisherEpochResetsTheRevisionFloor covers the whole reason the
// revision floor is keyed by boot_id rather than held globally.
//
// A revision counter is minted by ONE Emacs boot. When Emacs restarts against a
// surviving daemon, its counter restarts at 1 while the daemon still retains
// revision 41 from the previous boot. Compared as bare numbers the fresh boot's
// every publish is "stale" — forever, with no honest resync available to it,
// because a nack carries no retained revision to resync against. The sidebar
// would sit frozen on a dead editor's picture.
//
// Keying the floor by epoch makes that unrepresentable: a publish under a
// DIFFERENT boot_id is not comparable to the retained one at all, so it is
// accepted whatever its revision and becomes the new floor. This asserts both
// halves — the acceptance, and that a client connecting afterwards is handed
// the new epoch's roster rather than the higher-numbered dead one.
func TestANewPublisherEpochResetsTheRevisionFloor(t *testing.T) {
	// Arrange — the daemon retains revision 41 from the previous Emacs boot.
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	host := daemon.dialHost(t)
	host.publishOrFail(t, "roster-old-epoch", rostertest.ValidRoster("boot-a", 41))

	// Act — a fresh boot publishes its FIRST roster, revision 1.
	ack := host.publish(t, "roster-new-epoch", rostertest.ValidRoster("boot-b", 1))

	// Assert — accepted, and it is what a later connect is handed.
	if !ack.GetOk() {
		t.Fatalf("the daemon refused revision 1 from a new publisher epoch while holding revision 41 from the old one: a restarted Emacs can never publish again, and no resync exists for it: %s",
			ack.GetError())
	}
	received := connectRoster(t, daemon.dialGUI(t))
	if received == nil {
		t.Fatal("a client connecting after the epoch change received no roster at all")
	}
	if received.GetBootId() != "boot-b" || received.GetRevision() != 1 {
		t.Errorf("the connect burst carried boot_id=%q revision=%d, want the new epoch's boot-b/1: the dead boot's roster is still on screen",
			received.GetBootId(), received.GetRevision())
	}
}

// ---------------------------------------------------------------------------
// 4. Rendezvous: connect AFTER the publish
// ---------------------------------------------------------------------------

// TestAClientConnectingAfterAPublishReceivesTheRetainedRoster is THE test this
// whole redesign exists for.
//
// Under script injection, a push aimed at a page that had not yet planted its
// hook was dropped and never re-sent, so a client that arrived late held no
// roster at all. Retention plus connect-time delivery removes the race rather
// than narrowing it: the client below connects with the publish already long
// finished, sends nothing, asks for nothing, and still holds the roster.
//
// It also pins the daemon's role. The roster it hands back must be byte-for-
// byte what Emacs published — the daemon retains and fans out, it never
// synthesizes or amends.
func TestAClientConnectingAfterAPublishReceivesTheRetainedRoster(t *testing.T) {
	// Arrange
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	published := validRoster(9)
	daemon.dialHost(t).publishOrFail(t, "roster-publish-1", published)

	// Act — connect only now, and send no publish of any kind.
	received := connectRoster(t, daemon.dialGUI(t))

	// Assert
	if received == nil {
		t.Fatal("a client connecting after the publish received no roster in its connect burst: the rendezvous did not happen, and nothing will re-deliver it")
	}
	if !proto.Equal(published, received) {
		t.Errorf("the retained roster differs from the one Emacs published; the daemon is a retainer, not an author.\npublished: %v\nreceived:  %v", published, received)
	}
}

// ---------------------------------------------------------------------------
// 5. Broadcast to an already-connected client
// ---------------------------------------------------------------------------

// TestAPublishReachesAnAlreadyConnectedClient is the other half of delivery:
// retention answers the client that arrives late, and this answers the one
// already sitting there. A roster that were only ever snapshotted on connect
// would leave every live sidebar frozen at whatever it held when it connected.
func TestAPublishReachesAnAlreadyConnectedClient(t *testing.T) {
	// Arrange — the client connects FIRST and drains its connect burst, so the
	// frame it later reads can only be the broadcast.
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	conn := daemon.dialGUI(t)
	if roster := connectRoster(t, conn); roster != nil {
		t.Fatalf("the connect burst already carried revision %d before anything was published", roster.GetRevision())
	}

	// Act
	daemon.dialHost(t).publishOrFail(t, "roster-publish-1", validRoster(3))

	// Assert
	if got := awaitBroadcastRoster(t, conn).GetRevision(); got != 3 {
		t.Errorf("the broadcast carried revision %d, want the published 3", got)
	}
}

// ---------------------------------------------------------------------------
// 6. Reconnect
// ---------------------------------------------------------------------------

// TestReconnectingReceivesTheRetainedRosterAgain covers the failure the old
// per-view delivery could not recover from at all: a webview that reloaded lost
// its injected roster, and Emacs had no idea it needed to re-send one. Retention
// makes a reconnect an ordinary connect — the second connection is served from
// the same retained roster as the first, with no republish anywhere.
func TestReconnectingReceivesTheRetainedRosterAgain(t *testing.T) {
	// Arrange — one publish, one client that receives it and then goes away.
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	daemon.dialHost(t).publishOrFail(t, "roster-publish-1", validRoster(12))
	first := daemon.dialGUI(t)
	if roster := connectRoster(t, first); roster.GetRevision() != 12 {
		t.Fatalf("the first client's connect burst carried revision %d, want 12", roster.GetRevision())
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close the first client: %v", err)
	}

	// Act — reconnect, with no republish in between.
	received := connectRoster(t, daemon.dialGUI(t))

	// Assert
	if received == nil {
		t.Fatal("the reconnecting client received no roster: a reload loses the sidebar until Emacs happens to publish again")
	}
	if got := received.GetRevision(); got != 12 {
		t.Errorf("the reconnecting client received revision %d, want the retained 12", got)
	}
}

// ---------------------------------------------------------------------------
// 7. Editor-global: a scoped client still receives it
// ---------------------------------------------------------------------------

// TestASessionScopedClientStillReceivesTheRoster pins the roster's scope, which
// is deliberately none.
//
// Every other view on this transport is workspace or session state and is
// filtered to the connection's scope. The roster is not: it is EDITOR-GLOBAL,
// the whole sidebar, and the sidebar a session-scoped webview draws lists every
// workspace, not its own. Filtering it would leave each webview rendering a
// one-row sidebar.
func TestASessionScopedClientStillReceivesTheRoster(t *testing.T) {
	// Arrange — a roster that mentions neither the scope's session nor its
	// workspace, so nothing about it could match a filter.
	daemon := bootRosterDaemon(t, rosterStateDir(t))
	daemon.dialHost(t).publishOrFail(t, "roster-publish-1", validRoster(5))

	// Act
	received := connectRoster(t, daemon.dialScoped(t, "session-not-in-the-roster", "/tmp/roster/unrelated-workspace"))

	// Assert
	if received == nil {
		t.Fatal("a session-scoped client received no roster: the roster is editor-global and must not be scope-filtered, or every webview renders a sidebar of one")
	}
	if got := received.GetRevision(); got != 5 {
		t.Errorf("the scoped client received revision %d, want the retained 5", got)
	}
}

// ---------------------------------------------------------------------------
// 8. No-roster boot: absence, not an empty roster
// ---------------------------------------------------------------------------

// TestConnectingBeforeAnyPublishCarriesNoRosterFrame states what a client sees
// before Emacs has published anything: NOTHING.
//
// The distinction matters because an empty roster is a claim — "the editor has
// no workspaces" — and a client that received one would draw an empty sidebar
// and believe it. Absence is the honest report of "no author has spoken yet",
// and it is what lets a client tell "not yet" from "none".
func TestConnectingBeforeAnyPublishCarriesNoRosterFrame(t *testing.T) {
	// Arrange — a daemon nobody has published to.
	daemon := bootRosterDaemon(t, rosterStateDir(t))

	// Act
	received := connectRoster(t, daemon.dialGUI(t))

	// Assert
	if received != nil {
		t.Fatalf("a connect burst before any publish carried a roster (revision %d): an empty roster asserts the editor has no workspaces, which no author has said",
			received.GetRevision())
	}
}

// ---------------------------------------------------------------------------
// 9. Daemon restart: retention is in memory, and per-publisher
// ---------------------------------------------------------------------------

// TestARestartedDaemonHoldsNoRosterUntilTheNextPublish pins retention as IN
// MEMORY, which is a contract choice and not an implementation shortcut.
//
// The revision is monotonic PER EMACS BOOT, so a retained roster is only
// meaningful next to the counter that authored it. A daemon that persisted a
// roster across its own restart would hand clients revision 30 from a previous
// life and then refuse the restarted Emacs's revision 1 as stale — the sidebar
// would be frozen on a dead editor's picture with every fresh publish bouncing.
// Holding nothing until the next publish is what keeps that unrepresentable.
func TestARestartedDaemonHoldsNoRosterUntilTheNextPublish(t *testing.T) {
	// Arrange — a daemon that has a roster, proven by a client that receives it.
	stateDir := rosterStateDir(t)
	first := bootRosterDaemon(t, stateDir)
	first.dialHost(t).publishOrFail(t, "roster-publish-1", validRoster(30))
	if roster := connectRoster(t, first.dialGUI(t)); roster.GetRevision() != 30 {
		t.Fatalf("before the restart the connect burst carried revision %d, want 30", roster.GetRevision())
	}

	// Act — restart the daemon over the SAME state directory.
	first.stop()
	second := bootRosterDaemon(t, stateDir)

	// Assert
	received := connectRoster(t, second.dialGUI(t))
	if received != nil {
		t.Fatalf("a client connecting to the restarted daemon received revision %d: the roster outlived the daemon, so a restarted Emacs's revision 1 would now be refused as stale forever",
			received.GetRevision())
	}
}
