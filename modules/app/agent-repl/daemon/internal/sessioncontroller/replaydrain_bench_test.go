package sessioncontroller

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// THE REPLAY-DRAIN BENCHMARK.
//
// After a redeploy the shim replays a whole persisted transcript at the daemon
// on the standing subscription, and the daemon's single in-order read loop is
// the only thing draining it. A ~4000-event transcript measured at roughly 110
// events/s live, which is thirty-odd seconds before a large workspace is
// usable. This drives the SAME consumer seam the read loop drives —
// consumer.Apply for lifecycle payloads, consumer.Consume for everything else —
// over a real SSM, a real state database, a real progress resolver and a real
// durable log sink, so the cost it reports is the cost the drain pays.
//
// The frontend broadcast itself is deliberately NOT in scope: the pusher here
// records what it was handed and returns. Everything upstream of it — the
// translation, the curation chain, the SQLite writes and the per-event log
// records — is real.

const (
	benchWorkspace = "/ws/replay-drain"
	benchSessionID = "s_replay_drain"
	benchNowMs     = int64(1_700_000_000_000)
	// benchTranscriptEvents is the size of the transcript one iteration
	// drains: the live measurement that motivated this work was taken over a
	// transcript of about this size.
	benchTranscriptEvents = 4000
	// benchEventsPerTurn shapes the stream into turns of a realistic length.
	benchEventsPerTurn = 40
)

// benchResolver binds the bench session to its workspace for the SSM.
type benchResolver struct{}

func (benchResolver) Session(sessionID string) (ssm.Binding, bool) {
	if sessionID != benchSessionID {
		return ssm.Binding{}, false
	}
	return ssm.Binding{Workspace: benchWorkspace, SessionID: benchSessionID}, true
}

// benchPusher counts pushes instead of retaining them, so a multi-thousand
// event drain does not measure slice growth.
type benchPusher struct {
	conversations atomic.Int64
	typing        atomic.Int64
	catalogs      atomic.Int64
	states        atomic.Int64
	inits         atomic.Int64
	heartbeats    atomic.Int64
	queues        atomic.Int64
	progress      atomic.Int64
}

func (p *benchPusher) PushConversationDelta(*frontendv1.ConversationDelta) { p.conversations.Add(1) }
func (p *benchPusher) PushTypingDelta(*frontendv1.TypingDelta)            { p.typing.Add(1) }
func (p *benchPusher) PushTaskCatalog(*frontendv1.TaskCatalog)            { p.catalogs.Add(1) }
func (p *benchPusher) PushWorkspaceState(*frontendv1.WorkspaceState)      { p.states.Add(1) }
func (p *benchPusher) PushSessionInitView(*frontendv1.SessionInitView)    { p.inits.Add(1) }
func (p *benchPusher) PushHeartbeatView(*frontendv1.HeartbeatView)        { p.heartbeats.Add(1) }
func (p *benchPusher) PushQueueView(*frontendv1.QueueView)                { p.queues.Add(1) }
func (p *benchPusher) PushProgressView(*frontendv1.ProgressView)          { p.progress.Add(1) }

// benchKeepAliveWindows is the same adapter production wires
// (server.KeepAliveWindowStore), restated here because the server package
// imports this one.
type benchKeepAliveWindows struct{ windows *statedb.KeepAliveWindows }

func (s benchKeepAliveWindows) Open(w KeepAliveWindowRecord) error {
	return s.windows.Open(statedb.KeepAliveWindow{TurnID: w.TurnID, Workspace: w.Workspace, StartedAtMs: w.StartedAtMs})
}

func (s benchKeepAliveWindows) Close(turnID string, endedAtMs int64) error {
	return s.windows.Close(turnID, endedAtMs)
}

func (s benchKeepAliveWindows) Covers(workspace string, tsMs int64) (bool, error) {
	return s.windows.Covers(workspace, tsMs)
}

func (s benchKeepAliveWindows) HasTurn(workspace, turnID string) (bool, error) {
	return s.windows.HasTurn(workspace, turnID)
}

// benchRig is one workspace's consumer over production-shaped state.
type benchRig struct {
	m            *Manager
	cons         *consumer
	push         *benchPusher
	utilizations *statedb.TokenUtilizations
}

// newBenchRig wires a live Manager over a real SSM, a real state database and a
// real durable log sink, and returns the workspace's consumer.
func newBenchRig(tb testing.TB) *benchRig {
	tb.Helper()
	dir := tb.TempDir()
	store, err := statedb.Open(filepath.Join(dir, "state.db"))
	if err != nil {
		tb.Fatalf("open state store: %v", err)
	}
	tb.Cleanup(func() { _ = store.Close() })
	turnAccountings, err := statedb.NewTurnAccountings(store)
	if err != nil {
		tb.Fatalf("open turn accountings: %v", err)
	}
	tokenUtilizations, err := statedb.NewTokenUtilizations(store)
	if err != nil {
		tb.Fatalf("open token utilizations: %v", err)
	}
	keepAliveWindows, err := statedb.NewKeepAliveWindows(store)
	if err != nil {
		tb.Fatalf("open keep-alive windows: %v", err)
	}
	mgr, err := ssm.Open(ssm.Options{
		DB:       store,
		Resolver: benchResolver{},
		Logf:     func(string, ...any) {},
	})
	if err != nil {
		tb.Fatalf("open ssm: %v", err)
	}
	tb.Cleanup(func() { _ = mgr.Close() })

	// The durable log sink production writes: every record is a JSON encode
	// plus a file write, and the drain emits several per event.
	sink, err := os.Create(filepath.Join(dir, "daemon.log"))
	if err != nil {
		tb.Fatalf("open durable log sink: %v", err)
	}
	tb.Cleanup(func() { _ = sink.Close() })
	logger := dlog.New(sink, io.Discard, false)
	logf := dlog.Legacy(logger)

	prog := progress.New(progress.Options{
		Logf:           func(string, ...any) {},
		Clock:          func() int64 { return benchNowMs },
		CoalesceWindow: -1,
	})
	tb.Cleanup(func() { _ = prog.Close() })

	push := &benchPusher{}
	m, err := New(Config{
		Push:              push,
		SSM:               mgr,
		Progress:          prog,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{benchWorkspace: benchSessionID}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   turnAccountings,
		HistoricalUsage:   tokenUtilizations,
		KeepAliveWindows:  benchKeepAliveWindows{windows: keepAliveWindows},
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Logf:              logf,
		Now:               func() int64 { return benchNowMs },
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient: func(c shimclient.Config) sessionClient {
			return &fakeClient{cfg: c}
		},
	})
	if err != nil {
		tb.Fatalf("New: %v", err)
	}
	tb.Cleanup(m.Close)
	if err := m.Ensure(benchWorkspace); err != nil {
		tb.Fatalf("Ensure: %v", err)
	}
	d, err := m.existing(benchWorkspace)
	if err != nil {
		tb.Fatalf("existing: %v", err)
	}
	return &benchRig{m: m, cons: d.consumer, push: push, utilizations: tokenUtilizations}
}

// drain feeds the events in at the seam the shim's read loop feeds them,
// routing each payload to the sink the read loop's own type switch routes it
// to.
func (r *benchRig) drain(tb testing.TB, events []*corev1.Event) {
	tb.Helper()
	for _, ev := range events {
		switch ev.GetPayload().(type) {
		case *corev1.Event_TurnStarted, *corev1.Event_TurnEnded, *corev1.Event_SessionStarted:
			if err := r.cons.Apply(ev); err != nil {
				tb.Fatalf("apply seq=%d: %v", ev.GetSeq(), err)
			}
		default:
			if err := r.cons.Consume(ev); err != nil {
				tb.Fatalf("consume seq=%d: %v", ev.GetSeq(), err)
			}
		}
	}
}

// benchTranscript builds one realistic replayed transcript: turn lifecycle
// boundaries, transcript user and assistant lines carrying API usage, streamed
// assistant messages, and content deltas.
func benchTranscript(tb testing.TB, events int) []*corev1.Event {
	tb.Helper()
	out := make([]*corev1.Event, 0, events)
	var seq uint64
	next := func() *corev1.Event {
		seq++
		return &corev1.Event{
			SessionId:    benchSessionID,
			Seq:          seq,
			Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
			ProducedAtMs: benchNowMs,
		}
	}
	vendor := func(msg proto.Message) *corev1.Event {
		a, err := anypb.New(msg)
		if err != nil {
			tb.Fatalf("anypb.New: %v", err)
		}
		ev := next()
		ev.Payload = &corev1.Event_Vendor{Vendor: a}
		return ev
	}
	for turn := 0; len(out) < events; turn++ {
		turnID := fmt.Sprintf("turn-%d", turn)
		start := next()
		start.Payload = &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: turnID}}
		start.Plane = corev1.Plane_PLANE_STREAM
		start.RequestId = turnID
		out = append(out, start)
		out = append(out, vendor(&datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: fmt.Sprintf("u-%d", turn), SessionId: benchSessionID, Cwd: benchWorkspace},
			Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{
				ContentString: fmt.Sprintf("prompt for turn %d", turn),
			}},
		}}}))
		for i := 0; i < benchEventsPerTurn/5; i++ {
			// Ephemeral: a streamed delta carries no store seq, so it neither
			// consumes one nor advances the drain's high-water mark.
			out = append(out, &corev1.Event{
				SessionId:    benchSessionID,
				Class:        corev1.EventClass_EVENT_CLASS_EPHEMERAL,
				ProducedAtMs: benchNowMs,
				Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{
					Uuid:  fmt.Sprintf("a-%d-stream", turn),
					Delta: &corev1.ContentDelta_Text{Text: "streamed assistant text chunk"},
				}},
			})
		}
		for i := 0; len(out) < events && i < benchEventsPerTurn-2-benchEventsPerTurn/5; i++ {
			out = append(out, vendor(&datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
				Envelope: &datav1.LineEnvelope{
					Uuid:      fmt.Sprintf("a-%d-%d", turn, i),
					RequestId: fmt.Sprintf("req-%d-%d", turn, i),
					SessionId: benchSessionID,
					Cwd:       benchWorkspace,
				},
				Message: &datav1.ApiAssistantMessage{
					Id:      fmt.Sprintf("msg-%d-%d", turn, i),
					Model:   "claude-opus-4",
					Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "assistant prose"}}}},
					Usage: &datav1.ApiUsage{
						InputTokens:              120,
						OutputTokens:             340,
						CacheReadInputTokens:     20000,
						CacheCreationInputTokens: 800,
					},
				},
			}}}))
		}
		end := next()
		end.Payload = &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: turnID}}
		end.Plane = corev1.Plane_PLANE_STREAM
		end.RequestId = turnID
		out = append(out, end)
	}
	return out[:events]
}

// THE DRAIN'S FINAL STATE IS THE MEASUREMENT'S ANCHOR. A benchmark that got
// faster by doing less would report exactly the same number, so the stream it
// drives is also drained under assertion: every assistant message the
// transcript carried must have reached the durable response ledger, and every
// turn must have reached the conversation.
func TestReplayDrainPersistsEveryResponseItCarried(t *testing.T) {
	// Arrange.
	events := benchTranscript(t, benchTranscriptEvents)
	rig := newBenchRig(t)
	wantResponses := 0
	wantTurns := 0
	for _, ev := range events {
		switch ev.GetPayload().(type) {
		case *corev1.Event_TurnEnded:
			wantTurns++
		case *corev1.Event_Vendor:
			var line datav1.TranscriptLine
			if err := ev.GetVendor().UnmarshalTo(&line); err != nil {
				continue
			}
			if line.GetAssistant() != nil {
				wantResponses++
			}
		}
	}

	// Act.
	rig.drain(t, events)

	// Assert.
	got, err := rig.utilizations.List(benchSessionID)
	if err != nil {
		t.Fatalf("list durable token utilizations: %v", err)
	}
	if len(got) != wantResponses {
		t.Errorf("durable responses = %d, want %d (one per assistant transcript line)", len(got), wantResponses)
	}
	if pushed := rig.push.conversations.Load(); pushed < int64(wantTurns) {
		t.Errorf("conversation pushes = %d, want at least one per completed turn (%d)", pushed, wantTurns)
	}
}

// BenchmarkReplayDrain measures the whole consumer path for one replayed
// transcript, and reports the events-per-second the drain sustains.
func BenchmarkReplayDrain(b *testing.B) {
	events := benchTranscript(b, benchTranscriptEvents)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		rig := newBenchRig(b)
		b.StartTimer()
		rig.drain(b, events)
	}
	b.StopTimer()
	perEvent := float64(b.Elapsed().Nanoseconds()) / float64(b.N*benchTranscriptEvents)
	b.ReportMetric(1e9/perEvent, "events/s")
	b.ReportMetric(perEvent/1e6, "ms/event")
}
