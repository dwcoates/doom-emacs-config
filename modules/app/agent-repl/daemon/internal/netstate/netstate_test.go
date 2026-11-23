package netstate

import (
	"context"
	"errors"
	"slices"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
)

// scriptedProbe answers each Probe with the next of its answers, and counts.
type scriptedProbe struct {
	mu      sync.Mutex
	answers []probeStep
	asked   int
}

type probeStep struct {
	answer ProbeAnswer
	err    error
}

func (p *scriptedProbe) Probe(context.Context) (ProbeAnswer, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.asked++
	if len(p.answers) == 0 {
		panic("scriptedProbe: asked more often than the test scripted")
	}
	step := p.answers[0]
	p.answers = p.answers[1:]
	return step.answer, step.err
}

func (p *scriptedProbe) count() int {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.asked
}

var (
	unreachable = probeStep{answer: ProbeAnswer{Detail: "api.anthropic.com: dial tcp: lookup: no such host"}}
	reachable   = probeStep{answer: ProbeAnswer{Reachable: true, Detail: "api.anthropic.com answered HEAD with 404"}}
	refused     = probeStep{err: errors.New("vendor calls are forbidden")}
)

// beat is a clock whose every wait the test releases by hand: each After
// hands its channel to the test, so the test knows the beat is waiting.
type beat struct{ waits chan chan time.Time }

func newBeat() *beat { return &beat{waits: make(chan chan time.Time)} }

func (b *beat) After(time.Duration) <-chan time.Time {
	c := make(chan time.Time, 1)
	b.waits <- c
	return c
}

// tick releases the beat's next wait.
func (b *beat) tick() { (<-b.waits) <- time.Time{} }

func newMonitor(t *testing.T, probe Prober) (*Monitor, *dlog.TestLogger, *beat) {
	t.Helper()
	life, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	log := dlog.NewTestLogger()
	clock := newBeat()
	return New(life, Deps{Log: log, Probe: probe, After: clock.After, Now: clocktest.New(t0).Now}), log, clock
}

// t0 is when every test's first outage begins.
var t0 = time.Unix(1_800_000_000, 0).UTC()

func networkFault(id string) health.FaultLine {
	return health.FaultLine{ID: ids.FaultID(id), Kind: health.KindNetworkUnreachable}
}

func isClosed(ch <-chan struct{}) bool {
	select {
	case <-ch:
		return true
	default:
		return false
	}
}

func TestClassify(t *testing.T) {
	tests := []struct {
		name         string
		fault        bool
		words        string
		probe        []probeStep
		want         Verdict
		wantProbes   int
		wantObserved bool
	}{
		{name: "an open network fault decides it without a probe", fault: true, words: "timeout: killed",
			want: Verdict{Network: true, Evidence: EvidenceFaultOpen, Detail: "a network_unreachable fault stands"}},
		{name: "a socket code in the CLI's words decides it", words: "exit_status: getaddrinfo ENOTFOUND api.anthropic.com",
			want: Verdict{Network: true, Evidence: EvidenceCLIWords, Detail: "ENOTFOUND"}, wantObserved: true},
		{name: "the CLI's sentence decides it", words: "API Error: Can't reach the API server",
			want: Verdict{Network: true, Evidence: EvidenceCLIWords, Detail: "Can't reach the API server"}, wantObserved: true},
		{name: "an unreachable probe decides it", words: "timeout: the run did not answer", probe: []probeStep{unreachable},
			want: Verdict{Network: true, Evidence: EvidenceReachabilityCheck, Detail: unreachable.answer.Detail}, wantProbes: 1, wantObserved: true},
		{name: "a reachable probe says it was not the network", words: "timeout: the run did not answer", probe: []probeStep{reachable},
			want: Verdict{Detail: reachable.answer.Detail}, wantProbes: 1},
		{name: "a probe that cannot run says nothing", words: "timeout", probe: []probeStep{refused},
			want: Verdict{Detail: "the reachability check could not run: vendor calls are forbidden"}, wantProbes: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			probe := &scriptedProbe{answers: tc.probe}
			m, _, clock := newMonitor(t, probe)
			if tc.fault {
				m.FaultOpened("ws-1", networkFault("f-1"))
			}
			// Act
			got := m.Classify(context.Background(), tc.words)
			// Assert
			if got != tc.want {
				t.Fatalf("Classify = %+v, want %+v", got, tc.want)
			}
			if probe.count() != tc.wantProbes {
				t.Fatalf("probed %d times, want %d", probe.count(), tc.wantProbes)
			}
			if tc.wantObserved {
				// The observed outage's beat is waiting: the network is down.
				<-clock.waits
				if isClosed(m.Restored()) {
					t.Fatal("Restored is closed during an observed outage")
				}
			}
		})
	}
}

func TestRestoredIsClosedWhileTheNetworkIsUp(t *testing.T) {
	// Arrange
	m, _, _ := newMonitor(t, &scriptedProbe{})
	// Act
	restored := m.Restored()
	// Assert
	if !isClosed(restored) {
		t.Fatal("Restored is open with no outage standing")
	}
}

func TestFaultsDecideTheRestoration(t *testing.T) {
	tests := []struct {
		name         string
		open         []health.FaultLine
		close        []string
		wantRestored bool
	}{
		{name: "the last network fault closing restores the network",
			open: []health.FaultLine{networkFault("f-1")}, close: []string{"f-1"}, wantRestored: true},
		{name: "one of two network faults closing does not",
			open: []health.FaultLine{networkFault("f-1"), networkFault("f-2")}, close: []string{"f-1"}},
		{name: "both of two network faults closing does",
			open: []health.FaultLine{networkFault("f-1"), networkFault("f-2")}, close: []string{"f-2", "f-1"}, wantRestored: true},
		{name: "a fault of another kind closing is not the network's",
			open: []health.FaultLine{networkFault("f-1"), {ID: "f-9", Kind: health.KindShimReported}}, close: []string{"f-9"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, log, _ := newMonitor(t, &scriptedProbe{})
			for _, line := range tc.open {
				m.FaultOpened("ws-1", line)
			}
			restored := m.Restored()
			// Act
			for _, id := range tc.close {
				m.FaultClosed("ws-1", ids.FaultID(id))
			}
			// Assert
			if isClosed(restored) != tc.wantRestored {
				t.Fatalf("restored = %v, want %v", isClosed(restored), tc.wantRestored)
			}
			_, said := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "the last network fault closed; the network is restored"})
			if said != tc.wantRestored {
				t.Fatalf("restoration record = %v, want %v: %v", said, tc.wantRestored, log.Records())
			}
		})
	}
}

// awaitSaid waits for the record of MESSAGE, on the logger's own append, so
// a test waits for the canonical record of an edge rather than for time to
// pass. The messages each test awaits are distinct, so the first record of
// one is the one the step made.
func awaitSaid(t *testing.T, l *dlog.TestLogger, message string) {
	t.Helper()
	if _, err := l.Await(context.Background(), dlog.RecordQuery{Message: message}); err != nil {
		t.Fatalf("await %q: %v", message, err)
	}
}

const (
	saidBeatEnded     = "the reachability check reached the API; the outage the daemon observed is over"
	saidBeatRefused   = "the reachability check could not run while an observed outage stands; the outage ends only when it can"
	saidStillDown     = "the API is still unreachable"
	saidNewerObserved = "the API answered, but a newer outage was observed while it was asked; the beat goes on"
)

// observedOutage answers a Monitor in an outage it observed through its own
// probe, whose beat answers BEATS in turn.
func observedOutage(t *testing.T, beats ...probeStep) (*Monitor, *dlog.TestLogger, *beat) {
	t.Helper()
	life, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	log := dlog.NewTestLogger()
	clock := newBeat()
	m := New(life, Deps{Log: log, Probe: &scriptedProbe{answers: append([]probeStep{unreachable}, beats...)}, After: clock.After, Now: clocktest.New(t0).Now})
	m.Classify(context.Background(), "timeout")
	return m, log, clock
}

func TestTheProbeBeat(t *testing.T) {
	tests := []struct {
		name string
		// beats are what each beat's probe answers; each is released in turn
		// and its record awaited.
		beats        []probeStep
		said         []string
		wantRestored bool
	}{
		{name: "a beat that reaches the API ends the outage and restores the network",
			beats: []probeStep{reachable}, said: []string{saidBeatEnded}, wantRestored: true},
		{name: "a beat that cannot reach the API leaves the network down",
			beats: []probeStep{unreachable}, said: []string{saidStillDown}},
		{name: "a beat that cannot run is recorded and leaves the network down",
			beats: []probeStep{refused}, said: []string{saidBeatRefused}},
		{name: "the beat goes on until the API answers",
			beats: []probeStep{unreachable, refused, reachable}, said: []string{saidStillDown, saidBeatRefused, saidBeatEnded}, wantRestored: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, log, clock := observedOutage(t, tc.beats...)
			restored := m.Restored()
			// Act
			for i := range tc.beats {
				clock.tick()
				awaitSaid(t, log, tc.said[i])
			}
			// Assert
			if isClosed(restored) != tc.wantRestored {
				t.Fatalf("restored = %v, want %v", isClosed(restored), tc.wantRestored)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: op, Message: tc.said[len(tc.said)-1]}); !ok {
				t.Fatalf("no record %q: %v", tc.said[len(tc.said)-1], log.Records())
			}
		})
	}
}

func TestAFaultStandingPastTheBeatKeepsTheNetworkDown(t *testing.T) {
	// Arrange: an observed outage, then a shim's network fault opens.
	m, log, clock := observedOutage(t, reachable)
	m.FaultOpened("ws-1", networkFault("f-1"))
	restored := m.Restored()
	// Act: the beat reaches the API.
	clock.tick()
	awaitSaid(t, log, saidBeatEnded)
	// Assert: the fault still says down.
	if isClosed(restored) {
		t.Fatal("the beat restored the network while a network fault stands")
	}
	// Act: the fault closes.
	m.FaultClosed("ws-1", "f-1")
	// Assert
	if !isClosed(restored) {
		t.Fatal("the last network fault closing did not restore the network")
	}
}

func TestANewerObservationOutlivesTheAnswerAskedBeforeIt(t *testing.T) {
	// Arrange: the beat's probe answers only once the test says so, and a
	// newer outage is observed while it is being asked.
	life, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	log := dlog.NewTestLogger()
	clock := newBeat()
	gate := &gatedProbe{first: unreachable.answer, asked: make(chan struct{}), release: make(chan ProbeAnswer)}
	m := New(life, Deps{Log: log, Probe: gate, After: clock.After, Now: clocktest.New(t0).Now})
	m.Classify(context.Background(), "timeout")
	restored := m.Restored()
	clock.tick()
	<-gate.asked
	m.Classify(context.Background(), "ECONNRESET")
	// Act: the probe asked before that observation answers reachable.
	gate.release <- reachable.answer
	awaitSaid(t, log, saidNewerObserved)
	// Assert: the newer observation still stands.
	if isClosed(restored) {
		t.Fatal("an answer asked before a newer observation ended it")
	}
	// Act: the next beat reaches the API.
	clock.tick()
	<-gate.asked
	gate.release <- reachable.answer
	awaitSaid(t, log, saidBeatEnded)
	// Assert
	if !isClosed(restored) {
		t.Fatal("the beat that followed did not restore the network")
	}
}

// gatedProbe answers its first ask with FIRST at once, and every later ask
// with what the test releases, signalling each ask first.
type gatedProbe struct {
	mu      sync.Mutex
	asks    int
	first   ProbeAnswer
	asked   chan struct{}
	release chan ProbeAnswer
}

func (g *gatedProbe) Probe(context.Context) (ProbeAnswer, error) {
	g.mu.Lock()
	g.asks++
	n := g.asks
	g.mu.Unlock()
	if n == 1 {
		return g.first, nil
	}
	g.asked <- struct{}{}
	return <-g.release, nil
}

func TestNetworkWords(t *testing.T) {
	tests := []struct {
		text string
		want string
	}{
		{text: "getaddrinfo enotfound api.anthropic.com", want: "ENOTFOUND"},
		{text: "read ECONNRESET", want: "ECONNRESET"},
		{text: "connect EHOSTUNREACH 1.2.3.4:443", want: "EHOSTUNREACH"},
		{text: "API Error: Can't reach the API server", want: "Can't reach the API server"},
		{text: "TypeError: fetch failed", want: "fetch failed"},
		{text: "Connection error.", want: "Connection error"},
		{text: "timeout: the run did not answer within 21s", want: ""},
		{text: "", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.text, func(t *testing.T) {
			// Act
			got := NetworkWords(tc.text)
			// Assert
			if got != tc.want {
				t.Fatalf("NetworkWords(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}

// toldOutages collects every outage a Monitor tells its listener of.
type toldOutages struct {
	mu   sync.Mutex
	told []Outage
}

func (o *toldOutages) listen(outage Outage) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.told = append(o.told, outage)
}

func (o *toldOutages) all() []Outage {
	o.mu.Lock()
	defer o.mu.Unlock()
	return slices.Clone(o.told)
}

func TestOnRestoredTellsTheOutageThatEnded(t *testing.T) {
	at := func(d time.Duration) time.Time { return t0.Add(d) }
	tests := []struct {
		name string
		// act drives the Monitor, moving WALL between edges.
		act  func(m *Monitor, wall *clocktest.Scripted)
		want []Outage
	}{
		{name: "one fault's outage is dated from its opening to its closing", act: func(m *Monitor, wall *clocktest.Scripted) {
			m.FaultOpened("ws-1", networkFault("f-1"))
			wall.Set(at(7 * time.Minute))
			m.FaultClosed("ws-1", "f-1")
		}, want: []Outage{{Since: t0, Restored: at(7 * time.Minute)}}},
		{name: "overlapping faults are one outage, from the first opening to the last closing", act: func(m *Monitor, wall *clocktest.Scripted) {
			m.FaultOpened("ws-1", networkFault("f-1"))
			wall.Set(at(time.Minute))
			m.FaultOpened("ws-2", networkFault("f-2"))
			wall.Set(at(2 * time.Minute))
			m.FaultClosed("ws-1", "f-1")
			wall.Set(at(6 * time.Minute))
			m.FaultClosed("ws-2", "f-2")
		}, want: []Outage{{Since: t0, Restored: at(6 * time.Minute)}}},
		{name: "an outage still standing is told of nothing", act: func(m *Monitor, wall *clocktest.Scripted) {
			m.FaultOpened("ws-1", networkFault("f-1"))
			wall.Set(at(30 * time.Minute))
		}},
		{name: "a later outage is dated afresh", act: func(m *Monitor, wall *clocktest.Scripted) {
			m.FaultOpened("ws-1", networkFault("f-1"))
			wall.Set(at(time.Minute))
			m.FaultClosed("ws-1", "f-1")
			wall.Set(at(10 * time.Minute))
			m.FaultOpened("ws-1", networkFault("f-2"))
			wall.Set(at(12 * time.Minute))
			m.FaultClosed("ws-1", "f-2")
		}, want: []Outage{{Since: t0, Restored: at(time.Minute)}, {Since: at(10 * time.Minute), Restored: at(12 * time.Minute)}}},
		{name: "a fault of another kind dates no outage", act: func(m *Monitor, wall *clocktest.Scripted) {
			m.FaultOpened("ws-1", health.FaultLine{ID: "f-9", Kind: health.KindShimReported})
			wall.Set(at(time.Minute))
			m.FaultClosed("ws-1", "f-9")
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			life, cancel := context.WithCancel(context.Background())
			t.Cleanup(cancel)
			wall := clocktest.New(t0)
			m := New(life, Deps{Log: dlog.NewTestLogger(), Probe: &scriptedProbe{}, After: newBeat().After, Now: wall.Now})
			told := &toldOutages{}
			m.OnRestored(told.listen)
			// Act
			tc.act(m, wall)
			// Assert
			if got := told.all(); !slices.Equal(got, tc.want) {
				t.Fatalf("told %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestTheProbeBeatTellsTheObservedOutageThatEnded(t *testing.T) {
	// Arrange: an outage observed at t0, which the beat ends nine minutes on.
	life, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	log := dlog.NewTestLogger()
	clock := newBeat()
	wall := clocktest.New(t0)
	m := New(life, Deps{Log: log, Probe: &scriptedProbe{answers: []probeStep{unreachable, reachable}}, After: clock.After, Now: wall.Now})
	told := &toldOutages{}
	m.OnRestored(told.listen)
	m.Classify(context.Background(), "timeout")
	wall.Set(t0.Add(9 * time.Minute))
	// Act
	clock.tick()
	awaitSaid(t, log, saidBeatEnded)
	// Assert
	want := []Outage{{Since: t0, Restored: t0.Add(9 * time.Minute)}}
	if got := told.all(); !slices.Equal(got, want) {
		t.Fatalf("told %+v, want %+v", got, want)
	}
	record, _ := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: op, Message: saidBeatEnded})
	if record.Context["down_for"] != (9 * time.Minute).String() {
		t.Fatalf("the restoration record's down_for = %v, want %v: %+v", record.Context["down_for"], 9*time.Minute, record)
	}
}

func TestTheRestorationRecordCarriesTheOutage(t *testing.T) {
	// Arrange
	life, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	log := dlog.NewTestLogger()
	wall := clocktest.New(t0)
	m := New(life, Deps{Log: log, Probe: &scriptedProbe{}, After: newBeat().After, Now: wall.Now})
	m.FaultOpened("ws-1", networkFault("f-1"))
	wall.Set(t0.Add(6 * time.Minute))
	// Act
	m.FaultClosed("ws-1", "f-1")
	// Assert
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "the last network fault closed; the network is restored"})
	if !ok || record.Context["down_for"] != (6*time.Minute).String() || record.Context["down_since"] != t0 {
		t.Fatalf("restoration record = %+v (found %v), want down_for 6m0s since %v", record, ok, t0)
	}
}

func TestNewRefusesAMonitorWithNoWallClock(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("New built a Monitor with no wall clock")
		}
	}()
	// Act
	New(context.Background(), Deps{Log: dlog.NewTestLogger(), Probe: &scriptedProbe{}, After: newBeat().After})
}
