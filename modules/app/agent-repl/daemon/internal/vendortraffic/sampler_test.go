package vendortraffic

import (
	"bytes"
	"context"
	"errors"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

// epoch is the fixed instant every test sampler is built at.
var epoch = time.Date(2026, 10, 6, 12, 0, 0, 0, time.UTC)

// fakeConn is one scripted statistics control socket.
type fakeConn struct {
	mu       sync.Mutex
	writes   [][]byte
	writeErr error
	closed   bool
	closeErr error

	reads  chan []byte
	failed chan error
	done   chan struct{}
}

func newFakeConn() *fakeConn {
	return &fakeConn{reads: make(chan []byte, 16), failed: make(chan error, 1), done: make(chan struct{})}
}

func (c *fakeConn) Read(b []byte) (int, error) {
	select {
	case d := <-c.reads:
		return copy(b, d), nil
	case err := <-c.failed:
		return 0, err
	case <-c.done:
		return 0, os.ErrClosed
	}
}

func (c *fakeConn) Write(b []byte) (int, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.writeErr != nil {
		return 0, c.writeErr
	}
	c.writes = append(c.writes, append([]byte(nil), b...))
	return len(b), nil
}

func (c *fakeConn) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if !c.closed {
		c.closed = true
		close(c.done)
	}
	return c.closeErr
}

func (c *fakeConn) written() [][]byte {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([][]byte(nil), c.writes...)
}

func (c *fakeConn) isClosed() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.closed
}

// fakeProcs is a scripted process table.
type fakeProcs struct {
	groups map[int][]Proc
	err    map[int]error
}

func (p *fakeProcs) Group(pgid int) ([]Proc, error) {
	if err := p.err[pgid]; err != nil {
		return nil, err
	}
	return p.groups[pgid], nil
}

// fakeSink records what the sampler counts.
type fakeSink struct {
	mu      sync.Mutex
	total   Counts
	flushes int
	added   chan Counts
}

func newFakeSink() *fakeSink { return &fakeSink{added: make(chan Counts, 64)} }

func (s *fakeSink) AddTraffic(c Counts) {
	s.mu.Lock()
	s.total = s.total.Plus(c)
	s.mu.Unlock()
	s.added <- c
}

func (s *fakeSink) FlushTraffic() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.flushes++
}

func (s *fakeSink) totals() (Counts, int) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.total, s.flushes
}

// fixture is a sampler over fakes.
type fixture struct {
	sampler *Sampler
	procs   *fakeProcs
	sink    *fakeSink
	log     *dlog.TestLogger
	shims   []int
	conns   []*fakeConn
	dialErr error
	// nextWriteErr is handed to the next dialed conn.
	nextWriteErr error
}

func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{procs: &fakeProcs{groups: map[int][]Proc{}, err: map[int]error{}}, sink: newFakeSink(), log: dlog.NewTestLogger()}
	s, err := New(Config{
		ShimPIDs:  func() []int { return f.shims },
		Processes: f.procs,
		Dial: func() (Conn, error) {
			if f.dialErr != nil {
				return nil, f.dialErr
			}
			c := newFakeConn()
			c.writeErr = f.nextWriteErr
			f.conns = append(f.conns, c)
			return c, nil
		},
		Sink: f.sink,
		Now:  func() time.Time { return epoch },
		Log:  f.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.sampler = s
	t.Cleanup(s.closeAll)
	return f
}

// shim stands a shim with the given members in its process group.
func (f *fixture) shim(pid int, members ...Proc) {
	f.shims = append(f.shims, pid)
	f.procs.groups[pid] = append([]Proc{{PID: pid, PPID: 1, Started: epoch.Add(time.Second)}}, members...)
}

// child is a process of the shim's group started after the epoch.
func child(pid, ppid int) Proc {
	return Proc{PID: pid, PPID: ppid, Started: epoch.Add(2 * time.Second)}
}

// subscribedPIDs answers the pid every conn subscribed, in dial order.
func (f *fixture) subscribedPIDs() []int {
	var out []int
	for _, c := range f.conns {
		w := c.written()
		if len(w) == 0 {
			continue
		}
		out = append(out, int(int32(uint32(w[0][36])|uint32(w[0][37])<<8|uint32(w[0][38])<<16|uint32(w[0][39])<<24)))
	}
	return out
}

// errorRecords answers the ERROR records under operation.
func errorRecords(log *dlog.TestLogger, operation string) []dlog.Record {
	var out []dlog.Record
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// awaitEnded waits for a watch's reader to end, bounded.
func awaitEnded(t *testing.T, w *watch) {
	t.Helper()
	select {
	case <-w.done:
	case <-time.After(2 * time.Second):
		t.Fatal("the subscription's reader did not end")
	}
}

// awaitAdded waits for the sink to take one addition, bounded.
func awaitAdded(t *testing.T, s *fakeSink) Counts {
	t.Helper()
	select {
	case c := <-s.added:
		return c
	case <-time.After(2 * time.Second):
		t.Fatal("the sink took nothing")
		return Counts{}
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	full := func() Config {
		return Config{
			ShimPIDs:  func() []int { return nil },
			Processes: &fakeProcs{},
			Dial:      func() (Conn, error) { return newFakeConn(), nil },
			Sink:      newFakeSink(),
			Log:       dlog.NewTestLogger(),
		}
	}
	cases := []struct {
		name  string
		strip func(*Config)
		want  string
	}{
		{"shim pids", func(c *Config) { c.ShimPIDs = nil }, "pids are required"},
		{"process table", func(c *Config) { c.Processes = nil }, "process table is required"},
		{"dialer", func(c *Config) { c.Dial = nil }, "dialer is required"},
		{"sink", func(c *Config) { c.Sink = nil }, "sink is required"},
		{"logger", func(c *Config) { c.Log = nil }, "logger is required"},
		{"negative cadence", func(c *Config) { c.Every = -time.Second }, "not a cadence"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg := full()
			tc.strip(&cfg)

			// Act.
			_, err := New(cfg)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("New error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestRoundSubscribesTheShimAndItsDirectChildrenOnly(t *testing.T) {
	// Arrange: a shim, its vendor child, and a tool the child ran.
	f := newFixture(t)
	f.shim(500, child(501, 500), child(502, 501))

	// Act.
	f.sampler.round()

	// Assert.
	got := f.subscribedPIDs()
	if len(got) != 2 || got[0] != 500 || got[1] != 501 {
		t.Fatalf("subscribed pids = %v, want [500 501]", got)
	}
}

func TestRoundNeverSubscribesALockHolder(t *testing.T) {
	// Arrange: a shim, its vendor child, and the kernel-lock holder it spawned.
	f := newFixture(t)
	holder := child(502, 500)
	holder.Name = "shim-lock"
	f.shim(500, child(501, 500), holder)

	// Act.
	f.sampler.round()

	// Assert.
	got := f.subscribedPIDs()
	if len(got) != 2 || got[0] != 500 || got[1] != 501 {
		t.Fatalf("subscribed pids = %v, want [500 501]", got)
	}
}

func TestSubscribeSendsBothProvidersAndThePrimingPoll(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)

	// Act.
	f.sampler.round()

	// Assert.
	want := [][]byte{encodeAddAllSrcs(2, providerTCPKernel, 500), encodeAddAllSrcs(4, providerUDPKernel, 500), encodeGetUpdate(ctxPrime)}
	got := f.conns[0].written()
	if len(got) != len(want) {
		t.Fatalf("wrote %d messages, want %d", len(got), len(want))
	}
	for i := range want {
		if !bytes.Equal(got[i], want[i]) {
			t.Fatalf("message %d = %x, want %x", i, got[i], want[i])
		}
	}
}

func TestALaterRoundPollsALiveSubscription(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)
	f.sampler.round()

	// Act.
	f.sampler.round()

	// Assert.
	got := f.conns[0].written()
	if last := got[len(got)-1]; !bytes.Equal(last, encodeGetUpdate(ctxFirstPoll)) {
		t.Fatalf("the second round wrote %x, want the first poll", last)
	}
	if len(f.conns) != 1 {
		t.Fatalf("dialed %d sockets, want the one subscription reused", len(f.conns))
	}
}

func TestEveryRoundFlushesTheSinkOnce(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)

	// Act.
	f.sampler.round()
	f.sampler.round()

	// Assert.
	if _, flushes := f.sink.totals(); flushes != 2 {
		t.Fatalf("flushes = %d, want one per round", flushes)
	}
}

func TestADepartedProcessIsRetiredWithAFinalPoll(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500, child(501, 500))
	f.sampler.round()
	f.procs.groups[500] = f.procs.groups[500][:1] // the vendor child exited

	// Act.
	f.sampler.round()

	// Assert.
	got := f.conns[1].written()
	if last := got[len(got)-1]; !bytes.Equal(last, encodeGetUpdate(ctxFirstPoll)) {
		t.Fatalf("the departed process's socket last got %x, want its retiring poll", last)
	}
	if len(f.sampler.watches) != 1 || len(f.sampler.retired) != 1 {
		t.Fatalf("watches=%d retired=%d, want 1 live and 1 retiring", len(f.sampler.watches), len(f.sampler.retired))
	}
}

func TestARetiredSubscriptionClosesOnItsFinalAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500, child(501, 500))
	f.sampler.round()
	f.procs.groups[500] = f.procs.groups[500][:1]
	f.sampler.round()
	var retired *watch
	for w := range f.sampler.retired {
		retired = w
	}

	// Act.
	f.conns[1].reads <- message(ctxFirstPoll, msgSuccess, headerSize, 0)
	awaitEnded(t, retired)
	f.sampler.round()

	// Assert.
	if !f.conns[1].isClosed() {
		t.Fatal("the retired subscription's socket is still open after its last answer")
	}
	if len(f.sampler.retired) != 0 {
		t.Fatalf("retired = %d, want the ended subscription forgotten", len(f.sampler.retired))
	}
}

func TestAProcessRestartCountsEveryByteOnce(t *testing.T) {
	// Arrange: the vendor child 501 moves bytes, closes its socket and exits;
	// its replacement 502 opens a socket whose kernel reference happens to
	// reuse 501's number.
	f := newFixture(t)
	f.shim(500, child(501, 500))
	f.sampler.round()
	first := f.conns[1]
	first.reads <- datagram(updateMessage(1, 100, 10, false), message(ctxPrime, msgSuccess, headerSize, 0))
	awaitAdded(t, f.sink)
	first.reads <- updateMessage(1, 150, 12, true)
	awaitAdded(t, f.sink)
	f.procs.groups[500] = []Proc{f.procs.groups[500][0], child(502, 500)}

	// Act.
	f.sampler.round()
	second := f.conns[2]
	second.reads <- datagram(updateMessage(1, 30, 3, false), message(ctxPrime, msgSuccess, headerSize, 0))
	awaitAdded(t, f.sink)

	// Assert.
	if total, _ := f.sink.totals(); total != (Counts{Received: 180, Sent: 15}) {
		t.Fatalf("total = %+v, want 150+30 received and 12+3 sent", total)
	}
}

func TestAProcessThatPredatesTheEpochBaselinesItsOpenSockets(t *testing.T) {
	// Arrange: a vendor child that was running before this daemon began.
	f := newFixture(t)
	f.shim(500, Proc{PID: 501, PPID: 500, Started: epoch.Add(-time.Hour)})
	f.sampler.round()
	conn := f.conns[1]

	// Act.
	conn.reads <- datagram(updateMessage(1, 9000, 900, false), message(ctxPrime, msgSuccess, headerSize, 0))
	conn.reads <- updateMessage(1, 9500, 950, false)
	added := awaitAdded(t, f.sink)

	// Assert.
	if added != (Counts{Received: 500, Sent: 50}) {
		t.Fatalf("counted %+v, want only what moved after the subscription", added)
	}
}

func TestAProcessBornAfterTheEpochCountsItsOpenSocketsInFull(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500, child(501, 500))
	f.sampler.round()

	// Act.
	f.conns[1].reads <- datagram(updateMessage(1, 9000, 900, false), message(ctxPrime, msgSuccess, headerSize, 0))
	added := awaitAdded(t, f.sink)

	// Assert.
	if added != (Counts{Received: 9000, Sent: 900}) {
		t.Fatalf("counted %+v, want the socket's whole history", added)
	}
}

func TestASocketOpenedAfterPrimingCountsInFullEvenForAPredatingProcess(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500, Proc{PID: 501, PPID: 500, Started: epoch.Add(-time.Hour)})
	f.sampler.round()
	conn := f.conns[1]
	conn.reads <- message(ctxPrime, msgSuccess, headerSize, 0)

	// Act.
	conn.reads <- updateMessage(2, 700, 70, true)
	added := awaitAdded(t, f.sink)

	// Assert.
	if added != (Counts{Received: 700, Sent: 70}) {
		t.Fatalf("counted %+v, want the new socket's whole history", added)
	}
}

func TestAKernelRefusalIsRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	w := newWatch(procKey{pid: 501}, newFakeConn(), false, f.sink, f.log)

	// Act.
	w.handle(errorMessage(uint64(providerUDPKernel), 22))

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.read")
	if len(records) != 1 || records[0].Context["request"] != "subscribe udp" || records[0].Context["errno"] != uint32(22) {
		t.Fatalf("records = %+v, want one ERROR naming the udp subscription and errno 22", records)
	}
}

func TestAMalformedDatagramIsRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	w := newWatch(procKey{pid: 501}, newFakeConn(), false, f.sink, f.log)

	// Act.
	w.handle([]byte{1, 2, 3})

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.read")
	if len(records) != 1 || records[0].Context["bytes"] != 3 {
		t.Fatalf("records = %+v, want one ERROR naming the dropped datagram", records)
	}
}

func TestAShrinkingCounterIsRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	w := newWatch(procKey{pid: 501}, newFakeConn(), false, f.sink, f.log)
	w.handle(updateMessage(1, 100, 10, false))

	// Act.
	w.handle(updateMessage(1, 50, 10, false))

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.read")
	if len(records) != 1 || records[0].Context["source"] != uint64(1) {
		t.Fatalf("records = %+v, want one ERROR naming source 1", records)
	}
}

func TestADialFailureRefusesTheProcessAtError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)
	f.dialErr = errors.New("no control")

	// Act.
	f.sampler.round()
	f.sampler.round()

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.subscribe")
	if len(records) != 1 || records[0].Context["pid"] != 500 || records[0].Context["cause"] != "no control" {
		t.Fatalf("records = %+v, want ONE ERROR naming pid 500 and the cause", records)
	}
}

func TestASubscriptionThatCannotBeSentRefusesTheProcessAtError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)
	f.nextWriteErr = errors.New("broken pipe")

	// Act.
	f.sampler.round()

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.subscribe")
	if len(records) != 1 || !strings.Contains(records[0].Context["cause"].(string), "broken pipe") {
		t.Fatalf("records = %+v, want one ERROR naming the write failure", records)
	}
	if !f.conns[0].isClosed() || !f.sampler.refused[procKey{pid: 500, started: epoch.Add(time.Second)}] {
		t.Fatal("the failed subscription's socket was left open or its process not refused")
	}
}

func TestAFailedReaderIsRecordedAndNeverResubscribed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)
	f.sampler.round()
	w := f.sampler.watches[procKey{pid: 500, started: epoch.Add(time.Second)}]

	// Act.
	f.conns[0].failed <- errors.New("connection reset")
	awaitEnded(t, w)
	f.sampler.round()

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.read")
	if len(records) != 1 || records[0].Context["cause"] != "connection reset" {
		t.Fatalf("records = %+v, want one ERROR naming the failure", records)
	}
	if len(f.conns) != 1 {
		t.Fatalf("dialed %d sockets, want the failed process never subscribed again", len(f.conns))
	}
}

func TestAGroupListingFailureIsRecordedAndOtherShimsAreStillMeasured(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)
	f.shim(600)
	f.procs.err[500] = errors.New("sysctl failed")

	// Act.
	f.sampler.round()

	// Assert.
	records := errorRecords(f.log, "daemon.vendortraffic.discover")
	if len(records) != 1 || records[0].Context["shim_pid"] != 500 {
		t.Fatalf("records = %+v, want one ERROR naming shim 500", records)
	}
	if got := f.subscribedPIDs(); len(got) != 1 || got[0] != 600 {
		t.Fatalf("subscribed = %v, want the other shim", got)
	}
}

func TestRunClosesEverySubscriptionWhenItStops(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.shim(500)
	ctx, cancel := context.WithCancel(context.Background())
	rounds := make(chan time.Duration, 1)
	f.sampler.cfg.After = func(d time.Duration) <-chan time.Time {
		rounds <- d
		return make(chan time.Time)
	}
	stopped := make(chan struct{})

	// Act.
	go func() {
		f.sampler.Run(ctx)
		close(stopped)
	}()
	if every := <-rounds; every != DefaultEvery {
		t.Fatalf("the round waited %s, want %s", every, DefaultEvery)
	}
	cancel()
	<-stopped

	// Assert.
	if !f.conns[0].isClosed() {
		t.Fatal("a subscription's socket outlived the sampler")
	}
}
