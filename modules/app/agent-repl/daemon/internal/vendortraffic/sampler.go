package vendortraffic

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessionlock"
)

// DefaultEvery is how often the sampler runs a round: discovers the vendor
// processes, asks every subscription for its live sockets' counts, and has
// the sink state what accumulated. It is therefore also the fastest the
// topbar's traffic figure is pushed (owner: "not more than ~every few
// seconds"). A closing socket's final counts do not wait for it; they are
// counted the instant they arrive and stated at the next round.
const DefaultEvery = 5 * time.Second

// readBufferSize is one read's buffer. The kernel batches messages into one
// datagram; 64 KiB holds over a hundred source updates.
const readBufferSize = 64 << 10

// Config is the sampler's collaborators.
type Config struct {
	// ShimPIDs answers the live shims' pids (workspace.Fleet.ShimPIDs).
	ShimPIDs func() []int
	// Processes lists a shim's process group.
	Processes ProcessTable
	// Dial opens a statistics control socket.
	Dial Dialer
	// Sink takes the counted traffic.
	Sink Sink
	// Every is the round cadence; DefaultEvery when zero.
	Every time.Duration
	// After is the round timer; time.After when nil.
	After func(time.Duration) <-chan time.Time
	// Now is the clock; time.Now when nil. The sampler's EPOCH is read from
	// it at construction.
	Now func() time.Time
	// Log is the daemon's run log.
	Log dlog.Logger
}

// procKey names one process across pid reuse.
type procKey struct {
	pid     int
	started time.Time
}

// Sampler measures the vendor processes' traffic.
type Sampler struct {
	cfg Config
	// epoch is when this sampler began. A process that started before it may
	// have been measured by the daemon before this one, so the sockets it
	// already held when this sampler subscribed are BASELINED rather than
	// counted again (ledger.observe).
	epoch time.Time

	// watches are the live subscriptions, by process. refused are processes
	// whose subscription failed: the failure was recorded at ERROR, and the
	// process is not subscribed again, because a second subscription to a
	// process the first already counted could not tell its old sockets from
	// new ones.
	watches map[procKey]*watch
	refused map[procKey]bool
	// retired are subscriptions of departed processes still awaiting their
	// last answer; each leaves the set once its reader has ended.
	retired map[*watch]bool
}

// New builds a sampler, refusing a missing collaborator.
func New(cfg Config) (*Sampler, error) {
	switch {
	case cfg.ShimPIDs == nil:
		return nil, errors.New("vendortraffic: the live shims' pids are required")
	case cfg.Processes == nil:
		return nil, errors.New("vendortraffic: a process table is required")
	case cfg.Dial == nil:
		return nil, errors.New("vendortraffic: a statistics dialer is required")
	case cfg.Sink == nil:
		return nil, errors.New("vendortraffic: a sink is required")
	case cfg.Log == nil:
		return nil, errors.New("vendortraffic: a logger is required")
	case cfg.Every < 0:
		return nil, fmt.Errorf("vendortraffic: a round cadence of %s is not a cadence", cfg.Every)
	}
	if cfg.Every == 0 {
		cfg.Every = DefaultEvery
	}
	if cfg.After == nil {
		cfg.After = time.After
	}
	if cfg.Now == nil {
		cfg.Now = time.Now
	}
	return &Sampler{
		cfg:     cfg,
		epoch:   cfg.Now(),
		watches: map[procKey]*watch{},
		refused: map[procKey]bool{},
		retired: map[*watch]bool{},
	}, nil
}

// Run samples until ctx ends, then closes every subscription.
func (s *Sampler) Run(ctx context.Context) {
	s.cfg.Log.Info("daemon.vendortraffic.run", "measuring the vendor processes' network traffic",
		dlog.Context{"every": s.cfg.Every.String()})
	defer s.closeAll()
	for {
		s.round()
		select {
		case <-ctx.Done():
			return
		case <-s.cfg.After(s.cfg.Every):
		}
	}
}

// round is one sampling round.
func (s *Sampler) round() {
	want := s.vendorProcesses()

	// A live subscription whose reader ended has failed, at ERROR, and its
	// process is refused; a retired one whose reader ended has its last answer.
	for key, w := range s.watches {
		if w.ended() {
			delete(s.watches, key)
			s.refused[key] = true
		}
	}
	for w := range s.retired {
		if w.ended() {
			delete(s.retired, w)
		}
	}
	for _, key := range sortedKeys(s.watches) {
		if _, ok := want[key]; !ok {
			s.retire(key)
		}
	}
	for key := range s.refused {
		if _, ok := want[key]; !ok {
			delete(s.refused, key)
		}
	}
	for _, key := range sortedKeys(want) {
		if _, ok := s.watches[key]; ok {
			s.watches[key].poll()
			continue
		}
		if s.refused[key] {
			continue
		}
		s.subscribe(key)
	}
	s.cfg.Sink.FlushTraffic()
}

// vendorProcesses answers every live shim and its direct children, less the
// lock holders.
func (s *Sampler) vendorProcesses() map[procKey]bool {
	want := map[procKey]bool{}
	for _, shim := range s.cfg.ShimPIDs() {
		members, err := s.cfg.Processes.Group(shim)
		if err != nil {
			s.cfg.Log.Error("daemon.vendortraffic.discover", "the shim's process group could not be listed; its processes are not measured this round",
				dlog.Context{"shim_pid": shim, "cause": err.Error()})
			continue
		}
		for _, p := range members {
			if p.Name == sessionlock.HolderBinary {
				continue
			}
			if p.PID == shim || p.PPID == shim {
				want[procKey{pid: p.PID, started: p.Started}] = true
			}
		}
	}
	return want
}

// subscribe opens a process's subscription. A failure is recorded at ERROR and
// the process is refused for its lifetime.
func (s *Sampler) subscribe(key procKey) {
	fields := dlog.Context{"pid": key.pid, "process_started": key.started.UTC().Format(time.RFC3339Nano)}
	conn, err := s.cfg.Dial()
	if err != nil {
		s.refused[key] = true
		fields["cause"] = err.Error()
		s.cfg.Log.Error("daemon.vendortraffic.subscribe", "the network statistics control could not be opened; this process's traffic is not measured", fields)
		return
	}
	w := newWatch(key, conn, key.started.Before(s.epoch), s.cfg.Sink, s.cfg.Log.With(dlog.Context{"pid": key.pid}))
	if err := w.start(); err != nil {
		s.refused[key] = true
		fields["cause"] = err.Error()
		s.cfg.Log.Error("daemon.vendortraffic.subscribe", "the subscription could not be sent; this process's traffic is not measured", fields)
		return
	}
	s.watches[key] = w
	fields["baselined"] = w.baseline
	s.cfg.Log.Info("daemon.vendortraffic.subscribe", "measuring a vendor process's traffic", fields)
	go w.read()
}

// retire asks a departed process's subscription for its last answer; the
// reader closes it once that answer has arrived.
func (s *Sampler) retire(key procKey) {
	w := s.watches[key]
	delete(s.watches, key)
	s.retired[w] = true
	s.cfg.Log.Info("daemon.vendortraffic.retire", "a vendor process is gone; its subscription is closing", dlog.Context{
		"pid": key.pid, "live_sources": w.liveSources(),
	})
	w.finish()
}

// closeAll ends every subscription, as the sampler stops.
func (s *Sampler) closeAll() {
	for _, key := range sortedKeys(s.watches) {
		s.watches[key].close()
	}
	for w := range s.retired {
		w.close()
	}
	s.watches = map[procKey]*watch{}
	s.retired = map[*watch]bool{}
	s.cfg.Log.Info("daemon.vendortraffic.run", "stopped measuring the vendor processes' network traffic", nil)
}

// sortedKeys orders process keys by pid, so a round's work is deterministic.
func sortedKeys[V any](m map[procKey]V) []procKey {
	keys := make([]procKey, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	slices.SortFunc(keys, func(a, b procKey) int {
		if a.pid != b.pid {
			return a.pid - b.pid
		}
		return a.started.Compare(b.started)
	})
	return keys
}

// The request contexts a subscription uses. The two subscribe requests take
// one per provider; every poll after the priming one takes the next.
const (
	ctxPrime     uint64 = 100
	ctxFirstPoll uint64 = 101
)

// watch is one process's subscription.
type watch struct {
	key      procKey
	conn     Conn
	sink     Sink
	log      dlog.Logger
	baseline bool

	mu sync.Mutex
	// ledger accounts the process's sockets.
	ledger *ledger
	// priming holds until the priming poll's SUCCESS: every update before it
	// is a socket the process already held when the subscription began.
	priming bool
	// nextCtx is the next poll's context.
	nextCtx uint64
	// finalCtx is the retiring poll's context, zero until retired.
	finalCtx uint64
	// closing is set once this side closes the socket, so the reader's
	// resulting error is the close it is, not a failure.
	closing bool
	// done closes when the reader ends.
	done chan struct{}
}

// newWatch builds a subscription over an open socket.
func newWatch(key procKey, conn Conn, baseline bool, sink Sink, log dlog.Logger) *watch {
	return &watch{
		key:      key,
		conn:     conn,
		sink:     sink,
		log:      log,
		baseline: baseline,
		ledger:   newLedger(),
		priming:  true,
		nextCtx:  ctxFirstPoll,
		done:     make(chan struct{}),
	}
}

// start sends the subscription and the priming poll, closing the socket when
// any of them cannot be sent.
func (w *watch) start() error {
	for _, provider := range subscribedProviders {
		if _, err := w.conn.Write(encodeAddAllSrcs(uint64(provider), provider, int32(w.key.pid))); err != nil {
			w.close()
			return fmt.Errorf("subscribe provider %d: %w", provider, err)
		}
	}
	if _, err := w.conn.Write(encodeGetUpdate(ctxPrime)); err != nil {
		w.close()
		return fmt.Errorf("send the priming poll: %w", err)
	}
	return nil
}

// poll asks for the live sockets' counts. A poll that cannot be sent ends the
// subscription, at ERROR.
func (w *watch) poll() {
	w.mu.Lock()
	ctx := w.nextCtx
	w.nextCtx++
	w.mu.Unlock()
	if _, err := w.conn.Write(encodeGetUpdate(ctx)); err != nil {
		w.log.Error("daemon.vendortraffic.poll", "the poll could not be sent; this process's traffic is no longer measured",
			dlog.Context{"cause": err.Error()})
		w.close()
	}
}

// finish sends the retiring poll. Its SUCCESS is the subscription's last
// message: every closing socket of the gone process was delivered before it.
func (w *watch) finish() {
	w.mu.Lock()
	ctx := w.nextCtx
	w.nextCtx++
	w.finalCtx = ctx
	w.mu.Unlock()
	if _, err := w.conn.Write(encodeGetUpdate(ctx)); err != nil {
		w.log.Error("daemon.vendortraffic.retire", "the retiring poll could not be sent; any socket the process closed since the last poll is not counted",
			dlog.Context{"cause": err.Error()})
		w.close()
	}
}

// close closes the socket from this side, once: the reader's own close and
// the sampler's stop can both reach a retired subscription.
func (w *watch) close() {
	w.mu.Lock()
	already := w.closing
	w.closing = true
	w.mu.Unlock()
	if already {
		return
	}
	if err := w.conn.Close(); err != nil {
		w.log.Error("daemon.vendortraffic.close", "the statistics control socket did not close cleanly", dlog.Context{"cause": err.Error()})
	}
}

// ended reports whether the reader has ended.
func (w *watch) ended() bool {
	select {
	case <-w.done:
		return true
	default:
		return false
	}
}

// liveSources counts the sockets the ledger is following.
func (w *watch) liveSources() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.ledger.sources()
}

// read is the subscription's reader: it applies every datagram until the
// retiring poll's answer, or until the socket ends.
func (w *watch) read() {
	defer close(w.done)
	buf := make([]byte, readBufferSize)
	for {
		n, err := w.conn.Read(buf)
		if err != nil {
			w.mu.Lock()
			closing := w.closing
			w.mu.Unlock()
			if closing {
				w.log.Debug("daemon.vendortraffic.read", "the subscription's reader ended on its own close", nil)
				return
			}
			w.log.Error("daemon.vendortraffic.read", "the statistics control socket failed; this process's traffic is no longer measured",
				dlog.Context{"cause": err.Error()})
			w.close()
			return
		}
		if w.handle(buf[:n]) {
			w.log.Debug("daemon.vendortraffic.read", "the retired process's last answer arrived", nil)
			w.close()
			return
		}
	}
}

// handle applies one datagram and reports whether it carried the retiring
// poll's answer.
func (w *watch) handle(datagram []byte) bool {
	events, err := decodeDatagram(datagram)
	if err != nil {
		w.log.Error("daemon.vendortraffic.read", "a malformed datagram was dropped; the counts it carried are lost",
			dlog.Context{"bytes": len(datagram), "cause": err.Error()})
		return false
	}
	var counted Counts
	finished := false
	w.mu.Lock()
	for _, ev := range events {
		switch ev.kind {
		case eventSuccess:
			if ev.ctx == ctxPrime {
				w.priming = false
			}
			if w.finalCtx != 0 && ev.ctx == w.finalCtx {
				finished = true
			}
		case eventFailure:
			w.log.Error("daemon.vendortraffic.read", "the kernel refused a request; this process's traffic may be undercounted",
				dlog.Context{"request": requestName(ev.ctx), "errno": ev.errno})
		case eventUpdate:
			delta, err := w.ledger.observe(ev.srcRef, ev.counts, ev.closing, w.baseline && w.priming)
			if err != nil {
				w.log.Error("daemon.vendortraffic.read", "a socket's counter shrank; nothing was counted for it and it was re-anchored",
					dlog.Context{"source": ev.srcRef, "cause": err.Error()})
				continue
			}
			counted = counted.Plus(delta)
		case eventRemoved:
			w.ledger.remove(ev.srcRef)
		case eventIgnored:
		}
	}
	w.mu.Unlock()
	if !counted.IsZero() {
		w.sink.AddTraffic(counted)
	}
	return finished
}

// requestName names the request a context belongs to, for the record.
func requestName(ctx uint64) string {
	switch {
	case ctx == uint64(providerTCPKernel):
		return "subscribe tcp"
	case ctx == uint64(providerUDPKernel):
		return "subscribe udp"
	case ctx == ctxPrime:
		return "priming poll"
	default:
		return fmt.Sprintf("poll %d", ctx)
	}
}
