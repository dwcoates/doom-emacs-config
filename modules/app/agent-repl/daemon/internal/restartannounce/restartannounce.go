// Package restartannounce carries the daemon's INTENTIONAL-RESTART
// announcement to every connected client, before the daemon closes its
// listeners.
//
// THE DEFECT THIS ENDS: a deploy bounce (store + sidecar + daemon restart +
// shim roll) is an event the daemon KNOWS about and the clients do not. Every
// client learned about it the only way it could — by its socket dying — and a
// dead socket is indistinguishable from a daemon that crashed, so each bounce
// painted the blue severed banner in the webapp and a DAEMON LINK DEGRADED
// segment in Emacs for an outage that was wanted, expected, and bounded.
//
// The announcement is that missing fact: "this is deliberate, and it should be
// over within N seconds". It is what lets a client open a BOUNDED quiet window
// instead of alarming — bounded because the announcement is a prediction, not
// a promise, and a daemon that never comes back must still be reported. The
// bound belongs to the client; this package's job is to state the prediction
// once, to everyone, before the link goes away.
//
// TRANSPORT-AGNOSTIC BY CONSTRUCTION. An announcement reaches gui_stream
// clients and the Emacs UDS host over different carriers, and the daemon's own
// shutdown path must not know about either. Callers register Sinks; this
// package validates the announcement, fans it out to every sink, and reports
// what failed. It NEVER decides that a failed delivery is unimportant: a
// bounce whose announcement did not land is a bounce the user is about to see
// alarms for, and the shutdown log is the only place that can say so.
package restartannounce

import (
	"errors"
	"fmt"
	"time"
)

// DefaultExpectedOutage is the hint a caller sends when it has no better
// figure: the wall-clock a full backend bounce is expected to take, measured
// from the announcement to a client's successful reconnect-and-resync.
//
// It is deliberately generous relative to an observed bounce. Under-promising
// costs nothing (a client leaves the quiet window early, on its own successful
// reconnect), while over-promising is exactly the failure the bound exists to
// prevent: a client that stays quiet past the point where the outage stopped
// being expected.
const DefaultExpectedOutage = 60 * time.Second

// MaxExpectedOutage caps any hint a caller may state. A client is entitled to
// clamp further, but nothing may ask a client to stay quiet for an unbounded
// stretch: "suppress your alarms indefinitely" is not a representable request.
const MaxExpectedOutage = 5 * time.Minute

// ErrNoSinks reports that an announcement was composed and had nowhere to go.
// This is a LOUD result, never a quiet success: the clients that were supposed
// to hear it are about to lose their sockets, and every one of them will alarm.
var ErrNoSinks = errors.New("restartannounce: no announcement sink is registered")

// Announcement is one intentional-restart notice.
type Announcement struct {
	// Why the daemon is going down ("deploy-all rebuilt the daemon", "SIGTERM").
	// Display-grade and never parsed; empty is refused, because an
	// announcement that cannot say what it is about is indistinguishable from
	// a stray one.
	Cause string
	// Whether the restart also SIGTERMs the session shims (ShutdownCmd
	// semantics). A client renders a longer settle when the shims roll too.
	StopShims bool
	// How long the outage is expected to last. Must be positive and no more
	// than MaxExpectedOutage.
	ExpectedOutage time.Duration
	// When the announcement was minted (epoch ms), so a client that receives
	// it late shortens its own window by the delay rather than restarting the
	// clock.
	AtMs int64
}

// Validate reports whether this announcement is one a client could act on.
func (a Announcement) Validate() error {
	if a.Cause == "" {
		return errors.New("restartannounce: announcement needs a cause")
	}
	if a.ExpectedOutage <= 0 {
		return fmt.Errorf("restartannounce: expected outage must be positive, got %s", a.ExpectedOutage)
	}
	if a.ExpectedOutage > MaxExpectedOutage {
		return fmt.Errorf("restartannounce: expected outage %s exceeds the %s cap", a.ExpectedOutage, MaxExpectedOutage)
	}
	if a.AtMs <= 0 {
		return fmt.Errorf("restartannounce: announcement needs a mint time, got %d", a.AtMs)
	}
	return nil
}

// Sink is one carrier an announcement is delivered over (the gui_stream
// broadcast, the Emacs UDS host). A sink that cannot deliver returns an error;
// it never reports success it did not achieve.
type Sink interface {
	// Name identifies the carrier in the shutdown log, so a partial delivery
	// says WHICH clients were told and which were not.
	Name() string
	AnnounceRestart(Announcement) error
}

// SinkFunc adapts a plain function to Sink.
type SinkFunc struct {
	Label   string
	Deliver func(Announcement) error
}

func (s SinkFunc) Name() string { return s.Label }

func (s SinkFunc) AnnounceRestart(a Announcement) error {
	if s.Deliver == nil {
		return fmt.Errorf("restartannounce: sink %q has no delivery function", s.Label)
	}
	return s.Deliver(a)
}

// Announcer fans one announcement out to every registered sink.
type Announcer struct {
	sinks []Sink
	logf  func(string, ...any)
}

// New builds an Announcer. logf is required: this package's whole value on the
// failure path is the record it writes, so an Announcer that cannot speak is
// refused rather than silently constructed.
func New(logf func(string, ...any), sinks ...Sink) (*Announcer, error) {
	if logf == nil {
		return nil, errors.New("restartannounce: an announcer needs a logger")
	}
	for i, s := range sinks {
		if s == nil {
			return nil, fmt.Errorf("restartannounce: sink %d is nil", i)
		}
	}
	return &Announcer{sinks: append([]Sink(nil), sinks...), logf: logf}, nil
}

// Announce validates and delivers the announcement to EVERY sink, returning
// the joined failures.
//
// Every sink is attempted even after one fails: the sinks are independent
// carriers to different clients, so abandoning the rest because the first one
// broke would turn one client's missed announcement into everyone's.
//
// A zero-sink announcer returns ErrNoSinks. It is not a no-op success: see the
// package comment.
func (a *Announcer) Announce(ann Announcement) error {
	if err := ann.Validate(); err != nil {
		a.logf("restart-announce: REFUSING malformed announcement: %v", err)
		return err
	}
	if len(a.sinks) == 0 {
		a.logf("restart-announce: cause=%q expected_outage=%s stop_shims=%v NOT ANNOUNCED — %v; every connected client will see this deliberate bounce as an unexplained disconnect",
			ann.Cause, ann.ExpectedOutage, ann.StopShims, ErrNoSinks)
		return ErrNoSinks
	}
	var failures []error
	delivered := 0
	for _, s := range a.sinks {
		if err := s.AnnounceRestart(ann); err != nil {
			a.logf("restart-announce: sink=%s FAILED: %v; its clients will see this bounce as an unexplained disconnect", s.Name(), err)
			failures = append(failures, fmt.Errorf("sink %s: %w", s.Name(), err))
			continue
		}
		delivered++
	}
	a.logf("restart-announce: cause=%q expected_outage=%s stop_shims=%v delivered=%d/%d",
		ann.Cause, ann.ExpectedOutage, ann.StopShims, delivered, len(a.sinks))
	return errors.Join(failures...)
}

// Now is the clock an announcement's mint time comes from, injectable so tests
// state the moment instead of reading the wall clock.
type Now func() time.Time

// Compose builds a validated announcement from the shutdown decision. It is
// the one place the outage hint is chosen, so the daemon's two shutdown
// requesters (SIGTERM and the shutdown command) cannot disagree about it.
func Compose(now Now, cause string, stopShims bool, expected time.Duration) (Announcement, error) {
	if now == nil {
		return Announcement{}, errors.New("restartannounce: compose needs a clock")
	}
	if expected <= 0 {
		expected = DefaultExpectedOutage
	}
	ann := Announcement{
		Cause:          cause,
		StopShims:      stopShims,
		ExpectedOutage: expected,
		AtMs:           now().UnixMilli(),
	}
	if err := ann.Validate(); err != nil {
		return Announcement{}, err
	}
	return ann, nil
}
