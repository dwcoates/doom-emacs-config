package persistentwifi

import (
	"context"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opFailover = "daemon.persistentwifi.failover"

// tick is one beat of Run: read the standing, then hold the hotspot failover
// to it. Both run under the one lock, so a beat never acts on a reading an
// Update changed underneath it.
func (c *Controller) tick(ctx context.Context) {
	c.mu.Lock()
	defer c.mu.Unlock()
	rd := c.readLocked(ctx)
	if ctx.Err() != nil {
		// An abandoned read says nothing about either fact (readLocked).
		return
	}
	c.failoverLocked(ctx, rd)
}

// failoverRetryIn answers how long until the hotspot failover's next join is
// due, and whether one is waiting at all: only after a failed join, while the
// last standing still wants the failover and it has not given up.
func (c *Controller) failoverRetryIn() (time.Duration, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	s := c.failover
	if s.lost.IsZero() || s.gaveUp || s.next.IsZero() || c.last == nil || !failoverWants(c.last) {
		return 0, false
	}
	return max(s.next.Sub(c.clock.Now()), 0), true
}

// failoverLocked is THE HOTSPOT FAILOVER: while the mode is on, a Wi-Fi
// interface that is joined to nothing is joined to the configured hotspot.
// The first read that finds the mode on and no network joins at once, so the
// hotspot is rejoined as soon as the connection drops. A join
// that does not take is an outcome, never an error: it is tried again on the
// retry schedule (failoverBackoff), which backs off the longer Wi-Fi stays lost
// and gives up after three hours. Each distinct cause of a failover that cannot
// take is recorded once (failoverCause), and the failover's end is recorded
// when there was one. Caller holds mu.
func (c *Controller) failoverLocked(ctx context.Context, rd reading) {
	if !failoverWants(rd.state) {
		c.standDownFailoverLocked(rd)
		return
	}
	if c.cfg.Hotspot == "" {
		c.recordFailoverCause(errNoHotspot().Error(), func(cause string) {
			c.log.Error(opFailover, "Wi-Fi was lost with persistent wifi on, and no hotspot is configured to join",
				dlog.Context{"env": EnvHotspot, "cause": cause})
		})
		return
	}

	now := c.clock.Now()
	switch c.failover.due(now) {
	case failoverWaiting, failoverGivenUp:
		return
	case failoverExhausted:
		c.failover.gaveUp = true
		c.log.Warn(opFailover, "the hotspot failover gave up: Wi-Fi stayed lost and the hotspot could not be joined for three hours; "+
			"it resumes once a network is joined or the mode is turned off and on",
			dlog.Context{"hotspot": c.cfg.Hotspot, "lost_for": now.Sub(c.failover.lost).String(),
				"attempts": c.failover.attempts, "cause": c.failoverCause})
		return
	}

	c.failover.attempts++
	c.log.Info(opFailover, "Wi-Fi was lost with persistent wifi on; joining the hotspot", dlog.Context{
		"hotspot": c.cfg.Hotspot, "device": rd.device, "attempt": c.failover.attempts,
		"lost_for": now.Sub(c.failover.lost).String(),
	})
	outcome := c.joinHotspot(ctx, rd)
	if joined := outcome.GetJoined(); joined != nil {
		after := c.readLocked(ctx)
		c.log.Info(opFailover, "the hotspot was joined after Wi-Fi was lost", dlog.Context{
			"hotspot": joined.GetNetworkName(), "wifi": wifiArm(after.state), "was": c.failoverCause,
			"attempts": c.failover.attempts,
		})
		c.failoverCause = ""
		c.failover = failoverSchedule{}
		return
	}
	cause := hotspotArm(outcome)
	if failed := outcome.GetFailed(); failed != nil {
		cause = failed.GetDetail()
	}
	failedAt := c.clock.Now()
	every, backedOff := c.failover.failed(failedAt)
	c.recordFailoverCause(cause, func(cause string) {
		c.log.Warn(opFailover, "the hotspot could not be joined after Wi-Fi was lost; the join is tried again on the retry schedule",
			dlog.Context{"hotspot": c.cfg.Hotspot, "outcome": hotspotArm(outcome), "cause": cause,
				"retry_every": every.String()})
	})
	if backedOff {
		c.log.Info(opFailover, "the hotspot failover backs off: the join is now tried less often",
			dlog.Context{"hotspot": c.cfg.Hotspot, "retry_every": every.String(),
				"lost_for": failedAt.Sub(c.failover.lost).String(), "attempts": c.failover.attempts})
	}
}

// standDownFailoverLocked ends the failover once the mode is off or a network
// is joined: its schedule restarts from nothing on the next loss, and its end
// is recorded when something stood against it. Caller holds mu.
func (c *Controller) standDownFailoverLocked(rd reading) {
	if c.failoverCause != "" || c.failover.gaveUp {
		c.log.Info(opFailover, "the hotspot failover stood down: the mode is off or a network is joined",
			dlog.Context{"wifi": wifiArm(rd.state), "mode": modeArm(rd.state), "was": c.failoverCause,
				"gave_up": c.failover.gaveUp, "attempts": c.failover.attempts})
	}
	c.failoverCause = ""
	c.failover = failoverSchedule{}
}

// recordFailoverCause hands a NEW cause to record and keeps it as the one
// standing; a cause already standing is recorded at DEBUG only, so a hotspot
// out of reach for an hour is one warning and not one per beat.
func (c *Controller) recordFailoverCause(cause string, record func(cause string)) {
	if cause == c.failoverCause {
		c.log.Debug(opFailover, "the hotspot failover still cannot take", dlog.Context{"cause": cause})
		return
	}
	record(cause)
	c.failoverCause = cause
}

// failoverWants answers whether a standing is one the failover acts on: the
// mode on and no network joined.
func failoverWants(s *agentreplv1.PersistentWifiState) bool {
	return s.GetOn() != nil && s.GetNotJoined() != nil
}

// failoverBackoff is the hotspot failover's retry schedule: a failed join that
// ends less than `until` after Wi-Fi was lost is tried again `every` later.
// A join is never started once the last `until` has passed since the loss.
var failoverBackoff = []struct{ until, every time.Duration }{
	{until: 5 * time.Minute, every: 5 * time.Second},
	{until: 30 * time.Minute, every: time.Minute},
	{until: 3 * time.Hour, every: time.Hour},
}

// failoverRetryEvery answers how long after a failed join that ended `lost`
// after Wi-Fi was lost the next join waits; false once the schedule is spent.
func failoverRetryEvery(lost time.Duration) (time.Duration, bool) {
	for _, tier := range failoverBackoff {
		if lost < tier.until {
			return tier.every, true
		}
	}
	return 0, false
}

// failoverSchedule is one loss's retries. The zero value is a failover
// standing down: the next beat that wants it starts the loss and joins at once.
type failoverSchedule struct {
	// lost is when the failover first found Wi-Fi lost; zero before.
	lost time.Time
	// next is the earliest instant the next join may start.
	next time.Time
	// every is the retry interval the last failure set.
	every time.Duration
	// attempts counts the joins run since the loss.
	attempts int
	// gaveUp is set once the schedule is spent and the give-up recorded.
	gaveUp bool
}

// failoverDue is what a beat that wants the failover does.
type failoverDue int

const (
	// failoverJoin runs a join now.
	failoverJoin failoverDue = iota
	// failoverWaiting runs nothing: the next join is not due yet.
	failoverWaiting
	// failoverExhausted runs nothing and gives up: the schedule just ran out.
	failoverExhausted
	// failoverGivenUp runs nothing: the give-up was already recorded.
	failoverGivenUp
)

// due answers what a beat at now does, starting the loss on its first beat.
func (s *failoverSchedule) due(now time.Time) failoverDue {
	switch {
	case s.gaveUp:
		return failoverGivenUp
	case s.lost.IsZero():
		s.lost = now
		return failoverJoin
	case now.Before(s.next):
		return failoverWaiting
	}
	if _, ok := failoverRetryEvery(now.Sub(s.lost)); !ok {
		return failoverExhausted
	}
	return failoverJoin
}

// failed schedules the join after one that failed at `at`, answering the
// retry interval and whether it grew. A failure that ends after the schedule
// is spent makes the next beat due at once, and that beat gives up (due).
func (s *failoverSchedule) failed(at time.Time) (time.Duration, bool) {
	every, ok := failoverRetryEvery(at.Sub(s.lost))
	if !ok {
		s.next = at
		return s.every, false
	}
	backedOff := s.every != 0 && every != s.every
	s.every = every
	s.next = at.Add(every)
	return every, backedOff
}
