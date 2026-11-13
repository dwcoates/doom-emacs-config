package persistentwifi

import (
	"context"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opFailover = "daemon.persistentwifi.failover"

// tick is one beat of Run: read the standing, then hold the energy mode
// (reconcileEnergyLocked) and the Wi-Fi failover to it. Both run under the one lock, so a beat never acts on a reading an
// Update changed underneath it.
func (c *Controller) tick(ctx context.Context) {
	c.mu.Lock()
	defer c.mu.Unlock()
	rd := c.readLocked(ctx)
	if ctx.Err() != nil {
		// An abandoned read says nothing about either fact (readLocked).
		return
	}
	c.reconcileEnergyLocked(ctx, rd, "beat")
	c.failoverLocked(ctx, rd)
}

// failoverRetryIn answers how long until the Wi-Fi failover's next step is
// due, and whether one is waiting at all: only after a failed attempt or while
// the known-network step settles, while the last standing still wants the
// failover and it has not given up.
func (c *Controller) failoverRetryIn() (time.Duration, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	s := c.failover
	if s.lost.IsZero() || s.gaveUp || s.next.IsZero() || c.last == nil || !failoverWants(c.last) {
		return 0, false
	}
	return max(s.next.Sub(c.clock.Now()), 0), true
}

// failoverLocked is THE WI-FI FAILOVER: while the mode is on, a Wi-Fi
// interface that is joined to nothing is brought back to a network, the ones
// macOS already knows first and the configured hotspot second. Each attempt:
//
//  1. cycles the Wi-Fi radio's power (rejoinKnownLocked), which makes macOS
//     auto-join its preferred networks, the home network among them, whose
//     name it hides from the daemon; a network joined then ends the failover;
//  2. otherwise waits knownNetworkSettle for macOS's auto-join, during which a
//     network change that finds a network joined ends the failover;
//  3. joins the hotspot (joinHotspotLocked) once the wait passes with nothing
//     joined, or at once when the radio cannot be cycled.
//
// The first read that finds the mode on and no network starts an attempt at
// once. A step that does not take is an outcome, never an error: the attempt
// is made again on the retry schedule (failoverBackoff), which backs off the
// longer Wi-Fi stays lost and gives up after three hours. Each distinct cause
// a step cannot take is recorded once per step (knownCause, failoverCause),
// and the failover's end is recorded when it made an attempt. Caller holds mu.
func (c *Controller) failoverLocked(ctx context.Context, rd reading) {
	if !failoverWants(rd.state) {
		c.standDownFailoverLocked(rd)
		return
	}
	now := c.clock.Now()
	if c.failover.lost.IsZero() {
		c.failover.lost = now
		c.recordLossLocked(ctx, rd)
	}

	switch c.failover.due(now) {
	case failoverWaiting, failoverGivenUp:
		return
	case failoverExhausted:
		c.failover.gaveUp = true
		c.log.Warn(opFailover, "the Wi-Fi failover gave up: Wi-Fi stayed lost and no network could be joined for three hours; "+
			"it resumes once a network is joined or the mode is turned off and on",
			dlog.Context{"hotspot": c.cfg.Hotspot, "lost_for": now.Sub(c.failover.lost).String(),
				"attempts": c.failover.attempts, "cause": c.failoverCause})
		return
	}

	known := knownSettled
	if !c.failover.settling {
		c.failover.attempts++
		var done bool
		if done, known = c.rejoinKnownLocked(ctx, rd); done {
			return
		}
	}
	c.failover.settling = false
	c.joinHotspotLocked(ctx, rd, known)
}

// knownNetworkSettle is how long after the radio cycle macOS is left to
// auto-join a network it knows before the hotspot is joined. Measured: on the
// 2026-11-12 incident the home network rejoined by itself 9s after the lid
// opened (00:44:01 to 00:44:10 UTC); the wait is two to three times that.
const knownNetworkSettle = 20 * time.Second

// The known-network step's outcomes a hotspot join follows, for its record.
const (
	// knownSettled: the radio cycled and no network was joined within
	// knownNetworkSettle.
	knownSettled = "none_joined_within_settle"
	// knownRadioFailed: the radio could not be cycled.
	knownRadioFailed = "radio_cycle_failed"
	// knownNoInterface: the machine has no Wi-Fi interface to cycle.
	knownNoInterface = "no_wifi_interface"
)

// rejoinKnownLocked is the failover's FIRST STEP: cycle the Wi-Fi radio's power
// so macOS auto-joins the networks it knows. macOS withholds the joined
// network's name from the daemon (probe.go), so no known network can be named
// to join; the radio cycle is the one request that makes macOS run its own
// auto-join over every preferred network, and it is what turning the mode off
// already does to leave the hotspot (restartRadio). The read after it tells
// whether a network came back at once; when none did, the failover waits
// knownNetworkSettle for one (settling) before the hotspot. done answers
// whether the beat's attempt is over; when it is not, known names why the
// hotspot is joined now. Caller holds mu.
func (c *Controller) rejoinKnownLocked(ctx context.Context, rd reading) (done bool, known string) {
	if rd.device == "" {
		// No interface, no radio: the hotspot step names the outcome.
		return false, knownNoInterface
	}
	c.log.Info(opFailover, "Wi-Fi was lost with persistent wifi on; cycling the Wi-Fi radio so macOS rejoins a network it knows",
		dlog.Context{"device": rd.device, "attempt": c.failover.attempts, "lost_for": c.clock.Now().Sub(c.failover.lost).String()})
	var failures []string
	c.restartRadio(ctx, rd.device, &failures)
	if len(failures) > 0 {
		c.recordStandingCause(&c.knownCause, strings.Join(failures, "; "), func(cause string) {
			c.log.Warn(opFailover, "the Wi-Fi radio could not be cycled to rejoin a network macOS knows; the hotspot is joined at once",
				dlog.Context{"device": rd.device, "cause": cause})
		})
		return false, knownRadioFailed
	}
	after := c.readLocked(ctx)
	if ctx.Err() != nil {
		// An abandoned read says nothing about the link (readLocked).
		return true, ""
	}
	if after.state.GetJoined() != nil {
		c.log.Info(opFailover, "a network macOS knows was joined after the Wi-Fi radio was cycled", dlog.Context{
			"network_name": after.state.GetJoined().GetNetworkName(), "attempts": c.failover.attempts,
			"was": c.failoverCause, "lost_for": c.clock.Now().Sub(c.failover.lost).String(),
		})
		c.endFailoverLocked()
		return true, ""
	}
	fields := dlog.Context{"device": rd.device, "wifi": wifiArm(after.state), "settle": knownNetworkSettle.String()}
	if power, err := readRadioPower(ctx, c.runner, c.cfg.Tools, rd.device); err != nil {
		fields["radio_power"] = "unreadable"
		fields["radio_power_cause"] = err.Error()
	} else {
		fields["radio_power"] = power
	}
	c.failover.settling = true
	c.failover.next = c.clock.Now().Add(knownNetworkSettle)
	c.log.Info(opFailover, "the Wi-Fi radio was cycled and no network is joined yet; the hotspot is joined unless macOS joins a network it knows within the settle", fields)
	return true, ""
}

// joinHotspotLocked is the failover's SECOND STEP: join the configured
// hotspot, known naming how the known-network step ended. A join that does not
// take, or a hotspot that is not configured, schedules the next attempt.
// Caller holds mu.
func (c *Controller) joinHotspotLocked(ctx context.Context, rd reading, known string) {
	if c.cfg.Hotspot == "" {
		c.failover.failed(c.clock.Now())
		c.recordFailoverCause(errNoHotspot().Error(), func(cause string) {
			c.log.Error(opFailover, "Wi-Fi was lost with persistent wifi on, no network macOS knows came back, and no hotspot is configured to join",
				dlog.Context{"env": EnvHotspot, "known_network": known, "cause": cause})
		})
		return
	}
	c.log.Info(opFailover, "no network macOS knows came back; joining the hotspot", dlog.Context{
		"hotspot": c.cfg.Hotspot, "device": rd.device, "attempt": c.failover.attempts, "known_network": known,
		"lost_for": c.clock.Now().Sub(c.failover.lost).String(),
	})
	outcome := c.joinHotspot(ctx, rd)
	if joined := outcome.GetJoined(); joined != nil {
		after := c.readLocked(ctx)
		c.log.Info(opFailover, "the hotspot was joined after Wi-Fi was lost", dlog.Context{
			"hotspot": joined.GetNetworkName(), "wifi": wifiArm(after.state), "was": c.failoverCause,
			"attempts": c.failover.attempts,
		})
		c.endFailoverLocked()
		return
	}
	cause := hotspotArm(outcome)
	if failed := outcome.GetFailed(); failed != nil {
		cause = failed.GetDetail()
	}
	failedAt := c.clock.Now()
	every, backedOff := c.failover.failed(failedAt)
	c.recordFailoverCause(cause, func(cause string) {
		c.log.Warn(opFailover, "the hotspot could not be joined after Wi-Fi was lost; the failover is tried again on the retry schedule",
			dlog.Context{"hotspot": c.cfg.Hotspot, "outcome": hotspotArm(outcome), "cause": cause,
				"known_network": known, "retry_every": every.String()})
	})
	if backedOff {
		c.log.Info(opFailover, "the Wi-Fi failover backs off: it is now tried less often",
			dlog.Context{"hotspot": c.cfg.Hotspot, "retry_every": every.String(),
				"lost_for": failedAt.Sub(c.failover.lost).String(), "attempts": c.failover.attempts})
	}
}

// endFailoverLocked clears the failover once a step joined a network: its
// schedule restarts from nothing on the next loss. Caller holds mu.
func (c *Controller) endFailoverLocked() {
	c.failoverCause, c.knownCause = "", ""
	c.failover = failoverSchedule{}
}

// recordLossLocked is THE RECORD OF A LOSS: once per loss, at the beat that
// first finds Wi-Fi lost with the mode on, it records what the machine stood at
// when the link dropped, so a loss nobody saw says whether macOS powered the
// radio down or only disassociated it: the radio's power, the link summary
// ipconfig gave, the lid, the quiet state, the display and the energy mode. A
// radio power that cannot be read is named in the record with its cause, since
// the record is the only place the loss is seen. Caller holds mu.
func (c *Controller) recordLossLocked(ctx context.Context, rd reading) {
	fields := dlog.Context{
		"device": rd.device, "link_active": rd.link.joined, "link_summary": strings.TrimSpace(rd.summary),
		"lid": c.lid.name(), "quiet": c.quiet, "display_asleep": c.displayAsleep,
		"display_cause": c.displayCause, "energy_mode": rd.energy,
	}
	// A loss is a read Wi-Fi standing (failoverWants), so the interface was
	// read; only a machine with none has no radio to ask.
	if rd.device == "" {
		fields["radio_power"] = "no_wifi_interface"
	} else if power, err := readRadioPower(ctx, c.runner, c.cfg.Tools, rd.device); err != nil {
		fields["radio_power"] = "unreadable"
		fields["radio_power_cause"] = err.Error()
	} else {
		fields["radio_power"] = power
	}
	c.log.Info(opFailover, "Wi-Fi was lost with persistent wifi on", fields)
}

// standDownFailoverLocked ends the failover once the mode is off or a network
// is joined: its schedule restarts from nothing on the next loss, and its end
// is recorded when it made an attempt or something stood against it, which is
// how a network macOS joined by itself during the settle is recorded. Caller
// holds mu.
func (c *Controller) standDownFailoverLocked(rd reading) {
	if c.failoverCause != "" || c.knownCause != "" || c.failover.gaveUp || c.failover.attempts > 0 {
		c.log.Info(opFailover, "the Wi-Fi failover stood down: the mode is off or a network is joined",
			dlog.Context{"wifi": wifiArm(rd.state), "mode": modeArm(rd.state), "network_name": rd.state.GetJoined().GetNetworkName(),
				"was": c.failoverCause, "was_known": c.knownCause, "awaiting_known_network": c.failover.settling,
				"gave_up": c.failover.gaveUp, "attempts": c.failover.attempts})
	}
	c.endFailoverLocked()
}

// recordFailoverCause records the hotspot step's cause (recordStandingCause).
func (c *Controller) recordFailoverCause(cause string, record func(cause string)) {
	c.recordStandingCause(&c.failoverCause, cause, record)
}

// recordStandingCause hands a NEW cause to record and keeps it as the one
// standing in *standing; a cause already standing is recorded at DEBUG only,
// so a step out of reach for an hour is one warning and not one per attempt.
func (c *Controller) recordStandingCause(standing *string, cause string, record func(cause string)) {
	if cause == *standing {
		c.log.Debug(opFailover, "a Wi-Fi failover step still cannot take", dlog.Context{"cause": cause})
		return
	}
	record(cause)
	*standing = cause
}

// failoverWants answers whether a standing is one the failover acts on: the
// mode on and no network joined.
func failoverWants(s *agentreplv1.PersistentWifiState) bool {
	return s.GetOn() != nil && s.GetNotJoined() != nil
}

// failoverBackoff is the Wi-Fi failover's retry schedule: a failed attempt that
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
// standing down: the next beat that wants it starts the loss (failoverLocked
// sets lost) and joins at once.
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
	// settling is set while the radio was cycled and macOS is left until next
	// to auto-join a network it knows; the beat it falls due joins the hotspot.
	settling bool
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

// due answers what a beat at now does. The loss was started (lost) by the
// beat that first found it; a loss's first beat has no next and joins at once.
func (s *failoverSchedule) due(now time.Time) failoverDue {
	switch {
	case s.gaveUp:
		return failoverGivenUp
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
