package persistentwifi

import (
	"context"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opFailover = "daemon.persistentwifi.failover"

// tick is one beat of Run: read the standing, then hold the energy mode to
// the quiet state as it stands (holdEnergyLocked: a Low Power write or a
// restore that failed is retried; with neither owed nothing is written) and
// the Wi-Fi failover to the reading. The read and both decisions run under the
// one lock, so a beat never acts on a reading an Update changed underneath
// it; a failover attempt that is due then runs OFF the lock
// (attemptFailover).
func (c *Controller) tick(ctx context.Context) {
	c.mu.Lock()
	rd := c.readLocked(ctx)
	if ctx.Err() != nil {
		// An abandoned read says nothing about either fact (readLocked).
		c.mu.Unlock()
		return
	}
	c.holdEnergyLocked(ctx, rd.energy, "beat")
	attempt, due := c.failoverDueLocked(ctx, rd)
	c.mu.Unlock()
	if due {
		c.attemptFailover(attempt, rd)
	}
}

// failoverRetryIn answers how long until the Wi-Fi failover's next step is
// due, and whether one is waiting at all: while a loss stands in the lid-closed
// quiet state, its grace or its retry interval running.
func (c *Controller) failoverRetryIn() (time.Duration, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	s := c.failover
	if s.lost.IsZero() || s.next.IsZero() || c.last == nil || !c.quiet || !failoverWants(c.last) {
		return 0, false
	}
	return max(s.next.Sub(c.clock.Now()), 0), true
}

// failoverDueLocked is THE WI-FI FAILOVER's decision (owner ruling
// 2026-11-13): while the lid-closed quiet state stands (the lid closed, the
// mode on), a Wi-Fi interface that has been joined to nothing for
// failoverLossGrace is brought back to a network: the owner's saved networks
// by name, in their preference order (rejoinSaved), then the configured
// hotspot (joinHotspotStep). It answers the attempt that is due, if one is.
//
// IT NEVER POWER-CYCLES THE RADIO, and it never acts with the lid open. On
// 2026-11-13 a radio cycle 71ms into an ordinary network switch, with the lid
// open, cut the owner's connection twice; with the lid open macOS roams on its
// own and the owner is there to see it. The grace keeps a switch that macOS
// finishes by itself from being taken for a loss.
//
// A step that does not take is an outcome, never an error: the attempt is
// made again on the retry schedule (failoverBackoff), which backs off to once
// a minute after five minutes and never gives up while the loss lasts. Each
// distinct cause a step cannot take is recorded once per step (knownCause,
// failoverCause), and the failover's end is recorded when it made an attempt.
// Caller holds mu.
func (c *Controller) failoverDueLocked(ctx context.Context, rd reading) (failoverAttempt, bool) {
	if !c.quiet || !failoverWants(rd.state) {
		c.standDownFailoverLocked(rd)
		return failoverAttempt{}, false
	}
	now := c.clock.Now()
	if c.failover.lost.IsZero() {
		c.failover.lost = now
		c.failover.next = now.Add(failoverLossGrace)
		c.recordLossLocked(ctx, rd)
		return failoverAttempt{}, false
	}
	if !c.failover.due(now) {
		return failoverAttempt{}, false
	}
	c.failover.attempts++
	attemptCtx, cancel := context.WithCancel(ctx)
	c.failoverCancel = cancel
	attempt := failoverAttempt{ctx: attemptCtx, cancel: cancel, number: c.failover.attempts}
	c.log.Info(opFailover, "a Wi-Fi failover attempt starts", dlog.Context{
		"device": rd.device, "attempt": attempt.number, "lost_for": now.Sub(c.failover.lost).String()})
	return attempt, true
}

// failoverAttempt is one failover attempt in flight: its context, which
// leaving the quiet state cancels (abandonFailoverLocked), and its number.
type failoverAttempt struct {
	ctx    context.Context
	cancel context.CancelFunc
	number int
}

// failoverStep is how one failover step ended.
type failoverStep int

const (
	// stepJoined: a network was joined, which ends the failover.
	stepJoined failoverStep = iota + 1
	// stepNotJoined: nothing was joined; the next step, or the retry, follows.
	stepNotJoined
	// stepAbandoned: the quiet state ended or the daemon stood down
	// mid-attempt; nothing more is joined, and the schedule is left to the
	// next beat.
	stepAbandoned
)

// attemptFailover runs ATTEMPT OFF THE LOCK (2026-11-13: an attempt joining
// 116 saved networks one by one held the lock for six minutes, and the lid
// opening waited behind it, so the lid-closed quiet state kept every page
// view frozen six minutes after the owner opened the lid). Each step takes the
// lock only to read or record, so a lid fact lands between any two joins; each
// join first checks that the quiet state still stands (stillWanted); and
// leaving the quiet state cancels the attempt, which ends the join in flight.
func (c *Controller) attemptFailover(attempt failoverAttempt, rd reading) {
	defer attempt.cancel()
	defer func() {
		c.mu.Lock()
		c.failoverCancel = nil
		c.mu.Unlock()
	}()
	if c.rejoinSaved(attempt, rd) != stepNotJoined {
		return
	}
	c.joinHotspotStep(attempt, rd)
}

// stillWanted reports whether ATTEMPT may make its next join: the daemon has
// not stood down, and the quiet state and the mode still stand. An attempt
// that may not is recorded as abandoned, naming the step it stopped before.
func (c *Controller) stillWanted(attempt failoverAttempt, step string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	if attempt.ctx.Err() == nil && c.quiet && c.last != nil && failoverWants(c.last) {
		return true
	}
	c.log.Info(opFailover, "a Wi-Fi failover attempt was abandoned: the quiet state ended, the mode changed or the daemon stood down", dlog.Context{
		"attempt": attempt.number, "before": step, "quiet": c.quiet, "lid": c.lid.name()})
	return false
}

// abandonFailoverLocked cancels the failover attempt in flight, if one is,
// ending the join it is running. Caller holds mu.
func (c *Controller) abandonFailoverLocked() {
	if c.failoverCancel != nil {
		c.failoverCancel()
	}
}

// failoverLossGrace is how long Wi-Fi must stay lost before the failover acts.
// Measured: on 2026-11-12 macOS rejoined the home network by itself 9s after
// the lid opened (00:44:01 to 00:44:10 UTC); the grace is two to three times
// that, so a network switch macOS completes is never acted on.
const failoverLossGrace = 20 * time.Second

// rejoinSaved is the failover's FIRST STEP: join the owner's saved networks
// BY NAME, in the preference order `networksetup
// -listpreferredwirelessnetworks` lists them (readable without the location
// permission the joined network's name needs), the configured hotspot
// excepted (the second step joins it). macOS supplies each one's password from
// the keychain; one out of range fails ("Could not find network") and
// disconnects nothing. A join ends the failover. Saved network names are the
// owner's: the records name how many there are and which index was tried,
// never the name. It runs off the lock (attemptFailover).
func (c *Controller) rejoinSaved(attempt failoverAttempt, rd reading) failoverStep {
	ctx := attempt.ctx
	if rd.device == "" {
		// No interface: the hotspot step names the outcome.
		return stepNotJoined
	}
	if !c.stillWanted(attempt, "saved_list") {
		return stepAbandoned
	}
	out, err := run(ctx, c.runner, c.cfg.Tools.Networksetup, "-listpreferredwirelessnetworks", rd.device)
	if ctx.Err() != nil {
		c.stillWanted(attempt, "saved_list")
		return stepAbandoned
	}
	if err != nil {
		c.mu.Lock()
		c.recordStandingCause(&c.knownCause, err.Error(), func(cause string) {
			c.log.Error(opFailover, "the saved Wi-Fi networks could not be read to rejoin one; the hotspot is tried",
				dlog.Context{"device": rd.device, "cause": cause})
		})
		c.mu.Unlock()
		return stepNotJoined
	}
	var saved []string
	for _, name := range preferredNetworks(out) {
		if !sameNetwork(name, c.cfg.Hotspot) {
			saved = append(saved, name)
		}
	}
	for i, name := range saved {
		if !c.stillWanted(attempt, "saved_join") {
			return stepAbandoned
		}
		joinOut, err := run(ctx, c.runner, c.cfg.Tools.Networksetup, "-setairportnetwork", rd.device, name)
		if err != nil || strings.TrimSpace(joinOut) != "" {
			// `-setairportnetwork` prints nothing on success and a sentence
			// naming the network on failure, so only the index is kept.
			c.log.Debug(opFailover, "a saved Wi-Fi network could not be joined", dlog.Context{
				"device": rd.device, "index": i, "saved": len(saved), "failed": err != nil})
			continue
		}
		c.mu.Lock()
		after := c.readLocked(ctx)
		if ctx.Err() != nil {
			// An abandoned read says nothing about the link (readLocked).
			c.mu.Unlock()
			return stepAbandoned
		}
		if after.state.GetJoined() == nil {
			c.mu.Unlock()
			continue
		}
		c.log.Info(opFailover, "a saved Wi-Fi network was rejoined after Wi-Fi was lost", dlog.Context{
			"device": rd.device, "index": i, "saved": len(saved), "attempts": attempt.number,
			"was": c.failoverCause, "lost_for": c.clock.Now().Sub(c.failover.lost).String(),
		})
		c.endFailoverLocked()
		c.mu.Unlock()
		return stepJoined
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.knownCause = ""
	c.log.Info(opFailover, "no saved Wi-Fi network could be rejoined; the hotspot is tried", dlog.Context{
		"device": rd.device, "saved": len(saved), "attempt": attempt.number,
		"lost_for": c.clock.Now().Sub(c.failover.lost).String(),
	})
	return stepNotJoined
}

// joinHotspotStep is the failover's SECOND STEP: join the configured
// hotspot. A join that does not take, or a hotspot that is not configured,
// schedules the next attempt. It runs off the lock (attemptFailover).
func (c *Controller) joinHotspotStep(attempt failoverAttempt, rd reading) {
	ctx := attempt.ctx
	if c.cfg.Hotspot == "" {
		c.mu.Lock()
		defer c.mu.Unlock()
		c.failover.failed(c.clock.Now())
		c.recordFailoverCause(errNoHotspot().Error(), func(cause string) {
			c.log.Error(opFailover, "Wi-Fi was lost with the lid closed, no saved network could be rejoined, and no hotspot is configured to join",
				dlog.Context{"env": EnvHotspot, "cause": cause})
		})
		return
	}
	if !c.stillWanted(attempt, "hotspot_join") {
		return
	}
	c.log.Info(opFailover, "joining the hotspot after Wi-Fi was lost", dlog.Context{
		"hotspot": c.cfg.Hotspot, "device": rd.device, "attempt": attempt.number,
	})
	outcome := c.joinHotspot(ctx, rd)
	if ctx.Err() != nil {
		c.stillWanted(attempt, "hotspot_outcome")
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	if joined := outcome.GetJoined(); joined != nil {
		after := c.readLocked(ctx)
		c.log.Info(opFailover, "the hotspot was joined after Wi-Fi was lost", dlog.Context{
			"hotspot": joined.GetNetworkName(), "wifi": wifiArm(after.state), "was": c.failoverCause,
			"attempts": attempt.number,
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
				"retry_every": every.String()})
	})
	if backedOff {
		c.log.Info(opFailover, "the Wi-Fi failover backs off: it is now tried less often",
			dlog.Context{"hotspot": c.cfg.Hotspot, "retry_every": every.String(),
				"lost_for": failedAt.Sub(c.failover.lost).String(), "attempts": attempt.number})
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
// is joined, or the lid opens: its schedule restarts from nothing on the next
// loss, its end is recorded when it made an attempt or something stood against
// it, and a loss that ended within the grace is recorded at DEBUG. Caller holds
// mu.
func (c *Controller) standDownFailoverLocked(rd reading) {
	switch {
	case c.failoverCause != "" || c.knownCause != "" || c.failover.attempts > 0:
		c.log.Info(opFailover, "the Wi-Fi failover stood down: a network is joined, the lid opened or the mode is off",
			dlog.Context{"wifi": wifiArm(rd.state), "mode": modeArm(rd.state), "quiet": c.quiet,
				"network_name": rd.state.GetJoined().GetNetworkName(),
				"was":          c.failoverCause, "was_saved": c.knownCause, "attempts": c.failover.attempts})
	case !c.failover.lost.IsZero():
		c.log.Debug(opFailover, "a Wi-Fi loss ended before the failover's grace, so nothing was done",
			dlog.Context{"wifi": wifiArm(rd.state), "quiet": c.quiet, "grace": failoverLossGrace.String(),
				"lost_for": c.clock.Now().Sub(c.failover.lost).String()})
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
// ends less than `until` after Wi-Fi was lost is tried again `every` later, and
// one that ends later is tried again failoverRetryAfterBackoff later, for as
// long as Wi-Fi stays lost with the mode on. It never gives up: a give-up after
// three hours left the machine offline for hours on 2026-11-12, and an attempt
// a minute costs a few joins that disconnect nothing.
var failoverBackoff = []struct{ until, every time.Duration }{
	{until: 5 * time.Minute, every: 5 * time.Second},
}

// failoverRetryAfterBackoff is the retry interval once every tier of
// failoverBackoff has passed.
const failoverRetryAfterBackoff = time.Minute

// failoverRetryEvery answers how long after a failed attempt that ended `lost`
// after Wi-Fi was lost the next attempt waits.
func failoverRetryEvery(lost time.Duration) time.Duration {
	for _, tier := range failoverBackoff {
		if lost < tier.until {
			return tier.every
		}
	}
	return failoverRetryAfterBackoff
}

// failoverSchedule is one loss's retries. The zero value is a failover
// standing down: the next beat that wants it starts the loss (failoverDueLocked
// sets lost) and attempts at once.
type failoverSchedule struct {
	// lost is when the failover first found Wi-Fi lost; zero before.
	lost time.Time
	// next is the earliest instant the next step may start.
	next time.Time
	// every is the retry interval the last failure set.
	every time.Duration
	// attempts counts the attempts run since the loss.
	attempts int
}

// due answers whether a beat at now runs the failover's next step. The beat
// that first found the loss set next to the end of its grace.
func (s *failoverSchedule) due(now time.Time) bool {
	return !now.Before(s.next)
}

// failed schedules the attempt after one that failed at `at`, answering the
// retry interval and whether it grew.
func (s *failoverSchedule) failed(at time.Time) (time.Duration, bool) {
	every := failoverRetryEvery(at.Sub(s.lost))
	backedOff := s.every != 0 && every != s.every
	s.every = every
	s.next = at.Add(every)
	return every, backedOff
}
