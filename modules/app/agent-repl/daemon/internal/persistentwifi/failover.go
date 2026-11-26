package persistentwifi

import (
	"context"
	"fmt"
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
// quiet state, its retry interval running.
func (c *Controller) failoverRetryIn() (time.Duration, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.last == nil || !c.quiet {
		return 0, false
	}
	s := c.failover
	if !s.lost.IsZero() && !s.next.IsZero() && failoverWants(c.last) {
		return max(s.next.Sub(c.clock.Now()), 0), true
	}
	return 0, false
}

// failoverDueLocked is THE WI-FI FAILOVER's decision (owner rulings
// 2026-11-13 and 2026-11-14): while the lid-closed quiet state stands (the
// lid closed, the mode on), a Wi-Fi interface joined to nothing is brought
// back to a network at once: the configured hotspot (joinHotspotStep), and
// no other network ever. A saved network is macOS's own auto-join to rejoin
// (owner ruling, 2026-11-26: macOS rejoined saved networks with the lid
// closed, while this daemon's own rejoin of them fought it). It answers the
// attempt that is due, if one is.
//
// IT NEVER POWER-CYCLES THE RADIO, and it never acts with the lid open. On
// 2026-11-13 a radio cycle 71ms into an ordinary network switch, with the lid
// open, cut the owner's connection twice; with the lid open macOS roams on its
// own and the owner is there to see it.
//
// A step that does not take is an outcome, never an error: the attempt is
// made again on the retry schedule (failoverBackoff), which backs off to once
// a minute after five minutes and never gives up while the loss lasts. Each
// distinct cause a step cannot take is recorded once per step (scanCause,
// failoverCause), and the failover's end is recorded when it made
// an attempt.
// Caller holds mu.
func (c *Controller) failoverDueLocked(ctx context.Context, rd reading) (failoverAttempt, bool) {
	if !c.quiet || !failoverWants(rd.state) {
		c.standDownFailoverLocked(rd)
		return failoverAttempt{}, false
	}
	now := c.clock.Now()
	if c.failover.lost.IsZero() {
		// THE FIRST ATTEMPT IS AT ONCE (owner ruling, 2026-11-14); a switch
		// macOS finishes by itself meanwhile is read again before the join
		// (joinHotspotStep), so the join never undoes it.
		c.failover.lost = now
		c.failover.next = now
		c.recordLossLocked(ctx, rd)
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
	// stepNotJoined: nothing was joined yet; the next step follows.
	stepNotJoined failoverStep = iota + 1
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
	if c.surveyAttempt(attempt, rd) == stepAbandoned {
		return
	}
	c.joinHotspotStep(attempt, rd)
}

// THE ATTEMPT'S SURVEY (owner request, 2026-11-20): every attempt, before
// either step, scans once and records what stood around it, never
// de-duplicated: whether the hotspot was in sight and how strong, Bluetooth's
// power (Instant Hotspot finds the phone over it), the lid, the display and
// the power. Seven days of hotspot joins failing with "Could not find
// network" said nothing of whether the phone was broadcasting, since the
// scans before them recorded only counts.

// Expected time and bound of one Bluetooth read (`system_profiler
// SPBluetoothDataType -json`). Measured 2026-11-20 19:20 on this host at load
// average 249: five reads took 449ms to 639ms. The bound is 2.5 times the
// worst.
const (
	bluetoothExpected = 640 * time.Millisecond
	bluetoothBound    = 1600 * time.Millisecond
)

// surveyAttempt is the attempt's survey: one scan (none when there is no
// Wi-Fi interface; a failed scan's cause recorded once) and its record. It
// runs off the lock.
func (c *Controller) surveyAttempt(attempt failoverAttempt, rd reading) failoverStep {
	ctx := attempt.ctx
	fields := dlog.Context{"attempt": attempt.number, "device": rd.device}
	var scan *wifiScan
	if rd.device == "" {
		fields["scan"] = "no_wifi_interface"
	} else {
		if !c.stillWanted(attempt, "scan") {
			return stepAbandoned
		}
		got, err := c.scanWifi(ctx)
		if ctx.Err() != nil {
			c.stillWanted(attempt, "scan")
			return stepAbandoned
		}
		c.mu.Lock()
		if err != nil {
			c.recordScanCauseLocked(err)
			fields["scan"], fields["scan_cause"] = "failed", err.Error()
		} else {
			c.scanCause = ""
			fields["scan"], fields["visible"] = "read", len(got.Visible)
			scan = &got
		}
		c.mu.Unlock()
	}
	power, err := c.bluetoothPower(ctx)
	if ctx.Err() != nil {
		c.stillWanted(attempt, "bluetooth")
		return stepAbandoned
	}
	fields["bluetooth_power"] = power
	if err != nil {
		fields["bluetooth_power_cause"] = err.Error()
	}
	for k, v := range hotspotSighting(c.cfg.Hotspot, scan) {
		fields[k] = v
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	fields["lid"], fields["quiet"], fields["display_asleep"] = c.lid.name(), c.quiet, c.displayAsleep
	fields["display_cause"], fields["power_source"], fields["energy_mode"] = c.displayCause, c.powerSource.name(), rd.energy
	c.log.Info(opFailover, "a Wi-Fi failover attempt's survey: the hotspot's sighting, Bluetooth, the lid, the display and the power", fields)
	return stepNotJoined
}

// bluetoothPower reads Bluetooth's power, bounded by bluetoothBound:
// "on", "off", or "unreadable" with why.
func (c *Controller) bluetoothPower(ctx context.Context) (string, error) {
	bounded, cancel := context.WithTimeout(ctx, c.bluetoothBound)
	defer cancel()
	power, err := c.host.BluetoothPower(bounded)
	switch {
	case ctx.Err() != nil:
		return "", ctx.Err()
	case bounded.Err() != nil:
		return "unreadable", fmt.Errorf("the Bluetooth read did not answer within %s (expected %s): %w", c.bluetoothBound, bluetoothExpected, bounded.Err())
	case err != nil:
		return "unreadable", err
	}
	return power, nil
}

// hotspotSighting is the survey's say on HOTSPOT in SCAN: "visible" with the
// SSID as seen, its RSSI and channel; "not_visible"; or why it is unknown
// ("unscanned": no scan was read; "no_hotspot_configured").
func hotspotSighting(hotspot string, scan *wifiScan) dlog.Context {
	switch {
	case hotspot == "":
		return dlog.Context{"hotspot_sighting": "no_hotspot_configured"}
	case scan == nil:
		return dlog.Context{"hotspot": hotspot, "hotspot_sighting": "unscanned"}
	}
	seen, ok := scan.sighting(hotspot)
	if !ok {
		return dlog.Context{"hotspot": hotspot, "hotspot_sighting": "not_visible"}
	}
	fields := dlog.Context{"hotspot": hotspot, "hotspot_sighting": "visible", "hotspot_ssid": seen.SSID, "hotspot_rssi": seen.RSSI}
	if seen.Channel != nil {
		fields["hotspot_channel"] = *seen.Channel
	}
	return fields
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

// joinBound bounds one `networksetup -setairportnetwork`. NOT YET MEASURED on
// this host (a join changes the network, which no measurement may do): every
// join records its duration (joinNamed), and the bound is to be set to 2-3
// times the healthy worst case those records show (org/todo.md). Until then
// it is generous, so a slow join is never cut.
const joinBound = 30 * time.Second

// joinHotspotStep is the failover's step: join the configured hotspot. A join that does not take, or a hotspot that is not configured,
// schedules the next attempt. It runs off the lock (attemptFailover).
func (c *Controller) joinHotspotStep(attempt failoverAttempt, rd reading) {
	ctx := attempt.ctx
	if c.cfg.Hotspot == "" {
		c.mu.Lock()
		defer c.mu.Unlock()
		c.failover.failed(c.clock.Now())
		c.recordFailoverCause(errNoHotspot().Error(), func(cause string) {
			c.log.Error(opFailover, "Wi-Fi was lost with the lid closed, and no hotspot is configured to join",
				dlog.Context{"env": EnvHotspot, "cause": cause})
		})
		return
	}
	if !c.stillWanted(attempt, "hotspot_join") {
		return
	}
	rd, lost := c.stillLost(attempt)
	if !lost {
		return
	}
	c.log.Info(opFailover, "joining the hotspot after Wi-Fi was lost", dlog.Context{
		"hotspot": c.cfg.Hotspot, "device": rd.device, "attempt": attempt.number,
	})
	bounded, cancel := context.WithTimeout(ctx, joinBound)
	defer cancel()
	start := c.clock.Now()
	outcome := c.joinHotspot(bounded, rd)
	took := c.clock.Now().Sub(start)
	if ctx.Err() != nil {
		c.stillWanted(attempt, "hotspot_outcome")
		return
	}
	// EVERY HOTSPOT JOIN'S EXACT OUTCOME IS RECORDED (owner request,
	// 2026-11-20), whatever the de-duplicated cause record below says.
	c.log.Info(opFailover, "joined the hotspot, or tried to", hotspotJoinRecord(rd.device, attempt.number, c.cfg.Hotspot, outcome, took, bounded.Err() != nil))
	c.mu.Lock()
	defer c.mu.Unlock()
	if joined := outcome.GetJoined(); joined != nil {
		after := c.readLocked(ctx)
		c.log.Info(opFailover, "the hotspot was joined after Wi-Fi was lost", dlog.Context{
			"hotspot": joined.GetNetworkName(), "wifi": wifiArm(after.state), "was": c.failoverCause,
			"attempts": attempt.number,
		})
		// THE HOTSPOT STAYS while the lid is closed (owner ruling, 2026-11-18):
		// an idle iPhone hotspot stops broadcasting, so a hotspot left for a
		// known network that then drops cannot be rejoined unattended. A known
		// network is joined by macOS's own auto-join once the lid opens.
		c.endFailoverLocked()
		return
	}
	cause := hotspotArm(outcome)
	if failed := outcome.GetFailed(); failed != nil {
		cause = failed.GetDetail()
	}
	failedAt := c.clock.Now()
	every, backedOff := c.failover.failed(failedAt)
	// INFO, MARKED TRANSIENT: a hotspot that is out of sight or will not take
	// a join (the phone's hotspot off, the phone away, macOS's `tmpErr`) is
	// the machine's surroundings, which this daemon expects and cannot fix,
	// and the retry schedule answers it when the hotspot comes back
	// (2026-11-17 14:30: `Could not find network Dodge's iPhone.`).
	c.recordFailoverCause(cause, func(cause string) {
		c.log.Info(opFailover, "the hotspot could not be joined after Wi-Fi was lost; the failover is tried again on the retry schedule",
			dlog.AsTransient(dlog.Context{"hotspot": c.cfg.Hotspot, "outcome": hotspotArm(outcome), "cause": cause,
				"retry_every": every.String()}))
	})
	if backedOff {
		c.log.Info(opFailover, "the Wi-Fi failover backs off: it is now tried less often",
			dlog.Context{"hotspot": c.cfg.Hotspot, "retry_every": every.String(),
				"lost_for": failedAt.Sub(c.failover.lost).String(), "attempts": attempt.number})
	}
}

// stillLost reads the standing again right before the hotspot join and
// answers it, and whether Wi-Fi is still lost. THE JOIN NEVER UNDOES A
// NETWORK MACOS REJOINED (2026-11-26 13:08:04: an attempt whose survey began
// while the link was down joined the hotspot six seconds after the Mac was
// back on a network, and the join dropped it): a network joined meanwhile
// stands the failover down, recorded with what was joined. It takes the lock
// for the read alone.
func (c *Controller) stillLost(attempt failoverAttempt) (reading, bool) {
	c.mu.Lock()
	rd := c.readLocked(attempt.ctx)
	if attempt.ctx.Err() != nil {
		// An abandoned read says nothing about the link (readLocked).
		c.mu.Unlock()
		c.stillWanted(attempt, "hotspot_reread")
		return rd, false
	}
	defer c.mu.Unlock()
	if failoverWants(rd.state) {
		return rd, true
	}
	c.log.Info(opFailover, "Wi-Fi came back before the hotspot join, so the hotspot is not joined", dlog.Context{
		"attempt": attempt.number, "wifi": wifiArm(rd.state), "mode": modeArm(rd.state),
		"network_name": rd.state.GetJoined().GetNetworkName(), "lost_for": c.clock.Now().Sub(c.failover.lost).String(),
	})
	c.standDownFailoverLocked(rd)
	return rd, false
}

// hotspotJoinRecord is THE ONE record of a hotspot join: the interface, the
// SSID it named, how long the join took and whether its bound cut it, whether
// it joined, the outcome's arm, and its detail when it failed.
func hotspotJoinRecord(device string, attempt int, hotspot string, outcome *agentreplv1.UpdatePersistentWifiModeHotspot, took time.Duration, cut bool) dlog.Context {
	network, cause := "", ""
	switch {
	case outcome.GetJoined() != nil:
		network = outcome.GetJoined().GetNetworkName()
	case outcome.GetFailed() != nil:
		network, cause = outcome.GetFailed().GetNetworkName(), outcome.GetFailed().GetDetail()
	default:
		cause = hotspotArm(outcome)
	}
	record := dlog.Context{"device": device, "network": network, "took_ms": took.Milliseconds(), "took_bound": cut,
		"joined": cause == "", "attempt": attempt, "hotspot": hotspot, "outcome": hotspotArm(outcome)}
	if cause != "" {
		record["error"] = cause
	}
	return record
}

// endFailoverLocked clears the failover once a step joined a network: its
// schedule restarts from nothing on the next loss. Caller holds mu.
func (c *Controller) endFailoverLocked() {
	c.failoverCause = ""
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
	} else if power, err := c.host.RadioPower(ctx, rd.device); err != nil {
		fields["radio_power"] = "unreadable"
		fields["radio_power_cause"] = err.Error()
	} else {
		fields["radio_power"] = power
	}
	c.log.Info(opFailover, "Wi-Fi was lost with persistent wifi on", fields)
}

// standDownFailoverLocked ends the failover once the mode is off or a network
// is joined, or the lid opens: its schedule restarts from nothing on the next
// loss, and its end is recorded when it made an attempt or something stood
// against it. Caller holds mu.
func (c *Controller) standDownFailoverLocked(rd reading) {
	switch {
	case c.failoverCause != "" || c.failover.attempts > 0:
		c.log.Info(opFailover, "the Wi-Fi failover stood down: a network is joined, the lid opened or the mode is off",
			dlog.Context{"wifi": wifiArm(rd.state), "mode": modeArm(rd.state), "quiet": c.quiet,
				"network_name": rd.state.GetJoined().GetNetworkName(),
				"was":          c.failoverCause, "attempts": c.failover.attempts})
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
