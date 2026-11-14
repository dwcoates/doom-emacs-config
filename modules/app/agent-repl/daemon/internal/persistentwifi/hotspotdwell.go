package persistentwifi

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
)

// THE HOTSPOT DWELL (owner ruling, 2026-11-14): while the lid-closed quiet
// state stands on the hotspot, a network macOS knows that has stayed in sight
// for knownDwell is joined in its place, so the owner walking back into range
// of a known network leaves the phone's hotspot without a flap. Joining from
// NO connection is the failover's, at once (failoverDueLocked); the dwell is
// only for leaving the hotspot.
//
// Whether the machine stands on the hotspot is the scan's to say (the joined
// network's name is the helper's alone, scan.go): the failover's own hotspot
// join says so, and a quiet state that began joined learns it at its first
// check. Checks are scans, so they run only while that is so or unknown.

// hotspotCheckEvery is the dwell's scan cadence while on the hotspot: a known
// network in sight for knownDwell is seen at most this long after.
const hotspotCheckEvery = 15 * time.Second

// knownDwell is how long a known network must stay in sight before the
// hotspot is left for it (owner ruling, 2026-11-14).
const knownDwell = time.Minute

// hotspotDwell is the dwell's state. The zero value stands on no hotspot and
// checks nothing.
type hotspotDwell struct {
	// on reports that the quiet state stands on the hotspot.
	on bool
	// unknown reports that whether it does is not known yet: the quiet state
	// began joined, and its first check (a scan) says.
	unknown bool
	// check is when the next check is due.
	check time.Time
	// seen is when each known network in sight was first seen in sight,
	// dropped the first check it is out of sight.
	seen map[string]time.Time
}

// hotspotCheckDueLocked answers the dwell's check that is due, if one is:
// the quiet state stands joined with the mode on, on the hotspot or not yet
// known to be, and the check's time has come. A standing it does not act on
// leaves the dwell as it is; the quiet state's edges reset it. Caller holds mu.
func (c *Controller) hotspotCheckDueLocked(ctx context.Context, rd reading) (failoverAttempt, bool) {
	d := &c.hotspot
	if !c.quiet || rd.state.GetOn() == nil || rd.state.GetJoined() == nil || c.cfg.Hotspot == "" {
		return failoverAttempt{}, false
	}
	if (!d.on && !d.unknown) || c.clock.Now().Before(d.check) || c.failoverCancel != nil {
		return failoverAttempt{}, false
	}
	attemptCtx, cancel := context.WithCancel(ctx)
	c.failoverCancel = cancel
	return failoverAttempt{ctx: attemptCtx, cancel: cancel}, true
}

// checkHotspot is one dwell check, OFF the lock: a scan says whether the
// machine stands on the hotspot, and, while it does, which known networks are
// in sight and since when; the first in the owner's preference order that has
// stayed in sight for knownDwell is joined.
func (c *Controller) checkHotspot(attempt failoverAttempt, rd reading) {
	defer attempt.cancel()
	defer func() {
		c.mu.Lock()
		c.failoverCancel = nil
		c.hotspot.check = c.clock.Now().Add(hotspotCheckEvery)
		c.mu.Unlock()
	}()
	scan, err := c.scanWifi(attempt.ctx)
	if attempt.ctx.Err() != nil {
		return
	}
	if err != nil {
		// A SCAN THAT NAMES NOTHING ENDS THE DWELL for this quiet state: the
		// next would fail the same way (the helper unauthorized, its scan
		// failing) at a helper launch every hotspotCheckEvery while the lid
		// stays closed, on battery. A loss is still answered by the failover,
		// which joins the hotspot. The deferred reschedule finds no dwell.
		c.mu.Lock()
		c.recordScanCauseLocked(err)
		c.hotspot = hotspotDwell{}
		c.mu.Unlock()
		return
	}
	if !sameNetwork(scan.Current, c.cfg.Hotspot) {
		c.mu.Lock()
		if c.hotspot.on {
			c.log.Info(opFailover, "the machine no longer stands on the hotspot; its known networks are no longer watched", dlog.Context{"device": rd.device})
		}
		c.hotspot = hotspotDwell{}
		c.scanCause = ""
		c.mu.Unlock()
		return
	}
	known, step := c.knownNetworks(attempt, rd, c.stillDwelling)
	if step == stepAbandoned {
		return
	}
	now := c.clock.Now()
	c.mu.Lock()
	c.scanCause = ""
	if !c.hotspot.on {
		c.log.Info(opFailover, "the machine stands on the hotspot with the lid closed; a known network in sight for a minute is joined in its place",
			dlog.Context{"device": rd.device, "dwell": knownDwell.String()})
	}
	c.hotspot.on, c.hotspot.unknown = true, false
	seen := map[string]time.Time{}
	var ready []string
	for _, name := range visibleKnown(known, scan) {
		first, ok := c.hotspot.seen[name]
		if !ok {
			first = now
		}
		seen[name] = first
		if now.Sub(first) >= knownDwell {
			ready = append(ready, name)
		}
	}
	c.hotspot.seen = seen
	c.mu.Unlock()
	for i, name := range ready {
		if !c.stillDwelling(attempt, "known_join") {
			return
		}
		joined, step := c.joinNamed(attempt, rd, name, dlog.Context{"index": i, "ready": len(ready), "saved": len(known), "from": "hotspot"}, c.stillDwelling)
		if step == stepAbandoned {
			return
		}
		if joined {
			c.mu.Lock()
			c.log.Info(opFailover, "left the hotspot for a known Wi-Fi network that stayed in sight for a minute", dlog.Context{
				"device": rd.device, "index": i, "ready": len(ready), "saved": len(known)})
			c.hotspot = hotspotDwell{}
			c.mu.Unlock()
			return
		}
	}
}

// stillDwelling reports whether a dwell check may make its next step: the
// daemon has not stood down, and the quiet state, the mode and a joined
// network still stand. A check that may not is recorded as abandoned, naming
// the step it stopped before.
func (c *Controller) stillDwelling(attempt failoverAttempt, step string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	if attempt.ctx.Err() == nil && c.quiet && c.last.GetOn() != nil && c.last.GetJoined() != nil {
		return true
	}
	c.log.Info(opFailover, "a hotspot dwell check was abandoned: the quiet state ended, the mode changed, the network dropped or the daemon stood down", dlog.Context{
		"before": step, "quiet": c.quiet, "lid": c.lid.name()})
	return false
}
