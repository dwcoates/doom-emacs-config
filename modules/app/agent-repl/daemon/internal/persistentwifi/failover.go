package persistentwifi

import (
	"context"

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

// failoverLocked is THE HOTSPOT FAILOVER: while the mode is on, a Wi-Fi
// interface that is joined to nothing is joined to the configured hotspot.
// Every beat that reads the mode on and no network tries again, so the hotspot
// is rejoined within one cadence of the connection dropping, and keeps being
// tried while it is out of reach. A join that does not take is an outcome, never
// an error: the next beat tries again. Each distinct cause of a failover that
// cannot take is recorded once (failoverCause), and the failover's end is
// recorded when there was one. Caller holds mu.
func (c *Controller) failoverLocked(ctx context.Context, rd reading) {
	if !failoverWants(rd.state) {
		if c.failoverCause != "" {
			c.log.Info(opFailover, "the hotspot failover stood down: the mode is off or a network is joined",
				dlog.Context{"wifi": wifiArm(rd.state), "mode": modeArm(rd.state), "was": c.failoverCause})
			c.failoverCause = ""
		}
		return
	}
	if c.cfg.Hotspot == "" {
		c.recordFailoverCause(errNoHotspot().Error(), func(cause string) {
			c.log.Error(opFailover, "Wi-Fi was lost with persistent wifi on, and no hotspot is configured to join",
				dlog.Context{"env": EnvHotspot, "cause": cause})
		})
		return
	}

	c.log.Info(opFailover, "Wi-Fi was lost with persistent wifi on; joining the hotspot",
		dlog.Context{"hotspot": c.cfg.Hotspot, "device": rd.device})
	outcome := c.joinHotspot(ctx, rd)
	if joined := outcome.GetJoined(); joined != nil {
		after := c.readLocked(ctx)
		c.log.Info(opFailover, "the hotspot was joined after Wi-Fi was lost", dlog.Context{
			"hotspot": joined.GetNetworkName(), "wifi": wifiArm(after.state), "was": c.failoverCause,
		})
		c.failoverCause = ""
		return
	}
	cause := hotspotArm(outcome)
	if failed := outcome.GetFailed(); failed != nil {
		cause = failed.GetDetail()
	}
	c.recordFailoverCause(cause, func(cause string) {
		c.log.Warn(opFailover, "the hotspot could not be joined after Wi-Fi was lost; the next beat tries again",
			dlog.Context{"hotspot": c.cfg.Hotspot, "outcome": hotspotArm(outcome), "cause": cause})
	})
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
