package shimclient

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// A store seq is only meaningful inside one SHIM GENERATION's seq space.
//
// The vendor-uuid rotation is one way that space is retired, and it is already
// reconciled at handshake time (Config.OnHandshake). It is not the only way. A
// shim PROCESS that restarts can come back publishing under the same vendor
// uuid into a seq space that begins at 1 again — the durable high-water mark
// the daemon holds then names a position in a space that no longer exists, and
// the new generation's first live event reads as a terminal regression:
//
//	shim link LOST ... cause=shimclient: sequence regression:
//	session=cade0704-... got seq=90 after last_seen=4203 branch=reconnect
//
// 4203 belonged to the PREVIOUS generation; the surviving store held exactly
// 1..89 of the new one. The session controller exited, the shim was stopped and
// the workspace rendered SEVERED with no recovery — for an event that was
// perfectly legal in the space it was actually numbered in.
//
// The guard is not the problem, so it is not relaxed: WITHIN a generation a
// regression stays exactly as fatal as it was. What changes is that the mark
// now carries the identity of the generation it counts in, and a lower seq is
// re-read as a REBASE onto a new space only when the generation demonstrably
// differs. Where the generation cannot be identified, the answer is the old
// one — fatal — because an unprovable generation change must never become an
// amnesty for a real regression.

// shimGenerationID names the shim generation a connection belongs to: one shim
// PROCESS's lifetime, identified by the pid riding its ShimHello.
//
// The pid is safe as an identity here for the reason core.proto states for
// ShimHello.pid: it is only ever consulted while the CONNECTION that carried it
// is live, and a live connection is proof the process on the other end is the
// process that opened it. It is never persisted and never used after the
// connection ends.
//
// "" means UNIDENTIFIABLE (a hello carrying no pid — a shim built before the
// field existed), never "some generation". Callers must treat it as a generation
// they cannot compare rather than as one that matches.
func shimGenerationID(hello *corev1.ShimHello) string {
	if pid := hello.GetPid(); pid > 0 {
		return fmt.Sprintf("pid=%d", pid)
	}
	return ""
}

// reconcileSeqGeneration decides what a seq at or below the high-water mark
// MEANS, and is reached only for such a seq. It returns ErrSeqRegression when
// the event regresses inside the mark's own generation, and nil — after
// resetting the mark to the new generation's fresh space — when the generation
// has demonstrably changed under it.
//
// Both outcomes are logged loudly: the fatal one because it ends the session,
// and the reset because it silently drops a durable high-water mark to zero,
// which must be auditable from the log alone (old mark, both generations, and
// the seq that forced the decision).
func (c *Client) reconcileSeqGeneration(ev *corev1.Event, seq uint64) error {
	regression := fmt.Errorf("%w: session=%s got seq=%d after last_seen=%d",
		ErrSeqRegression, ev.GetSessionId(), seq, c.lastSeen)
	switch {
	case c.connGeneration == "":
		c.logf("SEQ REGRESSION session=%s seq=%d last_seen=%d mark_generation=%q decision=fatal cause=connection_generation_unidentifiable — this shim's hello carried no pid, so a generation change cannot be proven and the mark is trusted as-is",
			ev.GetSessionId(), seq, c.lastSeen, c.seqGeneration)
		return regression
	case c.connGeneration == c.seqGeneration:
		c.logf("SEQ REGRESSION session=%s seq=%d last_seen=%d generation=%q decision=fatal cause=in_generation_regression — the mark was advanced by THIS shim generation, so the merged stream can no longer be trusted",
			ev.GetSessionId(), seq, c.lastSeen, c.seqGeneration)
		return regression
	}
	c.logf("SHIM GENERATION CHANGE session=%s previous_generation=%q new_generation=%q retired_last_seen=%d first_seq=%d decision=reset_high_water — the seq space the mark counted in was retired with its shim; the mark is dropped to zero and this generation's space is adopted from seq=%d",
		ev.GetSessionId(), c.seqGeneration, c.connGeneration, c.lastSeen, seq, seq)
	c.lastSeen = 0
	c.seqGeneration = c.connGeneration
	return nil
}
