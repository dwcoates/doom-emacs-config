package rollout

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// opCarry is the operation the handover carry records under.
const opCarry = "daemon.rollout.carry"

// DefaultFactsBound bounds how long a successor adopting a shim MID-WORK waits
// for the shim to re-announce its session facts. The re-announcement is the
// first frame of the session watch the adoption opens, so the bound is a
// failure ceiling a healthy adoption never pays.
const DefaultFactsBound = 10 * time.Second

// ErrMidWorkRefused is a successor's refusal of a mid-work adoption it cannot
// make safely: the adopted shim is from before the contract a mid-work
// adoption relies on (it reported no build, or it never re-announced its
// session facts). The incumbent takes the workspace back and transfers it
// again at FREENESS, which is never unsafe. It is a not_yet_adopted answer to
// a participant, whose retry lands on that later transfer.
var ErrMidWorkRefused = fmt.Errorf("rollout: the adopted shim cannot be adopted mid-work; the incumbent transfers the workspace at freeness instead: %w", ErrNotYetAdopted)

// THE HANDOVER CARRY (owner ruling, 2026-09-27: a daemon handover moves a
// workspace to its successor WITHOUT waiting for its work to end). Everything
// a workspace's work IS survives the move by construction -- the shim keeps
// running, and every row it wrote is in the store -- but a few facts about it
// live only in the incumbent's memory: the prompt queue's acts, running cut
// and semantic head, the standing cold gate, and the shim replacements a move
// overtook. The incumbent writes them here, per workspace, BEFORE it releases
// the workspace's serving row; the successor reads them only AFTER that
// release, which is the durable edge proving they were written. There is no
// daemon-to-daemon channel: this file is the channel, exactly as the intent
// manifest is.

// Carry is one workspace's handover carry.
type Carry struct {
	// Workspace is the workspace carried.
	Workspace ids.WorkspaceID `json:"workspace"`
	// Daemon is the incumbent that wrote it: a successor honors only the
	// carry of the daemon it is adopting from.
	Daemon ids.InstanceID `json:"daemon"`
	// MidWork records that the workspace had a turn or detached work in
	// flight when its move was sealed: the successor must learn the adopted
	// shim's facts before it lets any held prompt go.
	MidWork bool `json:"mid_work"`
	// ShimBuild is the build the adopted shim last reported to the
	// incumbent, empty when it reported none.
	ShimBuild string `json:"shim_build"`
	// Queue is what the prompt queue held only in memory.
	Queue bounce.Handoff `json:"queue"`
	// ColdGate is the standing cold gate's facts (a protojson SessionCold),
	// empty when no gate stands.
	ColdGate json.RawMessage `json:"cold_gate,omitempty"`
	// Replacements are the shim replacements the move overtook or was asked
	// while it ran: the successor runs them after its adoption.
	Replacements []CarriedReplacement `json:"replacements,omitempty"`
}

// CarriedReplacement is one shim replacement handed to the successor.
type CarriedReplacement struct {
	Reason string `json:"reason"`
	Force  bool   `json:"force"`
}

// midWorkCompatible judges whether a mid-work adoption of the carried shim is
// safe, and names why not. A shim that reported no build is from before the
// diagnostics contract, which is also before the session re-announcement a
// mid-work adoption learns the turn in flight from.
func midWorkCompatible(carry Carry) (bool, string) {
	if carry.ShimBuild == "" {
		return false, "the shim reported no build, so it predates the session re-announcement a mid-work adoption learns the running turn from"
	}
	return true, ""
}

// carryDir is where the per-workspace carries live: beside the intent
// manifest, in the shared state root.
func (c *controller) carryDir() string {
	return filepath.Join(filepath.Dir(c.deps.IntentManifest), "handover-carry")
}

func (c *controller) carryPath(ws ids.WorkspaceID) string {
	return filepath.Join(c.carryDir(), string(ws)+".json")
}

func (c *controller) refusalPath(ws ids.WorkspaceID) string {
	return filepath.Join(c.carryDir(), string(ws)+".refused")
}

// writeCarry records one workspace's carry ATOMICALLY: a half-written carry
// read by the successor would install half of what the queue held.
func (c *controller) writeCarry(carry Carry) error {
	fields := dlog.Context{"workspace": string(carry.Workspace), "path": c.carryPath(carry.Workspace)}
	if c.deps.IntentManifest == "" {
		err := errors.New("rollout: no intent manifest path is configured, so there is nowhere to carry to")
		c.log.Error(opCarry, "cannot write the handover carry", withCause(fields, err))
		return err
	}
	body, err := json.MarshalIndent(carry, "", "  ")
	if err != nil {
		c.log.Error(opCarry, "could not encode the handover carry", withCause(fields, err))
		return fmt.Errorf("rollout: encode the carry of %q: %w", carry.Workspace, err)
	}
	if err := writeAtomically(c.carryDir(), c.carryPath(carry.Workspace), "carry-*.json", body); err != nil {
		c.log.Error(opCarry, "could not write the handover carry", withCause(fields, err))
		return fmt.Errorf("rollout: write the carry of %q: %w", carry.Workspace, err)
	}
	c.log.Info(opCarry, "wrote the handover carry", merge(fields, dlog.Context{
		"mid_work": carry.MidWork, "acts": len(carry.Queue.Acts), "running_cut": carry.Queue.Cut != nil,
		"cold_gate": len(carry.ColdGate) > 0, "replacements": len(carry.Replacements),
	}))
	return nil
}

// readCarry loads one workspace's carry. The bool reports whether one was
// there: a transfer from an incumbent that carried nothing -- or predates the
// carry -- writes none, which is not a failure.
func (c *controller) readCarry(ws ids.WorkspaceID) (Carry, bool, error) {
	body, err := os.ReadFile(c.carryPath(ws))
	if errors.Is(err, os.ErrNotExist) {
		return Carry{}, false, nil
	}
	if err != nil {
		return Carry{}, false, fmt.Errorf("rollout: read the carry of %q: %w", ws, err)
	}
	var carry Carry
	if err := json.Unmarshal(body, &carry); err != nil {
		return Carry{}, false, fmt.Errorf("rollout: decode the carry of %q: %w", ws, err)
	}
	return carry, true, nil
}

// removeCarry retires one workspace's carry and its refusal marker. Neither
// being there already is success.
func (c *controller) removeCarry(ws ids.WorkspaceID) error {
	var failures []error
	for _, path := range []string{c.carryPath(ws), c.refusalPath(ws)} {
		if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
			failures = append(failures, err)
		}
	}
	if err := errors.Join(failures...); err != nil {
		c.log.Error(opCarry, "could not retire the handover carry", withCause(dlog.Context{"workspace": string(ws)}, err))
		return fmt.Errorf("rollout: retire the carry of %q: %w", ws, err)
	}
	return nil
}

// writeRefusal records a successor's refusal of a mid-work adoption, which is
// how the incumbent -- with no channel to it -- learns to take the workspace
// back and transfer it at freeness.
func (c *controller) writeRefusal(ws ids.WorkspaceID, why string) error {
	if err := writeAtomically(c.carryDir(), c.refusalPath(ws), "refused-*", []byte(why+"\n")); err != nil {
		c.log.Error(opCarry, "could not record the refused mid-work adoption", withCause(dlog.Context{"workspace": string(ws)}, err))
		return fmt.Errorf("rollout: record the refusal of %q: %w", ws, err)
	}
	return nil
}

// refusedMidWork reports whether the successor refused this workspace's
// mid-work adoption. A marker that cannot be read is recorded and read as not
// refused: the adoption window still bounds the wait.
func (c *controller) refusedMidWork(ws ids.WorkspaceID) bool {
	_, err := os.Stat(c.refusalPath(ws))
	switch {
	case err == nil:
		return true
	case errors.Is(err, os.ErrNotExist):
		return false
	default:
		c.log.Error(opCarry, "could not read the refusal marker; the adoption window bounds the wait", withCause(dlog.Context{"workspace": string(ws)}, err))
		return false
	}
}

// writeAtomically writes body to path through a temporary file in dir.
func writeAtomically(dir, path, pattern string, body []byte) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("create %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, pattern)
	if err != nil {
		return fmt.Errorf("create a temporary file in %s: %w", dir, err)
	}
	if _, err := tmp.Write(body); err != nil {
		return errors.Join(err, tmp.Close(), os.Remove(tmp.Name()))
	}
	if err := tmp.Close(); err != nil {
		return errors.Join(err, os.Remove(tmp.Name()))
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return errors.Join(err, os.Remove(tmp.Name()))
	}
	return nil
}

// sealedMove is what an incumbent's transfer sealed and carried for one
// workspace, kept until the move's outcome is known: an adoption that landed
// tells the carried replacements' requesters they were handed across; a move
// taken back puts the queue's memory back and asks for the replacements here.
type sealedMove struct {
	handoff bounce.Handoff
	// carried are the replacements written into the carry, for the successor
	// to run.
	carried []bounce.Request
	// rejudged are the stale-build replacements the carry leaves out: the
	// successor judges every adopted shim against ITS installed build.
	rejudged []bounce.Request
	// written reports that the carry file was written.
	written bool
	// midWork reports that the carry was written MID-WORK, the one kind of
	// carry a successor may refuse: only then is its consumption, and not the
	// serving row alone, the proof the adoption landed.
	midWork bool
}

// seal takes a running transfer's queue memory and writes the workspace's
// carry. A seal that cannot be carried is put back before it fails.
func (c *controller) seal(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) (*sealedMove, error) {
	handoff, carried, err := c.deps.Bounces.SealMove(ctx, ws)
	if err != nil {
		c.log.Error(opTransfer, "could not seal the workspace's queue for its move", withCause(fields, err))
		return nil, fmt.Errorf("rollout: seal %q: %w", ws, err)
	}
	move := &sealedMove{handoff: handoff}
	carry := Carry{Workspace: ws, Daemon: c.deps.Instance, Queue: handoff}
	// NO FREENESS SOURCE IS READ AS WORK IN FLIGHT: the successor then
	// waits for the adopted facts, which costs nothing when there is no work.
	carry.MidWork = c.deps.Freeness == nil || !c.deps.Freeness.Free(ws)
	c.mu.Lock()
	carry.ShimBuild = c.reported[ws]
	c.mu.Unlock()
	for _, req := range carried {
		if RelaunchReason(req.Reason) == ReasonBuildStale {
			move.rejudged = append(move.rejudged, req)
			continue
		}
		carry.Replacements = append(carry.Replacements, CarriedReplacement{Reason: req.Reason, Force: req.Force})
		move.carried = append(move.carried, req)
	}
	if cold, standing := c.deps.Shims.ColdGateStanding(ws); standing {
		raw, err := protojson.Marshal(cold)
		if err != nil {
			c.log.Error(opTransfer, "could not encode the standing cold gate for the carry; the seal is put back", withCause(fields, err))
			return nil, errors.Join(fmt.Errorf("rollout: carry the cold gate of %q: %w", ws, err), c.unseal(ctx, ws, move, fields))
		}
		carry.ColdGate = raw
	}
	if err := c.writeCarry(carry); err != nil {
		return nil, errors.Join(err, c.unseal(ctx, ws, move, fields))
	}
	move.written = true
	move.midWork = carry.MidWork
	return move, nil
}

// unseal puts a sealed move's queue memory back and retires its carry. The
// carried replacements are asked for again separately (reRequest), at the
// moment the registry can take them here.
func (c *controller) unseal(ctx context.Context, ws ids.WorkspaceID, move *sealedMove, fields dlog.Context) error {
	if move == nil {
		return nil
	}
	var failures []error
	if err := c.deps.Bounces.UnsealMove(ctx, ws, move.handoff); err != nil {
		c.log.Error(opTransfer, "could not put the sealed queue memory back", withCause(fields, err))
		failures = append(failures, fmt.Errorf("rollout: unseal %q: %w", ws, err))
	}
	if move.written {
		if err := c.removeCarry(ws); err != nil {
			failures = append(failures, err)
		}
	}
	return errors.Join(failures...)
}

// reRequest asks this daemon's registry again for the replacements a move
// that did not land was carrying, each with its own requester.
func (c *controller) reRequest(ctx context.Context, ws ids.WorkspaceID, move *sealedMove, fields dlog.Context) error {
	if move == nil {
		return nil
	}
	var failures []error
	for _, req := range append(append([]bounce.Request(nil), move.carried...), move.rejudged...) {
		if _, err := c.deps.Bounces.RequestBounce(ctx, ws, req); err != nil {
			c.log.Error(opTransfer, "a replacement the move was carrying could not be asked for again here",
				withCause(merge(fields, dlog.Context{"reason": req.Reason}), err))
			failures = append(failures, fmt.Errorf("rollout: ask again for %s on %q: %w", req.Reason, ws, err))
			if req.Done != nil {
				req.Done(err)
			}
		}
	}
	return errors.Join(failures...)
}

// handedAcross tells every carried replacement's requester that the daemon
// the workspace moved to runs it.
func handedAcross(move *sealedMove) {
	if move == nil {
		return
	}
	for _, req := range append(append([]bounce.Request(nil), move.carried...), move.rejudged...) {
		if req.Done != nil {
			req.Done(bounce.ErrHandedAcross)
		}
	}
}

// decodeCold decodes a carried cold gate.
func decodeCold(raw json.RawMessage) (*conversationv1.SessionCold, error) {
	cold := &conversationv1.SessionCold{}
	if err := protojson.Unmarshal(raw, cold); err != nil {
		return nil, err
	}
	return cold, nil
}

// THE SUCCESSOR'S HALF. Everything below runs on the daemon adopting the
// workspace, inside adopt.

// takeCarry reads the carry the outgoing daemon wrote for ws. A carry some
// OTHER daemon wrote is a stale one -- its move never landed and nothing
// consumed it -- and installing it would replay acts that daemon already put
// back: it is retired at ERROR and the adoption goes on uncarried.
func (c *controller) takeCarry(ws ids.WorkspaceID, outgoing ids.InstanceID, fields dlog.Context) (Carry, bool, error) {
	carry, found, err := c.readCarry(ws)
	if err != nil {
		c.log.Error(opCarry, "could not read the handover carry; the workspace is not adopted", withCause(fields, err))
		return Carry{}, false, err
	}
	if !found {
		c.log.Debug(opCarry, "the outgoing daemon carried nothing for the workspace", fields)
		return Carry{}, false, nil
	}
	if carry.Workspace != ws || carry.Daemon != outgoing {
		c.log.Error(opCarry, "the handover carry was written by another daemon or for another workspace; it is stale and retired unread",
			merge(fields, dlog.Context{"carry_daemon": string(carry.Daemon), "carry_workspace": string(carry.Workspace)}))
		if err := c.removeCarry(ws); err != nil {
			return Carry{}, false, err
		}
		return Carry{}, false, nil
	}
	c.log.Info(opCarry, "read the handover carry", merge(fields, dlog.Context{
		"mid_work": carry.MidWork, "shim_build": carry.ShimBuild, "acts": len(carry.Queue.Acts),
		"running_cut": carry.Queue.Cut != nil, "cold_gate": len(carry.ColdGate) > 0, "replacements": len(carry.Replacements),
	}))
	return carry, true, nil
}

// refuseMidWork refuses, before any claim, a mid-work adoption this daemon
// cannot make safely. The row stays unowned for the incumbent's take-back.
func (c *controller) refuseMidWork(ws ids.WorkspaceID, why string, fields dlog.Context) error {
	return errors.Join(fmt.Errorf("rollout: adopt %q: %s: %w", ws, why, ErrMidWorkRefused), c.recordRefusal(ws, why, fields))
}

// recordRefusal states a refused mid-work adoption and writes the marker the
// incumbent takes the workspace back on.
func (c *controller) recordRefusal(ws ids.WorkspaceID, why string, fields dlog.Context) error {
	c.log.Info(opAdopt, "refused the mid-work adoption; the incumbent takes the workspace back and transfers it at freeness",
		merge(fields, dlog.Context{"why": why}))
	return c.writeRefusal(ws, why)
}

// refuseAdopted refuses a mid-work adoption AFTER the claim and the dial: the
// dialed shim is let go -- detached, never killed -- and the serving row given
// back, so the incumbent's take-back claims it, re-dials the shim and releases
// the hold it took, which this daemon never drained.
func (c *controller) refuseAdopted(ctx context.Context, ws ids.WorkspaceID, why string, fields dlog.Context) error {
	failures := []error{fmt.Errorf("rollout: adopt %q: %s: %w", ws, why, ErrMidWorkRefused)}
	if _, err := c.deps.Shims.HandOver(ws); err != nil {
		c.log.Error(opAdopt, "could not let go of the shim of a refused mid-work adoption", withCause(fields, err))
		failures = append(failures, fmt.Errorf("rollout: adopt %q: let go of the shim: %w", ws, err))
	}
	if err := c.deps.DB.ReleaseServing(ctx, ws, c.deps.Instance); err != nil {
		c.log.Error(opAdopt, "could not give back the serving row of a refused mid-work adoption", withCause(fields, err))
		failures = append(failures, fmt.Errorf("rollout: adopt %q: give back serving: %w", ws, err))
	}
	if err := c.recordRefusal(ws, why, fields); err != nil {
		failures = append(failures, err)
	}
	return errors.Join(failures...)
}

// dialAdopted dials the workspace's running shim: a shim parked at its cold
// gate is held with no watcher and its carried gate raised again; any other
// is adopted with its session watched.
func (c *controller) dialAdopted(ctx context.Context, ws ids.WorkspaceID, parked *conversationv1.SessionCold, fields dlog.Context) error {
	if parked != nil {
		if _, err := c.deps.Shims.AdoptParked(ctx, ws, parked); err != nil {
			c.log.Error(opAdopt, "could not adopt the workspace's shim parked at its cold gate; its held intake is drained all the same", withCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: dial the parked shim: %w", ws, err)
		}
		return nil
	}
	if _, err := c.deps.Shims.Adopt(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not adopt the workspace's running shim; its held intake is drained all the same", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: dial the running shim: %w", ws, err)
	}
	return nil
}

// consumeCarry retires a carry the adoption has taken up.
func (c *controller) consumeCarry(ws ids.WorkspaceID, carried bool) error {
	if !carried {
		return nil
	}
	return c.removeCarry(ws)
}

// runCarried asks this daemon's registry for every replacement the move
// carried. A carried restart is followed by the webapp reload its verb would
// have pushed.
func (c *controller) runCarried(ctx context.Context, ws ids.WorkspaceID, carried []CarriedReplacement, fields dlog.Context) {
	for _, r := range carried {
		reason := RelaunchReason(r.Reason)
		rFields := merge(fields, dlog.Context{"reason": r.Reason, "force": r.Force})
		done := func(err error) {
			if err != nil || reason != ReasonRestartVerb {
				return
			}
			if err := c.ReloadWebapp(ctx, ws); err != nil {
				c.log.Error(opCarry, "could not push the webapp reload after the carried restart", withCause(rFields, err))
			}
		}
		if _, err := c.BounceShim(ctx, ws, reason, r.Force, done); err != nil {
			// BounceShim recorded the refusal at ERROR; the adoption stands.
			continue
		}
		c.log.Info(opCarry, "asked this daemon's registry for a replacement the move carried", rFields)
	}
}

// THE FRESH BOOT'S HALF. A restart across a layout change stands each
// workspace down mid-work and carries its queue memory exactly as a handover
// does; with no successor listening, the carry is read by the replacement's
// BOOT, which adopts the running shims (boot.adopt) and then takes each carry
// up here. Any fresh boot whose reconciled manifest names the daemon that
// wrote a carry takes it up the same way: that daemon released the serving
// row and exited, so its carry is the only record of what its queue held.

// carryWorkspace names the workspace a file in the carry directory carries,
// false for a file that is no carry.
func carryWorkspace(name string) (ids.WorkspaceID, bool) {
	// A TEMPORARY FILE writeAtomically never renamed is a partial write, never
	// a carry: its name carries the pattern's prefix.
	if filepath.Ext(name) != ".json" || strings.HasPrefix(name, "carry-") {
		return "", false
	}
	return ids.WorkspaceID(strings.TrimSuffix(name, ".json")), true
}

// TakeUpCarries implements Controller.
func (c *controller) TakeUpCarries(ctx context.Context, adopted []ids.WorkspaceID) error {
	c.mu.Lock()
	outgoing := c.bootOutgoing
	c.mu.Unlock()
	fields := dlog.Context{"outgoing_daemon": string(outgoing), "adopted": len(adopted)}
	entries, err := os.ReadDir(c.carryDir())
	if errors.Is(err, os.ErrNotExist) {
		c.log.Debug(opCarry, "no carry was ever written here; there is nothing to take up", fields)
		return nil
	}
	if err != nil {
		c.log.Error(opCarry, "could not list the carries the outgoing daemon wrote; the boot cannot know what its queue held", withCause(fields, err))
		return fmt.Errorf("rollout: list the carries in %s: %w", c.carryDir(), err)
	}
	survived := make(map[ids.WorkspaceID]bool, len(adopted))
	for _, ws := range adopted {
		survived[ws] = true
	}
	var failures []error
	takenUp := map[ids.WorkspaceID]Carry{}
	for _, entry := range entries {
		ws, ok := carryWorkspace(entry.Name())
		if !ok {
			continue
		}
		wsFields := merge(fields, dlog.Context{"workspace": string(ws)})
		carry, carried, err := c.takeCarry(ws, outgoing, wsFields)
		if err != nil {
			failures = append(failures, err)
			continue
		}
		if !carried {
			continue
		}
		if !survived[ws] {
			// THE SHIM DID NOT SURVIVE, so there is no running turn the carried
			// cut or head could be about: the workspace comes up through its
			// ordinary bring-up, and its held prompts were restored from the
			// store.
			c.log.Info(opCarry, "the carried workspace's shim was not adopted; its carry is retired untaken", wsFields)
			if err := c.removeCarry(ws); err != nil {
				failures = append(failures, err)
			}
			continue
		}
		// A CARRY THAT DOES NOT LAND WHOLE STILL LEAVES A SERVED WORKSPACE:
		// each part that failed is ERROR in takeUp, and the held prompts are
		// drained without it, as the successor's adoption drains them.
		c.takeUp(ctx, ws, carry, wsFields)
		takenUp[ws] = carry
	}
	c.mu.Lock()
	c.takenUp = takenUp
	c.mu.Unlock()
	if err := errors.Join(failures...); err != nil {
		return err
	}
	c.log.Info(opCarry, "took up the carries the outgoing daemon wrote", merge(fields, dlog.Context{"taken_up": len(takenUp)}))
	return nil
}

// takeUp installs one adopted workspace's carry: the queue memory, and the
// cold gate over the parked shim the boot adopted. The carry is retired
// whether or not both land, as the successor's adoption retires it: an
// unretired carry would be taken up again by the next boot. Every part that
// fails is recorded at ERROR here, and the answer joins them.
func (c *controller) takeUp(ctx context.Context, ws ids.WorkspaceID, carry Carry, fields dlog.Context) error {
	var failures []error
	if err := c.deps.Bounces.AdoptHandoff(ctx, ws, carry.Queue); err != nil {
		c.log.Error(opCarry, "could not install the carried queue memory; the held prompts are drained without it", withCause(fields, err))
		failures = append(failures, fmt.Errorf("rollout: take up the carry of %q: install the queue memory: %w", ws, err))
	}
	if len(carry.ColdGate) > 0 {
		cold, err := decodeCold(carry.ColdGate)
		switch {
		case err != nil:
			c.log.Error(opCarry, "could not decode the carried cold gate; it is not raised", withCause(fields, err))
			failures = append(failures, fmt.Errorf("rollout: take up the carry of %q: decode the cold gate: %w", ws, err))
		default:
			if err := c.deps.Shims.RaiseCarriedColdGate(ctx, ws, cold); err != nil {
				c.log.Error(opCarry, "could not raise the carried cold gate over the adopted shim", withCause(fields, err))
				failures = append(failures, fmt.Errorf("rollout: take up the carry of %q: raise the cold gate: %w", ws, err))
			}
		}
	}
	if err := c.removeCarry(ws); err != nil {
		failures = append(failures, err)
	}
	return errors.Join(failures...)
}

// FinishCarries implements Controller.
func (c *controller) FinishCarries(ctx context.Context) {
	c.mu.Lock()
	takenUp := c.takenUp
	c.takenUp = nil
	c.mu.Unlock()
	for ws, carry := range takenUp {
		fields := dlog.Context{"workspace": string(ws), "outgoing_daemon": string(carry.Daemon)}
		if err := c.deps.Bounces.RejudgeHeld(ctx, ws); err != nil {
			c.log.Error(opCarry, "could not re-judge the held prompts whose verdicts the move superseded", withCause(fields, err))
		}
		c.runCarried(ctx, ws, carry.Replacements, fields)
	}
}
