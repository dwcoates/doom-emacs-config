package persistentwifi

import (
	"context"
	"errors"
	"fmt"
	"os"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/publish"
)

// Operations every record of this package carries.
const (
	opProbe  = "daemon.persistentwifi.probe"
	opUpdate = "daemon.persistentwifi.update"
)

// DefaultEvery is how often the standing is re-read. It bounds how long a
// change made OUTSIDE agent-repl (a network dropping, `pmset` run by hand)
// takes to reach the topbar and Emacs, and how long a dropped network goes
// unanswered by the hotspot failover; a change made through Update is
// published at once. Each read is three short-lived local tools.
const DefaultEvery = 5 * time.Second

// Deps are the controller's collaborators.
type Deps struct {
	// Config names the tools and the hotspot.
	Config Config
	// Runner runs every host tool. REQUIRED.
	Runner Runner
	// Clock paces the re-read. REQUIRED.
	Clock clock.Clock
	// Every is the re-read cadence; zero is DefaultEvery.
	Every time.Duration
	// Exists answers whether an optional tool is installed; nil is a stat of
	// a regular file.
	Exists func(path string) bool
	// OnChange is handed every standing that differs from the last one, after
	// it is published. The topbar takes the standing through it. Optional.
	OnChange func(*agentreplv1.PersistentWifiState)
	// Log is the package's canonical logger. REQUIRED.
	Log dlog.Logger
	// Lid hears the built-in display's lid (WatchLid). REQUIRED.
	Lid LidWatcher
	// LidDims keeps the pre-close brightness durably. REQUIRED.
	LidDims LidDims
}

// Controller reads, publishes and changes the persistent-wifi standing. One
// lock serializes every read and every change, so a toggle reads the mode it
// turns over under the same lock as the turn.
type Controller struct {
	cfg      Config
	runner   Runner
	clock    clock.Clock
	every    time.Duration
	exists   func(string) bool
	onChange func(*agentreplv1.PersistentWifiState)
	log      dlog.Logger
	lidWatch LidWatcher
	dims     LidDims
	topic    publish.Topic[*agentreplv1.PersistentWifiState]

	mu sync.Mutex
	// last is the standing last published; nil before the first read.
	last *agentreplv1.PersistentWifiState
	// wifiCause and modeCause are the last read failure recorded for each
	// fact, so a failure that persists is recorded once rather than on every
	// re-read.
	wifiCause, modeCause string
	// failoverCause is why the hotspot failover last could not take; empty
	// while it has nothing standing against it (failoverLocked).
	failoverCause string
	// lid is the lid as last heard (WatchLid), or why it is not heard.
	lid lidFact
	// dim mirrors the kept pre-close brightness (LidDims): nil while no
	// lid-close dim is in force. It is written only after the store took the
	// same change, and read only once dimLoaded.
	dim       *float64
	dimLoaded bool
}

// New builds the controller.
func New(d Deps) (*Controller, error) {
	switch {
	case d.Runner == nil:
		return nil, fmt.Errorf("persistentwifi: a runner is required")
	case d.Clock == nil:
		return nil, fmt.Errorf("persistentwifi: a clock is required")
	case d.Log == nil:
		return nil, fmt.Errorf("persistentwifi: a logger is required")
	case d.Lid == nil:
		return nil, fmt.Errorf("persistentwifi: a lid watcher is required")
	case d.LidDims == nil:
		return nil, fmt.Errorf("persistentwifi: a store for the pre-close brightness is required")
	case d.Every < 0:
		return nil, fmt.Errorf("persistentwifi: the re-read cadence must not be negative, got %s", d.Every)
	}
	every := d.Every
	if every == 0 {
		every = DefaultEvery
	}
	exists := d.Exists
	if exists == nil {
		exists = regularFile
	}
	return &Controller{
		cfg: d.Config, runner: d.Runner, clock: d.Clock, every: every,
		exists: exists, onChange: d.OnChange, log: d.Log,
		lidWatch: d.Lid, dims: d.LidDims,
	}, nil
}

// regularFile is the production Exists.
func regularFile(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.Mode().IsRegular()
}

// Topic is the standing's publication: WatchDaemon's `persistent_wifi` push.
func (c *Controller) Topic() *publish.Topic[*agentreplv1.PersistentWifiState] { return &c.topic }

// Refresh reads the standing once and publishes it. The composition root calls
// it before anything is served, so no stream or topbar is ever drawn from a
// standing nobody read. It only reads: the hotspot failover is Run's, so the
// boot never waits on a Wi-Fi join. It also reads the kept pre-close
// brightness, so a dim that outlived the previous daemon is in the standing
// from the first push.
func (c *Controller) Refresh(ctx context.Context) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.loadDimLocked(ctx, "boot")
	c.readLocked(ctx)
}

// Run re-reads the standing every cadence until ctx ends, and on each beat
// holds the hotspot failover to what it read (tick): with the mode on and no
// network joined, the hotspot is joined.
func (c *Controller) Run(ctx context.Context) error {
	clock.Repeat(ctx, c.clock, c.every, c.every, c.tick)
	return nil
}

// readLocked reads, records a CHANGE in either fact's readability, and
// publishes. Caller holds mu.
func (c *Controller) readLocked(ctx context.Context) reading {
	start := c.clock.Now()
	rd := probe(ctx, c.runner, c.cfg.Tools)
	// THE READ IS TIMED: it runs three host tools in sequence, and the boot
	// reads it before serving, so its cost is the boot's (an e2e boot under
	// load spent 1.74s between its reconciliation and serving with nothing said).
	took := c.clock.Now().Sub(start)
	if err := ctx.Err(); err != nil {
		// A read its caller abandoned (the daemon standing down mid-probe)
		// says nothing about either fact: neither its causes nor its standing
		// are recorded or published.
		c.log.Info(opProbe, "the persistent-wifi read was abandoned by its caller; nothing is recorded",
			dlog.Context{"cause": err.Error()})
		return rd
	}
	c.wifiCause = c.recordCause("wifi", c.wifiCause, rd.wifiErr)
	c.modeCause = c.recordCause("mode", c.modeCause, rd.modeErr)
	c.publishLocked(rd.state, dlog.Context{"device": rd.device, "took_ms": took.Milliseconds()})
	return rd
}

// publishLocked stamps the lid facts the controller holds onto a standing
// and publishes it when it differs from the last. Caller holds mu.
func (c *Controller) publishLocked(state *agentreplv1.PersistentWifiState, fields dlog.Context) {
	c.lid.stamp(state)
	state.LidDim = nil
	if c.dim != nil {
		state.LidDim = &agentreplv1.PersistentWifiLidDim{RestoreLevel: *c.dim}
	}
	if c.last != nil && proto.Equal(c.last, state) {
		return
	}
	fields["wifi"] = wifiArm(state)
	fields["mode"] = modeArm(state)
	fields["lid"] = c.lid.name()
	fields["lid_dim"] = state.LidDim != nil
	fields["network_name"] = state.GetJoined().GetNetworkName()
	c.log.Info(opProbe, "the persistent-wifi standing changed", fields)
	c.last = state
	c.topic.Publish(state)
	if c.onChange != nil {
		c.onChange(state)
	}
}

// republishLocked publishes the last standing again with the lid facts as
// they stand now, running no host tool. Before the first read there is
// nothing to republish: that read stamps them. Caller holds mu.
func (c *Controller) republishLocked() {
	if c.last == nil {
		return
	}
	c.publishLocked(proto.Clone(c.last).(*agentreplv1.PersistentWifiState), dlog.Context{})
}

// recordCause logs a fact's read failing (ERROR, once per distinct cause) or
// recovering (INFO), and answers the cause now standing.
func (c *Controller) recordCause(fact, was string, err error) string {
	now := ""
	if err != nil {
		now = err.Error()
	}
	switch {
	case now == was:
	case now != "":
		c.log.Error(opProbe, "a persistent-wifi fact could not be read; it is published as unknown",
			dlog.Context{"fact": fact, "cause": now})
	default:
		c.log.Info(opProbe, "a persistent-wifi fact is readable again", dlog.Context{"fact": fact, "was": was})
	}
	return now
}

// Update runs one UpdatePersistentWifiMode action and answers the response.
// The request was validated by the caller: its action is set.
func (c *Controller) Update(ctx context.Context, req *agentreplv1.UpdatePersistentWifiModeRequest) *agentreplv1.UpdatePersistentWifiModeResponse {
	c.mu.Lock()
	defer c.mu.Unlock()

	before := c.readLocked(ctx)
	var on bool
	switch req.GetAction().(type) {
	case *agentreplv1.UpdatePersistentWifiModeRequest_On:
		on = true
	case *agentreplv1.UpdatePersistentWifiModeRequest_Off:
		on = false
	case *agentreplv1.UpdatePersistentWifiModeRequest_Toggle:
		if before.modeErr != nil {
			c.log.Error(opUpdate, "a toggle could not read the mode it was to turn over; nothing was changed",
				dlog.Context{"action": "toggle", "cause": before.modeErr.Error()})
			return errorResponse(&agentreplv1.UpdatePersistentWifiModeError{
				Cause: &agentreplv1.UpdatePersistentWifiModeError_ModeUnreadable{
					ModeUnreadable: &agentreplv1.UpdatePersistentWifiModeModeUnreadable{Detail: before.modeErr.Error()},
				},
			})
		}
		on = before.state.GetOn() == nil
	default:
		panic(fmt.Sprintf("persistentwifi: Update was handed an unvalidated action %T", req.GetAction()))
	}

	var hotspotOutcome *agentreplv1.UpdatePersistentWifiModeHotspot
	switch {
	case c.cfg.Hotspot == "":
		hotspotOutcome = c.noHotspotConfigured(on)
	case on:
		hotspotOutcome = c.joinHotspot(ctx, before)
	default:
		hotspotOutcome = c.leaveHotspot(ctx, before)
	}
	if failed := hotspotOutcome.GetFailed(); failed != nil && c.cfg.Hotspot != "" {
		c.log.Warn(opUpdate, "the hotspot step did not take; the power step runs regardless",
			dlog.Context{"on": on, "hotspot": failed.GetNetworkName(), "cause": failed.GetDetail()})
	}

	if err := c.applyPower(ctx, on); err != nil {
		c.log.Error(opUpdate, "the power settings change was refused; the mode is as it was",
			dlog.Context{"on": on, "hotspot_outcome": hotspotArm(hotspotOutcome), "cause": err.Error()})
		c.readLocked(ctx)
		return errorResponse(&agentreplv1.UpdatePersistentWifiModeError{
			Cause: &agentreplv1.UpdatePersistentWifiModeError_PowerSettingsRefused{
				PowerSettingsRefused: &agentreplv1.UpdatePersistentWifiModePowerSettingsRefused{Detail: err.Error()},
			},
		})
	}

	energy := c.setEnergyMode(ctx, on)
	if failed := energy.GetFailed(); failed != nil {
		c.log.Warn(opUpdate, "the energy mode step failed; the mode change stands",
			dlog.Context{"on": on, "cause": failed.GetDetail()})
	}

	// THE DISPLAY FOLLOWS THE LID: the step moves it only where this change
	// does (moveDisplayLocked), and records its own failures.
	mode := modeIsOff
	if on {
		mode = modeIsOn
	}
	display := c.moveDisplayLocked(ctx, mode, "update")

	after := c.readLocked(ctx)
	c.log.Info(opUpdate, "persistent wifi mode was changed", dlog.Context{
		"on": on, "hotspot_outcome": hotspotArm(hotspotOutcome),
		"energy_mode_outcome": energyModeArm(energy), "display_outcome": displayArm(display),
		"wifi": wifiArm(after.state), "mode": modeArm(after.state),
	})
	return &agentreplv1.UpdatePersistentWifiModeResponse{
		Result: &agentreplv1.UpdatePersistentWifiModeResponse_Success{
			Success: &agentreplv1.UpdatePersistentWifiModeSuccess{
				State: after.state, Hotspot: hotspotOutcome, Display: display, EnergyMode: energy,
			},
		},
	}
}

// errorResponse wraps an error arm.
func errorResponse(e *agentreplv1.UpdatePersistentWifiModeError) *agentreplv1.UpdatePersistentWifiModeResponse {
	return &agentreplv1.UpdatePersistentWifiModeResponse{
		Result: &agentreplv1.UpdatePersistentWifiModeResponse_Error{Error: e},
	}
}

// wifiArm, modeArm, hotspotArm, energyModeArm and displayArm name a oneof's set arm for a
// record; "unknown" is an unassigned one.
func wifiArm(s *agentreplv1.PersistentWifiState) string {
	switch {
	case s.GetJoined() != nil:
		return "joined"
	case s.GetNotJoined() != nil:
		return "not_joined"
	}
	return "unknown"
}

func modeArm(s *agentreplv1.PersistentWifiState) string {
	switch {
	case s.GetOn() != nil:
		return "on"
	case s.GetOff() != nil:
		return "off"
	}
	return "unknown"
}

func hotspotArm(h *agentreplv1.UpdatePersistentWifiModeHotspot) string { return outcomeArm(h) }

func energyModeArm(e *agentreplv1.UpdatePersistentWifiModeEnergyMode) string { return outcomeArm(e) }

func displayArm(d *agentreplv1.UpdatePersistentWifiModeDisplay) string { return outcomeArm(d) }

// outcomeArm names the set arm of m's `outcome` oneof. Every step outcome the
// controller builds sets one, so an unset or absent oneof is a bug in this
// package and panics rather than being named.
func outcomeArm(m proto.Message) string {
	return string(protoarm.Must(m.ProtoReflect(), "outcome", "persistentwifi.outcome_arm"))
}

const opLid = "daemon.persistentwifi.lid"

// WatchLid hears the lid until ctx ends, moving the display on each change.
// A watch that cannot subscribe, or that ends while ctx is live, leaves the
// lid UNWATCHED: recorded at ERROR and published in the standing, with no
// polling in its place. It answers nil either way, since the rest of the
// daemon runs on without the lid.
func (c *Controller) WatchLid(ctx context.Context) error {
	err := c.lidWatch.Watch(ctx, func(l Lid) { c.lidHeard(ctx, heardLid(l)) })
	if ctx.Err() != nil {
		return nil
	}
	if err == nil {
		err = errors.New("the lid watch ended on its own")
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.log.Error(opLid, "the lid is not heard; the display no longer follows it until the daemon restarts",
		dlog.Context{"was": c.lid.name(), "cause": err.Error()})
	c.lid = lidFact{kind: lidUnwatched, cause: err.Error()}
	c.republishLocked()
	return nil
}

// lidHeard takes one lid state: the one read at subscription or a change. It
// reads the standing afresh (so the display decision reads the mode as it is
// now, not as the last beat left it), then moves the display. A state already
// held is taken again idempotently: the decision depends on states, never on
// transitions.
func (c *Controller) lidHeard(ctx context.Context, lid lidFact) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if lid != c.lid {
		c.log.Info(opLid, "the lid changed", dlog.Context{"was": c.lid.name(), "lid": lid.name()})
	}
	c.lid = lid
	rd := c.readLocked(ctx)
	if ctx.Err() != nil {
		return
	}
	display := c.moveDisplayLocked(ctx, modeOf(rd.state), "lid_"+lid.name())
	if display.GetUnchanged() == nil {
		c.republishLocked()
	}
}
