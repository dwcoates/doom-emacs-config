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

	"agent-repld/internal/appfreeze"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/publish"
	"agent-repld/internal/viewquiet"
)

// Operations every record of this package carries.
const (
	opProbe  = "daemon.persistentwifi.probe"
	opUpdate = "daemon.persistentwifi.update"
)

// Deps are the controller's collaborators.
type Deps struct {
	// Config names the tools and the hotspot.
	Config Config
	// Runner runs every host tool. REQUIRED.
	Runner Runner
	// Clock stamps the reads and paces the hotspot failover's retries.
	// REQUIRED.
	Clock clock.Clock
	// NetworkChanged answers a channel closed at the host's next network
	// change (netwatch.Watcher.Changed): the Wi-Fi link is re-read on it.
	// REQUIRED.
	NetworkChanged func() <-chan struct{}
	// PowerChanges signals the power settings changing (Config.PowerChanges):
	// the mode is re-read on it. REQUIRED.
	PowerChanges kernelevent.Source
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
	// LegacyLidDims is a lid dim an earlier build kept, undone once at boot.
	// REQUIRED.
	LegacyLidDims LegacyLidDims
	// Freezer freezes the configured applications for the quiet state.
	// REQUIRED.
	Freezer AppFreezer
	// Views is the view gate the quiet state moves. REQUIRED.
	Views *viewquiet.Gate
	// Telemetry is the quiet state's gauge; nil records nothing.
	Telemetry QuietTelemetry
}

// Controller reads, publishes and changes the persistent-wifi standing. One
// lock serializes every read and every change, so a toggle reads the mode it
// turns over under the same lock as the turn.
type Controller struct {
	cfg       Config
	runner    Runner
	clock     clock.Clock
	network   func() <-chan struct{}
	power     kernelevent.Source
	exists    func(string) bool
	onChange  func(*agentreplv1.PersistentWifiState)
	log       dlog.Logger
	lidWatch  LidWatcher
	legacy    LegacyLidDims
	freezer   AppFreezer
	views     *viewquiet.Gate
	telemetry QuietTelemetry
	topic     publish.Topic[*agentreplv1.PersistentWifiState]

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
	// energyCause is why the energy mode last could not follow the lid and the
	// power source; empty while nothing stands against it (reconcileEnergyLocked).
	energyCause string
	// failover is the hotspot failover's retry schedule since Wi-Fi was lost;
	// the zero value while the failover stands down (failoverLocked).
	failover failoverSchedule
	// lid is the lid as last heard (WatchLid), or why it is not heard.
	lid lidFact
	// quiet is whether the lid-closed quiet state stands (quiet.go); frozen
	// is what its freeze left frozen, displayAsleep whether its display
	// sleep took, and displayCause why the last sleep failed.
	quiet         bool
	frozen        []appfreeze.Frozen
	displayAsleep bool
	displayCause  string
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
	case d.LegacyLidDims == nil:
		return nil, fmt.Errorf("persistentwifi: the store of an earlier build's lid dim is required")
	case d.Freezer == nil:
		return nil, fmt.Errorf("persistentwifi: an application freezer is required")
	case d.Views == nil:
		return nil, fmt.Errorf("persistentwifi: a view gate is required")
	case d.NetworkChanged == nil:
		return nil, fmt.Errorf("persistentwifi: the network's change signal is required")
	case d.PowerChanges == nil:
		return nil, fmt.Errorf("persistentwifi: the power settings' change signal is required")
	}
	exists := d.Exists
	if exists == nil {
		exists = regularFile
	}
	return &Controller{
		cfg: d.Config, runner: d.Runner, clock: d.Clock,
		network: d.NetworkChanged, power: d.PowerChanges,
		exists: exists, onChange: d.OnChange, log: d.Log,
		lidWatch: d.Lid, legacy: d.LegacyLidDims, freezer: d.Freezer,
		views: d.Views, telemetry: d.Telemetry,
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
// boot never waits on a Wi-Fi join. It also undoes a lid dim an earlier build
// left in force (restoreLegacyDimLocked).
func (c *Controller) Refresh(ctx context.Context) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.restoreLegacyDimLocked(ctx)
	c.readLocked(ctx)
}

// Run re-reads the standing on every change the host reports until ctx ends,
// and on each read holds the hotspot failover to what it read (tick): with the
// mode on and no network joined, the hotspot is joined.
//
// THE WATCHES ARE TAKEN FIRST, then the standing read once, so a change
// between Refresh and the watches is never missed. Between changes nothing
// runs, save the hotspot failover's own retry when one is due: that schedule
// (failoverBackoff) is the failover's design, and its timer is armed only
// while a failed join waits for its next try.
//
// A power-settings watch that fails is recorded at ERROR and the mode is then
// re-read only on network changes and Update; the network keeps being heard.
func (c *Controller) Run(ctx context.Context) error {
	power, stop := c.power(ctx)
	defer stop()
	network := c.network()
	c.tick(ctx)
	for {
		var retry <-chan time.Time
		if wait, ok := c.failoverRetryIn(); ok {
			retry = c.clock.After(wait)
		}
		select {
		case <-ctx.Done():
			return nil
		case <-network:
			network = c.network()
			c.tick(ctx)
		case err := <-power:
			if err != nil {
				c.log.Error(opProbe, "the power settings could not be followed; a mode changed outside agent-repl is heard only on the next network change",
					dlog.Context{"dir": PowerPrefsDir, "cause": err.Error()})
				power = nil
				continue
			}
			c.tick(ctx)
		case <-retry:
			c.tick(ctx)
		}
	}
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
	c.stampQuiet(state)
	if c.last != nil && proto.Equal(c.last, state) {
		return
	}
	fields["wifi"] = wifiArm(state)
	fields["mode"] = modeArm(state)
	fields["lid"] = c.lid.name()
	fields["quiet"] = state.Quiet != nil
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

	// THE QUIET STATE FOLLOWS THE LID: the step moves it only where this
	// change does (holdQuietLocked), and records its own failures.
	mode := modeIsOff
	if on {
		mode = modeIsOn
	}
	display := c.holdQuietLocked(ctx, mode, "update")

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
	c.log.Error(opLid, "the lid is not heard; a closed lid no longer quiets the machine until the daemon restarts",
		dlog.Context{"was": c.lid.name(), "cause": err.Error()})
	c.lid = lidFact{kind: lidUnwatched, cause: err.Error()}
	// A LID NOBODY HEARS IS NEVER CLOSED: a quiet state standing is left, so
	// nothing stays frozen behind a watch that ended.
	c.holdQuietLocked(ctx, modeOf(c.last), "lid_unwatched")
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
	c.holdQuietLocked(ctx, modeOf(rd.state), "lid_"+lid.name())
	c.reconcileEnergyLocked(ctx, rd, "lid_"+lid.name())
	c.republishLocked()
}
