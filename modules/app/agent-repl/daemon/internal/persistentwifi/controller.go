package persistentwifi

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
	"claude-repld/internal/publish"
)

// Operations every record of this package carries.
const (
	opProbe  = "daemon.persistentwifi.probe"
	opUpdate = "daemon.persistentwifi.update"
)

// DefaultEvery is how often the standing is re-read. It bounds how long a
// change made OUTSIDE agent-repl (a network dropping, `pmset` run by hand)
// takes to reach the topbar and Emacs; a change made through Update is
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
	topic    publish.Topic[*agentreplv1.PersistentWifiState]

	mu sync.Mutex
	// last is the standing last published; nil before the first read.
	last *agentreplv1.PersistentWifiState
	// wifiCause and modeCause are the last read failure recorded for each
	// fact, so a failure that persists is recorded once rather than on every
	// re-read.
	wifiCause, modeCause string
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
	case d.Every < 0:
		return nil, fmt.Errorf("persistentwifi: the re-read cadence must not be negative, got %s", d.Every)
	case d.Config.Hotspot == "":
		return nil, fmt.Errorf("persistentwifi: a hotspot name is required")
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
// standing nobody read.
func (c *Controller) Refresh(ctx context.Context) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.readLocked(ctx)
}

// Run re-reads the standing every cadence until ctx ends.
func (c *Controller) Run(ctx context.Context) error {
	for {
		select {
		case <-ctx.Done():
			return nil
		case <-c.clock.After(c.every):
			c.Refresh(ctx)
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
	if c.last == nil || !proto.Equal(c.last, rd.state) {
		c.log.Info(opProbe, "the persistent-wifi standing changed", dlog.Context{
			"wifi": wifiArm(rd.state), "mode": modeArm(rd.state),
			"network_name": rd.state.GetJoined().GetNetworkName(), "device": rd.device,
			"took_ms": took.Milliseconds(),
		})
		c.last = rd.state
		c.topic.Publish(rd.state)
		if c.onChange != nil {
			c.onChange(rd.state)
		}
	}
	return rd
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

	hotspot := c.leaveHotspot
	if on {
		hotspot = c.joinHotspot
	}
	hotspotOutcome := hotspot(ctx, before)
	if failed := hotspotOutcome.GetFailed(); failed != nil {
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

	display := c.setDisplay(ctx, on)
	switch {
	case display.GetFailed() != nil:
		c.log.Warn(opUpdate, "the display step failed; the mode change stands",
			dlog.Context{"on": on, "cause": display.GetFailed().GetDetail()})
	case display.GetToolMissing() != nil:
		c.log.Warn(opUpdate, "the brightness tool is not installed; the display step was skipped",
			dlog.Context{"on": on, "tool": display.GetToolMissing().GetToolPath()})
	}

	after := c.readLocked(ctx)
	c.log.Info(opUpdate, "persistent wifi mode was changed", dlog.Context{
		"on": on, "hotspot_outcome": hotspotArm(hotspotOutcome), "display_outcome": displayArm(display),
		"wifi": wifiArm(after.state), "mode": modeArm(after.state),
	})
	return &agentreplv1.UpdatePersistentWifiModeResponse{
		Result: &agentreplv1.UpdatePersistentWifiModeResponse_Success{
			Success: &agentreplv1.UpdatePersistentWifiModeSuccess{State: after.state, Hotspot: hotspotOutcome, Display: display},
		},
	}
}

// errorResponse wraps an error arm.
func errorResponse(e *agentreplv1.UpdatePersistentWifiModeError) *agentreplv1.UpdatePersistentWifiModeResponse {
	return &agentreplv1.UpdatePersistentWifiModeResponse{
		Result: &agentreplv1.UpdatePersistentWifiModeResponse_Error{Error: e},
	}
}

// wifiArm, modeArm, hotspotArm and displayArm name a oneof's set arm for a
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

func displayArm(d *agentreplv1.UpdatePersistentWifiModeDisplay) string { return outcomeArm(d) }

// outcomeArm names the set arm of m's `outcome` oneof. Every step outcome the
// controller builds sets one, so an unset or absent oneof is a bug in this
// package and panics rather than being named.
func outcomeArm(m proto.Message) string {
	r := m.ProtoReflect()
	oneof := r.Descriptor().Oneofs().ByName("outcome")
	if oneof == nil {
		panic(fmt.Sprintf("persistentwifi: %s has no outcome oneof", r.Descriptor().FullName()))
	}
	field := r.WhichOneof(oneof)
	if field == nil {
		panic(fmt.Sprintf("persistentwifi: %s was built with no outcome", r.Descriptor().FullName()))
	}
	return string(field.Name())
}
