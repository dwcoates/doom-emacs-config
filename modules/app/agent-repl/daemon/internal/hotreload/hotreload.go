// Package hotreload draws A DEPLOY'S HOT RELOAD (frontend.v1.FeedHotReload;
// owner request, 2026-11-20): one card per runtime deploy, in the root feed of
// the deploy mediator of the repository the runtime is deployed from, telling
// every service the deploy builds and puts back into service as it does it.
//
// THE TRACKER IS THE CARD'S ONE AUTHOR. The deploy, the rollout controller and
// the server each tell it what happened (the build's steps, the install, each
// restart, each shim's judgement, each web view's return); it keeps the card's
// model, resolves every word, count and state from it, and publishes the card
// whole. No caller composes any part of the card.
//
// THE CARD CROSSES A HANDOVER. A deploy that moves the daemon hands the model
// to its successor in the rollout manifest (Handoff, Resume). Exactly one
// daemon publishes it at a time: the incumbent until it hands the card off
// (or its handover fails, when the card is its own again), the successor from
// the moment it takes over. The boot claim makes the two exclusive: the
// successor takes over only once the incumbent has exited.
package hotreload

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// opHotReload is the operation every record of the card is made under.
const opHotReload = "daemon.hotreload"

// The services a deploy reloads, in the order the card lists them.
const (
	ServiceStore   = "store"
	ServiceSidecar = "sidecar"
	ServiceZettel  = "zettel"
	ServiceTests   = "test service"
	ServiceDaemon  = "daemon"
	ServiceShim    = "shim"
	ServiceWebapp  = "webapp"
	ServiceElisp   = "elisp"
)

// The step every service shares, before any service's own.
const stepProtobufs = "protobufs"

// The service steps.
const (
	stepBuild   = "build"
	stepInstall = "install"
)

// serviceSpec is what the card says of one service before the deploy acts on
// it: what reloading it means, how it is put into service, and how its
// instances are counted.
type serviceSpec struct {
	name, about string
	// reload is the name of the step that puts the service into service.
	reload string
	// built reports whether the deploy builds and installs it; elisp is read
	// from the checkout and pushed, never built.
	built bool
	// counted is the verb an instance summary uses ("replaced"); empty for a
	// service with no instances.
	counted string
	// current is what the reload step says when no instance needed it.
	current string
}

// specs are every service, in card order.
var specs = []serviceSpec{
	{name: ServiceStore, reload: "restart", built: true, current: "already running the fresh build",
		about: "Keeps every conversation's records. A stale store is restarted at once, and the sidecar with it, since nothing holds a turn in it."},
	{name: ServiceSidecar, reload: "restart", built: true, current: "already running the fresh build",
		about: "Follows each Claude session's transcript into the store. A stale sidecar is restarted at once."},
	{name: ServiceZettel, reload: "restart", built: true, current: "already running the fresh build",
		about: "Serves the zettel chats. A stale one is restarted at once; nothing else depends on it."},
	{name: ServiceTests, reload: "restart", built: true, current: "already running the fresh build",
		about: "Runs test suites for the merge queue and the workspaces. A stale one is restarted at once."},
	{name: ServiceDaemon, reload: "hand over", built: true, current: "already running the fresh build",
		about: "Serves every workspace. A stale daemon starts its fresh build beside itself and hands each workspace over when it is free, so no turn is cut."},
	{name: ServiceShim, reload: "replace", built: true, counted: "replaced", current: "every shim runs the fresh build",
		about: "Hosts each workspace's Claude session. A stale shim is replaced at once when its workspace is free, and after its turn otherwise."},
	{name: ServiceWebapp, reload: "reload", built: true, counted: "reloaded", current: "every web view runs the fresh build",
		about: "Draws every workspace's feed. Each web view on an older build is told to reload, and is loaded once it reconnects on the fresh build."},
	{name: ServiceElisp, reload: "reload", counted: "", current: "every Emacs runs the fresh elisp",
		about: "The editor's side of agent-repl. Each Emacs on older elisp is pushed a reload, which it loads itself, once the daemon serving it is the fresh build."},
}

// buildTargets map each build-frontend target to the service whose build it
// is, and the step it draws as there.
var buildTargets = map[string]struct{ service, step string }{
	"shim":    {ServiceShim, stepBuild},
	"lock":    {ServiceShim, "build shim-lock"},
	"webapp":  {ServiceWebapp, stepBuild},
	"daemon":  {ServiceDaemon, stepBuild},
	"wifi":    {ServiceDaemon, "build Wi-Fi helper"},
	"store":   {ServiceStore, stepBuild},
	"sidecar": {ServiceSidecar, stepBuild},
	"zettel":  {ServiceZettel, stepBuild},
	"tests":   {ServiceTests, stepBuild},
}

// buildEverything is the one step a builder override runs in place of every
// target: it builds every service at once.
const buildEverything = "build"

// Publisher draws the card's row. The feed resolver is the production one.
type Publisher interface {
	UpsertDurable(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
}

// Deps are a Tracker's collaborators.
type Deps struct {
	// Feed draws the card.
	Feed Publisher
	// Mediator answers the deploy mediator the card is drawn for, and whether
	// the repository has one.
	Mediator func(ctx context.Context) (ids.WorkspaceID, bool, error)
	// WorkspaceName answers a workspace's name, which an instance is drawn as.
	WorkspaceName func(ctx context.Context, ws ids.WorkspaceID) (string, error)
	// Now is the tracker's clock.
	Now func() time.Time
	Log dlog.Logger
}

// Tracker keeps the card of the deploy being reloaded.
type Tracker struct {
	deps Deps

	mu sync.Mutex
	// card is the deploy's card; nil while no deploy is tracked.
	card *Card
	// publishing reports whether this daemon owns the card's row: false
	// between a Handoff and a failed handover, and in a successor until it
	// takes over.
	publishing bool
	// settled reports whether the card's last publication was settled, so a
	// late report on a settled card is not drawn.
	settled bool
}

// New builds a Tracker.
func New(deps Deps) (*Tracker, error) {
	switch {
	case deps.Feed == nil:
		return nil, errors.New("hotreload: a feed to draw the card in is required")
	case deps.Mediator == nil:
		return nil, errors.New("hotreload: the deploy mediator lookup is required")
	case deps.WorkspaceName == nil:
		return nil, errors.New("hotreload: the workspace name lookup is required")
	case deps.Now == nil:
		return nil, errors.New("hotreload: a clock is required")
	case deps.Log == nil:
		return nil, errors.New("hotreload: a logger is required")
	}
	return &Tracker{deps: deps}, nil
}

// ---- The model ----

// Kind is where a service, a step or an instance stands.
type Kind string

// The kinds, each one arm of frontend.v1.FeedHotReloadState.
const (
	KindScheduled Kind = "scheduled"
	KindRunning   Kind = "running"
	KindWaiting   Kind = "waiting"
	KindDone      Kind = "done"
	KindUnchanged Kind = "unchanged"
	KindFailed    Kind = "failed"
	KindSkipped   Kind = "skipped"
)

// terminal reports whether K is an end: nothing more happens to it.
func (k Kind) terminal() bool {
	switch k {
	case KindDone, KindUnchanged, KindFailed, KindSkipped:
		return true
	default:
		return false
	}
}

// State is a kind with its words and its span, epoch ms.
type State struct {
	Kind      Kind   `json:"kind"`
	Words     string `json:"words"`
	StartedMS int64  `json:"started_ms,omitempty"`
	EndedMS   int64  `json:"ended_ms,omitempty"`
}

// Step is one step of the deploy or of a service.
type Step struct {
	Name  string `json:"name"`
	State State  `json:"state"`
}

// Instance is one running copy of a service the deploy acts on.
type Instance struct {
	// Key identifies it within its service: a workspace id, or a fixed key.
	Key   string `json:"key"`
	Name  string `json:"name"`
	State State  `json:"state"`
}

// Service is one service's model.
type Service struct {
	Name      string     `json:"name"`
	Previous  string     `json:"previous,omitempty"`
	Fresh     string     `json:"fresh,omitempty"`
	Steps     []Step     `json:"steps"`
	Instances []Instance `json:"instances,omitempty"`
	// Judged reports that the deploy has decided every instance it acts on,
	// so the reload step is read off the instances from then on.
	Judged bool `json:"judged,omitempty"`
	// replaced reports that the install replaced one of its artifacts.
	Replaced bool `json:"replaced,omitempty"`
}

// Card is a deploy's card: the mediator it is drawn for, the deploy's start,
// and its model.
type Card struct {
	Workspace ids.WorkspaceID `json:"workspace"`
	StartedMS int64           `json:"started_ms"`
	Steps     []Step          `json:"steps"`
	Services  []*Service      `json:"services"`
	// Failure is the deploy's one-line account when it failed.
	Failure string `json:"failure,omitempty"`
}

// spec answers the spec of service NAME; an unknown name is a caller's bug.
func spec(name string) serviceSpec {
	for _, s := range specs {
		if s.name == name {
			return s
		}
	}
	panic(fmt.Sprintf("hotreload: %q is no service a deploy reloads", name))
}

// newCard is a fresh card for a deploy that began at STARTED, drawn for WS:
// every step scheduled.
func newCard(ws ids.WorkspaceID, started time.Time) *Card {
	c := &Card{Workspace: ws, StartedMS: started.UnixMilli(),
		Steps: []Step{{Name: stepProtobufs, State: State{Kind: KindScheduled, Words: "scheduled"}}}}
	for _, s := range specs {
		svc := &Service{Name: s.name}
		if s.built {
			svc.Steps = append(svc.Steps,
				Step{Name: stepBuild, State: scheduled()},
				Step{Name: stepInstall, State: scheduled()})
		}
		svc.Steps = append(svc.Steps, Step{Name: s.reload, State: scheduled()})
		c.Services = append(c.Services, svc)
	}
	return c
}

func scheduled() State { return State{Kind: KindScheduled, Words: "scheduled"} }

// service answers the card's service NAME.
func (c *Card) service(name string) *Service {
	for _, s := range c.Services {
		if s.Name == name {
			return s
		}
	}
	panic(fmt.Sprintf("hotreload: the card has no service %q", name))
}

// step answers SVC's step NAME, or nil.
func (s *Service) step(name string) *Step {
	for i := range s.Steps {
		if s.Steps[i].Name == name {
			return &s.Steps[i]
		}
	}
	return nil
}

// reloadStep answers the step that puts SVC into service: its last.
func (s *Service) reloadStep() *Step { return &s.Steps[len(s.Steps)-1] }

// instance answers SVC's instance KEY, or nil.
func (s *Service) instance(key string) *Instance {
	for i := range s.Instances {
		if s.Instances[i].Key == key {
			return &s.Instances[i]
		}
	}
	return nil
}

// ---- The deploy's reports ----

// Begin starts the card of a deploy that began at STARTED. A card still live
// from an earlier deploy is settled first: what it still waited on is this
// deploy's to carry. A repository with no deploy mediator draws no card.
func (t *Tracker) Begin(ctx context.Context, started time.Time) {
	ws, found, err := t.deps.Mediator(ctx)
	t.mu.Lock()
	defer t.mu.Unlock()
	t.settleEarlierLocked()
	fields := dlog.Context{"deploy_started_ms": started.UnixMilli()}
	switch {
	case err != nil:
		t.deps.Log.Error(opHotReload, "could not read the deploy mediator to draw the hot reload for; this deploy draws no card", dlog.WithCause(fields, err))
		t.card = nil
		return
	case !found:
		t.deps.Log.Info(opHotReload, "the repository has no deploy mediator; this deploy's hot reload draws no card", fields)
		t.card = nil
		return
	}
	t.card, t.publishing, t.settled = newCard(ws, started), true, false
	t.deps.Log.Info(opHotReload, "drawing the deploy's hot reload in the deploy mediator's feed", dlog.Merge(fields, dlog.Context{"workspace": string(ws)}))
	t.publishLocked()
}

// settleEarlierLocked settles a still-live earlier card: every service still
// working is skipped, since the next deploy carries it on. The caller holds
// t.mu.
func (t *Tracker) settleEarlierLocked() {
	if t.card == nil || !t.publishing || t.settled {
		return
	}
	for _, svc := range t.card.Services {
		for i := range svc.Steps {
			skipOpen(&svc.Steps[i].State, "carried on by the next deploy")
		}
		for i := range svc.Instances {
			skipOpen(&svc.Instances[i].State, "carried on by the next deploy")
		}
	}
	t.deps.Log.Info(opHotReload, "a new deploy began while the last one's hot reload still waited; the last card is settled", dlog.Context{"deploy_started_ms": t.card.StartedMS})
	t.publishLocked()
}

// skipOpen skips ST unless it already ended.
func skipOpen(st *State, why string) {
	if !st.Kind.terminal() {
		*st = State{Kind: KindSkipped, Words: why}
	}
}

// BuildStepBegan marks the build target TARGET running.
func (t *Tracker) BuildStepBegan(target string) {
	t.update("build_step_began", func(c *Card, now int64) {
		for _, st := range t.buildStepsLocked(c, target) {
			st.State = State{Kind: KindRunning, Words: "building", StartedMS: now}
		}
	})
}

// BuildStepEnded marks the build target TARGET built, or failed with ERR.
func (t *Tracker) BuildStepEnded(target string, err error) {
	t.update("build_step_ended", func(c *Card, now int64) {
		for _, st := range t.buildStepsLocked(c, target) {
			started := st.State.StartedMS
			if err != nil {
				st.State = State{Kind: KindFailed, Words: "the build failed", StartedMS: started, EndedMS: now}
				continue
			}
			words := "built"
			if st.Name == stepProtobufs {
				words = "generated"
			}
			st.State = State{Kind: KindDone, Words: words, StartedMS: started, EndedMS: now}
		}
	})
}

// buildStepsLocked answers the steps build target TARGET draws as: the
// protobufs step, every service's build for an override's one step, or one
// service's step, made when a target the card did not foresee (the Wi-Fi
// helper, shim-lock) first runs. An unknown target is a caller's bug.
func (t *Tracker) buildStepsLocked(c *Card, target string) []*Step {
	switch target {
	case "proto":
		return []*Step{&c.Steps[0]}
	case buildEverything:
		var out []*Step
		for _, svc := range c.Services {
			if st := svc.step(stepBuild); st != nil {
				out = append(out, st)
			}
		}
		return out
	}
	at, ok := buildTargets[target]
	if !ok {
		panic(fmt.Sprintf("hotreload: %q is no build target a deploy runs", target))
	}
	svc := c.service(at.service)
	if st := svc.step(at.step); st != nil {
		return []*Step{st}
	}
	// THE EXTRA BUILD STEP SITS JUST BEFORE THE INSTALL.
	i := len(svc.Steps) - 2
	svc.Steps = append(svc.Steps[:i], append([]Step{{Name: at.step, State: scheduled()}}, svc.Steps[i:]...)...)
	return []*Step{&svc.Steps[i]}
}

// Built records each service's fresh build, by service name.
func (t *Tracker) Built(fresh map[string]string) {
	t.update("built", func(c *Card, _ int64) {
		for name, build := range fresh {
			c.service(name).Fresh = build
		}
	})
}

// InstallBegan marks every built service's install running.
func (t *Tracker) InstallBegan() {
	t.update("install_began", func(c *Card, now int64) {
		for _, svc := range c.Services {
			if st := svc.step(stepInstall); st != nil {
				st.State = State{Kind: KindRunning, Words: "installing", StartedMS: now}
			}
		}
	})
}

// ArtifactInstalled records one of SERVICE's artifacts: PREVIOUS is the build
// installed before (empty when none could be read), and REPLACED whether the
// install wrote the fresh one. PRIMARY marks the artifact the service's
// builds are named by (the shim bundle, not shim-lock).
func (t *Tracker) ArtifactInstalled(service string, primary bool, previous string, replaced bool) {
	t.update("artifact_installed", func(c *Card, _ int64) {
		svc := c.service(service)
		if primary {
			svc.Previous = previous
		}
		svc.Replaced = svc.Replaced || replaced
	})
}

// InstallEnded ends every running install: replaced where an artifact was
// written, unchanged where every one already stood.
func (t *Tracker) InstallEnded() {
	t.update("install_ended", func(c *Card, now int64) {
		for _, svc := range c.Services {
			st := svc.step(stepInstall)
			if st == nil || st.State.Kind != KindRunning {
				continue
			}
			if svc.Replaced {
				st.State = State{Kind: KindDone, Words: "installed", StartedMS: st.State.StartedMS, EndedMS: now}
			} else {
				st.State = State{Kind: KindUnchanged, Words: "already installed"}
			}
		}
	})
}

// ServiceRestarting marks SERVICE's restart running.
func (t *Tracker) ServiceRestarting(service string) {
	t.update("service_restarting", func(c *Card, now int64) {
		c.service(service).reloadStep().State = State{Kind: KindRunning, Words: "restarting", StartedMS: now}
	})
}

// ServiceRestarted marks SERVICE restarted onto the fresh build.
func (t *Tracker) ServiceRestarted(service string) {
	t.update("service_restarted", func(c *Card, now int64) {
		st := c.service(service).reloadStep()
		st.State = State{Kind: KindDone, Words: "restarted", StartedMS: startOr(st.State, now), EndedMS: now}
	})
}

// ServiceCurrent marks SERVICE as already running the fresh build.
func (t *Tracker) ServiceCurrent(service string) {
	t.update("service_current", func(c *Card, _ int64) {
		c.service(service).reloadStep().State = State{Kind: KindUnchanged, Words: spec(service).current}
	})
}

// startOr answers ST's start, or NOW for a state never started.
func startOr(st State, now int64) int64 {
	if st.StartedMS != 0 {
		return st.StartedMS
	}
	return now
}

// DaemonCurrent marks the daemon as already the fresh build.
func (t *Tracker) DaemonCurrent() { t.ServiceCurrent(ServiceDaemon) }

// DaemonMoving marks the daemon moving to its fresh build with WORDS
// ("handing over 6 workspaces"), and every service only its successor can
// finish as waiting for it. STALE WEBVIEWS are the workspaces whose web views
// run an older webapp: each reloads onto the fresh build as it attaches to
// the successor.
func (t *Tracker) DaemonMoving(ctx context.Context, words string, staleWebviews []ids.WorkspaceID) {
	names := t.names(ctx, staleWebviews)
	t.update("daemon_moving", func(c *Card, now int64) {
		c.service(ServiceDaemon).reloadStep().State = State{Kind: KindRunning, Words: words, StartedMS: now}
		for _, name := range []string{ServiceShim, ServiceWebapp, ServiceElisp} {
			c.service(name).reloadStep().State = State{Kind: KindWaiting, Words: "waits for the new daemon", StartedMS: now}
		}
		webapp := c.service(ServiceWebapp)
		for i, ws := range staleWebviews {
			webapp.Instances = append(webapp.Instances, Instance{Key: string(ws), Name: names[i],
				State: State{Kind: KindWaiting, Words: "reloads as it reattaches", StartedMS: now}})
		}
		// EVERY WEB VIEW IS DECIDED HERE: one on the fresh build already
		// needs nothing, and the rest reload as they reattach.
		webapp.Judged = true
	})
}

// Handoff answers the card for the successor to go on drawing, and stops this
// daemon publishing it. Nil when no card is tracked.
func (t *Tracker) Handoff() []byte {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.card == nil {
		return nil
	}
	encoded, err := json.Marshal(t.card)
	if err != nil {
		// A CARD THIS PACKAGE BUILT ALWAYS ENCODES: its fields are strings
		// and integers.
		panic(fmt.Sprintf("hotreload: the card could not be encoded: %v", err))
	}
	t.publishing = false
	t.deps.Log.Info(opHotReload, "handed the hot reload's card to the successor daemon", dlog.Context{"workspace": string(t.card.Workspace)})
	return encoded
}

// Failed ends the card on a deploy that failed with ERR: the step at work
// fails with the deploy's account, and everything not yet done is skipped.
func (t *Tracker) Failed(err error) {
	t.update("failed", func(c *Card, now int64) {
		c.Failure = err.Error()
		failOpen(c, now)
	})
}

// failOpen fails every running or waiting step and instance of C and skips
// every scheduled one.
func failOpen(c *Card, now int64) {
	end := func(st *State) {
		switch st.Kind {
		case KindRunning, KindWaiting:
			*st = State{Kind: KindFailed, Words: "the deploy failed", StartedMS: st.StartedMS, EndedMS: now}
		case KindScheduled:
			*st = State{Kind: KindSkipped, Words: "the deploy stopped first"}
		}
	}
	for i := range c.Steps {
		end(&c.Steps[i].State)
	}
	for _, svc := range c.Services {
		for i := range svc.Steps {
			end(&svc.Steps[i].State)
		}
		for i := range svc.Instances {
			end(&svc.Instances[i].State)
		}
	}
}

// ---- Shims ----

// ShimOutcome is what one staleness judgement of a workspace's shim decided.
type ShimOutcome int

// The outcomes.
const (
	// ShimCurrent: the shim runs the installed build.
	ShimCurrent ShimOutcome = iota + 1
	// ShimReplacing: the stale shim is being replaced now.
	ShimReplacing
	// ShimWaitsForTurn: the stale shim is replaced once its turn ends.
	ShimWaitsForTurn
	// ShimWaitsForWork: the stale shim is replaced once its detached work
	// ends.
	ShimWaitsForWork
	// ShimQueued: the stale shim's replacement is already registered or
	// running for another reason, and replaces it too.
	ShimQueued
	// ShimStillStale: the shim was replaced and still runs the old build.
	ShimStillStale
)

// ShimJudged records a judgement of WS's shim. A judgement made while no
// deploy's shims are being reloaded is no part of any card and draws nothing.
func (t *Tracker) ShimJudged(ctx context.Context, ws ids.WorkspaceID, outcome ShimOutcome) {
	name := ""
	if outcome != ShimCurrent {
		name = t.names(ctx, []ids.WorkspaceID{ws})[0]
	}
	t.update("shim_judged", func(c *Card, now int64) {
		svc := c.service(ServiceShim)
		if svc.reloadStep().State.Kind == KindScheduled || svc.reloadStep().State.Kind == KindSkipped {
			return
		}
		in := svc.instance(string(ws))
		if outcome == ShimCurrent {
			if in != nil && !in.State.Kind.terminal() {
				in.State = State{Kind: KindDone, Words: "replaced", StartedMS: startOr(in.State, now), EndedMS: now}
			}
			return
		}
		if in == nil {
			svc.Instances = append(svc.Instances, Instance{Key: string(ws), Name: name})
			in = &svc.Instances[len(svc.Instances)-1]
		}
		started := startOr(in.State, now)
		switch outcome {
		case ShimReplacing:
			in.State = State{Kind: KindRunning, Words: "replacing", StartedMS: started}
		case ShimWaitsForTurn:
			in.State = State{Kind: KindWaiting, Words: "waits for its turn to end", StartedMS: started}
		case ShimWaitsForWork:
			in.State = State{Kind: KindWaiting, Words: "waits for its detached work to end", StartedMS: started}
		case ShimQueued:
			in.State = State{Kind: KindWaiting, Words: "waits for a replacement already under way", StartedMS: started}
		case ShimStillStale:
			in.State = State{Kind: KindFailed, Words: "still runs the old build after it was replaced", StartedMS: started, EndedMS: now}
		default:
			panic(fmt.Sprintf("hotreload: %d is no shim outcome", outcome))
		}
	})
}

// ShimReplaceFailed records that WS's shim could not be replaced.
func (t *Tracker) ShimReplaceFailed(ws ids.WorkspaceID, err error) {
	t.update("shim_replace_failed", func(c *Card, now int64) {
		in := c.service(ServiceShim).instance(string(ws))
		if in == nil || in.State.Kind.terminal() {
			return
		}
		in.State = State{Kind: KindFailed, Words: "could not be replaced: " + err.Error(), StartedMS: startOr(in.State, now), EndedMS: now}
	})
}

// ShimsJudging marks the shims' replacement begun: the judgements that
// follow are this deploy's.
func (t *Tracker) ShimsJudging() {
	t.update("shims_judging", func(c *Card, now int64) {
		st := c.service(ServiceShim).reloadStep()
		if st.State.Kind == KindScheduled {
			st.State = State{Kind: KindRunning, Words: "judging every shim", StartedMS: now}
		}
	})
}

// ShimsJudged marks every live shim judged: the shim's reload step is read off
// its instances from now on.
func (t *Tracker) ShimsJudged() {
	t.update("shims_judged", func(c *Card, _ int64) { c.service(ServiceShim).Judged = true })
}

// ---- The webapp ----

// WebviewReloading records that WS's web view was told to reload.
func (t *Tracker) WebviewReloading(ctx context.Context, ws ids.WorkspaceID) {
	name := t.names(ctx, []ids.WorkspaceID{ws})[0]
	t.update("webview_reloading", func(c *Card, now int64) {
		svc := c.service(ServiceWebapp)
		if svc.instance(string(ws)) == nil {
			svc.Instances = append(svc.Instances, Instance{Key: string(ws), Name: name})
		}
		svc.instance(string(ws)).State = State{Kind: KindRunning, Words: "reloading", StartedMS: now}
	})
}

// webhostKey is the web view host's instance key: no workspace's.
const webhostKey = "webhost"

// WebhostReloading records that the web view host was told to reload.
func (t *Tracker) WebhostReloading() {
	t.update("webhost_reloading", func(c *Card, now int64) {
		svc := c.service(ServiceWebapp)
		if svc.instance(webhostKey) == nil {
			svc.Instances = append(svc.Instances, Instance{Key: webhostKey, Name: "the web view host"})
		}
		svc.instance(webhostKey).State = State{Kind: KindRunning, Words: "reloading", StartedMS: now}
	})
}

// WebappJudged marks every web view decided.
func (t *Tracker) WebappJudged() {
	t.update("webapp_judged", func(c *Card, now int64) {
		svc := c.service(ServiceWebapp)
		svc.Judged = true
		if st := svc.reloadStep(); st.State.Kind == KindScheduled {
			st.State.StartedMS = now
		}
	})
}

// WebviewReported records that a web view of WS attached on BUILD.
func (t *Tracker) WebviewReported(ws ids.WorkspaceID, build string) {
	t.instanceReported("webview_reported", string(ws), build)
}

// WebhostReported records that a web view host page attached on BUILD.
func (t *Tracker) WebhostReported(build string) {
	t.instanceReported("webhost_reported", webhostKey, build)
}

// instanceReported marks the webapp instance KEY reloaded when BUILD is the
// fresh build.
func (t *Tracker) instanceReported(op, key, build string) {
	t.update(op, func(c *Card, now int64) {
		svc := c.service(ServiceWebapp)
		in := svc.instance(key)
		if in == nil || in.State.Kind.terminal() || build != svc.Fresh {
			return
		}
		in.State = State{Kind: KindDone, Words: "reloaded", StartedMS: startOr(in.State, now), EndedMS: now}
	})
}

// WebviewGone records that WS has no web view open any more: one still
// waiting to reload never will, and is skipped.
func (t *Tracker) WebviewGone(ws ids.WorkspaceID) {
	t.update("webview_gone", func(c *Card, _ int64) {
		in := c.service(ServiceWebapp).instance(string(ws))
		if in != nil && in.State.Kind == KindWaiting {
			in.State = State{Kind: KindSkipped, Words: "its web view closed"}
		}
	})
}

// ---- Elisp ----

// ElispPushed records the reload pushed to RECIPIENTS Emacs.
func (t *Tracker) ElispPushed(recipients int) {
	t.update("elisp_pushed", func(c *Card, now int64) {
		st := c.service(ServiceElisp).reloadStep()
		st.State = State{Kind: KindDone, Words: fmt.Sprintf("pushed to %d Emacs, which loads it itself", recipients),
			StartedMS: startOr(st.State, now), EndedMS: now}
	})
}

// ElispCurrent marks every Emacs as already on the fresh elisp.
func (t *Tracker) ElispCurrent() { t.ServiceCurrent(ServiceElisp) }

// ---- The handover ----

// Resume takes the card a deploy's handover passed to this successor. It is
// not drawn until this daemon takes over (TookOver). An empty handoff is a
// handover whose deploy drew no card.
func (t *Tracker) Resume(handoff []byte) {
	if len(handoff) == 0 {
		return
	}
	var c Card
	if err := json.Unmarshal(handoff, &c); err != nil {
		t.deps.Log.Error(opHotReload, "the handed-over hot reload card could not be read; this deploy's card is not drawn on", dlog.WithCause(nil, err))
		return
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.card != nil && t.card.Workspace == c.Workspace && t.card.StartedMS == c.StartedMS {
		t.deps.Log.Debug(opHotReload, "the handed-over hot reload card is the one already held", dlog.Context{"workspace": string(c.Workspace)})
		return
	}
	t.card, t.publishing, t.settled = &c, false, false
	t.deps.Log.Info(opHotReload, "took the hot reload's card from the daemon handing over; drawn once this daemon takes over", dlog.Context{"workspace": string(c.Workspace)})
}

// TookOver marks the daemon moved onto its fresh build and draws the card
// from this daemon on.
func (t *Tracker) TookOver() {
	t.mu.Lock()
	if t.card == nil {
		t.mu.Unlock()
		return
	}
	t.publishing = true
	t.mu.Unlock()
	t.update("took_over", func(c *Card, now int64) {
		st := c.service(ServiceDaemon).reloadStep()
		st.State = State{Kind: KindDone, Words: "handed over", StartedMS: startOr(st.State, now), EndedMS: now}
		shim := c.service(ServiceShim).reloadStep()
		if shim.State.Kind == KindWaiting {
			shim.State = State{Kind: KindRunning, Words: "judging every shim", StartedMS: now}
		}
	})
}

// HandoverFailed ends the card of a deploy whose handover cannot finish: the
// card is this daemon's again, and it fails with WHY.
func (t *Tracker) HandoverFailed(why string) {
	t.mu.Lock()
	if t.card == nil {
		t.mu.Unlock()
		return
	}
	t.publishing = true
	t.mu.Unlock()
	t.update("handover_failed", func(c *Card, now int64) {
		c.Failure = why
		failOpen(c, now)
	})
}

// ---- Updating and publishing ----

// update applies FN to the tracked card and publishes it. With no card, or
// one settled already, nothing is done.
func (t *Tracker) update(op string, fn func(c *Card, now int64)) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.card == nil || t.settled {
		return
	}
	fn(t.card, t.nowMS())
	t.deps.Log.Debug(opHotReload, "the hot reload moved", dlog.Context{"event": op, "publishing": t.publishing})
	if t.publishing {
		t.publishLocked()
	}
}

// names answers each workspace's name. A name that cannot be read is recorded
// and the workspace is drawn by its id rather than not drawn.
func (t *Tracker) names(ctx context.Context, wss []ids.WorkspaceID) []string {
	out := make([]string, len(wss))
	for i, ws := range wss {
		name, err := t.deps.WorkspaceName(ctx, ws)
		if err != nil {
			t.deps.Log.Error(opHotReload, "could not read a workspace's name for the hot reload card", dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
			name = string(ws)
		}
		out[i] = name
	}
	return out
}

func (t *Tracker) nowMS() int64 { return t.deps.Now().UnixMilli() }

// publishLocked draws the card. The caller holds t.mu.
func (t *Tracker) publishLocked() {
	card := Resolve(t.card)
	if _, settled := card.State.(*frontendv1.FeedHotReload_Settled); settled {
		t.settled = true
		t.deps.Log.Info(opHotReload, "the deploy's hot reload settled", dlog.Context{"workspace": string(t.card.Workspace), "failure": t.card.Failure})
	}
	t.deps.Feed.UpsertDurable(t.card.Workspace, feedid.Feed{Root: true}, Row(t.card.Workspace, t.card.StartedMS, card))
}

// Row is a deploy's card as its feed row, keyed by the deploy's start.
func Row(ws ids.WorkspaceID, startedMS int64, card *frontendv1.FeedHotReload) *frontendv1.FeedRow {
	return feedid.RootCardRow(ws,
		feedid.RowKey{Kind: feedid.KindHotReload, ID: strconv.FormatInt(startedMS, 10)},
		&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_HotReload{HotReload: card}})
}
