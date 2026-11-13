package deploy

import (
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/buildid"
	"agent-repld/internal/dlog"
)

// THE WEB VIEW HOST IS RELOADED ONTO THE INSTALLED WEBAPP ON THE EDGES THAT
// CAN BRING IT THERE, not once at a deploy's decision. A deploy that hands
// this daemon over defers the webapp to its successor, and nothing used to
// keep that promise for the host: no handover reloads it, so the host ran its
// old bundle on until some later deploy found the daemon already fresh, and
// read the new daemon's frames as undecodable (2026-10-24). A host's reload
// needs two things to stand together, the host on its page stream and an
// Emacs on its daemon stream, and they arrive in either order, so the
// reconcile runs on each: a host attaching, an Emacs attaching, and a deploy.

// The edges a web view host is reconciled on.
const (
	WebhostEdgeHostAttached  = "host_attached"
	WebhostEdgeEmacsAttached = "emacs_attached"
	WebhostEdgeDeploy        = "deploy"
)

const opWebhost = "daemon.deploy.webhost"

// webhostReload is what the reconcile last pushed: the installed webapp build
// a host was told to reload onto, and the highest reload generation a stale
// host stood at when it was (nil when none stated one). Guarded by its own
// mutex, apart from the deploy slot's.
type webhostReload struct {
	mu        sync.Mutex
	pushed    string
	pushedFor *uint32
}

// ReconcileWebhost pushes the web view host's reload when a standing host
// runs a webapp other than the installed one, answering how many Emacs
// streams took it. A reload is pushed ONCE per installed build.
//
// A HOST STILL STALE AFTER THE PUSH IS JUDGED BY ITS RELOAD GENERATION
// (agentrepl.v1.WebhostPage.reload_generation). A host's two documents attach
// together, often in the same millisecond, so the second can attach on the
// old build after the first one's attach pushed the reload: it has not
// reloaded yet, which is DEBUG. A stale host above the generation the reload
// was pushed for reloaded and still runs another build: it ignored the
// reload, recorded at ERROR on the attach that shows it rather than pushed
// again without end. A host that states no generation cannot be judged and
// is taken as not reloaded yet.
func (d *Deployer) ReconcileWebhost(edge string) int {
	installed, err := buildid.Webapp(d.deps.Live.WebappDist())
	if err != nil {
		d.log.Error(opWebhost, "could not read the installed webapp's build; the web view host is not reconciled",
			dlog.WithCause(dlog.Context{"edge": edge}, err))
		return 0
	}
	var stale []*agentreplv1.WebhostPage
	for _, page := range d.deps.Clients.WebhostPages() {
		if page.GetWebappBuild() != installed {
			stale = append(stale, page)
		}
	}
	if len(stale) == 0 {
		return 0
	}
	fields := dlog.Context{"edge": edge, "running": stale[0].GetWebappBuild(), "installed": installed}
	d.webhost.mu.Lock()
	defer d.webhost.mu.Unlock()
	if d.webhost.pushed == installed {
		d.judgeStaleWebhosts(edge, stale, fields)
		return 0
	}
	reached := d.deps.Clients.PushReloadWebhost(installed)
	if reached == 0 {
		d.log.Info(opWebhost, "a web view host runs another webapp and no Emacs stream stands to take its reload; it is pushed when one attaches", fields)
		return 0
	}
	d.webhost.pushed = installed
	d.webhost.pushedFor = highestGeneration(stale)
	fields["emacs"] = reached
	fields["reload_generation"] = generationField(d.webhost.pushedFor)
	d.log.Info(opWebhost, "the web view host runs another webapp than the installed one; pushed its reload", fields)
	return reached
}

// judgeStaleWebhosts records each host still STALE after its reload was
// pushed: at ERROR one that reloaded past the pushed generation and so
// ignored it, when a host's attach is the edge, and at DEBUG every other.
// The caller holds d.webhost.mu.
func (d *Deployer) judgeStaleWebhosts(edge string, stale []*agentreplv1.WebhostPage, fields dlog.Context) {
	pushedFor := d.webhost.pushedFor
	for _, page := range stale {
		at := dlog.Merge(fields, dlog.Context{
			"running":           page.GetWebappBuild(),
			"reload_generation": generationField(page.ReloadGeneration),
			"pushed_for":        generationField(pushedFor),
		})
		ignored := page.ReloadGeneration != nil && pushedFor != nil && *page.ReloadGeneration > *pushedFor
		switch {
		case ignored && edge == WebhostEdgeHostAttached:
			d.log.Error(opWebhost, "a web view host reloaded after its reload onto the installed webapp was pushed and still runs another webapp", at)
		case ignored:
			d.log.Debug(opWebhost, "the web view host's reload onto the installed webapp is already pushed", at)
		default:
			d.log.Debug(opWebhost, "a web view host has not reloaded onto the installed webapp yet; its reload is already pushed", at)
		}
	}
}

// highestGeneration answers the highest reload generation PAGES state, nil
// when none states one.
func highestGeneration(pages []*agentreplv1.WebhostPage) *uint32 {
	var highest *uint32
	for _, page := range pages {
		if g := page.ReloadGeneration; g != nil && (highest == nil || *g > *highest) {
			v := *g
			highest = &v
		}
	}
	return highest
}

// generationField is a reload generation as a record states it: the number,
// or "unstated".
func generationField(g *uint32) any {
	if g == nil {
		return "unstated"
	}
	return *g
}
