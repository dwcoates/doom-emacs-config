package deploy

import (
	"sync"

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
// a host was told to reload onto. Guarded by its own mutex, apart from the
// deploy slot's.
type webhostReload struct {
	mu     sync.Mutex
	pushed string
}

// ReconcileWebhost pushes the web view host's reload when a standing host
// runs a webapp other than the installed one, answering how many Emacs
// streams took it. A reload is pushed ONCE per installed build: a host that
// then attaches still on another build did not reload onto it, which is
// recorded at ERROR rather than pushed again without end.
func (d *Deployer) ReconcileWebhost(edge string) int {
	installed, err := buildid.Webapp(d.deps.Live.WebappDist())
	if err != nil {
		d.log.Error(opWebhost, "could not read the installed webapp's build; the web view host is not reconciled",
			dlog.WithCause(dlog.Context{"edge": edge}, err))
		return 0
	}
	var stale string
	for _, running := range d.deps.Clients.WebhostBuilds() {
		if running != installed {
			stale = running
			break
		}
	}
	if stale == "" {
		return 0
	}
	fields := dlog.Context{"edge": edge, "running": stale, "installed": installed}
	d.webhost.mu.Lock()
	defer d.webhost.mu.Unlock()
	if d.webhost.pushed == installed {
		if edge == WebhostEdgeHostAttached {
			d.log.Error(opWebhost, "a web view host attached on another webapp after its reload onto the installed one was pushed", fields)
		} else {
			d.log.Debug(opWebhost, "the web view host's reload onto the installed webapp is already pushed", fields)
		}
		return 0
	}
	reached := d.deps.Clients.PushReloadWebhost(installed)
	if reached == 0 {
		d.log.Info(opWebhost, "a web view host runs another webapp and no Emacs stream stands to take its reload; it is pushed when one attaches", fields)
		return 0
	}
	d.webhost.pushed = installed
	fields["emacs"] = reached
	d.log.Info(opWebhost, "the web view host runs another webapp than the installed one; pushed its reload", fields)
	return reached
}
