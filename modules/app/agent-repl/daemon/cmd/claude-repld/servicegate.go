package main

import (
	"context"
	"sync"
)

// serviceGate is the boot's one latch on the launchd services: it closes
// once the boot's service step has returned, and every shim spawn waits on
// it (shimclient.WithServicesReady). So no shim ever starts before the boot
// has made the store and the sidecar current -- not the boot's own bring-up,
// which already runs after that step, and not a session the editor's
// startup, OpenWorkspace or a revival starts while the step is still running.
//
// It opens on a FAILED step too. A service step that failed is recorded at
// ERROR by the bring-up, and each shim then raises its own store fault, which
// is what puts a dead store on every surface; holding the spawns forever
// would hide it behind sessions that never start.
type serviceGate struct {
	ready chan struct{}
	once  sync.Once
}

func newServiceGate() *serviceGate { return &serviceGate{ready: make(chan struct{})} }

// Ready is the latch every spawn waits on.
func (g *serviceGate) Ready() <-chan struct{} { return g.ready }

// step wraps the boot's service step so the latch opens once it has
// returned, whatever it returned.
func (g *serviceGate) step(ensure func(context.Context) error) func(context.Context) error {
	return func(ctx context.Context) error {
		defer g.once.Do(func() { close(g.ready) })
		return ensure(ctx)
	}
}

// bootServiceStep picks the boot's service step. An ordinary boot makes the
// services current (deploy.Restarter.EnsureCurrent): a bounce leaves a stale
// store and sidecar for exactly this boot to restart before any shim starts.
// A JOINING successor only ensures launchd holds them: the incumbent's shims
// are live and write into the store throughout a rollout, and the deploy that
// started the rollout already restarted whatever service was stale.
func bootServiceStep(joining bool, ensureLoaded, ensureCurrent func(context.Context) error) func(context.Context) error {
	if joining {
		return ensureLoaded
	}
	return ensureCurrent
}
