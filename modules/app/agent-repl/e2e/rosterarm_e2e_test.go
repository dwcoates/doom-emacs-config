// rosterarm_e2e_test.go — the roster arm's ORDER, against the real world.
//
// THE DEFECT, MEASURED. Emacs observed the walk
// :none -> :ready -> :init -> :submitting -> :thinking on a cold workspace,
// with the `ready` frame arriving ~340ms AFTER SubmitPrompt had already
// answered with a turn id. Both halves contradict
// proto/src/frontend/v1/sidebar.proto:226, which defines ready as "Live,
// proven usable, and idle": a workspace whose session record exists while no
// link state has been observed has proven nothing, and one whose prompt the
// daemon has accepted is not idle. `ready` also cannot precede the `init` it
// is supposed to follow.
//
// The resolver's own ordering is pinned in
// daemon/internal/resolve/sidebar/status_test.go; this file pins it where the
// frames actually reach a client, over the real store, sidecar, daemon and
// Node shim.
package e2e
