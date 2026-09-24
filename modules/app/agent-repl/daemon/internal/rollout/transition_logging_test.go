package rollout

import (
	"reflect"
	"testing"

	"claude-repld/internal/ids"
)

func TestRolloutStateTransitionsRecordTheirBeforeAndAfter(t *testing.T) {
	const ws ids.WorkspaceID = "ws-transition"
	tests := []struct {
		name      string
		operation string
		state     string
		before    any
		after     any
		arrange   func(*controller)
		act       func(*controller)
	}{
		{
			name: "a workspace transfers to the successor", operation: opTransfer, state: "serving_standing",
			before: "", after: "127.0.0.1:7788",
			act: func(c *controller) { c.recordTransfer(ws, "127.0.0.1:7788") },
		},
		{
			name: "a build stamp claims its first bounce", operation: opStaleness, state: "bounced_build_stamp",
			before: "", after: "build-a",
			act: func(c *controller) { c.claimStaleBounce(ws, "build-a") },
		},
		{
			name: "a repeated build stamp keeps its claim", operation: opStaleness, state: "bounced_build_stamp",
			before: "build-a", after: "build-a",
			// The first claim's bounce has FINISHED: a claim still in flight is
			// skipped before the stamp is read.
			arrange: func(c *controller) {
				c.claimStaleBounce(ws, "build-a")
				c.settleStaleBounce(ws, nil)
				c.staleChecks.Wait()
			},
			act: func(c *controller) { c.claimStaleBounce(ws, "build-a") },
		},
		{
			name: "a manifest arms a rendezvous", operation: opJoin, state: "rendezvous",
			before: "unarmed", after: "armed",
			act: func(c *controller) {
				c.armSessions(ids.InstanceID("daemon-outgoing-previous"), []ManifestSession{{Workspace: ws, ExpectedHost: true}})
			},
		},
		{
			name: "a headless workspace is claimed", operation: opJoin, state: "headless_claimed",
			before: false, after: true,
			arrange: func(c *controller) {
				c.armSessions(ids.InstanceID("daemon-outgoing-previous"), []ManifestSession{{Workspace: ws}})
			},
			act: func(c *controller) { c.claimHeadless() },
		},
		{
			name: "a failed headless adoption releases its claim", operation: opJoin, state: "headless_claimed",
			before: true, after: false,
			arrange: func(c *controller) {
				c.armSessions(ids.InstanceID("daemon-outgoing-previous"), []ManifestSession{{Workspace: ws}})
				c.claimHeadless()
			},
			act: func(c *controller) { c.releaseHeadless(ws) },
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.arrange != nil {
				tt.arrange(h.c)
			}
			beforeRecords := len(h.log.Records())

			// Act.
			tt.act(h.c)

			// Assert.
			for _, record := range h.log.Records()[beforeRecords:] {
				if record.Level == "debug" && record.Operation == tt.operation &&
					record.Context["workspace"] == string(ws) && record.Context["state"] == tt.state &&
					reflect.DeepEqual(record.Context["before"], tt.before) && reflect.DeepEqual(record.Context["after"], tt.after) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s state %s before=%v after=%v", h.log.Records()[beforeRecords:], tt.operation, tt.state, tt.before, tt.after)
		})
	}
}
