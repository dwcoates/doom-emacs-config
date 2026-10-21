package wsm

import (
	"context"
	"errors"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"
)

func TestDeployHooksRoundTripEveryHookKind(t *testing.T) {
	tests := []struct {
		name  string
		hooks []*repoconfigv1.DeployHook
	}{
		{name: "no hook", hooks: nil},
		{name: "a prompt", hooks: []*repoconfigv1.DeployHook{promptHook("run the linters")}},
		{name: "a script", hooks: []*repoconfigv1.DeployHook{scriptHook("bin/notify.sh", 30)}},
		{name: "both, in order", hooks: []*repoconfigv1.DeployHook{scriptHook("bin/a.sh", 1), promptHook("b")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			raw, err := encodeHooks(tt.hooks)
			if err != nil {
				t.Fatalf("encodeHooks: %v", err)
			}
			got, err := decodeHooks("t", "row", "col", raw)

			// Assert
			if err != nil {
				t.Fatalf("decodeHooks(%s): %v", raw, err)
			}
			if !sameHooks(DeployHooks{PreDeploy: got}, DeployHooks{PreDeploy: tt.hooks}) {
				t.Fatalf("decodeHooks(%s) = %v, want %v", raw, got, tt.hooks)
			}
		})
	}
}

func TestDeployHooksAreStoredWithTheirProtoFieldNames(t *testing.T) {
	// Act
	raw, err := encodeHooks([]*repoconfigv1.DeployHook{scriptHook("bin/x.sh", 7)})

	// Assert
	if err != nil || raw != `[{"script":{"path":"bin/x.sh","timeout_seconds":7}}]` {
		t.Fatalf("encodeHooks = %s, %v; want the proto field names the migration writes", raw, err)
	}
}

func TestDecodeHooksRefusesWhatIsNoHook(t *testing.T) {
	tests := []struct {
		name  string
		raw   string
		field string
	}{
		{name: "not a list", raw: `"x"`, field: "col"},
		{name: "a bare prompt text", raw: `["run the linters"]`, field: "col[0]"},
		{name: "an element with neither arm", raw: `[{}]`, field: "col[0]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := decodeHooks("t", "row", "col", tt.raw)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != tt.field {
				t.Fatalf("decodeHooks(%s) = %v, want a decode error naming %s", tt.raw, err, tt.field)
			}
		})
	}
}

func TestTheMigrationRewritesStoredActionsAsPromptHooks(t *testing.T) {
	// Arrange -- a creation job and a queued request a layout-66 build wrote,
	// each with before/after prompt texts.
	path := fixtureAt(t, 66)
	execRaw(t, path, `INSERT INTO creation_jobs (workspace_id, source_branch, source_dir, target_dir, layout_origin,
	   actions_before, actions_after, base_ref, materialized, one_shot, initial_prompt, consented_ungated_mode, created_at)
	 VALUES ('ws-1', 'b', '/s', '/t', 'create', '["lint","check"]', '[]', 'master', 1, 0, '', '', 1)`)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_actions_before, source_actions_after)
	 VALUES ('/repo', 'ws-1', 1, 0, 1, '[]', '["post"]')`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-66 database: %v", err)
	}
	defer handle.Close()
	s := handle.(*store)

	// Assert
	pre := scalar[string](t, s, `SELECT pre_deploy_hooks FROM creation_jobs WHERE workspace_id = 'ws-1'`)
	post := scalar[string](t, s, `SELECT post_deploy_hooks FROM creation_jobs WHERE workspace_id = 'ws-1'`)
	if pre != `[{"prompt":{"text":"lint"}},{"prompt":{"text":"check"}}]` || post != `[]` {
		t.Fatalf("creation job hooks = %s / %s, want the prompts as prompt hooks", pre, post)
	}
	queued := scalar[string](t, s, `SELECT source_post_deploy_hooks FROM merge_queue WHERE workspace_id = 'ws-1'`)
	if queued != `[{"prompt":{"text":"post"}}]` {
		t.Fatalf("queued post-deploy hooks = %s, want the prompt as a prompt hook", queued)
	}
	hooks, err := decodeHooks("creation_jobs", "ws-1", "pre_deploy_hooks", pre)
	if err != nil || !sameHooks(DeployHooks{PreDeploy: hooks}, DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("lint"), promptHook("check")}}) {
		t.Fatalf("decoded migrated hooks = %v, %v", hooks, err)
	}
}

func TestTheDeployHooksStepIsBreaking(t *testing.T) {
	for _, m := range migrations {
		if m.To == 68 {
			if m.Kind != MigrationBreaking {
				t.Fatalf("layout 68 is %v; it renames columns the previous build writes, so it is breaking", m.Kind)
			}
			return
		}
	}
	t.Fatal("no layout-68 step")
}
