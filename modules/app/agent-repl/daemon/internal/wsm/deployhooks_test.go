package wsm

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"reflect"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
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
			if !(DeployHooks{PreDeploy: got}).Equal(DeployHooks{PreDeploy: tt.hooks}) {
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
	if err != nil || !(DeployHooks{PreDeploy: hooks}).Equal(DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("lint"), promptHook("check")}}) {
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

// retiredPromptTabRow is a stored merge tab row as a build before the deploy
// queue wrote it: its kind the retired arm TAG, carrying a settled state.
func retiredPromptTabRow(t *testing.T, tag protowire.Number) []byte {
	t.Helper()
	state, err := proto.Marshal(&frontendv1.FeedMergeTabHookPrompt{State: &frontendv1.FeedMergeTabHookPrompt_Settled{Settled: &frontendv1.FeedMergeTabSettled{
		StartedAtMs: 10, EndedAtMs: 20, Outcome: &frontendv1.FeedMergeTabSettled_Succeeded{Succeeded: &frontendv1.FeedMergeTabSucceeded{}}}}})
	if err != nil {
		t.Fatal(err)
	}
	tab := &frontendv1.FeedMergeTab{Label: &frontendv1.FeedMergeTabLabel{Words: &frontendv1.FeedMergeTabLabel_Text{Text: "post-prompt"}, Round: 1}}
	var unknown []byte
	unknown = protowire.AppendTag(unknown, tag, protowire.BytesType)
	unknown = protowire.AppendBytes(unknown, state)
	tab.ProtoReflect().SetUnknown(unknown)
	raw, err := proto.Marshal(&frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: tab}})
	if err != nil {
		t.Fatal(err)
	}
	return raw
}

func TestRewriteRetiredMergeTabCarriesARetiredPromptTabOntoTheHookTab(t *testing.T) {
	for _, tag := range []protowire.Number{retiredPrePromptTag, retiredPostPromptTag} {
		t.Run(fmt.Sprintf("tag %d", tag), func(t *testing.T) {
			// Act
			out, changed, err := rewriteRetiredMergeTab(retiredPromptTabRow(t, tag))

			// Assert
			if err != nil || !changed {
				t.Fatalf("rewriteRetiredMergeTab = changed %v, %v; want the row rewritten", changed, err)
			}
			row := &frontendv1.FeedRow{}
			if err := proto.Unmarshal(out, row); err != nil {
				t.Fatal(err)
			}
			hook := row.GetMergeTab().GetHookPrompt()
			if hook.GetSettled().GetEndedAtMs() != 20 || hook.GetSettled().GetSucceeded() == nil {
				t.Fatalf("hook prompt tab = %v, want the retired tab's settled state", hook)
			}
			if row.GetMergeTab().GetLabel().GetText() != "post-prompt" || len(row.GetMergeTab().ProtoReflect().GetUnknown()) != 0 {
				t.Fatalf("tab = %v, want its label kept and nothing left unknown", row.GetMergeTab())
			}
		})
	}
}

func TestRewriteRetiredMergeTabLeavesEveryOtherRowAlone(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
	}{
		{name: "a row that is no merge tab", row: &frontendv1.FeedRow{}},
		{name: "a live tab kind", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
			Kind: &frontendv1.FeedMergeTab_Committing{Committing: &frontendv1.FeedMergeTabCommitting{}}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			raw, err := proto.Marshal(tt.row)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			out, changed, err := rewriteRetiredMergeTab(raw)

			// Assert
			if err != nil || changed || !bytes.Equal(out, raw) {
				t.Fatalf("rewriteRetiredMergeTab = changed %v, %v; want the row left byte for byte", changed, err)
			}
		})
	}
}

func TestTheMigrationRewritesStoredRetiredPromptTabs(t *testing.T) {
	// Arrange -- a layout-66 file holding a stored post-prompt tab row.
	path := fixtureAt(t, 66)
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'tab-1', 0, 'k', ?)`,
		retiredPromptTabRow(t, retiredPostPromptTag))

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-66 database: %v", err)
	}
	defer handle.Close()

	// Assert
	raw := scalar[[]byte](t, handle.(*store), `SELECT row FROM durable_feed_rows WHERE row_id = 'tab-1'`)
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil || row.GetMergeTab().GetHookPrompt() == nil {
		t.Fatalf("stored row = %v, %v; want it a hook prompt tab", row, err)
	}
}

func TestDeployHooksEqual(t *testing.T) {
	lint, notify := promptHook("lint"), scriptHook("bin/notify.sh", 30)
	tests := []struct {
		name string
		a, b DeployHooks
		want bool
	}{
		{name: "both empty", want: true},
		{name: "equal hooks held by different pointers", a: DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("lint")}, PostDeploy: []*repoconfigv1.DeployHook{notify}},
			b: DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{lint}, PostDeploy: []*repoconfigv1.DeployHook{scriptHook("bin/notify.sh", 30)}}, want: true},
		{name: "a hook in another stage", a: DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{lint}}, b: DeployHooks{PostDeploy: []*repoconfigv1.DeployHook{lint}}},
		{name: "a stage of a different length", a: DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{lint}}, b: DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{lint, lint}}},
		{name: "the same hooks in another order", a: DeployHooks{PostDeploy: []*repoconfigv1.DeployHook{lint, notify}}, b: DeployHooks{PostDeploy: []*repoconfigv1.DeployHook{notify, lint}}},
		{name: "a script of another timeout", a: DeployHooks{PostDeploy: []*repoconfigv1.DeployHook{notify}}, b: DeployHooks{PostDeploy: []*repoconfigv1.DeployHook{scriptHook("bin/notify.sh", 31)}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.a.Equal(tt.b)

			// Assert
			if got != tt.want {
				t.Errorf("Equal = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRewriteRetiredProgress(t *testing.T) {
	tests := []struct {
		name        string
		doc         string
		wantDrop    bool
		wantChanged bool
		want        map[string]any
	}{
		{
			name:        "a record at a configured pre-merge prompt stands at the pre-deploy stage",
			doc:         `{"step":"pre_prompt","rounds":{"queue":1,"pre_prompt":1},"active":{"kind":"pre_prompt","n":1},"open":[{"kind":"pre_prompt","n":1}],"emacs_repo":true,"self_checkout":false}`,
			wantChanged: true,
			want: map[string]any{"step": "pre_deploy", "rounds": map[string]any{"queue": float64(1), "pre_deploy": float64(1)},
				"active": map[string]any{"kind": "pre_deploy", "n": float64(1)}, "open": []any{map[string]any{"kind": "pre_deploy", "n": float64(1)}},
				"rollout_requested": false},
		},
		{
			name:        "a landing in the daemon's own checkout still rolls out",
			doc:         `{"step":"post_prompt","active":{"kind":"post_prompt","n":1},"self_checkout":true}`,
			wantChanged: true,
			want:        map[string]any{"step": "post_deploy", "active": map[string]any{"kind": "post_deploy", "n": float64(1)}, "rollout_requested": true},
		},
		{name: "a merged-upstream finish's record is dropped", doc: `{"step":"updating_main"}`, wantDrop: true, wantChanged: true},
		{name: "a record this build wrote is left alone", doc: `{"step":"tests","active":{"kind":"tests","n":1},"rollout_requested":false}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			out, drop, changed, err := rewriteRetiredProgress([]byte(tt.doc))

			// Assert
			if err != nil || drop != tt.wantDrop || changed != tt.wantChanged {
				t.Fatalf("rewriteRetiredProgress = drop %v, changed %v, %v; want drop %v, changed %v", drop, changed, err, tt.wantDrop, tt.wantChanged)
			}
			if tt.want == nil {
				return
			}
			var got map[string]any
			if err := json.Unmarshal(out, &got); err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("record = %s (%v), want %v", out, err, tt.want)
			}
		})
	}
}

func TestRewriteRetiredProgressRefusesARecordThatIsNoJSON(t *testing.T) {
	// Act
	_, _, _, err := rewriteRetiredProgress([]byte("not json"))

	// Assert
	if err == nil {
		t.Fatal("rewriteRetiredProgress = nil error, want the record refused")
	}
}

func TestTheMigrationCarriesSuspendedMergesIntoTheDeployQueue(t *testing.T) {
	// Arrange -- a layout-66 file with a merged-upstream entry suspended at
	// updating main, and an own-branch entry suspended at its post-merge prompt.
	path := fixtureAt(t, 66)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_actions_before, source_actions_after)
	 VALUES ('/repo', 'ws-up', 1, 1, 1, 3, '[]', '[]'), ('/repo', 'ws-own', 2, 1, 1, 0, '[]', '[]')`)
	execRaw(t, path, `INSERT INTO merge_progress (workspace_id, lease_id, updated_at, document)
	 VALUES ('ws-up', 'l-up', 1, '{"step":"updating_main"}'), ('ws-own', 'l-own', 1, '{"step":"post_prompt","self_checkout":true}')`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-66 database: %v", err)
	}
	defer handle.Close()
	s := handle.(*store)

	// Assert
	if kind := scalar[int](t, s, `SELECT source_kind FROM merge_queue WHERE workspace_id = 'ws-up'`); kind != int(MergeSourceOwnBranch) {
		t.Fatalf("the merged-upstream entry's kind = %d, want the own branch", kind)
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM merge_progress WHERE workspace_id = 'ws-up'`); n != 0 {
		t.Fatalf("the updating-main record survived (%d), want it dropped", n)
	}
	doc := scalar[string](t, s, `SELECT document FROM merge_progress WHERE workspace_id = 'ws-own'`)
	if doc != `{"rollout_requested":true,"step":"post_deploy"}` {
		t.Fatalf("the post-prompt record = %s, want it at the post-deploy stage with its rollout requested", doc)
	}
}
