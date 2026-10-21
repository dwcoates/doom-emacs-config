package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
)

// A request's or a creation's DEPLOY HOOKS (DeployHooks) are stored as one
// JSON array per stage, each element a repoconfig.v1.DeployHook in protojson
// with its proto field names: `{"prompt": {"text": "..."}}` or
// `{"script": {"path": "...", "timeout_seconds": 30}}`. An absent stage and an
// empty one are both stored as "[]", so a decode never has to guess.
//
// deployHooksDDL is the layout-68 change. It renames the before/after merge
// action columns to the stage they run in, and rewrites every stored action --
// a JSON array of prompt texts -- as prompt hooks, so a row a previous build
// wrote reads back as the same prompts it ran. IT IS BREAKING: the previous
// build writes the old column names, so the step runs only under a sole
// writer (a stop-then-start restart, never a handover).
//
// A queue entry of the retired merged-upstream kind (source_kind 3) becomes
// the requester's own branch (0): finishing a workspace always runs its
// repository's deploy, so the entry deploys like any other, and a row this
// build could not decode never refuses its boot.
const deployHooksDDL = `
UPDATE merge_queue SET source_kind = 0 WHERE source_kind = 3;
ALTER TABLE creation_jobs RENAME COLUMN actions_before TO pre_deploy_hooks;
ALTER TABLE creation_jobs RENAME COLUMN actions_after TO post_deploy_hooks;
UPDATE creation_jobs SET
  pre_deploy_hooks = (SELECT json_group_array(json_object('prompt', json_object('text', value))) FROM json_each(creation_jobs.pre_deploy_hooks)),
  post_deploy_hooks = (SELECT json_group_array(json_object('prompt', json_object('text', value))) FROM json_each(creation_jobs.post_deploy_hooks));
ALTER TABLE merge_queue RENAME COLUMN source_actions_before TO source_pre_deploy_hooks;
ALTER TABLE merge_queue RENAME COLUMN source_actions_after TO source_post_deploy_hooks;
UPDATE merge_queue SET
  source_pre_deploy_hooks = (SELECT json_group_array(json_object('prompt', json_object('text', value))) FROM json_each(merge_queue.source_pre_deploy_hooks)),
  source_post_deploy_hooks = (SELECT json_group_array(json_object('prompt', json_object('text', value))) FROM json_each(merge_queue.source_post_deploy_hooks));
`

// hookJSON is the one protojson spelling a stored hook uses.
var hookJSON = protojson.MarshalOptions{UseProtoNames: true}

// encodeHooks renders one stage's hooks as the JSON array the column holds.
func encodeHooks(hooks []*repoconfigv1.DeployHook) (string, error) {
	items := make([]json.RawMessage, 0, len(hooks))
	for i, hook := range hooks {
		raw, err := hookJSON.Marshal(hook)
		if err != nil {
			return "", fmt.Errorf("wsm: encode deploy hook %d: %w", i, err)
		}
		items = append(items, raw)
	}
	out, err := json.Marshal(items)
	if err != nil {
		return "", fmt.Errorf("wsm: encode deploy hooks: %w", err)
	}
	return string(out), nil
}

// decodeHooks parses one stored stage of TABLE's, failing the whole read when
// the column is not a JSON array of hooks.
func decodeHooks(table, row, field, raw string) ([]*repoconfigv1.DeployHook, error) {
	var items []json.RawMessage
	if err := json.Unmarshal([]byte(raw), &items); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: field, Err: err}
	}
	if len(items) == 0 {
		return nil, nil
	}
	out := make([]*repoconfigv1.DeployHook, 0, len(items))
	for i, item := range items {
		hook := &repoconfigv1.DeployHook{}
		if err := protojson.Unmarshal(item, hook); err != nil {
			return nil, &DecodeError{Table: table, Row: row, Field: fmt.Sprintf("%s[%d]", field, i), Err: err}
		}
		if hook.GetHook() == nil {
			return nil, &DecodeError{Table: table, Row: row, Field: fmt.Sprintf("%s[%d]", field, i), Err: fmt.Errorf("the hook is neither a script nor a prompt")}
		}
		out = append(out, hook)
	}
	return out, nil
}

// rewriteDeployQueueRows is layout 68's Go rewrite: the stored merge tabs and
// the stored progress records a previous build wrote, each read again in the
// deploy queue's vocabulary.
func rewriteDeployQueueRows(ctx context.Context, tx *sql.Tx) error {
	if err := rewriteRetiredMergeTabs(ctx, tx); err != nil {
		return err
	}
	return rewriteRetiredProgressSteps(ctx, tx)
}

// retiredProgressSteps maps each step and tab kind a progress record of a
// previous build could name onto the deploy stage that replaced it.
var retiredProgressSteps = map[string]string{
	"pre_prompt":  "pre_deploy",
	"post_prompt": "post_deploy",
}

// retiredUpdatingMain is the step of a merged-upstream finish, retired with it.
const retiredUpdatingMain = "updating_main"

// rewriteRetiredProgressSteps is layout 68's progress rewrite. A suspended
// merge's record (merge_progress.document, the merge package's JSON) names its
// step and its tab rounds by tab kind, and a previous build's record may name
// a configured prompt's step, which is now a deploy stage of the same place:
// it is renamed. Its `self_checkout`, which made the old teardown roll the
// runtime out, becomes `rollout_requested`, which the deploy hook sets now. A
// record standing at `updating_main` belongs to a merged-upstream finish, whose
// entry layout 68 turned into an own-branch deploy: the record is dropped, so
// recovery puts the entry back in line to deploy afresh. Every other record is
// left byte for byte as it was.
func rewriteRetiredProgressSteps(ctx context.Context, tx *sql.Tx) error {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, document FROM merge_progress`)
	if err != nil {
		return err
	}
	type rewrite struct {
		ws   string
		doc  []byte
		drop bool
	}
	var rewrites []rewrite
	for rows.Next() {
		var ws string
		var raw []byte
		if err := rows.Scan(&ws, &raw); err != nil {
			rows.Close()
			return err
		}
		out, drop, changed, err := rewriteRetiredProgress(raw)
		if err != nil {
			rows.Close()
			return fmt.Errorf("merge progress of %s: %w", ws, err)
		}
		if changed {
			rewrites = append(rewrites, rewrite{ws: ws, doc: out, drop: drop})
		}
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if err := rows.Err(); err != nil {
		return err
	}
	for _, rw := range rewrites {
		if rw.drop {
			if _, err := tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE workspace_id = ?`, rw.ws); err != nil {
				return err
			}
			continue
		}
		if _, err := tx.ExecContext(ctx, `UPDATE merge_progress SET document = ? WHERE workspace_id = ?`, rw.doc, rw.ws); err != nil {
			return err
		}
	}
	return nil
}

// rewriteRetiredProgress rewrites one progress record: the record anew, or
// that it is dropped, or that it is unchanged.
func rewriteRetiredProgress(raw []byte) (out []byte, drop, changed bool, err error) {
	var doc map[string]any
	if err := json.Unmarshal(raw, &doc); err != nil {
		return nil, false, false, err
	}
	if doc["step"] == retiredUpdatingMain {
		return nil, true, true, nil
	}
	rename := func(kind any) any {
		if s, ok := kind.(string); ok {
			if to, retired := retiredProgressSteps[s]; retired {
				changed = true
				return to
			}
		}
		return kind
	}
	doc["step"] = rename(doc["step"])
	if rounds, ok := doc["rounds"].(map[string]any); ok {
		for from, to := range retiredProgressSteps {
			if n, found := rounds[from]; found {
				rounds[to] = n
				delete(rounds, from)
				changed = true
			}
		}
	}
	if active, ok := doc["active"].(map[string]any); ok {
		active["kind"] = rename(active["kind"])
	}
	if open, ok := doc["open"].([]any); ok {
		for _, round := range open {
			if r, ok := round.(map[string]any); ok {
				r["kind"] = rename(r["kind"])
			}
		}
	}
	if self, found := doc["self_checkout"]; found {
		doc["rollout_requested"] = self
		delete(doc, "self_checkout")
		changed = true
	}
	if _, found := doc["emacs_repo"]; found {
		delete(doc, "emacs_repo")
		changed = true
	}
	if !changed {
		return nil, false, false, nil
	}
	out, err = json.Marshal(doc)
	return out, false, true, err
}

// The FeedMergeTab.kind tags the deploy queue retired whose stored rows are
// rewritten: a configured before- or after-merge prompt's tab, which a prompt
// hook's tab (FeedMergeTabHookPrompt) draws with the very same fields.
const (
	retiredPrePromptTag  protowire.Number = 3
	retiredPostPromptTag protowire.Number = 8
)

// rewriteRetiredMergeTabs is layout 68's row rewrite: every stored merge tab
// row (durable_feed_rows) whose kind is a retired pre- or post-prompt tab is
// re-encoded as a prompt hook's tab, so a bubble recorded before the deploy
// queue is drawn again whole. The two retired messages and FeedMergeTabHookPrompt
// share their field numbers (state: live 1, settled 2), so the arm's bytes are
// carried over as they are. Every other row is left byte for byte as it was.
func rewriteRetiredMergeTabs(ctx context.Context, tx *sql.Tx) error {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, row_id, row FROM durable_feed_rows`)
	if err != nil {
		return err
	}
	type rewrite struct {
		ws, id string
		row    []byte
	}
	var rewrites []rewrite
	for rows.Next() {
		var ws, id string
		var raw []byte
		if err := rows.Scan(&ws, &id, &raw); err != nil {
			rows.Close()
			return err
		}
		out, changed, err := rewriteRetiredMergeTab(raw)
		if err != nil {
			rows.Close()
			return fmt.Errorf("durable feed row %s/%s: %w", ws, id, err)
		}
		if changed {
			rewrites = append(rewrites, rewrite{ws: ws, id: id, row: out})
		}
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if err := rows.Err(); err != nil {
		return err
	}
	for _, rw := range rewrites {
		if _, err := tx.ExecContext(ctx, `UPDATE durable_feed_rows SET row = ? WHERE workspace_id = ? AND row_id = ?`, rw.row, rw.ws, rw.id); err != nil {
			return err
		}
	}
	return nil
}

// rewriteRetiredMergeTab re-encodes one stored frontend.v1.FeedRow whose merge
// tab carries a retired prompt arm, and reports whether it did.
func rewriteRetiredMergeTab(raw []byte) ([]byte, bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return nil, false, err
	}
	tab := row.GetMergeTab()
	if tab == nil || tab.GetKind() != nil {
		return raw, false, nil
	}
	unknown := tab.ProtoReflect().GetUnknown()
	var kept []byte
	var prompt []byte
	found := false
	for len(unknown) > 0 {
		num, typ, n := protowire.ConsumeTag(unknown)
		if n < 0 {
			return nil, false, protowire.ParseError(n)
		}
		m := protowire.ConsumeFieldValue(num, typ, unknown[n:])
		if m < 0 {
			return nil, false, protowire.ParseError(m)
		}
		field := unknown[:n+m]
		if (num == retiredPrePromptTag || num == retiredPostPromptTag) && typ == protowire.BytesType {
			value, _ := protowire.ConsumeBytes(unknown[n:])
			prompt, found = value, true
		} else {
			kept = append(kept, field...)
		}
		unknown = unknown[n+m:]
	}
	if !found {
		return raw, false, nil
	}
	hook := &frontendv1.FeedMergeTabHookPrompt{}
	if err := proto.Unmarshal(prompt, hook); err != nil {
		return nil, false, err
	}
	tab.ProtoReflect().SetUnknown(kept)
	tab.Kind = &frontendv1.FeedMergeTab_HookPrompt{HookPrompt: hook}
	out, err := proto.Marshal(row)
	if err != nil {
		return nil, false, err
	}
	return out, true, nil
}
