package wsm

import (
	"encoding/json"
	"fmt"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"google.golang.org/protobuf/encoding/protojson"
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
const deployHooksDDL = `
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
