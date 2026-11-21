package wsm

// mergeProfileGateDDL is the layout-189 addition: whether a merge asked for
// its test gate profiled (MergeSource.ProfileGate), on every table a request
// stands in: the queue's row, its record off the queue, and an async request
// awaiting its hand-off.
//
// IT IS ADDITIVE: the previous build's statements name no such column, so a
// row it writes takes the default 0, a gate that does not profile, which is
// all that build could ask for. The fresh-file schema appends this same text
// (schema.go), so a migrated file and a created one cannot differ.
const mergeProfileGateDDL = `
ALTER TABLE merge_queue ADD COLUMN source_profile_gate INTEGER NOT NULL DEFAULT 0;
ALTER TABLE off_queue_merges ADD COLUMN source_profile_gate INTEGER NOT NULL DEFAULT 0;
ALTER TABLE async_merge_requests ADD COLUMN profile_gate INTEGER NOT NULL DEFAULT 0;
`
