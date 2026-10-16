package integration

// SUBJECT — THE STREAM PLANE OWNS THE SERVED HOOK ROW (ruling 2026-09-04).
//
// A hook reaches this system on BOTH planes by vendor design, and the vendor
// hands them DISJOINT identity material: the stream's `hook_started` /
// `hook_response` pair carries a `hook_id` and never a `tool_use_id`, the
// transcript's attachment carries a `toolUseID` and never a `hook_id`, and the
// two records' uuids differ. No upsert key can name one firing on both, so a
// hook converted on both planes drew TWO feed rows nothing downstream could
// reconcile — the file plane's the poorer of the two, since it writes no start
// frame and so carries neither the hook's name nor its event.
//
// The ruling is R15's precedent applied to hooks: the shim's row is the one
// SERVED form, and this reader still READS the transcript record and states
// what it was. Under the 2026-09-13 residue ruling that classification is where
// the record ends — it is named and withheld rather than stored as an unserved
// item — so the reader's own account of it is what these subjects pin, against
// the real reader driving the real capture's own hook attachments.
