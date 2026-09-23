package db

import (
	"context"
	"database/sql"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// applyLifecycle writes the unpacked-column half of one entry: the agent row,
// the detached_work row, and the terminal columns a conclusion closes.
//
// IT RUNS IN THE SAME TRANSACTION AS THE `entry` ROW. A success frame is BOTH a
// page line (the feed's stop notice has no other source) and the agent's
// terminal state, and the two can never be observed apart — which is the whole
// reason the routing is one transaction rather than two writes.
func (d *DB) applyLifecycle(ctx context.Context, tx *sql.Tx, r routed, now int64) error {
	update := r.entry.GetAgentUpdate()
	if update == nil {
		// A session_update belongs to no agent and closes nothing.
		return nil
	}
	switch arm := update.GetAgentInfo().(type) {
	case *storev1.StoreAgentUpdate_ServeableFrame:
		return d.applyServeableFrameLifecycle(ctx, tx, r, arm.ServeableFrame, now)
	case *storev1.StoreAgentUpdate_Bash:
		return d.applyBashLifecycle(ctx, tx, arm.Bash, now)
	default:
		// Residue and workflow frames touch no lifecycle table: the residue
		// describes nothing the store models, and the workflow table takes no
		// writes this wave.
		return nil
	}
}

func (d *DB) applyServeableFrameLifecycle(ctx context.Context, tx *sql.Tx, r routed, line *storev1.StorePageLine, now int64) error {
	switch item := line.GetAgentItem().GetItem().(type) {
	case *storev1.StoreAgentItem_AgentPrompt:
		// A prompt is the first thing the store may ever hear about an agent
		// addressed directly, so it is a first-sight for the agent row.
		return d.ensureAgent(ctx, tx, item.AgentPrompt.GetAgent().GetValue(), now)
	case *storev1.StoreAgentItem_PeerMessage:
		// A peer message can likewise be a first-sight of the recipient agent
		// (a resumed session's first served line), so it ensures the agent row
		// exactly as a prompt does. It ends no turn and announces no work.
		return d.ensureAgent(ctx, tx, item.PeerMessage.GetAgent().GetValue(), now)
	case *storev1.StoreAgentItem_AgentFrame:
		frame := item.AgentFrame
		agentID := frame.GetAgentId().GetValue()
		if err := d.ensureAgent(ctx, tx, agentID, now); err != nil {
			return err
		}
		// THE STORED BLOB IS THE AgentFrame, not the store envelope around it.
		// A terminal column is read back as conversation vocabulary, and a
		// reader that had to strip a storage envelope off it would be reading
		// the datalayer's model to recover the protocol's.
		blob, err := proto.Marshal(frame)
		if err != nil {
			return invalidFieldf("agent_update.serveable_frame.agent_item.agent_frame", "frame of agent %q cannot be re-serialized: %v", agentID, err)
		}
		switch arm := frame.GetResult().(type) {
		case *conversationv1.AgentFrame_Update:
			return d.applyUpdateLifecycle(ctx, tx, agentID, arm.Update, blob, now)
		case *conversationv1.AgentFrame_Success, *conversationv1.AgentFrame_Failure:
			return d.endAgent(ctx, tx, agentID, blob, now)
		case *conversationv1.AgentFrame_DetachedWork:
			return d.announceDetachedWork(ctx, tx, agentID, arm.DetachedWork, now)
		}
	}
	return nil
}

// applyUpdateLifecycle handles the two things an `update` arm can do to the
// unpacked tables. It NEVER touches the acting agent's own terminal columns:
// only a success or a failure frame ends an agent, and an update arriving after
// one is an out-of-order write, not a resurrection.
func (d *DB) applyUpdateLifecycle(ctx context.Context, tx *sql.Tx, agentID string, update *conversationv1.AgentUpdate, blob []byte, now int64) error {
	activity, ok := update.GetUpdate().(*conversationv1.AgentUpdate_Activity)
	if !ok {
		return nil
	}
	act := activity.Activity
	if subagent, ok := act.GetItem().(*conversationv1.AgentActivity_Subagent); ok {
		switch result := subagent.Subagent.GetResult().(type) {
		case *conversationv1.AgentSubagent_Start:
			if err := d.createSpawnedAgent(ctx, tx, agentID, result.Start, now); err != nil {
				return err
			}
		case *conversationv1.AgentSubagent_Success:
			if err := d.recordSettledSpawnLineage(ctx, tx, agentID, result.Success.GetCreatedAgentId().GetValue(), now); err != nil {
				return err
			}
		}
	}
	if activityIsTerminal(act) {
		return d.closeDetachedByOrigin(ctx, tx, act.GetActivityId().GetValue(), blob, now)
	}
	return nil
}

// ensureAgent records FIRST SIGHT of an agent and nothing else.
//
// DO NOTHING ON CONFLICT, deliberately: `started_at_ms` is when the store first
// heard of the agent, and a later frame that overwrote it would move an agent's
// start forward every time it spoke. The spawn frame supplies the metadata
// (createSpawnedAgent) and NOT the instant — see there for why.
func (d *DB) ensureAgent(ctx context.Context, tx *sql.Tx, agentID string, now int64) error {
	if agentID == "" {
		// Unreachable: classify refuses an empty agent identity before this
		// runs. The guard stays because a silent INSERT of "" would create a
		// book nothing can ever address.
		return invalidFieldf("agent_update.serveable_frame.agent_item.agent_frame.agent_id", "refusing to record an agent with an empty identity")
	}
	const upsertSQL = `INSERT INTO agent (agent_id, started_at_ms) VALUES (?, ?)
	  ON CONFLICT(agent_id) DO NOTHING`
	if _, err := tx.ExecContext(ctx, upsertSQL, agentID, now); err != nil {
		return d.queryError("store.db.write-batch", "agent", logging.Fields{AgentID: agentID}, storagef(err, "recording first sight of agent %q", agentID))
	}
	d.log.LogVerbose(logging.Fields{Operation: "store.db.write-batch", Table: "agent", AgentID: agentID}, "agent row ensured")
	return nil
}

// createSpawnedAgent upserts the row for the agent a spawn CREATED — the join
// key the whole flat model rests on.
//
// The spawn's own unit lives in the SPAWNING agent's book as an ordinary page
// line; this row is the created agent's home, and it is what makes the
// subagent's own book addressable before a single frame of it has arrived.
//
// `started_at_ms` IS THE STORE'S OWN CLOCK, AT FIRST SIGHT, AND NOTHING ELSE.
// It is excluded from the DO UPDATE clause and bound to `now` in the insert, so
// a row already present keeps the instant `ensureAgent` gave it and a row
// created here gets the same kind of instant.
//
// This used to bind `start.GetStartedAt().GetAtMs()` and to overwrite the
// column with it on conflict, which was not a value the column could be
// ordered by. THREE producers reach this row: the store itself (ensureAgent),
// the SHIM's stream plane (its own Date.now(), stamped when it converted the
// SDK event) and the SIDECAR's file plane (the vendor's transcript
// `timestamp`, and a literal 0 whenever that field is missing or unparseable).
// The shim and the sidecar mint the SAME key for the same unit on purpose, so
// which of the three last wrote the column is an arrival race between two
// independent producers — and `LiveWork` orders the live agents by exactly
// this column (internal/db/live.go). A 0 sorted every such agent to the front
// of the listing and destroyed a real instant that was already in the row.
//
// The store's clock is the one clock every other timestamp column here is
// already taken from, it is read once per batch under the write transaction,
// and it therefore agrees with the store's own write order — which is the
// order `LiveWork` is trying to report.
func (d *DB) createSpawnedAgent(ctx context.Context, tx *sql.Tx, spawnedBy string, start *conversationv1.AgentSubagentStart, now int64) error {
	created := start.GetCreatedAgentId().GetValue()
	if created == "" {
		return invalidFieldf("agent_update.serveable_frame.agent_item.agent_frame.update.activity.subagent.start.created_agent_id", "a subagent start names no created_agent_id — the created agent could never be addressed")
	}
	prompt := start.GetPrompt()
	const upsertSQL = `INSERT INTO agent (
	    agent_id, spawned_by_agent, description, prompt_text, subagent_type, requested_name,
	    requested_model, spawn_depth, working_dir, transcript_suppressed, isolation,
	    forked_from_caller, started_at_ms)
	  VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
	  ON CONFLICT(agent_id) DO UPDATE SET
	    spawned_by_agent = excluded.spawned_by_agent,
	    description = excluded.description,
	    prompt_text = excluded.prompt_text,
	    subagent_type = excluded.subagent_type,
	    requested_name = excluded.requested_name,
	    requested_model = excluded.requested_model,
	    spawn_depth = excluded.spawn_depth,
	    working_dir = excluded.working_dir,
	    transcript_suppressed = excluded.transcript_suppressed,
	    isolation = excluded.isolation,
	    forked_from_caller = excluded.forked_from_caller`
	_, err := tx.ExecContext(ctx, upsertSQL,
		created,
		spawnedBy,
		optionalString(prompt.Description),
		nullableString(prompt.GetText()),
		optionalString(prompt.SubagentType),
		optionalString(prompt.RequestedName),
		requestedModel(prompt),
		optionalUint32(start.SpawnDepth),
		optionalString(start.WorkingDir),
		start.GetTranscriptSuppressed(),
		isolationKind(prompt),
		prompt.GetForkedFromCaller(),
		now,
	)
	if err != nil {
		return d.queryError("store.db.write-batch", "agent", logging.Fields{AgentID: created}, storagef(err, "recording spawned agent %q", created))
	}
	d.log.LogVerbose(logging.Fields{Operation: "store.db.write-batch", Table: "agent", AgentID: created},
		"spawned agent recorded spawned_by=%q", spawnedBy)
	return nil
}

// recordSettledSpawnLineage records WHO SPAWNED an agent from the spawn's
// SETTLED frame, for the delivery that carries no start at all.
//
// A FILE-PLANE DELIVERY OF A SYNCHRONOUS SPAWN IS ONLY EVER ITS CONCLUSION. The
// sidecar announces a spawn at its RESULT (the vendor names the outcome there),
// so a session no shim watched states the spawn as an AgentSubagentSuccess in
// the spawner's book and never as a start — and the success carries
// created_agent_id for exactly that case. It is the same statement a start
// makes: the agent whose book holds the spawn unit created that agent. Reading
// lineage from the start alone left every such subagent with no spawner, so no
// session's live-work lineage reached it (51 live `toolu_` agents on the owner's
// store, 2026-09-23).
//
// LINEAGE AND NOTHING ELSE. The start's metadata columns are the start's; a
// conclusion does not restate them, and overwriting them with a conclusion's
// view would erase what a start already recorded. The spawner is the one fact
// both arms state identically.
//
// A SUCCESS NAMING NO CREATED AGENT STATES NO LINEAGE. The field is optional —
// a producer that could not name the created agent leaves it UNSET rather than
// inventing one — so there is nothing to record and nothing wrong.
func (d *DB) recordSettledSpawnLineage(ctx context.Context, tx *sql.Tx, spawnedBy, created string, now int64) error {
	fields := logging.Fields{Operation: "store.db.write-batch", Table: "agent", AgentID: created}
	if created == "" {
		fields.AgentID = spawnedBy
		d.log.LogVerbose(fields, "a settled spawn names no created agent; it states no lineage")
		return nil
	}
	const upsertSQL = `INSERT INTO agent (agent_id, spawned_by_agent, started_at_ms) VALUES (?, ?, ?)
	  ON CONFLICT(agent_id) DO UPDATE SET spawned_by_agent = excluded.spawned_by_agent`
	if _, err := tx.ExecContext(ctx, upsertSQL, created, spawnedBy, now); err != nil {
		return d.queryError("store.db.write-batch", "agent", fields, storagef(err, "recording the spawner of settled spawn %q", created))
	}
	d.log.LogVerbose(fields, "spawn lineage recorded from the settled spawn spawned_by=%q", spawnedBy)
	return nil
}

// endAgent writes the terminal columns. `terminal` is the WHOLE serialized
// AgentFrame rather than the bare arm: a oneof arm is not a message and cannot
// be serialized alone, and the frame is self-describing, so a reader recovers
// which of the two terminals it was without a second column saying so.
func (d *DB) endAgent(ctx context.Context, tx *sql.Tx, agentID string, terminal []byte, now int64) error {
	const endSQL = `UPDATE agent SET ended_at_ms = ?, terminal = ? WHERE agent_id = ?`
	if _, err := tx.ExecContext(ctx, endSQL, now, terminal, agentID); err != nil {
		return d.queryError("store.db.write-batch", "agent", logging.Fields{AgentID: agentID}, storagef(err, "ending agent %q", agentID))
	}
	d.log.LogVerbose(logging.Fields{Operation: "store.db.write-batch", Table: "agent", AgentID: agentID}, "agent terminal recorded")
	return nil
}

// announceDetachedWork records the JOIN ROW for one AgentDetachedWork.
//
// THE ANNOUNCEMENT ITSELF IS THE PAGE LINE, and this row is only what the store
// filters and joins on. The spool path, the readability, the detach cause and
// the timeout live in that page line and nowhere else: unpacking them here as
// well would give one fact two homes that can disagree.
func (d *DB) announceDetachedWork(ctx context.Context, tx *sql.Tx, owner string, work *conversationv1.AgentDetachedWork, now int64) error {
	kind, err := validateDetachedWork(work, 0)
	if err != nil {
		return err
	}
	var originUnit sql.NullString
	switch arm := work.GetOrigin().(type) {
	case *conversationv1.AgentDetachedWork_Detached:
		originUnit = sql.NullString{String: arm.Detached.GetDetachedFromId().GetValue(), Valid: true}
	case *conversationv1.AgentDetachedWork_Created:
		// The created unit's own id, where the unit HAS one. Only a subagent
		// does: a bash run, a monitor and a workflow start carry no unit
		// identity in DetachableWork, and inventing one would fabricate a join.
		if subagent, ok := arm.Created.GetWorkCreated().GetWork().(*conversationv1.DetachableWork_Subagent); ok {
			if start, ok := subagent.Subagent.GetResult().(*conversationv1.AgentSubagent_Start); ok {
				if created := start.Start.GetCreatedAgentId().GetValue(); created != "" {
					originUnit = sql.NullString{String: created, Valid: true}
				}
				if err := d.createSpawnedAgent(ctx, tx, owner, start.Start, now); err != nil {
					return err
				}
			}
		}
	}
	// A run frame may already have created this run's row under the run's own
	// identity, so the announcement joins it rather than opening a second one.
	workID, err := d.resolveDetachedRowKey(ctx, tx, originUnit, work.GetWork().GetValue())
	if err != nil {
		return err
	}
	return d.upsertDetachedWork(ctx, tx, detachedRow{
		workID:     workID,
		kind:       kind,
		originUnit: originUnit,
		ownerAgent: sql.NullString{String: owner, Valid: owner != ""},
		now:        now,
	})
}

// applyBashLifecycle records a detached shell run's frame against the run's own
// row. Never a page line: the shell CALL in the spawning agent's book is the
// page line, and this frame is that run's state.
func (d *DB) applyBashLifecycle(ctx context.Context, tx *sql.Tx, bash *storev1.StoreAgentBash, now int64) error {
	runID := bash.GetRun().GetValue()
	state, err := proto.Marshal(bash.GetFrame())
	if err != nil {
		return invalidFieldf("agent_update.bash.frame", "bash frame for run %q cannot be re-serialized: %v", runID, err)
	}
	// The run IS the unit, so the origin join is the identity itself — which is
	// what lets a terminal on the spawning stream close this row. The row may
	// already exist under the ANNOUNCEMENT's handle, which is a different
	// string; resolving by origin unit finds it instead of opening a second row.
	origin := sql.NullString{String: runID, Valid: true}
	workID, err := d.resolveDetachedRowKey(ctx, tx, origin, runID)
	if err != nil {
		return err
	}
	if err := d.upsertDetachedWork(ctx, tx, detachedRow{
		workID:     workID,
		kind:       detachedKindBash,
		originUnit: origin,
		now:        now,
	}); err != nil {
		return err
	}
	switch bash.GetFrame().GetResult().(type) {
	case *conversationv1.AgentBash_Success, *conversationv1.AgentBash_Failure:
		return d.endDetachedWork(ctx, tx, workID, state, now)
	}
	return nil
}

// resolveDetachedRowKey answers which detached_work row a write belongs to.
//
// THE ORIGIN UNIT IS TRIED FIRST, AND THAT IS THE WHOLE POINT. The two writers
// of this table address the same run by DIFFERENT identities: the announcement
// knows the DetachedWorkId handle, and the run's own frames know the
// AgentActivityId of the run. Those strings are not required to be equal, and
// keying each writer by the identity it happens to hold produced TWO rows for
// one run — which GetLiveWork then reported as two open obligations, so the
// shim had to resolve a run that did not exist.
//
// Either writer may arrive first (the file plane can observe a spool before the
// stream plane announces it), so the rule is symmetric: if any row already
// carries this origin unit, that row IS the run and its key is returned;
// otherwise the caller's own identity keys it, and the later writer will find
// it through this same lookup. One indexed lookup on detached_work(origin_unit).
func (d *DB) resolveDetachedRowKey(ctx context.Context, tx *sql.Tx, originUnit sql.NullString, fallback string) (string, error) {
	if !originUnit.Valid || originUnit.String == "" {
		return fallback, nil
	}
	var existing string
	switch err := tx.QueryRowContext(ctx,
		`SELECT work_id FROM detached_work WHERE origin_unit = ? LIMIT 1`, originUnit.String).Scan(&existing); {
	case err == nil:
		return existing, nil
	case isNoRows(err):
		return fallback, nil
	default:
		return "", d.queryError("store.db.write-batch", "detached_work", logging.Fields{ActivityID: originUnit.String},
			storagef(err, "locating the detached work row of origin unit %q", originUnit.String))
	}
}

// detachedRow is one upsert of the detached_work table: the join columns only.
type detachedRow struct {
	workID     string
	kind       string
	originUnit sql.NullString
	ownerAgent sql.NullString
	now        int64
}

// upsertDetachedWork writes one detached run's join row.
//
// COALESCE ON EVERY OPTIONAL COLUMN, and the kind never downgrades: the two
// writers of this table see different halves of the same run. The announcement
// knows the kind and the origin unit; the run's own frames know the run
// identity and, for a `detached` origin, no kind at all. Letting either clobber
// the other's half with a NULL would lose the only copy of it.
func (d *DB) upsertDetachedWork(ctx context.Context, tx *sql.Tx, row detachedRow) error {
	const upsertSQL = `INSERT INTO detached_work (
	    work_id, kind, origin_unit, owner_agent, announced_at_ms)
	  VALUES (?,?,?,?,?)
	  ON CONFLICT(work_id) DO UPDATE SET
	    kind = CASE WHEN excluded.kind = ? THEN detached_work.kind ELSE excluded.kind END,
	    origin_unit = COALESCE(excluded.origin_unit, detached_work.origin_unit),
	    owner_agent = COALESCE(excluded.owner_agent, detached_work.owner_agent)`
	_, err := tx.ExecContext(ctx, upsertSQL,
		row.workID, row.kind, row.originUnit, row.ownerAgent, row.now, detachedKindDetached)
	if err != nil {
		return d.queryError("store.db.write-batch", "detached_work", logging.Fields{TaskID: row.workID}, storagef(err, "recording detached work %q", row.workID))
	}
	d.log.LogVerbose(logging.Fields{Operation: "store.db.write-batch", Table: "detached_work", TaskID: row.workID},
		"detached work join row recorded kind=%s", row.kind)
	return nil
}

// endDetachedWork writes a detached run's terminal columns by its own handle.
func (d *DB) endDetachedWork(ctx context.Context, tx *sql.Tx, workID string, terminal []byte, now int64) error {
	const endSQL = `UPDATE detached_work SET ended_at_ms = ?, terminal = ? WHERE work_id = ?`
	if _, err := tx.ExecContext(ctx, endSQL, now, terminal, workID); err != nil {
		return d.queryError("store.db.write-batch", "detached_work", logging.Fields{TaskID: workID}, storagef(err, "ending detached work %q", workID))
	}
	d.log.LogVerbose(logging.Fields{Operation: "store.db.write-batch", Table: "detached_work", TaskID: workID}, "detached work terminal recorded")
	return nil
}

// closeDetachedByOrigin ends every detached run whose ORIGIN UNIT just reached
// a terminal arm on the stream that spawned it.
//
// THE JOIN IS ONE INDEXED LOOKUP, which is the whole reason origin_unit is a
// column: the alternative is walking the run's lineage, and the store holds no
// lineage by design.
func (d *DB) closeDetachedByOrigin(ctx context.Context, tx *sql.Tx, activityID string, terminal []byte, now int64) error {
	if activityID == "" {
		return nil
	}
	const closeSQL = `UPDATE detached_work SET ended_at_ms = ?, terminal = ?
	  WHERE origin_unit = ? AND ended_at_ms IS NULL`
	result, err := tx.ExecContext(ctx, closeSQL, now, terminal, activityID)
	if err != nil {
		return d.queryError("store.db.write-batch", "detached_work", logging.Fields{ActivityID: activityID}, storagef(err, "closing detached work by origin unit %q", activityID))
	}
	closed, err := result.RowsAffected()
	if err != nil {
		return d.queryError("store.db.write-batch", "detached_work", logging.Fields{ActivityID: activityID}, storagef(err, "counting detached work closed by origin unit %q", activityID))
	}
	if closed > 0 {
		d.log.LogVerbose(logging.Fields{Operation: "store.db.write-batch", Table: "detached_work", ActivityID: activityID},
			"detached work closed by its origin unit's terminal rows=%d", closed)
	}
	return nil
}

func optionalString(value *string) sql.NullString {
	if value == nil {
		return sql.NullString{}
	}
	return sql.NullString{String: *value, Valid: true}
}

func nullableString(value string) sql.NullString {
	if value == "" {
		return sql.NullString{}
	}
	return sql.NullString{String: value, Valid: true}
}

func optionalUint32(value *uint32) sql.NullInt64 {
	if value == nil {
		return sql.NullInt64{}
	}
	return sql.NullInt64{Int64: int64(*value), Valid: true}
}

func requestedModel(prompt *conversationv1.AgentSubagentPrompt) sql.NullString {
	if prompt.GetRequestedModel() == nil {
		return sql.NullString{}
	}
	return sql.NullString{String: prompt.GetRequestedModel().GetName(), Valid: true}
}

// isolationKind names the set arm of AgentSubagentPrompt.isolation. The arm IS
// the isolation, so the column holds the arm name and never a flag beside it.
func isolationKind(prompt *conversationv1.AgentSubagentPrompt) sql.NullString {
	switch prompt.GetIsolation().(type) {
	case *conversationv1.AgentSubagentPrompt_None:
		return sql.NullString{String: "none", Valid: true}
	case *conversationv1.AgentSubagentPrompt_Worktree:
		return sql.NullString{String: "worktree", Valid: true}
	case *conversationv1.AgentSubagentPrompt_Remote:
		return sql.NullString{String: "remote", Valid: true}
	default:
		return sql.NullString{}
	}
}
