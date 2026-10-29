/**
 * store/update-entry.ts — the one shape of a row that carries an AgentUpdate.
 *
 * Several writers put an update frame of an agent's book in the store without
 * the vendor having written it: the permission gate's asks, the boot sweep's
 * closing rows, and the session's own page lines (a context cut, a response
 * revision). They differ in their key, their source and their turn; the row
 * around the update is the same, so it is built here, its frame by the
 * converter's own `updateFrame`.
 */
import { create } from "@bufbuild/protobuf";
import { updateFrame } from "../convert/entries.js";
import { conversationv1 } from "../proto.js";
import type { PersistEntry } from "./persistence.js";

/** What an update row is, besides the frame around its update. */
export interface UpdateEntrySpec {
  readonly agentId: conversationv1.AgentId;
  readonly upsertKey: string;
  readonly source: PersistEntry["source"];
  readonly keepalive: boolean;
  readonly turn: conversationv1.TurnId | undefined;
  readonly update: conversationv1.AgentUpdate["update"];
}

/** The store row carrying SPEC's update as a frame of its agent's book. */
export function updateEntry(spec: UpdateEntrySpec): PersistEntry {
  return {
    agentId: spec.agentId,
    upsertKey: spec.upsertKey,
    source: spec.source,
    keepalive: spec.keepalive,
    turn: spec.turn,
    item: {
      kind: "frame",
      frame: updateFrame(spec.agentId, create(conversationv1.AgentUpdateSchema, { update: spec.update })),
    },
  };
}
