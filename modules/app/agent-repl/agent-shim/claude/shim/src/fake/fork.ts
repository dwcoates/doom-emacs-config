/**
 * The mocked vendor's session fork: what the real SDK's `forkSession` does to a
 * transcript, done to the mock's own transcripts.
 *
 * FAITHFUL TO THE SHIPPED SDK (0.3.280, `sdk.mjs`): every record of the
 * vendor's transcript-message types is copied WHOLE — content, tool_use ids
 * and key order untouched — through the cut record inclusive, under a fresh
 * uuid per record with `parentUuid` remapped onto the copies, the new session
 * id, and `forkedFrom: {sessionId, messageUuid}` naming the record it was
 * copied from. `progress` records are dropped, file-history snapshots and the
 * sidebar metadata lines are not copied, and a `custom-title` line ends the
 * file. The fork is written beside the parent's transcript, as the vendor
 * writes it.
 */
import { randomUUID } from "node:crypto";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { FORKABLE_TYPES, type VendorForkSpec } from "../engine/fork-session.js";
import { transcriptPath } from "../engine/vendor-paths.js";

interface ForkableRecord {
  readonly type: string;
  readonly uuid: string;
  readonly parentUuid?: string | null;
  readonly [key: string]: unknown;
}

/**
 * Fork `spec`'s parent under the account root `configDir`; answers the fork's
 * vendor session id. Throws, as the vendor does, when the parent or the cut
 * record is missing.
 */
export function forkFakeSession(configDir: string, spec: VendorForkSpec, newId: () => string = randomUUID): string {
  const parentFile = transcriptPath(configDir, spec.parentDir, spec.parentSessionId);
  if (!existsSync(parentFile)) throw new Error(`Session ${spec.parentSessionId} not found in project directory for ${spec.parentDir}`);
  const records: ForkableRecord[] = [];
  for (const line of readFileSync(parentFile, "utf8").split("\n")) {
    if (line.trim() === "") continue;
    let parsed: unknown;
    try {
      parsed = JSON.parse(line);
    } catch {
      continue;
    }
    const record = parsed as Partial<ForkableRecord>;
    if (typeof record.type === "string" && FORKABLE_TYPES.has(record.type) && typeof record.uuid === "string") {
      records.push(record as ForkableRecord);
    }
  }
  let kept = records;
  if (spec.upToMessageId !== undefined) {
    const at = records.findIndex((record) => record.uuid === spec.upToMessageId);
    if (at < 0) throw new Error(`Message ${spec.upToMessageId} not found in session ${spec.parentSessionId}`);
    kept = records.slice(0, at + 1);
  }
  const copied = kept.filter((record) => record.type !== "progress");
  if (copied.length === 0) throw new Error(`Session ${spec.parentSessionId} has no messages to fork`);
  const forkedId = newId();
  const remapped = new Map(kept.map((record) => [record.uuid, newId()] as const));
  const lines = copied.map((record) => {
    const parent = record.parentUuid;
    const copy: Record<string, unknown> = {
      ...record,
      uuid: remapped.get(record.uuid),
      parentUuid: parent === null || parent === undefined ? null : (remapped.get(parent) ?? null),
      sessionId: forkedId,
      isSidechain: false,
      forkedFrom: { sessionId: spec.parentSessionId, messageUuid: record.uuid },
    };
    return JSON.stringify(copy);
  });
  lines.push(JSON.stringify({ type: "custom-title", sessionId: forkedId, customTitle: "Forked session (fork)", uuid: newId() }));
  writeFileSync(transcriptPath(configDir, spec.parentDir, forkedId), `${lines.join("\n")}\n`);
  return forkedId;
}
