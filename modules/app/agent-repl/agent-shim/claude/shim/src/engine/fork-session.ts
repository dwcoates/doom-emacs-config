/**
 * engine/fork-session.ts — ForkSession: make this shim's conversation a vendor
 * fork of another, cut at a fork point, before any session starts.
 *
 * THE VENDOR WRITES THE FORK (proto/src/shim/v1/endpoint_fork_session.proto).
 * Its fork copies the parent's records with their content unchanged, so the
 * fork's first request reads the parent's whole prefix from the prompt cache; a
 * fork the shim wrote itself would be a second spelling of the vendor's format
 * that drifts from it. The vendor files the copy beside the parent's transcript,
 * and this module moves it under this shim's working directory, where a resume
 * of the new id finds it.
 *
 * NOTHING IS WRITTEN UNLESS EVERYTHING CHECKS OUT: the parent transcript, the
 * fork point's record and the parent's book are all resolved before the vendor
 * is asked, so every refusal leaves the disk as it was.
 */
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import path from "node:path";
import type { shimv1 } from "../proto.js";
import { bindLog } from "../log.js";
import { workspaceLockKey } from "../locks.js";

import { createAgentIdentityStore } from "./identity.js";
import { transcriptPath } from "./vendor-paths.js";

const LOGGER = bindLog({ component: "shim-engine-fork-session", operation: "shim.engine.fork_session" });

/** What the vendor's fork is asked for. */
export interface VendorForkSpec {
  /** The parent conversation's vendor session id. */
  readonly parentSessionId: string;
  /** The parent's working directory: the project the parent's transcript is filed under. */
  readonly parentDir: string;
  /** The last record the fork carries, inclusive. Absent: the whole transcript. */
  readonly upToMessageId?: string;
}

/**
 * The vendor's session fork: the real SDK's `forkSession`, or the mocked
 * vendor's. Answers the fork's new vendor session id, with the new transcript
 * filed beside the parent's. Throws when the vendor refuses.
 */
export type ForkVendorSession = (spec: VendorForkSpec) => Promise<string>;

/** What ForkSession needs that it does not own. */
export interface ForkSessionDeps {
  /** The account root this shim runs under (`CLAUDE_CONFIG_DIR`). */
  readonly configDir: string;
  /** The agent-repl state directory the shims keep their identities in. */
  readonly stateDir: string;
  /** This shim's working directory: where the fork is filed. */
  readonly cwd: string;
  readonly forkVendorSession: ForkVendorSession;
}

/** The parent and the point, as ForkSession's request carries them. */
export interface ForkSessionAsk {
  readonly parentVendorSessionId: string;
  readonly parentWorkspaceDir: string;
  readonly point: shimv1.ForkPoint;
}

/** What became of one fork. THE KIND IS THE OUTCOME. */
export type ForkSessionOutcome =
  | { readonly kind: "forked"; readonly vendorSessionId: string; readonly parentBook: string }
  | {
      readonly kind: "unknown_session" | "record_absent" | "parent_book_unknown" | "vendor_failed";
      readonly detail: string;
    };

/**
 * The record types the vendor's fork counts as transcript messages (its own
 * set, read from the shipped SDK): only these carry a uuid it can cut at.
 */
export const FORKABLE_TYPES: ReadonlySet<string> = new Set(["user", "assistant", "attachment", "system", "progress"]);

/**
 * One forkable record: its uuid, and the send it files when it is a
 * `queued_command` attachment.
 */
export interface ForkableRecord {
  readonly uuid: string;
  /**
   * The client uuid of the send this record files: a `queued_command`
   * attachment's `source_uuid`. Absent on every other record.
   */
  readonly queuedSend?: string;
}

/** Every forkable record in `file`, in file order. */
export function forkableRecords(file: string): ForkableRecord[] {
  const records: ForkableRecord[] = [];
  for (const line of readFileSync(file, "utf8").split("\n")) {
    const trimmed = line.trim();
    if (trimmed === "") continue;
    let record: unknown;
    try {
      record = JSON.parse(trimmed);
    } catch {
      // A half-written trailing line is what a killed CLI leaves; the vendor's
      // own reader skips it, so skipping is faithful, not lenient.
      continue;
    }
    const { type, uuid, attachment } = record as { type?: unknown; uuid?: unknown; attachment?: unknown };
    if (typeof type === "string" && FORKABLE_TYPES.has(type) && typeof uuid === "string" && uuid !== "") {
      const queuedSend = queuedCommandSource(type, attachment);
      records.push(queuedSend === undefined ? { uuid } : { uuid, queuedSend });
    }
  }
  return records;
}

/** The send a `queued_command` attachment files, or absence for any other record. */
function queuedCommandSource(type: string, attachment: unknown): string | undefined {
  if (type !== "attachment" || typeof attachment !== "object" || attachment === null) return undefined;
  const { type: kind, source_uuid: source } = attachment as { type?: unknown; source_uuid?: unknown };
  return kind === "queued_command" && typeof source === "string" && source !== "" ? source : undefined;
}

/**
 * THE RECORD A FORK-POINT TOKEN NAMES, or absence when the transcript holds
 * none.
 *
 * The parent books each of its sends under the send's CLIENT uuid
 * (engine/session.ts `noteSendRecord`), and the vendor files a send as exactly
 * one record: a `user` record under that uuid when the send opened a turn, or
 * a `queued_command` attachment of its own uuid, whose `source_uuid` is the
 * client uuid, when the send arrived mid-turn and was folded in. The SDK
 * stream never carries the attachment's uuid, so the parent cannot book it;
 * the transcript is the one place both filings are visible, and this is the
 * one place they are resolved. (Measured 2026-10-15 over every local
 * transcript: 2,660 sourced queued commands, none of whose client uuids is
 * also a record's uuid; a repeated attachment line repeats its uuid too.) A
 * send filed both ways, or as two distinct attachments, breaks that filing and
 * fails hard.
 */
export function recordOfToken(records: readonly ForkableRecord[], token: string): string | undefined {
  const direct = records.some((record) => record.uuid === token);
  const queued = [...new Set(records.filter((record) => record.queuedSend === token).map((record) => record.uuid))];
  if (direct && queued.length > 0) {
    throw new Error(`fork session: the send ${token} is filed both as a record of its own and as the queued command ${queued.join(", ")}`);
  }
  if (queued.length > 1) {
    throw new Error(`fork session: the send ${token} is filed as ${queued.length} distinct queued commands: ${queued.join(", ")}`);
  }
  return direct ? token : queued[0];
}

/**
 * The last record the fork carries, from the point: undefined for the whole
 * transcript, or the refusal when the point's record is not in it.
 */
export function resolveUpTo(
  records: readonly ForkableRecord[],
  point: shimv1.ForkPoint,
): { readonly kind: "ok"; readonly upTo: string | undefined } | { readonly kind: "absent"; readonly detail: string } {
  switch (point.extent.case) {
    case "whole":
      return { kind: "ok", upTo: undefined };
    case "throughRecord": {
      const token = point.extent.value.record?.value ?? "";
      const record = recordOfToken(records, token);
      return record !== undefined
        ? { kind: "ok", upTo: record }
        : { kind: "absent", detail: `the fork point's record ${token} is not in the parent transcript` };
    }
    case "beforeRecord": {
      const token = point.extent.value.record?.value ?? "";
      const record = recordOfToken(records, token);
      const at = record === undefined ? -1 : records.findIndex((each) => each.uuid === record);
      if (at < 0) return { kind: "absent", detail: `the turn-opening record ${token} is not in the parent transcript` };
      if (at === 0) {
        return { kind: "absent", detail: `the turn-opening record ${token} is the parent's first; no finished turn precedes it` };
      }
      return { kind: "ok", upTo: records[at - 1]?.uuid };
    }
    case undefined:
      break;
  }
  // The validator refuses a point with no extent before the engine is reached,
  // so arriving here is a defect in that guard.
  throw new Error("fork session: a fork point with no extent reached the engine");
}

/** Make the fork, or answer why not. */
export async function forkSession(deps: ForkSessionDeps, ask: ForkSessionAsk): Promise<ForkSessionOutcome> {
  const context = {
    parent_vendor_session_id: ask.parentVendorSessionId,
    parent_workspace_dir: ask.parentWorkspaceDir,
    point: ask.point.extent.case ?? "",
  };
  const parentFile = transcriptPath(deps.configDir, ask.parentWorkspaceDir, ask.parentVendorSessionId);
  if (!existsSync(parentFile)) {
    const detail = `no transcript at ${parentFile}`;
    LOGGER.error({ ...context, parent_transcript: parentFile, detail }, "the fork was refused: no parent transcript under this account root");
    return { kind: "unknown_session", detail };
  }
  const upTo = resolveUpTo(forkableRecords(parentFile), ask.point);
  if (upTo.kind === "absent") {
    LOGGER.error({ ...context, parent_transcript: parentFile, cause: upTo.detail }, "the fork was refused: the fork point is not in the parent transcript");
    return { kind: "record_absent", detail: upTo.detail };
  }
  const parentBook = await createAgentIdentityStore(deps.stateDir, workspaceLockKey(ask.parentWorkspaceDir)).read();
  if (parentBook === undefined) {
    const detail = `no persisted identity for ${ask.parentWorkspaceDir}`;
    LOGGER.error({ ...context, detail }, "the fork was refused: the parent workspace has no persisted book identity");
    return { kind: "parent_book_unknown", detail };
  }
  let forkedId: string;
  try {
    forkedId = await deps.forkVendorSession({
      parentSessionId: ask.parentVendorSessionId,
      parentDir: ask.parentWorkspaceDir,
      ...(upTo.upTo === undefined ? {} : { upToMessageId: upTo.upTo }),
    });
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    LOGGER.error({ ...context, up_to: upTo.upTo ?? "", cause: detail }, "the fork was refused: the vendor's fork failed");
    return { kind: "vendor_failed", detail };
  }
  const written = transcriptPath(deps.configDir, ask.parentWorkspaceDir, forkedId);
  const filed = transcriptPath(deps.configDir, deps.cwd, forkedId);
  if (written !== filed) {
    // The vendor answered an id whose file it did not write: an invariant of
    // its fork, so it fails loudly rather than resuming nothing.
    if (!existsSync(written)) throw new Error(`fork session: the vendor answered ${forkedId} but wrote no ${written}`);
    mkdirSync(path.dirname(filed), { recursive: true });
    renameSync(written, filed);
  }
  const remapped = repairForkedTranscript(filed);
  LOGGER.info(
    { ...context, up_to: upTo.upTo ?? "", vendor_session_id: forkedId, parent_book: parentBook, transcript: filed, boundary_references_remapped: remapped },
    "forked the parent conversation through the vendor and filed it under this workspace",
  );
  return { kind: "forked", vendorSessionId: forkedId, parentBook };
}

/**
 * Point a forked transcript's compaction boundaries at the forked records,
 * rewriting the file in place; answers how many references were remapped.
 *
 * THE VENDOR'S FORK GIVES EVERY RECORD A NEW UUID (`forkedFrom.messageUuid`
 * names the parent record it was copied from) but copies each
 * `compact_boundary`'s `compactMetadata` whole, so its preserved segment and
 * preserved messages -- the tail of the conversation a resumed session
 * re-attaches after the summary -- still name the PARENT's uuids. Nothing in
 * the fork answers to them, so the fork's first request left that tail out:
 * its history differed from the parent's right after the summary, every cache
 * breakpoint after the system prompt missed, and the server dropped every
 * thinking block as `prefix_mismatch` (measured 2026-10-14: a 600k-token fork
 * read 28,550 from the cache; the same fork with the boundaries remapped read
 * 769,496 and wrote 10,020).
 *
 * ONLY THE BOUNDARY LINES ARE REWRITTEN; every other line keeps its bytes, so
 * nothing the request carries changes but the boundaries the vendor reads.
 */
export function repairForkedTranscript(file: string): number {
  const repaired = remapForkedCompactBoundaries(readFileSync(file, "utf8"));
  if (repaired.remapped === 0) return 0;
  const staged = `${file}.repairing`;
  writeFileSync(staged, repaired.text);
  renameSync(staged, file);
  return repaired.remapped;
}

/**
 * The transcript text with every `compact_boundary`'s `compactMetadata`
 * pointing at the forked records, and how many references were remapped. A
 * reference naming no forked record (a record the fork did not copy) is left
 * as it is.
 */
export function remapForkedCompactBoundaries(text: string): { text: string; remapped: number } {
  const lines = text.split("\n");
  const records = lines.map((line) => (line.trim() === "" ? undefined : (JSON.parse(line) as Record<string, unknown>)));
  const forkedOf = new Map<string, string>();
  for (const record of records) {
    const from = record?.["forkedFrom"] as { messageUuid?: unknown } | undefined;
    if (typeof from?.messageUuid === "string" && typeof record?.["uuid"] === "string") forkedOf.set(from.messageUuid, record["uuid"]);
  }
  let remapped = 0;
  const remap = (value: unknown): unknown => {
    if (typeof value === "string") {
      const forked = forkedOf.get(value);
      if (forked === undefined) return value;
      remapped++;
      return forked;
    }
    if (Array.isArray(value)) return value.map(remap);
    if (value !== null && typeof value === "object") {
      return Object.fromEntries(Object.entries(value).map(([key, inner]) => [key, remap(inner)]));
    }
    return value;
  };
  const out = lines.map((line, at) => {
    const record = records[at];
    if (record?.["type"] !== "system" || record["subtype"] !== "compact_boundary" || record["compactMetadata"] === undefined) return line;
    const before = remapped;
    const metadata = remap(record["compactMetadata"]);
    return remapped === before ? line : JSON.stringify({ ...record, compactMetadata: metadata });
  });
  return { text: out.join("\n"), remapped };
}
