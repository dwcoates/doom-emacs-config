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
import { existsSync, mkdirSync, readFileSync, renameSync } from "node:fs";
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
const FORKABLE_TYPES: ReadonlySet<string> = new Set(["user", "assistant", "attachment", "system", "progress"]);

/** Every forkable record's uuid in `file`, in file order. */
export function forkableRecordUuids(file: string): string[] {
  const uuids: string[] = [];
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
    const { type, uuid } = record as { type?: unknown; uuid?: unknown };
    if (typeof type === "string" && FORKABLE_TYPES.has(type) && typeof uuid === "string" && uuid !== "") {
      uuids.push(uuid);
    }
  }
  return uuids;
}

/**
 * The last record the fork carries, from the point: undefined for the whole
 * transcript, or the refusal when the point's record is not in it.
 */
export function resolveUpTo(
  uuids: readonly string[],
  point: shimv1.ForkPoint,
): { readonly kind: "ok"; readonly upTo: string | undefined } | { readonly kind: "absent"; readonly detail: string } {
  switch (point.extent.case) {
    case "whole":
      return { kind: "ok", upTo: undefined };
    case "throughRecord": {
      const record = point.extent.value.record?.value ?? "";
      return uuids.includes(record)
        ? { kind: "ok", upTo: record }
        : { kind: "absent", detail: `the fork point's record ${record} is not in the parent transcript` };
    }
    case "beforeRecord": {
      const record = point.extent.value.record?.value ?? "";
      const at = uuids.indexOf(record);
      if (at < 0) return { kind: "absent", detail: `the turn-opening record ${record} is not in the parent transcript` };
      if (at === 0) {
        return { kind: "absent", detail: `the turn-opening record ${record} is the parent's first; no finished turn precedes it` };
      }
      return { kind: "ok", upTo: uuids[at - 1] };
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
  const upTo = resolveUpTo(forkableRecordUuids(parentFile), ask.point);
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
  LOGGER.info(
    { ...context, up_to: upTo.upTo ?? "", vendor_session_id: forkedId, parent_book: parentBook, transcript: filed },
    "forked the parent conversation through the vendor and filed it under this workspace",
  );
  return { kind: "forked", vendorSessionId: forkedId, parentBook };
}
