/**
 * engine/identity.ts — the main agent's identity, and what a vendor id rotation
 * does to it.
 *
 * RESPONSIBILITY. The main agent's `AgentId` is the conversation's ORIGINAL
 * vendor session id (R9). This module mints it on the first fresh start,
 * PERSISTS it under `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/agent-id.json`,
 * and reports it unchanged on every later StartSession — including after a
 * rotation that gave the conversation a new resume handle. Persistence is what
 * makes the identity survive a bounce: without it, a restarted shim would mint
 * a second AgentId for one conversation and split its book at the restart.
 *
 * # THE SETTLED R9 RULE (evidence below)
 *
 *   1. FRESH START. The shim pre-mints a uuid, passes it as `Options.sessionId`
 *      and adopts it as the AgentId. It is written to `agent-id.json` BEFORE
 *      the query is created, so a crash between mint and first record still
 *      leaves the identity recoverable.
 *   2. RESUME. The resume id IS the original id, and a file-only reader can
 *      derive it: across 1,107 real transcripts every file's `sessionId` field
 *      equals its filename, and no file carries more than one session id. A
 *      resume appends to the SAME file under the SAME id, so nothing rotates.
 *      Compaction is likewise in-place: a `compact_boundary` line carries the
 *      unchanged `sessionId` and links to its predecessor with
 *      `logicalParentUuid`. THEREFORE: when `agent-id.json` is ABSENT on a
 *      resume, the resume id is adopted as the original id — that is a
 *      derivation, not a guess, and it is logged as one.
 *   3. ROTATION AND FORK. A `/clear` (`conversation_reset`) and a
 *      `forkSession` mint a NEW vendor session id and a new transcript file,
 *      and NOTHING IN EITHER FILE LINKS THEM: the union of every key seen
 *      across those 1,107 transcripts contains no `forkedFrom`,
 *      `parentSessionId`, `resumedFrom` or equivalent, and the SDK's own
 *      `ForkSessionResult` is `{ sessionId }` alone while `SDKSessionInfo`
 *      carries no lineage field. The link exists ONLY at runtime, on
 *      `SDKConversationResetMessage { session_id, new_conversation_id }`.
 *      So the shim WRITES the link the files do not carry: one tiny pointer
 *      file per rotated id, {@link vendorLinkPath}, naming the original. A
 *      file-plane reader resolves any transcript to its book with one stat
 *      instead of a scan, and the AgentId never moves.
 */
import { randomUUID } from "node:crypto";
import { mkdirSync, readFileSync, renameSync, rmSync, writeFileSync } from "node:fs";
import path from "node:path";
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import { mainAgentId } from "../convert/ids.js";

const LOGGER = bindLog({ component: "shim-engine-identity", operation: "shim.engine.identity" });

/** The persisted identity record; the field names are the on-disk contract. */
interface AgentIdentityRecord {
  readonly original_vendor_session_id: string;
  readonly workspace_key: string;
  readonly minted_at_ms: number;
}

/** The pointer a rotated vendor id leaves behind, so the files carry the link. */
interface VendorSessionLink {
  readonly vendor_session_id: string;
  readonly original_vendor_session_id: string;
  readonly linked_at_ms: number;
}

/** The directory this workspace's shim state lives in. */
export function identityDir(stateDir: string, workspaceKey: string): string {
  return path.join(stateDir, "shim", workspaceKey);
}

/** The main agent's persisted identity file. */
export function agentIdPath(stateDir: string, workspaceKey: string): string {
  return path.join(identityDir(stateDir, workspaceKey), "agent-id.json");
}

/** The pointer file a rotated vendor session id leaves, naming the original. */
export function vendorLinkPath(
  stateDir: string,
  workspaceKey: string,
  vendorSessionId: string,
): string {
  return path.join(identityDir(stateDir, workspaceKey), "vendor-id", `${vendorSessionId}.json`);
}

/**
 * Replace a file atomically.
 *
 * A torn `agent-id.json` is worse than an absent one: absent is recoverable by
 * the resume rule, half-written parses as garbage and there is no rule for
 * that. Write beside it and rename, which is atomic within one directory.
 */
function atomicWriteJson(target: string, value: unknown): void {
  mkdirSync(path.dirname(target), { recursive: true });
  const temporary = `${target}.${process.pid}.tmp`;
  writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`, "utf8");
  renameSync(temporary, target);
}

/** The main agent's persisted id for this workspace, and how to persist it. */
export interface AgentIdentityStore {
  /** The persisted id, or absence on a first run. */
  read(): Promise<string | undefined>;
  /** Persist the original vendor session id as this conversation's agent id. */
  write(originalVendorSessionId: string): Promise<void>;
  /** Record that `vendorSessionId` belongs to this conversation's book. */
  link(vendorSessionId: string): Promise<void>;
  /**
   * Remove the persisted identity, because the start that minted it FAILED.
   *
   * The file is written before the query is created on purpose — a crash
   * between the mint and the first record must still leave the identity
   * recoverable — but a start that never reached a query left no conversation
   * for that identity to name. Keeping it would hand the next reader a
   * persisted AgentId for a conversation the vendor never opened.
   *
   * Only ever called when the file was ABSENT before the failed attempt, so it
   * can never discard an identity an earlier session established.
   */
  forget(): Promise<void>;
}

/** The file-backed store, rooted at `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/`. */
export function createAgentIdentityStore(
  stateDir: string,
  workspaceKey: string,
  nowMs: () => number = () => Date.now(),
): AgentIdentityStore {
  const file = agentIdPath(stateDir, workspaceKey);
  return {
    read(): Promise<string | undefined> {
      try {
        const parsed = JSON.parse(readFileSync(file, "utf8")) as Partial<AgentIdentityRecord>;
        const value = parsed.original_vendor_session_id;
        if (typeof value !== "string" || value === "") {
          // A present-but-unusable record is a defect, not an absence: saying
          // "no identity" here would mint a second AgentId for one conversation.
          throw new Error(
            `shim identity: ${file} holds no original_vendor_session_id; refusing to mint a second identity for one conversation`,
          );
        }
        LOGGER.debug({ file, original_vendor_session_id: value }, "read the persisted main agent identity");
        return Promise.resolve(value);
      } catch (err) {
        if ((err as NodeJS.ErrnoException).code === "ENOENT") {
          LOGGER.debug({ file }, "no persisted main agent identity for this workspace");
          return Promise.resolve(undefined);
        }
        return Promise.reject(err instanceof Error ? err : new Error(String(err)));
      }
    },
    write(originalVendorSessionId: string): Promise<void> {
      const record: AgentIdentityRecord = {
        original_vendor_session_id: originalVendorSessionId,
        workspace_key: workspaceKey,
        minted_at_ms: nowMs(),
      };
      atomicWriteJson(file, record);
      LOGGER.debug({ file, ...record }, "persisted the main agent identity");
      return Promise.resolve();
    },
    forget(): Promise<void> {
      try {
        rmSync(file);
        LOGGER.debug({ file }, "removed the identity a failed start had minted");
      } catch (err) {
        if ((err as NodeJS.ErrnoException).code === "ENOENT") return Promise.resolve();
        return Promise.reject(err instanceof Error ? err : new Error(String(err)));
      }
      return Promise.resolve();
    },

    link(vendorSessionId: string): Promise<void> {
      const target = vendorLinkPath(stateDir, workspaceKey, vendorSessionId);
      const link: VendorSessionLink = {
        vendor_session_id: vendorSessionId,
        original_vendor_session_id: "",
        linked_at_ms: nowMs(),
      };
      // The original is read back rather than passed: the caller that rotates
      // holds the NEW id, and stating the original from the persisted record
      // keeps one source of truth for it.
      const parsed = JSON.parse(readFileSync(file, "utf8")) as AgentIdentityRecord;
      atomicWriteJson(target, {
        ...link,
        original_vendor_session_id: parsed.original_vendor_session_id,
      });
      LOGGER.debug(
        { file: target, vendor_session_id: vendorSessionId, original_vendor_session_id: parsed.original_vendor_session_id },
        "wrote the vendor-session link the transcript files do not carry",
      );
      return Promise.resolve();
    },
  };
}

/**
 * Resolve a vendor session id to the conversation's ORIGINAL id, from files
 * alone.
 *
 * This is the reader the transcripts cannot provide by themselves: a rotated
 * or forked id answers from its link file, an unrotated one answers itself.
 * The file plane (the sidecar) uses it to decide which book a transcript
 * belongs to without ever asking the shim.
 */
export function resolveOriginal(
  stateDir: string,
  workspaceKey: string,
  vendorSessionId: string,
): string {
  try {
    const parsed = JSON.parse(
      readFileSync(vendorLinkPath(stateDir, workspaceKey, vendorSessionId), "utf8"),
    ) as VendorSessionLink;
    return parsed.original_vendor_session_id;
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return vendorSessionId;
    throw err;
  }
}

/** Pre-mint a vendor session id for a fresh start (`Options.sessionId`). */
export function mintVendorSessionId(): string {
  return randomUUID();
}

/**
 * The conversation's identity for the life of the process.
 *
 * The AgentId is fixed at construction and NEVER moves; only the resume handle
 * rotates. That asymmetry is the whole point of R9: an AgentId that rotated
 * would make everything recorded before the rotation unreachable under the name
 * the consumer holds.
 */
export class SessionIdentity {
  private current: string;

  private constructor(
    readonly originalVendorSessionId: string,
    private readonly store: AgentIdentityStore,
  ) {
    this.current = originalVendorSessionId;
  }

  /** A fresh conversation: mint, persist, adopt. */
  static async fresh(store: AgentIdentityStore, mint: () => string = mintVendorSessionId): Promise<SessionIdentity> {
    const minted = mint();
    await store.write(minted);
    LOGGER.debug({ vendor_session_id: minted, binding: "fresh" }, "pre-minted the vendor session id and adopted it as the main AgentId (R9)");
    return new SessionIdentity(minted, store);
  }

  /**
   * A resumed conversation: report the persisted id unchanged, or — when the
   * file is absent — ADOPT the resume id as the original.
   *
   * The adoption is sound because a resume never rotates: every real transcript
   * file's `sessionId` equals its filename, so the id being resumed is the id
   * the conversation was created under unless a rotation intervened, and a
   * rotation leaves a link file that {@link resolveOriginal} finds first.
   */
  static async resume(store: AgentIdentityStore, resumeVendorSessionId: string): Promise<SessionIdentity> {
    const persisted = await store.read();
    if (persisted === undefined) {
      LOGGER.warn(
        { vendor_session_id: resumeVendorSessionId, binding: "resume", rule: "absent_file_adopts_resume_id" },
        "no persisted identity on resume: adopting the resume id as the ORIGINAL vendor session id (R9 resume rule)",
      );
      await store.write(resumeVendorSessionId);
      const adopted = new SessionIdentity(resumeVendorSessionId, store);
      return adopted;
    }
    const identity = new SessionIdentity(persisted, store);
    identity.current = resumeVendorSessionId;
    LOGGER.debug(
      { original_vendor_session_id: persisted, vendor_session_id: resumeVendorSessionId, binding: "resume" },
      "resumed under the persisted main AgentId",
    );
    return identity;
  }

  /** The book every frame of this conversation lands on. */
  get agentId(): conversationv1.AgentId {
    return mainAgentId(this.originalVendorSessionId);
  }

  /** The vendor session id currently in force — the resume handle, not the identity. */
  get vendorSessionId(): string {
    return this.current;
  }

  /**
   * The vendor rotated the conversation's id.
   *
   * Returns the update to push, writes the link file, and leaves the AgentId
   * exactly where it was. `reason` is a KNOWN-OPEN arm with no producer in the
   * declared surface, so it stays unset.
   */
  async rotate(newVendorSessionId: string): Promise<conversationv1.SessionUpdate> {
    const previous = this.current;
    this.current = newVendorSessionId;
    await this.store.link(newVendorSessionId);
    LOGGER.warn(
      {
        previous_vendor_session_id: previous,
        vendor_session_id: newVendorSessionId,
        original_vendor_session_id: this.originalVendorSessionId,
      },
      "vendor session id ROTATED; the main AgentId is unchanged (R9)",
    );
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "identityRotated",
        value: create(conversationv1.SessionIdentityRotatedSchema, {
          previousVendorSessionId: previous,
          vendorSessionId: newVendorSessionId,
        }),
      },
    });
  }
}
