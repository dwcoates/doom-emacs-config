/**
 * fake/rollback.ts — the mocked vendor's half of a rollback: the truncating
 * resume's boot-time judgement, and file checkpointing's `rewindFiles`.
 *
 * Both read the vendor's own transcript, as the CLI does, through the one chain
 * reader the shim plans its cut with (`engine/rollback.ts`), so both sides of
 * an offline test agree on what the conversation is. What each side DECIDES
 * stays its own: the vendor's guard is modeled here from sdk.d.ts
 * (`Options.resumeDropsTurn`), not from the shim's prompt predicate.
 */
import {
  isInterruptMarker,
  readTranscriptChain,
  RESUME_DROPS_TURN_REFUSAL_PREFIX,
  type ChainRecord,
} from "../engine/rollback.js";
import type { RewindFilesResultLike } from "../sdk/types.js";

/** How a truncating resume boots: on through, or refused in the vendor's words. */
export type TruncatingResume = { readonly kind: "booted" } | { readonly kind: "refused"; readonly message: string };

/**
 * THE CLI'S HEADLESS BOOT, for a resume naming `resumeSessionAt`.
 *
 * The CLI loads the conversation from the file's head, finds the fork point on
 * it, and — when `resumeDropsTurn` names the dropped turn's prompt — refuses
 * unless every entry past the fork point is that turn's. Both refusals are the
 * CLI's own sentences: an unknown fork point is `No message found with
 * message.uuid of: <uuid>`, a guard refusal starts with
 * {@link RESUME_DROPS_TURN_REFUSAL_PREFIX}.
 */
export function judgeTruncatingResume(
  transcript: string,
  resumeSessionAt: string,
  resumeDropsTurn: string | undefined,
): TruncatingResume {
  const read = readTranscriptChain(transcript);
  const chain = read.kind === "ok" ? read.transcript.chain : [];
  const at = chain.findIndex((record) => record.uuid === resumeSessionAt);
  if (at < 0) return { kind: "refused", message: `No message found with message.uuid of: ${resumeSessionAt}` };
  if (resumeDropsTurn === undefined) return { kind: "booted" };
  const foreign = chain.slice(at + 1).find((record) => !attributable(record, resumeDropsTurn));
  if (foreign === undefined) return { kind: "booted" };
  return {
    kind: "refused",
    message:
      `${RESUME_DROPS_TURN_REFUSAL_PREFIX} resuming at ${resumeSessionAt} would discard entries not ` +
      `attributable to turn ${resumeDropsTurn}: user entry ${foreign.uuid} belongs to another turn`,
  };
}

/**
 * Whether a discarded entry is the dropped turn's own: the turn's prompt, or
 * anything that is not a user message of its own — the turn's answers, its
 * tool-result carriers, the harness's meta records and the interrupt marker.
 * Any OTHER user message (another prompt, a queued message, a task
 * notification, a keep-alive) is not.
 */
function attributable(record: ChainRecord, droppedPrompt: string): boolean {
  if (record.uuid === droppedPrompt) return true;
  if (record.type !== "user" || record.isMeta === true || record.isCompactSummary === true) return true;
  const content = record.message?.content;
  if (Array.isArray(content) && content.some((block) => (block as { type?: unknown }).type === "tool_result")) {
    return true;
  }
  return typeof content === "string" && isInterruptMarker(content);
}

/** The edit tools whose changes file checkpointing tracks, and their path field. */
const EDIT_TOOL_PATHS: Readonly<Record<string, string>> = {
  Write: "file_path",
  Edit: "file_path",
  MultiEdit: "file_path",
  NotebookEdit: "notebook_path",
};

/**
 * `Query.rewindFiles` offline: the files the main agent's edit tools touched
 * from the prompt `userMessageId` onward, read off the live chain. A message
 * the transcript holds no user record of cannot be rewound to, in the vendor's
 * `canRewind: false` shape. The mock edits no real file, so a real rewind
 * restores nothing on disk: it reports what it would have restored.
 */
export function fakeRewindFiles(transcript: string, userMessageId: string): RewindFilesResultLike {
  const read = readTranscriptChain(transcript);
  const chain = read.kind === "ok" ? read.transcript.chain : [];
  const at = chain.findIndex((record) => record.uuid === userMessageId && record.type === "user");
  if (at < 0) return { canRewind: false, error: `No file checkpoint found for message ${userMessageId}` };
  const changed = new Set<string>();
  for (const record of chain.slice(at)) {
    if (record.type !== "assistant") continue;
    const content = record.message?.content;
    if (!Array.isArray(content)) continue;
    for (const block of content as { type?: unknown; name?: unknown; input?: Record<string, unknown> }[]) {
      if (block.type !== "tool_use" || typeof block.name !== "string") continue;
      const field = EDIT_TOOL_PATHS[block.name];
      const path = field === undefined ? undefined : block.input?.[field];
      if (typeof path === "string" && path !== "") changed.add(path);
    }
  }
  return { canRewind: true, filesChanged: [...changed], insertions: 0, deletions: 0 };
}
