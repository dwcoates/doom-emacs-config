/**
 * engine/replays.ts — the vendor's echo of a user message, and whose it was.
 *
 * WHY THE SHIM ASKS FOR ECHOES AT ALL. Every agent session's query is launched
 * with `--replay-user-messages` (sdk/real-query.ts), so the CLI emits a
 * `{type: "user", isReplay: true}` copy of every user message it takes into a
 * turn. The shim needs exactly one kind of them: a prompt typed on claude.ai
 * (or its mobile app) through the session's Remote Control bridge. The CLI
 * queues such a prompt itself, past the shim's input, so its echo is the only
 * word the shim gets of what was said (conversation.v1
 * PROMPT_ORIGIN_REMOTE_CONTROL).
 *
 * THE SHIM'S OWN SENDS ARE ECHOED TOO, under the very uuid each was sent with
 * (every send is stamped, engine/sends.ts). {@link OwnSends} remembers those
 * uuids, and an echo naming one is {@link Replay} `own`: the session drops it
 * before ANY other reading of the message, so asking for echoes changes
 * nothing the shim did before it asked.
 *
 * WHAT A REMOTE PROMPT IS, AND WHAT IS NOT ONE. An echo under a uuid the shim
 * never sent is a REMOTE prompt only when it is a person's words on the main
 * thread:
 *   - not a subagent's (`parent_tool_use_id` set);
 *   - not synthetic (`isSynthetic`: a message the harness composed, such as a
 *     model-switch reminder);
 *   - not of another origin (`origin.kind` other than `human`: a task
 *     notification, a peer message, a channel, an auto-continuation);
 *   - not one harness tag wrapping the whole text (`<local-command-stdout>`,
 *     `<bash-input>` and the like: a command's output or a shell line the CLI
 *     replays, never a prompt);
 *   - not wordless (no text block at all).
 * Anything else foreign is `other`, which the session also drops: it has no
 * reader on the stream plane, and the transcript plane carries what it was.
 *
 * CONSTANT SIZE. {@link OwnSends} is a bounded ring
 * ({@link OWN_SENDS_REMEMBERED}); the classification itself holds nothing.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../proto.js";
import type { SdkMessage, SdkUserMessageReplay } from "../sdk/types.js";

/**
 * How many of the shim's own send uuids are remembered.
 *
 * An echo comes when the vendor takes the message into a turn: at once for a
 * send to an idle session, or after the turn ahead of it for a queued one or a
 * join. The shim holds at most one open send and the joins waiting behind it
 * (engine/sends.ts, `OPEN_SENDS_BOUND`: 64), so 256 outlives every echo still
 * owed by a wide margin while staying a constant.
 */
export const OWN_SENDS_REMEMBERED = 256;

/** The uuids the shim has sent under, newest last, bounded. */
export class OwnSends {
  private readonly uuids = new Set<string>();

  /** A send is about to be pushed under `uuid`. A re-delivery under the same uuid is the same send. */
  note(uuid: string): void {
    this.uuids.delete(uuid);
    this.uuids.add(uuid);
    while (this.uuids.size > OWN_SENDS_REMEMBERED) {
      const oldest = this.uuids.keys().next();
      if (oldest.done === true) break;
      this.uuids.delete(oldest.value);
    }
  }

  /** Whether the shim sent under `uuid` (within the remembered window). */
  has(uuid: string): boolean {
    return this.uuids.has(uuid);
  }
}

/** What one echo is. */
export type Replay =
  /** The echo of a send of the shim's own. */
  | { readonly kind: "own"; readonly uuid: string }
  /** A prompt a person typed through the Remote Control bridge. */
  | { readonly kind: "remote"; readonly uuid: string; readonly said: conversationv1.UserSaid; readonly text: string }
  /** A foreign echo that is not a person's prompt; `why` says which test it failed. */
  | { readonly kind: "other"; readonly uuid: string; readonly why: string };

/** Whether `message` is the vendor's echo of a user message. */
export function isReplay(message: SdkMessage): message is SdkUserMessageReplay {
  return message.type === "user" && (message as { isReplay?: unknown }).isReplay === true;
}

/** One harness tag wrapping the whole text: a command's output or a shell line, never a prompt. */
const WHOLLY_TAGGED = /^<([a-z][a-z0-9-]*)>[\s\S]*<\/\1>$/;

/** The echo's text: the string content, or its text blocks joined. */
function replayText(message: SdkUserMessageReplay): string {
  const content = message.message.content;
  if (typeof content === "string") return content;
  return content
    .map((block) => (block.type === "text" ? block.text : ""))
    .filter((text) => text !== "")
    .join("\n");
}

/** Classify one echo against the shim's own sends. See the file comment for every test. */
export function classifyReplay(message: SdkUserMessageReplay, own: OwnSends): Replay {
  const uuid = message.uuid;
  if (own.has(uuid)) return { kind: "own", uuid };
  if (message.parent_tool_use_id !== null) return { kind: "other", uuid, why: "a subagent's message" };
  if (message.isSynthetic === true) return { kind: "other", uuid, why: "a synthetic message the harness composed" };
  const origin = message.origin;
  if (origin !== undefined && origin.kind !== "human") {
    return { kind: "other", uuid, why: `a message of origin ${origin.kind}` };
  }
  const text = replayText(message);
  if (text.trim() === "") return { kind: "other", uuid, why: "a message with no text" };
  if (WHOLLY_TAGGED.test(text.trim())) return { kind: "other", uuid, why: "one harness tag wrapping the whole text" };
  return { kind: "remote", uuid, said: remoteSaid(text), text };
}

/** What a remote prompt said, as the prompt row carries it: its text, as one block. */
function remoteSaid(text: string): conversationv1.UserSaid {
  return create(conversationv1.UserSaidSchema, {
    content: create(conversationv1.UserContentSchema, {
      blocks: [
        create(conversationv1.UserContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
        }),
      ],
    }),
  });
}
