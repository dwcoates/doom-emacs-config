/**
 * The shim's half of the BOUNDED, BACKWARD-ANCHORED page read
 * (`agentshim/core/v1/message-page.proto`).
 *
 * WHAT THIS ADDS THAT THE OLD VOCABULARY COULD NOT SAY. `Subscribe` and
 * `ReplayRequest` both read FORWARD FROM A LOWER BOUND, so neither can express
 * "the newest N": a reader wanting recent history has to GUESS a `from_seq`
 * low enough to cover it, and since one message can own hundreds of records
 * that guess cannot be computed. That guess is where the unbounded scan comes
 * from. {@link MessagePageHead} is the missing verb — the newest page asked for
 * WITHOUT naming a seq — and `before_seq` continues below a page already
 * received by copying that page's `last_page_seq` VERBATIM, a place the caller
 * has demonstrably been.
 *
 * NEITHER OLD DOOR IS TOUCHED HERE, deliberately: the contract's ordering
 * constraint is that the bounded page must WORK before the unbounded doors
 * close. A later branch closes them.
 */
import { create } from "@bufbuild/protobuf";
import {
  MessagePage,
  MessagePageHeadSchema,
  MessagePageRequest,
  MessagePageRequestSchema,
  StoredMessage,
} from "./proto.js";

/**
 * Where a page read is anchored. Exactly two, and neither is a position the
 * caller authored: `head` names nothing at all, and `before` carries a value
 * the serving side minted.
 */
export type PageAnchor =
  | { readonly kind: "head" }
  | { readonly kind: "before"; readonly lastPageSeq: bigint };

/** Anchor at the newest message held; the cold reader's verb. */
export const HEAD_ANCHOR: PageAnchor = { kind: "head" };

/**
 * Continue below `page`, using ITS `last_page_seq` VERBATIM.
 *
 * The value is copied, never derived: subtracting one, or computing a seq from
 * the records on the page, would re-introduce exactly the caller-authored
 * position this contract removes.
 */
export function continueBelow(page: MessagePage): PageAnchor {
  return { kind: "before", lastPageSeq: page.lastPageSeq };
}

/**
 * Build the request for `anchor`, tagged with `requestId` for correlation and
 * routed by `sessionId`.
 *
 * `sessionId` is the VENDOR session id, exactly as `Subscribe` carries it. The
 * store holds every live session in one database and scopes seq, dedup and
 * fan-out by it, so a request that named no session could not be answered at
 * all. It is a ROUTING key and never a position: it says WHICH history is
 * read, and nothing about where in that history the read starts.
 *
 * An EMPTY session id THROWS rather than travelling. The store would refuse it,
 * and a refusal arriving as a failed page is far less legible than the caller
 * discovering here that it does not yet know which conversation it is reading.
 */
export function messagePageRequest(
  requestId: string,
  sessionId: string,
  anchor: PageAnchor,
): MessagePageRequest {
  if (sessionId === "") {
    throw new Error("message page request requires a vendor session id to route by");
  }
  if (anchor.kind === "head") {
    return create(MessagePageRequestSchema, {
      requestId,
      sessionId,
      anchor: { case: "head", value: create(MessagePageHeadSchema, {}) },
    });
  }
  return create(MessagePageRequestSchema, {
    requestId,
    sessionId,
    anchor: { case: "beforeSeq", value: anchor.lastPageSeq },
  });
}

/**
 * How many message slots the TYPE has. Ten, and there is no eleventh field —
 * an over-large page is unencodable rather than merely non-compliant.
 */
export const MESSAGE_PAGE_SLOTS = 10;

/**
 * The page's occupied slots, NEWEST FIRST, as a readonly list that can never
 * exceed {@link MESSAGE_PAGE_SLOTS} because it is built by reading the ten
 * discrete fields and nothing else. There is no path here by which an eleventh
 * message reaches a consumer.
 *
 * Each {@link StoredMessage} is passed through WHOLE — its `records` are never
 * split, re-chunked, or re-correlated. That correlation is precisely what this
 * shape removes.
 */
export function pageMessages(page: MessagePage): readonly StoredMessage[] {
  const slots = [
    page.message1, page.message2, page.message3, page.message4, page.message5,
    page.message6, page.message7, page.message8, page.message9, page.message10,
  ];
  const filled: StoredMessage[] = [];
  for (const slot of slots) {
    if (slot !== undefined) filled.push(slot);
  }
  return filled;
}

/**
 * What the page says about older history, and NOTHING it does not say.
 *
 * - `more` — older messages remain below this page.
 * - `retained-floor` — the store reached the oldest RETAINED record. This is
 *   the SERVING SIDE'S OWN FACT and is NOT the conversation's beginning; the
 *   two are never collapsed, and a beginning is never inferred from a short
 *   page.
 * - `unset` — the serving side set no arm at all. A protocol violation, not a
 *   third answer, so it is surfaced rather than defaulted into either arm.
 */
export type PageBoundary = "more" | "retained-floor" | "unset";

/** Read the `boundary` oneof, keeping the two arms distinct. */
export function pageBoundary(page: MessagePage): PageBoundary {
  switch (page.boundary.case) {
    case "more":
      return "more";
    case "floor":
      return "retained-floor";
    default:
      return "unset";
  }
}

/**
 * True only when the store SAID it reached the retained floor.
 *
 * Explicitly NOT "the page held fewer than ten messages": a short page is not
 * evidence of anything, and reading it as the floor (or as the conversation's
 * beginning) is the inference this contract exists to make impossible.
 */
export function reachedRetainedFloor(page: MessagePage): boolean {
  return pageBoundary(page) === "retained-floor";
}
