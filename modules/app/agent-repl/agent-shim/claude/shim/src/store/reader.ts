/**
 * store/reader.ts — the READ half of the record plane.
 *
 * THE SHIM SERVES HISTORY FROM THE STORE, NEVER FROM MEMORY. That is the whole
 * point of this file: a shim that answered `WatchAgent` out of its own
 * recollection would be a second, divergent copy of the record, and a bounce
 * would lose it. Everything here is a translation of store.v1's page vocabulary
 * into conversation.v1's, and nothing here remembers a conversation.
 *
 * # Open-then-watch is ONE act
 *
 * The store pins a watch token at the instant of the open, so the tail begins
 * exactly after the opening page — nothing missed, nothing doubled. A caller
 * that opened and watched separately would have a race, so this file never
 * exposes the two apart.
 *
 * # The refused-open convention
 *
 * A watch token can be unknown: consumed, or minted by a store that has since
 * restarted. A stream has no failure message to carry that (its response type is
 * the frame), so the refusal arrives as a Connect `NotFound` and the answer is
 * to RE-OPEN with `known_through` set to the last pointer actually served. That
 * is why the tail tracks its high-water mark: it is the only thing that makes
 * the re-open lossless.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import { PersistenceError, type AgentPageSession } from "./persistence.js";

const LOGGER = bindLog({ component: "shim-store-reader", operation: "shim.store.reader" });

/** The component name every fault from this half carries. */
export const READER_COMPONENT = "store-reader";

// ---------------------------------------------------------------------------
// store.v1 → conversation.v1: one function per message (the proto→code mapping)
// ---------------------------------------------------------------------------

/** The store's pointer as the history pointer the daemon echoes. */
export function toHistoryPointer(pointer: storev1.StoreItemPointer): conversationv1.HistoryPointer {
  if (pointer.value === "") {
    throw new PersistenceError("stale_pointer", "the store served an empty item pointer");
  }
  return create(conversationv1.HistoryPointerSchema, { value: pointer.value });
}

/** The history pointer back as the store's own, for a request. */
export function toStorePointer(pointer: conversationv1.HistoryPointer): storev1.StoreItemPointer {
  if (pointer.value === "") {
    throw new PersistenceError("stale_pointer", "a history pointer is never the empty string");
  }
  return create(storev1.StoreItemPointerSchema, { value: pointer.value });
}

/** One stored line as the history entry it renders. */
export function toHistoryEntry(line: storev1.StorePageLine): conversationv1.HistoryEntry {
  const item = line.agentItem?.item;
  switch (item?.case) {
    case "agentPrompt":
      return create(conversationv1.HistoryEntrySchema, {
        entry: { case: "userPrompt", value: item.value },
      });
    case "agentFrame":
      return create(conversationv1.HistoryEntrySchema, {
        entry: { case: "agentFrame", value: item.value },
      });
    default:
      // AN UNSET ONEOF IS ILLEGAL AT THE CONSUMER, loudly: a line that is
      // neither a prompt nor a frame cannot be drawn, and forwarding it as an
      // empty entry would put a blank row in someone's feed.
      throw new PersistenceError(
        "store_unavailable",
        "the store served a page line whose item arm is unset",
      );
  }
}

/** One stored line and its position. */
export function toHistoryEntryAt(line: storev1.StoreLineAt): conversationv1.HistoryEntryAt {
  if (line.at === undefined || line.line === undefined) {
    throw new PersistenceError(
      "store_unavailable",
      "the store served a line with no pointer or no content",
    );
  }
  return create(conversationv1.HistoryEntryAtSchema, {
    at: toHistoryPointer(line.at),
    entry: toHistoryEntry(line.line),
  });
}

/** The page's completeness arm, in the history vocabulary. */
export function toHistoryBoundary(
  boundary: storev1.AgentSessionPage["boundary"] | storev1.ReadAgentPageSuccess["boundary"],
): conversationv1.HistoryPage["boundary"] {
  switch (boundary.case) {
    case "more":
      if (boundary.value.lastItem === undefined) {
        throw new PersistenceError(
          "store_unavailable",
          "the store said older lines remain but named no pointer to walk from",
        );
      }
      return {
        case: "more",
        value: create(conversationv1.HistoryMoreSchema, {
          lastEntry: toHistoryPointer(boundary.value.lastItem),
        }),
      };
    case "floor":
      return { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) };
    default:
      throw new PersistenceError(
        "store_unavailable",
        "the store served a page with no boundary arm set",
      );
  }
}

/** The opening page, whole. */
export function toHistoryPage(page: storev1.AgentSessionPage): conversationv1.HistoryPage {
  return create(conversationv1.HistoryPageSchema, {
    entries: page.lines.map(toHistoryEntryAt),
    boundary: toHistoryBoundary(page.boundary),
  });
}

/**
 * The budget a REFUSED-OPEN recovery re-opens with.
 *
 * Deliberately NOT the caller's own page size. The re-open's page is what
 * carries everything written during the gap, so a small budget — a caller that
 * asked for a page of zero, say — would leave those entries neither in the page
 * nor in the tail, because the new watch token is pinned after the NEWEST item
 * the store holds. A generous budget makes the recovery lossless in every
 * realistic gap, and a gap that still exceeds it is reported LOUDLY rather than
 * silently skipped.
 */
export const CATCHUP_PAGE_SIZE = 1024;

// ---------------------------------------------------------------------------
// Failure translation
// ---------------------------------------------------------------------------

/** A store refusal, as the kind the engine switches on. */
export function readFailure(detail: string): PersistenceError {
  // The store's `kind` arms on these failures are DERIVED at this wave and are
  // not yet declared, so the detail string is all there is to classify by. The
  // classification stays here, in one place, rather than at every call site.
  const lowered = detail.toLowerCase();
  if (lowered.includes("unknown agent") || lowered.includes("no such agent")) {
    return new PersistenceError("unknown_agent", detail);
  }
  if (lowered.includes("pointer")) return new PersistenceError("stale_pointer", detail);
  return new PersistenceError("store_unavailable", detail);
}

/** A thrown transport error, as the kind the engine switches on. */
export function transportFailure(error: unknown): PersistenceError {
  if (error instanceof PersistenceError) return error;
  const detail = error instanceof Error ? error.message : String(error);
  return new PersistenceError("store_unavailable", detail);
}

/** Whether a thrown error is the store's "I do not know this token" refusal. */
export function isNotFound(error: unknown): boolean {
  return error instanceof ConnectError && error.code === Code.NotFound;
}

// ---------------------------------------------------------------------------
// The reader
// ---------------------------------------------------------------------------

/** The read half, plus the two notes the write half feeds it about shell runs. */
export interface Reader {
  openAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
  ): Promise<AgentPageSession>;
  readAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    after: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage>;
  openBashRun(work: conversationv1.DetachedWorkId): Promise<AsyncIterable<conversationv1.AgentBash>>;
  /** Remember which run a detached-work handle names, from its announcement. */
  linkWork(workValue: string, runValue: string): void;
  /** Relay one shell-run frame to whoever is watching that run. */
  noteBashFrame(runValue: string, frame: conversationv1.AgentBash): void;
}

/** What a reader needs to exist. */
export interface ReaderOptions {
  readonly client: StoreClient;
}

export function createReader(options: ReaderOptions): Reader {
  const client = options.client;
  /** DetachedWorkId → the run's AgentActivityId, from the announcement. */
  const workToRun = new Map<string, string>();

  const openSession = async (
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
  ): Promise<storev1.OpenAgentSessionSuccess> => {
    let response: storev1.OpenAgentSessionResponse;
    try {
      response = await client.openAgentSession(
        create(storev1.OpenAgentSessionRequestSchema, {
          agent,
          pageSize,
          knownThrough: knownThrough === undefined ? undefined : toStorePointer(knownThrough),
        }),
      );
    } catch (error) {
      LOGGER.log(
        { level: "error", agent: agent.value, detail: String(error) },
        "the store could not be reached to open an agent's book",
      );
      throw transportFailure(error);
    }
    const result = response.result;
    if (result.case === "success") return result.value;
    if (result.case === "failure") {
      LOGGER.log(
        { level: "warn", agent: agent.value, detail: result.value.detail },
        "the store refused to open an agent's book",
      );
      throw readFailure(result.value.detail);
    }
    throw new PersistenceError(
      "store_unavailable",
      "the store answered OpenAgentSession with no result arm set",
    );
  };

  return {
    async openAgentPage(agent, pageSize, knownThrough) {
      const opened = await openSession(agent, pageSize, knownThrough);
      if (opened.page === undefined || opened.watch === undefined) {
        throw new PersistenceError(
          "store_unavailable",
          "the store opened a reading session with no page or no watch token",
        );
      }
      const page = toHistoryPage(opened.page);
      LOGGER.log(
        { agent: agent.value, page_size: pageSize, entries: page.entries.length },
        "opened an agent's book and pinned its tail",
      );

      // The caller's high-water mark, kept so a refused re-open is lossless.
      let servedThrough: conversationv1.HistoryPointer | undefined =
        page.entries[0]?.at ?? knownThrough;
      let token: storev1.AgentSessionToken = opened.watch;
      let stopped = false;
      // CANCELLING THE CALL IS HOW A STANDING TAIL ENDS. Connect's stream close
      // drains the body, which on a standing stream never completes — so
      // `close()` aborts the call rather than merely leaving the loop.
      let abort = new AbortController();

      const tail: AsyncIterable<conversationv1.HistoryEntryAt> = {
        async *[Symbol.asyncIterator]() {
          for (;;) {
            if (stopped) return;
            try {
              for await (const push of client.watchAgentSession(
                create(storev1.WatchAgentSessionRequestSchema, { watch: token }),
                abort.signal,
              )) {
                if (stopped) return;
                if (push.line === undefined) {
                  throw new PersistenceError(
                    "store_unavailable",
                    "the store pushed a watch frame with no line",
                  );
                }
                const entry = toHistoryEntryAt(push.line);
                servedThrough = entry.at;
                yield entry;
              }
              // A tail that ends without a refusal is the store closing; a
              // standing stream concludes nothing on its own, so stop.
              return;
            } catch (error) {
              if (stopped) return;
              if (!isNotFound(error)) throw transportFailure(error);
              // THE REFUSED-OPEN CONVENTION: an unknown token means the store
              // forgot the session (a restart, a consumed token). Re-open from
              // the last pointer actually served — the only thing that makes
              // the recovery lossless — and carry on.
              LOGGER.log(
                { level: "warn", agent: agent.value, served_through: servedThrough?.value },
                "the store refused the watch token; re-opening the book from the last served pointer",
              );
              const reopened = await openSession(agent, CATCHUP_PAGE_SIZE, servedThrough);
              if (reopened.watch === undefined) {
                throw new PersistenceError(
                  "store_unavailable",
                  "the store re-opened a reading session with no watch token",
                );
              }
              // The re-open's page is bounded by `known_through`, so anything
              // it carries is newer than what was served and must be yielded
              // before the tail continues.
              if (reopened.page?.boundary.case === "more") {
                LOGGER.log(
                  { level: "error", agent: agent.value, budget: CATCHUP_PAGE_SIZE },
                  "the gap since the last served pointer exceeds the catch-up budget; entries were skipped",
                );
              }
              for (const line of [...(reopened.page?.lines ?? [])].reverse()) {
                const entry = toHistoryEntryAt(line);
                servedThrough = entry.at;
                yield entry;
              }
              token = reopened.watch;
              // A fresh controller per attempt: the aborted one stays aborted.
              abort = new AbortController();
            }
          }
        },
      };

      return {
        page,
        tail,
        close: () => {
          if (stopped) return;
          stopped = true;
          abort.abort();
        },
      };
    },

    async readAgentPage(agent, pageSize, after) {
      let response: storev1.ReadAgentPageResponse;
      try {
        response = await client.readAgentPage(
          create(storev1.ReadAgentPageRequestSchema, {
            book: agent,
            pageSize,
            after: toStorePointer(after),
          }),
        );
      } catch (error) {
        LOGGER.log(
          { level: "error", agent: agent.value, detail: String(error) },
          "the store could not be reached to read an older page",
        );
        throw transportFailure(error);
      }
      const result = response.result;
      if (result.case === "failure") {
        LOGGER.log(
          { level: "warn", agent: agent.value, detail: result.value.detail },
          "the store refused an older page",
        );
        throw readFailure(result.value.detail);
      }
      if (result.case !== "success") {
        throw new PersistenceError(
          "store_unavailable",
          "the store answered ReadAgentPage with no result arm set",
        );
      }
      LOGGER.log(
        { agent: agent.value, entries: result.value.lines.length },
        "served an older page of an agent's book",
      );
      // EVERY LINE CARRIES ITS OWN POINTER (landing 3): a continuation page is
      // a reconnect mark like any other, so nothing here is minted and nothing
      // has to be refused if a caller echoes one back.
      return create(conversationv1.HistoryPageSchema, {
        entries: result.value.lines.map(toHistoryEntryAt),
        boundary: toHistoryBoundary(result.value.boundary),
      });
    },

    async openBashRun(work) {
      const runValue = workToRun.get(work.value);
      if (runValue === undefined) {
        LOGGER.log(
          { level: "warn", work: work.value },
          "no announced shell run carries this detached-work handle",
        );
        throw new PersistenceError(
          "unknown_work",
          `no announced shell run carries the detached-work handle ${JSON.stringify(work.value)}`,
        );
      }
      // ONE PATH FOR EVERY RUN, and it is the STORE's. A detached shell's output
      // is written by the SIDECAR as deltas — no SDK route carries a byte of it
      // — so serving the run from what this shim happened to observe would show
      // a command's start and its ending with the whole middle missing.
      // `WatchBashRun` replays every stored row in write order and then follows,
      // which is exactly what a watcher of a growing spool needs, and it is the
      // same path whether this shim wrote the rows or the sidecar did.
      const run = create(conversationv1.AgentActivityIdSchema, { value: runValue });
      const abort = new AbortController();
      LOGGER.log({ run: runValue, work: work.value }, "following a shell run's stored rows");
      let opened = false;
      return {
        async *[Symbol.asyncIterator]() {
          try {
            for await (const push of client.watchBashRun(
              create(storev1.WatchBashRunRequestSchema, { run }),
              abort.signal,
            )) {
              opened = true;
              const frame = push.row?.frame;
              if (frame === undefined) {
                throw new PersistenceError(
                  "store_unavailable",
                  "the store pushed a bash row with no frame",
                );
              }
              yield frame;
            }
          } catch (error) {
            if (isNotFound(error)) {
              // A REFUSED OPEN means the store holds no row for this run — the
              // announcement reached us before the run's first row did. It is
              // an `unknown_work` refusal, not a transport failure, so the
              // caller can say so rather than reporting the store as broken.
              LOGGER.log(
                { level: "warn", run: runValue, work: work.value },
                "the store holds no rows for this shell run yet",
              );
              throw new PersistenceError(
                "unknown_work",
                `the store holds no rows for shell run ${JSON.stringify(runValue)}`,
              );
            }
            throw transportFailure(error);
          } finally {
            // CANCELLING THE CALL IS HOW A STREAM ENDS EARLY: a consumer that
            // breaks out of the loop would otherwise leave the call draining a
            // body that has not finished.
            if (opened || !abort.signal.aborted) abort.abort();
          }
        },
      };
    },

    linkWork(workValue, runValue) {
      if (workValue === "" || runValue === "") return;
      workToRun.set(workValue, runValue);
    },

    noteBashFrame(runValue, frame) {
      // NOTHING TO RELAY ANY MORE. A run is served from the store's own rows,
      // so a frame this shim wrote reaches a watcher the same way the sidecar's
      // do — through `WatchBashRun`. Kept as the writer's one observation point
      // so a frame that never reached the store is visible in the log.
      LOGGER.logVerbose(
        { run: runValue, arm: frame.result.case },
        "wrote a shell run's lifecycle row; watchers read it back from the store",
      );
    },
  };
}
