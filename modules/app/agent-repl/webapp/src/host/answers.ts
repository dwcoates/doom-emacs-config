/**
 * answers — HOW EACH OF EMACS'S WEB VIEWS ANSWERS A CALL.
 *
 * Emacs calls the host (host.ts) and the sidebar page (sidebar-page.ts)
 * through a script, and every call answers a JSON string Emacs records: the
 * call, the workspace it names, its outcome, anything the call adds, and every
 * note the document made since its last answer (lisp/webhost.el
 * `agent-repl--webhost-record-note' records each at its level). Built here
 * once, so the two documents answer in one shape, and drain the calls Emacs
 * queued before they started in one way (`runQueuedCalls`).
 */
import { log } from "../log.js";
import { errorCause } from "../error-cause.js";

/**
 * One note a document made for Emacs to record, at the level it deserves:
 * INFO for what a working document does (a page cooling, a page from another
 * build mid-rollout), WARN for what it should never see.
 */
export interface CallNote {
  readonly level: "info" | "warn";
  readonly text: string;
}

/** A document's notes and its answers, which carry them. */
export interface CallAnswers {
  /** Note TEXT at LEVEL, for the next answer to carry. */
  readonly note: (level: CallNote["level"], text: string) => void;
  /** The answer to OP for workspace ID: OUTCOME, EXTRA's fields, and every note since the last answer. */
  readonly answer: (op: string, id: string | null, outcome: string, extra?: Record<string, unknown>) => string;
}

/** Answers whose notes keep at most LIMIT, dropping the oldest first. */
export function createCallAnswers(limit: number): CallAnswers {
  let notes: CallNote[] = [];
  return {
    note: (level, text) => {
      notes.push({ level, text });
      if (notes.length > limit) notes = notes.slice(notes.length - limit);
    },
    answer: (op, id, outcome, extra = {}) => {
      const taken = notes;
      notes = [];
      return JSON.stringify({ op, id, outcome, ...extra, notes: taken });
    },
  };
}

/**
 * One queued call's answer, as the drain's record carries it: a call answer's
 * op, id and outcome (every answer is JSON, `createCallAnswers`, so one that
 * is not throws), and any other value as it is.
 */
function drainedAnswer(answer: unknown): unknown {
  if (typeof answer !== "string") return answer;
  const { op, id, outcome } = JSON.parse(answer) as { op?: unknown; id?: unknown; outcome?: unknown };
  return { op, id, outcome };
}

/**
 * Run on PAGE every call Emacs queued before the document started (QUEUE, in
 * order), and record what each answered: the answers say whether the replay
 * Emacs made at the document's commit reached it (2026-10-31). A call that
 * throws stops the start: the error names which call it was, and the boot
 * records it (boot.ts).
 */
export function runQueuedCalls<P>(queue: readonly ((page: P) => unknown)[] | undefined, page: P): void {
  const calls = queue ?? [];
  const answers = calls.map((call, index) => {
    try {
      return drainedAnswer(call(page));
    } catch (err) {
      throw new Error(`queued call ${index + 1} of ${calls.length} threw: ${errorCause(err)}`, { cause: err });
    }
  });
  log.info("the document started and ran the calls Emacs queued", {
    operation: "webhost.boot-started",
    context: { drained: answers.length, answers },
  });
}
