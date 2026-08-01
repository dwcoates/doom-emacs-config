/**
 * Bootstrap contract tests.
 *
 * `main.ts` is the SPA's top-level wiring — a single bootstrap closure with
 * no importable seam — so its contracts are asserted against the module
 * source, the same way test/styles.test.ts asserts against the stylesheet.
 *
 * The contract pinned here: an inbound frame injects NOTHING. The webapp
 * once remembered a per-session "mid-task" flag in localStorage and, the
 * first time a fresh connection learned its claude session id, silently
 * submitted a hidden `/continue` prompt to pick an interrupted task back
 * up. That was pre-reattach machinery: the shim now outlives both the GUI
 * and the daemon, so an in-flight turn keeps running and reattach picks it
 * up on its own. The injection could therefore only ever fire stale,
 * spending a real turn to have the agent report it had nothing to do.
 * Reconnecting is a read of existing state and must stay one — every
 * prompt on the wire originates from a user action.
 */
import { describe, expect, it } from "vitest";

import main from "../src/main.ts?raw";

/** Bodies of every brace-balanced block introduced by `marker`. */
function blocksAfter(source: string, marker: string): string[] {
  const found: string[] = [];
  for (let at = source.indexOf(marker); at !== -1; at = source.indexOf(marker, at + 1)) {
    const open = source.indexOf("{", at);
    let depth = 0;
    for (let i = open; i < source.length; i++) {
      if (source[i] === "{") depth++;
      else if (source[i] === "}" && --depth === 0) {
        found.push(source.slice(open + 1, i));
        break;
      }
    }
  }
  if (found.length === 0) throw new Error(`main.ts has no ${marker} block`);
  return found;
}

// Every socket's inbound-frame handler: the live session's and the
// bootstrap connection's alike. A prompt injection would have to live in
// one of them, since nothing else reacts to a frame arriving.
const inboundHandlers = blocksAfter(main, "onMessage: (data) => {");

// Every webapp module, as source text, so a reintroduction anywhere in the
// frontend trips the storage assertion rather than only one in main.ts.
const sources = import.meta.glob<string>("../src/*.ts", {
  query: "?raw",
  import: "default",
  eager: true,
});

describe("inbound frame handling", () => {
  it("submits no prompt when a frame arrives", () => {
    // Assert — receiving is a read, so no handler puts words in the user's mouth.
    const offenders = inboundHandlers.filter((body) => body.includes("submitPrompt("));
    expect(offenders).toEqual([]);
  });

  it("covers every socket's handler, so no injection site is unexamined", () => {
    // Assert — the live socket and the bootstrap socket, per src/main.ts.
    expect(inboundHandlers).toHaveLength(2);
  });
});

/** The sole handler that MAY skip an unreadable frame: the bootstrap socket's. */
const bootstrapHandler = onlyHandlerSaying("bootstrap frame decode failed");
/** The live session's handler, whose decode refusal must stay fatal. */
const sessionHandler = onlyHandlerSaying("frontend frame decode/adapt threw");
/** The bootstrap handler's decode-refusal branch. */
const bootstrapCatch = blocksAfter(bootstrapHandler, "catch (err) {")[0]!;
/** The same branch with its prose removed, for assertions about control flow. */
const bootstrapCatchCode = stripLineComments(bootstrapCatch);

/** `source` minus its `//` lines, so a comment cannot answer a code question. */
function stripLineComments(source: string): string {
  return source
    .split("\n")
    .filter((line) => !line.trimStart().startsWith("//"))
    .join("\n");
}

/** The one inbound handler whose body carries `phrase`. */
function onlyHandlerSaying(phrase: string): string {
  const found = inboundHandlers.filter((body) => body.includes(phrase));
  if (found.length !== 1) throw new Error(`main.ts has ${found.length} handlers saying ${phrase}`);
  return found[0]!;
}

// A bootstrap frame carries StateSnapshots (progress views included), so a
// frame this end drops is state the user is missing. Skipping it is allowed;
// skipping it quietly is the defect these pin shut.
describe("a bootstrap frame that will not decode", () => {
  it("is reported at error level, not as a warning", () => {
    // Assert — it used to log at "warn", the level a reader filters out.
    expect(bootstrapCatch).toMatch(/clog\(\s*"error"/);
  });

  it("records the frame head, the same evidence the session socket keeps", () => {
    // Assert — the decoder's complaint alone does not say WHICH frame.
    expect(bootstrapCatch).toContain("data.slice(0, 200)");
  });

  it("mints a durable failure card, not only a log line", () => {
    // Assert — the daemon log is not a surface a user reads unprompted.
    expect(bootstrapCatch).toContain("frameUndecodableFailure(");
  });

  it("schedules the paint that puts the card on screen", () => {
    // Assert — a card merged into the store nobody re-renders is still silent.
    expect(bootstrapCatch).toContain("frames.schedule()");
  });

  it("skips the frame rather than aborting the boot", () => {
    // Assert — resilience is the point of the branch: the next frame can still
    // carry the snapshot `createSession` is waiting for.
    expect(bootstrapCatchCode).toContain("return;");
  });

  it("never re-throws, which would strand the boot on one bad frame", () => {
    // Assert — read past the prose, which is allowed to say "re-throws".
    expect(bootstrapCatchCode).not.toContain("throw");
  });
});

describe("a session frame that will not decode", () => {
  it("stays fatal, because the store it feeds is already wrong", () => {
    // Assert — the bootstrap socket's tolerance must never spread to this one.
    expect(blocksAfter(sessionHandler, "catch (err) {")[0]!).toContain("throw err;");
  });
});

describe("mid-task memory", () => {
  it("is kept by no webapp module", () => {
    // Act — the mid-task flag was localStorage-keyed, so scan every module.
    const offenders = Object.keys(sources).filter((path) =>
      /midtask|midTask|mid-task/i.test(sources[path]),
    );
    // Assert — the flag and its keys are gone for good.
    expect(offenders).toEqual([]);
  });
});

/**
 * The composer's ONE submit path, so the merge gate cannot be bypassed by a
 * second entry point. `submit` is shared by the send button's click and the
 * textarea's Enter chord; both must meet the same gate.
 */
const composerSubmit = blocksAfter(main, "const submit = (): void => {")[0]!;

describe("the composer's merge gate", () => {
  it("has exactly one submit path, which is what makes the gate total", () => {
    // Assert — a second `submit` closure would be a second way past the gate.
    expect(blocksAfter(main, "const submit = (): void => {")).toHaveLength(1);
  });

  it("consults the gate before handing a prompt to the dispatcher", () => {
    // Assert — the daemon refuses the prompt anyway; asking here is what turns
    // a vanished draft and a delayed failure card into an immediate answer.
    expect(composerSubmit).toContain("submitBlocked(store.state.mergeLeaseHeld)");
  });

  it("surfaces the explanation on a blocked attempt rather than no-opping", () => {
    // Assert — a blocked send that showed nothing would read as a broken app.
    expect(composerSubmit).toContain("mergeGateNoticeHtml(true)");
  });

  it("logs the blocked attempt through the canonical logging API", () => {
    // Assert — the record carries the same wording the user was shown.
    expect(composerSubmit).toContain("mergeGateBlockedLog(");
  });

  it("keeps the draft, so a blocked send is never a lost one", () => {
    // Assert — the blocked branch returns BEFORE the `input.value = ""` clear,
    // which only the accepted path reaches.
    const blocked = composerSubmit.indexOf("mergeGateBlockedLog(");
    const cleared = composerSubmit.indexOf('input.value = ""');
    expect(blocked).toBeLessThan(cleared);
  });
});
