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
    expect(composerSubmit).toContain("mergeGateNoticeHtml(true, store.state.mergeStatus)");
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

describe("the composer's hibernation gate", () => {
  it("consults the gate on the same one submit path the merge gate uses", () => {
    // Assert — one path is what makes both gates total; a second entry point
    // would be a second way to spend a prompt the daemon will only nack.
    expect(composerSubmit).toContain("store.state.hibernation");
  });

  it("checks hibernation BEFORE the merge lease", () => {
    // Assert — the daemon nacks a prompt on a hibernated session regardless of
    // any lease, so the sleeping session is the more fundamental answer and
    // the one the user has to act on.
    const asleep = composerSubmit.indexOf("store.state.hibernation");
    const merge = composerSubmit.indexOf("submitBlocked(store.state.mergeLeaseHeld)");
    expect(asleep).toBeLessThan(merge);
  });

  it("surfaces the explanation on a blocked attempt rather than no-opping", () => {
    // Assert — a blocked send showing nothing would read as a broken app.
    expect(composerSubmit).toContain("hibernationNoticeHtml(asleep)");
  });

  it("logs the blocked attempt through the canonical logging API", () => {
    // Assert
    expect(composerSubmit).toContain("hibernationBlockedLog(");
  });

  it("keeps the draft, so a blocked send is never a lost one", () => {
    // Assert — the blocked branch returns BEFORE the clear, which only the
    // accepted path reaches.
    const blocked = composerSubmit.indexOf("hibernationBlockedLog(");
    const cleared = composerSubmit.indexOf('input.value = ""');
    expect(blocked).toBeLessThan(cleared);
  });
});

describe("the revival gate's chrome wiring", () => {
  /** The chrome render, which repaints every state-derived surface. */
  const chrome = blocksAfter(main, "const renderChrome = (): void => {")[0]!;

  it("repaints the gate from the daemon's live state every frame", () => {
    // Assert — a pure function of state, like the two banners beside it, so
    // there is no local lifetime to unwind when the session wakes.
    expect(chrome).toContain("revivalGateHtml(");
  });

  it("disables the send button while the session is asleep", () => {
    // Assert — the two gates are independent facts and EITHER blocks.
    expect(chrome).toContain("composerEls.send.disabled = mergeHeld || asleep");
  });

  it("clears an in-flight decision only when the daemon reports the session awake", () => {
    // Assert — the pushed SessionView is the one authority on whether the
    // revive landed, so a decision cannot be settled locally.
    expect(chrome).toContain("if (s.hibernation === null) {");
  });

  it("hides the sleep verb on a session that is already asleep", () => {
    // Assert — there is nothing to hibernate, and the gate is what that
    // session is asking for instead.
    expect(chrome).toContain("hibernateEl.hidden = s.hibernation !== null");
  });

  it("marks the document while the gate stands, so chrome can paint against it", () => {
    // Assert
    expect(chrome).toContain("HIBERNATED_BODY_CLASS");
  });
});

describe("the revival decision's dispatch", () => {
  const sendRevive = blocksAfter(main, 'const sendRevive = (mode: "compactFirst" | "direct"): void => {')[0]!;

  it("sends exactly one ReviveSessionCmd for the chosen mode", () => {
    // Assert
    expect(sendRevive).toContain("dispatcher.reviveSession(workspace, mode)");
  });

  it("marks the decision pending before the send, so the gate stops offering both", () => {
    // Assert
    const marked = sendRevive.indexOf("revivePending = mode");
    const sent = sendRevive.indexOf("dispatcher.reviveSession");
    expect(marked).toBeLessThan(sent);
  });

  it("unwinds the pending mark when the daemon refuses the decision", () => {
    // Assert — a refused revive leaves the session exactly as asleep as it
    // was, so the user has to be able to choose again.
    expect(sendRevive).toContain("revivePending = null");
  });

  it("does NOT clear the gate on a successful ack", () => {
    // Assert — the ack means the daemon ACCEPTED the decision, not that the
    // session is up. Taking the gate down there would put a live composer in
    // front of a session that has no shim yet; only the pushed view can. The
    // accepted arm ARMS the expectation and clears nothing.
    const accepted = sendRevive.slice(sendRevive.indexOf(".then("), sendRevive.indexOf(".catch("));
    expect(accepted).not.toContain("revivePending = null");
  });

  it("arms the one-shot expectation on an accepted ack", () => {
    // Assert — an accepted decision whose bring-up fails is the state that
    // used to leave the gate on "Waking the session…" forever. The next pushed
    // view for this workspace is what settles it.
    expect(sendRevive).toContain("reviveWatch.arm(workspace, mode)");
  });

  it("owes no verdict on a refused decision", () => {
    // Assert — a rejected ack never reached the daemon's revival path, so no
    // view is judging it.
    expect(sendRevive).toContain("reviveWatch.disarm()");
  });

  it("drops a previous attempt's complaint before sending a new decision", () => {
    // Assert — a stale failure line beside a fresh "waking…" line would
    // describe the wrong attempt.
    const cleared = sendRevive.indexOf('reviveFailure = ""');
    const sent = sendRevive.indexOf("dispatcher.reviveSession");
    expect(cleared).toBeLessThan(sent);
  });
});

describe("the revival verdict's ingest wiring", () => {
  /** The session socket's frame handler, where every ingest batch lands. */
  const onMessage = blocksAfter(main, "      onMessage: (data) => {")[0]!;

  it("rules on the batch the store just ingested", () => {
    // Assert — the verdict is a wire fact about the daemon's next word on this
    // session, not a deadline: nothing here counts time.
    expect(onMessage).toContain("reviveWatch.observe(effects)");
  });

  it("restores the gate's buttons when an accepted revival did not take", () => {
    // Assert — choosing again is the only exit left to offer.
    expect(onMessage).toContain("revivePending = null");
  });

  it("records the failed revival once, through the workspace log", () => {
    // Assert
    expect(onMessage).toContain("reviveFailedLog(");
  });
});
