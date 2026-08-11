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

// The prompt bubble is drawn by the webapp on the frame the user hits send,
// and the daemon's receipt only starts its breath — so the filing, the id it
// is filed under, and the take-down on a refusal are boot-scope closure state
// with no importable seam, pinned here against the source.
// The DISPATCH half of the submit path: `submitPrompt` now offers the prompt
// to the held-prompt queue first (prompt-queue.ts), and everything below —
// filing the bubble, the id it is filed under, the take-down on a refusal — is
// what happens once a prompt actually goes onto the wire, whether it went
// straight out or was drained out of the queue.
const dispatchPromptAnchor =
  "const dispatchPrompt = (\n    workspace: string,\n    text: string,\n    promptOrigin: PromptOrigin,\n  ): Promise<void> => {";
const submitPrompt = blocksAfter(main, dispatchPromptAnchor)[0]!;

describe("the local prompt bubble", () => {
  it("files the bubble on the one dispatch path every sent prompt goes through", () => {
    // Assert — a second sending path would be a prompt with no bubble.
    expect(blocksAfter(main, dispatchPromptAnchor)).toHaveLength(1);
    expect(submitPrompt).toContain("store.addLocalPrompt(requestId, text)");
  });

  it("files it BEFORE waiting on the daemon's answer", () => {
    // Assert — the whole point: the words are on screen this frame, and the
    // acknowledgement only starts the breath.
    const filed = submitPrompt.indexOf("store.addLocalPrompt(");
    const awaited = submitPrompt.indexOf("return ack.catch");
    expect(filed).toBeLessThan(awaited);
  });

  it("paints the newly filed bubble rather than waiting for the next frame's cause", () => {
    // Assert — nothing else would schedule a render until the daemon speaks.
    expect(submitPrompt).toContain("store.addLocalPrompt(requestId, text)) frames.schedule()");
  });

  it("files it under the request id the daemon's receipt will carry", () => {
    // Assert — the id is what reconciles the two onto one bubble; matching on
    // the prompt's text would be a second, drift-prone identity.
    expect(submitPrompt).toContain("const { requestId, ack } = dispatcher.submitPrompt(");
  });

  it("takes the bubble down when the daemon refuses the submit", () => {
    // Assert — a refused prompt started no turn, and a bubble left standing
    // would assert that it had.
    expect(submitPrompt).toContain("store.dropUnackedPrompt(requestId)");
  });

  it("still surfaces the refusal through the dispatcher's owned failure path", () => {
    // Assert — taking the bubble down is not a substitute for the failure card.
    expect(submitPrompt).toContain("consumeOwnedDispatchFailure(err)");
  });

  it("hands the refusal back to its caller instead of swallowing it", () => {
    // Assert — the held-prompt queue owes a drained prompt its own failure
    // card, which it can only mint if the rejection reaches it.
    expect(submitPrompt).toContain("throw err instanceof Error ? err : new Error(String(err))");
  });
});

// The held-prompt queue's wiring is boot-scope closure state with no importable
// seam, so the decisions that make a bounce imperceptible are pinned here: what
// counts as the link being down, what counts as the workspace being back, and
// where the drain is triggered from.
const promptQueueWiring = blocksAfter(main, "const promptQueue = new PromptQueue({")[0]!;
const composerSubmitPrompt = blocksAfter(
  main,
  "const submitPrompt = (text: string, promptOrigin: PromptOrigin): void => {",
)[0]!;

describe("the held-prompt queue's wiring", () => {
  it("offers every submitted prompt to the queue before dispatching it", () => {
    // Assert — a prompt that reached the dispatcher during a bounce is the
    // failure card this queue exists to prevent.
    const offered = composerSubmitPrompt.indexOf("promptQueue.offer(");
    const dispatched = composerSubmitPrompt.indexOf("dispatchPrompt(");
    expect(offered).toBeLessThan(dispatched);
  });

  it("treats anything short of a current socket as the link being down", () => {
    // Assert — a socket that is merely open has not yet proven it carries
    // authoritative state, and the reading is the ONE shared helper the
    // footer's liveness gate also takes, so the two cannot disagree about
    // whether this page is connected.
    expect(promptQueueWiring).toContain("!linkIsCurrent()");
    expect(main).toContain('const linkIsCurrent = (): boolean => (ws as WsClient | undefined)?.state === "current";');
  });

  it("gates the drain on the workspace's wired axis, not just the socket", () => {
    // Assert — draining into a severed or hibernated workspace would submit
    // into a durable replay with no controller reading it.
    expect(promptQueueWiring).toContain("drainableRenderState(store.state.renderState)");
  });

  it("refuses to drain into a hibernated workspace", () => {
    // Assert — hibernation is a gate the user must answer, not an outage.
    expect(promptQueueWiring).toContain("store.state.hibernation === null");
  });

  it("refuses to drain into a workspace other than the one the store holds", () => {
    // Assert — the store carries state for ONE workspace, so its wired axis
    // says nothing about any other.
    expect(promptQueueWiring).toContain("store.state.cwd === workspace");
  });

  it("draws a held prompt as pending rather than acking it", () => {
    // Assert — filed under the queue entry's own id, which no daemon receipt
    // can ever reconcile onto.
    expect(promptQueueWiring).toContain("store.addLocalPrompt(entry.queueId, entry.text)");
  });

  it("gives a lost held prompt its own failure card", () => {
    // Assert — never a silent drop.
    expect(promptQueueWiring).toContain("heldPromptUnsentFailure(entry.queueId, reason)");
  });

  it("drains on snapshot adoption, the edge that proves state is authoritative", () => {
    // Assert
    expect(main).toContain("void promptQueue.drain(store.state.cwd);");
  });
});

// The footer's liveness gate is boot-scope closure state with no importable
// seam either, so the two things that make the dock incapable of lying — that
// the render goes through the resolve, and that the gates are the shared
// readings rather than fresh ones — are pinned here.
const footerWiring = blocksAfter(main, "const liveness = resolveFooterLiveness(")[0]!;

describe("the footer's liveness wiring", () => {
  it("renders the dock from the resolution, never from the raw parts", () => {
    // Assert — `footer.render` takes the resolved arm, so a dock painted from
    // remembered values is not an expression that exists in this file.
    expect(main).toContain("footer.render(liveness);");
  });

  it("takes the link reading from the shared helper", () => {
    // Assert — the same reading the held-prompt queue's own gate takes.
    expect(footerWiring).toContain("linkUp: linkIsCurrent()");
  });

  it("gates on the workspace's wired axis, not just the socket", () => {
    // Assert — an unwired workspace has no live session to verify a figure
    // against, however healthy the socket is.
    expect(footerWiring).toContain(
      "wired: s.hibernation === null && drainableRenderState(s.renderState)",
    );
  });

  it("announces every cleared dock before rendering it", () => {
    // Assert — the footer going silent is never itself silent.
    const announced = main.indexOf("footerLivenessLog.observe(liveness");
    const rendered = main.indexOf("footer.render(liveness);");
    expect(announced).toBeGreaterThan(-1);
    expect(announced).toBeLessThan(rendered);
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

  it("weighs the gate by the store's own context figure", () => {
    // Assert — the same field the topbar chip prints, so the number the gate
    // prices a resume by cannot disagree with the one shown above it.
    expect(chrome).toContain("contextTokens: s.contextTokens");
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

  it("rebuilds the card ONLY when its state moved", () => {
    // Assert — an unconditional rewrite destroys whichever button the user is
    // mid-press on, and the browser then fires no click at all.
    expect(chrome).toContain("if (gateSignature !== lastRevivalGateSignature) {");
    expect(chrome).toContain("revivalGateEl.innerHTML = revivalGateHtml(");
  });

  it("writes the age as text into the standing card instead of rebuilding it", () => {
    // Assert — the age is the one thing that moves without the state moving,
    // so it must not cost the buttons their nodes once a second.
    expect(chrome).toContain("since.textContent = revivalSinceText(");
  });

  it("keeps the clock out of the signature it compares on", () => {
    // Assert — a signature carrying Date.now() would differ every frame and
    // guard nothing.
    expect(chrome).toContain("const gateSignature = revivalGateSignature(gateState)");
    expect(chrome).not.toContain("revivalGateSignature({ ...gateState, now");
  });

  it("guards the topbar strip's repaint, since it carries the tokens chip", () => {
    // Assert — the third click target painted in this block, on the same
    // guard: a rewrite mid-press means the browser fires no click at all.
    expect(chrome).toContain("infoSlot.paint(");
  });

  it("guards the dequeue card's repaint, since it carries buttons of its own", () => {
    // Assert — an unconditional rewrite destroys whichever button the user is
    // mid-press on, and the browser then fires no click at all.
    expect(chrome).toContain("mergeDequeueSlot.paint(mergeDequeueCardHtml(");
  });
});

describe("the revival decision's dispatch", () => {
  const sendRevive = blocksAfter(main, "const sendRevive = (mode: ReviveDecision): void => {")[0]!;

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

  it("reads the clicked decision out of its attribute rather than inferring one", () => {
    // Assert — with five options, "not the compact button" is no longer the
    // same statement as "resume as-is", so an inferred decision would resume a
    // conversation at full context that the user asked to compact.
    expect(main).toContain("sendRevive(reviveDecisionFromAttr(el.getAttribute(REVIVE_ATTR)))");
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

// A page's URL is its ADDRESS, and an address that changes underneath a page is
// a different page on the next reload. A workspace-addressed webview once
// rewrote itself into `?session=<id>` the moment it created a session, which
// made every reload, bookmark, restored tab and remount of that URL an attach
// against whatever session it had recorded — stale by then in every case the
// rewrite was supposed to help.
describe("the page's own URL", () => {
  /** Every mutation of the live location's query, across every module. */
  const searchParamWrites = Object.entries(sources).flatMap(([path, source]) =>
    [...source.matchAll(/(?:searchParams|history)\.(?:set|replaceState|pushState)\s*\(/g)].map(
      (match) => `${path}: ${match[0]}`,
    ),
  );

  it("never acquires a session parameter", () => {
    // Assert — no module writes the page's session identity into its address.
    const offenders = Object.entries(sources).filter(([, source]) =>
      /searchParams\.set\(\s*["']session["']/.test(source),
    );
    expect(offenders.map(([path]) => path)).toEqual([]);
  });

  it("is never rewritten at all, so the address a page opened with is the one it keeps", () => {
    // Assert — the broader guard: any query mutation or history rewrite is a
    // second way to change what a reload attaches to.
    expect(searchParamWrites).toEqual([]);
  });
});

// A session-addressed side-call made before the daemon has ruled on which
// session the workspace owns is a request against `/sessions//…`, and the
// account lookup made exactly that request on every workspace-addressed mount.
describe("session-addressed side-calls at mount", () => {
  it("gates the account lookup on a bound identity", () => {
    // Assert — the only account refresh at boot scope goes through the gate,
    // so nothing re-wires it back to mount.
    expect(main).toContain("sessionIdentity.whenBound(");
    expect(main).not.toMatch(/\n {2}void refreshAccount\(\);/);
  });
});

// A refused command must reach the user. The dispatcher's `onFailure` sink is
// boot-scope closure state with no importable seam, so the delegation is pinned
// here; the rule it delegates to is tested in command-dispatch.test.ts.
describe("the refusal sink", () => {
  it("delegates every refusal to the one surfacing rule", () => {
    // Assert — an inline branch here is how a missed reveal came to log and
    // return, leaving the refusal on screen nowhere at all.
    expect(main).toContain("onFailure: (refusal) =>\n      surfaceRefusal(refusal, {");
  });
});

// A workspace switch relayouts the webview around the host's tail snap, in an
// order neither Emacs nor the page controls, so the feed's return to its tail
// hangs on the resize event rather than on the snap winning that race. The
// wiring is boot-scope closure state with no importable seam, so it is pinned
// here.
describe("the feed's tail re-anchor", () => {
  it("is armed on the feed element itself", () => {
    // Assert — the box observed is the one the snap parks, not the document.
    expect(main).toContain("new TailFollow(feedEl)");
  });

  it("hands every scroll-moving mechanism the SAME owner", () => {
    // Assert — the host snap, the renderer, the chess mount and the sidebar
    // reveal all park through one latched decision. A second `new TailFollow`
    // in the boot closure is a second answer to the one question, which is the
    // shape of defect this whole change removes.
    expect(main.match(/new TailFollow\(/g)).toHaveLength(1);
  });

  it("is driven by a real resize observation", () => {
    // Assert — the resize event is the signal; a timer would be a bet on how
    // long the relayout takes.
    expect(main).toContain("new ResizeObserver(onResize).observe(feedEl)");
  });
});

// BOTH HALVES OF THE PAGE'S SOURCE ATTRIBUTION MUST CONVERGE ON THE SAME EDGE.
// The agent-repl session id is re-read from the pushed plane on every ingest,
// but the Claude uuid moved only when a `SessionView` announced a new one — so
// a successor session that had announced none inherited its predecessor's uuid,
// and every forwarded log record carried an identity the daemon's registry
// check refuses. Production ran that way for hours: ~25 refused client_log
// records/second from the master workspace's page, its bound uuid four days and
// three sessions behind the registry's. The rebind is boot-scope closure state
// with no importable seam, so the wiring is pinned here; the store's retirement
// of the dead uuid is tested in store.test.ts.
describe("the page's bound log identity", () => {
  it("reconciles the vendor uuid against the store on every batch", () => {
    // Assert — the store's gated value is the single authority, re-read after
    // every ingest rather than on a rebase verdict, so the stamp can never
    // drift from the identity the daemon's registry holds.
    expect(main).toContain("if (\n          store.state.claudeSessionId !== boundClaudeSessionId &&");
    expect(main).toContain("bindLogContext({ claude_session_id: boundClaudeSessionId });");
  });

  it("reads the batch's uuid only off the owning session's view", () => {
    // Assert — a StateSnapshot's session catalog carries RETIRED sessions, and
    // an ungated scan of it is how a page adopted a four-day-dead uuid at boot,
    // on every reload, from a value nothing had persisted.
    expect(main).toContain("claudeSessionIdOf(effects, store.state.sessionId)");
  });

  it("unbinds the stamp when the daemon refuses a forwarded record", () => {
    // Assert — a refusal says the stamp is not the registry's identity, so the
    // page drops it and the next record is filed under the daemon's own.
    expect(main).toContain("onClientLogRefused: () => {");
    expect(main).toContain('bindLogContext({ claude_session_id: "" });');
  });

  it("starts every load with no vendor uuid bound", () => {
    // Assert — no URL param, no storage, no cached snapshot: a page that has
    // been told nothing sends no uuid at all.
    expect(main).toContain('let boundClaudeSessionId = "";');
  });

  it("forgets the rebase identity when the page rebinds sessions", () => {
    // Assert — without this the successor's first announcement rules as a
    // rotation and wipes a history that was never in the retired seq space.
    expect(main).toContain("sessionRebase.forget();");
  });
});
