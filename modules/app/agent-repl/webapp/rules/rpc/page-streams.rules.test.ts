/**
 * THE PAGE'S CONNECTION BUDGET (`src/rpc/page-streams.ts`): no module but the
 * page mux opens a standing stream, over every server-streaming rpc the
 * service declares.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { filesUnder } from "../files-under.js";

/**
 * THE COUNT, ENFORCED AT ITS SOURCE.
 *
 * The one-connection guarantee is a property of the WHOLE page, and no single
 * component test can see it: each component is correct on its own while the
 * seventh stream still queues. So it is checked where it IS decidable — every
 * module but the page mux is forbidden to call a server-streaming rpc on the
 * client at all, which makes a second connection something the tree does not
 * contain rather than something a reviewer has to notice.
 *
 * THIS TEST FAILS ON THE CODE THE PAGE MUX REPLACED. That page called
 * `client.watchWorkspaceRoster`, `client.watchWebWorkspace`, `client.watchDaemon`,
 * `client.watchTopbar`, `client.watchFooter`, `client.watchDaemonHolds`,
 * `client.watchFeed` (from both the root feed and every bubble) and
 * `client.watchLoginTerminal` from eight different modules — against a browser
 * cap measured, in the e2e sandbox on this daemon, at exactly six.
 */
/**
 * THE FORBIDDEN LIST IS DERIVED, NEVER TYPED OUT.
 *
 * A hand-written list guards only the rpcs somebody remembered to add to it: a
 * NEW server-streaming rpc in the service would be a second connection this
 * check silently permits, which is the exact class of defect the mux exists to
 * make impossible. So it is read off the service descriptor — every
 * server-streaming method except the mux's own — and a streaming rpc landed
 * tomorrow is guarded the moment its bindings regenerate.
 */
/**
 * THE FINITE STREAMS: server-streaming rpcs that END on their own with a
 * terminal frame (`LoadFeedThrough`: pages, then `reached` or `error`). They
 * hold a connection for the length of one walk, as a slow unary call does, not
 * for the page's life, so they do not spend the standing-stream budget this
 * guard protects. Named here, one by one: a new streaming rpc is guarded until
 * someone states in this list that it is finite.
 */
const FINITE_STREAMING_RPCS: readonly string[] = ["loadFeedThrough"];

const STREAMING_RPCS: readonly string[] = Object.entries(AgentRepl.method)
  .filter(
    ([name, method]) =>
      method.methodKind === "server_streaming" &&
      name !== "watchPage" &&
      !FINITE_STREAMING_RPCS.includes(name),
  )
  .map(([name]) => name);

describe("the page's connection budget", () => {
  it("opens a standing stream from nowhere but the page mux", () => {
    // Arrange.
    const srcRoot = join(fileURLToPath(new URL(".", import.meta.url)), "../../src");
    const mux = join(srcRoot, "rpc/page-streams.ts");

    // Act.
    const offenders: string[] = [];
    for (const file of filesUnder(srcRoot)) {
      if (file === mux) continue;
      const body = readFileSync(file, "utf8");
      for (const rpc of STREAMING_RPCS) {
        if (body.includes(`.${rpc}(`)) offenders.push(`${file}: ${rpc}`);
      }
    }

    // Assert: a page holds ONE connection because there is nowhere else in the
    // tree that opens one, not because six components each remembered not to.
    expect(offenders).toEqual([]);
  });

  it("guards every server-streaming rpc the service declares but the mux's own", () => {
    // Arrange: the rpcs the page used to open a connection apiece for, plus
    // the host watch and the tab opening Emacs holds. Named here so a method
    // DROPPED from the service is noticed too, rather than quietly shrinking
    // the guard.
    const expected = [
      "openTabs",
      "watchFeed",
      "watchWorkspaceRoster",
      "watchTopbar",
      "watchFooter",
      "watchHostWorkspace",
      "watchDaemon",
      "watchWebWorkspace",
      "watchLoginTerminal",
      // A chat thread's view: the page reads it through the mux's `chat` arm.
      "watchChat",
      // A workspace page's mention targets: read through the mux's
      // `workspaceMentions` arm.
      "watchWorkspaceMentions",
      // The agent CLI's test run and redundancy measurement: finite, but no
      // page has any business opening either, so both stay guarded rather
      // than exempted.
      "runTests",
      "runRedundancy",
    ];

    // Act.
    const guarded = [...STREAMING_RPCS].sort();

    // Assert: the derivation covers exactly those, and `watchPage` — the one
    // stream a page is allowed to hold — is not among them.
    expect(guarded).toEqual([...expected].sort());
    expect(guarded).not.toContain("watchPage");
  });

  it("exempts only rpcs the service still declares as server-streaming", () => {
    // Arrange / Act.
    const stale = FINITE_STREAMING_RPCS.filter(
      (name) => AgentRepl.method[name as keyof typeof AgentRepl.method]?.methodKind !== "server_streaming",
    );

    // Assert: an exemption for a dropped or reshaped rpc is noticed, not kept.
    expect(stale).toEqual([]);
  });
});
