/**
 * HistoryPager — the two positionless verbs, and nothing else.
 *
 * One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { HistoryPager, type HistoryPageVerb } from "../src/history-pager.js";

interface Sent {
  verb: HistoryPageVerb;
  workspace: string;
  requestId: string;
}

function newPager(opts: { workspace?: string } = {}) {
  const sent: Sent[] = [];
  const acks: Array<{ resolve: () => void; reject: (err: unknown) => void }> = [];
  let n = 0;
  const pager = new HistoryPager({
    workspace: () => opts.workspace ?? "/ws/a",
    send: (verb, workspace) => {
      const requestId = `r${++n}`;
      sent.push({ verb, workspace, requestId });
      let resolve!: () => void;
      let reject!: (err: unknown) => void;
      const ack = new Promise<void>((res, rej) => {
        resolve = res;
        reject = rej;
      });
      acks.push({ resolve, reject });
      return { requestId, ack };
    },
    now: () => 0,
    random: () => 0,
  });
  return { pager, sent, acks };
}

describe("HistoryPager", () => {
  it("asks for the FIRST page on a cold open", () => {
    // Arrange
    const { pager, sent } = newPager();
    // Act
    void pager.openFirst().catch(() => {});
    // Assert
    expect(sent).toEqual([{ verb: "first", workspace: "/ws/a", requestId: "r1" }]);
  });

  it("asks for the NEXT page with no position of any kind", () => {
    // Arrange — the send seam takes a verb and a workspace, and nothing else.
    const { pager, sent } = newPager();
    // Act
    void pager.next().catch(() => {});
    // Assert
    expect(Object.keys(sent[0])).toEqual(["verb", "workspace", "requestId"]);
  });

  it("answers a REFUSED next page with a first page", () => {
    // Arrange — a refusal means the daemon dropped this reader's position.
    const { pager, sent, acks } = newPager();
    void pager.next().catch(() => {});
    // Act
    pager.observeRefusal("r1", "no position");
    // Assert — a first page, never a retry of the next page.
    expect(sent.map((s) => s.verb)).toEqual(["next", "first"]);
    acks[0].reject(new Error("refused"));
  });

  it("does NOT re-ask a refused FIRST page from the refusal itself", () => {
    // Arrange — a first-page failure is a failure to back off from, not a
    // dropped position to recover.
    const { pager, sent, acks } = newPager();
    void pager.openFirst().catch(() => {});
    // Act
    pager.observeRefusal("r1", "daemon busy");
    // Assert
    expect(sent).toHaveLength(1);
    acks[0].reject(new Error("refused"));
  });

  it("keeps ONE request in flight", () => {
    // Arrange
    const { pager, sent } = newPager();
    void pager.openFirst().catch(() => {});
    // Act
    void pager.next().catch(() => {});
    // Assert
    expect(sent).toHaveLength(1);
  });

  it("settles the request a page answers, freeing the next ask", () => {
    // Arrange
    const { pager, sent } = newPager();
    void pager.openFirst().catch(() => {});
    // Act
    pager.observePage("r1");
    void pager.next().catch(() => {});
    // Assert
    expect(sent.map((s) => s.verb)).toEqual(["first", "next"]);
  });

  it("ignores a page settling a request id it does not hold", () => {
    // Arrange
    const { pager } = newPager();
    void pager.openFirst().catch(() => {});
    // Act
    const settled = pager.observePage("r-someone-else");
    // Assert
    expect(settled).toBe(false);
  });

  it("defers when no workspace can be named yet", async () => {
    // Arrange
    const { pager, sent } = newPager({ workspace: "" });
    // Act / Assert
    await expect(pager.openFirst()).rejects.toThrow(/no live workspace/);
    expect(sent).toEqual([]);
  });
});
