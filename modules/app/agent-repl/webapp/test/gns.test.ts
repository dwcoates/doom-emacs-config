import { describe, expect, it } from "vitest";
import { gnsFolds, isBridgeSpawn } from "../src/gns.js";
import { ConversationItem, ResultItem, ToolItem } from "../src/store.js";

function userTurn(requestId = "u1"): ConversationItem {
  return {
    kind: "user-turn",
    requestId,
    content: [{ type: "text", text: "go" }],
    ts: "2026-05-24T10:00:00.000Z",
  } as unknown as ConversationItem;
}

function text(blockId: string, parent?: string): ConversationItem {
  return {
    kind: "text",
    blockId,
    messageId: "m1",
    parentToolUseId: parent,
    text: "done",
    done: true,
    ts: "2026-05-24T10:00:00.000Z",
  };
}

function tool(id: string, name = "Bash", parent?: string): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId: id,
    messageId: "m1",
    toolName: name,
    parentToolUseId: parent,
    inputJson: "{}",
    input: {},
    inputDone: true,
  };
}

/** A gns-sockets bridge respawn: the Agent spawn the Stop hook dictates. */
function bridgeSpawn(id: string, parent?: string): ToolItem {
  return {
    ...tool(id, "Agent", parent),
    input: { subagent_type: "sockets-listener", run_in_background: true },
  };
}

function result(subtype = "success"): ResultItem {
  return { kind: "result", subtype } as unknown as ResultItem;
}

describe("isBridgeSpawn", () => {
  it("is true for an Agent spawn of the sockets-listener", () => {
    // Arrange / Act / Assert
    expect(isBridgeSpawn(bridgeSpawn("t1"))).toBe(true);
  });

  it("is false for a spawn of any other subagent type", () => {
    // Arrange
    const explore = { ...tool("t1", "Agent"), input: { subagent_type: "Explore" } };
    // Act / Assert
    expect(isBridgeSpawn(explore)).toBe(false);
  });

  it("is false for a nested (parented) sockets-listener spawn", () => {
    // Arrange / Act / Assert
    expect(isBridgeSpawn(bridgeSpawn("t1", "outer"))).toBe(false);
  });

  it("is false for a non-subagent tool carrying the input field", () => {
    // Arrange — only Task/Agent can spawn; a Bash with the field is noise.
    const bash = { ...tool("t1", "Bash"), input: { subagent_type: "sockets-listener" } };
    // Act / Assert
    expect(isBridgeSpawn(bash)).toBe(false);
  });
});

describe("gnsFolds: tail segment (Stop-hook continuation)", () => {
  it("folds the respawn segment into the real answer before it", () => {
    // Arrange — answer, then the Stop-hook segment: spawn + acknowledgment.
    const spawn = bridgeSpawn("t1");
    const ack = text("ack");
    const items = [userTurn(), text("answer"), spawn, ack, result()];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.get("answer")).toEqual([spawn, ack]);
  });

  it("keeps the host text and the result out of the folded set", () => {
    // Arrange
    const spawn = bridgeSpawn("t1");
    const answer = text("answer");
    const chip = result();
    const items = [userTurn(), answer, spawn, text("ack"), chip];
    // Act
    const folds = gnsFolds(items);
    // Assert — the result must stay to pair the green border onto the host.
    expect(folds.folded.has(answer)).toBe(false);
    expect(folds.folded.has(chip)).toBe(false);
  });

  it("folds nothing when real work follows the spawn", () => {
    // Arrange — a later real tool means the spawn was the turn's own doing.
    const items = [userTurn(), text("a1"), bridgeSpawn("t1"), tool("t2"), text("a2"), result()];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.folded.size).toBe(0);
  });

  it("folds nothing when no text precedes the spawn in a prompted turn", () => {
    // Arrange — nothing above the segment could host it in feed order.
    const items = [userTurn(), bridgeSpawn("t1"), text("ack"), result()];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.folded.size).toBe(0);
  });

  it("folds nothing from a turn that did not end in success", () => {
    // Arrange — a severed turn's tail is not an answer to fold under.
    const items = [userTurn(), text("a1"), bridgeSpawn("t1"), text("ack"), result("aborted")];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.folded.size).toBe(0);
  });

  it("starts the segment at the first spawn of a respawn run", () => {
    // Arrange — two respawns with prose between them, all one segment.
    const s1 = bridgeSpawn("t1");
    const mid = text("mid");
    const s2 = bridgeSpawn("t2");
    const ack = text("ack");
    const items = [userTurn(), text("answer"), s1, mid, s2, ack, result()];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.get("answer")).toEqual([s1, mid, s2, ack]);
  });

  it("never lets a parented block host or break the segment", () => {
    // Arrange — the listener's own parented output rides inside its card.
    const spawn = bridgeSpawn("t1");
    const nested = text("nested", "t1");
    const items = [userTurn(), text("answer"), spawn, nested, text("ack"), result()];
    // Act
    const folds = gnsFolds(items);
    // Assert — the parented block is already confined; it is not folded.
    expect(folds.byBubble.get("answer")).toEqual([spawn, items[4]]);
    expect(folds.folded.has(nested)).toBe(false);
  });

  it("folds nothing while the turn is still streaming", () => {
    // Arrange — no result yet: the next block could always continue the turn.
    const items = [userTurn(), text("answer"), bridgeSpawn("t1"), text("ack")];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.folded.size).toBe(0);
  });
});

describe("gnsFolds: whole bridge-woken turn", () => {
  /** A completed prompted turn answering with A1, hosting what follows. */
  function promptedTurn(blockId: string): ConversationItem[] {
    return [userTurn(), text(blockId), result()];
  }

  it("folds a promptless turn that respawns the bridge, chip included", () => {
    // Arrange — the notification-woken shape: text, respawn, text, result.
    const closedNote = text("closed");
    const spawn = bridgeSpawn("t2");
    const ack = text("ack");
    const chip = result();
    const items = [...promptedTurn("answer"), closedNote, spawn, ack, chip];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.get("answer")).toEqual([closedNote, spawn, ack, chip]);
    expect(folds.folded.has(chip)).toBe(true);
  });

  it("folds a spawn-less promptless turn woken by the bridge's own output", () => {
    // Arrange — the bridge-expiry shape: the wake rides in as the earlier
    // spawn's parented digest, and the turn only writes one note.
    const spawn = bridgeSpawn("t1");
    const items = [
      userTurn(),
      text("answer"),
      spawn,
      text("ack"),
      result(),
      text("digest", "t1"),
      text("expired"),
      result(),
    ];
    // Act
    const folds = gnsFolds(items);
    // Assert — both the Stop-hook segment and the woken turn fold into the answer.
    expect(folds.byBubble.get("answer")).toEqual([
      spawn,
      items[3],
      items[6],
      items[7],
    ]);
  });

  it("keeps a promptless turn with no gns signal visible", () => {
    // Arrange — some other background work woke the turn; not ours to fold.
    const items = [...promptedTurn("answer"), text("real note"), result()];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.folded.size).toBe(0);
  });

  it("stacks successive bridge-woken turns under the same host", () => {
    // Arrange — two woken turns in a row, one host answer above them.
    const first = [text("closed1"), bridgeSpawn("t2"), text("ack1"), result()];
    const second = [text("closed2"), bridgeSpawn("t3"), text("ack2"), result()];
    const items = [...promptedTurn("answer"), ...first, ...second];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.get("answer")).toEqual([...first, ...second]);
  });

  it("degrades a hostless bridge-woken turn to its own tail fold", () => {
    // Arrange — nothing completed above it, so the whole turn cannot move;
    // the Stop-hook tail still folds into the turn's own leading text.
    const spawn = bridgeSpawn("t1");
    const ack = text("ack");
    const items = [text("closed"), spawn, ack, result()];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.get("closed")).toEqual([spawn, ack]);
  });

  it("voids the host when a prompted turn answers with tools only", () => {
    // Arrange — noise must not fold into a bubble above newer visible cards,
    // so the woken turn degrades to a tail fold under its OWN leading text.
    const spawn = bridgeSpawn("t2");
    const ack = text("ack");
    const items = [
      ...promptedTurn("answer"),
      userTurn("u2"),
      tool("t9"),
      result(),
      text("closed"),
      spawn,
      ack,
      result(),
    ];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.has("answer")).toBe(false);
    expect(folds.byBubble.get("closed")).toEqual([spawn, ack]);
  });

  it("hosts a woken turn on the REAL answer a tail fold preserved", () => {
    // Arrange — turn 1 ends in a Stop-hook segment; turn 2 is bridge-woken.
    const items = [
      userTurn(),
      text("answer"),
      bridgeSpawn("t1"),
      text("ack1"),
      result(),
      text("closed"),
      bridgeSpawn("t2"),
      text("ack2"),
      result(),
    ];
    // Act
    const folds = gnsFolds(items);
    // Assert — everything lands under the real answer, not the ack: the
    // turn-1 segment (spawn, ack1) plus the whole woken turn (closed,
    // spawn, ack2, chip).
    expect([...folds.byBubble.keys()]).toEqual(["answer"]);
    expect(folds.byBubble.get("answer")).toHaveLength(6);
  });

  it("voids the host after an aborted turn", () => {
    // Arrange — the abort severs the anchor, so the woken turn cannot move
    // above it and degrades to a tail fold under its own leading text.
    const spawn = bridgeSpawn("t1");
    const ack = text("ack");
    const items = [
      userTurn(),
      text("answer"),
      result("aborted"),
      text("closed"),
      spawn,
      ack,
      result(),
    ];
    // Act
    const folds = gnsFolds(items);
    // Assert
    expect(folds.byBubble.has("answer")).toBe(false);
    expect(folds.byBubble.get("closed")).toEqual([spawn, ack]);
  });
});
