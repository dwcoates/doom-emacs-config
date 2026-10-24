import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { TailFollow, type ReaderInput, type SubscribeInput } from "../src/scroll.js";
import { tailTriggers } from "./tail-triggers.js";

/** A tail owner whose three subscriptions are recorded as they fire. */
function recording(): { tail: TailFollow; fired: string[] } {
  const fired: string[] = [];
  const tail = new TailFollow({ scrollTop: 0, scrollHeight: 1000, clientHeight: 300 });
  tail.observe = (scroll, resize, input: SubscribeInput) => {
    scroll(() => fired.push("scroll"));
    resize(() => fired.push("resize"));
    input((kind: ReaderInput) => fired.push(`input:${kind}`));
  };
  return { tail, fired };
}

describe("tailTriggers", () => {
  it.each<[string, (t: ReturnType<typeof tailTriggers>) => void, string[]]>([
    ["scroll fires the scroll event alone", (t) => t.scroll(), ["scroll"]],
    ["resize fires the resize pass alone", (t) => t.resize(), ["resize"]],
    ["input fires a wheel by default", (t) => t.input(), ["input:wheel"]],
    ["input fires the kind it is given", (t) => t.input("keydown"), ["input:keydown"]],
    ["gesture fires a wheel, then the scroll event", (t) => t.gesture(), ["input:wheel", "scroll"]],
  ])("%s", (_name, act, expected) => {
    // Arrange
    const r = recording();
    const triggers = tailTriggers(r.tail);
    // Act
    act(triggers);
    // Assert
    expect(r.fired).toEqual(expected);
  });

  it("is the only test code that subscribes a tail owner by hand", () => {
    // Arrange
    const root = fileURLToPath(new URL(".", import.meta.url));
    const files = (readdirSync(root, { recursive: true }) as string[]).filter(
      (f) => /\.test\.ts$/.test(f) && f !== "tail-triggers.test.ts",
    );
    // Act
    const offenders = files.filter((f) => /\btail\.observe\(/.test(readFileSync(join(root, f), "utf8")));
    // Assert
    expect(offenders).toEqual([]);
  });
});
