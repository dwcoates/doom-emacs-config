/**
 * The wait on the engine's pushes.
 */
import { readFileSync } from "node:fs";
import { dirname, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { SessionPushes } from "../src/engine/pushes.js";
import { conversationv1 } from "../src/proto.js";
import { filesUnder } from "./files-under.js";
import { untilPushed } from "./until-pushed.js";

const update = (): conversationv1.SessionUpdate =>
  create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "compacting",
      value: create(conversationv1.SessionCompactingSchema, {}),
    },
  });

describe("untilPushed", () => {
  it("resolves at once when the state already holds", async () => {
    // Arrange
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    // Act / Assert
    await expect(
      untilPushed(pushes, "nothing", () => true),
    ).resolves.toBeUndefined();
  });

  it("resolves at the push after which the state holds, not before", async () => {
    // Arrange
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    let pushed = 0;
    let done = false;
    const waiting = untilPushed(pushes, "two pushes", () => pushed >= 2).then(
      () => {
        done = true;
      },
    );

    // Act
    pushed = 1;
    pushes.push(update());
    await Promise.resolve();
    const afterOne = done;
    pushed = 2;
    pushes.push(update());
    await waiting;

    // Assert
    expect([afterOne, done]).toEqual([false, true]);
  });
});

describe("the shim's tests", () => {
  it("never poll a condition with vi.waitFor: they await its event (until-pushed.ts, log-records.ts)", () => {
    // Arrange
    const dir = dirname(fileURLToPath(import.meta.url));
    const self = fileURLToPath(import.meta.url);

    // Act
    const polling = filesUnder(dir).filter(
      (file) => file !== self && (file.endsWith(".ts") || file.endsWith(".mjs")) && readFileSync(file, "utf8").includes("vi.waitFor("),
    );

    // Assert
    expect(polling.map((file) => relative(dir, file))).toEqual([]);
  });
});

