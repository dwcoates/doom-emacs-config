// @vitest-environment jsdom
/**
 * The feed harness's shared setup helpers: a harness whose root feed has a
 * live tail (`liveRootHarness`), and the scroll box holding the feed's host
 * (`scrollHost`). The feed suite builds both through them, and the scan below
 * fails a test that hand-rolls either again.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { create } from "@bufbuild/protobuf";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { OpenFeedRequestSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import { mountFeed } from "../../src/feed/feed.js";
import {
  ROOT_WATCH_TOKEN,
  liveRootHarness,
  openSuccess,
  page,
  push,
  interimRow,
  responseRow,
  scrollHost,
  settledResponseRow,
  thinkingRow,
  stubRenderers,
  tokenFor,
} from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

async function settle(): Promise<void> {
  for (let i = 0; i < 40; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("liveRootHarness", () => {
  it("delivers a frame pushed on its channel to the mounted root feed", async () => {
    // Arrange
    const { h, channel } = liveRootHarness();
    const { scroll, host } = scrollHost();
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
    await settle();
    // Act
    channel.push(push(responseRow("r1")));
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="r1"]')).not.toBeNull();
    feed.dispose();
  });

  it("keeps the rest of the script", async () => {
    // Arrange
    const { h } = liveRootHarness({
      openFeed: (req) => openSuccess(page([responseRow("served")]), tokenFor(req)),
    });
    const { scroll, host } = scrollHost();
    // Act
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="served"]')).not.toBeNull();
    feed.dispose();
  });

  it("watches under the token the root's open answers", () => {
    // Arrange
    const rootOpen = create(OpenFeedRequestSchema, {});
    // Act
    const token = tokenFor(rootOpen);
    // Assert
    expect(token).toBe(ROOT_WATCH_TOKEN);
  });
});

describe("scrollHost", () => {
  it("puts the host inside the scroll box", () => {
    // Arrange / Act
    const { scroll, host } = scrollHost();
    // Assert
    expect(host.parentElement).toBe(scroll);
  });

  it("makes the scroll box the document's whole body", () => {
    // Arrange
    document.body.append(document.createElement("aside"));
    // Act
    const { scroll } = scrollHost();
    // Assert
    expect([...document.body.children]).toEqual([scroll]);
  });
});

/**
 * THE FEED SUITE SHARES THE SETUP: a hand-rolled root channel or scroll box is
 * the drift the helpers exist to prevent.
 */
describe("the feed suite's setup call sites", () => {
  const suite = readFileSync(join(process.cwd(), "test/feed/feed.test.ts"), "utf8");

  it("registers no root channel by hand", () => {
    // Arrange / Act
    const handRolled = /channels\.set\(\s*"tok:root"/.test(suite);
    // Assert
    expect([handRolled, suite.includes("liveRootHarness(")]).toEqual([false, true]);
  });

  it("builds no scroll box around a host by hand", () => {
    // Arrange / Act
    const handRolled = /const scroll = document\.createElement\("div"\);\s*const host = document\.createElement\("div"\);\s*scroll\.append\(host\);/.test(suite);
    // Assert
    expect([handRolled, suite.includes("scrollHost()")]).toEqual([false, true]);
  });
});

describe("settledResponseRow", () => {
  it.each([
    ["thinking", { thinking: true }, [true, false]],
    ["interim", { interim: true }, [false, true]],
  ] as const)("stamps a %s row's flag alone", (_name, flags, want) => {
    // Arrange / Act
    const row = settledResponseRow("k1", "words", flags);
    // Assert
    const unit = row.row.case === "activity" ? row.row.value.unit : undefined;
    const response = unit?.case === "response" ? unit.value : undefined;
    expect([response?.thinking, response?.interim]).toEqual(want);
  });

  it("settles the row with its markdown under its own id", () => {
    // Arrange / Act
    const row = settledResponseRow("k1", "words", { interim: true });
    // Assert
    const unit = row.row.case === "activity" ? row.row.value.unit : undefined;
    const result = unit?.case === "response" ? unit.value.result : undefined;
    expect([row.id?.value, result?.case, result?.case === "success" ? result.value.prose?.markdown : undefined]).toEqual([
      "k1",
      "success",
      "words",
    ]);
  });

  it.each([
    ["thinkingRow", thinkingRow, { thinking: true }],
    ["interimRow", interimRow, { interim: true }],
  ] as const)("is the shape %s builds", (_name, build, flags) => {
    // Arrange / Act
    const built = build("k1", "words");
    // Assert
    expect(built).toEqual(settledResponseRow("k1", "words", flags));
  });
});
