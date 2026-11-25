/**
 * THE SHARED COLD-GATE STYLE IS ONE HELPER AND ONE RULE: both sections'
 * sources draw it through `markColdGatedName`, and the stylesheet keys one
 * rule on its class.
 */
import { describe, expect, it } from "vitest";
import { COLD_GATED_NAME_CLASS } from "../../src/sidebar/cold-gate-name.js";
import styles from "../../src/styles.css?raw";
import mergeQueueSource from "../../src/sidebar/merge-queue-view.ts?raw";
import rowSource from "../../src/sidebar/row.ts?raw";

describe("the shared cold-gate style", () => {
  it("is drawn by the one helper in both sections", () => {
    expect([rowSource, mergeQueueSource].map((source) => source.includes("markColdGatedName("))).toEqual([true, true]);
  });

  it("is one rule in the stylesheet, keyed on the shared class", () => {
    expect([styles.includes('[data-arm="coldGate"] > .row > .name'), styles.split(`.${COLD_GATED_NAME_CLASS} {`).length - 1]).toEqual([false, 1]);
  });
});
