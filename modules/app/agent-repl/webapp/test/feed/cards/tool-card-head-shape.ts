/**
 * The shape every tool card's head must have, read off a drawn card: its
 * title, its badge, then its clock, and nothing else (owner ruling,
 * 2026-10-13). Every card's suite reads its head through this one reader, so a
 * card that hand-rolls a head, reorders it or puts a chip in it fails there.
 */
import { create } from "@bufbuild/protobuf";
import {
  FeedToolCardClockSchema,
  type FeedToolCardClock,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { TOOL_CARD_HEAD_ATTRIBUTE, TOOL_CLOCK_CLASS, TOOL_NAME_CLASS } from "../../../src/feed/cards/tool-head.js";

/** A head clock running since STARTMS. */
export function runningClock(startMs: number): FeedToolCardClock {
  return create(FeedToolCardClockSchema, { startedAtMs: BigInt(startMs), span: { case: "running", value: {} } });
}

/** A head clock that ran from STARTMS to ENDMS. */
export function stoppedClock(startMs: number, endMs: number): FeedToolCardClock {
  return create(FeedToolCardClockSchema, {
    startedAtMs: BigInt(startMs),
    span: { case: "stopped", value: { endedAtMs: BigInt(endMs) } },
  });
}

/** One part of a head, as `kind:text`. */
export type HeadPart = `${"title" | "badge" | "clock"}:${string}`;

/** CARD's head, part by part, refusing any child that is none of the three. */
export function toolCardHeadParts(card: Element): HeadPart[] {
  const head = card.querySelector(`[${TOOL_CARD_HEAD_ATTRIBUTE}]`);
  if (head === null) throw new Error("the card has no head built by drawToolCardHead");
  if (head !== card.firstElementChild) throw new Error("the card's head is not its first child");
  return [...head.children].map((child): HeadPart => {
    const text = child.textContent ?? "";
    if (child.classList.contains(TOOL_NAME_CLASS)) return `title:${text}`;
    if (child.classList.contains("badge")) return `badge:${text}`;
    if (child.classList.contains(TOOL_CLOCK_CLASS)) return `clock:${text}`;
    throw new Error(`the head holds a child that is no title, badge or clock: ${child.outerHTML}`);
  });
}
