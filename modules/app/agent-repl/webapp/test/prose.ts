/**
 * prose — the webapp tests' frontend.v1.ParsedProse.
 *
 * The webapp draws only what the daemon parses, so a test never builds a tree's
 * graph by hand: treeProse answers the DAEMON'S OWN PARSE of a text, read from
 * test/fixtures/parsed-prose.json. That file is written by the daemon's prose
 * package from test/fixtures/prose-inputs.json, and a daemon test fails while
 * it is stale, so the two parsers cannot drift. A text a test needs is added
 * to the inputs and the fixture regenerated, from daemon/:
 *
 *   go test ./internal/prose -run WebappFixture -update
 *
 * plainProse is prose with no tree in it, which the daemon serves as one plain
 * block: it refuses a text with a tree's shape, so a tree can never reach a
 * test as plain prose by accident. servedProse picks between the two by that
 * same shape, for a helper handed either.
 */
import { fromJson, type JsonValue } from "@bufbuild/protobuf";
import { unparsedProse } from "../src/bubble/body.js";
import {
  ParsedProseSchema,
  type ParsedProse,
} from "../../proto/gen/ts/frontend/v1/prose_pb";
import fixture from "./fixtures/parsed-prose.json";

type FixtureEntry = { text: string; arriving?: boolean; prompt?: boolean; prose: JsonValue };

// The JSON import types each entry by its literal shape; every entry is a
// FixtureEntry, the daemon test's webappEntry.
const ENTRIES = fixture as unknown as readonly FixtureEntry[];

/** The lowest code point a root's marker opens on (daemon/internal/prose markerFloor). */
const MARKER_FLOOR = 0x2190;

/** The daemon's parse of TEXT, settled or still ARRIVING. */
export function treeProse(text: string, { arriving = false } = {}): ParsedProse {
  return fixtureParse(text, { arriving, prompt: false });
}

/** The daemon's parse of TEXT as a PROMPT, its entries read (ParsePrompt). */
export function promptProse(text: string): ParsedProse {
  return fixtureParse(text, { arriving: false, prompt: true });
}

/** The fixture's parse of TEXT, as ARRIVING served prose or as a PROMPT. */
function fixtureParse(text: string, { arriving, prompt }: { arriving: boolean; prompt: boolean }): ParsedProse {
  const entry = ENTRIES.find(
    (e) => e.text === text && (e.arriving ?? false) === arriving && (e.prompt ?? false) === prompt,
  );
  if (entry === undefined) {
    throw new Error(
      `no daemon parse of ${JSON.stringify(text)} (arriving: ${arriving}, prompt: ${prompt}) in test/fixtures/parsed-prose.json: ` +
        "add it to test/fixtures/prose-inputs.json and run `go test ./internal/prose -run WebappFixture -update` from daemon/",
    );
  }
  return fromJson(ParsedProseSchema, entry.prose);
}

/**
 * TEXT as the daemon serves it, for a helper that takes either kind: the
 * daemon's own parse when TEXT holds a tree's shape (treeProse), and one plain
 * block when it holds none (plainProse).
 */
export function servedProse(text: string, { arriving = false } = {}): ParsedProse {
  return treeShaped(text) ? treeProse(text, { arriving }) : plainProse(text);
}

/** Whether TEXT holds a connector line or a labelled root line. */
function treeShaped(text: string): boolean {
  const rootLike = [...text.matchAll(/^[0-9]+(?:\.[0-9]+)* (.)/gmu)].some(
    (m) => (m[1].codePointAt(0) ?? 0) >= MARKER_FLOOR,
  );
  return /[├└]──/.test(text) || rootLike;
}

/**
 * MARKDOWN with no tree in it, as the daemon serves it: one plain block of the
 * text less one trailing newline, or none when it is only whitespace.
 */
export function plainProse(markdown: string): ParsedProse {
  if (treeShaped(markdown)) {
    throw new Error(`plainProse given a tree: ${JSON.stringify(markdown)}; use treeProse`);
  }
  return unparsedProse(markdown);
}
