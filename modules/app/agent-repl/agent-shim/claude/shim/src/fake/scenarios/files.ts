/**
 * fake/scenarios/files.ts — Read, Write, Edit, Grep, Glob.
 *
 * The read/write/edit `toolUseResult` shapes are the CORPUS's, field for field
 * (`tool-results/read.jsonl`, `read-image.jsonl`, `write.jsonl`, `edit.jsonl`).
 *
 * Grep and Glob have NO corpus fixture, so their results follow the DECLARED
 * `GrepOutput` / `GlobOutput` in `sdk-tools.d.ts` — a weaker grade of evidence,
 * recorded here so a reader knows which of these shapes were observed and which
 * were only declared.
 *
 * The totals matter more than they look: `AgentGrepContentPartial`,
 * `AgentGlobPartial` and `AgentGlobOmittedExact`/`AtLeast` exist precisely
 * because the vendor reports a TOTAL and the shim subtracts the omitted figure.
 * A fixture whose total equalled its returned count could not reach the partial
 * arms at all.
 */
import { conclude, scenario } from "./support.js";

const FILE = "/w/s/example.ts";

const FILE_BODY = [
  "export const one = 1;",
  "export const two = 2;",
  "export const three = 3;",
  "export const four = 4;",
].join("\n");

/** The `toolUseResult` a Read answers with (corpus: tool-results/read.jsonl). */
function readResult(fields: {
  content: string;
  numLines: number;
  startLine: number;
  totalLines: number;
  /**
   * The vendor's own statement that IT auto-paginated the read at a token
   * budget. This is the ONLY signal for AgentReadSuccess.cut=token_cap — the
   * converter reads exactly this field (convert/tools/read.ts textExtent), and
   * the `read_truncation_notice` banner is prose beside it, never the fact. A
   * fixture that omits it declares a token cap and produces a line cap.
   */
  truncatedByTokenCap?: boolean;
}): Record<string, unknown> {
  const file: Record<string, unknown> = {
    filePath: FILE,
    content: fields.content,
    numLines: fields.numLines,
    startLine: fields.startLine,
    totalLines: fields.totalLines,
  };
  if (fields.truncatedByTokenCap === true) file.truncatedByTokenCap = true;
  return { type: "text", file };
}

const READ_WHOLE = scenario({
  name: "read",
  prompt: "!read",
  emits: "a `Read` tool_use with only `file_path`, then a text tool_result whose `toolUseResult.file` spans the whole file",
  writes: "the tool_use assistant line, the tool_result user line with `toolUseResult`, the closing text line",
  arms: "AgentRead.start + AgentReadSuccess.extent=whole",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "read-whole" }, "fake read (whole file) turn");
    const call = ctx.toolUse("Read", { file_path: FILE });
    ctx.toolResult(
      call,
      FILE_BODY,
      readResult({ content: FILE_BODY, numLines: 4, startLine: 1, totalLines: 4 }),
    );
    conclude(ctx, "Read the whole file.");
  },
});

const READ_HEAD = scenario({
  name: "read-head",
  prompt: "!read-head",
  emits: "a `Read` with `limit` and no `offset`, answered with the first lines and a total that exceeds them",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentRead.start + AgentReadSuccess.extent=head",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "read-head" }, "fake read (head) turn");
    const call = ctx.toolUse("Read", { file_path: FILE, limit: 2 });
    const head = FILE_BODY.split("\n").slice(0, 2).join("\n");
    ctx.toolResult(call, head, readResult({ content: head, numLines: 2, startLine: 1, totalLines: 4 }));
    conclude(ctx, "Read the head of the file.");
  },
});

const READ_RANGE = scenario({
  name: "read-range",
  prompt: "!read-range",
  emits: "a `Read` with both `offset` and `limit`, answered with a window whose `startLine` is not 1",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentRead.start + AgentReadSuccess.extent=range",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "read-range" }, "fake read (range) turn");
    const call = ctx.toolUse("Read", { file_path: FILE, offset: 2, limit: 2 });
    const window = FILE_BODY.split("\n").slice(1, 3).join("\n");
    ctx.toolResult(call, window, readResult({ content: window, numLines: 2, startLine: 2, totalLines: 4 }));
    conclude(ctx, "Read a range of the file.");
  },
});

const READ_TRUNCATED = scenario({
  name: "read-truncated",
  prompt: "!read-truncated",
  emits: "a `Read` cut at the token cap, plus the vendor's `read_truncation_notice` attachment naming the call",
  writes: "the tool_use line, the tool_result line, a `read_truncation_notice` attachment line, the closing text line",
  arms: "AgentReadSuccess.cut=token_cap",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "read-truncated" }, "fake truncated-read turn");
    const call = ctx.toolUse("Read", { file_path: FILE });
    const head = FILE_BODY.split("\n").slice(0, 2).join("\n");
    ctx.toolResult(
      call,
      head,
      readResult({
        content: head,
        numLines: 2,
        startLine: 1,
        totalLines: 4,
        truncatedByTokenCap: true,
      }),
    );
    ctx.attachment({
      type: "read_truncation_notice",
      banner:
        `[Truncated: PARTIAL view — ${FILE}: showing lines 1-2 of 4 total (47124 tokens, cap 25000). ` +
        "Call Read with offset=3 limit=2 for the next page, or Grep to find a specific section.]",
      toolUseID: call.toolUseId,
    });
    conclude(ctx, "The read was truncated at the token cap.");
  },
});

const READ_IMAGE = scenario({
  name: "read-image",
  prompt: "!read-image",
  emits: "a `Read` of a png, answered with an image content block and an image `toolUseResult` carrying dimensions",
  writes: "the tool_use line, the image tool_result line, the closing text line",
  arms: "AgentReadSuccess with an ImageBlock",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "read-image" }, "fake image-read turn");
    const call = ctx.toolUse("Read", { file_path: "/w/s/shot.png" });
    const base64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==";
    ctx.toolResult(
      call,
      "",
      {
        type: "image",
        file: {
          base64,
          type: "image/png",
          originalSize: 10_418,
          dimensions: {
            originalWidth: 500,
            originalHeight: 300,
            displayWidth: 500,
            displayHeight: 300,
          },
        },
      },
      { blocks: [{ type: "image", source: { type: "base64", data: base64, media_type: "image/png" } }] },
    );
    conclude(ctx, "Read the image.");
  },
});

const WRITE_CREATE = scenario({
  name: "write-create",
  prompt: "!write-create",
  emits: "a `Write` answered with `toolUseResult.type: \"create\"` and an empty `structuredPatch`",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentWrite.start + AgentWriteSuccess.outcome=created",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "write-create" }, "fake write (create) turn");
    const call = ctx.toolUse("Write", { file_path: "/w/s/new.ts", content: "export const fresh = true;\n" });
    ctx.toolResult(call, "File created successfully.", {
      type: "create",
      filePath: "/w/s/new.ts",
      content: "export const fresh = true;\n",
      structuredPatch: [],
      originalFile: null,
      userModified: false,
    });
    conclude(ctx, "Created the file.");
  },
});

const WRITE_UPDATE = scenario({
  name: "write-update",
  prompt: "!write-update",
  emits: "a `Write` over an existing file, answered with `type: \"update\"`, the prior body and a structuredPatch",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentWrite.start + AgentWriteSuccess.outcome=updated",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "write-update" }, "fake write (update) turn");
    const call = ctx.toolUse("Write", { file_path: FILE, content: `${FILE_BODY}\nexport const five = 5;\n` });
    ctx.toolResult(call, "File updated successfully.", {
      type: "update",
      filePath: FILE,
      content: `${FILE_BODY}\nexport const five = 5;\n`,
      structuredPatch: [
        {
          oldStart: 4,
          oldLines: 1,
          newStart: 4,
          newLines: 2,
          lines: ["   export const four = 4;", "+  export const five = 5;"],
        },
      ],
      originalFile: FILE_BODY,
      userModified: false,
    });
    conclude(ctx, "Updated the file.");
  },
});

const EDIT = scenario({
  name: "edit",
  prompt: "!edit",
  emits: "an `Edit` answered with the corpus edit shape — filePath, oldString, newString, structuredPatch, replaceAll",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentEdit.start + AgentEditSuccess",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "edit" }, "fake edit turn");
    const call = ctx.toolUse("Edit", {
      replace_all: false,
      file_path: FILE,
      old_string: "export const two = 2;",
      new_string: "export const two = 22;",
    });
    ctx.toolResult(call, "The file has been updated.", {
      filePath: FILE,
      oldString: "export const two = 2;",
      newString: "export const two = 22;",
      originalFile: null,
      structuredPatch: [
        {
          oldStart: 2,
          oldLines: 1,
          newStart: 2,
          newLines: 1,
          lines: ["-export const two = 2;", "+export const two = 22;"],
        },
      ],
      userModified: false,
      replaceAll: false,
    });
    conclude(ctx, "Edited the file.");
  },
});

const IDE_DIAGNOSTICS = scenario({
  name: "ide-diagnostics",
  prompt: "!ide-diagnostics",
  emits: "an `Edit`, then the vendor's `diagnostics` attachment reporting a typescript error in the edited file",
  writes: "the tool_use line, the tool_result line, a `diagnostics` attachment line, the closing text line",
  arms: "AgentEdit.diagnostics (AgentDiagnosticsReport joined to the last edit by adjacency)",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "ide-diagnostics" }, "fake edit-then-diagnostics turn");
    const call = ctx.toolUse("Edit", {
      replace_all: false,
      file_path: FILE,
      old_string: "export const three = 3;",
      new_string: "export const three = missing;",
    });
    ctx.toolResult(call, "The file has been updated.", {
      filePath: FILE,
      oldString: "export const three = 3;",
      newString: "export const three = missing;",
      originalFile: null,
      // REAL HUNKS, grounded in testdata/captures/ide-diagnostics-after-edit:
      // the vendor states the changed line with one line of context either
      // side, context lines carrying a leading space. An empty patch is a
      // shape the vendor never sends, and it made this scenario the one edit
      // whose diff a consumer could not draw.
      structuredPatch: [
        {
          oldStart: 2,
          oldLines: 3,
          newStart: 2,
          newLines: 3,
          lines: [
            " export const two = 2;",
            "-export const three = 3;",
            "+export const three = missing;",
            " export const four = 4;",
          ],
        },
      ],
      userModified: false,
      replaceAll: false,
    });
    ctx.attachment({
      type: "diagnostics",
      files: [
        {
          uri: FILE,
          diagnostics: [
            {
              message: "Cannot find name 'missing'.",
              severity: "Error",
              range: { start: { line: 2, character: 21 }, end: { line: 2, character: 28 } },
              source: "typescript",
              code: "2304",
            },
          ],
        },
      ],
      isNew: true,
    });
    conclude(ctx, "The edit introduced a type error.");
  },
});

const GREP_CONTENT = scenario({
  name: "grep-content",
  prompt: "!grep-content",
  emits: "a `Grep` in content mode answered with matching lines and a total that exceeds them",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentGrep.start + AgentGrepSuccess.matches=content (extent=partial)",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "grep-content" }, "fake grep (content) turn");
    const call = ctx.toolUse("Grep", { pattern: "export const", path: "/w/s", output_mode: "content" });
    ctx.toolResult(call, "example.ts:1:export const one = 1;\nexample.ts:2:export const two = 2;", {
      mode: "content",
      numFiles: 1,
      filenames: [FILE],
      content: "example.ts:1:export const one = 1;\nexample.ts:2:export const two = 2;",
      numLines: 2,
      numMatches: 2,
      totalFiles: 1,
      // The vendor reports the TOTAL; the omitted figure the converter renders
      // is `totalLines - numLines`, subtracted shim-side and never sent.
      totalLines: 5,
      appliedLimit: 2,
      appliedOffset: 0,
    });
    conclude(ctx, "Found two matching lines.");
  },
});

const GREP_FILES = scenario({
  name: "grep-files",
  prompt: "!grep-files",
  emits: "a `Grep` in files_with_matches mode answered with paths only",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentGrepSuccess.matches=files (extent=all)",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "grep-files" }, "fake grep (files) turn");
    const call = ctx.toolUse("Grep", { pattern: "export", path: "/w/s", output_mode: "files_with_matches" });
    ctx.toolResult(call, FILE, {
      mode: "files_with_matches",
      numFiles: 1,
      filenames: [FILE],
      totalFiles: 1,
    });
    conclude(ctx, "One file matched.");
  },
});

const GREP_COUNT = scenario({
  name: "grep-count",
  prompt: "!grep-count",
  emits: "a `Grep` in count mode answered with per-file counts",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentGrepSuccess.matches=count",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "grep-count" }, "fake grep (count) turn");
    const call = ctx.toolUse("Grep", { pattern: "export", path: "/w/s", output_mode: "count" });
    ctx.toolResult(call, `${FILE}:4`, {
      mode: "count",
      numFiles: 1,
      filenames: [FILE],
      content: `${FILE}:4`,
      numMatches: 4,
      totalFiles: 1,
    });
    conclude(ctx, "Counted four matches.");
  },
});

const GLOB = scenario({
  name: "glob",
  prompt: "!glob",
  emits: "a `Glob` answered with a truncated path list and a total larger than the list",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentGlob.start + AgentGlobSuccess.extent=partial with an omitted count",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "glob" }, "fake glob turn");
    const call = ctx.toolUse("Glob", { pattern: "**/*.ts" });
    ctx.toolResult(call, `${FILE}\n/w/s/other.ts`, {
      durationMs: 12,
      numFiles: 2,
      filenames: [FILE, "/w/s/other.ts"],
      truncated: true,
      // `countIsComplete: true` makes the omitted figure EXACT (7 - 2 = 5);
      // `false` would make it a floor. Both arms exist in the proto and only
      // this flag distinguishes them.
      totalMatches: 7,
      countIsComplete: true,
    });
    conclude(ctx, "Two of several files matched.");
  },
});

export const FILE_SCENARIOS = [
  READ_WHOLE,
  READ_HEAD,
  READ_RANGE,
  READ_TRUNCATED,
  READ_IMAGE,
  WRITE_CREATE,
  WRITE_UPDATE,
  EDIT,
  IDE_DIAGNOSTICS,
  GREP_CONTENT,
  GREP_FILES,
  GREP_COUNT,
  GLOB,
];
