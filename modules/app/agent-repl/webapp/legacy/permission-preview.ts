/**
 * Reconstruct a permission card's preview from the tool input.
 *
 * The daemon used to compute a structured `PermissionPreview` and send it
 * alongside the request. `core.v1.PermissionItem` carries no preview field, so
 * after the S9 cutover the pushed item arrives with the tool NAME and its raw
 * INPUT and nothing else. Without this, a pending card asks "Allow Bash?" with
 * no sight of the command — the user would be approving blind, which is worse
 * than useless for the one control whose entire job is informed consent.
 *
 * The input is the tool's own argument object, so the salient field is derived
 * per known tool and everything else falls back to showing the input itself.
 * Nothing here invents information: a diff preview, for instance, is NOT
 * synthesized, because an edit's input carries no unified diff to show.
 */
import type { PermissionPreview } from "./protocol.js";

/** How much of a written file body the card shows. */
const WRITE_PREVIEW_CHARS = 2000;

function field(input: unknown, key: string): string | undefined {
  if (typeof input !== "object" || input === null || Array.isArray(input)) return undefined;
  const v = (input as Record<string, unknown>)[key];
  return typeof v === "string" ? v : undefined;
}

/**
 * The preview to render for `toolName` called with `input`, or undefined when
 * the input carries nothing worth showing (an argument-less tool).
 */
export function previewFromInput(toolName: string, input: unknown): PermissionPreview | undefined {
  const command = field(input, "command");
  if (toolName === "Bash" && command !== undefined) {
    return { kind: "bash", command };
  }

  const filePath = field(input, "file_path");
  if (toolName === "Write" && filePath !== undefined) {
    const content = field(input, "content") ?? "";
    return {
      kind: "write",
      file_path: filePath,
      bytes: content.length,
      preview: content.slice(0, WRITE_PREVIEW_CHARS),
    };
  }

  // Edit/MultiEdit and friends: name the file being changed. The input has no
  // unified diff, so none is claimed — the file path is the honest summary.
  if (filePath !== undefined) {
    return { kind: "generic", summary: `${toolName}: ${filePath}` };
  }

  // Anything else: show the arguments themselves rather than nothing.
  if (typeof input !== "object" || input === null) return undefined;
  const json = JSON.stringify(input, null, 2);
  if (json === undefined || json === "{}") return undefined;
  return { kind: "generic", summary: json };
}
