/**
 * The ONE place a WorkspaceRef is built in this webapp.
 *
 * Identities are DAEMON-MINTED ECHO TOKENS: `id` is opaque, compared
 * byte-wise, never parsed and never derived from a path. Everything else in
 * the app takes a ref from a view and hands it straight back, so the only ref
 * this end ever CONSTRUCTS is the one for the workspace its own URL names.
 *
 * `dir` IS PROVISIONAL. The webview knows only the id — its address carries no
 * path, and the daemon's own registry holds the normalized directory — so this
 * sends the empty string. The proto states outright that `dir` is display
 * material and NOT an identifier, so the daemon routes on the id regardless.
 * Pending the project lead's ruling: either the daemon accepts an empty `dir`
 * on request addressing (the reading taken here), or the address must carry
 * the directory too, in which case this function grows a second argument and
 * every caller keeps working. Concentrating the construction here is what
 * makes that a one-line change.
 */
import { create } from "@bufbuild/protobuf";
import {
  WorkspaceRefSchema,
  type WorkspaceRef,
} from "../../../proto/gen/ts/workspace/v1/workspace_pb";

/** The ref addressing every request this page makes. */
export function workspaceRefFromId(id: string): WorkspaceRef {
  return create(WorkspaceRefSchema, { id, dir: "" });
}
