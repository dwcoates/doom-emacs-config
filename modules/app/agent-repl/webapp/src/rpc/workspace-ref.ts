/**
 * The ONE place a WorkspaceRef is built in this webapp.
 *
 * Identities are DAEMON-MINTED ECHO TOKENS: `id` is opaque, compared
 * byte-wise, never parsed and never derived from a path. Everything else in
 * the app takes a ref from a view and hands it straight back, so the only ref
 * this end ever CONSTRUCTS is the one for the workspace its own URL names —
 * and it constructs it once, here, from the page address.
 *
 * BOTH FIELDS COME OFF THE URL. The webview is launched with
 * `?workspace=<id>&dir=<dir>`, so `dir` is the daemon's own normalized
 * spelling relayed through the address rather than anything this end derived.
 * It is display material and NOT an identifier — the daemon routes on the id —
 * but a request that carried an empty one would make every surface drawing the
 * directory blank, so it is required at the address and complete here.
 */
import { create } from "@bufbuild/protobuf";
import {
  WorkspaceRefSchema,
  type WorkspaceRef,
} from "../../../proto/gen/ts/workspace/v1/workspace_pb";

/** The ref addressing every request this page makes. */
export function workspaceRef(id: string, dir: string): WorkspaceRef {
  return create(WorkspaceRefSchema, { id, dir });
}
