/**
 * mention-targets — the ONE way a webapp test builds the workspace mention
 * targets the daemon serves (frontend.v1.WorkspaceMentionTargets), so every
 * test's target carries the standing a served target always has.
 */
import { create } from "@bufbuild/protobuf";
import {
  WorkspaceMentionTargetsSchema,
  type WorkspaceMentionTargets,
} from "../../../proto/gen/ts/frontend/v1/workspace_mention_pb";

/** One served target, as `create` takes it. */
export interface MentionTargetInit {
  name: { text: string };
  workspace: { id: string; dir: string };
  standing: { case: "open" | "closed"; value: Record<string, never> };
}

/** An open target naming NAME, its workspace id ID (`ws-<name>`) at DIR (`/w/<name>`). */
export function openTarget(name: string, id = `ws-${name}`, dir = `/w/${name}`): MentionTargetInit {
  return { name: { text: name }, workspace: { id, dir }, standing: { case: "open", value: {} } };
}

/** A recently closed target naming NAME, as `openTarget` but closed. */
export function closedTarget(name: string, id = `ws-${name}`, dir = `/w/${name}`): MentionTargetInit {
  return { ...openTarget(name, id, dir), standing: { case: "closed", value: {} } };
}

/** The served set holding TARGETS, in order. */
export function mentionTargets(...targets: MentionTargetInit[]): WorkspaceMentionTargets {
  return create(WorkspaceMentionTargetsSchema, { targets });
}
