// Renaming a workspace from the sidebar: a double-click on a row's name opens
// the name popup (`name-popup.ts`), as a double-click on a subsection's name
// does, and sends SetWorkspaceName. The row's new name arrives on the roster
// push the answer names, and the drawing is held until then (`sendHeld`), so
// the old name is never drawn again once the popup closes.

import {
  SetWorkspaceNameResponseSchema,
  type SetWorkspaceNameError,
  type SetWorkspaceNameResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_name_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { unreachableArm } from "../rpc/strict.js";
import { sendHeld } from "./arrange.js";
import type { SidebarContext } from "./context.js";
import { openNamePopup } from "./name-popup.js";
import type { CauseOf } from "./verbs.js";

/**
 * Make LABEL, a row's name, rename WORKSPACE (now named NAME) on a
 * double-click. Only a row of a live grouping takes it: a Recently Merged or
 * Hidden row's click is its band's own gesture, which a double-click would
 * fire twice.
 */
export function renameOnDoubleClick(label: HTMLElement, sc: SidebarContext, workspace: WorkspaceRef, name: string): void {
  label.title = "double-click to rename";
  label.addEventListener("dblclick", (event) => {
    event.preventDefault();
    event.stopPropagation();
    openNamePopup(sc, label, {
      subject: "workspace",
      placeholder: "workspace name",
      value: name,
      submitLabel: "Rename",
      send: (newName, control) => renameWorkspace(sc, control, workspace, newName),
    });
  });
}

/** Send the rename of WORKSPACE to NAME, saying a refusal at CONTROL. */
function renameWorkspace(sc: SidebarContext, control: HTMLElement, workspace: WorkspaceRef, name: string): Promise<boolean> {
  return sendHeld(sc, control, {
    rpc: "SetWorkspaceName",
    call: (client) => client.setWorkspaceName({ workspace, name }),
    schema: SetWorkspaceNameResponseSchema,
    refusalText: (cause) => setWorkspaceNameRefusal(cause as CauseOf<SetWorkspaceNameError>),
    revision: (response: SetWorkspaceNameResponse) =>
      response.result.case === "success" ? response.result.value.rosterRevision : null,
  });
}

/**
 * SetWorkspaceName's arms of its own. The cross-cutting four are worded once
 * in `rpc/refusal.ts`.
 */
export function setWorkspaceNameRefusal(cause: CauseOf<SetWorkspaceNameError>): string {
  switch (cause.case) {
    case "invalidName": {
      const rule = cause.value.rule.case;
      if (rule === "slug") return `"${cause.value.component}" is not lowercase words joined by hyphens`;
      if (rule === "prefix") return `the prefix "${cause.value.component}" is not letters and digits`;
      return unreachableArm("SetWorkspaceNameInvalidName.rule", String(rule));
    }
    case "nameTaken": {
      const holder = cause.value.holder.case;
      if (holder === "workspace") return `another workspace is already named ${cause.value.name}, or works on that branch`;
      if (holder === "pending") return `${cause.value.name} is being given to a workspace that is being created`;
      return unreachableArm("SetWorkspaceNameTaken.holder", String(holder));
    }
    default:
      return unreachableArm("SetWorkspaceNameError.cause", cause.case);
  }
}
