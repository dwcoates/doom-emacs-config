/**
 * section-fold — THE ONE FOLD every daemon-held sidebar fold draws: its ▸/▾
 * triangle, the gesture that asks for the other fold, and the tracking that
 * paints the wire's fold unless this page's own ask is still in flight
 * (`view.ts`).
 *
 * Every section box (`roster.ts`) and every subsection (`subsections.ts`)
 * folds through here, so no two of them can disagree on how a fold is asked
 * for or painted.
 */
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { SidebarViewFoldSectionSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import type { SidebarContext } from "./context.js";
import { log } from "../log.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { changeView } from "./view-change.js";
import { foldViewKey, paintSectionFold, paintTriangle } from "./view.js";

/**
 * What a section header's fold gesture does, handed the section and the
 * control the gesture landed on.
 */
export type FoldGesture = (section: HTMLElement, control: HTMLElement) => void;

/** Which section an UpdateSidebarView fold names. */
export type FoldTarget = MessageInitShape<typeof SidebarViewFoldSectionSchema>["section"];

/**
 * The attribute a repository section or subsection box carries while the
 * daemon says its expanded fold holds the current workspace
 * (`frontend.v1.RosterRepoSectionExpanded.collapse`,
 * `frontend.v1.RosterSubsectionExpanded.collapse`). It lives on the box
 * rather than in the gesture, so a push that moves the selection repaints it
 * without redrawing the section (`roster.ts`'s row patch).
 */
export const HOLDS_CURRENT_ATTRIBUTE = "data-holds-current";

/**
 * A section's fold gesture: the other fold, painted on every copy of the
 * section at once and asked of the daemon, whose push carries it to every
 * page (`view-change.ts`).
 *
 * A SECTION HOLDING THE CURRENT WORKSPACE NEVER COLLAPSES (owner request,
 * 2026-10-20): collapsing it would stand down the session the user is in, so
 * the gesture asks nothing while the box carries `HOLDS_CURRENT_ATTRIBUTE`.
 * The daemon refuses such a collapse too, for a page a push behind a switch.
 * The other way round, selecting a workspace in a collapsed section expands
 * it daemon-side, so its push repaints the fold (wsm's setCurrent).
 */
export function sectionFold(sc: SidebarContext, foldKey: string, target: FoldTarget): FoldGesture {
  return (section, control) => {
    const collapse = !section.classList.contains("folded");
    if (collapse && section.hasAttribute(HOLDS_CURRENT_ATTRIBUTE)) {
      log.debug("the section holds the current workspace; its fold gesture asks nothing", {
        operation: "sidebar.fold.holds-current",
        context: { section: foldKey },
      });
      return;
    }
    // A FOLD THAT ASKS IS RECORDED at info: a collapse hides a section's
    // workspaces and stands down the ones it does not spare, so the daemon's
    // fold record (`daemon.workspace.fold_repository`, named by this
    // connection's request id) is matched to the gesture that asked for it.
    log.info(collapse ? "the section's fold gesture asks the daemon to collapse it" : "the section's fold gesture asks the daemon to expand it", {
      operation: "sidebar.fold.asked",
      context: { section: foldKey, collapse },
    });
    void changeView(sc, {
      key: foldViewKey(foldKey),
      value: collapse,
      control,
      change: {
        case: "foldSection",
        value: {
          section: target,
          fold: collapse ? { case: "collapse", value: {} } : { case: "expand", value: {} },
        },
      },
    });
  };
}

/**
 * The fold gesture of a section that NEVER COLLAPSES: the one-shot subsection
 * (`frontend.v1.RosterSubsectionNeverCollapses`), whose workspaces work
 * unattended, so a collapse that stood their sessions down would stop the
 * work it shows. The gesture asks nothing, as on a section holding the
 * current workspace.
 */
export function neverFolds(foldKey: string): FoldGesture {
  return () => {
    log.debug("the section never collapses; its fold gesture asks nothing", {
      operation: "sidebar.fold.never-collapses",
      context: { section: foldKey },
    });
  };
}

/**
 * Fold SECTION as the wire says (WIRE_FOLDED), unless this page's own ask
 * under FOLD_KEY is still in flight, and keep it painted as the ask settles.
 */
export function trackDaemonFold(section: HTMLElement, foldKey: string, wireFolded: boolean, sc: SidebarContext): void {
  const folded = sc.view.track(foldViewKey(foldKey), wireFolded, (value) => paintSectionFold(section, value));
  section.classList.toggle("folded", folded);
}

/** The ▸/▾ fold triangle, drawn the way its section stands. */
export function drawFoldToggle(section: HTMLElement, gesture: FoldGesture): HTMLElement {
  const triangle = document.createElement("span");
  triangle.className = "tri";
  triangle.setAttribute("data-section-fold", "");
  triangle.addEventListener("click", (event) => {
    event.stopPropagation();
    gesture(section, triangle);
  });
  paintTriangle(triangle, section.classList.contains("folded"));
  return triangle;
}

/**
 * A section's two-arm fold oneof, as every folding section spells it: the arm
 * IS the fold, each a dedicated empty message.
 */
export type RosterSectionFold =
  | { case: "expanded"; value: unknown }
  | { case: "collapsed"; value: unknown }
  | { case: undefined; value?: undefined };

/** Whether a section is collapsed. EVERY arm is named; unset is malformed. */
export function drawRosterSectionFold(fold: RosterSectionFold, path: string): boolean {
  const arm = requireCase(fold, path);
  switch (arm.case) {
    case "expanded":
      return false;
    case "collapsed":
      return true;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * Whether an expanded repository section or subsection may collapse, as the
 * daemon resolved it. Both carry the same two arms in their own messages; a
 * subsection's carries a third, `neverCollapses`, which only the one-shot
 * subsection sets.
 */
export type RosterExpandedCollapse =
  | { case: "collapsible"; value: unknown }
  | { case: "holdsCurrent"; value: unknown }
  | { case: "neverCollapses"; value: unknown }
  | { case: undefined; value?: undefined };

/** A repository section's or subsection's fold, whose expanded arm says whether it may collapse. */
export type CollapsibleSectionFold =
  | { case: "expanded"; value: { collapse: RosterExpandedCollapse } }
  | { case: "collapsed"; value: unknown }
  | { case: undefined; value?: undefined };

/**
 * Whether FOLD holds the current workspace: only an expanded fold can, and
 * it says so. EVERY arm is named; an unset fold or collapse is malformed.
 */
export function drawRosterHoldsCurrent(fold: CollapsibleSectionFold, path: string): boolean {
  const arm = requireCase(fold, path);
  switch (arm.case) {
    case "collapsed":
      return false;
    case "expanded": {
      const collapse = requireCase(arm.value.collapse, `${path}.expanded.collapse`);
      switch (collapse.case) {
        case "collapsible":
          return false;
        case "holdsCurrent":
          return true;
        // The one-shot subsection's arm says it never collapses, and nothing
        // of the current workspace.
        case "neverCollapses":
          return false;
        default: {
          const other: { case: string } = collapse;
          return unreachableArm(`${path}.expanded.collapse`, other.case);
        }
      }
    }
    default: {
      const other: { case: string } = arm;
      return unreachableArm(path, other.case);
    }
  }
}

/** Say on SECTION's box whether it holds the current workspace. */
export function paintHoldsCurrent(section: HTMLElement, holdsCurrent: boolean): void {
  section.toggleAttribute(HOLDS_CURRENT_ATTRIBUTE, holdsCurrent);
}
