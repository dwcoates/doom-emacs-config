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
 * A section's fold gesture: the other fold, painted on every copy of the
 * section at once and asked of the daemon, whose push carries it to every
 * page (`view-change.ts`).
 */
export function sectionFold(sc: SidebarContext, foldKey: string, target: FoldTarget): FoldGesture {
  return (section, control) => {
    const collapse = !section.classList.contains("folded");
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
