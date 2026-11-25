/**
 * folded-class — THE ONE CLASS a folded sidebar box wears: a repository
 * section, a subsection, a family's row, the Recently Merged and Backlog
 * sections and a deploys group alike. The stylesheet hides a box's body under
 * it, and every drawer and reader of a fold names it from here, so no site can
 * spell a fold of its own. It is a leaf module, because the fold drawers
 * (`section-fold.ts`, `deploys-fold.ts`) and the band fitter (`band-fit.ts`)
 * import one another's neighbours and a shared constant must not close a
 * cycle.
 */

/** The class a folded sidebar box wears. */
export const FOLDED_CLASS = "folded";
