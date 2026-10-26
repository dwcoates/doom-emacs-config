package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// The operations UpdateSidebarView's records carry, one per change.
const (
	opFoldMergedSection = "daemon.workspace.fold_merged_section"
	opFoldHiddenSection = "daemon.workspace.fold_hidden_section"
	opFoldOpenSection   = "daemon.workspace.fold_open_section"
)

// FoldMergedSection records whether the merge queue section is collapsed,
// then republishes the roster to every page. There is nothing to refuse: the
// roster carries exactly one.
func (v *verbs) FoldMergedSection(ctx context.Context, folded bool) error {
	return v.foldSingletonSection(ctx, opFoldMergedSection, "merge queue section", v.deps.DB.SetMergedSectionFolded, folded)
}

// FoldHiddenSection records whether the hidden-workspaces section is
// collapsed, then republishes the roster to every page. There is nothing to
// refuse: the roster carries exactly one hidden section.
func (v *verbs) FoldHiddenSection(ctx context.Context, folded bool) error {
	return v.foldSingletonSection(ctx, opFoldHiddenSection, "hidden section", v.deps.DB.SetHiddenSectionFolded, folded)
}

// FoldOpenSection records whether the open-workspaces section is collapsed,
// then republishes the roster to every page. There is nothing to refuse: the
// roster carries exactly one open section.
func (v *verbs) FoldOpenSection(ctx context.Context, folded bool) error {
	return v.foldSingletonSection(ctx, opFoldOpenSection, "open section", v.deps.DB.SetOpenSectionFolded, folded)
}

// foldSingletonSection records the fold of a section the roster carries
// exactly one of (no key, so nothing to refuse), then republishes the roster
// to every page (recordView).
func (v *verbs) foldSingletonSection(ctx context.Context, op, section string, write func(context.Context, bool) error, folded bool) error {
	return v.recordView(ctx, op, "the "+section+"'s fold", dlog.Context{"folded": folded, "section": section},
		func(ctx context.Context) error { return write(ctx, folded) })
}

// recordView records one piece of the sidebar's view state that belongs to
// no keyed section, WHAT naming it, then republishes the roster to every
// page. A failed write is recorded at error under OP with FIELDS, and
// nothing is republished.
func (v *verbs) recordView(ctx context.Context, op, what string, fields dlog.Context, write func(context.Context) error) error {
	log := v.deps.Log.Global().With(fields)
	if err := write(ctx); err != nil {
		log.Error(op, "could not record "+what, dlog.Context{"cause": err.Error()})
		return fmt.Errorf("record %s: %w", what, err)
	}
	log.Info(op, "recorded "+what, nil)
	v.republishRegistry(ctx, log, op)
	return nil
}
