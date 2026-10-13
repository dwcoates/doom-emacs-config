package workspace

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// The operations UpdateSidebarView's records carry, one per change.
const (
	opFoldTaskSection     = "daemon.workspace.fold_task_section"
	opFoldMergedSection   = "daemon.workspace.fold_merged_section"
	opFoldHiddenSection   = "daemon.workspace.fold_hidden_section"
	opFoldMaturitySection = "daemon.workspace.fold_maturity_section"
	opShowGrouping        = "daemon.workspace.show_grouping"
)

// FoldTaskSection records whether a task's roster section is collapsed, then
// republishes the roster, so EVERY page draws the one fold (owner ruling,
// 2026-10-06: a sidebar fold is no workspace's own). Asking for the fold the
// section already holds is success: two pages may race to the same fold.
func (v *verbs) FoldTaskSection(ctx context.Context, task ids.TaskID, folded bool) error {
	log := v.deps.Log.Global().With(dlog.Context{"task": string(task), "folded": folded})
	if err := v.deps.DB.SetTaskFolded(ctx, task, folded); err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			return refuse(log, "UpdateSidebarView", ArmUnknownTask,
				fmt.Sprintf("no task %q is recorded", task), true)
		}
		log.Error(opFoldTaskSection, "could not record the task section's fold", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold task section %q: %w", task, err)
	}
	log.Info(opFoldTaskSection, "recorded the task section's fold", nil)
	v.republishRegistry(ctx, log, opFoldTaskSection)
	return nil
}

// FoldMergedSection records whether the recently-merged band is collapsed,
// then republishes the roster to every page. There is nothing to refuse: the
// roster carries exactly one band.
func (v *verbs) FoldMergedSection(ctx context.Context, folded bool) error {
	return v.foldSingletonSection(ctx, opFoldMergedSection, "recently-merged band", v.deps.DB.SetMergedSectionFolded, folded)
}

// FoldHiddenSection records whether the hidden-workspaces section is
// collapsed, then republishes the roster to every page. There is nothing to
// refuse: the roster carries exactly one hidden section.
func (v *verbs) FoldHiddenSection(ctx context.Context, folded bool) error {
	return v.foldSingletonSection(ctx, opFoldHiddenSection, "hidden section", v.deps.DB.SetHiddenSectionFolded, folded)
}

// foldSingletonSection records the fold of a section the roster carries
// exactly one of (no key, so nothing to refuse), then republishes the roster
// to every page. A failed write is recorded at error under OP, naming
// SECTION, and nothing is republished.
func (v *verbs) foldSingletonSection(ctx context.Context, op, section string, write func(context.Context, bool) error, folded bool) error {
	log := v.deps.Log.Global().With(dlog.Context{"folded": folded, "section": section})
	if err := write(ctx, folded); err != nil {
		log.Error(op, "could not record the "+section+"'s fold", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold the %s: %w", section, err)
	}
	log.Info(op, "recorded the "+section+"'s fold", nil)
	v.republishRegistry(ctx, log, op)
	return nil
}

// FoldMaturitySection records whether one maturity stage's section is
// collapsed, then republishes the roster to every page. There is nothing to
// refuse: the stages are fixed, and the server decodes the stage arm.
func (v *verbs) FoldMaturitySection(ctx context.Context, stage wsm.MaturityStage, folded bool) error {
	log := v.deps.Log.Global().With(dlog.Context{"stage": string(stage), "folded": folded})
	if err := v.deps.DB.SetMaturityFolded(ctx, stage, folded); err != nil {
		log.Error(opFoldMaturitySection, "could not record the maturity stage's fold", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold the %s maturity section: %w", stage, err)
	}
	log.Info(opFoldMaturitySection, "recorded the maturity stage's fold", nil)
	v.republishRegistry(ctx, log, opFoldMaturitySection)
	return nil
}

// ShowGrouping records which grouping every page shows, then republishes the
// roster.
func (v *verbs) ShowGrouping(ctx context.Context, grouping wsm.Grouping) error {
	log := v.deps.Log.Global().With(dlog.Context{"grouping": string(grouping)})
	if err := v.deps.DB.SetGrouping(ctx, grouping); err != nil {
		log.Error(opShowGrouping, "could not record the grouping shown", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("show the %s grouping: %w", grouping, err)
	}
	log.Info(opShowGrouping, "recorded the grouping every page shows", nil)
	v.republishRegistry(ctx, log, opShowGrouping)
	return nil
}
