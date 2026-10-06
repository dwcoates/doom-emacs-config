package workspace

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The operations UpdateSidebarView's records carry, one per change.
const (
	opFoldTaskSection   = "daemon.workspace.fold_task_section"
	opFoldMergedSection = "daemon.workspace.fold_merged_section"
	opShowGrouping      = "daemon.workspace.show_grouping"
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
	log := v.deps.Log.Global().With(dlog.Context{"folded": folded})
	if err := v.deps.DB.SetMergedSectionFolded(ctx, folded); err != nil {
		log.Error(opFoldMergedSection, "could not record the recently-merged band's fold", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold the recently-merged band: %w", err)
	}
	log.Info(opFoldMergedSection, "recorded the recently-merged band's fold", nil)
	v.republishRegistry(ctx, log, opFoldMergedSection)
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
