// Package rostertest is the SINGLE SOURCE of workspace-roster fixtures.
//
// # Why this package exists
//
// The roster contract used to be written down twice: once as a fixture in the
// frontend package's unit tests and once, independently, as a fixture in the
// e2e suite. Two hand-built copies of one contract drift the moment the
// contract gains a required field — and they did. When `boot_id` became
// required, the unit fixture was amended and the e2e fixture was not, so eight
// e2e tests failed against a validator that was behaving exactly as specified.
// The tests were wrong; the daemon was right; and nothing in the build could
// have said so, because neither fixture was checked against the validator.
//
// Single-sourcing removes the drift rather than fixing one instance of it.
// Every roster any suite publishes is built here, so a future required field is
// added in ONE place, and `TestTheSharedRosterFixtureSatisfiesTheValidator` (in
// the frontend package, which is where `validateRoster` lives) fails at the
// commit that tightens the validator — before any e2e wave runs.
//
// # The shape
//
// [ValidRoster] is deliberately the RICHER of the two fixtures it replaced: the
// repository grouping, a folded section that still carries its rows, a nested
// child row, a hoisted recently_merged section, and a current_dir / nav_dir
// pair that differ. A fixture that exercised less would silently weaken every
// assertion written against the richer one.
//
// # The illegal shapes
//
// Tests of the REFUSALS need rosters the validator must reject. They get them
// from the named [Option] helpers below, never by hand-editing a fixture at the
// call site: a named helper makes the bypass visible, keeps the illegality
// spelled out in exactly one place, and keeps the legal fixture legal.
package rostertest

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// The identifiers [ValidRoster] builds with. They are exported so an assertion
// can name the row a refusal must mention without restating the string.
const (
	// RepoKey is the unfolded repository section's key.
	RepoKey = "github.com/example/doom"
	// FoldedRepoKey is the FOLDED repository section's key. A folded section
	// still carries its rows: folding is a display state, so unfolding needs no
	// republish.
	FoldedRepoKey = "github.com/example/other"

	// CurrentRowDir is the row marked current, and the roster's current_dir.
	CurrentRowDir = "/tmp/roster/doom-main"
	// ChildRowDir is CurrentRowDir's NESTED child row.
	ChildRowDir = "/tmp/roster/doom-main-spawn"
	// MergingRowDir is the second top-level row, and the roster's nav_dir —
	// which differs from current_dir because the cursor moves through the
	// roster without switching workspaces.
	MergingRowDir = "/tmp/roster/doom-merging"
	// FoldedRowDir is the row inside the folded section.
	FoldedRowDir = "/tmp/roster/other-parked"
	// RecentlyMergedRowDir is the row in the hoisted recently_merged section,
	// which is reachable under both view arms and so is validated independently
	// of whichever one is set.
	RecentlyMergedRowDir = "/tmp/roster/doom-settled"
)

// An Option mutates a freshly built roster. Every option that produces an
// ILLEGAL roster says so in its name and its doc comment.
type Option func(*frontendv1.WorkspaceRoster)

// ValidRoster builds a complete, contract-legal roster under publisher epoch
// bootID at the given revision, then applies opts in order.
//
// bootID and revision are parameters rather than constants because the two
// invariants the daemon enforces are stated in exactly those terms: revisions
// are monotonic WITHIN one boot_id, and a different boot_id opens a new epoch
// whose revision is not comparable to the retained one.
//
// Passing an empty bootID or a non-positive revision builds a roster the
// validator refuses. That is intentional and legible at the call site — the
// argument itself names the illegality — so those two cases need no Option.
func ValidRoster(bootID string, revision int64, opts ...Option) *frontendv1.WorkspaceRoster {
	roster := &frontendv1.WorkspaceRoster{
		Revision: revision,
		BootId:   bootID,
		View: &frontendv1.WorkspaceRoster_Repository{
			Repository: &frontendv1.RosterRepositoryView{
				Sections: []*frontendv1.RosterRepoSection{
					{
						RepoKey: RepoKey,
						Folded:  false,
						Rows: []*frontendv1.RosterRow{
							{
								Dir:     CurrentRowDir,
								Name:    "doom-main",
								Current: true,
								Status:  &frontendv1.RosterRow_Ready{Ready: &frontendv1.RosterRowStatusReady{}},
								Children: []*frontendv1.RosterRow{{
									Dir:    ChildRowDir,
									Name:   "doom-main-spawn",
									Status: &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}},
								}},
							},
							{
								Dir:    MergingRowDir,
								Name:   "doom-merging",
								Status: &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}},
							},
						},
					},
					{
						RepoKey: FoldedRepoKey,
						Folded:  true,
						Rows: []*frontendv1.RosterRow{{
							Dir:    FoldedRowDir,
							Name:   "other-parked",
							Status: &frontendv1.RosterRow_Hibernated{Hibernated: &frontendv1.RosterRowStatusHibernated{}},
						}},
					},
				},
			},
		},
		RecentlyMerged: &frontendv1.RosterSection{
			Rows: []*frontendv1.RosterRow{{
				Dir:    RecentlyMergedRowDir,
				Name:   "doom-settled",
				Status: &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}},
			}},
		},
		CurrentDir: CurrentRowDir,
		NavDir:     MergingRowDir,
	}
	for _, opt := range opts {
		opt(roster)
	}
	return roster
}

// WithoutView clears the view oneof, which is ILLEGAL: the SET ARM IS THE
// GROUPING, so with none set there is no grouping and no rows — which is not
// the same thing as an empty roster (an empty repository view is a perfectly
// good empty roster).
func WithoutView() Option {
	return func(roster *frontendv1.WorkspaceRoster) { roster.View = nil }
}

// WithStatuslessRow strips the status oneof from the top-level row at
// [MergingRowDir], which is ILLEGAL: the SET ARM IS THE STATUS, so an unset
// oneof is the ABSENCE of an assertion rather than a value to default. ("The
// author looked and there is no status" has its own arm — RosterRowStatusNone.)
func WithStatuslessRow() Option {
	return func(roster *frontendv1.WorkspaceRoster) {
		roster.GetRepository().GetSections()[0].GetRows()[1].Status = nil
	}
}

// WithStatuslessChildRow strips the status oneof from the NESTED row at
// [ChildRowDir], which is ILLEGAL for the same reason as [WithStatuslessRow]: a
// child row is a row, and is held to the same standard recursively.
func WithStatuslessChildRow() Option {
	return func(roster *frontendv1.WorkspaceRoster) {
		roster.GetRepository().GetSections()[0].GetRows()[0].GetChildren()[0].Status = nil
	}
}

// WithStatuslessRecentlyMergedRow strips the status oneof from the row at
// [RecentlyMergedRowDir], which is ILLEGAL. recently_merged is hoisted out of
// the view oneof, so it is validated independently of whichever view arm is set
// and needs its own coverage.
func WithStatuslessRecentlyMergedRow() Option {
	return func(roster *frontendv1.WorkspaceRoster) {
		roster.GetRecentlyMerged().GetRows()[0].Status = nil
	}
}
