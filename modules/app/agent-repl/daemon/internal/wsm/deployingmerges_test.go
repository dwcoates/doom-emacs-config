package wsm

import (
	"context"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// deployingMergeOf is merge workspace WS's landing in the queue keyed REPO,
// still deploying since second AT.
func deployingMergeOf(repo RepoKey, ws WorkspaceID, at int64) DeployingMerge {
	return DeployingMerge{
		Repo: repo, Workspace: ws, Lease: LeaseID("lease-" + ws), Credited: "requester", Async: true, Name: string(ws) + "-name",
		Branch: "feature", TargetDir: "/w/doom", ActiveSince: time.Unix(at-60, 0).UTC(), DeployingSince: time.Unix(at, 0).UTC(), Timed: true,
	}
}

// canonicalRepo is a queue key the store keeps as it is spelled.
func canonicalRepo(t *testing.T) RepoKey {
	t.Helper()
	key, err := normalizeRepoKey(RepoKey(t.TempDir()))
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	return key
}

func TestADeployingMergeIsReadAsItWasRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	want := deployingMergeOf(repo, "mws-a", 100)

	// Act
	replaced, err := s.RecordDeployingMerge(context.Background(), want)

	// Assert
	got, readErr := s.DeployingMergeOf(context.Background(), repo)
	if err != nil || replaced != nil || readErr != nil || got == nil || !reflect.DeepEqual(*got, want) {
		t.Fatalf("record = (%+v, %v), read = (%+v, %v); want %+v recorded with nothing replaced", replaced, err, got, readErr, want)
	}
}

func TestAQueueWithNoDeployingMergeReadsNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)

	// Act
	got, err := s.DeployingMergeOf(context.Background(), repo)

	// Assert
	if err != nil || got != nil {
		t.Fatalf("DeployingMergeOf = (%+v, %v), want none", got, err)
	}
}

func TestANewerDeployingMergeReplacesTheQueuesAndAnswersIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	first, second := deployingMergeOf(repo, "mws-a", 100), deployingMergeOf(repo, "mws-b", 200)
	if _, err := s.RecordDeployingMerge(context.Background(), first); err != nil {
		t.Fatal(err)
	}

	// Act
	replaced, err := s.RecordDeployingMerge(context.Background(), second)

	// Assert
	got, readErr := s.DeployingMergeOf(context.Background(), repo)
	if err != nil || replaced == nil || !reflect.DeepEqual(*replaced, first) {
		t.Fatalf("record = (%+v, %v), want the first answered as replaced", replaced, err)
	}
	if readErr != nil || got == nil || !reflect.DeepEqual(*got, second) {
		t.Fatalf("read = (%+v, %v), want only the second", got, readErr)
	}
}

func TestEndingADeployingMergeTakesItOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	want := deployingMergeOf(repo, "mws-a", 100)
	if _, err := s.RecordDeployingMerge(context.Background(), want); err != nil {
		t.Fatal(err)
	}

	// Act
	first, firstErr := s.EndDeployingMerge(context.Background(), repo, DeployingLanded)
	second, secondErr := s.EndDeployingMerge(context.Background(), repo, DeployingLanded)

	// Assert
	if firstErr != nil || first == nil || !reflect.DeepEqual(*first, want) {
		t.Fatalf("first end = (%+v, %v), want %+v", first, firstErr, want)
	}
	if secondErr != nil || second != nil {
		t.Fatalf("second end = (%+v, %v), want nothing left to end", second, secondErr)
	}
}

func TestADeployingMergeNamingNoInstantIsRefused(t *testing.T) {
	tests := []struct {
		name  string
		blank func(*DeployingMerge)
	}{
		{name: "no admission instant", blank: func(m *DeployingMerge) { m.ActiveSince = time.Time{} }},
		{name: "no deploying instant", blank: func(m *DeployingMerge) { m.DeployingSince = time.Time{} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := canonicalRepo(t)
			m := deployingMergeOf(repo, "mws-a", 100)
			tt.blank(&m)

			// Act
			_, err := s.RecordDeployingMerge(context.Background(), m)

			// Assert
			got, readErr := s.DeployingMergeOf(context.Background(), repo)
			if err == nil || readErr != nil || got != nil {
				t.Fatalf("record = %v, read = (%+v, %v); want the record refused and nothing written", err, got, readErr)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.record_deploying_merge",
				Message: "refused a deploying merge that names no instant"}); !ok {
				t.Fatalf("records = %v, want the refusal at ERROR", logs.Records())
			}
		})
	}
}

// deployingAt records merge workspace WS's landing in queue REPO and moves
// it to STAGE as a deploy does: covered by its commit, checked by the check
// it owes at second AT.
func deployingAt(t *testing.T, s DB, repo RepoKey, ws WorkspaceID, at int64, stage DeployingStage) DeployingMerge {
	t.Helper()
	ctx := context.Background()
	m := deployingMergeOf(repo, ws, at)
	if _, err := s.RecordDeployingMerge(ctx, m); err != nil {
		t.Fatalf("RecordDeployingMerge: %v", err)
	}
	if stage >= DeployingCovered {
		if _, err := s.CoverDeployingLandings(ctx); err != nil {
			t.Fatalf("CoverDeployingLandings: %v", err)
		}
	}
	if stage >= DeployingChecked {
		if _, err := s.OweMediation(ctx, repo, time.Unix(at, 0).UTC(), ""); err != nil {
			t.Fatalf("OweMediation: %v", err)
		}
	}
	m.Stage = stage
	return m
}

// stageOf reads queue REPO's deploying merge's stage, failing when none
// stands.
func stageOf(t *testing.T, s DB, repo RepoKey) DeployingStage {
	t.Helper()
	m, err := s.DeployingMergeOf(context.Background(), repo)
	if err != nil || m == nil {
		t.Fatalf("DeployingMergeOf = (%+v, %v), want one standing", m, err)
	}
	return m.Stage
}

func TestADeployingMergeIsRecordedLandedWhateverItSays(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	m := deployingMergeOf(repo, "mws-a", 100)
	m.Stage = DeployingChecked

	// Act
	_, err := s.RecordDeployingMerge(context.Background(), m)

	// Assert
	if err != nil || stageOf(t, s, repo) != DeployingLanded {
		t.Fatalf("RecordDeployingMerge = %v, stage %v; want it recorded landed", err, stageOf(t, s, repo))
	}
}

func TestADeployCoversOnlyTheLandedMergesNoDeployCovered(t *testing.T) {
	tests := []struct {
		from        DeployingStage
		wantStage   DeployingStage
		wantCovered int
	}{
		{from: DeployingLanded, wantStage: DeployingCovered, wantCovered: 1},
		{from: DeployingCovered, wantStage: DeployingCovered},
		{from: DeployingChecked, wantStage: DeployingChecked},
	}
	for _, tt := range tests {
		t.Run(tt.from.String(), func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := canonicalRepo(t)
			deployingAt(t, s, repo, "mws-a", 100, tt.from)

			// Act
			covered, err := s.CoverDeployingLandings(context.Background())

			// Assert
			if err != nil || covered.Merges != tt.wantCovered || stageOf(t, s, repo) != tt.wantStage {
				t.Fatalf("CoverDeployingLandings = (%+v, %v), stage %v; want %d covered, stage %v", covered, err, stageOf(t, s, repo), tt.wantCovered, tt.wantStage)
			}
		})
	}
}

func TestOwingACheckChecksOnlyTheMergeItsDeployCovered(t *testing.T) {
	tests := []struct {
		from      DeployingStage
		wantStage DeployingStage
	}{
		{from: DeployingLanded, wantStage: DeployingLanded},
		{from: DeployingCovered, wantStage: DeployingChecked},
		{from: DeployingChecked, wantStage: DeployingChecked},
	}
	for _, tt := range tests {
		t.Run(tt.from.String(), func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := canonicalRepo(t)
			deployingAt(t, s, repo, "mws-a", 100, tt.from)

			// Act
			_, err := s.OweMediation(context.Background(), repo, time.Unix(200, 0).UTC(), "")

			// Assert
			if err != nil || stageOf(t, s, repo) != tt.wantStage {
				t.Fatalf("OweMediation = %v, stage %v; want %v", err, stageOf(t, s, repo), tt.wantStage)
			}
		})
	}
}

func TestReleasingACheckEndsOnlyTheMergeItChecks(t *testing.T) {
	tests := []struct {
		name         string
		arrange      func(t *testing.T, s DB, repo RepoKey) DeployingMerge
		release      time.Time
		wantReleased bool
		wantEnded    bool
	}{
		{name: "the merge its deploy covered ends with it", release: time.Unix(100, 0).UTC(), wantReleased: true, wantEnded: true,
			arrange: func(t *testing.T, s DB, repo RepoKey) DeployingMerge {
				return deployingAt(t, s, repo, "mws-a", 100, DeployingChecked)
			}},
		{name: "a merge a later deploy covered stands", release: time.Unix(100, 0).UTC(), wantReleased: true,
			arrange: func(t *testing.T, s DB, repo RepoKey) DeployingMerge {
				if _, err := s.OweMediation(context.Background(), repo, time.Unix(100, 0).UTC(), ""); err != nil {
					t.Fatalf("OweMediation: %v", err)
				}
				return deployingAt(t, s, repo, "mws-a", 150, DeployingCovered)
			}},
		{name: "a release of an older deploy's check ends nothing", release: time.Unix(50, 0).UTC(),
			arrange: func(t *testing.T, s DB, repo RepoKey) DeployingMerge {
				return deployingAt(t, s, repo, "mws-a", 100, DeployingChecked)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := canonicalRepo(t)
			want := tt.arrange(t, s, repo)

			// Act
			got, err := s.ReleaseMediation(context.Background(), repo, tt.release)

			// Assert
			if err != nil || got.Released != tt.wantReleased {
				t.Fatalf("ReleaseMediation = (%+v, %v), want released %v", got, err, tt.wantReleased)
			}
			if tt.wantEnded != (got.Deploying != nil) || (got.Deploying != nil && !reflect.DeepEqual(*got.Deploying, want)) {
				t.Fatalf("ended %+v, want %+v ended = %v", got.Deploying, want, tt.wantEnded)
			}
			standing, err := s.DeployingMergeOf(context.Background(), repo)
			if err != nil || (standing == nil) != tt.wantEnded {
				t.Fatalf("standing after the release = (%+v, %v), want ended %v", standing, err, tt.wantEnded)
			}
		})
	}
}

func TestEndingADeployingMergeEndsItOnlyFromItsStage(t *testing.T) {
	tests := []struct {
		at, from  DeployingStage
		wantEnded bool
	}{
		{at: DeployingLanded, from: DeployingCovered},
		{at: DeployingCovered, from: DeployingCovered, wantEnded: true},
		{at: DeployingCovered, from: DeployingChecked},
		{at: DeployingChecked, from: DeployingChecked, wantEnded: true},
	}
	for _, tt := range tests {
		t.Run(tt.at.String()+" from "+tt.from.String(), func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := canonicalRepo(t)
			deployingAt(t, s, repo, "mws-a", 100, tt.at)

			// Act
			ended, err := s.EndDeployingMerge(context.Background(), repo, tt.from)

			// Assert
			if err != nil || (ended != nil) != tt.wantEnded {
				t.Fatalf("EndDeployingMerge = (%+v, %v), want ended %v", ended, err, tt.wantEnded)
			}
		})
	}
}

func TestUncheckedDeployingMergesAnswersOnlyThoseWhoseDeployOwesNoCheck(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	landed, covered, checked := canonicalRepo(t), canonicalRepo(t), canonicalRepo(t)
	deployingAt(t, s, checked, "mws-c", 100, DeployingChecked)
	deployingAt(t, s, covered, "mws-b", 200, DeployingCovered)
	deployingAt(t, s, landed, "mws-a", 300, DeployingLanded)

	// Act
	got, err := s.UncheckedDeployingMerges(context.Background())

	// Assert
	stages := map[WorkspaceID]DeployingStage{}
	for _, m := range got {
		stages[m.Workspace] = m.Stage
	}
	want := map[WorkspaceID]DeployingStage{"mws-a": DeployingLanded, "mws-b": DeployingCovered}
	if err != nil || !reflect.DeepEqual(stages, want) {
		t.Fatalf("UncheckedDeployingMerges = (%+v, %v), want %v", got, err, want)
	}
}

func TestTheDeployCheckHoldReadsTheLandedMergeStillDeploying(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	want := deployingAt(t, s, repo, "mws-a", 100, DeployingLanded)

	// Act
	hold, err := s.DeployCheckHoldOf(context.Background(), repo)

	// Assert
	if err != nil || hold.Deploying == nil || !reflect.DeepEqual(*hold.Deploying, want) || hold.Mediation != nil {
		t.Fatalf("DeployCheckHoldOf = (%+v, %v), want only %+v holding", hold, err, want)
	}
}

func TestTheMigrationReadsAnEarlierDeployingMergeAsChecked(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 205)
	repo := canonicalRepo(t)
	execRaw(t, path, `INSERT INTO deploying_merges (repo_key, workspace, lease, credited, async, name, branch, target_dir, active_since, deploying_since, timed)
		VALUES ('`+string(repo)+`', 'mws-a', 'lease-a', 'mws-a', 0, 'a', 'b', '/w/doom', 1, 2, 1)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-205 database: %v", err)
	}
	defer handle.Close()

	// Assert
	if stage := stageOf(t, handle, repo); stage != DeployingChecked {
		t.Fatalf("stage after the migration = %v, want checked", stage)
	}
}

func TestDeployingStageNamesEveryStage(t *testing.T) {
	tests := []struct {
		stage DeployingStage
		want  string
	}{
		{stage: DeployingLanded, want: "landed"},
		{stage: DeployingCovered, want: "covered"},
		{stage: DeployingChecked, want: "checked"},
		{stage: DeployingStage(9), want: "DeployingStage(9)"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.stage.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String = %q, want %q", got, tt.want)
			}
		})
	}
}
