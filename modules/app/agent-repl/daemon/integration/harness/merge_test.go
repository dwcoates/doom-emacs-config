package harness

import "testing"

func TestOwnBranchNamesTheRequestersBranchAndItsKeepOpen(t *testing.T) {
	for _, keepOpen := range []bool{false, true} {
		// Act
		source := OwnBranch(keepOpen)

		// Assert
		own := source.GetOwnBranch()
		if own == nil || own.GetKeepOpen() != keepOpen {
			t.Fatalf("OwnBranch(%v) = %v, want the own-branch arm with keep_open %v", keepOpen, source, keepOpen)
		}
	}
}

func TestBranchSourceNamesTheBranch(t *testing.T) {
	// Act
	source := BranchSource("agent-fix")

	// Assert
	if source.GetBranch().GetName() != "agent-fix" {
		t.Fatalf("BranchSource = %v, want the branch arm naming agent-fix", source)
	}
}

func TestIsLandingDeployedRecognizesOnlyTheLandingsDoneRecord(t *testing.T) {
	cases := []struct {
		name   string
		record LogRecord
		want   bool
	}{
		{name: "the landing's done record", record: LogRecord{Operation: "daemon.deploy.landing", Message: "the landing is deployed"}, want: true},
		{name: "another record of the landing's deploy", record: LogRecord{Operation: "daemon.deploy.landing", Message: "deploying the landing"}},
		{name: "the same words under another operation", record: LogRecord{Operation: "daemon.deploy.run", Message: "the landing is deployed"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert
			if got := IsLandingDeployed(tc.record); got != tc.want {
				t.Fatalf("IsLandingDeployed(%+v) = %v, want %v", tc.record, got, tc.want)
			}
		})
	}
}
