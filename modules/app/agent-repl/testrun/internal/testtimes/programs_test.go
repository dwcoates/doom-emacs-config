package testtimes

import "testing"

func TestProgramFoldsEveryExecutableIntoTheClosedVocabulary(t *testing.T) {
	cases := []struct{ exe, want string }{
		{"_.test", ProgramTestBinary},
		{"integration.test", ProgramTestBinary},
		{"agent-repld", ProgramDaemon},
		{"fakeshim", ProgramFakeShim},
		{"git", ProgramGit},
		{"covdata", ProgramCovdata},
		{"testrun", ProgramTestrun},
		{"compile", ProgramGo},
		{"node", ProgramNode},
		{"Emacs", ProgramEmacs},
		{"bash", ProgramShell},
		{"shim-store", ProgramStore},
		{"shim-claude-sidecar", ProgramSidecar},
		{"zettel", ProgramZettel},
		{"agent-repl-tests", ProgramTestService},
		{"lsregister", ProgramOther},
		{"", ProgramOther},
	}
	for _, tc := range cases {
		t.Run(tc.exe, func(t *testing.T) {
			// Act
			got := Program(tc.exe)

			// Assert
			if got != tc.want {
				t.Fatalf("Program(%q) = %q; want %q", tc.exe, got, tc.want)
			}
		})
	}
}
