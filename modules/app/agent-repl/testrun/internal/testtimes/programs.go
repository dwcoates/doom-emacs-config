package testtimes

import "strings"

// THE PROGRAM LABEL IS A CLOSED VOCABULARY. A unit's CPU is read by process
// (cputime.Sampler) and each process is named by its executable
// (proctable.Usage.Program); Program folds every executable a test run starts
// into one of these, so the series' label never grows with a new binary name.
const (
	ProgramTestBinary  = "test_binary"
	ProgramDaemon      = "daemon"
	ProgramFakeShim    = "fakeshim"
	ProgramGit         = "git"
	ProgramCovdata     = "covdata"
	ProgramTestrun     = "testrun"
	ProgramGo          = "go_toolchain"
	ProgramNode        = "node"
	ProgramEmacs       = "emacs"
	ProgramShell       = "shell"
	ProgramStore       = "store"
	ProgramSidecar     = "sidecar"
	ProgramZettel      = "zettel"
	ProgramTestService = "test_service"
	ProgramOther       = "other"
	// ProgramUnsampled is the CPU the unit's wait accounting holds beyond
	// every read of its tree (withRemainder): the leader's use after the last
	// read, and what an unmarked parent reaped. No read named its program.
	ProgramUnsampled = "unsampled"
)

// programs maps an executable's base name to its label; Program handles the
// names that come in families (a Go test binary, the go command's tools).
var programs = map[string]string{
	"agent-repld":         ProgramDaemon,
	"fakeshim":            ProgramFakeShim,
	"git":                 ProgramGit,
	"covdata":             ProgramCovdata,
	"testrun":             ProgramTestrun,
	"go":                  ProgramGo,
	"compile":             ProgramGo,
	"link":                ProgramGo,
	"asm":                 ProgramGo,
	"vet":                 ProgramGo,
	"cgo":                 ProgramGo,
	"cover":               ProgramGo,
	"node":                ProgramNode,
	"emacs":               ProgramEmacs,
	"Emacs":               ProgramEmacs,
	"bash":                ProgramShell,
	"sh":                  ProgramShell,
	"zsh":                 ProgramShell,
	"dash":                ProgramShell,
	"shim-store":          ProgramStore,
	"shim-claude-sidecar": ProgramSidecar,
	"zettel":              ProgramZettel,
	"agent-repl-tests":    ProgramTestService,
}

// Program is the label of the executable named EXE (a process's argv[0] base
// name): a Go test binary (`_.test`, `<pkg>.test`) is ProgramTestBinary, a
// listed executable its own label, and every other ProgramOther.
func Program(exe string) string {
	if strings.HasSuffix(exe, ".test") {
		return ProgramTestBinary
	}
	if label, ok := programs[exe]; ok {
		return label
	}
	return ProgramOther
}
