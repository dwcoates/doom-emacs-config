package cores

import "fmt"

// UnitEnv tags a unit's process tree with the unit's tag (UnitTag). The
// runner sets it on every unit it starts, and every process the unit starts
// inherits it, whatever group or session it moves to, as it does the run's
// mark (NestedEnv). Two readers count CPU by it: the CPU governor, which
// tells a just-started unit's measured use from the rest of the tests'
// (Governor.Units), and each test's CPU share (cputime).
const UnitEnv = "AGENT_REPL_TESTRUN_UNIT"

// UnitTag is unit ID's tag in the run whose process is PID: the ONE shape of
// a unit's tag, so the runner that sets it, the ledger that states it and
// every reader that counts by it name the same unit alike.
func UnitTag(pid int, id string) string { return fmt.Sprintf("%d/%s", pid, id) }

// UnitEnvEntry is the environment entry that tags unit ID's tree in the run
// whose process is PID.
func UnitEnvEntry(pid int, id string) string { return UnitEnv + "=" + UnitTag(pid, id) }
