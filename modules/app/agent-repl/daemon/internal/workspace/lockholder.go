package workspace

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// describeLockHolderFailure words how a shim's own kernel-lock holder failed,
// as the refusal's reason states it. It is the daemon's half of the account
// the shim's locks.ts words the same way, and the arm is the account: each
// case says what that arm carries.
//
// ok is false when the failure states no `how`, which the contract forbids;
// the caller records that breach and still relays the refusal the shim gave.
func describeLockHolderFailure(failure *conversationv1.LockHolderFailure) (text string, ok bool) {
	switch how := failure.GetHow().(type) {
	case *conversationv1.LockHolderFailure_SpawnFailed:
		return fmt.Sprintf("could not be spawned: %s", how.SpawnFailed.GetOsError()), true
	case *conversationv1.LockHolderFailure_Exited:
		return fmt.Sprintf("exited with code %d before taking the lock%s",
			how.Exited.GetCode(), stderrSuffix(how.Exited.GetStderr())), true
	case *conversationv1.LockHolderFailure_Signaled:
		return fmt.Sprintf("was killed by %s before taking the lock%s",
			how.Signaled.GetSignal(), stderrSuffix(how.Signaled.GetStderr())), true
	case *conversationv1.LockHolderFailure_Misanswered:
		return fmt.Sprintf("answered %q instead of \"locked\" and was killed", how.Misanswered.GetLine()), true
	case *conversationv1.LockHolderFailure_Silent:
		return fmt.Sprintf("gave no \"locked\" answer within %d ms and was killed", how.Silent.GetTimeoutMs()), true
	default:
		return "failed without saying how", false
	}
}

// stderrSuffix appends a holder's stderr only when it said something.
func stderrSuffix(stderr string) string {
	if stderr == "" {
		return ""
	}
	return " (" + stderr + ")"
}
