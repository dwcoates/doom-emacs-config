package main

import "fmt"

// THE SENTENCES EVERY WORKSPACE VERB'S REFUSAL SHARES. Every verb that
// addresses a workspace can be refused for the same four reasons: the id is
// unknown, the echoed dir disagrees, or a restarting daemon has handed the
// workspace on or not yet taken it over. Each describer says them through
// these, so a caller reads one wording for one cause whichever verb it ran.
// REQUESTED names what the request named ("the prompt", "the request"), and
// AGAIN is what to do once the restart allows ("send the prompt again").

// sayNoWorkspace is the unknown_workspace sentence.
func sayNoWorkspace(ws string) string { return fmt.Sprintf("the daemon has no workspace %s", ws) }

// sayLivesElsewhere is the workspace_ref_mismatch sentence.
func sayLivesElsewhere(ws, registryDir, requested string) string {
	return fmt.Sprintf("%s lives at %s, not at the directory %s named", ws, registryDir, requested)
}

// sayHandedOver is the transferring_away sentence.
func sayHandedOver(ws, successor, again string) string {
	return fmt.Sprintf("the daemon is restarting and handed %s to its successor at %s; %s", ws, successor, again)
}

// sayNotYetAdopted is the not_yet_adopted sentence.
func sayNotYetAdopted(ws, again string) string {
	return fmt.Sprintf("the daemon is restarting and has not taken %s over yet; %s once the restart finishes", ws, again)
}
