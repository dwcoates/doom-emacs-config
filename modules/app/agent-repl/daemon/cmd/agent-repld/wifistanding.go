package main

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/viewquiet"
)

// persistentWifiTopbar is what takes the persistent-wifi standing for the
// topbar's chip (topbar.Resolver).
type persistentWifiTopbar interface {
	SetPersistentWifi(state *agentreplv1.PersistentWifiState)
}

// wifiStandingReceiver is what the daemon does with each standing its
// persistent-wifi client follows: the view gate follows the quiet state the
// standing carries, and the chip takes the standing. A standing the service
// did not serve (service_unreachable) carries no quiet state, so the gate
// lifts: with the service gone, nothing holds the machine quiet.
func wifiStandingReceiver(gate *viewquiet.Gate, topbar persistentWifiTopbar) func(*agentreplv1.PersistentWifiState) {
	return func(standing *agentreplv1.PersistentWifiState) {
		gate.Set(standing.GetQuiet() != nil)
		topbar.SetPersistentWifi(standing)
	}
}
