package sessioncontroller

import "errors"

// Resync preserves the compact test vocabulary used by the pre-generation
// controller tests.  Production code has no unconditioned resync entry point:
// every wire command must enter ResyncForGeneration with its snapshot identity.
func (m *Manager) Resync(workspace string, fromSeq uint64) error {
	d, err := m.existing(workspace)
	if errors.Is(err, ErrNoLiveSessionController) {
		return m.resyncFromDurableHistory(workspace, fromSeq)
	}
	if err != nil {
		return err
	}
	return m.resyncFromController(d, fromSeq, d.sessionID, d.generationID)
}
