package runtimemetrics

import "github.com/prometheus/client_golang/prometheus"

// LogRecordsName counts the Warn and Error records the daemon wrote, its own
// and the ones it writes for Emacs and the webapp, by runtime, level and
// operation (dlog.Surfaces.BindRecordCounter). A stall beat or a stream that
// keeps failing is a rate here, so the version where it started can be found.
const LogRecordsName = "agentrepl_log_records_total"

// newRecords answers the record counter. Each count is client_golang's own
// atomic add.
func newRecords() *prometheus.CounterVec {
	return prometheus.NewCounterVec(prometheus.CounterOpts{
		Name: LogRecordsName,
		Help: "Warn and Error records the daemon wrote, its own and Emacs's and the webapp's, by runtime, level and operation.",
	}, []string{"runtime", "level", "operation"})
}

// CountRecord implements dlog.RecordCounter. The off surface counts nothing.
func (s *Surface) CountRecord(runtime, level, operation string) {
	if s == nil {
		return
	}
	s.records.WithLabelValues(runtime, level, operation).Inc()
}
