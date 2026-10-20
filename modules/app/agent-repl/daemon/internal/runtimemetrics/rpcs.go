package runtimemetrics

import (
	"time"

	"github.com/prometheus/client_golang/prometheus"
)

// RPCName is the histogram every unary rpc the daemon answers lands in,
// labeled by `rpc` (its method) and `outcome` ("ok" or its Connect code).
const RPCName = "agentrepl_rpc_seconds"

// rpcBuckets span a unary rpc from a cached read to a slow write.
var rpcBuckets = []float64{0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10}

// newRPCs answers the rpc histogram. Its observations are client_golang's
// own atomic adds, so timing a call takes no lock any request takes.
func newRPCs() *prometheus.HistogramVec {
	return prometheus.NewHistogramVec(prometheus.HistogramOpts{
		Name:    RPCName,
		Help:    "How long the daemon took to answer a unary rpc, from its arrival to its handler's return, by rpc and outcome.",
		Buckets: rpcBuckets,
	}, []string{"rpc", "outcome"})
}

// ObserveRPC implements server.RPCRecorder. The off surface records nothing.
func (s *Surface) ObserveRPC(rpc, outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.rpcs.WithLabelValues(rpc, outcome).Observe(took.Seconds())
}
