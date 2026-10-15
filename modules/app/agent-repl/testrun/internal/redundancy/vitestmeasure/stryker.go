// Package vitestmeasure is the vitest adapter of `testrun redundancy`: per
// test case, the statements it reaches (istanbul counters diffed around each
// test) and the mutants it kills (Stryker's report, coverageAnalysis
// perTest with bail disabled, so every killer of every mutant is named).
package vitestmeasure

import (
	"encoding/json"
	"fmt"
	"sort"

	"agentrepl/testrun/internal/redundancy"
)

// strykerReport is the part of a mutation-testing-report-schema (v2)
// report the adapter reads.
type strykerReport struct {
	Files map[string]struct {
		Mutants []struct {
			ID          string   `json:"id"`
			MutatorName string   `json:"mutatorName"`
			Replacement string   `json:"replacement"`
			Status      string   `json:"status"`
			KilledBy    []string `json:"killedBy"`
			CoveredBy   []string `json:"coveredBy"`
			Location    struct {
				Start struct {
					Line   int `json:"line"`
					Column int `json:"column"`
				} `json:"start"`
			} `json:"location"`
		} `json:"mutants"`
	} `json:"files"`
	TestFiles map[string]struct {
		Tests []struct {
			ID   string `json:"id"`
			Name string `json:"name"`
		} `json:"tests"`
	} `json:"testFiles"`
}

// Matrix is a Stryker report read into the measurement's terms: every
// mutant with the case IDs ("<test file>:<full name>") killing and reaching
// it, and the mutants whose killers Stryker could not name.
type Matrix struct {
	Mutants []redundancy.MutantResult
	// Cases are every test the report names, by case ID.
	Cases []string
	// Unattributed are the mutants Stryker detected by a timeout or a
	// runtime error, which name no killer: a deletion keeps the cheapest
	// deleted case reaching one, since none can be shown not to catch it.
	Unattributed []string
}

// ParseStryker reads a Stryker JSON report. A mutant status the adapter does
// not know is refused, never guessed.
func ParseStryker(data []byte) (*Matrix, error) {
	var r strykerReport
	if err := json.Unmarshal(data, &r); err != nil {
		return nil, fmt.Errorf("vitestmeasure: decode the Stryker report: %w", err)
	}
	if len(r.Files) == 0 {
		return nil, fmt.Errorf("vitestmeasure: the Stryker report names no mutated file")
	}
	ids := map[string]string{}
	var cases []string
	for file, tf := range r.TestFiles {
		for _, t := range tf.Tests {
			id := file + ":" + t.Name
			if prev, dup := ids[t.ID]; dup && prev != id {
				return nil, fmt.Errorf("vitestmeasure: Stryker test id %s names both %q and %q", t.ID, prev, id)
			}
			ids[t.ID] = id
			cases = append(cases, id)
		}
	}
	name := func(tid string) (string, error) {
		id, ok := ids[tid]
		if !ok {
			return "", fmt.Errorf("vitestmeasure: a mutant names test id %s, which the report's testFiles do not hold", tid)
		}
		return id, nil
	}
	out := &Matrix{}
	var files []string
	for f := range r.Files {
		files = append(files, f)
	}
	sort.Strings(files)
	for _, f := range files {
		for _, mu := range r.Files[f].Mutants {
			key := fmt.Sprintf("%s:%d:%d:%s:%s", f, mu.Location.Start.Line, mu.Location.Start.Column, mu.MutatorName, mu.Replacement)
			res := redundancy.MutantResult{Key: key}
			for _, tid := range mu.KilledBy {
				id, err := name(tid)
				if err != nil {
					return nil, err
				}
				res.KilledBy = append(res.KilledBy, id)
			}
			for _, tid := range mu.CoveredBy {
				id, err := name(tid)
				if err != nil {
					return nil, err
				}
				res.CoveredBy = append(res.CoveredBy, id)
			}
			sort.Strings(res.KilledBy)
			sort.Strings(res.CoveredBy)
			switch mu.Status {
			case "Killed":
				if len(res.KilledBy) == 0 {
					return nil, fmt.Errorf("vitestmeasure: mutant %s is Killed by no named test: run Stryker with coverageAnalysis perTest", key)
				}
				res.Status = "killed"
			case "Survived":
				res.Status = "survived"
			case "NoCoverage":
				res.Status = "unreached"
			case "CompileError", "Ignored":
				res.Status = "dropped"
			case "Timeout", "RuntimeError":
				res.Status = "unattributed"
				out.Unattributed = append(out.Unattributed, key)
			default:
				return nil, fmt.Errorf("vitestmeasure: mutant %s has status %q, which the adapter does not know", key, mu.Status)
			}
			out.Mutants = append(out.Mutants, res)
		}
	}
	sort.Strings(cases)
	out.Cases = cases
	return out, nil
}
