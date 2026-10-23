// Package storeimport sends samples to the always-on metrics store's import
// endpoint, the one way anything writes into the store other than its own
// scrapes.
//
// The store's address and its scrape job are named by File under the metrics
// root (the installer copies metrics/store.json there). A pushed sample is
// labeled with that job, so it lands on the series the store's scrapes write,
// plus whatever labels the caller names.
//
// It imports nothing but the standard library, so a program that does not
// link the Prometheus client (the test runner) pushes through the same code a
// runtime's final values do (metrics.Server).
package storeimport

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
)

// File is the file under the metrics root that names the store's import
// endpoint and the scrape job its series belong to.
const File = "store.json"

// store is File's content.
type store struct {
	// ImportURL takes Prometheus text exposition.
	ImportURL string `json:"import_url"`
	// Job is the scrape config's job_name.
	Job string `json:"job"`
}

// Post sends BODY, Prometheus text exposition, to the import endpoint File
// under ROOT names, labeling every sample with the store's job and each of
// LABELS ("name=value"). Every failure is answered, never absorbed: a missing
// or incomplete File, an unreachable store, and a store that refuses.
func Post(ctx context.Context, client *http.Client, root string, body []byte, labels ...string) error {
	raw, err := os.ReadFile(filepath.Join(root, File))
	if err != nil {
		return fmt.Errorf("read the store's address: %w", err)
	}
	var st store
	if err := json.Unmarshal(raw, &st); err != nil || st.ImportURL == "" || st.Job == "" {
		return fmt.Errorf("%s names no import_url and job (%v)", File, err)
	}
	target, err := url.Parse(st.ImportURL)
	if err != nil {
		return fmt.Errorf("import_url %q: %w", st.ImportURL, err)
	}
	q := target.Query()
	for _, l := range append([]string{"job=" + st.Job}, labels...) {
		q.Add("extra_label", l)
	}
	target.RawQuery = q.Encode()
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, target.String(), bytes.NewReader(body))
	if err != nil {
		return err
	}
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("reach %s: %w", st.ImportURL, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode/100 != 2 {
		detail, _ := io.ReadAll(io.LimitReader(resp.Body, 512))
		return fmt.Errorf("the samples were refused by %s: %s %s", st.ImportURL, resp.Status, bytes.TrimSpace(detail))
	}
	return nil
}
