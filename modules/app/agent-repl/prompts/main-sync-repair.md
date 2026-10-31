<!-- used by: daemon/internal/mainsync (the main checkout's fast-forward to its upstream failed); placeholders: {{repository}}, {{branch}}, {{upstream}}, {{local_sha}}, {{upstream_sha}}, {{cause}} -->
The daemon could not bring this repository's main checkout up to its upstream.

A merge landed on `{{upstream}}`, so the daemon tried to fast-forward `{{branch}}` in `{{repository}}` from `{{local_sha}}` to `{{upstream_sha}}`, and could not:

{{cause}}

Bring `{{branch}}` in this checkout to `{{upstream_sha}}`, keeping any local work that should survive: commit or set aside what is in the way, and reconcile a diverged branch the way this repository lands work. Do not push anything to the upstream to make the two meet. Once `{{branch}}` is no longer behind, the daemon's next sweep finds nothing to do; if it is still behind, the sweep tries the fast-forward again.
