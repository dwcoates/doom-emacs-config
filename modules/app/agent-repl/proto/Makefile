# Codegen for the agent-shim protobuf protocol.
#
# Outputs:
#   Go  → gen/go/...   (consumed by daemon/, agent-shim/shim-store/, agent-shim/claude/shim-sidecar/)
#   TS  → gen/ts/...   (consumed by agent-shim/claude/shim/, webapp/)
#
# Requires: protoc, protoc-gen-go, and @bufbuild/protoc-gen-es (via npx).

# Listed by SURFACE, most upstream first. conversation imports nothing, so it
# leads; state is daemon-internal and depends on everything, so it trails.
PROTOS := \
  conversation/v1/content.proto \
  conversation/v1/message.proto \
  conversation/v1/payloads.proto \
  conversation/v1/tokens.proto \
  protocol/v1/bookkeeping.proto \
  protocol/v1/core.proto \
  protocol/v1/entry-delivery.proto \
  protocol/v1/external.proto \
  protocol/v1/message-page.proto \
  store/v1/cursor.proto \
  store/v1/entry.proto \
  store/v1/unsupported.proto \
  store/v1/write.proto \
  agentrepl/v1/shared.proto \
  frontend/v1/feed.proto \
  frontend/v1/topbar.proto \
  frontend/v1/sidebar.proto \
  frontend/v1/footer.proto \
  agentrepl/v1/frame.proto \
  state/v1/durable.proto

# The component tree the structural gates scan. Overridable so the gate
# self-tests can drive the REAL targets against a fixture tree; the default is
# the schema tree this Makefile generates from.
COMPONENT_DIR ?= frontend/v1
# The tree I7 scans for forbidden imports. Overridable so the codegen-gate
# self-test can point it at its own fixture root.
ISOLATION_ROOT ?= ..

.PHONY: all go ts lint codegen-gate conversation-isolation test-check-conversation-isolation test-check-generated check-generated validate coverage clean

all: go ts

# EVERY path that emits bindings passes through this target. The structural
# invariants below are not review notes, they are preconditions on codegen: a
# schema that has already drifted must not be able to produce bindings by any
# Makefile route, so `go`, `ts`, `all`, and `lint` all depend on this rather
# than on `lint` alone. New structural gates are added here, once.
codegen-gate: conversation-isolation

go: codegen-gate
	mkdir -p gen/go
	protoc -I . --go_out=gen/go --go_opt=paths=source_relative $(PROTOS)

ts: codegen-gate
	mkdir -p gen/ts
	npx --yes @bufbuild/protoc-gen-es --version >/dev/null
	protoc -I . \
	  --plugin=protoc-gen-es=$$(npx --yes --package=@bufbuild/protoc-gen-es which protoc-gen-es 2>/dev/null || echo ./node_modules/.bin/protoc-gen-es) \
	  --es_out=gen/ts --es_opt=target=ts $(PROTOS)

# Syntax-check every proto without emitting code, then enforce the structural
# invariants the syntax check cannot see.
lint: codegen-gate
	protoc -I . --descriptor_set_out=/dev/null $(PROTOS)

# INVARIANT I6 IS GONE, AND ITS GATE WITH IT. `durable.proto` needed a script
# only while it sat INSIDE frontend/v1 being definitionally not frontend, so the
# one thing separating persistence evidence from the push surface was a grep.
# It is now `state.v1`, its own package, its own Go import path and its own TS
# module — the compiler enforces what the script used to check.

# INVARIANT I7. store.v1 is the half of a stored record that never crosses
# the shim wire — the observation plane, the store's dedup key, and anything the
# producer could not convert. No daemon or webapp source may import it. It is a
# separate PACKAGE rather than a separate file precisely so the boundary is a Go
# import path and a TS module rather than a convention; this gate refuses the one
# remaining way to cross it.
conversation-isolation:
	./check-conversation-isolation.sh $(ISOLATION_ROOT)

test-check-conversation-isolation:
	./test-check-conversation-isolation.sh

# Regenerate and compare with the exact pre-generation bytes. Comparing with
# HEAD rejects a correct uncommitted schema edit, which makes the pre-commit
# validation gate impossible to satisfy.
check-generated:
	test -d gen
	@before="$$(mktemp -d "$${TMPDIR:-/tmp}/agent-repl-proto-gen.XXXXXX")"; \
	trap 'rm -rf "$$before"' EXIT; \
	cp -R gen/. "$$before"/; \
	$(MAKE) all; \
	diff -ru "$$before" gen

test-check-generated:
	./test-check-generated.sh

# Validate both the source schema and the checked-in Go/TS generated artifacts.
validate: lint test-check-conversation-isolation test-check-generated check-generated

# Generated stubs are dependencies, not coverage subjects.  Each consumer's
# own coverage configuration scopes measurement to handwritten package source.
coverage: validate
	../bin/report-nonlisp-coverage.sh daemon sidecar store wire webapp shim

clean:
	rm -rf gen
