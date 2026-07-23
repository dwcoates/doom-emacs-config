# Codegen for the agent-shim protobuf protocol.
#
# Outputs:
#   Go  → gen/go/...   (consumed by daemon/, shim-store/, shim-sidecar/)
#   TS  → gen/ts/...   (consumed by shim/, webapp/)
#
# Requires: protoc, protoc-gen-go, and @bufbuild/protoc-gen-es (via npx).

PROTOS := \
  agentshim/core/v1/core.proto \
  agentshim/claude/v1/tools.proto \
  agentshim/claude/v1/stream.proto \
  agentshim/claude/v1/transcript.proto \
  agentshim/claude/v1/journal.proto \
  agentshim/frontend/v1/frontend.proto

.PHONY: all go ts lint clean

all: go ts

go:
	mkdir -p gen/go
	protoc -I . --go_out=gen/go --go_opt=paths=source_relative $(PROTOS)

ts:
	mkdir -p gen/ts
	npx --yes @bufbuild/protoc-gen-es --version >/dev/null
	protoc -I . \
	  --plugin=protoc-gen-es=$$(npx --yes --package=@bufbuild/protoc-gen-es which protoc-gen-es 2>/dev/null || echo ./node_modules/.bin/protoc-gen-es) \
	  --es_out=gen/ts --es_opt=target=ts $(PROTOS)

# Syntax-check every proto without emitting code.
lint:
	protoc -I . --descriptor_set_out=/dev/null $(PROTOS)

clean:
	rm -rf gen
