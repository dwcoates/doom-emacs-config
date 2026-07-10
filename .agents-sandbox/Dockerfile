FROM ghcr.io/catthehacker/ubuntu:act-22.04

ENV DEBIAN_FRONTEND=noninteractive

# Signal to agent-repl's host-only scripts (e.g. .claude/install.sh, doctor
# checks) that we are running inside the agent sandbox and they should
# no-op rather than touch the host-flavored ~/.claude/ layout.
ENV DOOM_SANDBOX=1

RUN apt-get update && apt-get install -y \
    ca-certificates curl git ripgrep uuid-runtime emacs-nox \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    rm -rf /var/lib/apt/lists/*

# npm 11.16+ gates package lifecycle scripts by default (the `allow-scripts`
# model, distinct from the `ignore-scripts` flag), so claude-code's postinstall
# does NOT run during `npm install` and never links its platform-native binary,
# leaving `claude` a stub that dies at container start with "exec format error".
# Installing the platform package explicitly guarantees the native binary is
# present (the image is always built --platform linux/amd64), and the
# `claude --version` check fails the build loudly if the CLI still is not
# runnable instead of silently baking a broken stub.
RUN npm install -g @anthropic-ai/claude-code @anthropic-ai/claude-code-linux-x64 && \
    claude --version
RUN npm install -g @openai/codex

RUN useradd -m -u 1000 -s /bin/bash claude
RUN git config --global --add safe.directory '*' && \
    git config --system --add safe.directory '*'

USER claude
WORKDIR /home/claude
