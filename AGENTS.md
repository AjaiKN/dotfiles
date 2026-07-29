# AGENTS.md

Notes for coding agents working in this repo. See `README.md` for the human-facing overview and basic repository structure (`config/`, `dot-home/`, `bin/`, `scripts/`, `launchd/`, `nix/`, `vendor/`, `private/`).

## This repo is live, not just source

Files under `config/`, `dot-home/`, `launchd/`, etc. are installed into `$HOME` / `$XDG_CONFIG_HOME` by a **custom hand-rolled installer** (`install-files.sh`), not GNU Stow — Stow only shows up elsewhere as an installed package dependency (e.g. `install.debian.sh`), it isn't what actually links these files. `install-files.sh` generally symlinks whole directories (e.g. `~/.config` -> `config/`) rather than per-file, except where a directory contains a `.unfold` marker file, which makes it recurse and symlink that directory's contents individually instead. So on this machine, editing e.g. `config/zsh/...` or `config/git/config` here is editing the user's actual live shell/git/etc. config, the same way `config/doom` is the live Emacs config (see below).

## Other repo-structure notes

- `private/` is a git submodule pointing at a separate private repo.
- `vendor/` holds vendored third-party code (`fasd`, `zap`, `stow.sh`) — read-only upstream snapshots, not something to edit.
- `nix/` (home-manager, nix-darwin, nixos) and `guix/` (Guix Home) are experiments the user has tried at various points, not maintained alternatives to the primary `install-files.sh`-based install — don't assume they need to stay in sync with it, and don't treat them as evidence of "three parallel provisioning systems" to reconcile.
- `.gitmodules` lists many more submodules than are actually checked out. `submodules-update.sh` (run by `install`/`install-files.sh`) only `git submodule init`s a fixed subset (`config/nano`, `vendor`, `config/zsh/themes`) plus whatever paths are passed explicitly as args, then updates whatever's already initialized. So most of `config/zsh/plugins/*/*` (individual zsh plugins, each its own submodule/vendored upstream code) start out **uninitialized** and appear as empty directories — that's expected, not missing/broken content. Run `git submodule status` to see current state (a `-` prefix means uninitialized); don't try to "fix" an empty plugin dir by writing content into it.

## Commit message conventions

In recent history (`git log`), the first line is consistently `scope: description`, all lowercase, no trailing period, terse/imperative rather than full sentences:
- The scope is usually the top-level directory or subsystem touched (`emacs:`, `shell:`, `git:`, `keymapper:`, `install:`, `guix:`, `vscode:`). `emacs:` dominates by far, since it's the most active part of the repo.
- Scopes can nest for a second level of specificity, e.g. `emacs: llm-extra:`, `emacs: mediawiki:`, `emacs: completion:` — subsystem, then the more specific module/area within it.
- Changes that don't fit neatly under one subsystem sometimes skip the scope prefix entirely (e.g. `set readline history-size in .inputrc`, `exclude vendor/ and config/zsh/plugins/ from editorconfig`).

## Emacs layout

- `~/.config/emacs` is a symlink to `config/emacs` in this repo. It's a full from-source checkout of the Doom Emacs framework itself (not just config) — `config/emacs/README.md` is upstream Doom's own README, not something to edit.
- `~/.config/doom` is a symlink to `config/doom` in this repo. This is the actual personal config: `+major-modes.el`, `+general.el`, `+buffers.el`, `config.el`, and private module overrides/additions under `config/doom/modules/`. Changes belong here.
- Doom was restructured upstream: the official module sources (e.g. `:lang python`, `:lang common-lisp`) now live at `config/emacs/sources/doom+/modules/`, not under `config/doom/`. This is vendored framework code — treat it as read-only reference (e.g. to check what a module does or doesn't already set up); put overrides/customizations in `config/doom/` instead, typically via `(after! <feature> ...)` blocks in `+major-modes.el` or a private module under `config/doom/modules/`.
- `config/doom/modules/` also contains a separate library of the user's own standalone Doom modules (see `config/doom/modules/README.org`), distinct from the personal `~/.config/doom` customization layer above — these are meant to be reusable by other Doom users too, so keep them relatively generic/self-contained rather than coupling them to personal preferences (those belong in `+major-modes.el`/`+general.el`/etc. instead). They can depend on `config/doom/lisp/akn.el`, though.
- After changing which modules/packages are enabled (`config/doom/+modules.el`, `config/doom/packages.el`), Doom itself expects a `doom sync` run to pick that up — this is separate from and in addition to the daemon-restart caveat below (a `doom sync` without a subsequent daemon restart won't take effect either).

## Working with the live Emacs

- Emacs runs as persistent daemons via launchd, not started fresh per session. Two are configured:
  - `~/Library/LaunchAgents/dev.ajai.emacsdaemonserver.plist` (symlink to `launchd/dev.ajai.emacsdaemonserver.plist`) — the main/GUI daemon, default `emacsclient` socket.
  - `~/Library/LaunchAgents/dev.ajai.emacsdaemonterm.plist` (symlink to `launchd/dev.ajai.emacsdaemonterm.plist`) — a separate daemon for terminal clients, reached via `emacsclient --socket-name=term`.
  - For Linux, the same two daemons are run using systemd - see `config/systemd/user/emacs@.service`.
- **Always use `emacsclient`, never `emacs`/`emacs --batch`**, for evaluating elisp, checking parens, byte-compiling, etc. — there's always a live server to talk to.
- Editing a config `.el` file does **not** hot-reload into the running daemon(s). You can verify a fix against the live daemon via `emacsclient --eval ...`.
- Since state (buffers, hooks, rings, etc.) persists indefinitely across "sessions" in this daemon model, in-memory state can go stale relative to the source files in ways that a fresh `emacs` invocation would never exhibit — e.g. an old buffer created before a fix was loaded can still carry forward pre-fix behavior (buffer-local hooks, variables) until it's actually killed/recreated.
