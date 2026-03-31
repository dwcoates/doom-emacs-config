# Doom Emacs Configuration

Personal Doom Emacs configuration at `~/.config/doom`.

## Structure

```
.
├── init.el       # Enabled Doom modules
├── config.el     # Personal configuration
├── packages.el   # Additional package declarations
└── modules/
    ├── app/
    │   ├── chess/        # Chess functionality
    │   └── claude-repl/  # Claude AI REPL
    ├── config/
    │   ├── personal-bindings/    # Custom keybindings
    │   ├── treemacs/             # File explorer config
    │   ├── doom-tomorrow-night/  # Theme configuration
    │   ├── vc/                   # Version control enhancements
    │   └── javascript/           # JavaScript settings
    └── lang/
        ├── personal-cc/   # C/C++ with LSP
        ├── personal-org/  # Org-mode customizations
        └── protobuf/      # Protocol buffer support
```

## Key Bindings

| Key | Action |
|-----|--------|
| `SPC g h` | Copy GitHub link for current line/selection |
| `SPC b p` | Toggle between last two buffers |
| `SPC w f` | Toggle window fullscreen |
| `SPC c u` | Compile with unit tests |
| `C-h/j/k/l` | Window navigation |

## Management

```bash
doom sync     # Sync after editing init.el or packages.el
doom upgrade  # Update Doom and packages
doom doctor   # Check for issues
doom env      # Refresh environment variables
```

## Environment Variables

- `EMACS_FULL_NAME` — Full name for templates
- `EMACS_EMAIL` — Email for templates
