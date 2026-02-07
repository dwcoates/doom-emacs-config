# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a Doom Emacs configuration directory located at `~/.config/doom`. It contains a comprehensive Emacs setup with custom modules, keybindings, and configuration tailored for development work.

## Core Architecture

### Main Configuration Files
- **`init.el`** - Controls which Doom modules are enabled and their loading order
- **`config.el`** - Main personal configuration and customizations
- **`packages.el`** - Package declarations and additional package installations
- **`custom.el`** - Custom Emacs settings (untracked)

### Module System
Custom modules are organized in the `modules/` directory with three main categories:

#### Language Modules (`modules/lang/`)
- **`personal-cc/`** - C/C++ development configuration with LSP support
- **`personal-org/`** - Org-mode customizations for note-taking and task management
- **`protobuf/`** - Protocol buffer file support

#### Configuration Modules (`modules/config/`)
- **`personal-bindings/`** - Custom keybindings and window navigation
- **`treemacs/`** - File explorer configuration
- **`doom-tomorrow-night/`** - Custom theme configuration
- **`vc/`** - Version control enhancements
- **`javascript/`** - JavaScript development settings

#### Application Modules (`modules/app/`)
- **`chess/`** - Chess-related functionality

## Key Features and Customizations

### Development Tools
- **LSP Integration** - Enabled for C/C++, Go, JavaScript, Python, PHP, Swift, and Shell
- **Tree-sitter** - Syntax highlighting for multiple languages
- **Magit** - Git integration with custom keybindings
- **Company** - Code completion with reduced idle delay (0.1s)
- **Compilation** - Enhanced compilation buffer with auto-jump to errors

### Custom Functions
- **GitHub Integration** - Generate GitHub links for current line/selection (`SPC g h`)
- **Buffer Management** - Toggle between last two buffers (`SPC b p`)
- **Window Management** - Toggle window fullscreen (`SPC w f`)
- **Compilation Shortcuts** - Quick build with unit tests (`SPC c u`)
- **Org Mode Integration** - Custom TODO management and agenda views

### Keybinding Philosophy
- Uses Evil mode (Vim-like navigation) with Emacs insert mode bindings
- Window navigation with `C-h/j/k/l`
- Leader key (`SPC`) for most custom commands
- Maintains Emacs keybindings in insert mode (`C-a`, `C-e`, etc.)

## Development Commands

### Doom Emacs Management
```bash
doom sync          # Synchronize package changes after modifying init.el or packages.el
doom upgrade        # Update Doom and packages
doom doctor         # Check for configuration issues
doom env            # Refresh environment variables
```

### Package Management
After modifying `packages.el`, always run:
```bash
doom sync
```
Then restart Emacs or run `M-x doom/reload`.

## Working with This Configuration

### Adding New Packages
1. Add package declaration to `packages.el`
2. Run `doom sync`
3. Add configuration in `config.el` or create a new custom module

### Custom Module Structure
Each module follows the pattern:
- `config.el` - Main configuration
- `packages.el` - Package declarations
- `autoload.el` - Custom functions (optional)
- `doctor.el` - Health check functions (optional)

### Important Paths
- Snippets directory: `~/.doom.d/snippets`
- Workspace directory: `~/workspace` (added to load-path)
- Org files: `~/org/` and `~/workspace/ChessCom/org/`

## Environment Variables
Set these environment variables for full functionality:
- `EMACS_FULL_NAME` - Your full name for templates
- `EMACS_EMAIL` - Your email address for templates

## Performance Optimizations
- Garbage collection tuned for interactive use (100MB threshold)
- Idle GC collection every 5 seconds
- Increased GC threshold during minibuffer operations
- Projectile uses 'alien indexing for large projects

DO NOT MAKE CHANGES TO CODE UNLESS EXPLICITLY ASKED OR SUGGESTED. If the prompt is a question or aside, do not come to own conclusions on changes to make -- answer question and, if you feel like it, suggest the change inferred while answering.
