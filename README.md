# esh-tldr

`esh-tldr` is an Emacs 31 package for browsing local [tldr pages](https://tldr.sh/) and inserting their examples as editable command templates.

It understands regular buffers, `shell-mode`, `comint-mode`, and `eshell-mode`. Optional Ghostty support through [Ghostel](https://github.com/dakra/ghostel) lives in `esh-tldr-ghostty.el`. The package never executes an example command.

## Requirements

- Emacs 31 or newer
- A local checkout or cache of tldr pages
- Optional: the `tldr` command-line client for updating pages
- Optional: Tempel as an alternative to the built-in Tempo template engine
- Optional: Ghostel for native terminal integration

The package uses Emacs' standard completion protocol. It does not depend on Consult, Vertico, Orderless, Marginalia, Embark, or a particular completion UI, but works with completion frontends configured by the user.

## Installation

Clone the repository somewhere on your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/esh-tldr")
(require 'esh-tldr)
(require 'esh-tldr-ghostty) ; Optional Ghostty/Ghostel integration
(global-set-key (kbd "C-h t") #'esh-tldr-dwim)
```

With `use-package`:

```elisp
(use-package esh-tldr
  :load-path "/path/to/esh-tldr"
  :bind ("C-h t" . esh-tldr-dwim))

(use-package esh-tldr-ghostty
  :load-path "/path/to/esh-tldr"
  :after esh-tldr)
```

## Page setup

By default, pages are read from `~/.tldrc/tldr` using the standard repository layout:

```text
pages/common/tar.md
pages/linux/apt.md
pages/osx/pbcopy.md
pages.zh/common/tar.md
```

Configure a different location when needed:

```elisp
(setq esh-tldr-pages-directory "/path/to/tldr")
```

Language and platform directories are selected automatically, with the base English and `common` pages used as fallbacks.

## Usage

Use the context-aware entry point in most situations:

```text
M-x esh-tldr-dwim
```

It checks an active region, the current terminal input, and the command at point. If no command can be inferred, it opens the standard Emacs command completion UI.

To always search for a command first:

```text
M-x esh-tldr
```

Selecting a command opens a Help-style page. Missing commands open an empty-state page instead of failing or starting a network update.

### Page actions

| Key | Action |
| --- | --- |
| `RET` | Insert or replace with the selected example |
| `w` or `y` | Copy the selected example |
| `TAB` / `S-TAB` | Move between clickable actions |
| `n` / `p` | Move between examples, wrapping at the ends |
| `s` | Search for another command |
| `g` | Reload the current page |
| `q` | Close the page |

Commands and the visible `[Insert/replace]` and `[Copy]` controls can also be clicked.

## Safe replacement

The page records the source text it intends to replace. Existing commands are replaced rather than duplicated:

```text
ls          + choose "ls -a"  => ls -a
ls -l       + choose "ls -a"  => ls -a -l
echo ok | ls -l               => echo ok | ls -a -l
FOO=1 ls -l                    => FOO=1 ls -a -l
```

Only the detected command token is replaced. Existing arguments, environment assignments, pipelines, redirections, comments, and neighboring commands remain untouched. An active region limits command detection but does not cause its arguments to be deleted. In ordinary buffers, the command word at point is replaced. Without source context, the template is inserted at point.

Complex or incomplete shell syntax such as command substitutions, backticks, process substitutions, and unbalanced quotes is handled conservatively: DWIM declines automatic replacement and falls back to command search.

If the source buffer becomes read-only, disappears, or changes while the page is open, the example is copied instead of overwriting newer text. Template insertion is atomic, so a Tempo or Tempel failure restores the original input.

Placeholders such as `{{source.tar}}` become editable fields. Repeated placeholders share the same value.

To use Tempel instead of Tempo:

```elisp
(setq esh-tldr-use-tempel t)
```

## Ghostty through Ghostel

Ghostty support is isolated in `esh-tldr-ghostty.el` and is not loaded by the core package. Install Ghostel before enabling the adapter explicitly:

```elisp
(require 'esh-tldr-ghostty)
```

The adapter registers itself when loaded and can be disabled again with `M-x esh-tldr-ghostty-teardown`.

- In Ghostel `line` mode, `esh-tldr-dwim` reads and replaces the editable input directly.
- In the default `semi-char` mode, opening a TL;DR page does not change modes. Pressing `RET` on an example switches the source terminal to `ghostel-line-mode`, adopts the current readline input, verifies that it has not changed, and then replaces only the selected command token.
- Ghostel remains in line mode so Tempo or Tempel fields can be edited. Press `RET` in Ghostel to submit the finished command, or `C-c C-j` to return to semi-char mode.
- In char, copy, or Emacs mode, inside a TUI, without a usable prompt, or when the input changed, the example is copied instead. `esh-tldr` never simulates `C-u`, backspaces, or other destructive terminal keys.

Ghostel's default semi-char exceptions already allow the global `C-h t` binding to reach Emacs. If those exceptions were customized, ensure `C-h` remains in `ghostel-keymap-exceptions`.

## Updating pages

When `esh-tldr-executable` names an installed tldr client, update pages explicitly with:

```text
M-x esh-tldr-update
```

The update runs asynchronously. Open TL;DR pages are reloaded after a successful update.

## Customization

- `esh-tldr-pages-directory`: root directory of local pages
- `esh-tldr-language`: language selection or `auto`
- `esh-tldr-platform`: platform selection or `auto`
- `esh-tldr-executable`: executable used for explicit updates
- `esh-tldr-use-tempel`: use Tempel instead of Tempo
