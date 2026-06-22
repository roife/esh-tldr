# esh-tldr

`esh-tldr` is an Emacs package for browsing local [tldr pages](https://tldr.sh/), copying examples, and inserting examples as editable templates.

It works well from regular buffers, `shell-mode`, `comint-mode`, and `eshell-mode`: run a command, ask for its tldr page, then copy or insert one of the examples.

## Requirements

- Emacs 27.1 or newer
- A local checkout/cache of tldr pages
- Optional: the `tldr` command-line client for `esh-tldr-update`
- Optional: Consult, Embark, and Tempel integrations

## Installation

Clone the repository somewhere on your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/esh-tldr")
(require 'esh-tldr)
```

If you use `use-package`:

```elisp
(use-package esh-tldr
  :load-path "/path/to/esh-tldr")
```

## Setup

`esh-tldr` reads local markdown files from `esh-tldr-pages-directory`. By default it uses:

```elisp
~/.tldrc/tldr
```

If your tldr pages live somewhere else:

```elisp
(setq esh-tldr-pages-directory "/path/to/tldr")
```

The expected layout is the standard tldr repository layout, for example:

```text
pages/common/tar.md
pages/linux/apt.md
pages/osx/pbcopy.md
pages.zh/common/tar.md
```

## Usage

Open a page by command name:

```elisp
M-x esh-tldr
```

Open the page for the command at point:

```elisp
M-x esh-tldr-at-point
```

Use the region, shell input, command at point, or prompt as a fallback:

```elisp
M-x esh-tldr-dwim
```

If Consult is installed, select an example directly:

```elisp
M-x consult-esh-tldr
```

Inside an `esh-tldr` buffer:

| Key | Action |
| --- | --- |
| `y` | Copy the current example command |
| `RET` or `e` | Insert the current example as a template in the source buffer |
| `n` | Move to the next example |
| `p` | Move to the previous example |
| `g` | Reload the current page |
| `q` | Quit the page buffer |

## Templates

tldr placeholders such as `{{source.tar}}` become editable template fields when inserted.

For example, this tldr command:

```text
tar xf {{source.tar}} -C {{directory}}
```

is inserted as a template where `source.tar` and `directory` can be filled interactively.

By default, `esh-tldr` uses Emacs' built-in Tempo. To use Tempel instead:

```elisp
(setq esh-tldr-use-tempel t)
```

## Completion At Point

To complete tldr examples at point in a buffer:

```elisp
M-x esh-tldr-capf-setup
```

or enable it from a mode hook:

```elisp
(add-hook 'eshell-mode-hook #'esh-tldr-capf-setup)
```

Then complete strings like:

```text
tar/
```

and choose an example to insert.

## Updating Pages

If `esh-tldr-executable` points to the `tldr` command-line client, you can update local pages with:

```elisp
M-x esh-tldr-update
```

## Customization

Useful options:

- `esh-tldr-pages-directory`: root directory of local tldr pages
- `esh-tldr-language`: language directory selection, or `auto`
- `esh-tldr-platform`: platform directory selection, or `auto`
- `esh-tldr-executable`: external `tldr` executable used for updates
- `esh-tldr-use-tempel`: use Tempel instead of Tempo for template insertion
