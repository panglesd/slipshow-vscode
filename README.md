# slipshow README

This is a VS Code extension to write, preview and compile [slipshow](https://github.com/panglesd/slipshow/) presentations.

## Features

Whenever you open a `.slp` file, the extension will:

- Syntax-highlight the file as a markdown file,
- Start a preview server, accessible at `https://localhost:8080` (or a higher port if this one is already used)
- Refresh the preview, depending on the "Refresh on" setting, on each key press or on save.
- Compile the input in a `.html` file.

In addition, it features three commands

- `slipshow.next` and `slipshow.previous`, to control the preview's state from the editor,
- `slipshow.startServer`, which starts the preview server without requiring to open a `.slp` file.

## Installation

This requires an (unreleased) version of slipshow, and is unreleased. You can still install the previous version from a VSCode marketplace (the official one, or open-vsx).
