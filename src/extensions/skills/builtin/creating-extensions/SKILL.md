---
name: creating-extensions
description: Author kli user extensions in Common Lisp - slash commands, event handlers, tools, and other live contributions loaded from ~/.config/kli/extensions/ with hot reload via /reload. Use when asked to create, modify, or debug a kli extension, add a custom command, react to kli events, or automate kli behavior.
allowed-tools: Bash(kli docs:*)
---

# Creating kli Extensions

An extension is a named bundle of contributions - slash commands, event
handlers, tools, and more - installed into a running kli session and
retractable at any time. Extensions load from files on disk and hot-reload
with `/reload`: no rebuild, no restart.

## Start here

This file is a discovery stub, not the usage guide. Before writing or editing
an extension, load the current API from the docs:

```bash
kli docs                                              # the kli docs index
kli docs extend/lisp-extensions/anatomy               # the shape of an extension
kli docs extend/lisp-extensions/contribution-kinds    # commands, event handlers, tools, ...
kli docs extend/lisp-extensions/write-your-first      # a minimal worked example
kli docs extend/lisp-extensions/examples              # fuller worked extensions
kli docs extend/lisp-extensions/recoding-live         # the live edit-and-reload loop
kli docs extend/lisp-extensions/loading-and-managing  # where files live, enable/disable, config
kli docs config/capabilities                          # the capability gates on dangerous operations
```

Find anything else by topic:

```bash
kli docs search <topic>                               # e.g. kli docs search event handler
```

The docs are fetched live and always describe the current extension API, so
these instructions never go stale. This stub carries no API of its own, which
is why it just points at `kli docs`.

A Lisp extension is the deepest tier. For lighter automation, first check
whether a skill or a prompt template fits the task:

```bash
kli docs extend/choosing-a-tier
```

## The dev loop

This loop does not change between releases:

1. Write the extension file into `~/.config/kli/extensions/` (or
   `<project>/.kli/extensions/` when it is specific to the project kli was
   launched in).
2. `/reload` re-indexes every user extension from disk and reinstalls the
   enabled set. Loading is fail-soft: a broken file is isolated with a warning
   while the rest keep working.
3. `/extensions` lists what is available and enabled; `/enable NAME` and
   `/disable NAME` toggle one live.
4. Exercise the new behavior. If a file failed to load, fix it and `/reload`
   again.

## Acting on "make me an extension that X"

1. Load the API: `kli docs extend/lisp-extensions/anatomy` and
   `kli docs extend/lisp-extensions/contribution-kinds`, adding
   `kli docs config/capabilities` when the extension touches anything gated.
2. Write the file into `~/.config/kli/extensions/`, or
   `<project>/.kli/extensions/` when it belongs to the current project.
3. Run `/reload` yourself if you can invoke commands, or ask the user to.
4. Confirm with `/extensions` that it shows as enabled, then exercise it.
