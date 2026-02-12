---
description: Commit changes with auto-generated conventional commit messages
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(git add:*), Bash(git commit:*), Bash(git rev-parse:*), Bash(git ls-files:*), Bash(git branch:*), mcp__task__task_get, mcp__task__timeline, mcp__task__task_graph, mcp__task__task_set_current, mcp__task__task_bootstrap, mcp__task__observe
---

# Commit Changes

Intelligently commit changes with auto-generated conventional commit messages. Integrates with task MCP for context-aware commits and reflection nudging.

## Context

Git state gathered automatically before execution:

- Current branch: !`git branch --show-current`
- Git status: !`git status --porcelain | head -30`
- Staged changes: !`git diff --cached --stat | tail -20`
- Unstaged changes: !`git diff --stat | tail -20`
- Recent commits: !`git log --oneline -5`

## Usage

```
/commit <task-name>           # Commit task artifacts + implementation files
/commit <scope>               # Commit changes related to scope
/commit <file1> <file2> ...   # Commit specific files only
/commit                       # Analyze all changes and infer scope
```

## Process

### Step 1: Parse Arguments and Detect Mode

Determine intent from arguments:

| Pattern | Mode | Behavior |
|---------|------|----------|
| No args | Full analysis | Analyze all changes, infer scope |
| `YYYY-MM-DD-*` | Task | Commit `ace/tasks/<name>/` + implementation files from plan |
| Existing file paths | Specific files | Commit only those files |
| Single word | Scope filter | Commit files matching scope pattern |

### Step 2: Validate Git State

Before proceeding, verify:
1. We're in a git repository (`git rev-parse --git-dir`)
2. No merge conflicts exist (`git diff --name-only --diff-filter=U`)
3. Working tree has changes (staged, unstaged, or untracked)

If any check fails, report the error and stop.

### Step 3: Task Context Detection

Check for active task context to enrich the commit:

1. **If task name provided** (YYYY-MM-DD pattern): call `task_bootstrap(task_id)`
2. **Otherwise**: call `task_get()` — if a current task is already set, use it
3. **No task**: proceed without task context (purely git-based commit)

When a task is active:
- Use the task description to inform commit message generation
- Check `task_query("(query \"plan\")")` for phase completion status
- If all plan phases are done and committing task artifacts, note this is a final commit

### Step 4: Discover and Filter Changes

1. Run `git status --porcelain` to discover all changes
2. Filter based on mode:
   - **Task mode**: Include files under `ace/tasks/<task>/` plus files referenced in plan.md
   - **File mode**: Include only specified files (verify they exist and have changes)
   - **Scope mode**: Include files whose paths match the scope keyword
   - **Full mode**: Include all changed files
3. Get diffs for filtered files (`git diff` or `git diff --cached`)
4. Present summary: file count, insertions/deletions, change types

### Step 5: Generate Commit Message

**Conventional commit format**: `type(scope): subject`

**Type inference** (priority order):
- `ace/tasks/*/research.md`, `*/plan.md`, `*/reflection.md` → `docs`
- `tests/`, `*_test.*`, `*Test.*` → `test`
- `*.nix`, `nixos/*` → scope=nix
- `ops/*` → scope=ops
- New functions/classes/features → `feat`
- Error handling / corrections → `fix`
- Code restructuring → `refactor`
- Formatting only → `style`
- Deps, config, tooling → `chore`

**Scope inference** from file paths:
- `ace/*`, `.claude/*` → `ace`
- `ops/mcp/*` → `mcp`
- `shells/*` → `shells` or more specific
- Multiple scopes → most common one

**Subject line**: Imperative mood, max 72 chars total. Use task description to inform subject when task context is available.

Present the generated message and inference details to the user.

### Step 6: Confirm and Execute

Present the commit plan:
```
Commit Message: <type(scope): subject>
Files: <N files, +X/-Y lines>
```

Options:
1. Commit with this message
2. Edit commit message
3. Show detailed diff
4. Cancel

On confirmation:
- Stage filtered files with `git add`
- Commit using heredoc pattern:
  ```bash
  git commit -m "$(cat <<'EOF'
  <commit message>
  EOF
  )"
  ```
- Verify success, show `git log -1 --stat`

### Step 7: Post-Commit — Task Integration

**If a task is active**, record the commit in the task event stream:

```
observe("Committed: <short-hash> — <commit-message> (<N> files, +X/-Y)")
```

Then evaluate the **Reflect Decision Matrix**:

#### Reflect Decision Matrix

Gather signals:
1. `task_get()` → observation count from task state
2. `timeline(limit=30)` → check for existing `:handoff.create` or reflection artifacts
3. `task_graph(query="plan")` → plan completion status (N/M phases done)

| Observation Count | Plan Status | Already Reflected? | Action |
|---|---|---|---|
| No task set | — | — | **SKIP** silently |
| < 5 | Any | — | **SKIP** — insufficient evidence |
| 5-9 | Incomplete | No | **SUGGEST** — "Consider `/ace:reflect` when task completes" |
| 5-9 | All phases done | No | **RECOMMEND** — "Task complete with observations. Run `/ace:reflect`?" |
| 10+ | Any | No | **STRONGLY RECOMMEND** — "Task has rich evidence (N observations). Run `/ace:reflect` to extract learnings?" |
| Any | Any | Yes | **SKIP** — "Reflection already recorded" |

**Detection of prior reflection**: Search timeline events for artifacts containing `reflection.md` or observation text matching "Reflection complete".

**If no task context**: skip this step entirely (no output).

## Commit Message Templates

**ACE task commits**:
```
feat(ace): <action from task description>
docs(ace): <research/plan/reflection for task>
```

**Infrastructure**:
```
feat(nix): <what changed>
fix(nixos): <what was fixed>
ops(mcp): <what was updated>
```

**General**:
```
feat: <new functionality>
fix: <bug fix>
docs: <documentation>
refactor: <restructuring>
chore: <maintenance>
```

## Error Handling

| Error | Response |
|-------|----------|
| Not a git repo | Report error, stop |
| Merge conflicts | List conflicted files, stop |
| No changes | Report clean tree, stop |
| Task dir not found | List available tasks, stop |
| File not found | List which files exist/don't, stop |
| Commit failed (hooks) | Show output, suggest fixing and retrying |

## Guidelines

- Always present the file list and commit message for review before executing
- Follow conventional commit format: `type(scope): subject` (max 72 chars)
- Use imperative mood in subject ("add" not "added")
- Never commit files that likely contain secrets (.env, credentials, keys)
- Task-based commits group related changes logically
- The reflect nudge is informational — the user decides whether to run it
