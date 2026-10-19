# Repository Guidelines

## Structure and Behavior

- `app/Main.hs` handles CLI input, output, and credentials.
- `src/EasyLang/` contains configuration, options, HTTP requests, language pairs, reasoning, response parsing, and terminal progress.
- `config.example.toml` documents configuration; `easy-lang.cabal` declares components and dependencies.
- Local tests, fixtures, translation-check scripts, and reports are excluded from version control and package sources.

Preserve one independent, non-streaming OpenRouter request per input, with no conversation history. Interactive mode accepts one input per nonblank line; other input modes send one request per invocation. Language detection and bidirectional routing belong in the LLM prompt; local code validates pair conflicts. Keep translations on stdout and errors/progress on stderr, with animation limited to terminals.

## Development

Use GHC 9.2+ and Cabal 3.6+:

```bash
cabal build
cabal run easy-lang -- 'Hello, world!'
cabal install exe:easy-lang --installdir="$HOME/.local/bin" --overwrite-policy=always
```

Add `--offline` when dependencies are available. Verify changes with a build; live translation checks require configuration and API credentials and consume credits.

Write all documentation, comments, prompts, and application messages in English, including README.md and AGENTS.md. Follow Haskell2010 with two-space indentation, explicit top-level type signatures, `PascalCase` modules/types, and `lowerCamelCase` functions/fields. Use `Text` and explicit UTF-8 handling for user text. Builds enable `-Wall`; no formatter or separate linter is configured.

## Configuration and Security

Read all eight required fields from `~/.easy-lang/config.toml`; do not introduce built-in runtime defaults or automatic configuration writeback. Preserve precedence: CLI > `EASY_LANG_*` environment > TOML. Initialize from `config.example.toml` without overwriting existing configuration. Keep option changes synchronized with the example and README.

Supply credentials through `OPENROUTER_API_KEY`. Never commit tokens or log authorization headers or credential-bearing HTTP exceptions.

## Commits and Pull Requests

Use concise imperative commit subjects and keep changes focused. Describe behavior changes, verification results, and configuration impacts in pull requests.
