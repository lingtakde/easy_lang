# easy-lang

A small Haskell CLI for translating UTF-8 text through OpenRouter. Each invocation sends one independent, non-streaming request. Translations go to stdout; errors and terminal progress go to stderr.

## Setup

Requires GHC 9.2+ and Cabal 3.6+.

```bash
cabal update
cabal install exe:easy-lang --installdir="$HOME/.local/bin" --overwrite-policy=always
mkdir -p "$HOME/.easy-lang"
cp -n config.example.toml "$HOME/.easy-lang/config.toml"
export OPENROUTER_API_KEY="your-api-key"
```

Add `$HOME/.local/bin` to your `PATH`. Edit `~/.easy-lang/config.toml` to choose your model and languages. All eight fields in [config.example.toml](config.example.toml) are required. Settings use this precedence: CLI > `EASY_LANG_*` environment variables > config file. Overrides apply only to the current invocation.

## Usage

```bash
easy-lang 'Keep it simple.'
printf '%s\n' 'Please review this change.' | easy-lang
easy-lang --file notes.txt --to ja > translated.txt
easy-lang --pair en:zh 'The meeting starts at ten.'
easy-lang --help
```

Language pairs translate automatically in either direction. Use `--to` to choose a target explicitly or `--no-pairs` to use the configured target. Run `cabal build --offline` to build locally when dependencies are available.
