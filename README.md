# easy-lang

A small Haskell CLI for translating UTF-8 text through OpenRouter. Each input sends one independent, non-streaming request. Translations go to stdout; errors and terminal progress go to stderr.

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
easy-lang --interactive
printf '%s\n' 'Please review this change.' | easy-lang
easy-lang --file notes.txt --to ja > translated.txt
easy-lang --pair en:zh 'The meeting starts at ten.'
easy-lang --help
```

Language pairs translate automatically in either direction. Use `--to` to choose a target explicitly or `--no-pairs` to use the configured target. Run `cabal build --offline` to build locally when dependencies are available.

## Interactive mode

Run `easy-lang --interactive` (or `easy-lang -i`) to keep entering text. Running `easy-lang` without text or a file also starts an interactive session when stdin is a terminal. Press Enter to translate the current line. Every nonblank line makes a separate request containing only that line, with no conversation history. Blank or whitespace-only lines are skipped. A failed request is reported on stderr and the session continues with the next input.

Press Ctrl+D at an empty prompt to end the session, or Ctrl+C to exit. There are no special text commands: words such as `exit` and `quit` are translated normally. Prompts and line editing appear on the terminal only when stdin and stderr are terminals, so redirected stdout contains only translations.

In a UTF-8 terminal locale, Haskeline handles Backspace, Delete, and arrow keys, including the display width of Chinese characters. Input history is not saved. Full line editing requires a UTF-8 locale, such as `LC_ALL=C.UTF-8`, and access to the terminal. Other environments use basic line input with explicit UTF-8 decoding.

Use `--interactive` with piped input to translate each line separately:

```bash
printf '%s\n' 'Good morning.' 'See you tomorrow.' | easy-lang -i
```

Without `--interactive`, piped stdin is translated as one input, including any line breaks. `--file PATH` and `--file -` also translate the entire input in one request. `--interactive` cannot be combined with positional text or `--file`. It is a CLI-only option; no configuration fields or environment variables are added.
