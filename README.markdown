# process-dpans3r

This project is for processing the files from <https://github.com/lisp-docs/cl-standard> into broken down pieces of markdown to be used by <https://github.com/lisp-docs/cl-language-reference>

## Usage

1. First Step is to esecute `run-project` in [main.lisp](/src/main.lisp)
2. Then execute the python scripts to:
   - fix all html tags by running [fix-html-tags.py](/fix-html-tags.py)
   - additional text fixes running [additional-text-fixes.py](/additional-text-fixes.py)
   - code blocks
   - split the dictionary files for files not already split with code blocks [split-dictionary.py](/split-dictionary.py)
   - remove double lines in markdown code blocks: [clean_double_lines.py](/clean_double_lines.py)
   - indent code blocks [indent-code-blocks.py](/indent-code-blocks.py)
   - make the glossary [make-glossary.py](/make-glossary.py)
   - make dictionary json [make-spec-dictionary-json.py](/make-spec-dictionary-json.py)

## Installation
