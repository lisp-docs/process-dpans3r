# process-dpans3r

This project is for processing the files from <https://github.com/lisp-docs/cl-standard> into broken down pieces of markdown to be used by <https://github.com/lisp-docs/cl-language-reference>

## Usage

1. First Step is to execute `(run-project)` in [main.lisp](/src/main.lisp)
2. Run any of the following scripts:
   - The following scripts can only be executed in this project's `output` folder:
      - [split-glossary.py](/split-glossary.py)
        - Note that the glossary file path is hard coded.
   - The following python scripts can be executed with a given directory:
      - fix all html tags by running [fix-html-tags.py](/fix-html-tags.py)
        - Note: This should be executed after making adding code blocks
      - additional text fixes running [text-fixes-for-code-blocks.py](/text-fixes-for-code-blocks.py)
      - clear footers by running [clear_footers.py](/clear_footers.py)
      - add markdown code blocks with [add-md-code-blocks.py](/add-md-code-blocks.py)
      - split the dictionary files [make-dictionary.py](/make-dictionary.py)
      - remove double lines in markdown code blocks: [clean_double_lines.py](/clean_double_lines.py)
      - indent code blocks [indent-code-blocks.py](/indent-code-blocks.py)
      - fix the glossary [fix-glossary.py](/fix-glossary.py)
      - make the glossary [make-glossary.py](/make-glossary.py)
      - make dictionary json [make-spec-dictionary-json.py](/make-spec-dictionary-json.py)

To get the same results as in <https://lisp-docs.github.io/cl-language-reference/> run the scripts in the following order:

1. Execute `(run-project)` in [main.lisp](/src/main.lisp)
2. Add markdown code blocks with [add-md-code-blocks.py](/add-md-code-blocks.py)
3. Additional text fixes running [text-fixes-for-code-blocks.py](/text-fixes-for-code-blocks.py)
4. Remove double lines in markdown code blocks: [clean_double_lines.py](/clean_double_lines.py)
5. Indent code blocks [indent-code-blocks.py](/indent-code-blocks.py)
6. Clear footers by running [clear_footers.py](/clear_footers.py)
7. Split the dictionary files [make-dictionary.py](/make-dictionary.py)
8. Split Glossary [split-glossary.py](/split-glossary.py)
9. Fix all html tags by running [fix-html-tags.py](/fix-html-tags.py)
10. Make the glossary [make-glossary.py](/make-glossary.py)
11. Make dictionary json [make-spec-dictionary-json.py](/make-spec-dictionary-json.py)

## TODO

1. Test all the scripts
2. Produce new json files for glossary and dictionary (glossary, convert to html from markdown?)
3. Do Tables Fix
4. In Code Blocks that have `<p></p>` tags, replace them with new lines...
