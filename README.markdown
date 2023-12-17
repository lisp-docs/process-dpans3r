# process-dpans3r

This project is for processing the files from <https://github.com/lisp-docs/cl-standard> into broken down pieces of markdown to be used by <https://github.com/lisp-docs/cl-language-reference>

## Usage

1. First Step is to esecute `run-project` in [main.lisp](/src/main.lisp)
2. The following scripts can only be executed in this project's `output` folder:
   - fix all html tags by running [fix-html-tags.py](/fix-html-tags.py) 
   - [split-glossary.py](/split-glossary.py)
3. Then execute the python scripts to:
   - additional text fixes running [additional-text-fixes.py](/additional-text-fixes.py)
   - clear footers by running [clear_footers.py](/clear_footers.py)
   - add markdown code blocks with [add-md-code-blocks.py](/add-md-code-blocks.py)
   - split the dictionary files for files not already split with code blocks [split-dictionary.py](/split-dictionary.py)
     - Note that the above file is incomplete, the functionality has to be merged with code-blocks to do the proper split...
   - remove double lines in markdown code blocks: [clean_double_lines.py](/clean_double_lines.py)
   - indent code blocks [indent-code-blocks.py](/indent-code-blocks.py)
   - make the glossary [make-glossary.py](/make-glossary.py)
   - make dictionary json [make-spec-dictionary-json.py](/make-spec-dictionary-json.py)

## TODO

1. Test all the scripts
2. Produce new json files for glossary and dictionary (glossary, convert to html from markdown?)
3. Fix the `split-dictionary.py` and `code-blocks.py` to have the functionality in one file (`split-dictionary.py`) and make it work everywhere even if directory seems to be working properly. Add check for dictionary directory...
