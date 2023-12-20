import os, sys, json, re
from pprint import pprint 
import importlib
add_code_blocks_module = importlib.import_module("add-md-code-blocks")
add_md_code_blocks_module = importlib.import_module("add-md-code-blocks")
text_fixes_for_code_blocks_module = importlib.import_module("text-fixes-for-code-blocks")
clean_double_lines_module = importlib.import_module("clean_double_lines")
indent_code_blocks_module = importlib.import_module("indent-code-blocks")
clear_footers_module = importlib.import_module("clear_footers")
make_dictionary_module = importlib.import_module("make-dictionary")
split_glossary_module = importlib.import_module("split-glossary")
fix_html_tags_module = importlib.import_module("fix-html-tags")
fix_glossary_module = importlib.import_module("fix-glossary")
make_glossary_module = importlib.import_module("make-glossary")
make_spec_dictionary_json_module = importlib.import_module("make-spec-dictionary-json")

MD_DIR = "./output/"


def process_dpans3r_lisp_output(dir_path):
    print("For slower running scripts a dot \".\" will be printed for each processed file to show progress.")
    print("The following steps are being taken:")
    # 2. Add markdown code blocks with [add-md-code-blocks.py](/add-md-code-blocks.py)
    print("\t- Add markdown code blocks with [add-md-code-blocks.py](/add-md-code-blocks.py)")
    add_md_code_blocks_module.main([dir_path])
    # 3. Additional text fixes running [text-fixes-for-code-blocks.py](/text-fixes-for-code-blocks.py)
    print("\t- Additional text fixes running [text-fixes-for-code-blocks.py](/text-fixes-for-code-blocks.py)")
    text_fixes_for_code_blocks_module.main([dir_path])
    # 4. Remove double lines in markdown code blocks: [clean_double_lines.py](/clean_double_lines.py)
    print("\t- Remove double lines in markdown code blocks: [clean_double_lines.py](/clean_double_lines.py)")
    clean_double_lines_module.main([dir_path])
    # 5. Indent code blocks [indent-code-blocks.py](/indent-code-blocks.py)
    print("\t- Indent code blocks [indent-code-blocks.py](/indent-code-blocks.py)")
    indent_code_blocks_module.main([dir_path])
    # 6. Clear footers by running [clear_footers.py](/clear_footers.py)
    print("\t- Clear footers by running [clear_footers.py](/clear_footers.py)")
    clear_footers_module.main([dir_path])
    # 7. Split the dictionary files [make-dictionary.py](/make-dictionary.py)
    print("\t- Split the dictionary files [make-dictionary.py](/make-dictionary.py)")
    make_dictionary_module.main([dir_path])
    # 8. Split Glossary [split-glossary.py](/split-glossary.py)
    print("\t- Split Glossary [split-glossary.py](/split-glossary.py)")
    # TODO right now the path in output is hard coded! Maybe change this??
    # If changed, would need to manually provide path...
    split_glossary_module.main() # [dir_path]
    # 9. Fix all html tags by running [fix-html-tags.py](/fix-html-tags.py)
    print("\t- Fix all html tags by running [fix-html-tags.py](/fix-html-tags.py)")
    fix_html_tags_module.main([dir_path])
    print("\t- Fix glossary [fix-glossary.py](/fix-glossary.py)")
    fix_glossary_module.main([dir_path])
    # 10. Make the glossary [make-glossary.py](/make-glossary.py)
    print("\t- Make the glossary [make-glossary.py](/make-glossary.py)")
    make_glossary_module.main([f"{dir_path}/chap-26/"])
    # 11. Make dictionary json [make-spec-dictionary-json.py](/make-spec-dictionary-json.py)
    print("\t- Make dictionary json [make-spec-dictionary-json.py](/make-spec-dictionary-json.py)")
    make_spec_dictionary_json_module.main([dir_path])
    print(f"Finished. Please check out the in-place changes that happened in {dir_path}")

def main(args=[MD_DIR]):
    for arg in args:
        process_dpans3r_lisp_output(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        # print("Please provide a directory with markdown files")
        print("No directory with markdown files provided")
        print(f"Defaulting to run on {MD_DIR}")
        main()
    else:
        main(sys.argv[1:])
