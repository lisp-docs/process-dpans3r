import re, sys, os
from pprint import pprint
# indent-code-blocks.py

# emacs --batch MY_FILE --eval '(indent-region (point-min) (point-max))' -f 'save-buffer'

REGEX_MATCH_UNTIL = r"(?:(?!X).)*"

def indent_code_blocks(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    code_blocks_regex = f'```lisp{REGEX_MATCH_UNTIL.replace("X", "```")}```'
    code_blocks_in_file = re.finditer(code_blocks_regex, text)
    new_text = text
    for code_block in code_blocks_in_file:
        print("")
    
    if len(code_blocks_in_file) > 0:
        file = open(filepath, "w")
        file.write(new_text)
        file.close()

def indent_code_blocks_in_dir(given_dir):
    for root, directories, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                filepath = os.path.join(root, filename)
                indent_code_blocks(filepath)


def main(args=[]):
    for arg in args:
        indent_code_blocks_in_dir(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files containing lisp code blocks to indent")
        print("Note that the code blocks must be formatted as:\n\n```lisp\n\n...\n\n```\n\n")
    else:
        main(sys.argv[1:])
