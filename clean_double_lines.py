import re, sys, os
from pprint import pprint

REGEX_MATCH_UNTIL = r"(?:(?!X)[\w\W\s\S\d\D.])*"
START_CODE_BLOCK = "```lisp"
END_CODE_BLOCK = "```"

def process_path(given_path, given_function, filter_function=lambda x: True):
    if os.path.isfile(given_path) and filter_function(given_path):
        given_function(given_path)
    elif os.path.isdir(given_path):
        for root, directories, filenames in os.walk(given_path):
            for filename in filenames:
                if filter_function(filename):
                    filepath = os.path.join(root, filename)
                    given_function(filepath)

def replace_double_lines_in_code_blocks(filepath):
    text = get_file_text(filepath)
    code_blocks_regex = f'{START_CODE_BLOCK}{REGEX_MATCH_UNTIL.replace("X", END_CODE_BLOCK)}{END_CODE_BLOCK}'
    code_blocks_in_file = re.finditer(code_blocks_regex, text)
    new_text = ""
    start_index = 0
    for code_block in code_blocks_in_file:
        # (start,end) = (code_block.start() + len(START_CODE_BLOCK), code_block.end() - len(END_CODE_BLOCK))
        print(text[code_block.start():code_block.end()].replace("\n\n", "\n"))
        new_text += text[start_index:code_block.start()] + text[code_block.start():code_block.end()].replace("\n\n", "\n")
        start_index = code_block.end()
    new_text += text[start_index:]
    write_to_file(filepath, new_text)

def get_file_text(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    return text

def write_to_file(filepath, text):
    file = open(filepath, "w")
    file.write(text)
    file.close()


def replace_double_lines_in_code_blocks_in_dir(given_dir):
    process_path(given_dir, replace_double_lines_in_code_blocks, lambda x: x.endswith(".md"))

def main(args=[]):
    for arg in args:
        replace_double_lines_in_code_blocks_in_dir(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files containing lisp code blocks to indent")
        print("Note that the code blocks must be formatted as:\n\n```lisp\n\n...\n\n```\n\n")
    else:
        main(sys.argv[1:])
