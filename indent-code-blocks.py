import re, sys, os
from pprint import pprint
# indent-code-blocks.py

# emacs --batch MY_FILE --eval '(indent-region (point-min) (point-max))' -f 'save-buffer'
# emacs --batch output/chap-5/f-d-dictionary/_progn.md --eval '(indent-region 506 704)' -f 'save-buffer'
EMACS_COMMAND = "emacs --batch {} --eval '(indent-region {} {})' -f 'save-buffer'"
REGEX_MATCH_UNTIL = r"(?:(?!X)[\w\W\s\S\d\D.])*"
START_CODE_BLOCK = "```lisp"
END_CODE_BLOCK = "```"

def indent_code_blocks(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    code_blocks_regex = f'{START_CODE_BLOCK}{REGEX_MATCH_UNTIL.replace("X", END_CODE_BLOCK)}{END_CODE_BLOCK}'
    code_blocks_in_file = re.finditer(code_blocks_regex, text)
    # quantity_code_blocks_in_file = len(re.findall(code_blocks_regex, text))
    new_text = text
    # import pdb; pdb.set_trace()
    # print(filepath)
    for code_block in code_blocks_in_file:
        print(f"code block: {code_block.span()}")
        print(text[code_block.start():code_block.end()])
        (start,end) = (code_block.start() + len(START_CODE_BLOCK), code_block.end() - len(END_CODE_BLOCK))
        print(EMACS_COMMAND.format(filepath, start, end))

        print(text[code_block.start():code_block.end()].replace("\n\n", "\n"))
        # import pdb; pdb.set_trace()
        # return quantity_code_blocks_in_file
    # return quantity_code_blocks_in_file
    
    # if len(code_blocks_in_file) > 0:
    #   write_to_file(new_text)
def get_file_text(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    return text

def write_to_file(filepath, text):
    file = open(filepath, "w")
    file.write(text)
    file.close()

    
def indent_code_blocks_in_dir(given_dir):
    if os.path.isfile(given_dir):
        indent_code_blocks(given_dir)
    elif os.path.isdir(given_dir):
        for root, directories, filenames in os.walk(given_dir):
            for filename in filenames:
                if filename.endswith(".md"):
                    filepath = os.path.join(root, filename)
                    indent_code_blocks(filepath)
                    # if indent_code_blocks(filepath) > 0:
                    #     return True


def main(args=[]):
    for arg in args:
        indent_code_blocks_in_dir(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files containing lisp code blocks to indent")
        print("Note that the code blocks must be formatted as:\n\n```lisp\n\n...\n\n```\n\n")
    else:
        main(sys.argv[1:])
