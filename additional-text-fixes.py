import re, os, sys
from pprint import pprint

MD_DIR = "./output/"
# REGEXES_USED_BY_HAND_IN_EDITOR
r'([\w<>/\*\.,\-]+)(\s*\n+\s*)([\w<>/\*\.,\-]+)'
r'([\w,\. ]*)(\n+)( ?\*\w)'
r'([\w,\. ]*\*? ?)(\n+)( ?\*\w)'
r'([\(\)\w,\. ]*\*? ?)(\n+)( ?\*?[\w\(\)])'
r'([\(\)\w,\. "\']*\*? ?)(\n+)( ?\*?[\w\(\)"\'])'
r'([\(\)\w,\.:; "\']*\*? ?)(\n+)( ?\*?[\w:;\(\)"\'])'
r'([\(\)\w,\.:; "\']*\*?)( *\n+ *)(\*?[\w:;\(\)"\'])'
r''
r'(\w+)(\s*\n+\s*)(\w+)'
r'$1 $3'
r''
r''
r'<b>((?:(?!</b>).)*)</b>'
r'$1'
r''
r'(\*\*A\*\* )'
r'(\*\*[A-Z]\*\* )'


def process_all_md_files(given_dir, given_function):
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                given_function(filename, root)


def find_non_ascii_chars_in_dir(given_dir):
    found_non_ascii = []
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            find_non_ascii_characters(filename, root, found_non_ascii)
    pprint(found_non_ascii)


def find_non_ascii_characters(filename, root, found_non_ascii):
    filepath = os.path.join(root, filename)
    print(filepath)
    file = open(filepath, "r")
    file_text = file.read()
    file.close()
    for e in file_text:
        if re.sub(r'[ -~]', '', e) != "":
            if not e in found_non_ascii:
                found_non_ascii.append(e)


def remove_double_lines_from_code_blocks(filename, root):
    filepath = os.path.join(root, filename)
    file = open(filepath, "r")
    file_text = file.read()
    file.close()
    code_block_regex = r'```lisp[^```]+```'
    code_blocks_found = re.findall(code_block_regex, file_text)
    curr_text = file_text
    for code_block in code_blocks_found:
        new_code_block = code_block.replace("\n\n", "\n")
        curr_text = curr_text.replace(code_block, new_code_block)

    if curr_text != file_text:
        print(filepath)
        # import pdb; pdb.set_trace()
        file = open(filepath, "w")
        file.write(curr_text)
        file.close()

def fix_symbols_in_code_blocks(filename, root):
    # print("&lt;")
    # TODO see ▷
    # TODO see http://localhost:3000/cl-language-reference/docs/chap-5/f-b-generalized-reference#51121-examples-of-setf-expansions
    # The example named sections may have figures, each figure is a code block... should not be hard to parse...
    filepath = os.path.join(root, filename)
    file = open(filepath, "r")
    file_text = file.read()
    file.close()
    code_block_regex = r'```lisp[^```]+```'
    code_blocks_found = re.findall(code_block_regex, file_text)
    curr_text = file_text
    for code_block in code_blocks_found:
        new_code_block = code_block.replace("&lt;", "<")
        new_code_block = code_block.replace("&#126;", "~")
        new_code_block = new_code_block.replace("&gt;", ">")
        curr_text = curr_text.replace(code_block, new_code_block)

    if curr_text != file_text:
        print(filepath)
        # import pdb; pdb.set_trace()
        file = open(filepath, "w")
        file.write(curr_text)
        file.close()

def main(args=[]):
    for arg in args:
        process_all_md_files(arg, fix_symbols_in_code_blocks)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files containing lisp code blocks to fix")
        print("Note that the code blocks must be formatted as:\n\n```lisp\n\n...\n\n```\n\n")
    else:
        main(sys.argv[1:])
