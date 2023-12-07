import re, os
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
        new_code_block = new_code_block.replace("&gt;", ">")
        curr_text = curr_text.replace(code_block, new_code_block)

    if curr_text != file_text:
        print(filepath)
        # import pdb; pdb.set_trace()
        file = open(filepath, "w")
        file.write(curr_text)
        file.close()

def split_glossary(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    section_matches = re.finditer(r'(\*\*([A-Z])\*\* )', text)
    sections = []
    curr_index = 0
    curr_name = filepath.split("/")[-1].removesuffix(".md")
    for curr_match in section_matches:
        sections.append((curr_name, text[curr_index:curr_match.start()]))
        curr_index = curr_match.start()
        curr_name = curr_match.groups()[-1].lower()
    sections.append((curr_name, text[curr_index:]))
    # import pdb; pdb.set_trace()
    for section in sections:
        base_dir = "/".join(filepath.split("/")[:-1])
        curr_path = os.path.join(base_dir, section[0]) + ".md"
        file = open(curr_path, "w")
        text = file.write(section[1])
        file.close()
    

def main():
    # process_all_md_files(MD_DIR, remove_double_lines_from_code_blocks)
    split_glossary("./output/chap-26/cg-b-glossary/_cg-b-glossary.md")

if __name__ == "__main__":
    main()