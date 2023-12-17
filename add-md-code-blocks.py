import os, sys, json, re
from pprint import pprint 

MD_DIR = "./output/"
MD_CODE_BEGIN = '```lisp'
MD_CODE_BLOCK_TITLE = '\n```lisp title="{}"\n{}\n```\n'
MD_CODE_END = '```'
LOOK_AHEAD_REGEX = r'(?:(?!X).)*'

def process_files_in_dir(given_dir):
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename != ".DS_Store":
                add_md_code_blocks(filename, root)

def apply_first_example_code_block(given_text):
    # There are markdown code blocks in the text already
    if given_text.find(MD_CODE_BEGIN) != -1:
        return given_text
    example_title = "**Examples:**"
    example_index = given_text.find(example_title)
    title_regex = r'(\*\*\w+(\w*\s*)*:\*\*\s*?\n)'
    # No examples section
    if example_index == -1:
        return given_text
    
    # figure_regex = r'\|(?:(?!\|)[^\|])*\|[\s\n]*\|\s*:-\s*\|[\s\n]+\*\*Figure \d+(–\d+)*\.[\w\s]*\*\*'
    figure_regex = r'\|(?P<lisp_code>(?:(?!\|)[^\|])*)\|[\s\n]*\|\s*:-\s*\|[\s\n]+\*\*(?P<figure_name>Figure \d+(–\d+)*\.[\w\s]*)\*\*'
    
    # case where there are Figure examples
    matches = [match for match in re.finditer(figure_regex, given_text)]
    if len(matches) > 0:
        new_text = ""
        prev_end = 0
        for match in matches:
            (start, end) = match.span()
            title = match.groupdict()["figure_name"]
            lisp_code = match.groupdict()["lisp_code"]
            new_text += given_text[prev_end: start]
            new_text += MD_CODE_BLOCK_TITLE.format(title, lisp_code)
            prev_end = end
        new_text += given_text[prev_end:]
        return new_text
    
    # Regular case
    post_example_index = example_index + len(example_title)
    next_title_match = re.search(title_regex, given_text[post_example_index:])
    pre_example_text = given_text[:post_example_index]
    if next_title_match:
        next_title_index = next_title_match.span()[0] + post_example_index
        post_example_text = given_text[next_title_index:]
        example_text = given_text[post_example_index:next_title_index]
        return f'{pre_example_text}\n{MD_CODE_BEGIN}\n{example_text}\n{MD_CODE_END}\n{post_example_text}'
    else:
        example_text = given_text[post_example_index:]
        return f'{pre_example_text}\n{MD_CODE_BEGIN}\n{example_text}\n{MD_CODE_END}\n'

def conditionally_add_filename_codeblock(filename, root):
    if "example" in filename.lower():
        filepath = os.path.join(root, filename)
        file = open(filepath, "r")
        text = file.read()
        file.close()
        if text.find(MD_CODE_BEGIN) == -1:
            code_block_wrapped_text = f'{MD_CODE_BEGIN}\n{text}\n{MD_CODE_END}'
            file = open(filepath, "w")
            file.write(code_block_wrapped_text)
            file.close()
            return True
    return False

def conditionally_add_section_codeblock(filename, root):
    filepath = os.path.join(root, filename)
    print(filepath)
    file = open(filepath, "r")
    given_text = file.read()
    file.close()
    example_title = "**Examples:**"
    example_title_regex = f"({re.escape(example_title)})"
    all_indices = [m.span()[0] for m in re.finditer(example_title_regex, given_text)]
    if len(all_indices) == 0:
        return False
    start = 0
    text_parts = []
    for i in all_indices:
        text_parts.append(given_text[start: i])
        start = i
    text_parts.append(given_text[start:])
    text_w_code_blocks = "".join([apply_first_example_code_block(text_part) for text_part in text_parts])
    file = open(filepath, "w")
    file.write(text_w_code_blocks)
    file.close()

def add_md_code_blocks(filename, root):
    # given_text
    conditionally_add_filename_codeblock(filename, root)
    conditionally_add_section_codeblock(filename, root)


def main(args=[MD_DIR]):
    for dir in args:
        process_files_in_dir(dir)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        main()
    else:
        main(sys.argv[1:])
