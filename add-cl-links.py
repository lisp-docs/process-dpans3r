# Needs python >= 3.9 
import os, re, sys, functools

MD_DIR = "./output/"
REGEX_MATCH_UNTIL = r"(?:(?!X)[\w\W\s\S\d\D.])*"
LOOK_AHEAD_REGEX_MATCH_OPEN = '(?:(?!{}){})*'
LOOK_AHEAD_REGEX = '(?:(?!{})[^\n])*'
UNTIL_NEW_LINE_REGEX = LOOK_AHEAD_REGEX.format("\n")
START_CODE_BLOCK = f"```lisp{UNTIL_NEW_LINE_REGEX}"
END_CODE_BLOCK = "```"
DICTIONARY_ITEM_NAME = LOOK_AHEAD_REGEX_MATCH_OPEN.format("\\*\\*", "\\S")
GLOSSARY_ITEM_NAME = LOOK_AHEAD_REGEX_MATCH_OPEN.format("\\*", "\\S")
DICTIONARY_ITEM_REGEX = f'\*\*(?P<item>{DICTIONARY_ITEM_NAME})\*\*'
GLOSSARY_ITEM_REGEX = f'\*(?P<item>{GLOSSARY_ITEM_NAME})\*'
TITLE_LINES_REGEX = r'\n#(?:(?!\n)[^\n])*'

def get_file_text(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    return text

def write_to_file(filepath, text):
    file = open(filepath, "w")
    file.write(text)
    file.close()

def process_all_md_files(given_dir):
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                process_file_without_codeblocks(filename, root)

def process_file_without_codeblocks(filename, root):
    filepath = os.path.join(root, filename)
    text = get_file_text(filepath)
    code_blocks_regex = f'(?P<code_block_start>{START_CODE_BLOCK}){REGEX_MATCH_UNTIL.replace("X", END_CODE_BLOCK)}{END_CODE_BLOCK}'
    code_blocks_in_file = re.finditer(code_blocks_regex, text)
    text_parts = []
    start_index = 0
    for code_block in code_blocks_in_file:
        groupdict = code_block.groupdict()
        code_block_start_string = groupdict["code_block_start"]
        (start_code,end_code) = (code_block.start() + len(code_block_start_string), code_block.end() - len(END_CODE_BLOCK))
        # Non code block text
        pre_code_block_text = text[start_index:start_code]
        text_parts.append({"match": None, "text": pre_code_block_text})
        # code block text
        code_block_contents = text[start_code:end_code]
        text_parts.append({"match": code_block, "text": code_block_contents})
        # to catch next non code block text
        start_index = end_code
    # From last code block until the end of the file
    text_parts.append({"match": None, "text": text[start_index:]})
    # here will need to do the html processing, then build a string and save to file
    for text_part in text_parts:
        if text_part["match"] == None:
            text_part["text"] = add_cl_links(text_part["text"])
    final_text = "".join([text_part["text"] for text_part in text_parts])
    write_to_file(filepath, final_text)

def add_cl_links(file_text):
    curr_text = replace_dictionary_links(file_text)
    curr_text = replace_glossary_links(curr_text)
    return curr_text

def replace_glossary_links(file_text):
    glossary_items = re.finditer(GLOSSARY_ITEM_REGEX, file_text)
    title_lines_matches_iter = re.finditer(TITLE_LINES_REGEX, file_text)
    title_lines_matches = [m for m in title_lines_matches_iter]
    all_items = [m for m in glossary_items]
    text_array = []
    start_index = 0
    for match in all_items:
        if len(title_lines_matches) > 0:
            in_titles = [match.start() > title.start() and match.start() < title.end() for title in title_lines_matches]
            in_title = functools.reduce(lambda x,y: x or y, in_titles)
        else:
            in_title = False
        item = match.group("item")
        if not in_title and len(item) <= 32 and len(item) > 0:
            extra_asterisk = "*" if len(item) > 0 and item[-1] == "\\" else ""
            cl_link = '<ClLinks styled={true}>'  + item + extra_asterisk + '</ClLinks>'
            text_array.append(file_text[start_index:match.start()])
            text_array.append(cl_link)
            start_index = match.end()
    text_array.append(file_text[start_index:])
    processed_text = "".join(text_array)
    return processed_text

def replace_dictionary_links(file_text):
    dictionary_items = re.finditer(DICTIONARY_ITEM_REGEX, file_text)
    title_lines_matches_iter = re.finditer(TITLE_LINES_REGEX, file_text)
    title_lines_matches = [m for m in title_lines_matches_iter]
    all_items = [m for m in dictionary_items]
    text_array = []
    start_index = 0
    for match in all_items:
        if len(title_lines_matches) > 0:
            in_titles = [match.start() > title.start() and match.start() < title.end() for title in title_lines_matches]
            in_title = functools.reduce(lambda x,y: x or y, in_titles)
        else:
            in_title = False
        item = match.group("item")
        if not in_title and len(item) <= 32 and len(item) > 0:
            extra_asterisk = "*" if len(item) > 0 and item[-1] == "\\" else ""
            cl_link = '<ClLinks styled={true}>'  + item + extra_asterisk + '</ClLinks>'
            text_array.append(file_text[start_index:match.start()])
            text_array.append(cl_link)
            start_index = match.end()
    text_array.append(file_text[start_index:])
    processed_text = "".join(text_array)
    return processed_text


def main(args=[MD_DIR]):
    for arg in args:
        process_all_md_files(arg)

if __name__ == "__main__":
    if not ((sys.version_info.major == 3 and sys.version_info.minor >= 9) or sys.version_info.major > 3):
        print("This script can only use Python Versions >= 3.9")
        print(f"Your current version is {sys.version}")
        print("Please get an appropiate version to run this script")
    else:
        if len(sys.argv) < 2:
            print("No directory with markdown files provided")
            print(f"Defaulting to run on {MD_DIR}")
            main()
        else:
            main(sys.argv[1:])
