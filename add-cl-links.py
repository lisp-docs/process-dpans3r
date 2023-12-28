# Needs python >= 3.9 
import os, re, sys, functools, json
from pprint import pprint

MD_DIR = "./output/"
REGEX_MATCH_UNTIL = r"(?:(?!X)[\w\W\s\S\d\D.])*"
LOOK_AHEAD_REGEX_MATCH_OPEN = '(?:(?!{}){})*'
LOOK_AHEAD_REGEX = '(?:(?!{})[^\n])*'
UNTIL_NEW_LINE_REGEX = LOOK_AHEAD_REGEX.format("\n")
START_CODE_BLOCK = f"```lisp{UNTIL_NEW_LINE_REGEX}"
END_CODE_BLOCK = "```"
DICTIONARY_ITEM_NAME = LOOK_AHEAD_REGEX_MATCH_OPEN.format("\\*\\*", "\\S")
GLOSSARY_ITEM_NAME = LOOK_AHEAD_REGEX_MATCH_OPEN.format("\\*", "\\S")
DICTIONARY_ITEM_NAME_PERMISSIVE = LOOK_AHEAD_REGEX_MATCH_OPEN.format("\\*\\*", "[^\n]")
GLOSSARY_ITEM_NAME_PERMISSIVE = LOOK_AHEAD_REGEX_MATCH_OPEN.format("\\*", "[^\n]")
DICTIONARY_ITEM_REGEX = f'(?P<pre>[^\\\\])(\\*\\*)(?P<item>{DICTIONARY_ITEM_NAME}[^\\\\])(?P<post>\\*\\*)'
GLOSSARY_ITEM_REGEX = f'(?P<pre>[^\\*\\\\])(\\*)(?P<item>{GLOSSARY_ITEM_NAME}[^\\*\\\\])(?P<post>\\*)'
DICTIONARY_ITEM_REGEX_PERMISSIVE = f'(?P<pre>[^\\\\])(\\*\\*)(?P<item>{DICTIONARY_ITEM_NAME_PERMISSIVE}[^\\\\])(?P<post>\\*\\*)'
GLOSSARY_ITEM_REGEX_PERMISSIVE = f'(?P<pre>[^\\*\\\\])(\\*)(?P<item>{GLOSSARY_ITEM_NAME_PERMISSIVE}[^\\*\\\\])(?P<post>\\*)'
CLLINK_DICTIONARY_ITEM_REGEX = r'(<ClLinks( styled=\{\w*\}\s*)?(?P<item>\s+term=\{"[⟨⟩`~:;?/,\.\w\d!@$#$%^&*\(\)_=+\\\*\{\}\-]+"\}><b>[⟨⟩`~:;?/,\.\w\d!@$#$%^&*\(\)_=+\\\*\{\}\- ]+</b></)ClLinks>)'
CLLINK_GLOSSARY_ITEM_REGEX = r'(<ClLinks( styled=\{\w*\}\s*)?(?P<item>\s+term=\{"[⟨⟩`~:;?/,\.\w\d!@$#$%^&*\(\)_=+\\\*\{\}\-]+"\}><i>[⟨⟩`~:;?/,\.\w\d!@$#$%^&*\(\)_=+\\\*\{\}\- ]+</i></)ClLinks>)'
TITLE_LINES_REGEX = r'\n#(?:(?!\n)[^\n])*'
dictionary_json_path = "./glossary_output/dictionary.json"
glossary_json_path = "./glossary_output/glossary.json"

file = open(dictionary_json_path, "r")
dictionary_json = json.load(file)
file.close()

file = open(glossary_json_path, "r")
glossary_json = json.load(file)
file.close()

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
    # curr_text = file_text
    curr_text = replace_dictionary_links(file_text)
    curr_text = replace_glossary_links(curr_text)
    return curr_text

def is_in_glossary_permissive(match, glossary):
    item = match.group("item")
    # word_match = re.match(r'([\(\)\{\}\[\]]*([\w\- ]+)[\(\)\{\}\[\]]*)', match.group(0))
    if len(item) > 0 and item[0].lower() in glossary:
        glossary_letter = glossary[item[0].lower()]
        if item.lower() in glossary_letter:
            is_glossary_entry = True
            term = item.lower()
            return (is_glossary_entry, term)
        elif len(item) > 1 and item[-1].lower() == "s" and item[:-1].lower() in glossary_letter:
            term = item[:-1].lower()
            is_glossary_entry = True
            return (is_glossary_entry, term)
    return (False, None)

def is_in_glossary(match, glossary):
    item = match.group("item")
    if len(item) > 0 and item[0].lower() in glossary:
        glossary_letter = glossary[item[0].lower()]
        if item.lower() in glossary_letter:
            is_glossary_entry = True
            term = item.lower()
            return (is_glossary_entry, term)
            # return {"valid": is_glossary_entry, "term": term}
        elif len(item) > 1 and item[-1].lower() == "s" and item[:-1].lower() in glossary_letter:
            term = item[:-1].lower()
            is_glossary_entry = True
            # return {"valid": is_glossary_entry, "term": term}
            return (is_glossary_entry, term)
        # else:
        #     is_glossary_entry = False
    word_match = re.search(r'([\w\-]+)', match.group(0))
    if word_match:
        item = word_match.group(0)
        # is_word_match_glossary_entry = word_match.group(0) in glossary
        if len(item) > 0 and item[0].lower() in glossary:
            glossary_letter = glossary[item[0].lower()]
            if item.lower() in glossary_letter:
                is_glossary_entry = True
                term = item.lower()
                return (is_glossary_entry, term)
                # return {"valid": is_glossary_entry, "term": term}
            elif len(item) > 1 and item[-1].lower() == "s" and item[:-1].lower() in glossary_letter:
                term = item[:-1].lower()
                is_glossary_entry = True
                # return {"valid": is_glossary_entry, "term": term}
                return (is_glossary_entry, term)
    # import pdb; pdb.set_trace()

    # return {"valid": False, "term": None}
    return (False, None)

def replace_glossary_links(file_text):
    processed_text = replace_glossary_links_strict(file_text)
    processed_text = replace_glossary_links_permissive(processed_text)
    processed_text = replace_glossary_links_cllinks(processed_text)
    return processed_text

def replace_glossary_links_permissive(file_text):
    glossary_items = re.finditer(GLOSSARY_ITEM_REGEX_PERMISSIVE, file_text)
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
        already_has_cllinks = "ClLinks".lower() in match.group(0).lower()
        inside_table = "|" in match.group(0)
        is_small = len(item) <= 32 and len(item) > 0
        in_right_place = not in_title and not inside_table

        (is_valid_entry, term) = is_in_glossary_permissive(match, glossary_json)
        in_setf_line = is_in_setf_line(match, file_text)

        if in_right_place and is_small and not already_has_cllinks and is_valid_entry and not in_setf_line:
            # print(item)
            extra_asterisk = "*" if len(item) > 0 and item[-1] == "\\" else ""
            pre = match.group("pre")
            post = match.group("post")
            cl_link = pre + '<GlossaryTerm styled={true} term={"' + term + '"}><i>'  + item + extra_asterisk + '</i></GlossaryTerm>' 
            # + post
            text_array.append(file_text[start_index:match.start()])
            text_array.append(cl_link)
            start_index = match.end()
    text_array.append(file_text[start_index:])
    processed_text = "".join(text_array)
    return processed_text

def replace_glossary_links_strict(file_text):
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
        already_has_cllinks = "ClLinks".lower() in match.group(0).lower()
        inside_table = "|" in match.group(0)
        is_small = len(item) <= 32 and len(item) > 0
        in_right_place = not in_title and not inside_table

        (is_valid_entry, term) = is_in_glossary(match, glossary_json)
        in_setf_line = is_in_setf_line(match, file_text)

        if in_right_place and is_small and not already_has_cllinks and is_valid_entry and not in_setf_line:
            # print(item)
            extra_asterisk = "*" if len(item) > 0 and item[-1] == "\\" else ""
            pre = match.group("pre")
            post = match.group("post")
            cl_link = pre + '<GlossaryTerm styled={true} term={"' + term + '"}><i>'  + item + extra_asterisk + '</i></GlossaryTerm>' 
            # + post
            text_array.append(file_text[start_index:match.start()])
            text_array.append(cl_link)
            start_index = match.end()
    text_array.append(file_text[start_index:])
    processed_text = "".join(text_array)
    return processed_text

def replace_dictionary_links(file_text):
    processed_text = replace_dictionary_links_strict(file_text)
    processed_text = replace_dictionary_links_permissive(processed_text)
    processed_text = replace_dictionary_links_cllinks(processed_text)
    return processed_text

def replace_dictionary_links_cllinks(file_text):
    return replace_cl_link(file_text, CLLINK_DICTIONARY_ITEM_REGEX, '<DictionaryLink{}DictionaryLink>')

def replace_glossary_links_cllinks(file_text):
    return replace_cl_link(file_text, CLLINK_GLOSSARY_ITEM_REGEX, '<GlossaryTerm{}GlossaryTerm>')

def replace_cl_link(file_text, cl_link_regex, replacement_format_string):
    dictionary_items = re.finditer(cl_link_regex, file_text)
    all_items = [m for m in dictionary_items]
    text_array = []
    start_index = 0
    
    for match in all_items:
        item = match.group("item")
        cl_link = replacement_format_string.format(item)
        # + post
        text_array.append(file_text[start_index:match.start()])
        text_array.append(cl_link)
        start_index = match.end()
    text_array.append(file_text[start_index:])
    processed_text = "".join(text_array)
    return processed_text

def replace_dictionary_links_permissive(file_text):
    dictionary_items = re.finditer(DICTIONARY_ITEM_REGEX_PERMISSIVE, file_text)
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
        already_has_cllinks = "ClLinks".lower() in match.group(0).lower()
        inside_table = "|" in match.group(0)
        is_small = len(item) <= 32 and len(item) > 0
        in_right_place = not in_title and not inside_table
        is_dictionary_entry = item in dictionary_json
        in_setf_line = is_in_setf_line(match, file_text)
        pre = match.group("pre")
        post = match.group("post")

        if not is_dictionary_entry:
            # TODO this does not seem to have added any entries...
            items = item.split(", ")
            all_match = all([curr_item in dictionary_json for curr_item in items])
            if all_match:
                is_dictionary_entry = True
                get_link_string = lambda item: '<DictionaryLink styled={true} term={"' + item + '"}' + f'><b>{item}</b></DictionaryLink>'
                cl_link = pre + ", ".join([get_link_string(curr_item) for curr_item in items])
        else:
            term = item
            extra_asterisk = "*" if len(item) > 0 and item[-1] == "\\" else ""
            cl_link = pre + '<DictionaryLink styled={true} term={"' + term + '"}><b>'  + item + extra_asterisk + '</b></DictionaryLink>' 
            # + post

        if in_right_place and is_small and not already_has_cllinks and is_dictionary_entry and not in_setf_line:
            text_array.append(file_text[start_index:match.start()])
            text_array.append(cl_link)
            start_index = match.end()
    
    text_array.append(file_text[start_index:])
    processed_text = "".join(text_array)
    return processed_text

def is_in_setf_line(match, file_text):
    new_line_indices = [i for i, ltr in enumerate(file_text) if ltr == "\n"]
    (new_line_index, end_line_index) = (0, len(file_text))
    for i in new_line_indices:
        if match.start() > i:
            new_line_index = i
        if match.end() < i and end_line_index < i:
            end_line_index = i
    match_line = file_text[new_line_index:end_line_index]
    in_setf_line = "(setf" in match_line
    return in_setf_line

def replace_dictionary_links_strict(file_text):
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
        already_has_cllinks = "ClLinks".lower() in match.group(0).lower()
        inside_table = "|" in match.group(0)
        is_small = len(item) <= 32 and len(item) > 0
        in_right_place = not in_title and not inside_table
        is_dictionary_entry = item in dictionary_json
        # if "do\*" in match.group(0):
        word_match = re.search(r'([\w\-]+)', match.group("item"))
        if word_match and not is_dictionary_entry:
            word_item = word_match.group(0)
            # is_word_match_dictionary_entry = word_item in dictionary_json and len(word_item) >= len(item)-4
            is_word_match_dictionary_entry = word_item in dictionary_json
        else:
            is_word_match_dictionary_entry = False
            # import pdb; pdb.set_trace()
        is_valid_entry = is_dictionary_entry or is_word_match_dictionary_entry
        
        in_setf_line = is_in_setf_line(match, file_text)

        if in_right_place and is_small and not already_has_cllinks and is_valid_entry and not in_setf_line:
            term = item if is_dictionary_entry else word_match.group(0)
            if "*)" in match.group(0) or "(setf (macro-function" in file_text:
                # or "*)" in match_line
                print(f'match: {match.group(0)}')
                print(f'item: {item}')
                print(f'term: {term}')
                print(f'dictionary_json[term]: {dictionary_json[term]}')
                # pprint(file_text)
                import pdb; pdb.set_trace()
            # print(item)
            extra_asterisk = "*" if len(item) > 0 and item[-1] == "\\" else ""
            pre = match.group("pre")
            post = match.group("post")
            cl_link = pre + '<DictionaryLink styled={true} term={"' + term + '"}><b>'  + item + extra_asterisk + '</b></DictionaryLink>' 
            # + post
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
