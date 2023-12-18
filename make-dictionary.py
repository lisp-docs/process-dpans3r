import os, sys, json, re
from pprint import pprint 

TEX_DIR = "./tex-files"
MD_DIR = "./output/"
CODE_BLOCKS_JSON = "./output/code-blocks.json"
ITEM_EXPLICIT_REGEX = r'((\*\∗\*)?\*\*(?P<item_name>([\w=/<>\-+\\]+(-[\w=/<>\-+\\]+)*)(, ([\w=/<>\-+\\]+(-[\w=/<>\-+\\]+)*))*)\*\*[\s\n]*\*(\∗ )?(?P<item_type>\w+([\s\n]*\w+)*)\*[\s\n]*\*\*(Syntax|(Class Precedence List)|(Value Type)|Supertypes|(Constant Value))*:\*\*)'

def get_last_section(filenames):
    # section_name_regex = r'_((\w-)+).*\.md'
    section_name_regex = r'_?((\w-)+).*'
    all_matches_list = []
    all_matches_dict = {}
    for filename in filenames:
        matches = re.search(section_name_regex, filename)
        if matches:
            section_number = matches.groups()[0]
            # print(section_number)
            all_matches_dict[section_number] = filename
            all_matches_list.append(section_number)
    if len(all_matches_list) > 0:
        all_matches_list.sort()
        return all_matches_dict[all_matches_list[-1]]
    return None

def get_section_number(section_filename):
    section_name_regex = r'_?((\w+-)+).*'
    section_number_regex = r'^\w+-\w+'
    section_name_match = re.search(section_name_regex, section_filename)
    if section_name_match:
        # print(section_name_match)
        section_name_matched_string = section_name_match.groups()[0]
        section_number_match = re.search(section_number_regex, section_name_matched_string).group()
        return section_number_match
    raise Exception("Invalid Section Name " + section_filename)

def get_new_section_name(last_section_filename):
    section_number_match = get_section_number(last_section_filename)
    new_section_number = section_number_match[:-1] + chr(ord(section_number_match[-1]) + 1)
    new_section_name = new_section_number + "-dictionary"
    return new_section_name
    

def split_dictionary_files(given_dir):
    # Loop through all files up to one depth
    # get the last file _x-x- or _x-x-x- where x.x.x would be the section number
    # check if it has a dictionary item
    # TODO check if dictionary already exists!
    # if it does: create a new file _x-(x+1)-dictionary.md
    # add title: make title be Chapter_name + Dictionary
    # save file
    # then later on modify the content and save the file...
    # the subsections always end with a colon **Notes:**. The titles don't have colons
    for root, dirs, filenames in os.walk(given_dir):
        # Chapters
        dictionary_dirs = [dir for dir in dirs if "dictionary" in dir]
        if len(dictionary_dirs) > 1:
            print("Weird, multiple dictionary directories found. Please check what's going on.")
            import pdb; pdb.set_trace()
        elif len(dictionary_dirs) == 0:
            # TODO will need to create a dictionary directory etc
            print("create dictionary dir")
            dictionary_dir = "" # TODO
        else: # Must be len(dictionary_dirs) == 1
            dictionary_dir = dictionary_dirs[0]
        for dir in dirs:
            curr_root = os.path.join(given_dir, dir)
            section_dir_names = [filename for filename in os.listdir(curr_root) if os.path.isdir(os.path.join(curr_root, filename))]
            # TODO should actually read each file, not just the last one...
            # TODO and check if there's a dictionary directory in the file's directory...
            last_section = get_last_section(section_dir_names)
            if last_section:
                last_sec_dir = os.path.join(curr_root, last_section)
                last_file = get_last_section(os.listdir(last_sec_dir))
                curr_file_path = os.path.join(last_sec_dir, last_file)
                curr_file = open(curr_file_path, "r")
                curr_text = curr_file.read()
                curr_file.close()
                # TODO get dictionary path if it exists... pass it to process file below
                process_dictionary_file(curr_root, last_section, curr_file_path, curr_text)
        break

def process_dictionary_file(root, last_section, curr_file_path, curr_text, dictionary_dir=None):
    # Create Folder
    # Create _category_.json file
    # Open category json file, parse, get name without r'\d+\. ', add " Dictionary"...
    # create files (make sure no name conflicts! error if exists!) as _name.md
    # Create name.md files importing the _name.md and adding "\n\n## Expanded Reference: "
    #   and add there the first heading 
    # Add a first heading, get the name from the dicionary item...
    # Parse all dictionary items appropiately...
    # TODO probably move this into whoever called this function
    if dictionary_dir == None:
        new_section_name = get_new_section_name(last_section)
        dictionary_dir = os.path.join(root, new_section_name)
        os.mkdir(dictionary_dir)
        make_category_json_file(new_section_name, root)
    if is_dictionary_file_content(curr_text): # DONE
        new_file_sections = split_dictionary_text(curr_text) # DONE
        if len(new_file_sections) > 1:
            last_file_before_dictionary_section = open(curr_file_path, "w")
            last_file_before_dictionary_section.write(new_file_sections[0])
            last_file_before_dictionary_section.close()
            for file_section in new_file_sections[1:]:
                create_dicionary_entry_files(file_section, dictionary_dir) # TODO check not overwriting file

def split_dictionary_text(curr_text):
    ITEM_EXPLICIT_REGEX
    start_indices = [m.start(0) for m in re.finditer(ITEM_EXPLICIT_REGEX, curr_text)]
    split_items = [curr_text[0:start_indices[0]]] 
    split_items += [curr_text[start_indices[i]:start_indices[i+1]] for i in range(len(start_indices)-1)]
    split_items += [curr_text[start_indices[-1]:]]
    return split_items

def get_new_item_name(names_used, curr_name):
    names_used = {}
    found_name = False
    curr_sufix = "a"
    while not found_name:
        new_name = curr_name + curr_sufix
        if new_name in names_used:
            curr_sufix = chr(ord(curr_sufix) + 1)
        else:
            # found_name = True
            return new_name

def apply_first_example_code_block(given_text):
    example_title = "**Examples:**"
    title_regex = r'(\*\*\w+(\w*\s*)*:\*\*\s*?\n)'
    begin_code = "\n```lisp\n"
    end_code = "\n```\n"
    example_index = given_text.find(example_title)

    # No examples section
    if example_index == -1:
        return given_text
    
    post_example_index = example_index + len(example_title)
    # next_title_index = len(given_text)
    next_title_match = re.search(title_regex, given_text[post_example_index:])
    if next_title_match:
        next_title_index = next_title_match.span()[0] + post_example_index
        return given_text[:post_example_index] + begin_code + given_text[post_example_index:next_title_index] + end_code + given_text[next_title_index:]
    else:
        return given_text[:post_example_index] + begin_code + given_text[post_example_index:] + end_code

def get_title_indices(title, given_text):
    # # title = "**Examples:**"
    # title_regex = f'({title})'
    # all_titles = re.findall(re.escape(title_regex), given_text)
    # indices = []
    # # curr_index = 0 - len(title)
    # offset = 0
    # for i in all_titles:
    #     # offset = curr_index + len(title)
    #     found_index = given_text[offset:].find(title) + offset
    #     indices.append(found_index)
    #     offset = found_index + len(title)
    # return indices
    return [index for index in range(len(given_text)) if given_text.startswith(title, index)]

def apply_example_code_blocks_old(given_text):
    example_title = "**Examples:**"

    # all_examples = re.findall(re.escape(example_title), given_text)
    all_indices = get_title_indices(example_title, given_text)

    if len(all_indices) == 0:
        return given_text

    curr_text = given_text[0:all_indices[0]]

    for i in range(len(all_indices)):
        text_with_escaped_code_block = apply_first_example_code_block(curr_text[all_indices[i]:])
        if i +1 == len(all_indices):
            curr_text += text_with_escaped_code_block
        else:
            # import pdb; pdb.set_trace()
            new_indices = get_title_indices(example_title, text_with_escaped_code_block)
            print(new_indices)
            # there has to be at least one more, so the array above should be at least of two elements
            curr_text += text_with_escaped_code_block[:new_indices[1]]
    return curr_text

def apply_example_code_blocks(given_text):
    example_title = "**Examples:**"
    example_title_regex = f"({re.escape(example_title)})"
    all_indices = [m.span()[0] for m in re.finditer(example_title_regex, given_text)]
    if len(all_indices) == 0:
        return given_text
    start = 0
    text_parts = []
    for i in all_indices:
        text_parts.append(given_text[start: i])
        start = i
    text_parts.append(given_text[start:])
    # import pdb; pdb.set_trace()
    return "".join([apply_first_example_code_block(text_part) for text_part in text_parts])

def create_dicionary_entry_files(file_section, new_section_dir):
    # create _x.md, x.md filename,s import correctly... add first heading as title to x.md
    # avoid name conflicts, make a dic, +a to filenames...
    item_regex = r'(\*\*(.*?)\*\* \*[A-Z][a-z]*(\s+\w+)*\*\s*\n)'
    matches = re.match(item_regex, file_section)
    names_used = {}
    if matches:
        groups = matches.groups()
        item_name = re.sub("\W", "a", "".join([name_part for name_part in groups[1].split(",")[0].split("-")]))
        if item_name in names_used:
            item_name = get_new_item_name(names_used, item_name)
        names_used[item_name] = True
        heading = "# " + groups[1] + "\n\n"
        component_name = "".join([re.sub("\W", "a", item).capitalize() for item in groups[1].split("-")])
        filename = item_name + ".md"
        hidden_filename = "_" + filename
        hidden_file_text = apply_example_code_blocks(file_section)
        import_statement = f"import {component_name} from './{hidden_filename}';\n\n"
        import_component = f"<{component_name} />\n\n"
        expanded_reference = f"## Expanded Reference: {groups[1]}\n\n:::tip\nTODO: Please contribute to this page by adding explanations and examples\n:::\n\n```lisp\n({groups[1]} )\n```\n"
        markdown_contents = heading + import_statement + import_component + expanded_reference
        hidden_file = open(os.path.join(new_section_dir, hidden_filename), "w")
        hidden_file.write(hidden_file_text)
        hidden_file.close()

        display_file = open(os.path.join(new_section_dir, filename), "w")
        display_file.write(markdown_contents)
        display_file.close()

        print(f"- [{groups[1]}]({os.path.join(new_section_dir, filename)})")

def make_category_json_file(section_name, root):
    CATEGORY_JSON = '{\n  "label": "1. Introduction",\n  "link": {\n    "type": "generated-index",\n    "description": "1. Introduction"\n  }\n}\n'
    curr_json = json.loads(CATEGORY_JSON)
    section_number = get_section_number(section_name)
    new_section_number = ".".join([char_to_num_decode(curr_str) for curr_str in section_number.split("-")])
    # print(new_section_number)
    position = int(char_to_num_decode(section_number.split("-")[1]))
    # print(position)
    chapter_category_file = open(os.path.join(root, "_category_.json"))
    section_label = json.load(chapter_category_file)["label"]
    chapter_category_file.close()
    # print(new_section_number + section_label.split(".")[1] + " Dictionary")
    section_label = new_section_number + section_label.split(".")[1] + " Dictionary"
    section_label = re.sub("[\s\*]*&#60;[\*\s]*", "<", section_label)
    section_label = re.sub("[\s\*]*&#62;[\*\s]*", ">", section_label)
    # curr_json["position"] = position
    curr_json["label"] = section_label
    curr_json["link"]["description"] = section_label
    # print(curr_json)
    # print(os.path.join(root, section_name + "/_category_.json"))
    final_category_file = open(os.path.join(root, section_name + "/_category_.json"), "w")
    final_category_file.write(json.dumps(curr_json))
    final_category_file.close()

    # curr_json["position"] = 

def char_to_num_decode(char_string):
    return "".join([str(ord(curr_char) - ord("a")) for curr_char in char_string])

def is_dictionary_file_content(content):
    matches = re.search(ITEM_EXPLICIT_REGEX, content)
    if matches:
        return True
    return False

# def split_dictionary_content(content):
#     print("split_dictionary_content")

def clear_footers(given_dir):
    footer_regex = r'([A-Z][a-z]+ \*\*(\w|\d+)–\d+\*\*)'
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                curr_filepath = os.path.join(root, filename)
                curr_file = open(curr_filepath, "r")
                curr_text = curr_file.read()
                curr_file.close()
                # match = re.search(footer_regex, curr_text)
                matches = re.findall(footer_regex, curr_text)
                if len(matches) > 0:
                    # print(matches)
                    for match in matches:
                        curr_text = curr_text.replace(match[0], "")
                        curr_file = open(curr_filepath, "w")
                        curr_file.write(curr_text)
                        curr_file.close()
                        

if __name__ == "__main__":
    split_dictionary_files(MD_DIR)
