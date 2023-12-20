import os, sys, json, re
from pprint import pprint 

# TEX_DIR = "./tex-files"
MD_DIR = "./output/"
# CODE_BLOCKS_JSON = "./output/code-blocks.json"
DICTIONARY_LINKS = "./output/dictionary-entries.md"
DICTIONARY_TEXT = []
ITEM_EXPLICIT_REGEX = r'((\*\∗\*)?\*\*(?P<item_name>([\w=/<>\-+\\]+(-[\w=/<>\-+\\]+)*)(, ([\w=/<>\-+\\]+(-[\w=/<>\-+\\]+)*))*)\*\*[\s\n]*\*(\∗ )?(?P<item_type>\w+([\s\n]*\w+)*)\*[\s\n]*\*\*(Syntax|(Class Precedence List)|(Value Type)|Supertypes|(Constant Value))*:\*\*)'

def get_last_section(filenames):
    # section_name_regex = r'_((\w-)+).*\.md'
    section_name_regex = r'_?((\w\w?-)+).*'
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

def get_char_key_for_chapter_name(chapter_name):
    return chr(ord("a") + int(chapter_name.split("-")[-1]))

def get_new_section_name(last_section_filename, chapter_name):
    if last_section_filename == None:
        chapter_key = get_char_key_for_chapter_name(chapter_name)
        return  f"{chapter_key}-b-dictionary"
    else:
        section_number_match = get_section_number(last_section_filename)
        new_section_number = section_number_match[:-1] + chr(ord(section_number_match[-1]) + 1)
        new_section_name = new_section_number + "-dictionary"
        return new_section_name
    
def ensure_dictionary_exists(current_dictionary, chapter_dir):
    dictionary_dir_path = os.path.join(chapter_dir, current_dictionary)
    if not os.path.isdir(dictionary_dir_path):
        os.mkdir(dictionary_dir_path)
    category_file_path = os.path.join(chapter_dir, current_dictionary + "/_category_.json")
    if not os.path.isfile(category_file_path):
        make_category_json_file(current_dictionary, chapter_dir)

# TODO creating dictionary in wrong directory!
    # 'output/chap-13/bd-b-character-concepts/bd-c-dictionary'
# TODO not always should I make a dictionary...

def split_dictionary_files(given_dir):
    current_dictionary = None
    chapter_dir = None
    for root, dirs, filenames in os.walk(given_dir):
        if len([filename for filename in filenames if ".md" in filename]) > 0:
            dictionary_dirs = [dir for dir in dirs if "dictionary" in dir]
            chapter_parts = [part for part in root.split("/") if "chap" in part]
            # We potentially are inside a chapter directory or lower
            if len(chapter_parts) > 0:
                curr_chapter = chapter_parts[0]
                if chapter_dir != None and not curr_chapter in chapter_dir:
                    current_dictionary = None
                    chapter_dir = None
                if len(dictionary_dirs) > 1 and current_dictionary == None:
                    print("Weird, multiple dictionary directories found. Please check what's going on.")
                    import pdb; pdb.set_trace()
                elif len(dictionary_dirs) == 0 and current_dictionary == None:
                    chapter_dir = root
                    try:
                        last_section = get_last_section(filenames)
                        dictionary_section_name = get_new_section_name(last_section, curr_chapter)
                        current_dictionary = dictionary_section_name
                    except:
                        print(root)
                        print(filenames)
                        import pdb; pdb.set_trace()
                elif len(dictionary_dirs) == 1 and current_dictionary == None:
                    dictionary_dir = dictionary_dirs[0]
                    current_dictionary = dictionary_dir
                    chapter_dir = root
                for filename in [filename for filename in filenames if ".md" in filename]:
                    filepath = os.path.join(root, filename)
                    process_dictionary_file(filepath, dictionary_dir=current_dictionary, chapter_dir=chapter_dir)
    if len(DICTIONARY_TEXT) > 0:
        print("Rewriting dictionary links file {DICTIONARY_LINKS}")
        file = open(DICTIONARY_LINKS, "w")
        file.write("\n".join(DICTIONARY_TEXT))
        file.close()

def process_dictionary_file(curr_file_path, dictionary_dir=None, chapter_dir=None):
    file = open(curr_file_path, "r")
    curr_text = file.read()
    file.close()

    if is_dictionary_file_content(curr_text): 
        new_file_sections = split_dictionary_text(curr_text)
        if len(new_file_sections) > 1:
            ensure_dictionary_exists(dictionary_dir, chapter_dir)
            last_file_before_dictionary_section = open(curr_file_path, "w")
            last_file_before_dictionary_section.write(new_file_sections[0])
            last_file_before_dictionary_section.close()
            for file_section in new_file_sections[1:]:
                dictionary_path = os.path.join(chapter_dir, dictionary_dir)
                create_dicionary_entry_files(file_section, dictionary_path) 

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

ITEM_FILE_TEMPLATE = "---\ntitle: \"{}\"\n---\n\n# {}\n\nimport {} from './_{}';\n\n<{} />\n\n## Expanded Reference: {}\n\n:::tip\nTODO: Please contribute to this page by adding explanations and examples\n:::\n\n```lisp\n{}\n```\n"

def replace_special_chars(given_string):
    temp_string = given_string.replace("=", "equal").replace("/", "slash").replace("<", "lt")
    temp_string = temp_string.replace(">", "gt").replace("+", "plus").replace("\\", "back-slash")
    temp_string = temp_string.replace("*", "asterisk").replace("#", "hash")
    return temp_string

def create_dicionary_entry_files(file_section, dictionary_path):
    # create _x.md, x.md filenames import correctly... add first heading as title to x.md
    # avoid name conflicts, make a dic, +a to filenames...

    matches = re.match(ITEM_EXPLICIT_REGEX, file_section)
    names_used = {}
    if matches:
        definition = matches
        if definition.groups()[2] != definition.groupdict()["item_name"]:
            import pdb; pdb.set_trace()
        item_name = definition.groupdict()["item_name"]
        item_type = definition.groupdict()["item_type"].strip().replace(" ", "-").lower()
        item_title = item_name if definition.groups()[1] == None else f"\*{item_name}\*"
        lisp_item_name = f"({item_name} )" if definition.groups()[1] == None else f"*{item_name}*"
        item_name_for_path = item_name.replace(", ", "_")
        item_name_for_path = replace_special_chars(item_name_for_path)
        variable_react_name = definition.groupdict()["item_type"].strip().replace(" ", "")
        item_filename = f"{item_name_for_path}_{item_type}.md"
        react_item_component = "".join([part.capitalize() for part in replace_special_chars(item_name).split(",")[0].split("-")])
        react_item_component += variable_react_name
        md_file = ITEM_FILE_TEMPLATE.format(f"{item_name}", item_title, react_item_component, item_filename, react_item_component, item_title, lisp_item_name)
        # final_items.append({"filepath": item_filename, "start_index": definition.start(), "md_text": md_file})

        # groups = matches.groups()
        # item_name = re.sub("\W", "a", "".join([name_part for name_part in groups[1].split(",")[0].split("-")]))
        # if item_name in names_used:
        #     item_name = get_new_item_name(names_used, item_name)
        # names_used[item_name] = True
        # heading = "# " + groups[1] + "\n\n"
        # component_name = "".join([re.sub("\W", "a", item).capitalize() for item in groups[1].split("-")])
        # filename = item_name + ".md"
        # hidden_filename = "_" + filename
        # hidden_file_text = apply_example_code_blocks(file_section)
        # import_statement = f"import {component_name} from './{hidden_filename}';\n\n"
        # import_component = f"<{component_name} />\n\n"
        # expanded_reference = f"## Expanded Reference: {groups[1]}\n\n:::tip\nTODO: Please contribute to this page by adding explanations and examples\n:::\n\n```lisp\n({groups[1]} )\n```\n"
        # markdown_contents = heading + import_statement + import_component + expanded_reference

        # Visible Markdown File
        visible_md_path = f"{dictionary_path}/{item_filename}"
        if os.path.exists(visible_md_path):
            # print("Problem! This path should not exist!")
            # print(visible_md_path)
            print(f"+ Overwriting: {visible_md_path}")
            # import pdb; pdb.set_trace()
        file = open(visible_md_path, "w")
        file.write(md_file)
        file.close()
        # Hidden Markdown File
        hidden_md_path = f"{dictionary_path}/_{item_filename}"
        if os.path.exists(hidden_md_path):
            # print("Problem! This path should not exist!")
            # print(hidden_md_path)
            print(f"+ Overwriting: {hidden_md_path}")
            # import pdb; pdb.set_trace()
        file = open(hidden_md_path, "w")
        file.write(file_section)
        file.close()

        # print(f"- [{item_filename}]({os.path.join(dictionary_path, item_filename)})")
        DICTIONARY_TEXT.append(f"- [{item_filename}]({os.path.join(dictionary_path, item_filename)})")
    else:
        print("This should be a valid dictionary entry because it was already checked! This code should not execute! ERROR")
        import pdb; pdb.set_trace()

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

def main(args=[MD_DIR]):
    for arg in args:
        split_dictionary_files(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        # print("Please provide a directory with markdown files")
        print("No directory with markdown files provided")
        print(f"Defaulting to run on {MD_DIR}")
        main()
    else:
        main(sys.argv[1:])
