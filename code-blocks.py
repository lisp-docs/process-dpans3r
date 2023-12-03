import os, sys, json, re
from pprint import pprint 

TEX_DIR = "./tex-files"
MD_DIR = "./output/"
CODE_BLOCKS_JSON = "./output/code-blocks.json"

def get_block_list():
    # for root, dirs, filenames in os.walk(TEX_DIR):
    #     for filename in filenames:
    #         if filename.endswith(".tex"):
    #             print(filename)
    #             print(os.path.join(root, filename))
    with open(CODE_BLOCKS_JSON, "r") as filehandle:
        return json.load(filehandle)

def replace_code_blocks(code_blocks, given_dir):
    # print(os.listdir(given_dir))
    for root, dirs, filenames in os.walk(given_dir):
        # print(root)
        # print(dirs)
        # print`([os.path.join(given_dir, dir) for dir in dirs].)
        for dir in dirs:
            for curr_root, chap_dirs, section_filenames in os.walk(os.path.join(given_dir, dir)):
                print(os.path.join(given_dir, dir))
                # print(section_filenames)
                last_section = get_last_section(section_filenames)
                print(last_section)
            break
        break
        for filename in filenames:
            if filename.endswith(".md"):
                # print(filename)
                filepath = os.path.join(root, filename)
                # print(filepath)
                # curr_file = open(filepath, "r")
                # curr_text = curr_file.read()
                # curr_file.close()
        break

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
        # print(all_matches_list)
        all_matches_list.sort()
        # print(all_matches_list)
        # print(all_matches_list[-1])
        # print(all_matches_dict[all_matches_list[-1]])
        return all_matches_dict[all_matches_list[-1]]
    # print(all_matches_dict[all_matches_list.sort()[-1]])
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
    # print(last_section_filename)
    # section_name_regex = r'_?((\w+-)+).*'
    # section_number_regex = r'^\w+-\w+'
    # section_name_match = re.search(section_name_regex, last_section_filename)
    # if section_name_match:
    #     # print(section_name_match)
    #     section_name_matched_string = section_name_match.groups()[0]
        # section_number_match = re.search(section_number_regex, section_name_matched_string).group()
    section_number_match = get_section_number(last_section_filename)
        # print(section_name_matched_string)
        # print(section_number_match)
    new_section_number = section_number_match[:-1] + chr(ord(section_number_match[-1]) + 1)
    new_section_name = new_section_number + "-dictionary"
        # print(section_number_match[:-1] + chr(ord(section_number_match[-1]) + 1))
        # print(new_section_name)
    return new_section_name
    # raise Exception("Could not find section name for " + last_section_filename)
    

def split_dictionary_files(given_dir):
    # Loop through all files up to one depth
    # get the last file _x-x- or _x-x-x- where x.x.x would be the section number
    # check if it has a dictionary
    # if it does: create a new file _x-(x+1)-dictionary.md
    # add title: make title be Chapter_name + Dictionary
    # save file
    # then later on modify the content and save the file...
    # the subsections always end with a colon **Notes:**. The titles don't have colons
    # 
    print("split_dictionary_files")
    for root, dirs, filenames in os.walk(given_dir):
        # print(root)
        # print(dirs)
        # print`([os.path.join(given_dir, dir) for dir in dirs].)
        # Chapters
        for dir in dirs:
            # for sect
            curr_root = os.path.join(given_dir, dir)
            section_dir_names = [filename for filename in os.listdir(curr_root) if os.path.isdir(os.path.join(curr_root, filename))]
            # for curr_root, chap_dirs, section_filenames in os.walk(os.path.join(given_dir, dir)):
            print(curr_root)
            # print(section_dir_names)
            last_section = get_last_section(section_dir_names)
            # print(last_section)
            if last_section:
                # print(last_section)
                last_sec_dir = os.path.join(curr_root, last_section)
                last_file = get_last_section(os.listdir(last_sec_dir))
                # print(last_file)
                curr_file_path = os.path.join(last_sec_dir, last_file)
                curr_file = open(curr_file_path, "r")
                curr_text = curr_file.read()
                curr_file.close()
                if is_dictionary_file_content(curr_text):
                    process_dictionary_file(curr_root, last_section, curr_file_path, curr_text)
        break

def process_dictionary_file(root, last_section, filapath, curr_text):
    # Create Folder
    # Create _category_.json file
    # Open category json file, parse, get name without r'\d+\. ', add " Dictionary"...
    # create files (make sure no name conflicts! error if exists!) as _name.md
    # Create name.md files importing the _name.md and adding "\n\n## Expanded Reference: "
    #   and add there the first heading 
    # Add a first heading, get the name from the dicionary item...
    # Parse all dictionary items appropiately...
    # print("hello")
    # os.mkdir()
    new_section_name = get_new_section_name(last_section)
    new_section_dir = os.path.join(root, new_section_name)
    print(new_section_dir)
    # print(os.path.join(root, new_section_name))
    # os.mkdir(new_section_dir)
    make_category_json_file(new_section_name, root)
    new_file_sections = split_dictionary_text(curr_text)
    # for file_section in new_file_sections:
    #     print(file_section)
    #     create_dicionary_entry_files(file_section, new_section_dir)

def split_dictionary_text(curr_text):
    # item_regex = r'\*\*.*?\*\* \*\w+(\s+\w+)*\* \n\n\*\*Class Precedence List:\*\* \n'
    item_regex = r'(\*\*.*?\*\* \*[A-Z][a-z]*(\s+\w+)*\*\s*\n)'
    matches = re.findall(item_regex, curr_text)
    start_indices = [m.start(0) for m in re.finditer(item_regex, curr_text)]
    pprint(matches)
    pprint(start_indices)
    print(curr_text[:start_indices[0]])
    return matches

def create_dicionary_entry_files(file_section, new_section_dir):
    print("create_dicionary_entry_files(file_section, new_section_dir)")

def make_category_json_file(section_name, root):
    CATEGORY_JSON = '{\n  "label": "1. Introduction",\n  "position": 1,\n  "link": {\n    "type": "generated-index",\n    "description": "1. Introduction"\n  }\n}\n'
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
    curr_json["position"] = position
    curr_json["label"] = section_label
    curr_json["link"]["description"] = section_label
    # print(curr_json)
    # print(os.path.join(root, section_name + "/_category_.json"))
    # final_category_file = open(os.path.join(root, section_name + "/_category_.json"), "w")
    # final_category_file.write(curr_json)
    # final_category_file.close()

    # curr_json["position"] = 

def char_to_num_decode(char_string):
    return "".join([str(ord(curr_char) - ord("a")) for curr_char in char_string])

def is_dictionary_file_content(content):
    dic_file_regex = r'(\*\*Class Precedence List:\*\*)|(\*\*Syntax:\*\*)'
    matches = re.search(dic_file_regex, content)
    if matches:
        # print(matches.span())
        # print(matches.group())
        return True
    return False
        # print("is_dictionary_file")

def split_dictionary_content(content):
    print("split_dictionary_content")

def clear_footers(given_dir):
    footer_regex = r'[A-Z][a-z]+ \*\*(\w|\d+)–\d+\*\*'
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                curr_filepath = os.path.join(root, filename)
                curr_file = open(curr_filepath, "r")
                curr_text = curr_file.read()
                curr_file.close()
                match = re.search(footer_regex, curr_text)
                if match:
                    print(match.group())
                    curr_text.replace(match.group(), "")

def main(args=[]):
    code_blocks = get_block_list()
    print(code_blocks[0][0])
    print("\n")
    print(code_blocks[0][1])

if __name__ == "__main__":
    # main(sys.argv[1:])
    # replace_code_blocks([], MD_DIR)
    # split_dictionary_files(MD_DIR)
    clear_footers(MD_DIR)