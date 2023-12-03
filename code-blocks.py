import os, sys, json, re

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
                curr_file = open(os.path.join(last_sec_dir, last_file), "r")
                curr_text = curr_file.read()
                curr_file.close()
                is_dictionary_file_content(curr_text)
        break


def is_dictionary_file_content(content):
    dic_file_regex = r'(\*\*Class Precedence List:\*\*)|(\*\*Syntax:\*\*)'
    matches = re.search(dic_file_regex, content)
    if matches:
        print(matches.span())
        print(matches.group())
        return True
    return False
        # print("is_dictionary_file")

def split_dictionary_content(content):
    print("split_dictionary_content")

def main(args=[]):
    code_blocks = get_block_list()
    print(code_blocks[0][0])
    print("\n")
    print(code_blocks[0][1])

if __name__ == "__main__":
    # main(sys.argv[1:])
    # replace_code_blocks([], MD_DIR)
    split_dictionary_files(MD_DIR)