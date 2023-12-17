import re, sys, os, subprocess, json
from pprint import pprint

# DEFINITION_REGEX = r'\n(\s*(\*\*([\w\\/ \-]+)\*\*)((?:(?!\n)[\w\W.])+))\n'
# HEADER_REGEX = r'(\n*\---\n+(\w+\:[\"\' \w\*_]+\n+)+\---\n+\s*\*\*\w\*\*\s*)'
# GLOSSARY_FILE = "./glossary_output/glossary.json"
# NOT_MATCHED_FILE = "./glossary_output/not_matched.json"

VARIABLE_ITEM_REGEX = r'(\*\*\w+(-\w+)*\*\*[\s\n]*\*\w+([\s\n]*\w+)*\*[\s\n]*\*\*\w+([\s\n]*\w+)*:\*\*)'
# VARIABLE_ITEM_SYNTAX_REGEX = r'(\*\*\w+(-\w+)*\*\*[\s\n]*\*\w+([\s\n]*\w+)*\*[\s\n]*\*\*Syntax*:\*\*)'
# ITEM_EXPLICIT_REGEX = r'(\*\*(\w+(-\w+)*)\*\*[\s\n]*\*\w+([\s\n]*\w+)*\*[\s\n]*\*\*(Syntax|(Class Precedence List))*:\*\*)'
ITEM_EXPLICIT_REGEX = r'((\*\∗\*)?\*\*((\w+(-\w+)*)(, (\w+(-\w+)*))*)\*\*[\s\n]*\*(\∗ )?\w+([\s\n]*\w+)*\*[\s\n]*\*\*(Syntax|(Class Precedence List)|(Value Type))*:\*\*)'


# FUNCTION_REGEX = r'(\*\*(\w+(-\w+)*)\*\*[\s\n]*\*\w+([\s\n]*\w+)*\*[\s\n]*\*\*(Syntax|(Class Precedence List)|(Value Type))*:\*\*)'
# VARIABLE_REGEX = r'(\*\*(\w+(-\w+)*)\*\*[\s\n]*\*\w+([\s\n]*\w+)*\*[\s\n]*\*\*(Syntax|(Class Precedence List)|(Value Type))*:\*\*)'

ITEM_FILE_TEMPLATE = "# {}\n\nimport {} from './_{}.md';\n\n<{} />\n\n## Expanded Reference: {}\n\n:::tip\nTODO: Please contribute to this page by adding explanations and examples\n:::\n\n```lisp\n{}\n```\n"

def split_dictionary_for_file(filepath):
    print(filepath)
    file = open(filepath, "r")
    text = file.read()
    file.close()
    final_files = []
    # import pdb; pdb.set_trace()
    # definitions_in_file = re.finditer(VARIABLE_ITEM_REGEX, text)
    # definitions_in_file_list = re.findall(VARIABLE_ITEM_REGEX, text)
    # definitions_in_file_list = [re.findall(VARIABLE_ITEM_REGEX, text)]
    definitions_in_file_w_syntax = [match for match in re.finditer(ITEM_EXPLICIT_REGEX, text)]
    definitions_in_file_w_syntax_list = re.findall(ITEM_EXPLICIT_REGEX, text)
    if len(definitions_in_file_w_syntax_list) == 0:
        return False
    filename_parts = filepath.split("/")[-1].replace(".md", "").split("_")
    first_item_name = definitions_in_file_w_syntax_list[0][2].split(",")[0]
    first_item_name_no_dash = first_item_name.replace("-", "")
    valid_item = first_item_name in filename_parts or first_item_name_no_dash in filename_parts
    if len(definitions_in_file_w_syntax_list) == 1 and valid_item:
        return False
    # if "floating" in filepath or "copyalist" in filepath:
    #     import pdb; pdb.set_trace()
    final_items = [{"filepath": filepath, "start_index": 0}]
    for definition in definitions_in_file_w_syntax:
        # import pdb; pdb.set_trace()
        # TODO fix react component names for multiple variables case
        # TODO make sure not overwriting same file?
        item_name = definition.groups()[2]
        item_title = item_name if definition.groups()[1] == None else f"\*{item_name}\*"
        lisp_item_name = f"({item_name} )" if definition.groups()[1] == None else f"*{item_name}*"
        item_name_for_path = item_name.replace(", ", "_")
        variable_path_suffix = "" if definition.groups()[1] == None else f"_variable"
        variable_react_name = "" if definition.groups()[1] == None else f"Variable"
        item_filename = f"{item_name_for_path}{variable_path_suffix}.md"
        react_item_component = "".join([part.capitalize()for part in item_name.split(",")[0].split("-")])
        react_item_component += variable_react_name
        md_file = ITEM_FILE_TEMPLATE.format( item_title, react_item_component, item_name, react_item_component, item_title, lisp_item_name)
        final_items.append({"filepath": item_filename, "start_index": definition.start(), "md_text": md_file})
        # if definition.groups() != None and "print-array" in definition.groups()[0]:
        #     import pdb; pdb.set_trace()
    if len(final_items) == 1:
        final_items[0]["text"] = text
    elif len(final_items) > 1:
        for index, end_index in enumerate([item["start_index"] for item in final_items[1:] + [{"start_index": len(text)}]]):
            start_index = final_items[index]["start_index"]
            final_items[index]["text"] = text[start_index:end_index]
    # new_text = new_text.strip()
    # not_matched = []
    # if len(new_text) != 0:
        # not_matched.append([filepath, new_text])
    # return (glossary, not_matched)
    # import pdb; pdb.set_trace()
    return final_items

def save_split_items(split_items, given_path):
    if len(split_items) > 1:
        # import pdb; pdb.set_trace()
        for item in split_items:
            # import pdb; pdb.set_trace()
            if item["filepath"] == given_path:
                file = open(item["filepath"], "w")
                file.write(item["text"])
                file.close()
            else:
                dir_path = "/".join(given_path.split("/")[:-1])
                # Visible Markdown File
                file = open(f"{dir_path}/{item['filepath']}", "w")
                file.write(item["md_text"])
                file.close()
                # Hidden Markdown File
                file = open(f"{dir_path}/_{item['filepath']}", "w")
                file.write(item["text"])
                file.close()

def split_dictionary(given_path):
    if os.path.isfile(given_path):
        split_items = split_dictionary_for_file(given_path)
        if split_items:
            save_split_items(split_items, given_path)
    elif os.path.isdir(given_path):
        for root, directories, filenames in os.walk(given_path):
            for filename in filenames:
                if filename.endswith(".md"):
                    filepath = os.path.join(root, filename)
                    split_items = split_dictionary_for_file(filepath)
                    if split_items:
                        save_split_items(split_items, filepath)


def main(args=[]):
    for arg in args:
        split_dictionary(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files ")
    else:
        main(sys.argv[1:])
