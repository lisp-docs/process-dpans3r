import re, sys, os, subprocess, json
from pprint import pprint

DEFINITION_REGEX = r'\n(\s*(\*\*([\w\\/ \-]+)\*\*)((?:(?!\n)[\w\W.])+))\n'
HEADER_REGEX = r'(\n*\---\n+(\w+\:[\"\' \w\*_]+\n+)+\---\n+\s*\*\*\w\*\*\s*)'
GLOSSARY_FILE = "./glossary_output/glossary.json"
NOT_MATCHED_FILE = "./glossary_output/not_matched.json"

VARIABLE_ITEM_REGEX = r'(\*\*\w+(-\w+)*\*\*[\s\n]*\*\w+([\s\n]*\w+)*\*[\s\n]*\*\*\w+([\s\n]*\w+)*:\*\*)'

def split_dictionary_for_file(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    definitions_in_file = re.finditer(DEFINITION_REGEX, text)
    re.findall(DEFINITION_REGEX, text)[-1]
    header = re.findall(HEADER_REGEX, text)
    new_text = text
    if len(header) != 0:
        # import pdb; pdb.set_trace()
        new_text = new_text.replace(header[0][0], "", 1).strip()
    glossary = {}
    for definition in definitions_in_file:
        if definition.groups()[3].strip() != "":
            glossary[definition.groups()[2]] = definition.groups()[3]
            new_text = new_text.replace(definition.groups()[0].strip(), "", 1)
    new_text = new_text.strip()
    not_matched = []
    if len(new_text) != 0:
        not_matched.append([filepath, new_text])
    return (glossary, not_matched)


def split_dictionary(given_dir):
    glossary = {}
    not_matched = []
    if os.path.isfile(given_dir):
        (glossary, not_matched) = split_dictionary_for_file(given_dir)
    elif os.path.isdir(given_dir):
        for root, directories, filenames in os.walk(given_dir):
            for filename in filenames:
                if filename.endswith(".md"):
                    filepath = os.path.join(root, filename)
                    (temp_glossary, temp_not_matched) = split_dictionary_for_file(filepath)
                    glossary[filename.replace(".md", "")] = temp_glossary
                    not_matched = not_matched + temp_not_matched
        file = open(GLOSSARY_FILE, "w")
        file.write(json.dumps(glossary))
        file.close()
        file = open(NOT_MATCHED_FILE, "w")
        file.write(json.dumps(not_matched))
        file.close()
        pprint(not_matched)
        print(f"Items Note Matched: {len(not_matched)}")


def main(args=[]):
    for arg in args:
        split_dictionary(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files ")
    else:
        main(sys.argv[1:])
