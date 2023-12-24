import re, sys, os, subprocess, json
from pprint import pprint

DICTIONARY_FILE = "./glossary_output/dictionary.json"
TITLE_REGEX = r'(title:\s"(?P<title>[^"]+)")'

# TODO delete this function???
def make_dictionary_items_dict_for_file(filepath):
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
    dictionary_items = {}
    for definition in definitions_in_file:
        if definition.groups()[3].strip() != "":
            # print(len(definition.groups()))
            # print(definition.groups())
            # print(definition.groups()[0])
            # print(definition.groups()[1])
            dictionary_items[definition.groups()[2]] = definition.groups()[3]
            new_text = new_text.replace(definition.groups()[0].strip(), "", 1)
    new_text = new_text.strip()
    not_matched = []
    if len(new_text) != 0:
        not_matched.append([filepath, new_text])
    return (dictionary_items, not_matched)

def get_title_items(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    title_match = re.search(TITLE_REGEX, text)
    if title_match:
        return [title_match.group("title").strip()] + [item.strip() for item in title_match.group("title").split(",")]
    else:
        return None

def make_dictionary_items_dict(given_dir):
    dictionary_items = {}
    # not_matched = []
    if os.path.isfile(given_dir):
        # (dictionary_items, not_matched) = make_dictionary_items_dict_for_file(given_dir)
        print(given_dir)
    elif os.path.isdir(given_dir):
        for root, directories, filenames in os.walk(given_dir):
            if "dictionary" in root.lower():
                # print(root)
                for filename in filenames:
                    if filename.endswith(".md") and not filename.startswith("_"):
                        filepath = os.path.join(root, filename)
                        # print(filename)
                        # item = filename.replace(".md", "")
                        items = get_title_items(filepath)
                        url = "/".join(filepath.split("/")[-4:]).replace(".md", "")
                        # print(filepath)
                        # print(item)
                        # print(url)
                        for item in items:
                            dictionary_items[item] = url
        file = open(DICTIONARY_FILE, "w")
        file.write(json.dumps(dictionary_items))
        file.close()


def main(args=[]):
    for arg in args:
        make_dictionary_items_dict(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files ")
    else:
        main(sys.argv[1:])
