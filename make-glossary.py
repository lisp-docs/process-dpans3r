import re, sys, os, subprocess, json, functools
from pprint import pprint

MD_DIR = "./output/chap-26/"

# MALFORMED_DEFINITION_REGEX = r'([\S](?P<whitespace>( ?\n+\s*)+))'
# ALTERNATIVE_DEFINITION_START_REGEX = r'(\n((<b>)|(\*\*))(?P<definition_name>(?:(?!((</b>)|(\*\*))).+))((</b>)|(\*\*))(?P<definition_content>(?:(?!\n).+))\n)'
# MD_DEFINITION = r'(\n\*\*(?P<definition_name>(?:(?!(\*\*))[\w\d\S \t.]+))\*\*(?P<definition_content>(?:(?!\n)[\w\d\S \t.]+))\n)'
MD_DEFINITION = r'(\n\s*\*\*(?P<definition_name>(?:(?!(\*\*)).)+)\*\*(?P<definition_content>(?:(?!\n).)+)\n)'
HTML_DEFINITION = r'(\n\s*<b>(?P<definition_name>(?:(?!(</b>)).)+)</b>(?P<definition_content>(?:(?!\n).)+)\n)'
# DEFINITION_REGEX = r'\n(\s*(\*\*([\w\\/ \-]+)\*\*)((?:(?!\n)[\w\W.]+)+))\n'
# HEADER_REGEX = r'(\n*---\n+\w+:["\' \w\*]\n+---\n+\s*\*\*\w\*\*\s*)'
# HEADER_REGEX = r'(\n*\---\n+(\w+\:[\"\' \w\*_]+\n+)+\---\n+\s*\*\*\w\*\*\s*)'
HEADER_REGEX = r'(\n*\---\n+(\w+\:[\"\' \S\w\*_]+\n+)+\---\n+\s*\*\*\w\*\*\s*)'
#[\"\' \w\*]\n+\-\-\-\n+)' 
#\s*\*\*\w\*\*\s*)'

GLOSSARY_FILE = "./glossary_output/glossary.json"
NOT_MATCHED_FILE = "./glossary_output/not_matched.json"

def make_glossary_for_file(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    # print(filepath)
    # code_blocks_regex = f'{START_CODE_BLOCK}{REGEX_MATCH_UNTIL.replace("X", END_CODE_BLOCK)}{END_CODE_BLOCK}'
    # definitions_in_file = re.finditer(DEFINITION_REGEX, text)
    html_definitions_in_file = re.finditer(HTML_DEFINITION, text)
    md_definitions_in_file = re.finditer(MD_DEFINITION, text)
    # definitions_in_file = 
    # [match.group("definition_name") for match in re.finditer(HTML_DEFINITION, text)]
    # [match.group("definition_name") for match in re.finditer(MD_DEFINITION, text)]
    definitions_in_file = [match for match in html_definitions_in_file] + [match for match in md_definitions_in_file]
    # [match.group("definition_name") for match in definitions_in_file]
    # text_definitions_in_file = re.findall(ALTERNATIVE_DEFINITION_START_REGEX, text)
    # re.findall(DEFINITION_REGEX, text)[-1]
    header = re.findall(HEADER_REGEX, text)
    new_text = text
    if len(header) != 0:
        # import pdb; pdb.set_trace()
        new_text = new_text.replace(header[0][0], "", 1).strip()
    # quantity_code_blocks_in_file = len(re.findall(code_blocks_regex, text))
    glossary = {}
    # import pdb; pdb.set_trace()
    for definition in definitions_in_file:
        # if definition.groups()[3].strip() != "":
        if definition.group("definition_content").strip() != "":
            # print(len(definition.groups()))
            # print(definition.groups())
            # print(definition.groups()[0])
            # print(definition.groups()[1])
            # print(definition.groups()[2])
            # print(definition.groups()[3])
            # import pdb; pdb.set_trace()
            # glossary[definition.groups()[2]] = definition.groups()[3]
            glossary[definition.group("definition_name")] = definition.group("definition_content")
            new_text = new_text.replace(definition.groups()[0].strip(), "", 1)
    new_text = new_text.strip()
    not_matched = []
    if len(new_text) != 0:
        # print(filepath)
        # print(len(new_text))
        # print(new_text)
        not_matched.append([filepath, new_text])
        # import pdb; pdb.set_trace()
        # new_text = new_text.replace(definition.group)
    return (glossary, not_matched)


def make_glossary(given_dir):
    glossary = {}
    not_matched = []
    if os.path.isfile(given_dir):
        (glossary, not_matched) = make_glossary_for_file(given_dir)
    elif os.path.isdir(given_dir):
        for root, directories, filenames in os.walk(given_dir):
            for filename in filenames:
                if filename.endswith(".md"):
                    filepath = os.path.join(root, filename)
                    (temp_glossary, temp_not_matched) = make_glossary_for_file(filepath)
                    # for k in temp_glossary.keys():
                    #     if k in glossary:
                    #         print(filename)
                    #         print(k)
                    #         print(glossary[k])
                    #         print(temp_glossary[k])
                    #         not_matched.append([filepath, k + " " + temp_glossary[k]])
                    #         # import pdb; pdb.set_trace()
                    #         # raise Exception("item already in dictionary!")
                    #     else:
                    #         glossary[k] = temp_glossary[k]
                    glossary[filename.replace(".md", "")] = temp_glossary
                    not_matched = not_matched + temp_not_matched
        file = open(GLOSSARY_FILE, "w")
        file.write(json.dumps(glossary))
        file.close()
        file = open(NOT_MATCHED_FILE, "w")
        file.write(json.dumps(not_matched))
        file.close()
        if len(not_matched) > 0:
            total_lines_per_file = [len(nm[1].split("\n")) for nm in not_matched]
            total_lines = functools.reduce(lambda x, y: x+y, total_lines_per_file)
            print(f'There were a total of {total_lines} lines which were not matched over {len(not_matched)} files.')
            print(f'Please check the file {NOT_MATCHED_FILE} to see which text was not matched')
        # pprint(not_matched)
        # print(f"Items Note Matched: {len(not_matched)}")


def main(args=[MD_DIR]):
    for arg in args:
        make_glossary(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("No directory with markdown files provided")
        print(f"Defaulting to run on {MD_DIR}")
        main()
    else:
        main(sys.argv[1:])
