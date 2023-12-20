import re, sys, os, subprocess, json, functools
from pprint import pprint

MD_DIR = "./output/chap-26/cg-b-glossary/"
GLOSSARY_FILE = "./glossary_output/glossary.json"
NOT_MATCHED_FILE = "./glossary_output/not_matched.json"

MD_DEFINITION = r'(\n\s*\*\*(?P<definition_name>(?:(?!(\*\*)).)+)\*\*(?P<definition_content>(?:(?!\n).)+)\n)'
HTML_DEFINITION = r'(\n\s*<b>(?P<definition_name>(?:(?!(</b>)).)+)</b>(?P<definition_content>(?:(?!\n).)+)\n)'
HEADER_REGEX = r'(\n*\---\n+(\w+\:[\"\' \S\w\*_]+\n+)+\---\n+\s*\*\*\w\*\*\s*)'
MD_BOLD = r'(\*\*(?P<tag_content>\S(?:(?!(\*\*)).)*?)\*\*)'
MD_ITALICS = r'(\*(?P<tag_content>\S(?:(?!(\*\*)).)*?)\*)'

def md_tag_to_tag(tag_regex, html_tag_name, text):
    md_tags = re.finditer(tag_regex, text)
    start_index = 0
    text_array = []
    for tag in md_tags:
        text_array.append(text[start_index:tag.start()])
        html_tag = f'<{html_tag_name}>{tag.group("tag_content")}</{html_tag_name}>'
        text_array.append(html_tag)
        start_index = tag.end()
    text_array.append(text[start_index:])
    final_text = "".join(text_array)
    return final_text


def md_to_html(text):
    html_bold_text = md_tag_to_tag(MD_BOLD, "b", text)
    html_italics_text = md_tag_to_tag(MD_ITALICS, "i", html_bold_text)
    return html_italics_text

def make_glossary_for_file(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()

    html_definitions_in_file = re.finditer(HTML_DEFINITION, text)
    md_definitions_in_file = re.finditer(MD_DEFINITION, text)
    definitions_in_file = [match for match in html_definitions_in_file] + [match for match in md_definitions_in_file]
    header = re.findall(HEADER_REGEX, text)
    new_text = text
    if len(header) != 0:
        new_text = new_text.replace(header[0][0], "", 1).strip()
    glossary = {}
    for definition in definitions_in_file:
        if definition.group("definition_content").strip() != "":
            glossary[definition.group("definition_name")] = md_to_html(definition.group("definition_content"))
            new_text = new_text.replace(definition.groups()[0].strip(), "", 1)
    new_text = new_text.strip()
    not_matched = []
    if len(new_text) != 0:
        not_matched.append([filepath, new_text])
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
