import os, sys, json, re
from pprint import pprint 

MD_DIR = "./output/chap-26/cg-b-glossary/"
# DEFINITION_TITLE = r'\*\*[\w]+(\w+\-?)+\*\*'
# LOOK_AHEAD_REGEX_MATCH_OPEN = '(?:(?!{}){})*'
# LOOK_AHEAD_REGEX_MATCH_OPEN = '(?:(?!\*\*).)*'
MALFORMED_DEFINITION_REGEX = r'([\S](?P<whitespace>( ?\n+\s*)+)(?:(?!((<b>)|(\*\*))).))'
HEADER_REGEX = r'(\n*\---\n+([\w_]+\:[\"\' \d\w\*_]+\n+)+\---\n+\s*\*\*\w\*\*\s*)'

def fix_glossary(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    header = re.findall(HEADER_REGEX, text)
    new_text = []
    if len(header) != 0:
        offset = len(header[0][0])
        text = text[offset:]
        new_text.append(header[0][0])
    # Fixing messed up quantities of new line characters
    text = re.sub("\n+", "\n\n", text)
    matched_strings = re.findall(MALFORMED_DEFINITION_REGEX, text)
    matches = re.finditer(MALFORMED_DEFINITION_REGEX, text)
    # matches = re.search(MALFORMED_DEFINITION_REGEX, text)
    start_index = 0
    for match in matches:
        # The + 1's are for the non white space character matched before the white space
        white_space_index = match.start() + 1
        new_text.append(text[start_index:white_space_index])
        start_index = match.start() + 1 + len(match.group("whitespace")) 
        # import pdb; pdb.set_trace()
    new_text.append(text[start_index:])
    final_text = " ".join(new_text)
    # pprint(final_text)
    # pprint(re.sub("\n+", "\n\n", final_text))
    # import pdb; pdb.set_trace()
    file = open(filepath, "w")
    file.write(final_text)
    file.close()

def process_dir(given_dir):
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if "glossary" not in filename and ".DS_Store" not in filename:
                filepath = os.path.join(root, filename)
                fix_glossary(filepath)

def main(args=[MD_DIR]):
    for arg in args:
        process_dir(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        # print("Please provide a directory with markdown files")
        print("No directory with markdown files provided")
        print(f"Defaulting to run on {MD_DIR}")
        main()
    else:
        main(sys.argv[1:])
