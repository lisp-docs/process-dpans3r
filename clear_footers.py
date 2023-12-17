import os, sys, json, re
from pprint import pprint 

MD_DIR = "./output/"

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
    for dir in args:
        clear_footers(dir)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        main()
    else:
        main(sys.argv[1:])
