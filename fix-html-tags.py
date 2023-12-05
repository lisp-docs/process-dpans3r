# Needs python >= 3.9 
# fix-html-tags.py
from bs4 import BeautifulSoup
import os

MD_DIR = "./output/"

def process_all_md_files(given_dir):
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                process_file(filename, root)

def process_file(filename, root):
    filepath = os.path.join(root, filename)
    file = open(filepath, "r")
    file_text = file.read()
    div_text = f'<div>{file_text}</div>'
    file.close()
    # soup = BeautifulSoup(file_text, 'html5')
    soup = BeautifulSoup(div_text, 'html5lib')
    # soup = BeautifulSoup(div_text, 'xml')
    cleaned_text = str(soup.div)
    processed_text = cleaned_text.strip().removeprefix("<div>").removesuffix("</div>")
    if processed_text.strip().lower() != file_text.strip().lower():
        import pdb; pdb.set_trace()
        print(filepath)
        file = open(filepath, "w")
        file.write(processed_text)
        file.close()

    # import pdb; pdb.set_trace()
    # file = open(filepath, "w")
    # file.write(str(soup.))
    # file.close()

def main():
    process_all_md_files(MD_DIR)
    # sample_problem = "_b-e-b-c-b-splicing-in-modified-bnf-syntax.md"
    # sample_root = "./output/chap-1/b-e-definitions/"
    # process_file(sample_problem, sample_root)
    # filepath = "./ou"
if __name__ == "__main__":
    main()