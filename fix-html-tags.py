# Needs python >= 3.9 
# fix-html-tags.py
from bs4 import BeautifulSoup
import os, re

MD_DIR = "./output/"

def process_all_md_files(given_dir):
    for root, dirs, filenames in os.walk(given_dir):
        for filename in filenames:
            if filename.endswith(".md"):
                process_file(filename, root)

def fix_case(file_text, processed_text):
    react_import_tag_regex = r"import \w+ from '[\.\/\-\w]+';\n\n<(\w+) />"
    import_tag_regex = r"import \w+ from '[\.\/\-\w]+';\n\n<(\w+)>"
    html_tags = re.findall(import_tag_regex, processed_text)
    react_tags = re.findall(react_import_tag_regex, file_text)
    html_close_tags = [f'</{tag.lower()}>' for tag in react_tags]
    for tag in html_close_tags:
        processed_text = processed_text.replace(tag, "")
    for tag in react_tags:
        if tag.lower() in html_tags:
            processed_text = processed_text.replace(f"<{tag.lower()}>", f"<{tag} />")
    return processed_text

def fix_case_lisp(file_text, processed_text):
    cl_tag_regex = r'(#<[A-Z\-]+ "[A-Z\-]+">)'
    html_tag_regex = r'(#<[a-z\-]+ "[a-z\-]+"="">)'
    close_tag_regex = r'#<([a-z\-]+) "[a-z\-]+"="">'
    html_tags = re.findall(html_tag_regex, processed_text)
    cl_tags = re.findall(cl_tag_regex, file_text)
    html_close_tags = re.findall(close_tag_regex, processed_text)
    # html_close_tags = [f'</{tag.lower()}>' for tag in cl_tags]
    for tag in html_close_tags:
        processed_text = processed_text.replace(tag, "")
    for tag in cl_tags:
        cl_to_html_tag = tag.lower().removesuffix(">") + '="">'
        if cl_to_html_tag in html_tags:
            processed_text = processed_text.replace(cl_to_html_tag, tag)
    import pdb; pdb.set_trace()
    return processed_text


def process_file(filename, root):
    filepath = os.path.join(root, filename)
    file = open(filepath, "r")
    file_text = file.read()
    file_text = file_text.replace("\\{", "{").replace("\\}", "}")
    div_text = f'<div>{file_text}</div>'
    file.close()
    # soup = BeautifulSoup(file_text, 'html5')
    soup = BeautifulSoup(div_text, 'html5lib')
    # soup = BeautifulSoup(div_text, 'xml')
    cleaned_text = str(soup.div)
    processed_text = cleaned_text.strip().removeprefix("<div>").removesuffix("</div>")
    processed_text = fix_case(file_text, processed_text)
    # [].index()
    if processed_text.strip().lower() != file_text.strip().lower():
        print(filepath)
        # import pdb; pdb.set_trace()
        processed_text = fix_case_lisp(file_text, processed_text)
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