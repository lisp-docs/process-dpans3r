# Needs python >= 3.9 
# fix-html-tags.py
from bs4 import BeautifulSoup
import os, re

MD_DIR = "./output/"
STANDARD_HTML_TAGS = ["i", "b", "sub", "sup", "p"]

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

def new_fix_lisp_tags(file_text, processed_text):
    # cl_tag_regex = r'(#<([A-Z\-]+) (["\w\d A-Z\-\W]+)>)'
    cl_tag_regex = r'(#<([^>]+)>)'
    all_tags_regex = r'(<([^>]+)>)'
    html_tag_regex = r'(#<[a-z\-]+ ([\d\w\s"a-z\-\W]+="")*>)'
    html_tags = re.findall(html_tag_regex, processed_text)
    cl_tags = re.findall(cl_tag_regex, file_text)
    all_tags = re.findall(all_tags_regex, processed_text)
    import pdb; pdb.set_trace()
    all_tags = [tag for tag in all_tags if not tag[1] in STANDARD_HTML_TAGS]
    cl_tags = [tag for tag in cl_tags if not tag[1].split(" ") in STANDARD_HTML_TAGS]
    # html_close_tags = [f'</{tag[1].lower()}>' for tag in cl_tags]
    # for tag in html_close_tags:
    #     processed_text = processed_text.replace(tag, "")
    for index, tag in enumerate(cl_tags):
        # here's the problem again, I'm not checking correctly....
        import pdb; pdb.set_trace()
        processed_text = processed_text.replace(html_tags[index][0], tag[0])
    # if processed_text.strip().endswith(">"):
    #     import pdb; pdb.set_trace()
    return processed_text

def fix_case_lisp(file_text, processed_text):
    # cl_tag_regex = r'(#<[A-Z\-]+ "[A-Z\-]+">)'
    # html_tag_regex = r'(#<[a-z\-]+ "[a-z\-]+"="">)'
    # close_tag_regex = r'#<([A-Z\-]+) ["\w\d A-Z\-]+>'
    # cl_tag_regex = r'(#<[A-Z\-]+ ["\w\d A-Z\-]+>)'
    cl_tag_regex = r'(#<([A-Z\-]+) ["\w\d A-Z\-\W]+>)'
    html_tag_regex = r'(#<[a-z\-]+ ([\d\w\s"a-z\-\W]+="")*>)'
    html_tags = re.findall(html_tag_regex, processed_text)
    cl_tags = re.findall(cl_tag_regex, file_text)
    # html_close_tags = re.findall(close_tag_regex, processed_text)
    html_close_tags = [f'</{tag[1].lower()}>' for tag in cl_tags]
    for tag in html_close_tags:
        processed_text = processed_text.replace(tag, "")
    for index, tag in enumerate(cl_tags):
        # here's the problem again, I'm not checking correctly....
        # cl_to_html_tag = tag[0].lower().removesuffix(">") + '="">'
        # if cl_to_html_tag in html_tags:
        # import pdb; pdb.set_trace()
        processed_text = processed_text.replace(html_tags[index][0], tag[0])
    # if processed_text.strip().endswith(">"):
    #     import pdb; pdb.set_trace()
    return processed_text

def fix_case_simple(file_text, processed_text):
    # problems because of deleted or added tags in the processed.. should first remove all standard tags, 
    # then manage the rest...
    simple_tag = r"(<\s*([\w\-\*\~,#&;\d]+)\s*>)"
    actual_html_tags = ["p", "i", "sub", "sup", "b"]
    tags = re.findall(simple_tag, processed_text)
    closed_tags = [(tag[0], tag[1], f'</{tag[1]}>') for tag in tags]
    original_tags = re.findall(simple_tag, file_text)
    for tag in closed_tags:
        if not tag[1] in actual_html_tags:
            processed_text = processed_text.replace(tag[2], "")
    for index, tag in enumerate(tags):
        if not tag[1] in actual_html_tags:
            if tag[0] != original_tags[index][0] and tag[0].lower() == original_tags[index][0].lower():
                # import pdb; pdb.set_trace()
                new_tag = original_tags[index][0].replace("<", "&lt;").replace(">", "&gt;")
                processed_text = processed_text.replace(tag[0], new_tag)
    return processed_text

def fix_tildes(text):
    return text.replace("~", "&#126;")

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
    processed_text = fix_case_simple(file_text, processed_text)
    processed_text = fix_tildes(processed_text)
    if processed_text.strip().lower() != file_text.strip().lower():
        processed_text = fix_case_lisp(file_text, processed_text)
        # processed_text = new_fix_lisp_tags(file_text, processed_text)
        file = open(filepath, "w")
        file.write(processed_text)
        file.close()

def main():
    process_all_md_files(MD_DIR)

if __name__ == "__main__":
    main()