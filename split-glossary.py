import re, os
from pprint import pprint

MD_DIR = "./output/"
# REGEXES_USED_BY_HAND_IN_EDITOR
r'([\w<>/\*\.,\-]+)(\s*\n+\s*)([\w<>/\*\.,\-]+)'
r'([\w,\. ]*)(\n+)( ?\*\w)'
r'([\w,\. ]*\*? ?)(\n+)( ?\*\w)'
r'([\(\)\w,\. ]*\*? ?)(\n+)( ?\*?[\w\(\)])'
r'([\(\)\w,\. "\']*\*? ?)(\n+)( ?\*?[\w\(\)"\'])'
r'([\(\)\w,\.:; "\']*\*? ?)(\n+)( ?\*?[\w:;\(\)"\'])'
r'([\(\)\w,\.:; "\']*\*?)( *\n+ *)(\*?[\w:;\(\)"\'])'
r''
r'(\w+)(\s*\n+\s*)(\w+)'
r'$1 $3'
r''
r''
r'<b>((?:(?!</b>).)*)</b>'
r'$1'
r''
r'(\*\*A\*\* )'
r'(\*\*[A-Z]\*\* )'


def get_glossary_toc(sections):
    toc = "\n\n## Table of Contents\n\n"
    for section in sections:
        if section[0] == "intro":
            toc += f"\n- [26.1 Glossary - Introduction](/docs/chap-26/cg-b-glossary/{section[0]})"
        else:
            toc += f"\n- [{section[0]}](/docs/chap-26/cg-b-glossary/{section[0]})"
    toc += "\n\n"
    return toc

def add_header(text, name):
    if name == "intro":
        return "---\ntitle: 26.1 Glossary - Introduction\nsidebar_position: 0\n---\n\n" + text
    else:
        return f"---\ntitle: {name.upper()}\nsidebar_position: {ord(name)}\n---\n\n" + text

def split_glossary(target_filepath):
    file = open(target_filepath, "r")
    text = file.read()
    file.close()
    section_matches = re.finditer(r'(\*\*([A-Z])\*\* )', text)
    sections = []
    curr_index = 0
    # curr_name = filepath.split("/")[-1].removesuffix(".md")
    curr_name = "intro"
    for curr_match in section_matches:
        sections.append((curr_name, text[curr_index:curr_match.start()]))
        curr_index = curr_match.start()
        curr_name = curr_match.groups()[-1].lower()
    sections.append((curr_name, text[curr_index:]))
    # import pdb; pdb.set_trace()
    for section in sections:
        base_dir = "/".join(target_filepath.split("/")[:-1])
        curr_path = os.path.join(base_dir, section[0]) + ".md"
        file = open(curr_path, "w")
        if section[0] == "intro":
            curr_text = add_header(section[1] + get_glossary_toc(sections), section[0])
        else:
            curr_text = add_header(section[1], section[0])
        text = file.write(curr_text)
        file.close()
    

def main():
    # process_all_md_files(MD_DIR, remove_double_lines_from_code_blocks)
    split_glossary("./output/chap-26/cg-b-glossary/_cg-b-glossary.md")
    

if __name__ == "__main__":
    main()