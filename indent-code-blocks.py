import re, sys, os, subprocess
from pprint import pprint

# emacs --batch MY_FILE --eval '(indent-region (point-min) (point-max))' -f 'save-buffer'
# emacs --batch output/chap-5/f-d-dictionary/_progn.md --eval '(indent-region 506 704)' -f 'save-buffer'
EMACS_COMMAND = "emacs --batch {} --eval '(indent-region {} {})' -f 'save-buffer'"
REGEX_MATCH_UNTIL = r"(?:(?!X)[\w\W\s\S\d\D.])*"
LOOK_AHEAD_REGEX = '(?:(?!{})[^\n])*'
UNTIL_NEW_LINE_REGEX = LOOK_AHEAD_REGEX.format("\n")
START_CODE_BLOCK = f"```lisp{UNTIL_NEW_LINE_REGEX}"
# START_CODE_BLOCK = f"```lisp"
END_CODE_BLOCK = "```"
TEMP_FILE = "./temp_processing/cl-code.lisp"

def get_shell_command_output(command_string):
    process = subprocess.Popen(command_string, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = process.communicate()
    return stdout.decode("utf-8").splitlines()
    
def execute_shell_command(command_string):
    process = subprocess.Popen(command_string, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, shell=True)
    process.communicate()

def execute_indent_for_file(given_path, start, end):
    execute_shell_command(EMACS_COMMAND.format(given_path, start, end))

def indent_code_blocks(filepath):
    # ensure temp file directory and file exist
    if not os.path.exists(TEMP_FILE):
        temp_dir = "/".join(TEMP_FILE.split("/")[:-1])
        os.makedirs(temp_dir)

    file = open(filepath, "r")
    text = file.read()
    file.close()
    code_blocks_regex = f'(?P<code_block_start>{START_CODE_BLOCK}){REGEX_MATCH_UNTIL.replace("X", END_CODE_BLOCK)}{END_CODE_BLOCK}'
    code_blocks_in_file = re.finditer(code_blocks_regex, text)
    new_text = text
    for code_block in code_blocks_in_file:
        groupdict = code_block.groupdict()
        code_block_start_string = groupdict["code_block_start"]
        (start,end) = (code_block.start() + len(code_block_start_string), code_block.end() - len(END_CODE_BLOCK))
        code_block_contents = text[start:end]
        # IMPORTANT NOTE: EMACS WAS NOT INDENTING THE CODE BECAUSE WHEN IT READS THE FILE 
        # IT DOESN'T DO LISP CODE EVEN THOUGH THE WHOLE REGION IS LISP, THEREFORE THE CODE
        # HAS TO BE IN A NEW FILE FOR EMACS TO INDENT IT
        write_to_file(TEMP_FILE, code_block_contents)
        execute_indent_for_file(TEMP_FILE, 0, len(code_block_contents))
        indented_code = get_file_text(TEMP_FILE)
        new_text = text[:start] + indented_code + text[end:]
    write_to_file(filepath, new_text)

def get_file_text(filepath):
    file = open(filepath, "r")
    text = file.read()
    file.close()
    return text

def write_to_file(filepath, text):
    file = open(filepath, "w")
    file.write(text)
    file.close()

    
def indent_code_blocks_in_dir(given_dir):
    if os.path.isfile(given_dir):
        indent_code_blocks(given_dir)
    elif os.path.isdir(given_dir):
        for root, directories, filenames in os.walk(given_dir):
            for filename in filenames:
                if filename.endswith(".md"):
                    filepath = os.path.join(root, filename)
                    indent_code_blocks(filepath)
                    print(".", end="", flush=True)
        print("\n")


def main(args=[]):
    for arg in args:
        indent_code_blocks_in_dir(arg)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide a directory with markdown files containing lisp code blocks to indent")
        print("Note that the code blocks must be formatted as:\n\n```lisp\n\n...\n\n```\n\n")
    else:
        main(sys.argv[1:])
