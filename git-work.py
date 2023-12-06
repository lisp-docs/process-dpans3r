import sys, os, re, subprocess
from pprint import pprint


WORDS_ADDED = 'git diff --word-diff=porcelain {} | grep -e "^+[^+]" | wc -w | xargs'
WORDS_DELETED = 'git diff --word-diff=porcelain {} | grep -e "^-[^-]" | wc -w | xargs'
WORDS_DOUBLED = 'git diff --word-diff=porcelain {} |grep -e"^+[^+]" -e"^-[^-]"|sed -e\'s/.//\'|sort|uniq -d|wc -w|xargs'
GIT_ADD = 'git add {}'
'echo $(($(gitwa) - $(gitwd)))'

def get_shell_command_output(command_string):
    process = subprocess.Popen(command_string, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = process.communicate()
    return stdout.decode("utf-8").splitlines()
    
def execute_shell_command(command_string):
    process = subprocess.Popen(command_string, shell=True)
    # stdout, stderr = process.communicate()
    process.communicate()
    # return stdout.decode("utf-8").splitlines()
    
def stage(word_count=20):
    # output = subprocess.check_output(['./usr/local/bin/git status -s'])
    # output = subprocess.check_output(['./childdir/execute.sh',str(var1),str(var2)])
    # process = subprocess.Popen('./usr/local/bin/git status -s', stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # process = subprocess.Popen('git status -s', stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    # stdout, stderr = process.communicate()
    # files_changed_list = stdout.decode("utf-8").splitlines()
    word_count = int(word_count)
    files_changed_list = get_shell_command_output('git status -s')
    unstaged_files = [file.split(" ")[-1] for file in files_changed_list if file[0] != "M" and file.endswith(".md")]
    words_added_commands = [WORDS_ADDED.format(file) for file in unstaged_files]
    words_deleted_commands = [WORDS_DELETED.format(file) for file in unstaged_files]
    words_added = [get_shell_command_output(wa) for wa in words_added_commands]
    words_added = [int(wa[0]) for wa in words_added]
    words_deleted = [get_shell_command_output(wd) for wd in words_deleted_commands]
    words_deleted = [int(wd[0]) for wd in words_deleted]
    files_to_stage = [file for index, file in enumerate(unstaged_files) if words_added[index] < word_count and words_deleted[index] < word_count and words_added[index] - words_deleted[index] < 5]
    for file in files_to_stage:
        execute_shell_command(GIT_ADD.format(file))
    # pprint(files_changed_list[:5])
    # pprint(unstaged_files[:5])
    # pprint(words_added[:5])
    # pprint(files_to_stage[:5])
    print(f"Staged {len(files_to_stage)} files")

def main(args=[]):
    if len(args) <= 1:
        print("Please provide a word count changed as an upper limit for files to stage.")
    else:
        stage(args[1])


if __name__ == "__main__":
    main(sys.argv)