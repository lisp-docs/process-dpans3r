import sys, os, re, subprocess
from pprint import pprint


WORDS_ADDED = f'git diff --word-diff=porcelain origin/master | grep -e "^+[^+]" | wc -w | xargs'
WORDS_DELETED = f'git diff --word-diff=porcelain origin/master | grep -e "^-[^-]" | wc -w | xargs'
WORDS_DOUBLED = f'git diff --word-diff=porcelain origin/master |grep -e"^+[^+]" -e"^-[^-]"|sed -e\'s/.//\'|sort|uniq -d|wc -w|xargs'

f'echo $(($(gitwa) - $(gitwd)))'

def stage(word_count):
    # output = subprocess.check_output(['./usr/local/bin/git status -s'])
    # output = subprocess.check_output(['./childdir/execute.sh',str(var1),str(var2)])
    # process = subprocess.Popen('./usr/local/bin/git status -s', stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    process = subprocess.Popen('git status -s', stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = process.communicate()
    files_changed_list = stdout.decode("utf-8").splitlines()
    unstaged_files = [file.split(" ")[-1] for file in files_changed_list if file[0] != "M"]
    # pprint(files_changed_list[:5])
    pprint(unstaged_files[:5])

def main(args=[]):
    if len(args) <= 1:
        print("Please provide a word count changed as an upper limit for files to stage.")
    else:
        stage(args[1])


if __name__ == "__main__":
    main(sys.argv)