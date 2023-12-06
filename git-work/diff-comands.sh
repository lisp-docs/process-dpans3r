alias gitwa='git diff --word-diff=porcelain origin/master | grep -e "^+[^+]" | wc -w | xargs'
alias gitwd='git diff --word-diff=porcelain origin/master | grep -e "^-[^-]" | wc -w | xargs'
alias gitwdd='git diff --word-diff=porcelain origin/master |grep -e"^+[^+]" -e"^-[^-]"|sed -e's/.//'|sort|uniq -d|wc -w|xargs'

alias gitw='echo $(($(gitwa) - $(gitwd)))'


git diff --word-diff=porcelain | grep -e "^+[^+]" | wc -w | xargs

./output/chap-1/b-e-definitions/_b-e-b-c-b-splicing-in-modified-bnf-syntax.md


git diff --word-diff=porcelain ./output/chap-1/b-e-definitions/_b-e-b-c-b-splicing-in-modified-bnf-syntax.md | grep -e "^+[^+]" | wc -w | xargs


./output/chap-1/b-e-definitions/_b-e-b-c-d-additional-uses-for-indirect-definitions-in-modified-bnf-syntax.md
./output/chap-1/b-e-definitions/_b-e-b-c-d-additional-uses-for-indirect-definitions-in-modified-bnf-syntax.md

git diff --word-diff=porcelain ./output/chap-1/b-e-definitions/_b-e-b-c-d-additional-uses-for-indirect-definitions-in-modified-bnf-syntax.md  | grep -e "^+[^+]" | wc -w | xargs
git diff --word-diff=porcelain ./output/chap-1/b-e-definitions/_b-e-b-c-d-additional-uses-for-indirect-definitions-in-modified-bnf-syntax.md  | grep -e "^-[^-]" | wc -w | xargs
