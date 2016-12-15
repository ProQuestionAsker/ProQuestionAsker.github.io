#!/usr/bin/env bash

# This script does the required work to set up your personal GitHub Pages
# repository for deployment using Hugo. Run this script only once -- when the
# setup has been done, run the `deploy.sh` script to deploy changes and update
# your website. See
# https://hjdskes.github.io/blog/deploying-hugo-on-personal-github-pages/index.html
# for more information.

# GitHub username
USERNAME=ProQuestionAsker
# Name of the branch containing the Hugo source files.
SOURCE=sources

msg() {
    printf "\033[1;32m :: %s\n\033[0m" "$1"
}

msg "Deleting the \`master\` branch"
git branch -D master
git push origin --delete master

msg "Creating an empty, orphaned \`master\` branch"
git checkout --orphan master
git rm --cached $(git ls-files)

msg "Grabbing one file from the \`$SOURCE\` branch so that a commit can be made"
git checkout "$SOURCE" README.md
git commit -m "Initial commit on master branch"
git push origin master

msg "Returning to the \`$SOURCE\` branch"
git checkout -f "$SOURCE"

msg "Removing the \`public\` folder to make room for the \`master\` subtree"
rm -rf public
git add -u
git commit -m "Remove stale public folder"

msg "Adding the new \`master\` branch as a subtree"
git subtree add --squash --prefix public 
	git@github.com:ProQuestionAsker/ProQuestionAsker.github.io.git master

	
msg "Pulling down the just committed file to help avoid merge conflicts"
git subtree pull --squash --prefix public 
	git@github.com:ProQuestionAsker/ProQuestionAsker.github.io.git master
