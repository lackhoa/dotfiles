abbr cls 'clear'
abbr desktop 'cdl ~/Desktop'
abbr subl '/usr/bin/subl'
abbr edit-abbr 'emacs ~/.config/fish/config.fish'
abbr update-abbr 'source ~/.config/fish/config.fish'
abbr cp 'cp -vi'
abbr mv 'mv -vi'
abbr cdw 'cdl ~/Work'
abbr cdd 'cdl ~/Downloads'
abbr cdc 'cdl ~/Coq'
abbr x 'xset dpms force off'
abbr gs 'git status'
abbr grep 'grep -C 10'

# clean up stuff that were created less than 1 minute ago
abbr clean 'find . -type f -cmin -1 -delete'
abbr conky-conf 'sudo emacs /etc/conky/conky.conf'

# this does everything I want to do about updating
# this command is stored in the root user's script path
abbr khoa-update 'sudo sh /usr/local/sbin/khoa-update'

#This command deals with updaing the hash of tex:
abbr texhash 'texhash & texhash ~/texmf'

#ls right after cd
function cdl
    builtin cd $argv; and ls -F
end

