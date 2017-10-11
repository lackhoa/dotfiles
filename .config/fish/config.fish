# MY DOINGS: Aliases:
alias cls 'clear'
alias desktop 'cdl ~/Desktop'
alias subl '/usr/bin/subl'
alias add-alias 'vim ~/.profile'
alias update-alias 'source ~/.profile'
alias cp 'cp -vi'
alias mv 'mv -vi'
alias cdw 'cdl ~/Work'
alias cdd 'cdl ~/Downloads'

# clean up stuff that were created less than 1 minute ago
alias clean 'find . -type f -cmin -1 -delete'
alias conky-conf 'sudo vim /etc/conky/conky.conf'

# this does everything I want to do about updating
# this command is stored in the root user's script path
alias khoa-update 'sudo sh /usr/local/sbin/khoa-update'

#This command deals with updaing the hash of tex:
alias texhash 'texhash & texhash ~/texmf'

#ls right after cd
function cdl
    builtin cd $argv; and ls -F
end

