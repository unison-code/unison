#!/usr/bin/expect -f

set svn_username [lindex $argv 0]
set svn_password [lindex $argv 1]
set svn_url [lindex $argv 2]

spawn svn --username=${svn_username} --password=${svn_password} list ${svn_url}
expect "(R)eject, accept (t)emporarily or accept (p)ermanently? "
send -- "p\r"
expect -re "root@.*:\/#"
