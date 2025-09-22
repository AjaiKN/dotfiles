compdef _dirs d

compdef _gnu_generic delete-ds-stores concatpdf
compdef _restic rstc
compdef _restic-backup rstc-backup
compdef _directories mkcd
compdef _directories takedir
# GNU coreutils (/opt/homebrew/Cellar/coreutils/9.5/bin/)
# "When defining a function for command names the `-n' option may be given and keeps the definitions from overriding any previous definitions for the commands"
compdef -n _gnu_generic b2sum base32 basenc chcon factor gb2sum gbase32 gbase64 gbasename gbasenc gcat gchcon gchgrp gchmod gchown gchroot gcksum gcomm gcp gcsplit gcut gdate gdd gdf gdir gdircolors gdirname gdu gecho genv gexpand gexpr gfactor gfalse gfmt gfold ggroups ghead ghostid gid ginstall gjoin gkill glink gln glogname gls gmd5sum gmkdir gmkfifo gmknod gmktemp gmv gnice gnl gnohup gnproc gnumfmt god gpaste gpathchk gpinky gpr gprintenv gprintf gptx gpwd greadlink grealpath grm grmdir gruncon gseq gsha1sum gsha224sum gsha256sum gsha384sum gsha512sum gshred gshuf gsleep gsort gsplit gstat gstdbuf gstty gsum gsync gtac gtail gtee gtimeout gtouch gtr gtrue gtruncate gtsort gtty guname gunexpand guniq gunlink guptime gusers gvdir gwc gwho gwhoami gyes hostid md5sum nproc numfmt pinky ptx runcon sha1sum sha224sum sha256sum sha384sum sha512sum shred shuf stdbuf tac timeout
compdef -n _gnu_generic gsed gtar dirmngr gpg gpg-card gpg-mail-tube gpg-wks-server gpgparsemail gpgsm gpgtar kbxutil dirmngr-client gpg-agent gpg-connect-agent gpg-wks-client gpgconf gpgscm gpgsplit gpgv watchgnupg gnuplot danetool gnutls-certtool gnutls-cli gnutls-cli-debug gnutls-serv ocsptool p11tool psktool
compdef _gnu_generic help2man groff
