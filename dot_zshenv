# for gpg-agent/pinentry to work, even in non-interactive shells
export GPG_TTY="${TTY}"

# Otherwise we cannot use ^o as a keybinding
if [[ `uname` == "Darwin" ]]; then
    stty discard undef
fi

aklogs () {
    for r in ANDREW.CMU.EDU CLUB.CC.CMU.EDU; do
        KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` aklog -c $r -k $r
    done
}

kinits () {
    kinit -r 200h -f rkavanag@ANDREW.CMU.EDU
    kinit -r 200h -f rak@CLUB.CC.CMU.EDU
}

rkinits () {
    for r in rkavanag@ANDREW.CMU.EDU rak@CLUB.CC.CMU.EDU; do
        KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` kinit -R $r
    done
    KRB5CCNAME=DIR:$HOME/.cache/krb5cc aklogs
}
