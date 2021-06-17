aklogs () {
    for r in CS.CMU.EDU ANDREW.CMU.EDU CLUB.CC.CMU.EDU; do
        KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` aklog -c $r -k $r
        if test "x$r" = "xCS.CMU.EDU"; then
            KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` aklog -c club.cc.cmu.edu
        fi
    done
}
kinits () {
    kinit -r 200h -f rkavanag@CS.CMU.EDU
    kinit -r 200h -f rkavanag@ANDREW.CMU.EDU
    kinit -r 200h -f rak@CLUB.CC.CMU.EDU
}
rkinits () {
    for r in CS.CMU.EDU ANDREW.CMU.EDU CLUB.CC.CMU.EDU; do
        KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` kinit -R rkavanag@$r
    done
    KRB5CCNAME=DIR:$HOME/.cache/krb5cc aklogs
}
