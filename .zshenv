aklogs () {
    for r in CS.CMU.EDU ANDREW.CMU.EDU; do
        KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` aklog -c $r -k $r
    done
    aklog club.cc.cmu.edu
}
rkinits () {
    for r in CS.CMU.EDU ANDREW.CMU.EDU; do
        KRB5CCNAME=`KRB5CCNAME=DIR:$HOME/.cache/krb5cc klist -l | grep $r | sed -e s'/.*:://g'` kinit -R rkavanag@$r
    done
    aklogs
}
